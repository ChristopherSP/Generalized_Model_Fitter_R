#################################################################################################
#                                   Modelo 3 - Proc e Improc
#################################################################################################
mod_m3 = function(dt,indep.var,method = c('rf','gbm','svm','glm','naive','ensemble'),wrapper = 'h2o.glm.wrapper',ntrees = 1000, max_depth = 15, learn_rate = 0.001, kernel = 'radial', family = 'binomial'){
  
  outputColumnName = variables$outputColumnNames$Sentence
  dependentColumn = variables$independentVariables$Sentence
  
  method = match.arg(method)
  
  # Remove acordo label
  enc_prep = dt[get(dependentColumn)%in%variables$filterLabels$Sentence]
  
  # Split train e test
  linhas = 1:nrow(enc_prep)
  set.seed(112358)
  idx_treino = sample(linhas,floor(0.8*length(linhas)))
  train = enc_prep[idx_treino]
  test = enc_prep[-idx_treino]
  
  tryCatch(h2o.shutdown(prompt = F),
           error = function(cond){
             return(NULL)
           })
  # Check open connections
  while(tryCatch(h2o.clusterIsUp(conn = h2o.getConnection()), error = function(cond){print(cond);return(F)})){
    Sys.sleep(1)
  }
  # Initialize a connection
  localH2O = h2o.init(ip = "localhost", port = 54323,nthreads = -1,min_mem_size = '10M',max_mem_size = paste0(floor(as.integer(system("cat /proc/meminfo | head -n 1 | cut -d' ' -f9",intern = T))/(1024^2)) - 1,'G'))
  
  # Convert train and test datasets to h2o type
  train.h2o = as.h2o(train)
  test.h2o = as.h2o(test)
  
  # Define dependent and independent variables
  y.dep = which(names(train.h2o) == dependentColumn)
  x.indep = which(names(train.h2o) %in% indep.var)
  
  # Define parameter to randomForest
  mtry = floor(sqrt(length(x.indep)))
  print("#################################################")
  print("                   START  MODEL                  ")
  print("#################################################")
  gc()
  switch(method,
         gbm = {
           model = h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = ntrees, learn_rate = learn_rate, max_depth = max_depth, seed = 112358, model_id = paste0('mod3_',method))
         },
         rf = {
           model = h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = ntrees, mtries = mtry, max_depth = max_depth, seed = 112358, model_id = paste0('mod3_',method))
         },
         glm = {
           model = h2o.glm(y=y.dep, x=x.indep, training_frame = train.h2o,family = family, seed = 112358, model_id = paste0('mod3_',method))
         },
         naive = {
           model = h2o.naiveBayes(y=y.dep, x=x.indep, training_frame = train.h2o, seed = 112358, model_id = paste0('mod3_',method))
         },
         ensemble = {
           learner = c("h2o.glm.wrapper","h2o.randomForest.wrapper", "h2o.gbm.wrapper",'h2o.naiveBayes.wrapper')
           metalearner = wrapper
           model = h2o.ensemble(y = y.dep, x=x.indep, training_frame = train.h2o, family = family, learner = learner, metalearner = metalearner, cvControl = list(V = 5))
         }
  )
  print("#################################################")
  print("                    END MODEL                    ")
  print("#################################################")
  gc()
  
  # Get variable importance
  if(method%in%c('rf','gbm')){
    varimp = as.data.table(h2o.varimp(model))
    varimp[,cpercentage:=cumsum(percentage)] 
  }else{
    varimp = NULL
  }
  
  # Predict for train and test
  predict.teste = as.data.table(h2o.predict(model, test.h2o))
  predict.treino = as.data.table(h2o.predict(model, train.h2o))
  
  # Confusion matrix and goodness of fit for test dataset
  conf_mat_test = caret::confusionMatrix(table(predict.teste$predict,as.data.table(test.h2o[names(test.h2o) == dependentColumn])[[dependentColumn]]))
  
  # Confusion matrix and goodness of fit for train dataset
  conf_mat_train = caret::confusionMatrix(table(as.data.table(h2o.predict(model, train.h2o))$predict,as.data.table(train.h2o[names(test.h2o) == dependentColumn])[[dependentColumn]]))
  
  # Accuracy for test dataset                                        
  accuracy_test = data.table(accuracy = conf_mat_test$overall[1], accuracy_inf = conf_mat_test$overall[3], accuracy_sup = conf_mat_test$overall[4])
  
  # Accuracy for train dataset
  accuracy_train = data.table(accuracy = conf_mat_train$overall[1], accuracy_inf = conf_mat_train$overall[3], accuracy_sup = conf_mat_train$overall[4])
  
  # Generalization measure and my own normalization
  generalization_measure = abs((1-accuracy_train$accuracy_sup) - (1-accuracy_test$accuracy_sup))
  prop_gen = 100 - round(100*generalization_measure,2)
  
  # ROC curve and AUC
  roc = roc(as.data.table(test.h2o[names(test.h2o) == dependentColumn])[[dependentColumn]],predict.teste[[variables$probabilityClass$Sentence]])
  # Best threshold for proc/improc
  best_threshold = coords(roc, "best", ret = "threshold")

  # PR curve and AUC for procedente
  pr_positive = pr.curve(scores.class0 = predict.teste[test[[dependentColumn]] == variables$probabilityClass$Sentence][[variables$probabilityClass$Sentence]], scores.class1 = predict.teste[test[[dependentColumn]] != variables$probabilityClass$Sentence][[variables$probabilityClass$Sentence]], curve = TRUE,max.compute = T,min.compute = T,rand.compute = T)
  
  # PR curve and AUC for improcedente
  pr_negative = pr.curve(scores.class0 = predict.teste[test[[dependentColumn]] != variables$probabilityClass$Sentence][[names(predict.teste)[!names(predict.teste)%in%c("predict",variables$probabilityClass$Sentence)]]], scores.class1 = predict.teste[test[[dependentColumn]] == variables$probabilityClass$Sentence][[names(predict.teste)[!names(predict.teste)%in%c("predict",variables$probabilityClass$Sentence)]]], curve = TRUE,max.compute = T,min.compute = T,rand.compute = T)
  
  # Save model
  h2o.saveModel(model, path = "./output/models/",force = T)
  # Clean h2o objects
  h2o.removeAll()
  # Close connection to h2o cluster
  h2o.shutdown(prompt = F)
  # Clean memory
  gc()
  # Return model info
  
  return(list(accuracy = conf_mat_test, gen_measure = generalization_measure, prop_gen = prop_gen, roc = roc, best_threshold = best_threshold, pr_positive = pr_positive, pr_negative = pr_negative, var.importante = varimp))
  
}
