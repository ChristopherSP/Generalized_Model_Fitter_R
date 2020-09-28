#################################################################################################
#                             Modelo 2 - Previsao de tempo de processo
#################################################################################################
mod_m2 = function(dt,indep.var,method = c('gbm','rf','svm','ensemble'),wrapper = 'h2o.glm.wrapper',family = 'gaussian',ntrees = 1000, max_depth = 15, learn_rate = 0.001,kernel = 'polynomial',degree=3,remove_tail = T,conf = 0.95){
  
  outputColumnName = variables$outputColumnNames$ProcessDuration
  dependentColumn = variables$independentVariables$ProcessDuration
  
  method = match.arg(method)
  
  if(remove_tail){
    # Remove 5% most extreme values
    prep = dt[get(dependentColumn) <= quantile(get(dependentColumn), conf, na.rm=T)]
  }else{
    prep = copy(dt)
  }
  
  # Split train e test
  linhas = 1:nrow(prep)
  set.seed(112358)
  idx_treino = sample(linhas,floor(0.8*length(linhas)))
  train = prep[idx_treino]
  test = prep[-idx_treino]
  
  tryCatch(h2o.shutdown(prompt = F),
           error = function(cond){
             return(NULL)
           })
  # Check open connections
  while(tryCatch(h2o.clusterIsUp(conn = h2o.getConnection()), error = function(cond){return(F)})){
    Sys.sleep(1)
  }
  # Initialize a connection
  localH2O = h2o.init(ip = "localhost", port = 54322,nthreads = -1,min_mem_size = '10M',max_mem_size = paste0(floor(as.integer(system("cat /proc/meminfo | head -n 1 | cut -d' ' -f9",intern = T))/(1024^2)) - 1,'G'))
  
  # Convert train and test datasets to h2o type
  train.h2o = as.h2o(train)
  test.h2o = as.h2o(test)
  
  # Define dependent and independent variables
  y.dep = which(names(train.h2o) == dependentColumn)
  x.indep = which(names(train.h2o) %in% indep.var)
  
  # Create model
  gc()
  print("#################################################")
  print("                   START  MODEL                  ")
  print("#################################################")
  switch(method,
         gbm = {
           model = h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = ntrees, max_depth = max_depth, learn_rate = learn_rate, seed = 112358, model_id = paste0('mod2_',method))
         },
         rf = {
           mtry = floor(sqrt(length(x.indep)))
           model = h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = ntrees, max_depth = max_depth, mtries = mtry, seed = 112358, model_id = paste0('mod2_',method))
         },
         svm = {
           model = svm(eval(parse(text = paste0(dependentColumn,'~',paste(indep.var,collapse = '+')))),data=train,scale = F,kernel = kernel,degree=degree,type = 'eps-regression')
         },
         ensemble = {
           learner = c("h2o.randomForest.wrapper", "h2o.gbm.wrapper")
           metalearner = wrapper
           model = h2o.ensemble(y = y.dep, x=x.indep, training_frame = train.h2o, family = family, learner = learner, metalearner = metalearner, cvControl = list(V = 5))
         }
  )
  print("#################################################")
  print("                    END MODEL                    ")
  print("#################################################")
  gc()
  
  # Get variable importance
  if(method!='svm'){
    varimp = as.data.table(h2o.varimp(model))
    varimp[,cpercentage:=cumsum(percentage)]
    
    # Predict for train and test
    predict.test = as.data.table(h2o.predict(model, test.h2o))
    names(predict.test) = outputColumnName
    predict.train = as.data.table(h2o.predict(model, train.h2o))
    names(predict.train) =  outputColumnName
    
    # Rescale
    real_tg_test = cbind(test[,.SD, .SDcols = dependentColumn],predict.test)
    real_tg_test = (real_tg_test*scale_info_enc$scales[which(scale_info_enc$name == dependentColumn)]) + scale_info_enc$centers[which(scale_info_enc$name == dependentColumn)]
    real_tg_train = cbind(train[,.SD, .SDcols = dependentColumn],predict.train)
    real_tg_train = (real_tg_train*scale_info_enc$scales[which(scale_info_enc$name == dependentColumn)]) + scale_info_enc$centers[which(scale_info_enc$name == dependentColumn)]
  }else{
    # 
    varimp = NULL
    predict.test = as.data.table(predict(model, test[,.SD,.SDcols = indep.var]))
    names(predict.test) = outputColumnName
    predict.train = as.data.table(predict(model, train[,.SD,.SDcols = indep.var]))
    names(predict.train) = outputColumnName
    
    # Rescale
    real_tg_test = cbind(test[,.SD, .SDcols = dependentColumn][complete.cases(test[,.SD,.SDcols = indep.var])],predict.test)
    real_tg_test = (real_tg_test*scale_info_enc$scales[which(scale_info_enc$name == dependentColumn)]) + scale_info_enc$centers[which(scale_info_enc$name == dependentColumn)]
    real_tg_train = cbind(train[,.SD, .SDcols = dependentColumn][complete.cases(train[,.SD,.SDcols = indep.var])],predict.train)
    real_tg_train = (real_tg_train*scale_info_enc$scales[which(scale_info_enc$name == dependentColumn)]) + scale_info_enc$centers[which(scale_info_enc$name == dependentColumn)]
  }
  
  # Correlation between target and real
  correlation = with(real_tg_test,cor(get(outputColumnName),get(dependentColumn)))
  
  # Root mean squared error
  rmse = with(real_tg_test,RMSE(get(outputColumnName),get(dependentColumn)))
  rmse_treino = with(real_tg_train,RMSE(get(outputColumnName),get(dependentColumn)))
  
  # Generalization measure and my own normalization
  generalization_measure = abs(rmse_treino - rmse)
  prop_gen = 100 - round(100*generalization_measure/max(rmse,rmse_treino),2)
  
  # Save model
  if(method!='svm'){
    h2o.saveModel(model, path = "./output/models/",force = T)
  }else{
    save(model,file= paste0("./output/models/mod2_",method,'.RData'))
  }
  # Clean h2o objects
  h2o.removeAll()
  # Close connection to h2o cluster
  h2o.shutdown(prompt = F)
  # Clean memory
  gc()
  # Return model info
  return(list(rmse = rmse, gen_measure = generalization_measure, prop_gen = prop_gen, var.importante = varimp, correlation = correlation))
}
