predicao = function(df,modelo = c(1:5),best_threshold = NULL){
  dt = copy(df)
  
  model_function = c('mod1','mod2','mod3','mod4','mod5','mod6')
  
  # read savedmodels
  model_ids = list.files('./output/models/')
  
  tryCatch(h2o.shutdown(prompt = F),
           error = function(cond){
             return(NULL)
           })
  # Check open connections
  while(tryCatch(h2o.clusterIsUp(conn = h2o.getConnection()), error = function(cond){return(F)})){
    Sys.sleep(1)
  }
  # Initialize H2O connection
  localH2O = h2o.init(ip = "localhost", port = 54322,nthreads = -1,min_mem_size = '10M',max_mem_size = paste0(floor(as.integer(system("cat /proc/meminfo | head -n 1 | cut -d' ' -f9",intern = T))/(1024^2)) - 1,'G'))
  
  # Load model
  model = h2o.loadModel(paste0('./output/models/',model_ids[grepl(model_function[modelo],model_ids)]))
  # Load model info
  load(paste0('./output/modelsInfo/',model@parameters$model_id,'_info.RData'))
  info = eval(parse(text = paste0('info_',model_function[modelo])))  
  
  # Scale data to apply model
  dt = scale_unseen_data(dt,scale_info_enc)
  dt = as.h2o(dt)
  
  # Use model to predict
  pred = as.data.table(h2o.predict(model,newdata = dt))
  

  # Change dataframe colnames for a better output and unscale data
  if(modelo%in%c(2,5,6)){
    names(pred) = variables$outputColumnNames[[modelo]]
    pred[,names(pred)[1] := (.SD*scale_info_enc$scales[scale_info_enc$name == model@parameters$y]) + scale_info_enc$centers[scale_info_enc$name == model@parameters$y], .SDcols = names(pred)[1]]
  } else {
    best_threshold = info[grepl('best_threshold',names(info))][[1]]
    prob_name = variables$probabilityClass[[names(variables$independentVariables[3])]]
    prob_col = which(names(pred) == prob_name)
    pred[,predict := ifelse(eval(parse(text = prob_name))>=best_threshold,prob_name,names(pred)[!names(pred)%in%c(prob_name,'predict')])]
    names(pred) = c(paste0('predicted_m',modelo,"_",model@parameters$y),paste0('prob_m',modelo,'_',names(pred)[2]),paste0('prob_m',modelo,'_',names(pred)[3]))
  }
  
  h2o.removeAll()
  h2o.shutdown(prompt = F)
  gc()
  return(pred)
}
