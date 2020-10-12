change_vartype = function(dt,cols,type,format = '%Y-%m-%d'){
  switch(type,
         factor = {
           change = 'as.factor(x)'
         },
         date = {
           change = paste0('as.Date(x,',format,')')
         },
         numeric = {
           change = paste0('as.numeric(x)')
         },
         integer = {
           change = 'as.integer(x)'
         },
         boolean = {
           change = 'as.boolean(x)'
         }
         )
  if(class(cols) == 'integer'){
    dt[ , names(dt)[cols] :=lapply(.SD, function(x) x = eval(parse(text = change))) , .SDcols=cols] 
  }else if(class(cols) == 'character'){
    dt[, cols := lapply(.SD,as.numeric), .SDcols = cols]
  }else{
    stop('Error:\nColumns vector must be either integer or character')
  }
}

outlier_detection = function(df,cols,by = c(),method = c('euclidian','mahalanobis'),conf_level = 0.975){
  dt = copy(df)
  method = match.arg(method)
  if(method == 'mahalanobis'){
    m=ncol(dt[,.SD,.SDcols=cols])
    dt[,mahal:=mahalanobis(.SD, colMeans(.SD), cov(.SD)),.SDcols=cols,by=by]
    outlier_threshold = qchisq(conf_level,m)
    dt = dt[mahal<=outlier_threshold]
    dt[,mahal:=NULL]
  }
  if(method == 'euclidian'){
    centroid = dt[,lapply(.SD,mean,na.rm=T),.SDcols=cols,by=eval(parse(text = paste0('list(',paste(by,sep=",",collapse = ","),')')))]
    if(length(by)!=0){
      centroid_split = split(centroid,by=by)
      dt_split = split(dt,by=by)
    }else{
      centroid_split = list()
      dt_split = list()
      centroid_split$geral = centroid
      dt_split$geral = dt
    }
    
    distances = mapply(function(x,y) as.data.table(rdist(x[,.SD,.SDcols = cols],y[,.SD,.SDcols = as.integer(length(by) + 1):ncol(y)])),dt_split,centroid_split,SIMPLIFY = F)
    distances = rbindlist(Map(cbind, distances, group = names(distances)))
    if(length(by)!=0){
      distances[,c(by) := tstrsplit(group, ".", fixed=TRUE)]
      distances[,group:=NULL]
    }
    names(distances)[1] = 'distance'
    distances[,threshold := quantile(distance,conf_level,na.rm=T),by=by]
    idx = distances$distance <= distances$threshold
    dt = dt[idx]
  }
  return(list(dt=dt,idx=idx))
}

group_classes = function(df,cols,method=c('frequence','proportion','ht','tukey'),max_levels = 200, explained_proportion = 0.90, explained_diff = 0.1){
  dt = copy(df)
  method = match.arg(method)
  if(method=='proportion'){
    count_values = lapply(cols,function(x)dt[,.(.N,total=nrow(dt)),by=x])
    lapply(count_values, function(x) x[,prop := N/total])
    lapply(count_values, function(x) setorder(x,-prop))
    lapply(count_values, function(x) x[,cumprop:= cumsum(prop)])
    lapply(count_values, function(x) x[,cumprop_diff := cumprop - shift(cumprop,1)])
    lapply(count_values, function(x) x[is.na(cumprop_diff), cumprop_diff := 1])
    lapply(count_values, function(x) x[,c(paste0('agg_',names(x)[1])) := x[,1,with=F]])
    lapply(count_values, function(x) x[nrow(x) > max_levels & cumprop > explained_proportion & cumprop_diff < explained_diff, c(paste0('agg_',names(x)[1])) := 'Outros'])
    lapply(count_values, function(x) x[,variable := names(x)[1]])
    
    count_values = rbindlist(count_values)
    names(count_values)[1] = 'level'
    names(count_values)[ncol(count_values)-1] = 'agg_level'
    count_values = count_values[,c(1,ncol(count_values)-1,ncol(count_values)),with=F]
    dt[,id:= eval(parse(text = paste('paste0(',paste(cols2,sep = '',collapse = ','),')')))]
    dt = melt(dt,measure.vars = cols2, value.name = 'level')
    dt = merge(dt,count_values,all.x=T)
    dt[,level:=NULL]
    dt = dcast.data.table(dt,...~variable,value.var = 'agg_level')
  }
  if(method=='frequence'){
    count_values = lapply(cols,function(x)dt[,.(.N),by=x])
    lapply(count_values, function(x) setorder(x,-N))
    count_values = lapply(count_values, function(x) x[!is.na(names(x)[1])])
    count_values = lapply(count_values, function(x) if(nrow(x) > max_levels)x[1:max_levels]else x[1:.N])
    lapply(count_values, function(x) x[,N:=NULL])
    lapply(count_values, function(x) x[,c(paste0('agg_',names(x)[1])) := x[,1,with=F]])
    
    lapply(count_values, function(x) dt <<-merge(dt,x,all.x = T,by=names(x)[1]))
    dt = cbind(dt[,-paste0('agg_',cols),with=F],dt[,lapply(.SD,function(x){ifelse(is.na(x),'Outros',x)}),.SDcols = paste0('agg_',cols)])
  }
  if(method=='tukey'){
    mod = aov(resp~.,data=dt)
  }
  return(dt)
}

scale_vals = function(df,cols){
  dt = copy(df)
  name = cols
  centers = dt[ , colMeans(.SD,na.rm=T), .SDcols=cols]
  scales = as.numeric(dt[ , lapply(.SD, sd, na.rm = T), .SDcols=cols])
  names(scales) = cols
  return(list(name = name, centers = centers, scales=scales))
}

scale_unseen_data = function(df,info){
  dt = copy(df)
  
  lapply(info$name,function(x) dt[,names(dt)[which(names(dt) == x)] := lapply(.SD,function(y) (y-info$centers[info$name == x])/info$scales[info$name == x]),.SDcols = which(names(dt) == x)])
  
  return(dt)
}

unscale_vals = function(df,info){
  dt = copy(df)
  
  lapply(info$name,function(x) dt[,names(dt)[which(names(dt) == x)] := lapply(.SD,function(y) (y*info$scales[info$name == x]) + info$centers[info$name == x]),.SDcols = which(names(dt) == x)])
  
  return(dt)
}

split_scale = function(dt,cols,proportion = 0.8){
  n = nrow(dt)
  idx_train = sample(1:n,dt,as.integer(round(proportion*n,0)))
  train = dt[idx_train]
  test = dt[-idx_test]
  if(class(cols) %in% c('integer','character')){
    center_train = train[ , colMeans(.SD), .SDcols=cols]
    scale_train = as.numeric(train[ , lapply(.SD, sd), .SDcols=cols])
  }else{
    stop('Error:\nColumns vector must be either integer or character')
  }
  train[,,.SD]
}

proportion = function(x,n){
  round(100*x/n,2)
}

count_na = function(dt,by=c()){
  count = melt(dt[, lapply(.SD, function(x) sum(is.na(x)))],measure.vars = names(dt),variable.name = 'variable',value.name = 'count_na')
  setorder(count,-count_na)
  count[,p:=proportion(count_na,nrow(dt))]
  count[,variable:=as.character(variable)]
  return(count)
}

count_levels = function(dt,by=c()){
  count = melt(dt[, lapply(.SD, function(x) length(unique(x)))],measure.vars = names(dt),variable.name = 'variable',value.name = 'count_levels')
  setorder(count,-count_levels)
  count[,variable:=as.character(variable)]
  return(count)
}

get_class = function(dt){
  classes = melt(dt[, lapply(.SD, class)],measure.vars = names(dt),variable.name = 'variable',value.name = 'type')
  setorder(classes,type)
  classes[,variable:=as.character(variable)]
  return(classes)
}

get_example = function(dt,by=c()){
  example = melt(dt[, lapply(.SD, function(x) sort(unique(x), decreasing = T, na.last = T)[1])],measure.vars = names(dt),variable.name = 'variable',value.name = 'example')
  setorder(example,variable)
  example[,variable:=as.character(variable)]
  return(example)
}

non_char_summary = function(dt){
  classes = get_class(dt)
  numericalCols = classes[grepl("integer|numeric",type)]$variable
  dateCols = classes[grepl("Date|POSIX",type)]$variable
  
  if(length(numericalCols) != 0){
    numericalSummary = dt[, lapply(.SD, function(x) c(min(x, na.rm = T), quantile(x, 0.25, na.rm = T), quantile(x, 0.5, na.rm = T), mean(x, na.rm = T), quantile(x, 0.75, na.rm = T), max(x, na.rm = T))), .SDcols = numericalCols]
    numericalSummary = round(numericalSummary,2)
  }
  if(length(dateCols) != 0){
    dateSummary = dt[, lapply(.SD, function(x) c(min(x, na.rm = T), quantile(x, 0.25, na.rm = T, type = 1), quantile(x, 0.5, na.rm = T, type = 1), mean(x, na.rm = T), quantile(x, 0.75, na.rm = T, type = 1), max(x, na.rm = T))), .SDcols = dateCols]
  }
  
    if(any(grepl("numericalSummary", ls())) & any(grepl("dateSummary", ls()))){
      valuesSummary = cbind(numericalSummary, dateSummary)
    } else if (any(grepl("numericalSummary", ls()))){
      valuesSummary = numericalSummary
    } else if (any(grepl("dateSummary", ls()))){
      valuesSummary = dateSummary
    } else {
      valuesSummary = NULL
    }
    # valuesSummary = cbind(numericalSummary, dateSummary)
    valuesSummary[, names(valuesSummary) := lapply(.SD, as.character), .SDcols = names(valuesSummary)]
    metricNames = c("Min","1st Quantile","Median","Mean","3rd Quantile","Max")
    
    valuesSummary[, metric := metricNames]
    valuesSummary = melt.data.table(valuesSummary, id.vars = "metric")
    valuesSummary = dcast.data.table(valuesSummary, variable~metric)
    setcolorder(valuesSummary, c("variable",metricNames))
  
  return(valuesSummary)
}

hour2decimal = function(hour){
  splited = strsplit(hour,":",T)
  splited = lapply(splited, as.numeric)
  splited = lapply(splited, function(x) x[1] + x[2]/60)
  return(unlist(splited))
}

remove_string_garbage = function(string){
  if(grepl("[[:alnum:]]",string)){
    stri_trim_both(stri_replace_all_regex(string,"\\s\\s+","\\s"))
  } else {
    stri_trim_both(stri_replace_all_regex(stri_replace_all_regex(string,"[[:punct:]]",""),"\\s\\s+","\\s"))
  }
}

integral = function(x,y){
  # library(zoo)
  id = order(x)
  auc = sum(diff(x[id])*rollmean(y[id],2))
  return(auc)
}

clean_name = function(string) {
  return(stri_trim_both(stri_trans_general(stri_replace_all_regex(string,"[[:punct:]\\s]",''), "lower; latin-ascii")))
}
