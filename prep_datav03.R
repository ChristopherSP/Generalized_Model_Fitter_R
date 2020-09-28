#################################################################################################
#                                      Preparing Data
#################################################################################################
prep_data = function(df){
  dt = copy(df)
  names(dt) = stri_replace_all_fixed(stri_trim_both(gsub(' +',' ',gsub('[^[:alnum:][:space:]_]','',tolower(stri_trans_general(names(dt),'latin-ascii')))))," ","_")
  # get scale information. usefull when splitting the data in train and test sets
  numeric_cols = unique(c(variables$dependentVariables$numerical, variables$independentVariables$ProcessDuration, variables$independentVariables$SentenceValue, variables$independentVariables$AgreementValue))
  
  categorical_cols = unique(c(variables$dependentVariables$categorical, variables$independentVariables$Sentence, variables$independentVariables$AgreementPropensity))
  
  dt = dt[,.SD,.SDcols = intersect(names(dt),c("pasta","status", numeric_cols, categorical_cols, unlist(variables$independentVariables)))]
  dt[,c(numeric_cols) := lapply(.SD,as.numeric),.SDcols = numeric_cols]
  dt[,c(categorical_cols) := lapply(.SD,as.factor),.SDcols = categorical_cols]
    
  ativos = dt[status == "Ativo"]
  encerrados = dt[status == "Encerrado"]
  encerrados = encerrados[sentenca %in% variables$filterLabels$Regression]
  
  scale_info_enc = scale_vals(encerrados, numeric_cols)
  
  # scale numeric columns
  # dt[, c(numeric_cols) := lapply(.SD, scale), .SDcols=numeric_cols]
  encerrados = scale_unseen_data(encerrados,info = scale_info_enc)
  ativos <<- scale_unseen_data(ativos,info = scale_info_enc)
  
  # eliminates outliers
  enc_outlier = outlier_detection(encerrados, numeric_cols, by="sentenca")
  idx_enc = enc_outlier$idx
  
  encerrados <<- enc_outlier$dt
  scale_info_enc <<- scale_info_enc
}
