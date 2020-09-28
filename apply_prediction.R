#########################
# Training model
#########################
info_mod2 = mod_m2(encerrados, indep.var = c(variables$dependentVariables$numerical,variables$dependentVariables$categorical), method = variables$methods$ProcessDuration, ntrees = 10)
save(info_mod2,file=paste0('./output/modelsInfo/mod2_',variables$methods$ProcessDuration,'_info','.RData'))

info_mod3 = mod_m3(encerrados, indep.var = c(variables$dependentVariables$numerical,variables$dependentVariables$categorical), method = variables$methods$Sentence, ntrees = 10)
save(info_mod3,file=paste0('./output/modelsInfo/mod3_',variables$methods$Sentence,'_info','.RData'))

info_mod4 = mod_m4(encerrados, indep.var = c(variables$dependentVariables$numerical,variables$dependentVariables$categorical), method = variables$methods$AgreementPropensity, ntrees = 10)
save(info_mod4,file=paste0('./output/modelsInfo/mod4_',variables$methods$AgreementPropensity,'_info','.RData'))

info_mod5 = mod_m5(encerrados, indep.var = c(variables$dependentVariables$numerical,variables$dependentVariables$categorical), method = variables$methods$SentenceValue, ntrees = 10)
save(info_mod5,file=paste0('./output/modelsInfo/mod5_',variables$methods$SentenceValue,'_info','.RData'))

info_mod6 = mod_m6(encerrados, indep.var = c(variables$dependentVariables$numerical,variables$dependentVariables$categorical), method = variables$methods$AgreementValue, ntrees = 10)
save(info_mod6,file=paste0('./output/modelsInfo/mod6_',variables$methods$AgreementValue,'_info','.RData'))

#########################
# Predict
#########################
pred = list()
modelo = 5
pred[[paste0('mod',modelo)]] = predicao(encerrados,modelo)
encerrados_output = cbind(encerrados,do.call(cbind,pred))

