import_chile_func <- function(){
  test1 <- read.csv('./Data/Chile/Mortality/DEF_2005_2018.csv')
  
  test1$dob <- as.Date(paste(test1$ano_nac,test1$mes_nac, test1$dia_nac, sep='-'))
  test1$dod <- as.Date(paste(test1$ano_def,test1$mes_def, test1$dia_def, sep='-'))
  test1$age <- test1$edad_cant
  test1$icd1 <- test1$diag1
  
  test1$age_type <- test1$edad_tipo
  test1 <- test1[c('dob','dod','age','icd1','age_type')]
  
  test1$country= 'CH'
  

  saveRDS(test1,'./Data/Chile/Mortality/all_deaths_compiled.rds')
  
  
  return('Complete')
}