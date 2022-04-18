# #Hi Dan,
# 
# #I believe the best alternative for Brazil and Chile is option 3, considering Date of Death – Date of birth.
# 
# #For Mexico they will not have the DOB variable (as they consider this personal identified and thus are not allowed to extract/share). But there age will have already be calculated prior to extraction so we should be good.
# 
# Cris
# 
# 
# 
# 
# From: Dan Weinberger [mailto:dweinber@gmail.com]
# Sent: domingo, 10 de abril de 2022 21:46
# To: Cristiana Toscano <ctoscano@terra.com.br>
#   Subject: Age variables
# 
# 
# 
# Hi Cris,
# 
# Can you confirm which age variable we should use for Brazil and Chile? 
#   
#   We can 
# 
# 1) use the edad variable itself
# 
# 2) use edad combined with edad tipo
# 
# 3) calculate manually from DOB and date of death
# 
# 
# 
# I was trying #2, but it seems the edad_tipo variable is maybe not using the same definition for Brazil and Chile (it seem maybe right for Brazil, though there is a category 1 and 9, which I wasn't sure what these are. 9=missing? 1= hours?; for Chile, most people have edad_tipo=1
# 
# 
# 
# Dan
import_chile_func <- function(){
  test1 <- read.csv('./Data/Chile/Mortality/DEF_2005_2018.csv')
  
  test1$dob <- as.Date(paste(test1$ano_nac,test1$mes_nac, test1$dia_nac, sep='-'))
  test1$dod <- as.Date(paste(test1$ano_def,test1$mes_def, test1$dia_def, sep='-'))
  test1$agey <- round(as.numeric(test1$dod - test1$dob)/365)
  test1$icd1 <- test1$diag1
  
  test1 <- test1[c('dob','dod','agey','icd1')]
  
  test1$country= 'CH'
  

  saveRDS(test1,'./Data/cleaned_country_mortality_files/ch.rds')
  
  
  return('Complete')
}