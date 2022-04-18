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



import_brazil_func <- function(){
file_list <- list.files(path="./Data/Brazil/Mortality", pattern='*.dbf')

test1 <- 
  all.res <-
  pblapply(file_list, function(x){
    print(x)
    path1 <- paste0("./Data/Brazil/Mortality/",x)
    ds <- read.dbf(path1)
    return(ds)
  })

test2 <- bind_rows(test1) #combine all years

names(test2) <- c('id','dod','dob','age','sex','muni','place_of_death','muni_code','icd2','icd3','icd4','icd5','icd6','icd1') #rename variables

test2$agey <- round(as.numeric(test2$dod - test2$dob)/365) #overwrite age variable


test2 <- test2[,c('id','dod','dob','icd1','icd2','icd3','icd4','icd5','icd6', 'agey')]


test2$country <- 'BR'

saveRDS(test2,'./Data/cleaned_country_mortality_files/br.rds')


return('Complete')
}