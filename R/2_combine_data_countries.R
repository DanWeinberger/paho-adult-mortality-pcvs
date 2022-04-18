combine_data_countries <- function(){
  a1 <- readRDS('./Data/cleaned_country_mortality_files/br.rds')
  a1$icd1 <- as.character(a1$icd1)
  a1$icd2 <- as.character(a1$icd2)
  a1$icd3 <- as.character(a1$icd3)
  a1$icd4 <- as.character(a1$icd4)
  a1$icd5 <- as.character(a1$icd5)
  a1$icd6 <- as.character(a1$icd6)
  
  b1 <- readRDS('./Data/cleaned_country_mortality_files/ch.rds')
  #c1 <- readRDS('./Data/Argentina/Mortality/all_deaths_compiled.rds')
  
  c1 <- readRDS('./Data/cleaned_country_mortality_files/ar.rds')
  
  d1 <- readRDS('./Data/cleaned_country_mortality_files/mx.rds')
  
  #combine across countries
  a2 <- bind_rows(a1[c('dob','dod','agey','icd1','icd2','icd3','icd4','icd5','icd6','country')],b1, c1, d1) #, c1)
  names(a2) <- c('dob','dod','agey','dx1','dx2','dx3','dx4','dx5','dx6','country')
  
  a2$agec <- NA
  a2$agec[a2$agey>=0 & a2$agey<2] <-   1 
  a2$agec[a2$agey>=2 & a2$agey<5] <-   2 
  a2$agec[a2$agey>=5 & a2$agey<18] <-  3 
  a2$agec[a2$agey>=18 & a2$agey<40] <- 4 
  a2$agec[a2$agey>=40 & a2$agey<65] <- 5 
  a2$agec[a2$agey>=65 & a2$agey<80] <- 6 
  a2$agec[a2$agey>=80 & a2$agey<150]<- 7 
  
  a2$monthdate <- floor_date(a2$dod, 'month')  
  saveRDS(a2,'./Data/cleaned_country_mortality_files/individual_level_data_combined.rds')
  return('Done')
}

