combine_data_countries <- function(){
  a1 <- readRDS('./Data/Brazil/Mortality/all_deaths_compiled.rds')
  a1$icd1 <- as.character(a1$icd1)
  a1$icd2 <- as.character(a1$icd2)
  a1$icd3 <- as.character(a1$icd3)
  a1$icd4 <- as.character(a1$icd4)
  a1$icd5 <- as.character(a1$icd5)
  a1$icd6 <- as.character(a1$icd6)
  
  
    a1$age_type <- as.character(a1$age_type)
  
  b1 <- readRDS('./Data/Chile/Mortality/all_deaths_compiled.rds')
  b1$age_type <- as.character(b1$age_type)
  b1$age <- as.character(b1$age)
  #c1 <- readRDS('./Data/Argentina/Mortality/all_deaths_compiled.rds')
  
  #combine across countries
  a2 <- bind_rows(a1[c('dob','dod','age','age_type','icd1','icd2','icd3','icd4','icd5','icd6','country')],b1) #, c1)
  names(a2) <- c('dob','dod','age','age_type','dx1','dx2','dx3','dx4','dx5','dx6','country')
  
  a2$age <- as.numeric(as.character(a2$age))
  
  #table(a2$age_type) #What is age_type 1 or 9?
  
  a2$agey <- NA 
  a2$agey[a2$age_type=='4' & !is.na(a2$age)]  <- a2$age[a2$age_type=='4'& !is.na(a2$age)]
  a2$agey[a2$age_type=='1' & !is.na(a2$age)]  <- a2$age[a2$age_type=='1'& !is.na(a2$age)]/(24*365)
  
  a2$agey[a2$age_type=='2' & !is.na(a2$age)]  <- a2$age[a2$age_type=='2'& !is.na(a2$age)]/365
  a2$agey[a2$age_type=='3' & !is.na(a2$age)]  <- a2$age[a2$age_type=='3'& !is.na(a2$age)]/12
  a2$agey[a2$age_type=='5' & !is.na(a2$age)]  <- a2$age[a2$age_type=='5'& !is.na(a2$age)]+100
  
  # hist(a2$agey[a1$country=='BR'])
  # hist(a2$agey[a1$country=='CH'])
  
  #sum(is.na(a2$agey))/nrow(a2)*100 #What percent of deaths missing an age classification?
  
  a2$agec <- NA
  a2$agec[a2$agey>=0 & a2$agey<2] <-   1 
  a2$agec[a2$agey>=2 & a2$agey<5] <-   2 
  a2$agec[a2$agey>=5 & a2$agey<18] <-  3 
  a2$agec[a2$agey>=18 & a2$agey<40] <- 4 
  a2$agec[a2$agey>=40 & a2$agey<65] <- 5 
  a2$agec[a2$agey>=65 & a2$agey<80] <- 6 
  a2$agec[a2$agey>=80 & a2$agey<150]<- 7 
  
  a2$monthdate <- floor_date(a2$dod, 'month')  
  write.csv(a2,'./Data/individual_level_data_combined.csv')
  return('Done')
}

