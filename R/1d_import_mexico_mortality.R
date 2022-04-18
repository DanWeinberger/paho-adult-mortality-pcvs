
file_list <- list.files(path="./Data/Mexico/Deaths_MEX_1999-2019", pattern='*.csv')

b1 <- pblapply(file_list, function(x){
  a1 <- read.csv(paste0("./Data/Mexico/Deaths_MEX_1999-2019",'/',x))
  
  a1$dod <- as.Date(paste(a1$year_death, a1$month_death, a1$day_death, sep='-'), '%Y-%m-%d')
  
  a2 <- a1[,c('dod','age_created','death_cause' )]
  
  names(a2) <- c('dod','agey','icd1')
  
  a2$country <- 'MX'
  
  return(a2)
})

b1 <- bind_rows(b1)

b1$agey[b1$agey>115] <- NA
saveRDS(b1,'./Data/cleaned_country_mortality_files/mx.rds')
