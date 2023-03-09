import_colombia_func <- function(){
 # data_old <- read.csv("./raw_data_Colombia/DEF_COL_2001-2004_OPEN_SOURCE.csv")
  file_list <- list.files(path="./raw_data_Colombia/", pattern='*.csv')
  
  test1 <- 
    all.res <-
    pblapply(file_list, function(x){
      print(x)
      path1 <- paste0("./raw_data_Colombia/",x)
      ds1 <- read.csv(path1)
      ds2 <- ds1[,c("death_date","age","immediate_cause")]
      names(ds2) <- c('dod','agey','icd1')
      return(ds2)
    })
  
  test2 <- bind_rows(test1) #combine all years
  test2$dod <- as.Date(test2$dod,'%Y-%m-%d')
  test2$country <- 'COL'
  
  saveRDS(test2,'./Data/cleaned_country_mortality_files/col.rds')
  return('Complete')
}