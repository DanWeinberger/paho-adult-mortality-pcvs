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

names(test2) <- c('id','dod','dob','age','sex','muni','place_of_death','muni_code','icd2','icd3','icd4','icd5','icd6','icd1','age_type') #rename variables

test2 <- test2[,c('id','dod','dob','sex','muni','place_of_death','muni_code','icd1','icd2','icd3','icd4','icd5','icd6', 'age','age_type')]


saveRDS(test2,'./Data/Brazil/Mortality/all_deaths_compiled.rds')


return('Complete')
}