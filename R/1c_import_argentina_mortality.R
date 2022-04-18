a1 <- read.csv('./Data/Argentina/Mortalidad_Proc_25-11.csv')
View(a1[1:100,])

a2 <- a1[,c('X','EDAD','MESDEF', 'CODMUER')]
names(a2) <- c('id','agey','dod','icd1')

a2$dod <- as.Date(a2$dod)

a2$icd1 <- as.character(a2$icd1)
#range(a2$agey) #min age =5

a2$country <- 'AR'
saveRDS(a2,'./Data/Brazil/Mortality/all_deaths_compiled.rds')
