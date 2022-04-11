#Subchapters from icd package :
#https://github.com/jackwasey/icd/blob/main/data/icd10_sub_chapters.rda

# icd10_sub_chapters <- readRDS( './Data/idc10_sub_chapters.rds')
# icd10_sub_chapters[[179]] <- NULL #094-09A
# 
# all.icd10.ranges <- sapply(icd10_sub_chapters,function(x){
#   var.name <- paste(c(x[1], x[2]), collapse='_' )
#   return(var.name)
# })
# names(all.icd10.ranges) <- NULL

create_subchapters <- function(icd10_sub_chapters_vector,dx_3_digit){

  dx.range <- str_split(icd10_sub_chapters_vector,'_')[[1]]
  var.name <- paste(dx.range, collapse='_')
  
  if(dx.range[1]!=dx.range[2]){
    icd_letter <- substr(dx.range[1],1,1)
    icd.num <- as.numeric(substring(dx.range,2))
    all.codes <- paste0(icd_letter,sprintf("%02d",icd.num[1]:icd.num[2]))
  
  }else{
    icd.num <- as.numeric(substring(dx.range,2))
    all.codes <- icd.num[1]
  }
  
  test_icd <- dx_3_digit %in% all.codes
  
  test_icd[d$possible_pneumo_code==1] <- 0
  test_icd <- test_icd*1
 
  return(test_icd)
}

#dx1 <- c('A009', 'A01','A30','B127')
#dx1_ln3 <- substr(dx1,1,3)
#ds <- sapply(all.icd10.ranges,create_subchapters,dx_3_digit=dx1_ln3, simplify='array')
