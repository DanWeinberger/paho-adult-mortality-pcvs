#Subchapters from icd package :
#https://github.com/jackwasey/icd/blob/main/data/icd10_sub_chapters.rda
#https://github.com/jackwasey/icd/blob/main/data/chapters.R

#All chapter ranges
icd10_chapters <- list(
  "Certain infectious and parasitic diseases" = c(start = "A00", end = "B99"),
  "Neoplasms" = c(start = "C00", end = "D49"),
  "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" =
    c(start = "D50", end = "D89"),
  "Endocrine, nutritional and metabolic diseases" = c(start = "E00", end = "E89"),
  "Mental, Behavioral and Neurodevelopmental disorders" = c(start = "F01", end = "F99"),
  "Diseases of the nervous system" = c(start = "G00", end = "G99"),
  "Diseases of the eye and adnexa" = c(start = "H00", end = "H59"),
  "Diseases of the ear and mastoid process" = c(start = "H60", end = "H95"),
  "Diseases of the circulatory system" = c(start = "I00", end = "I99"),
  "Diseases of the respiratory system" = c(start = "J00", end = "J99"),
  "Diseases of the digestive system" = c(start = "K00", end = "K95"),
  "Diseases of the skin and subcutaneous tissue" = c(start = "L00", end = "L99"),
  "Diseases of the musculoskeletal system and connective tissue" = c(start = "M00", end = "M99"),
  "Diseases of the genitourinary system" = c(start = "N00", end = "N99"),
  "Pregnancy, childbirth and the puerperium" = c(start = "O00", end = "O9A"),
  "Certain conditions originating in the perinatal period" = c(start = "P00", end = "P96"),
  "Congenital malformations, deformations and chromosomal abnormalities" = c(start = "Q00", end = "Q99"),
  "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" =
    c(start = "R00", end = "R99"),
  "Injury, poisoning and certain other consequences of external causes" = c(start = "S00", end = "T88"),
  "External causes of morbidity" = c(start = "V00", end = "Y99"),
  "Factors influencing health status and contact with health services" = c(start = "Z00", end = "Z99")
)

#there are 2 functions here...assign subchapters is where most of the action happens; create_subchapters is a wrapper for this and allows it to work with dplyr
create_subchapters <- function(ds){
  icd10_sub_chapters <- readRDS( './Data/idc10_sub_chapters.rds')
  icd10_sub_chapters[[179]] <- NULL #094-09A
  
  icd10_sub_chapters <- c(icd10_sub_chapters,icd10_chapters)
  
  all.icd10.ranges <- sapply(icd10_sub_chapters,function(x){
    var.name <- paste(c(x[1], x[2]), collapse='_' )
    return(var.name)
  })
  names(all.icd10.ranges) <- NULL
  
  #Save as sparse vectors, then sparse matrix
 icd_code_vec.list <- pblapply(all.icd10.ranges,assign_subchapters,dx_3_digit=substr(ds$dx1,1,3),ds=ds,pneumo_code=ds$possible_pneumo_code)

  mat1 <- bind_rows(icd_code_vec.list)

  #TO DO, add these variables to the extraction program..probably manually aggregate these, then combined with the other time series for teh controls, thenreshapre
 # c('acm_noj_nodiarr_prim','acm_noj_prim','J12_J18_any','J13_prim','J00_J99_prim','J12_J18_prim','J09_J18_prim')

    
  mat1.c <- reshape2::dcast(mat1,agec+country+monthdate ~ variable, value.var='x')

 return(mat1.c)
  }



assign_subchapters <- function(icd10_sub_chapters_vector,ds,dx_3_digit, pneumo_code){

  dx.range <- str_split(icd10_sub_chapters_vector,'_')[[1]]
  var.name <- paste(dx.range, collapse='_')
  
  if(dx.range[1]!=dx.range[2]){
    icd_letter <- substr(dx.range,1,1)
    icd.num <- as.numeric(substring(dx.range,2))
    
    if(icd_letter[1]==icd_letter[2] & !is.na(icd.num[2]) &!is.nan(icd.num[2]) ){
            all.codes <- paste0(icd_letter[1],sprintf("%02d",icd.num[1]:icd.num[2])) #if whole chapter is within same letter
    } else if(!is.na(icd.num[2]) &!is.nan(icd.num[2])){
            all.codes1 <- paste0(icd_letter[1],sprintf("%02d",icd.num[1]:99) )
            all.codes2 <- paste0(icd_letter[2],sprintf("%02d",0:icd.num[2]) )
            all.codes <- c(all.codes1,all.codes2)
    }
  }else{
    icd.num <- as.numeric(substring(dx.range,2))
    all.codes <- icd.num[1]
  }
  
  
  ts <- ds %>%
    mutate(test_icd = 1*(dx_3_digit %in% all.codes & pneumo_code==0)) %>%
    group_by(agec, country,monthdate) %>%
    summarize(x=sum(test_icd)) %>%
    ungroup() %>%
    mutate(variable=var.name)
  
  # test_icd[pneumo_code==1] <- 0
  # test_icd <- test_icd*1
  
  # ts <-  aggregate(test_icd, by=list('agec'=ds$agec, 'country'=ds$country,'monthdate'=ds$monthdate), FUN=sum)
  # ts$variable <- var.name
#  sparse1 <- as(test_icd, "sparseVector")
  return(ts)
}

#dx1 <- c('A009', 'A01','A30','B127')
#dx1_ln3 <- substr(dx1,1,3)
#ds <- sapply(all.icd10.ranges,create_subchapters,dx_3_digit=dx1_ln3, simplify='array')



