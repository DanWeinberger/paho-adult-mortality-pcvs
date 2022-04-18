################################################################################
# PAHO Mortality Data                                                          #
#                                                                              #
#        DATE: December 2018                                                   #
#    CODED BY: Kayoko Shioda (kayoko.shioda@yale.edu)                          #
#     ADVISOR: Dan Weinberger (daniel.weinberger@yale.edu)                     #
################################################################################

categories_icd10 <- function(){
    #------------------------------------------------------------------------------#
    # DESCRIPTION
    #------------------------------------------------------------------------------#
    
    # 1. Create ICD-10 chapters and subchapters in the PAHO moratlity data
    # 2. Create monthdate and quarterdate
    
    #------------------------------------------------------------------------------#
    # SET UP
    #------------------------------------------------------------------------------#
    
    
    d <- read.csv('./Data/individual_level_data_combined.csv', as.is = T)
    
    
    #------------------------------------------------------------------------------#
    # ICD10 CODE
    #------------------------------------------------------------------------------#
    
    # Remove decimals and astariscs from ICD10 code
    d$dx1 <- gsub("[.]", "", d$dx1)
    d$dx2 <- gsub("[.]", "", d$dx2)
    d$dx3 <- gsub("[.]", "", d$dx3)
    d$dx4 <- gsub("[.]", "", d$dx4)
    d$dx5 <- gsub("[.]", "", d$dx5)
    d$dx6 <- gsub("[.]", "", d$dx6)
    d$dx1 <- gsub("[*]", "", d$dx1)
    d$dx2 <- gsub("[*]", "", d$dx2)
    d$dx3 <- gsub("[*]", "", d$dx3)
    d$dx4 <- gsub("[*]", "", d$dx4)
    d$dx5 <- gsub("[*]", "", d$dx5)
    d$dx6 <- gsub("[*]", "", d$dx6)
    #head(d[d$country=="br",])
    
    # Lower to upper case
    d$dx1 <- toupper(d$dx1)
    d$dx2 <- toupper(d$dx2)
    d$dx3 <- toupper(d$dx3)
    d$dx4 <- toupper(d$dx4)
    d$dx5 <- toupper(d$dx5)
    d$dx6 <- toupper(d$dx6)
    
    # J chapter
    #table(d$dx1[substr(d$dx1, 1, 1)=="J"])
    d$J09_J18_prim <- ifelse(substr(d$dx1, 1, 3) %in% c("J09","J10","J11","J12","J13","J14","J15","J16","J17","J18"), 1, 0)
    d$J12_J18_prim <- ifelse(substr(d$dx1, 1, 3) %in% c("J12","J13","J14","J15","J16","J17","J18"), 1, 0)
    d$J00_J99_prim <- ifelse(substr(d$dx1, 1, 1)=="J", 1, 0)
    d$J13_prim <- ifelse(substr(d$dx1, 1, 3)=="J13", 1, 0)
    
    # J12_J18_any: pneumococcal pneumonia or pneumonia with pneumococcus anywhere in Dxn
    d$J12_J18_any <- ifelse(apply(d[,c("dx1","dx2","dx3","dx4","dx5","dx6")], 1,
                                  function(x) sum(substr(x, 1, 3) %in% c("J12","J13","J14","J15","J16","J17","J18"))) > 0, 1, 0)
    
    # J13_any: pneumococcal pneumonia or pneumonia with pneumococcus anywhere in Dxn
    d$J13_any <- ifelse(apply(d[,c("dx1","dx2","dx3","dx4","dx5","dx6")], 1,
                              function(x) sum(substr(x, 1, 3) == "J13")) > 0, 1, 0)
    
    # possible_pneumo_code: any pneumo code anywhere
    d$possible_pneumo_code <- ifelse(d$J12_J18_any==1, 1, 0)
    d$possible_pneumo_code <- ifelse(apply(d[,c("dx1","dx2","dx3","dx4","dx5","dx6")], 1,
                                           function(x) sum(substr(x, 1, 3) %in% c("A40","A49","B953","R652","H10","H65",
                                                                                  "H66","G00","G01","G02","G03","G04"))) > 0, 
                                     1, d$possible_pneumo_code)
   # table(d$J12_J18_any)
  #  table(d$possible_pneumo_code)
    
    # acm_noj_prim
    d$acm_noj_prim <- ifelse(c(substr(d$dx1, 1, 1)=="J" | d$possible_pneumo_code==1), 0, 1)
   # table(d$acm_noj_prim)
    
    # acm_noj_nodiarr_prim <------------------- A00_A09 should also be excluded. Replace A08 with A00_A09.
    d$acm_noj_nodiarr_prim <- ifelse(c(substr(d$dx1, 1, 1)=="J" | d$possible_pneumo_code==1), 0, 1)
    d$acm_noj_nodiarr_prim <- ifelse(substr(d$dx1, 1, 3) =="A08", 0, d$acm_noj_nodiarr_prim)
    #table(noj = d$acm_noj_prim, noj_nodiarr = d$acm_noj_nodiarr_prim)
    length(which(substr(d$dx1, 1, 3) =="A08"))
    


    #d.ts <- 
   
    
    #Create the subchaptertime series
    #d.spl <- split(d, paste0(d$agec,d$country))
    
    # d.subs <- d %>%
    #   group_by(agec, country) %>%
    #   create_subchapters() %>%
    #   ungroup() 
    d.subs <- create_subchapters(ds=d)
      
    saveRDS(d.subs,'./Data/subchapter_control_time_series.rds')  
      
    #d2 <- cbind.data.frame(d, d.subs)
    # Save
    write.csv(d2, "./Data/PAHO_adults_ICD10reformatted_subchapters.csv", row.names = F)
}