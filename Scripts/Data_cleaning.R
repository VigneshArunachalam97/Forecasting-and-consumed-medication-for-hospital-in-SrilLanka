# import the required library # 
library(data.table)
library(readxl)
library(dplyr)

#### import required files ####
### set the working directory 
wd <- "C:/Users/n11142006/OneDrive - Queensland University of Technology/PhD/Nimmi_data/"
setwd(wd)
### To get a sheet names##
sheetname <- excel_sheets("data/Data_Hospital wise_18.01.2024combined_toVignesh_updated.xlsx")[1:35] ### #to get the sheet name and we will use to import the later
print(sheetname)

### import the files in loop ######
hospital_wide <- list()
for(i in sheetname) {
  data <- read_excel("data/Data_Hospital wise_18.01.2024combined_toVignesh_updated.xlsx", sheet = i)
  data <- data[1:57,]
  data[,3:ncol(data)] <- lapply(data[,3:ncol(data)], as.numeric)
  hospital_wide[[i]] <- as.data.frame(data)
}
saveRDS(hospital_wide, paste0("hospital_Wide_data_all_hospitals_unedited_", format(Sys.Date(), "%b_%d_%y"),".RDS")) ### Check point 1 - save the data in RDS object
##### remove the blank columns ##############
blank_columns <- list()
for(i in sheetname){
  blank <- colSums(is.na(hospital_wide[[i]])) == nrow(hospital_wide[[i]])  ### find the colnames which doesnt have any observation return true or false
  coln <- names(blank)[blank] ##### only subset the columns with no observation
  coln <- grep("\\.\\.\\.*.", coln, value = T) #### search for a given pattern and keep only column names
  blank_columns[[i]] <- coln  ### add it to our list
}
blank_columns
for(i in seq_along(sheetname)) {
  blank_col_names <- blank_columns[[i]]  ### Remove the unwanted columns from the hospital data and gives final output
  hospital_wide[[sheetname[i]]] <- hospital_wide[[sheetname[i]]][ , !(names(hospital_wide[[sheetname[i]]]) %in% blank_col_names)]
}

######### correct the column names accordingly ####
colnames(hospital_wide[[4]])[colnames(hospital_wide[[4]]) == "FA_H4_0070_2015"] <- "FA_H4_0080_2015"
colnames(hospital_wide[[15]])[colnames(hospital_wide[[15]]) == "FA15_H15_0230_2015"] <- "FA_H15_0230_2015"
hospital_wide[[22]]$FA_H22_0300_2015 <- NA
colnames(hospital_wide[[15]]) <- gsub("FA15", "FA", colnames(hospital_wide[[15]]))
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2016"] <- "FA_H31_2020_2016"
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2015"] <- "FA_H31_2020_2015"
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2020"] <- "FA_H31_2020_2020"
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2021"] <- "FA_H31_2020_2021"
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2022"] <- "FA_H31_2020_2022"
colnames(hospital_wide[[32]])[colnames(hospital_wide[[32]]) == "CS-H32_6010_2016"] <- "CS_H32_6010_2016"
colnames(hospital_wide[[34]])[colnames(hospital_wide[[34]]) == "FA_H34_J010_2022...63"] <- "FA_H34_J010_2022"
colnames(hospital_wide[[34]])[colnames(hospital_wide[[34]]) == "FA_H34_J010_2022...73"] <- "FA_H34_J010_2023"
colnames(hospital_wide[[4]])[colnames(hospital_wide[[4]]) == "FA_H3_0070_2019"] <- "FA_H4_0080_2019"
colnames(hospital_wide[[32]])[colnames(hospital_wide[[32]]) == "CS-H32_6010_2019"] <- "CS_H32_6010_2019"
colnames(hospital_wide[[32]])[colnames(hospital_wide[[32]]) == "CS-H32_6010_2022"] <- "CS_H32_6010_2022"
colnames(hospital_wide[[5]]) <- gsub("OS_H5_0080", "OS_H5_0090", colnames(hospital_wide[[5]]))
table(colnames(hospital_wide[[5]]) %in% os_names[[5]])
colnames(hospital_wide[[11]]) <- gsub("OS_H11_0200", "OS_H11_0190", colnames(hospital_wide[[11]]))
table(colnames(hospital_wide[[11]]) %in% os_names[[11]])
colnames(hospital_wide[[16]]) <- gsub("OS_H16_0250", "OS_H16_0240", colnames(hospital_wide[[16]]))
table(colnames(hospital_wide[[16]]) %in% os_names[[16]])
colnames(hospital_wide[[21]]) <- gsub("OS_H20_0290", "OS_H21_0290", colnames(hospital_wide[[21]]))
table(colnames(hospital_wide[[21]]) %in% os_names[[21]])
colnames(hospital_wide[[26]])[colnames(hospital_wide[[26]]) == "SH_DESC"] <- "Drug_Name"
table(colnames(hospital_wide[[26]]) %in% os_names[[26]])
colnames(hospital_wide[[29]]) <- gsub("OS_H29_OA20", "OS_H29_0A20", colnames(hospital_wide[[29]]))
table(colnames(hospital_wide[[29]]) %in% os_names[[29]])
colnames(hospital_wide[[3]])[colnames(hospital_wide[[3]]) == "%OS_H3_0070_2016...11"] <- "%OS_H3_0070_2016"
colnames(hospital_wide[[3]])[colnames(hospital_wide[[3]]) == "%OS_H3_0070_2016...21"] <- "%OS_H3_0070_2017"
colnames(hospital_wide[[6]])[colnames(hospital_wide[[6]]) == "O%S_H6_0100_2021"] <- "%OS_H6_0100_2021"
colnames(hospital_wide[[7]])[colnames(hospital_wide[[7]]) == "%OS_H7_0120_2018...31"] <- "%OS_H7_0120_2018"
colnames(hospital_wide[[7]])[colnames(hospital_wide[[7]]) == "%OS_H7_0120_2018...41"] <- "%OS_H7_0120_2019"
hospital_wide[[25]]$`%OS_H25_0330_2015` <- (hospital_wide[[25]]$OS_H25_0330_2015/12)*100
hospital_wide[[31]]$`%OS_H31_2020_2015` <- NA

saveRDS(hospital_wide, paste0("hospital_wide_edited_", format(Sys.Date(), "%b_%d_%y"),".RDS")) ### the RDS file for future use
hospital_wide <- readRDS("hospital_wide_edited_Feb_22_24.RDS")
names(hospital_wide)
######### load the required drugs and hos id data to subset ######
drugs_id <- read_xlsx("data/ids_to_plot.xlsx", sheet = "Drugs")
drugs_id
hos_id <- read_xlsx("data/ids_to_plot.xlsx", sheet = "Hospital")
hos_id$HID <- gsub("H19_0270", "H9_0170", hos_id$HID)
hos_id$new_id <- paste("H", 1:nrow(hos_id), sep = "")

##### subset #####
hospital_wide_subset <- hospital_wide[hos_id$HID] ### this will subset required hospital data ###
length(hospital_wide_subset)
hospital_wide_subset_final <- list()
for(i in 1:length(hospital_wide_subset)) {
  data <- as.data.frame(hospital_wide_subset [[i]])
  data <- subset(data, STOCK_CODE %in% drugs_id$STOCK_CODE)
  print(dim(data))
  hospital_wide_subset_final[[i]] <- as.data.table(data)
}
names(hospital_wide_subset_final) <- hos_id$HID
names(hospital_wide_subset_final)

colnames(hospital_wide_subset_final[[1]])[colnames(hospital_wide_subset_final[[1]]) == "DE_H2_0060_2020...53"] <- "DE_H2_0060_2020"
colnames(hospital_wide_subset_final[[1]])[colnames(hospital_wide_subset_final[[1]]) == "DE_H2_0060_2020...73"] <- "DE_H2_0060_2022"
colnames(hospital_wide_subset_final[[4]])[colnames(hospital_wide_subset_final[[4]]) == "FI_H5_0090_2021...62"] <- "FI_H5_0090_2021"
colnames(hospital_wide_subset_final[[4]])[colnames(hospital_wide_subset_final[[4]]) == "FI_H5_0090_2021...72"] <- "FI_H5_0090_2022"
colnames(hospital_wide_subset_final[[11]])[colnames(hospital_wide_subset_final[[11]]) == "%ADE_H41_0220_2017"] <- "%ADE_H14_0220_2017"
colnames(hospital_wide_subset_final[[11]])[colnames(hospital_wide_subset_final[[11]]) == "%FAC_H41_0220_2017"] <- "%FAC_H14_0220_2017"
hospital_wide_subset_final[[13]]$FI_H33_A010_2016 <- NA
hospital_wide_subset_final[[13]]$'%FAC_H33_A010_2016' <- NA


saveRDS(hospital_wide_subset_final, paste0("data/hospital_wide_edited_subset_final_", format(Sys.Date(), "%b_%d_%y"),".RDS"))  
### final saved object name is "hospital_wide_edited_subset_final_Mar_18_24.RDS"