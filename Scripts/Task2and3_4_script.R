# import the required library # 
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(openxlsx)

### set the working directory 
wd <- "C:/Users/n11142006/OneDrive - Queensland University of Technology/PhD/Nimmi_data/"
setwd(wd)

### Import the RDS object saved in the Task 1 ####
#rdfile <- "hospital_Wide_data_all_hospitals_updated_Feb_22_24.RDS"
#rdfile <- "hospital_wide_edited_subset_final_Feb_25_24.RDS"
rdfile <- "hospital_wide_edited_subset_final_Mar_02_24.RDS"
hospital_wide <- readRDS(rdfile)
length(hospital_wide)
names(hospital_wide)

###### check the summary of OS ###
#### First create a variable names for the os based on the pattern ####
hospital_id <- names(hospital_wide) ### hospital name #####
years <- seq(2015, 2022, 1) ### years data we have

#### get os percentage column from the each hospital data
os_names_per <- list()
for(i in hospital_id){
  #col <- paste("OS_",i, "_", years, sep ="")
  #col <- c("Drug_Name", col)
  per <- paste("%OS_",i, "_", years, sep ="")
  per <- c("Drug_Name", per)
  os_names_per[[i]] <- per
}

##### get os column name from the each hospital data ###
os_names <- list()
for(i in hospital_id){
  col <- paste("OS_",i, "_", years, sep ="")
  col <- c("Drug_Name", col)
  #per <- paste("%OS_",i, "_", years, sep ="")
  os_names[[i]] <- col
}


# # ######## Change the incorrect names according to each hospital data ####
# # colnames(hospital_wide[[5]]) <- gsub("OS_H5_0080", "OS_H5_0090", colnames(hospital_wide[[5]]))
# # table(colnames(hospital_wide[[5]]) %in% os_names[[5]])
# # colnames(hospital_wide[[11]]) <- gsub("OS_H11_0200", "OS_H11_0190", colnames(hospital_wide[[11]]))
# # table(colnames(hospital_wide[[11]]) %in% os_names[[11]])
# # colnames(hospital_wide[[16]]) <- gsub("OS_H16_0250", "OS_H16_0240", colnames(hospital_wide[[16]]))
# # table(colnames(hospital_wide[[16]]) %in% os_names[[16]])
# # colnames(hospital_wide[[21]]) <- gsub("OS_H20_0290", "OS_H21_0290", colnames(hospital_wide[[21]]))
# # table(colnames(hospital_wide[[21]]) %in% os_names[[21]])
# # colnames(hospital_wide[[26]])[colnames(hospital_wide[[26]]) == "SH_DESC"] <- "Drug_Name"
# # table(colnames(hospital_wide[[26]]) %in% os_names[[26]])
# # colnames(hospital_wide[[29]]) <- gsub("OS_H29_OA20", "OS_H29_0A20", colnames(hospital_wide[[29]]))
# # table(colnames(hospital_wide[[29]]) %in% os_names[[29]])
# # colnames(hospital_wide[[3]])[colnames(hospital_wide[[3]]) == "%OS_H3_0070_2016...11"] <- "%OS_H3_0070_2016"
# # colnames(hospital_wide[[3]])[colnames(hospital_wide[[3]]) == "%OS_H3_0070_2016...21"] <- "%OS_H3_0070_2017"
# # colnames(hospital_wide[[6]])[colnames(hospital_wide[[6]]) == "O%S_H6_0100_2021"] <- "%OS_H6_0100_2021"
# # colnames(hospital_wide[[7]])[colnames(hospital_wide[[7]]) == "%OS_H7_0120_2018...31"] <- "%OS_H7_0120_2018"
# # colnames(hospital_wide[[7]])[colnames(hospital_wide[[7]]) == "%OS_H7_0120_2018...41"] <- "%OS_H7_0120_2019"
# # hospital_wide[[25]]$`%OS_H25_0330_2015` <- (hospital_wide[[25]]$OS_H25_0330_2015/12)*100
# # hospital_wide[[31]]$`%OS_H31_2020_2015` <- NA
# # 
# saveRDS(hospital_wide, paste0("hospital_wide_edited_", format(Sys.Date(), "%b_%d_%y"),".RDS")) ### the RDS file for future use
# hospital_wide <- readRDS("hospital_wide_edited_Feb_22_24.RDS")

#### get OS column from the data for this task ####
os_dataset <- list()
for(i in hospital_id){
  hospital_wide[[i]] <- data.table::as.data.table(hospital_wide[[i]])
  data <- subset(hospital_wide[[i]], select = os_names[[i]])
  colnames(data) <- gsub("OS_", "", colnames(data))
  print(dim(data))
  os_dataset[[i]] <- data
  #os_dataset[[i]] <- hospital_wide[[i]][,os_names[[i]]]
}

#### Combine the data for each plot ####

complete_os <- os_dataset[[1]]
for(i in 2:length(os_dataset)){
  complete_os <- merge(complete_os, os_dataset[[i]], by = "Drug_Name", all = TRUE)
  print(dim(complete_os))
}

write.table(complete_os, paste0("os_data_all_hospital_", format(Sys.Date(), "%b_%d_%y"),".tsv"), quote = F, row.names = F, sep = "\t") ### save the os_data_future

######## subset by medicine and merge it #####
drug_dataframes <- vector("list", length = 31)
drugs <- os_dataset[[1]]$Drug_Name
drug_order <- paste("drugs_", 1:31, sep = "")
ord <- data.frame(sheetname = drug_order, Drugs = drugs)
ord$STOCK_cODE <- merge(ord, hospital_wide[[1]], by.y = "Drug_Name", by.x = "Drugs")$STOCK_CODE
write.table(ord, "Output/druglist_order.tsv", sep = "\t", row.names = F, quote = F)
#writeLines(drugs, "Output/druglist_order.txt")
# Remove dosage, "Tab.", and "Cap." from drug names
names(drug_dataframes) <- os_dataset[[1]]$Drug_Name
# Iterate over each drug
for (drug_index in 1:31) {
  drug_data <- data.frame()
  for (df_index in 1:length(os_dataset)) {
    colnames(os_dataset[[df_index]]) <- c("Drug_Name", paste("Y1_", seq(2015, 2022,1), sep = ""))
    drug_data_df <- os_dataset[[df_index]][drug_index, ]
    drug_data <- rbind(drug_data, drug_data_df)
  }
  drug_data <- drug_data[,-1]
  drug_data$HID <- hospital_id
  drug_data <- drug_data[,c("HID", paste("Y1_", seq(2015, 2022,1), sep = ""))]
  drug_data <- as.data.frame(drug_data)
  rownames(drug_data) <- hospital_id
  drug_data <- drug_data[,-1]
  drug_dataframes[[drug_index]] <- drug_data
}

########## calculate the average and other measures ######
addition <- c("Minimum", "Maximum", "Average", "StandardDeviation", "Quartile1", "Median", "Quartile3")
wb <- createWorkbook()
calcuated_drug_dataframes <- list()
for(i in 1:length(drug_dataframes)){
  data <- drug_dataframes[[i]]
  data <- as.data.frame(t(data))
  data$Years <- rownames(data)
  data <- as.data.table(data[,c(14, 1:13)])
  data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
  data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,15)]]
  data[,Average:= round(rowSums(.SD, na.rm = TRUE)/13, 2), .SDcols = names(data)[-c(1,15,16)]]
  data[,StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17)]]
  data[,Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17,18)]]
  data[,Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19)]]
  data[,Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19,20)]]
  data <- as.data.frame(data)
  rownames(data) <- data$Years
  data <- data[,-1]
  data <- as.data.frame(t(data))
  data$HID <- rownames(data)
  data <- data[,c(9, 1:8)]
  data <- as.data.table(data)
  data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
  data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,10)]]
  data[,Average:= round(rowSums(.SD, na.rm = TRUE)/8, 2), .SDcols = names(data)[-c(1,10,11)]]
  data[,StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,10,11,12)]]
  data[,Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13)]]
  data[,Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13, 14)]]
  data[,Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13, 14, 15)]]
  data[14:20, 10:16] <- NA
  data[, c(paste0("Y1_per_", 2015:2022)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2015:2022)]
  data[14:20, 17:24] <- NA
  colo <- colnames(data)[-c(1, 10:16)]
  ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
  data <- as.data.frame(data)
  data <- data[,ordered]
  calcuated_drug_dataframes[[i]] <- data
  addWorksheet(wb = wb, sheetName = paste0("drugs_", i))
  writeData(wb = wb, sheet = i, x = calcuated_drug_dataframes[[i]])
}
tail(calcuated_drug_dataframes[[1]], 9)

# try <- calcuated_drug_dataframes[[1]]
# try[, c(paste0("Y1_per_", 2015:2022)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2015:2022)]
# try[14:20, 17:24] <- NA
# colo <- colnames(try)[-c(1, 10:16)]
# ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
# try <- as.data.frame(try)
# try <- try[,ordered]

saveWorkbook(wb, "Output/Summary_drug_Wise_all_Year_and_hospitals_subset_Mar_2_24.xlsx", overwrite = TRUE)

########## subset the data form 2015 to 2019 ##### period 1 ###
period1 <- createWorkbook()
for(i in 1:length(drug_dataframes)) {
  data <- drug_dataframes[[i]]
  data <- data[,paste("Y1_", 2015:2019, sep = "")]
  data <- as.data.frame(t(data))
  data$Years <- rownames(data)
  data <- as.data.table(data[,c(14, 1:13)])
  data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
  data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,15)]]
  data[,Average:= round(rowSums(.SD, na.rm = TRUE)/13, 2), .SDcols = names(data)[-c(1,15,16)]]
  data[,StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17)]]
  data[,Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17,18)]]
  data[,Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19)]]
  data[,Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19,20)]]
  data <- as.data.frame(data)
  rownames(data) <- data$Years
  data <- data[,-1]
  data <- as.data.frame(t(data))
  data$HID <- rownames(data)
  data <- as.data.table(data[,c(6, 1:5)])
  data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
  data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,7)]]
  data[,Average:= round(rowSums(.SD, na.rm = TRUE)/5, 2), .SDcols = names(data)[-c(1,7,8)]]
  data[,StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,7,8,9)]]
  data[,Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1,7,8,9,10)]]
  data[,Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,7,8,9,10,11)]]
  data[,Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,7,8,9,10,11,12)]]
  data[14:20, 7:13] <- NA
  data[, c(paste0("Y1_per_", 2015:2019)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2015:2019)]
  data[14:20, 14:18] <- NA
  colo <- colnames(data)[-c(1, 7:13)]
  ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
  data <- as.data.frame(data)
  data <- data[,ordered]
  
  calcuated_drug_dataframes[[i]] <- data
  addWorksheet(wb = period1, sheetName = paste0("drugs_", i))
  writeData(wb = period1, sheet = i, x = calcuated_drug_dataframes[[i]])
}

tail(calcuated_drug_dataframes[[1]], 9)
saveWorkbook(period1, "Output/Summary_drug_Wise_all_Year_and_hospitals_subset_period1_Mar_2_24.xlsx", overwrite = TRUE)

########## subset the data form 2020 to 2022 ##### period 2 ###
period2 <- createWorkbook()
for(i in 1:length(drug_dataframes)) {
  data <- drug_dataframes[[i]]
  data <- data[,paste("Y1_", 2020:2022, sep = "")]
  data <- as.data.frame(t(data))
  data$Years <- rownames(data)
  data <- as.data.table(data[,c(14, 1:13)])
  data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
  data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,15)]]
  data[,Average:= round(rowSums(.SD, na.rm = TRUE)/13, 2), .SDcols = names(data)[-c(1,15,16)]]
  data[,StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17)]]
  data[,Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17,18)]]
  data[,Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19)]]
  data[,Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19,20)]]
  data <- as.data.frame(data)
  rownames(data) <- data$Years
  data <- data[,-1]
  data <- as.data.frame(t(data))
  data$HID <- rownames(data)
  data <- as.data.table(data[,c(4, 1:3)])
  data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
  data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,5)]]
  data[,Average:= round(rowSums(.SD, na.rm = TRUE)/3, 2), .SDcols = names(data)[-c(1,5,6)]]
  data[,StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,5,6,7)]]
  data[,Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1,5,6,7,8)]]
  data[,Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,5,6,7,8,9)]]
  data[,Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,5,6,7,8,9,10)]]
  data[14:20, 5:11] <- NA
  data[, c(paste0("Y1_per_", 2020:2022)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2020:2022)]
  data[14:20, 12:14] <- NA
  colo <- colnames(data)[-c(1, 5:11)]
  ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
  data <- as.data.frame(data)
  data <- data[,ordered]
  calcuated_drug_dataframes[[i]] <- data
  addWorksheet(wb = period2, sheetName = paste0("drugs_", i))
  writeData(wb = period2, sheet = i, x = calcuated_drug_dataframes[[i]])
}

# data <- calcuated_drug_dataframes[[1]]
# data[, c(paste0("Y1_per_", 2020:2022)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2020:2022)]
# data[14:20, 12:14] <- NA
# colo <- colnames(data)[-c(1, 5:11)]
# ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
# data <- as.data.frame(data)
# data <- data[,ordered]

View(calcuated_drug_dataframes[[1]])
saveWorkbook(period2, "Output/Summary_drug_Wise_all_Year_and_hospitals_subset_period2_Mar_2_24.xlsx", overwrite = TRUE)


############## create a OS percentage data for all the hospitals ####
os_dataset_per <- list()
for(i in hospital_id){
  hospital_wide[[i]] <- data.table::as.data.table(hospital_wide[[i]])
  data <- subset(hospital_wide[[i]], select = os_names_per[[i]])
  colnames(data) <- gsub("%OS_", "", colnames(data))
  print(dim(data))
  os_dataset_per[[i]] <- data
  #os_dataset[[i]] <- hospital_wide[[i]][,os_names[[i]]]
}

#### Combine the data for each plot ####

complete_os_per <- os_dataset_per[[1]]
for(i in 2:length(os_dataset_per)){
  complete_os_per <- merge(complete_os_per, os_dataset_per[[i]], by = "Drug_Name", all = TRUE)
  print(dim(complete_os_per))
}

write.csv(complete_os_per, paste0("os_percenatge_data_all_hospital_subset_", format(Sys.Date(),"%b_%d_%y"), ".csv"), quote = F, row.names = F) ### save the os_data_percentage for plot future

# ### calculate the average and minimum for the os data year wise ######
# wb <- createWorkbook()
# for(i in 1:length(os_dataset)){
#   data <- os_dataset[[i]]
#   data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
#   data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,10)]]
#   data[,Average:= round(rowSums(.SD, na.rm = TRUE)/8, 2), .SDcols = names(data)[-c(1,10,11)]]
#   data[, StandardDeviation:= apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,10,11,12)]]
#   data[, Quartile1:= apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13)]]
#   data[, Median:= apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13, 14)]]
#   data[, Quartile3:= apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13, 14, 15)]]
#   os_dataset[[i]] <- data
#   addWorksheet(wb = wb, sheetName = hospital_id[i])
#   writeData(wb = wb, sheet = i, x = os_dataset[[i]], keepNA = TRUE)
# }
# saveWorkbook(wb, "Output/Summary_hospital_Wise_all_Year.xlsx", overwrite = TRUE)
# 
#data <- os_dataset[[1]]
#data <- mutate(data, Minimum = across(-1, ~min(.,na.rm= TRUE)))
#data[,Minimum:= do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
#head(data)
##data[,Maximum:= do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1, 10)]]  ### needs to check 
#head(data)
#data[,Average:= round(rowSums(.SD,na.rm = TRUE)/8, 2), .SDcols = names(data)[-c(1,10,11)]]
#head(data)
#data[, StandardDeviation := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,10,11,12)]]
#head(data)
#data[, Q1 := apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13)]]
#data[, Median := apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13, 14)]]
#data[, Q3 := apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1, 10, 11, 12, 13, 14, 15)]]


##### Import the IDs to plot #######
drugs_id <- read.xlsx("data/ids_to_plot.xlsx", sheet = "Drugs")
drugs_id
hos_id <- read.xlsx("data/ids_to_plot.xlsx", sheet = "Hospital")
hos_id$HID <- gsub("H19_0170", "H9_0170", hos_id$HID)
hos_id$new_id <- paste("H", 1:nrow(hos_id), sep = "")
hos_id


### convert the data into required format for plot ####
long_data <- melt(complete_os_per, id.vars = "Drug_Name", variable.name = "Year", value.name = "Stock")
long_data$Stock_code <- left_join(long_data, hospital_wide[[1]], by = "Drug_Name")$STOCK_CODE
spl <- str_split_fixed(long_data$Year, pattern = "_", n = 3)
long_data$HID_full <- paste0(spl[,1], "_", spl[,2])
long_data$HID <- spl[,1]
long_data$Years <- spl[,3]
long_data <- as.data.frame(long_data)
colnames(long_data)[colnames(long_data)  == "HID"] <- "HID_old"
colnames(long_data)[colnames(long_data)  == "HID_full"] <- "HID"
long_data$HID <- as.character(long_data$HID)
long_data$HID_new <- left_join(long_data, hos_id, by = "HID")$new_id
head(long_data)
write.table(long_data, "AllDurgs_OS_selected_hospitals.tsv", quote = F, row.names = F, sep = "\t")
#### subset the required data #####
#long_data_sub <- subset(long_data, Stock_code %in% drugs_id$STOCK_CODE)
#long_data_sub <- subset(long_data_sub, HID_full %in% hos_id$HID)


# try <- subset(long_data, Stock_code == "200201")
# #wide <- dcast(try, HID_new ~ Years , value.var = "Stock")
# #mel <- melt(wide)
# #mel <- as.data.frame(mel)
# try <- try[order(try$HID_new),]
# mel$HID_new <- factor(mel$HID_new, levels = paste("H", 1:13, sep = ""))


#n <- 60
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#col <- sample(col_vector, 35)
# saveRDS(col, "colour_35.RDS")
# col <- readRDS("colour_35.RDS")
colr <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
          "#9467bd","#8c564b","#e377c2","#7f7f7f",
          "#bcbd22","#17becf","#aec7e8","#ffbb78",
          "#98df8a")

names(colr) <- unique(long_data$HID_new)
drug_name <- gsub("\\/", "_", gsub("Tab.", "", gsub(" ", "", drugs)))


############ plot drugs x axis is year and y axis is out of stock across the plot #######
##### with title #############
for(drug in 1:length(drugs)){
  try <- subset(long_data, Drug_Name == drugs[drug]) 
  try <- try[order(try$HID_new),]
  try$HID_new <- factor(try$HID_new, levels = paste("H", 1:13, sep = ""))
  plot <- ggplot(data = try, aes(x = Years, y = Stock,  color = HID_new, group = HID_new)) + 
            geom_point(size = 1.2) + 
            geom_line(size = 1.5, alpha = 0.7) + 
            scale_color_manual(values = colr, name = "Hospitals") + 
            theme_minimal() + 
            xlab("Years") +
            ylab("Percentage of out-of-stock period") +
           ggtitle(paste0("Percentage of out-of-stocks in ", drug_name[drug], " from 2015 to 2022")) +
            theme(legend.position = "bottom", 
                  plot.title = element_text(size = 14, hjust = 0.5)) 
  png(paste0("Plots/Figures_with_title/line_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}

############ without title ###########
for(drug in 1:length(drugs)){
  try <- subset(long_data, Drug_Name == drugs[drug]) 
  try <- try[order(try$HID_new),]
  try$HID_new <- factor(try$HID_new, levels = paste("H", 1:13, sep = ""))
  plot <- ggplot(data = try, aes(x = Years, y = Stock,  color = HID_new, group = HID_new)) + 
    geom_point(size = 1.2) + 
    geom_line(size = 1.5, alpha = 0.7) + 
    scale_color_manual(values = colr, name = "Hospitals") + 
    theme_minimal() + 
    xlab("Years") +
    ylab("Percentage of out-of-stock period") +
    #ggtitle(paste0("Percentage of out-of-stocks in ", drug_name[drug], " from 2015 to 2022")) +
    theme(legend.position = "bottom", 
          plot.title = element_blank()) 
  png(paste0("Plots/Figures_without_title/line_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}

################### hospital among years #################
########### without title ############
colr_for_year <- colr[1:8]
names(colr_for_year) <- seq(2015, 2022, 1)

for(drug in 1:length(drugs)){
  try <- subset(long_data, Drug_Name == drugs[drug]) 
  try <- try[order(try$HID_new),]
  try$HID_new <- factor(try$HID_new, levels = paste("H", 1:13, sep = ""))
  # plot <- ggplot(data = try, aes(x = HID_new, y = Stock,  color = Years, group = Years)) +
  #   geom_point(size = 1.2) +
  #   geom_line(size = 1.5, alpha = 0.7) +
  #   scale_color_manual(values = colr_for_year, name = "Years") +
  #   theme_minimal() +
  #   xlab("Hosptials") +
  #   ylab("Percentage of out-of-stock period") +
  #   #ggtitle(paste0("Percentage of out-of-stocks in ", drug_name[drug], " between Hospitals")) +
  #   theme(legend.position = "bottom",
  #         plot.title = element_blank())
  plot <- ggplot(data = try, aes(x = HID_new, y = Stock, fill = Years)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) + # Stacked bar plot
    scale_fill_manual(values = colr_for_year, name = "Years") +
    theme_minimal() +
    xlab("Hospitals") +
    ylab("Percentage of out-of-stock period") +
    theme(legend.position = "bottom")
  png(paste0("Plots/Figures_without_title_hospitals_bar/bar_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}

for(drug in 1:length(drugs)){
  try <- subset(long_data, Drug_Name == drugs[drug]) 
  try <- try[order(try$HID_new),]
  try$HID_new <- factor(try$HID_new, levels = paste("H", 1:13, sep = ""))
  # plot <- ggplot(data = try, aes(x = HID_new, y = Stock,  color = Years, group = Years)) + 
  #   geom_point(size = 1.2) + 
  #   geom_line(size = 1.5, alpha = 0.7) + 
  #   scale_color_manual(values = colr_for_year, name = "Years") + 
  #   theme_minimal() + 
  #   xlab("Hosptials") +
  #   ylab("Percentage of out-of-stock period") +
  plot <- ggplot(data = try, aes(x = HID_new, y = Stock, fill = Years)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) + # Stacked bar plot
    scale_fill_manual(values = colr_for_year, name = "Years") +
    theme_minimal() +
    xlab("Hospitals") +
    ylab("Percentage of out-of-stock period") +
    ggtitle(paste0(drugs[drug]," - Percentage of OS period Vs Hospitals")) +
    theme(legend.position = "bottom", 
          plot.title = element_text(hjust = 0.5, size = 14)) 
  png(paste0("Plots/Figures_with_title_hospitals_bar/bar_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}

########### Task 3 - part two - inferential statistics ####
### load required packages #####
library(rstatix)
library(lme4)
library(ggeffects)

long_data <- melt(complete_os, id.vars = "Drug_Name", variable.name = "Year", value.name = "Stock")
long_data$Stock_code <- left_join(long_data, hospital_wide[[1]], by = "Drug_Name")$STOCK_CODE
spl <- str_split_fixed(long_data$Year, pattern = "_", n = 3)
long_data$HID_full <- paste0(spl[,1], "_", spl[,2])
long_data$HID <- spl[,1]
long_data$Years <- spl[,3]
long_data <- as.data.frame(long_data)
colnames(long_data)[colnames(long_data)  == "HID"] <- "HID_old"
colnames(long_data)[colnames(long_data)  == "HID_full"] <- "HID"
long_data$HID <- as.character(long_data$HID)
long_data$HID_new <- left_join(long_data, hos_id, by = "HID")$new_id
#eRDS(long_data, paste0("data/long_data_os_all_drug_subset_", format(Sys.Date(),"%b_%d_%y"), ".RDS"))
long_data <- readRDS("data/long_data_os_all_drug_subset_Mar_03_24.RDS")
## subset by drugs #####
os_data_drugs <- list()
for(drug in drugs){
  sub <- subset(long_data, Drug_Name == drug)
  os_data_drugs[[drug]] <- sub
}
length(os_data_drugs)
saveRDS(os_data_drugs, paste0("data/os_data_each_drug_subset_", format(Sys.Date(),"%b_%d_%y"), ".RDS"))

##### subset and do the Generalized linear mixed model fit ####
sub <- na.omit(os_data_drugs[[1]])
sub_try <- subset(sub, Years != 2015)
sub_try$covid <- 

summary(sub)
summary(sub_try)
sub$Years <- as.factor(sub$Years)
sub_try$Years <- as.factor(sub_try$Years)
table(sub$Years)
table(sub_try$Years)

os_glmer <- glmer(Stock ~ Years + (1|HID_new), data = sub_try, family = "poisson")
summary(os_glmer)
os_glmer <- glmer.nb(Stock ~ Years + (1|HID_new), data = sub_try)
os_glmer
summary(os_glmer)

emm <- emmeans::emmeans(os_glmer, specs = "Years")
#summary(contrast(emm, method="trt.vs.ctrl", adjust="bonferroni"), infer=c(TRUE, TRUE))
#contrasts(emm, method="trt.vs.ctrl", adjust="bonferroni")

#try <- anova_test(data = sub, dv = Stock, wid = HID, within = Years)
#get_anova_table(try)

os_glmer <- glmer(Stock ~ Years + (1|HID_new), data = sub, family = "poisson")
summary(os_glmer, corr = FALSE)
plot(ggemmeans(os_glmer, terms = c("Years"), 
               condition = c(Years = 2022))) + 
  ggplot2::ggtitle("GLMER Effect plot")


