#### import required packages ####
library(data.table)
library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(cowplot)
library(tidyr)

### Load the created function ###
source("Script/Functions.R")

#### import the drug file and hospital id file ####
drugs_id <- read_xlsx("data/ids_to_plot.xlsx", sheet = "Drugs")
dim(drugs_id)
drugs_id$class <- c(rep("cardio", 24), rep("endo", 7))
str(drugs_id)
drugs_id$Drug_Name <- drugs
drugs_id <- as.data.frame(drugs_id)
write.table(drugs_id, "data/drug_data_for_analysis.tsv", row.names = F, quote = F, sep = "\t")
### import the hospital file ###
hos_id <- read_xlsx("data/ids_to_plot.xlsx", sheet = "Hospital")
hos_id$HID <- gsub("H19_0270", "H9_0170", hos_id$HID)
hos_id$new_id <- paste("H", 1:nrow(hos_id), sep = "")
str(hos_id)
hospital_id <- hos_id$HID
hospital_id

##### get all data ####
hospital_wide <- readRDS("data/hospital_wide_edited_subset_final_Mar_18_24.RDS")
length(hospital_wide)
lapply(hospital_wide, dim)
### add drug_class ###
for(i in 1:length(hospital_wide)){
  hospital_wide[[i]]$class <- left_join(hospital_wide[[i]], drugs_id, by = "Drug_Name")$class
  print(dim(hospital_wide[[i]]))
}
hospital_wide_backup <- hospital_wide
years <- c(2015:2022)
years
############# table 10 #######################
head(hospital_wide[[1]])
update_hospital_wide <- list()
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  for(j in 1:length(years)){
    var1 <- paste0("FA_", hospital_id[i], "_", years[j])
    out1 <- paste0("FA_upper_", years[j])
    out2 <- paste0("FA_lower_", years[j])
    var2 <- paste0("CS_", hospital_id[i], "_", years[j])
    out3 <- paste0("Deviataion_", years[j])
    data[[out1]] <- data[[var1]] * 1.25
    data[[out2]] <- data[[var1]] * 0.75
    data[[out3]] <- ifelse(data[[var2]] >= data[[out2]] & data[[var2]] <= data[[out1]], "WR", "OR")
    print(table(data[[out3]], useNA = "always"))
  }
  update_hospital_wide[[i]] <- data
}
View(update_hospital_wide[[1]])
########## subset the required column ###################
deviation_status <- list()
for(i in 1:length(update_hospital_wide)){
  data <- as.data.frame(update_hospital_wide[[i]])
  col1 <- grep("%FAC_", colnames(data), value = T)
  col2 <- grep("Deviataion_", colnames(data), value = T)
  data <- data[,c("Drug_Name","class",col1, col2)]
  colnames(data) <- gsub(paste0(hospital_id[i], "_"), "", gsub("%", "", colnames(data)))
  deviation_status[[i]] <- data
}
lapply(deviation_status, colnames)

View(deviation_status[[1]])
############# convert the data into drug wise - each data frame for each drug ####
dev_drug_dataframes <- list()
for (drug_index in 1:31) {
  drug_data <- data.frame()
  for (df_index in 1:length(deviation_status)) {
    #colnames(ADE_data[[df_index]]) <- c("Drug_Name", "STOCK_CODE", gsub(paste0(hospital_id[df_index], "_"), "", colnames(ADE_data[[df_index]])[-c(1,2)]))
    drug_data_df <- deviation_status[[df_index]][drug_index, ]
    drug_data <- rbind(drug_data, drug_data_df)
  }
  drug_data <- drug_data[,-1]
  drug_data$HID <- hospital_id
  col_order <- c("HID", colnames(drug_data)[-ncol(drug_data)])
  drug_data <- drug_data[,col_order]
  drug_data <- as.data.frame(drug_data)
  rownames(drug_data) <- hospital_id
  drug_data <- drug_data[,-1]
  dev_drug_dataframes[[drug_index]] <- drug_data
}

########## only subset %ADE columns form the the data #### d
dev_per <- list()
for(i in 1:length(dev_drug_dataframes)){
  data <- dev_drug_dataframes[[i]]
  data <- data[,grep("FAC_", colnames(data), value = T)]
  print(colnames(data))
  print(dim(data))
  data$HID <- rownames(data)
  dev_per[[i]] <- data
}

###### calculate the summary for this data #####
addition <- c("Minimum", "Maximum", "Average", "StandardDeviation", "Quartile1", "Median", "Quartile3")
FAC_summary <- calculate_fac_summary(dev_per, addition)
lapply(FAC_summary, colnames)
View(FAC_summary[[1]])

###### calculate the overall status ######
dev_overall_status <- list()
for(i in 1:length(dev_drug_dataframes)){
  data <- dev_drug_dataframes[[i]]
  data <- data[,grep("Deviataion_", colnames(data), value = T)]
  print(colnames(data))
  print(dim(data))
  dev_overall_status[[i]] <- data
}

dev_calcuated <- calculate_max_category(dev_overall_status) ## calculate the overall status and return the list as a vector for all drugs

for(i in 1:length(dev_overall_status)){
  data <- dev_overall_status[[i]]
  data$overall_status <- dev_calcuated[[i]]
  dev_overall_status[[i]] <- data
}

############# Now transpose and repeat the process #####
dev_trans <- lapply(dev_overall_status, function(x) as.data.frame(t(x)))
calculate <- calculate_max_category(dev_trans)
final_data <- list()
for(i in 1:length(dev_trans)){
  data <- dev_trans[[i]]
  data$overall_status <- calculate[[i]]
  data <- as.data.frame(t(data))
  data$HID <- rownames(data) ### add HID as a variable in the data
  print(head(data))
  final_data[[i]] <- data
}



roworder <- c(hospital_id, "overall_status", addition)
roworder
colo <- c(colnames(dev_per[[1]])[-8], colnames(final_data[[1]])[-c(9,10)])
colo
#colorder <- c("HID", colnames(FAC_summary[[1]])[-1], colnames(final_data[[3]])[-10])
ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition, "overall_status")
#colorder
#### add the FAC_status with this data ####
wb <- createWorkbook()
complete <- list()
for(i in 1:length(ade)){
  v1 <- as.data.frame(FAC_summary[[i]])
  v2 <- as.data.frame(final_data[[i]])
  merg1 <- merge(v1, v2, by = "HID", all = TRUE)
  #merg2 <- merge(merg1, v3, by = "HID", all = TRUE)
  rownames(merg1) <- merg1$HID
  merg1 <- merg1[,ordered]
  merg1 <- merg1[roworder,]
  print(head(merg1))
  complete[[i]] <- merg1
  addWorksheet(wb = wb, sheetName = paste0("drug_", i))
  writeData(wb = wb, sheet = i, x = complete[[i]])
}

saveWorkbook(wb, "Output/FA_data_complete_data_summary_task11_table10.xlsx", overwrite = TRUE)



####### table 11 ###################
##Percentage of medicine that has %FAC between 0-25% - Group A
##Percentage of medicine that has %FAC between 26%-50% - Group B
##Percentage of medicine that has %FAC between 51%-75% - Group C
##Percentage of medicine that has %FAC between 76%-100% - Group D
##Percentage of medicine that has %FAC < 0 - Group E
######## subset the column you need ###############
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  print(hospital_id[i])
  x <- grep("%FAC_", colnames(data), value = T)
  print(x) ### print the colnames with the pattern #####
  print(length(x))
}

#hospital_wide[[13]]$'%FAC_H33_A010_2016' <- NA

FAC_data <- list()
for(i in 1:length(hospital_wide)){
  data <- as.data.frame(hospital_wide[[i]])
  col1 <- grep("%FAC_", colnames(data), value =T)
  data <- data[,c("Drug_Name", col1)]
  colnames(data) <- gsub("%FAC", "FAC_per", colnames(data))
  colnames(data) <- gsub(paste0(hospital_id[i], "_"), "", colnames(data))
  print(head(data))
  data$class <- left_join(data, drugs_id, by = "Drug_Name")$class
  FAC_data[[i]] <- data
}

######### breaks and corresponding labels #######
breaks <- c(-Inf, 0, 25, 50, 75, 100)
breaks
labels <- c("Group E", "Group A", "Group B", "Group C", "Group D")
labels
years <- c(2016:2022)
years
hospital_id
################## run it for all the dataset and years #################3
FAC_status <- categorize_data(FAC_data, hospital_id, years, breaks, labels)
lapply(FAC_status, summary)

######## subset by cardio and endo ############
cardio <- list()
endo <- list()
for(i in 1:length(FAC_status)){
  data <- FAC_status[[1]]
  #hospital_wide[[i]] <- as.data.frame(hospital_wide[[i]])
  #hospital_wide[[i]]$class_drug <- left_join(hospital_wide[[i]], drugs_id, by = "STOCK_CODE")$class  
  #hospital_wide[[i]]$class <- as.factor(hospital_wide[[i]]$class)
  #hospital_wide[[i]]$class <- c(rep("cardio", 24), rep("endo", 7))
  cardio[[i]] <- subset(data, class == "cardio")
  endo[[i]] <- subset(data, class == "endo")
}
drug_list <- list()

################### get the FAC drugs for cardio 
result_cardio <- list()
for(year in years) {
  for(idname in hospital_id) {
    data <- as.data.frame(subset(hospital_wide[[idname]], class == "cardio"))
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    x <- FAC_count_sep(data, variable1, variable2)
    print(x$count)
    result_cardio[[paste0(idname, "_", year)]] <- x$Drug_Name
  }
}



result_endo <- list()
for(year in years) {
  for(idname in hospital_id) {
    data <- as.data.frame(subset(hospital_wide[[idname]], class == "endo"))
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    x <- FAC_count_sep(data, variable1, variable2)
    print(x$count)
    result_endo[[paste0(idname, "_", year)]] <- x$Drug_Name
  }
}



###################### convert into pivot longer and create the data frame with count data ############
# try <- cardio[[1]]
# try <- try[,c("Drug_Name", "FAC_status_2016")]
# colnames(try) <- c("Drug_Name", "var1")
# try <- subset(try, Drug_Name %in% result_cardio$H2_0060_2016)
# count_cardio <- try %>% group_by(var1) %>% summarize(y_2016 = n())
# count_cardio
wide_cardio <- list()
for(i in 1:length(cardio)){
  hospital <- list()
  for(j in 1:length(years)){
      vara <- paste0("FAC_status_", years[j])
      print(vara)
      data <- cardio[[i]][,c("Drug_Name", vara)]
      hos <- paste0(hospital_id[i], "_", years[j])
      data <- subset(data, Drug_Name %in% result_cardio[[hos]])
      colnames(data) <- c("Drug_Name", "var1")
      out <- data %>% group_by(var1) %>% summarize(var2 = n())
      out$var1 <- factor(out$var1, levels = c("Group A", "Group B", "Group C", "Group D", "Group E"))
      colnames(out) <- c("var1", paste0("Y_", years[j]))
      #print(out)
      hospital[[j]] <- out
      
  }
  print(length(hospital))
  print("years calculated")
  final <- hospital[[1]]
  #print(colnames(final))
  for(y in 2:length(hospital)){
    final <- merge(final, hospital[[y]], by = "var1", all = TRUE)
  }
  final <- final[!is.na(final$var1),]
  rownames(final) <- final$var1
  final <- final[,-grep("var1", colnames(final))]
  final <- as.data.frame(t(final))
  final$Year <- rownames(final)
  wide_cardio[[i]] <- final
}
      
#data_long <- data %>%
#pivot_longer(cols = starts_with("FAc_status_"), names_to = "Year", values_to = "Category")
#data_long <- data_long[,c("Drug_Name", "Year", "Category")]
#data_long$Category <- factor(data_long$Category, levels = c("Over estimate", "Under estimate","Equivalent"))
#counts <- data_long %>%
#group_by(Year, Category) %>%
#summarize(Count = n())
#counts_wide <- counts %>%
#pivot_wider(names_from = Category, values_from = Count, values_fill = 0)

wide_endo <- list()
for(i in 1:length(endo)){
  hospital <- list()
  for(j in 1:length(years)){
    vara <- paste0("FAC_status_", years[j])
    print(vara)
    data <- endo[[i]][,c("Drug_Name", vara)]
    hos <- paste0(hospital_id[i], "_", years[j])
    data <- subset(data, Drug_Name %in% result_endo[[hos]])
    colnames(data) <- c("Drug_Name", "var1")
    out <- data %>% group_by(var1) %>% summarize(var2 = n())
    out$var1 <- factor(out$var1, levels = c("Group A", "Group B", "Group C", "Group D", "Group E"))
    colnames(out) <- c("var1", paste0("Y_", years[j]))
    #print(out)
    hospital[[j]] <- out
    
  }
  print(length(hospital))
  print("years calculated")
  final <- hospital[[1]]
  #colnames(final)
  for(y in 2:length(hospital)){
    final <- merge(final, hospital[[y]], by = "var1", all = TRUE)
  }
  final <- final[!is.na(final$var1),]
  rownames(final) <- final$var1
  final <- final[,-grep("var1", colnames(final))]
  final <- as.data.frame(t(final))
  final$Year <- rownames(final)
  wide_endo[[i]] <- final
}
wide_endo[[1]]
############# import the FAC data for both cardio and ento #####
fac_cardio <- read.csv("Output/FAC_data_cardio.csv", header = T)
fac_cardio <- fac_cardio[,-2] ### remove the 2015 data
rownames(fac_cardio) <- fac_cardio$HID
colnames(fac_cardio)[2:ncol(fac_cardio)] <- gsub("FAC", "Y", colnames(fac_cardio)[2:ncol(fac_cardio)])
#### convert into long format ############
fac_cardio_long <- fac_cardio %>% pivot_longer(cols = starts_with("Y_"), names_to = "Year", values_to = "FAC")
fac_cardio_long <- as.data.frame(fac_cardio_long)
head(fac_cardio_long)
fac_cardio_hos <- list()
for(i in 1:length(hospital_id)){
  data <- subset(fac_cardio_long, HID == hospital_id[[i]])
  data <- data[,c(2,3)]
  fac_cardio_hos[[i]] <- data
}
######### import the endo ############3
fac_endo <- read.csv("Output/FAC_data_endo.csv", header = T)
fac_endo <- fac_endo[,-2]
rownames(fac_endo) <- fac_endo$HID
colnames(fac_endo)[2:ncol(fac_endo)] <- gsub("FAC", "Y", colnames(fac_endo)[2:ncol(fac_endo)])
####### convert into long format ########
fac_endo_long <- fac_endo %>% pivot_longer(cols = starts_with("Y_"), names_to = "Year", values_to = "FAC")
fac_endo_long <- as.data.frame(fac_endo_long)
fac_endo_long
fac_endo_hos <- list()
for(i in 1:length(hospital_id)){
  data <- subset(fac_endo_long, HID == hospital_id[[i]])
  data <- data[,c(2,3)]
  fac_endo_hos[[i]] <- data
}
############ combine the FAC and each hospital data ########## 
for(i in 1:length(fac_cardio_hos)) {
  wide_cardio[[i]] <- merge(wide_cardio[[i]], fac_cardio_hos[[i]], by = "Year", all = TRUE)
  colnames(wide_cardio[[i]]) <- gsub(" ", "_", colnames(wide_cardio[[i]]))
  wide_endo[[i]] <- merge(wide_endo[[i]], fac_endo_hos[[i]], by = "Year", all = TRUE)
  colnames(wide_endo[[i]]) <- gsub(" ", "_", colnames(wide_endo[[i]]))
}
wide_endo[[1]]
wide_cardio[[1]]
#### calcuate the proportion ####
lapply(wide_endo, colnames)
lapply(wide_cardio, colnames)

######################### few columns are not there so add #####
extra_column <- "Group_E"
########## if equivalent is not there, it will add a new column
for(i in 1:length(wide_cardio)) {
  if(!(extra_column %in% colnames(wide_cardio[[i]]))) {
    wide_cardio[[i]][[extra_column]] <- NA  # Add extra column with NA values
  }
}
for(i in 1:length(wide_endo)){ 
  if(!(extra_column %in% colnames(wide_endo[[i]]))) {
    wide_endo[[i]][[extra_column]] <- NA  # Add extra column with NA values
  }
}


######## Add a average row ############
for(i in 1:length(wide_cardio)){
  ### process the wide cardio list ####
  data <- wide_cardio[[i]]
  new_row <- data %>%
    dplyr::summarise(across(c(Group_A, Group_B, Group_C, Group_D, Group_E, FAC), ~ceiling(mean(., na.rm = TRUE))),  # Compute the mean for numeric columns
                     Year = "Average")
  data <- rbind(data, new_row)
  wide_cardio[[i]] <- data
  #### process the wide endo list #####
  data <- wide_endo[[i]]
  new_row <- data %>%
    dplyr::summarise(across(c(Group_A, Group_B, Group_C, Group_D, Group_E, FAC), ~ceiling(mean(., na.rm = TRUE))),  # Compute the mean for numeric columns
                     Year = "Average")
  data <- rbind(data, new_row)
  wide_endo[[i]] <- data
}

######### calculate proportion for each data #######
final_cardio <- list()
final_endo <- list()
cardio_wb <- createWorkbook()
endo_wb <- createWorkbook()
for(i in 1:length(wide_cardio)){
  ##### process the wide cardio list ####
  data <- wide_cardio[[i]]
  data <- data %>% mutate(Group_A_prop = Group_A/FAC*100,
                          Group_B_prop = Group_B/FAC*100,
                          Group_C_prop = Group_C/FAC*100,
                          Group_D_prop = Group_D/FAC*100,
                          Group_E_prop = Group_E/FAC*100)  # Corrected column name
  #rownames(data) <- data$Year
  #data <- data[,-1]
  #data <- as.data.frame(t(data))
  final_cardio[[i]] <- data
  addWorksheet(wb = cardio_wb, sheetName = hospital_id[i])
  writeData(wb = cardio_wb, sheet = i, x = final_cardio[[i]])
}
saveWorkbook(cardio_wb, "Output/cardio_summary_task11_table11_all_hospital.xlsx", overwrite = TRUE) ## save the cardio summary ###

for(i in 1:length(wide_endo)){
#### process the wide endo list #####
  data <- wide_endo[[i]]
  data <- data %>% mutate(Group_A_prop = Group_A/FAC*100,
                          Group_B_prop = Group_B/FAC*100,
                          Group_C_prop = Group_C/FAC*100,
                          Group_D_prop = Group_D/FAC*100,
                          Group_E_prop = Group_E/FAC*100)  # Corrected column name
  final_endo[[i]] <- data
  addWorksheet(wb = endo_wb, sheetName = hospital_id[i])
  writeData(wb = endo_wb, sheet = i, x = final_endo[[i]])
}
final_endo
saveWorkbook(endo_wb, "Output/endo_summary_task11_table11_all_hospital.xlsx", overwrite = TRUE) ### save the endo summary ####