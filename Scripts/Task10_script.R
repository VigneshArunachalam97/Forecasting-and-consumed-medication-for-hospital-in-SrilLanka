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
hospital_wide <- readRDS("data/hospital_wide_edited_subset_final_Mar_16_24.RDS")
length(hospital_wide)
lapply(hospital_wide, dim)

################### FI status ############3
##### print colnames with DE ####3
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  print(hospital_id[i])
  x <- grep("FI", colnames(data), value = T)
  print(x)
  print(length(x))
}

############ great a status for each year ####
years <- seq(2016, 2022, 1)
years
hospital_id
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  for(j in 1:length(years)){
    var1 <- paste0("FI_", hospital_id[i], "_", years[j])
    var2 <- paste0("OS_", hospital_id[i], "_", years[j])
    print(var1)
    out1 <- paste0("FA_WHO_", hospital_id[i], "_", years[j])
    print(out1)
    out2 <- paste0("OS_status_", hospital_id[i], "_", years[j])
    data[[out1]] <- ifelse((data[[var1]] < 100 | data[[var1]] > 100) & data[[var2]] > 0, "unconfirmed inaccurate forecast", 
                           ifelse((data[[var1]] < 100 | data[[var1]] > 100) & data[[var2]] == 0, "confirmed inaccurate forecast",
                                  ifelse(data[[var1]] == 100, "accurate forecast",NA)))
    data[[out2]] <- ifelse(data[[var2]]==0, "Absence", "Presence")
  }
  hospital_wide[[i]] <- data
}

############# subset the required column
FI_data <- extract_FI_data(hospital_wide)
FI_data[[13]]$FI_H33_A010_2016 <- NA
length(FI_data)
View(FI_data[[1]])

for(i in 1:length(FI_data)){
    data <- FI_data[[i]]
    colo <- colnames(data)[-c(1,2)]
    ordered <- c("Drug_Name","STOCK_CODE", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))])
    data <- data[,ordered]
    colnames(data) <- gsub(paste0(hospital_id[i], "_"), "", colnames(data))
    FI_data[[i]] <- data
}
############################# subset by drug data and combine the hospital #############
Drug_FI <- list()
for (drug_index in 1:31) {
  drug_data <- data.frame()
  for (df_index in 1:length(FI_data)) {
    #colnames(FI_data[[df_index]]) <- c("Drug_Name", paste("AAC_", seq(2015, 2022,1), sep = ""))
    drug_data_df <- FI_data[[df_index]][drug_index, ]
    drug_data <- rbind(drug_data, drug_data_df)
  }
  drug_data <- drug_data[,-c(grep("STOCK_CODE", colnames(drug_data)))]
  drug_data$HID <- hospital_id
  #drug_data <- drug_data[,c("HID", paste("AAC_", seq(2015, 2022,1), sep = ""))]
  drug_data <- drug_data[,c(ncol(drug_data), 2:ncol(drug_data)-1)]
  drug_data <- as.data.frame(drug_data)
  rownames(drug_data) <- hospital_id
  drug_data$class <- left_join(drug_data, drugs_id, by = "Drug_Name")$class
  drug_data <- drug_data[,-1]
  Drug_FI[[drug_index]] <- drug_data
}


View(Drug_FI[[1]])

######### save the Drug FI data ############
#### calculate minimum and maximum #########
for(i in 1:length(Drug_FI)){
  data <- as.data.table(Drug_FI[[i]])
  data[, Minimum := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = grep("^FI_", colnames(data), value = TRUE)]
  data[, Maximum := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = grep("^FI_", colnames(data), value = TRUE)]
  data$HID <- hospital_id
  Drug_FI[[i]] <- data
}

#### To calculate average -- subset the required variable and convert into long format #####
cal_FI <- list()
for(i in 1:length(Drug_FI)){
  data <- as.data.frame(Drug_FI[[i]])
  print(dim(data))
  col1 <- grep("FI_", colnames(data), value = T)
  #col2 <- grep("OS_status_", colnames(data), value = T)
  data <- data[,c("HID", col1)]
  cal_FI[[i]] <- data
}
cal_OS <- list()
for(i in 1:length(Drug_FI)){
  data <- as.data.frame(Drug_FI[[i]])
  print(dim(data))
  #col1 <- grep("FI_", colnames(data), value = T)
  col2 <- grep("OS_status_", colnames(data), value = T)
  data <- data[,c("HID", col2)]
  cal_OS[[i]] <- data
}
calculate_ave <- list()
for(i in 1:length(cal_FI)){
      data1 <- cal_FI[[i]]
      data2 <- cal_OS[[i]]
      long_df <- melt(data1, id.vars = c("HID"), value.name = "FI")
      long_df$ID <- paste0(long_df$HID, "_", str_split_fixed(long_df$variable, "_", 2)[,2])
      long_df1 <- melt(data2, id.vars = c("HID"), value.name = "Status")
      long_df1$ID <- paste0(long_df1$HID, "_", str_split_fixed(long_df1$variable, "_", 3)[,3])
      long_df$status <- left_join(long_df, long_df1, by = "ID")$Status
      long_df <- long_df[,c("HID", "ID", "FI", "status")]
      calculate_ave[[i]] <- long_df
}
length(calculate_ave)
View(calculate_ave[[1]])
presence <- list()
absence <- list()

for(i in 1:length(calculate_ave)){
  presence[[i]] <- subset(calculate_ave[[i]], status == "Presence")
  absence[[i]] <-  subset(calculate_ave[[i]], status == "Absence")
}

View(presence[[1]])
summary_pre <- list()
summary_abs <- list()
for(i in 1:length(presence)){
      summary_pre[[i]] <- presence[[i]] %>% group_by(HID) %>% summarise(N_presence = n(),
                                              Quartile1_presence = quantile(FI, 0.25, na.rm = TRUE),
                                              Median_presence = median(FI, na.rm = TRUE),
                                              Quartile3_presence = quantile(FI, 0.75, na.rm = TRUE))
      summary_pre[[i]] <- as.data.frame(summary_pre[[i]])
      summary_abs[[i]] <- absence[[i]] %>% group_by(HID) %>% summarise(N_absence = n(),
                                              Quartile1_absence = quantile(FI, 0.25, na.rm = TRUE),
                                              Median_absence = median(FI, na.rm = TRUE),
                                              Quartile3_absence = quantile(FI, 0.75, na.rm = TRUE))
      summary_abs[[i]] <- as.data.frame(summary_abs[[i]])
}
wb <- createWorkbook()
final_cal <- list()
for(i in 1:length(Drug_FI)){
  data <- Drug_FI[[i]]
  data <- left_join(data, summary_pre[[i]], by = "HID")
  data <- left_join(data, summary_abs[[i]], by = "HID")
  final_cal[[i]] <- data
  addWorksheet(wb = wb, sheetName = paste0("Drugs_", i))
  writeData(wb = wb, sheet = i, x = final_cal[[i]])
}
saveWorkbook(wb, overwrite = TRUE, "Output/FI_data_output_table8.xlsx")
################### Table 9 ####################################
#### subset the only the required column from the FI_data ###
FA_status <- list()
for(i in 1:length(FI_data)){
  data <- FI_data[[i]]
  col1 <- grep("FA_WHO_", colnames(data), value =T)
  data <- data[,c("Drug_Name", col1)]
  data$class <- left_join(data, drugs_id, by = "Drug_Name")$class
  FA_status[[i]] <- data
}
### subset by cardio and ento ####
cardio <- list()
endo <- list()
for(i in 1:length(FA_status)){
  cardio[[i]] <- subset(FA_status[[i]], class == "cardio")
  endo[[i]] <- subset(FA_status[[i]], class == "endo")
}
length(endo)
length(cardio)
############# create a number of under estimate, over estimate, Equivalent #####
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
    vara <- paste0("FA_WHO_", years[j])
    print(vara)
    data <- cardio[[i]][,c("Drug_Name", vara)]
    hos <- paste0(hospital_id[i], "_", years[j])
    data <- subset(data, Drug_Name %in% result_cardio[[hos]])
    colnames(data) <- c("Drug_Name", "var1")
    out <- data %>% group_by(var1) %>% summarize(var2 = n())
    out$var1 <- factor(out$var1, levels = c("confirmed inaccurate forecast", "unconfirmed inaccurate forecast", "accurate forecast"))
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
    vara <- paste0("FA_WHO_", years[j])
    print(vara)
    data <- endo[[i]][,c("Drug_Name", vara)]
    hos <- paste0(hospital_id[i], "_", years[j])
    data <- subset(data, Drug_Name %in% result_endo[[hos]])
    colnames(data) <- c("Drug_Name", "var1")
    #print(data)
    out <- data %>% group_by(var1) %>% summarize(var2 = n())
    out$var1 <- factor(out$var1, levels = c("confirmed inaccurate forecast", "unconfirmed inaccurate forecast", "accurate forecast"))
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
extra_column <- "accurate_forecast"
########## if equivalent is not there, it will add a new column
for(i in 1:length(wide_cardio)) {
  if(!(extra_column %in% colnames(wide_cardio[[i]]))) {
    wide_cardio[[i]][[extra_column]] <- NA  # Add extra column with NA values
  }
}

for(i in 1:length(wide_endo)) {
  if(!(extra_column %in% colnames(wide_endo[[i]]))) {
    wide_endo[[i]][[extra_column]] <- NA  # Add extra column with NA values
  }
}


# Assuming 'data' is your data frame
for(i in 1:length(wide_cardio)){
  ### process the wide cardio list ####
  data <- wide_cardio[[i]]
  new_row <- data %>%
    dplyr::summarise(across(c(confirmed_inaccurate_forecast, unconfirmed_inaccurate_forecast, accurate_forecast, FAC), ~ceiling(mean(., na.rm = TRUE))),  # Compute the mean for numeric columns
                     Year = "Average")
  data <- rbind(data, new_row)
  wide_cardio[[i]] <- data
  #### process the wide endo list #####
  data <- wide_endo[[i]]
  new_row <- data %>%
    dplyr::summarise(across(c(confirmed_inaccurate_forecast, unconfirmed_inaccurate_forecast, accurate_forecast, FAC), ~ceiling(mean(., na.rm = TRUE))),  # Compute the mean for numeric columns
                     Year = "Average")
  data <- rbind(data, new_row)
  wide_endo[[i]] <- data
}


### calculate the average #############
final_cardio <- list()
final_endo <- list()
cardio_wb <- createWorkbook()
endo_wb <- createWorkbook()
for(i in 1:length(wide_cardio)){
  ##### process the wide cardio list ####
  data <- wide_cardio[[i]]
  data <- data %>% mutate(conf_inacc_prop = confirmed_inaccurate_forecast/FAC*100, 
                          unconf_inacc_prop = unconfirmed_inaccurate_forecast/FAC*100,
                          acc_prop = accurate_forecast/FAC*100)  # Corrected column name
  final_cardio[[i]] <- data
  addWorksheet(wb = cardio_wb, sheetName = hospital_id[i])
  writeData(wb = cardio_wb, sheet = i, x = final_cardio[[i]])
}
final_cardio
saveWorkbook(cardio_wb, "Output/cardio_summary_FA_WHO_status_table9_all_hospital.xlsx", overwrite = TRUE) ## save the cardio summary ###

for(i in 1:length(wide_endo)){
  #### process the wide endo list #####
  data <- wide_endo[[i]]
  data <- data %>% mutate(conf_inacc_prop = confirmed_inaccurate_forecast/FAC*100, 
                          unconf_inacc_prop = unconfirmed_inaccurate_forecast/FAC*100,
                          acc_prop = accurate_forecast/FAC*100)  # Corrected column name
  final_endo[[i]] <- data
  addWorksheet(wb = endo_wb, sheetName = hospital_id[i])
  writeData(wb = endo_wb, sheet = i, x = final_endo[[i]])
}
final_endo
saveWorkbook(endo_wb, "Output/endo_summary_FA_WHO_status_table9_all_hospital.xlsx", overwrite = TRUE) ### save the endo summary ####
