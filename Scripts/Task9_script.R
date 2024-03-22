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
drugs_id <- as.data.frame(drugs_id)
### import the hospital file ###
hos_id <- read_xlsx("data/ids_to_plot.xlsx", sheet = "Hospital")
hos_id$HID <- gsub("H19_0270", "H9_0170", hos_id$HID)
hos_id$new_id <- paste("H", 1:nrow(hos_id), sep = "")
str(hos_id)
hospital_id <- hos_id$HID
hospital_id

##### get all data ####
hospital_wide <- readRDS("data/hospital_wide_edited_subset_final_Mar_11_24.RDS")
length(hospital_wide)
lapply(hospital_wide, dim)

##### print colnames with DE ####3
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  print(hospital_id[i])
  x <- grep("DE", colnames(data), value = T)
  print(x)
  print(length(x))
}
#### data set H33_A010 doesn't have 2016 column ## create a column with NA
hospital_wide[["H33_A010"]]$DE_H33_A010_2016 <- NA
hospital_wide[["H33_A010"]]$ADE_H33_A010_2016 <- NA
hospital_wide[["H33_A010"]]$'%ADE_H33_A010_2016' <- NA

############ great a status for each year ####
years <- seq(2016, 2022, 1)
years
hospital_id
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
    for(j in 1:length(years)){
      var1 <- paste0("DE_", hospital_id[i], "_", years[j])
      print(var1)
      out <- paste0("DE_status_", hospital_id[i], "_", years[j])
      print(out)
      data[[out]] <- ifelse(data[[var1]] < 0, "Under estimate", 
                            ifelse(data[[var1]] == 0, "Equivalent", "Over estimate"))
    }
  hospital_wide[[i]] <- data
}
colnames(hospital_wide[[11]])[colnames(hospital_wide[[11]]) == "%ADE_H41_0220_2017"] <- "%ADE_H14_0220_2017"


########## created a status - Now subse the required columns from the each dataset ##### 
ADE_data <- list()

for(j in 1:length(hospital_wide)) {
  data <- hospital_wide[[j]]
  col1 <- grep("DE_status", colnames(data), value = T)
  #col2 <- grep("%ADE_", colnames(data), value = T)
  col3 <- grep("ADE_", colnames(data), value = T)
  col_need <- c("Drug_Name", "STOCK_CODE", col1, col3)
  print(col_need)
  data <- data[,..col_need]
  colnames(data) <- gsub(paste0(hospital_id[j], "_"), "", colnames(data))
  #print(paste0("The dimension of ", hospital_id[i], " is ", dim(data)))
  ADE_data[[j]] <- data
}
names(ADE_data) <- hospital_id
View(ADE_data[[2]])


### order them based on the years #######
wb <- createWorkbook()
for(i in 1:length(ADE_data)){
  data <- as.data.frame(ADE_data[[i]])
  col_names <- colnames(data)[3:ncol(data)]
  sorted_col_names <- col_names[order(as.numeric(sub(".*_(\\d+)$", "\\1", col_names)))]
  col_names <- c("Drug_Name", sorted_col_names)
  data <- data[, col_names]
  ADE_data[[i]] <- as.data.table(data)
  addWorksheet(wb = wb, sheetName = hospital_id[i])
  writeData(wb = wb, sheet = i, x = ADE_data[[i]])
}

saveWorkbook(wb, "data/ADE_data_status_per_all_hospitals.xlsx", overwrite = TRUE)

############## calculate the Maximum, minimum and mean, average #######
######## subset by medicine and merge it #####
drug_ADE <- vector("list", length = 31)
drugs <- hospital_wide[[1]]$Drug_Name
drug_order <- paste("drugs_", 1:31, sep = "")
ord <- data.frame(sheetname = drug_order, Drugs = drugs)
ord$STOCK_cODE <- merge(ord, hospital_wide[[1]], by.y = "Drug_Name", by.x = "Drugs")$STOCK_CODE
write.table(ord, "Output/druglist_order.tsv", sep = "\t", row.names = F, quote = F)
# Iterate over each drug
ade_drug_dataframes <- list()
for (drug_index in 1:31) {
  drug_data <- data.frame()
  for (df_index in 1:length(ADE_data)) {
    #colnames(ADE_data[[df_index]]) <- c("Drug_Name", "STOCK_CODE", gsub(paste0(hospital_id[df_index], "_"), "", colnames(ADE_data[[df_index]])[-c(1,2)]))
    drug_data_df <- ADE_data[[df_index]][drug_index, ]
    drug_data <- rbind(drug_data, drug_data_df)
  }
  drug_data <- drug_data[,-1]
  drug_data$HID <- hospital_id
  col_order <- c("HID", colnames(drug_data)[-ncol(drug_data)])
  drug_data <- drug_data[,..col_order]
  drug_data <- as.data.frame(drug_data)
  rownames(drug_data) <- hospital_id
  drug_data <- drug_data[,-1]
  ade_drug_dataframes[[drug_index]] <- drug_data
}

########## only subset %ADE columns form the the data #### 
ade_per <- list()
for(i in 1:length(ade_drug_dataframes)){
  data <- ade_drug_dataframes[[i]]
  data <- data[,grep("%ADE_", colnames(data), value = T)]
  print(colnames(data))
  print(dim(data))
  ade_per[[i]] <- data
}

###### calculate the summary for this data #####
addition <- c("Minimum", "Maximum", "Average", "StandardDeviation", "Quartile1", "Median", "Quartile3")
ade_summary <- calculate_ade_summary(ade_per, addition)

########## calculate the status of overall #####
ade_status <- list()
for(i in 1:length(ade_drug_dataframes)){
  data <- ade_drug_dataframes[[i]]
  data <- data[,grep("DE_status", colnames(data), value = T)]
  print(colnames(data))
  print(dim(data))
  ade_status[[i]] <- data
}

ade_status_calculated <- calculate_ade_status(ade_status)

########### combine the ADE_status, %ADE, and ADE 

ade <- list()
for(i in 1:length(ade_drug_dataframes)){
  data <- ade_drug_dataframes[[i]]
  data$HID <- row.names(data)
  data <- data[,c("HID", grep("^ADE_", colnames(data), value = T))]
  print(colnames(data))
  print(dim(data))
  ade[[i]] <- data
}

roworder <- c(hospital_id, "overall_status", addition)
colorder <- c("HID", colnames(ADE_data[[3]])[-1], "overall_status", addition)

wb <- createWorkbook()
complete <- list()
for(i in 1:length(ade)){
  v1 <- as.data.frame(ade[[i]])
  v2 <- as.data.frame(ade_status_calculated[[i]])
  v3 <- as.data.frame(ade_summary[[i]])
  merg1 <- merge(v1, v2, by = "HID", all = TRUE)
  merg2 <- merge(merg1, v3, by = "HID", all = TRUE)
  rownames(merg2) <- merg2$HID
  merg2 <- merg2[roworder,]
  merg2 <- merg2[,colorder]
  complete[[i]] <- merg2
  addWorksheet(wb = wb, sheetName = paste0("drug_", i))
  writeData(wb = wb, sheet = i, x = complete[[i]])
}

saveWorkbook(wb, "Output/ADE_data_complete_data_summary.xlsx", overwrite = TRUE)

names(complete) <- drugs
names(complete)

new_complete_data <- list()
for(i in 1:length(complete)){
 data <- complete[[i]]
 data <- data[1:13, 1:22]
 new_complete_data[[i]] <- data
}

######## change the data from drugs to hospital wide ##### 
#colnames(drugs_id) <- c("STOCK_CODE", "Drug_Name", "class")
drugs_id$Drug_Name <- drugs
drugs_id <- as.data.frame(drugs_id)
View(drugs_id)

Hospital_ade <- list()
for(drug_index in 1:13) {
  drug_data <- data.frame()
  for (df_index in 1:length(new_complete_data)) {
    #colnames(ADE_data[[df_index]]) <- c("Drug_Name", "STOCK_CODE", gsub(paste0(hospital_id[df_index], "_"), "", colnames(ADE_data[[df_index]])[-c(1,2)]))
    drug_data_df <- new_complete_data[[df_index]][drug_index, ]
    drug_data <- rbind(drug_data, drug_data_df)
  }
  #print(head(drug_data))
  drug_data <- drug_data[,-1]
  print("removed the first column")
  drug_data$Drug_Name <- drugs
  print("added the new column with drug Name")
  colnms <- c("Drug_Name", colnames(drug_data)[-ncol(drug_data)])
  #print(colnms)
  #print(colnames(drug_data))
  drug_data <- drug_data[,colnms]
  print(paste0("column name ordered ", drug_index))
  drug_data <- as.data.frame(drug_data)
  rownames(drug_data) <- drugs
  #drug_data <- drug_data[,-1]
  drug_data$class <- left_join(drug_data, drugs_id, by = "Drug_Name")$class
  Hospital_ade[[drug_index]] <- drug_data
}

###############  set a factor for all three variables
for(i in 1:length(Hospital_ade)){
   for(j in 2016:2022){
        var1 <- paste0("DE_status_", j) 
        Hospital_ade[[i]][[var1]] <- factor(Hospital_ade[[i]][[var1]], levels = c("Over estimate", "Under estimate","Equivalent")) 
   }
}

#### subset the only the required column from the FI_data ###
DE_status <- list()
for(i in 1:length(FI_data)){
  data <- Hospital_ade[[i]]
  col1 <- grep("DE_status", colnames(data), value =T)
  data <- data[,c("Drug_Name", col1)]
  data$class <- left_join(data, drugs_id, by = "Drug_Name")$class
  DE_status[[i]] <- data
}

################# subset the data frame into cardi and endocrine ##########
cardio <- list()
endo <- list()
for(i in 1:length(Hospital_ade)){
  cardio[[i]] <- subset(DE_status[[i]], class == "cardio")
  endo[[i]] <- subset(DE_status[[i]], class == "endo")
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
wide_cardio <- list()
for(i in 1:length(cardio)){
  hospital <- list()
  for(j in 1:length(years)){
    vara <- paste0("DE_status_", years[j])
    print(vara)
    data <- cardio[[i]][,c("Drug_Name", vara)]
    hos <- paste0(hospital_id[i], "_", years[j])
    data <- subset(data, Drug_Name %in% result_cardio[[hos]])
    colnames(data) <- c("Drug_Name", "var1")
    out <- data %>% group_by(var1) %>% summarize(var2 = n())
    out$var1 <- factor(out$var1, levels = c("Over estimate", "Under estimate", "Equivalent"))
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
    vara <- paste0("DE_status_", years[j])
    print(vara)
    data <- endo[[i]][,c("Drug_Name", vara)]
    hos <- paste0(hospital_id[i], "_", years[j])
    data <- subset(data, Drug_Name %in% result_endo[[hos]])
    colnames(data) <- c("Drug_Name", "var1")
    #print(data)
    out <- data %>% group_by(var1) %>% summarize(var2 = n())
    out$var1 <- factor(out$var1, levels = c("Over estimate", "Under estimate", "Equivalent"))
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
wide_endo
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
extra_column <- "Equivalent"
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



########## calculate the Average ##############
for(i in 1:length(wide_cardio)){
     ### process the wide cardio list ####
      data <- wide_cardio[[i]]
      new_row <- data %>%
        dplyr::summarise(across(c(Over_estimate, Under_estimate, Equivalent, FAC), ~ceiling(mean(., na.rm = TRUE))),  # Compute the mean for numeric columns
                  Year = "Average")
      data <- rbind(data, new_row)
      wide_cardio[[i]] <- data
      #### process the wide endo list #####
      data <- wide_endo[[i]]
      new_row <- data %>%
        dplyr::summarise(across(c(Over_estimate, Under_estimate, Equivalent, FAC), ~ceiling(mean(., na.rm = TRUE))),  # Compute the mean for numeric columns
                         Year = "Average")
      data <- rbind(data, new_row)
      wide_endo[[i]] <- data
}

wide_cardio
wide_endo
### calculate the average #############
final_cardio <- list()
final_endo <- list()
cardio_wb <- createWorkbook()
endo_wb <- createWorkbook()
for(i in 1:length(wide_cardio)){
  ##### process the wide cardio list ####
  data <- wide_cardio[[i]]
  data <- data %>% mutate(over_estimate_prop = Over_estimate/FAC*100, 
                        under_estimate_prop = Under_estimate/FAC*100,
                        equivalent_prop = Equivalent/FAC*100)  # Corrected column name
  final_cardio[[i]] <- data
  addWorksheet(wb = cardio_wb, sheetName = hospital_id[i])
  writeData(wb = cardio_wb, sheet = i, x = final_cardio[[i]])
}
final_cardio
saveWorkbook(cardio_wb, "Output/cardio_summary_FA_status_all_hospital.xlsx", overwrite = TRUE) ## save the cardio summary ###

for(i in 1:length(wide_endo)){
  #### process the wide endo list #####
  data <- wide_endo[[i]]
  data <- data %>% mutate(over_estimate_prop = Over_estimate/FAC*100, 
                        under_estimate_prop = Under_estimate/FAC*100,
                        equivalent_prop = Equivalent/FAC*100)  # Corrected column name
  final_endo[[i]] <- data
  addWorksheet(wb = endo_wb, sheetName = hospital_id[i])
  writeData(wb = endo_wb, sheet = i, x = final_endo[[i]])
}
final_endo
saveWorkbook(endo_wb, "Output/endo_summary_FA_status_all_hospital.xlsx", overwrite = TRUE) ### save the endo summary ####
