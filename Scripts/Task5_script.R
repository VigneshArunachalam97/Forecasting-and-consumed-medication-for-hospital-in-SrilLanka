# import the required library # 
library(data.table)
library(readxl)
library(dplyr)

source("Script/Functions.R")
############ import the objective 1 results ####
obje1_result <- read.csv("Output/task_1_forecasted_consumed_hospitals_Mar_02_24.csv", header = T)
head(obje1_result)
dim(obje1_result)
str(obje1_result)

#### step 1 - you have to repeat the first task for drug class 
#### step 2 - Number of drugs are in the over stock period in the each year ####
#### step 3 - estimate the prevalence and make a single data 
############## ############

#### import the drug file ##
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
hospital_wide <- readRDS("hospital_wide_edited_subset_final_Mar_02_24.RDS")
length(hospital_wide)
lapply(hospital_wide, dim)

cardio_vas <- list()
endo <- list()
for(i in 1:length(hospital_wide)){
    hospital_wide[[i]] <- as.data.frame(hospital_wide[[i]])
    #hospital_wide[[i]]$class_drug <- left_join(hospital_wide[[i]], drugs_id, by = "STOCK_CODE")$class  
    #hospital_wide[[i]]$class <- as.factor(hospital_wide[[i]]$class)
    hospital_wide[[i]]$class <- c(rep("cardio", 24), rep("endo", 7))
    cardio_vas[[i]] <- subset(hospital_wide[[i]], class == "cardio")
    endo[[i]] <- subset(hospital_wide[[i]], class == "endo")
}
######## structure of cardio_vas and endocrine ####
length(cardio_vas)
names(cardio_vas) <- hospital_id
lapply(cardio_vas, dim)
length(endo)
names(endo) <- hospital_id
lapply(endo, dim)
############ Period 1 #################
#### calculate for cardiovascular drug ###
## FAC - should present in both FA and CS ####
#### calculate fac for the hospitals ####
period1 <- 2015:2019
fac_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period1)+1))
colnames(fac_data) <- c("HID", paste("FAC_", period1, sep =""))
fac_data$HID <- hospital_id
rownames(fac_data) <- fac_data$HID
head(fac_data)

for(year in period1) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    cardio_vas[[idname]] <- as.data.frame(cardio_vas[[idname]])
    count <- FAC_count(cardio_vas[[idname]], variable1, variable2)
    fac_data[idname, paste0("FAC_", year)] <- count
  }
}
fac_data$HID <- as.character(fac_data$HID)
fac_data_cardio_p1 <- fac_data
head(fac_data_cardio_p1)
#saveRDS(fac_data, "FAC_data_year_hospital.RDS") #### save as R object ####

#### calculate DFBC for the hospitals ####
dfbc_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period1)+1))
colnames(dfbc_data) <- c("HID", paste("DFBC_", period1, sep =""))
dfbc_data$HID <- hospital_id
rownames(dfbc_data) <- dfbc_data$HID
head(dfbc_data)

for(year in period1) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- DFBC_count(cardio_vas[[idname]], variable1, variable2)
    dfbc_data[idname, paste0("DFBC_", year)] <- count
  }
}
dfbc_data$HID <- as.factor(dfbc_data$HID)
dfbc_data_cardio_p1 <- dfbc_data
#saveRDS(dfbc_data, "DFBC_data_year_hospital.RDS") #### save as R object ####

#### period 2 ########
period2 <- 2020:2022
fac_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period2)+1))
colnames(fac_data) <- c("HID", paste("FAC_", period2, sep =""))
fac_data$HID <- hospital_id
rownames(fac_data) <- fac_data$HID
head(fac_data)

for(year in period2) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- FAC_count(cardio_vas[[idname]], variable1, variable2)
    fac_data[idname, paste0("FAC_", year)] <- count
  }
}
fac_data$HID <- as.character(fac_data$HID)
fac_data_cardio_p2 <- fac_data
#saveRDS(fac_data, "FAC_data_year_hospital.RDS") #### save as R object ####

#### calculate DFBC for the hospitals ####
dfbc_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period2)+1))
colnames(dfbc_data) <- c("HID", paste("DFBC_", period2, sep =""))
dfbc_data$HID <- hospital_id
rownames(dfbc_data) <- dfbc_data$HID
head(dfbc_data)

for(year in period2) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- DFBC_count(cardio_vas[[idname]], variable1, variable2)
    dfbc_data[idname, paste0("DFBC_", year)] <- count
  }
}
dfbc_data$HID <- as.factor(dfbc_data$HID)
dfbc_data_cardio_p2 <- dfbc_data

### combine the data ############# 
cardio_data_p1 <- fac_data_cardio_p1
cardio_data_p1 <- left_join(fac_data_cardio_p1, dfbc_data_cardio_p1, by = "HID")
for(i in period1){
  cardio_data_p1[[paste0("Total_", i)]] <-  cardio_data_p1[[paste0("FAC_", i)]] + cardio_data_p1[[paste0("DFBC_", i)]]
  print(dim(cardio_data_p1))
 }
rownames(cardio_data_p1) <- cardio_data_p1$HID
#### calculate the OS drug for each year and hospital ###
names(cardio_vas)
for(i in period1){
  for(idname in hospital_id){
    variable1 <- paste0("OS_", idname, "_", i)
    print(variable1)
    count <- os_count(cardio_vas[[idname]], variable1)
    cardio_data_p1[idname, paste0("OS_no_", i)] <- count
  }
}
View(cardio_data_p1)

###### calculate prevalance ####### 
for(i in period1){
  cardio_data_p1[[paste0("prevalance_", i)]] <- round((cardio_data_p1[[paste0("OS_no_", i)]]/cardio_data_p1[[paste0("Total_", i)]])*100, 2)
}
View(cardio_data_p1)
cardio_data_p1$class <- "Cardiovascular drugs"
### period 2 ###
cardio_data_p2 <- fac_data_cardio_p2
cardio_data_p2 <- left_join(fac_data_cardio_p2, dfbc_data_cardio_p2, by = "HID")
for(i in period2){
  cardio_data_p2[[paste0("Total_", i)]] <-  cardio_data_p2[[paste0("FAC_", i)]] + cardio_data_p2[[paste0("DFBC_", i)]]
  print(dim(cardio_data_p2))
}
rownames(cardio_data_p2) <- cardio_data_p2$HID
#### calculate the OS drug for each year and hospital ###
names(cardio_vas)
for(i in period2){
  for(idname in hospital_id){
    variable1 <- paste0("OS_", idname, "_", i)
    print(variable1)
    count <- os_count(cardio_vas[[idname]], variable1)
    cardio_data_p2[idname, paste0("OS_no_", i)] <- count
  }
}
View(cardio_data_p2)
###### calculate prevalance ####### 
for(i in period2){
  cardio_data_p2[[paste0("prevalance_", i)]] <- round((cardio_data_p2[[paste0("OS_no_", i)]]/cardio_data_p2[[paste0("Total_", i)]])*100, 2)
}
View(cardio_data_p2)
cardio_data_p2$class <- "Cardiovascular drugs"

#### calculate for endocrine drugs ###
############ period 1 #################
## FAC - should present in both FA and CS ####
#### calculate fac for the hospitals ####
period1 <- 2015:2019
fac_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period1)+1))
colnames(fac_data) <- c("HID", paste("FAC_", period1, sep =""))
fac_data$HID <- hospital_id
rownames(fac_data) <- fac_data$HID
head(fac_data)

for(year in period1) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- FAC_count(endo[[idname]], variable1, variable2)
    fac_data[idname, paste0("FAC_", year)] <- count
  }
}
fac_data$HID <- as.character(fac_data$HID)
fac_data_endo_p1 <- fac_data
#saveRDS(fac_data, "FAC_data_year_hospital.RDS") #### save as R object ####

#### calculate DFBC for the hospitals ####
dfbc_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period1)+1))
colnames(dfbc_data) <- c("HID", paste("DFBC_", period1, sep =""))
dfbc_data$HID <- hospital_id
rownames(dfbc_data) <- dfbc_data$HID
head(dfbc_data)

for(year in period1) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- DFBC_count(endo[[idname]], variable1, variable2)
    dfbc_data[idname, paste0("DFBC_", year)] <- count
  }
}
dfbc_data$HID <- as.factor(dfbc_data$HID)
dfbc_data_endo_p1 <- dfbc_data
#saveRDS(dfbc_data, "DFBC_data_year_hospital.RDS") #### save as R object ####

#### period 2 ########
period2 <- 2020:2022
fac_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period2)+1))
colnames(fac_data) <- c("HID", paste("FAC_", period2, sep =""))
fac_data$HID <- hospital_id
rownames(fac_data) <- fac_data$HID
head(fac_data)

for(year in period2) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- FAC_count(endo[[idname]], variable1, variable2)
    fac_data[idname, paste0("FAC_", year)] <- count
  }
}
fac_data$HID <- as.character(fac_data$HID)
fac_data_endo_p2 <- fac_data
#saveRDS(fac_data, "FAC_data_year_hospital.RDS") #### save as R object ####

#### calculate DFBC for the hospitals ####
dfbc_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(period2)+1))
colnames(dfbc_data) <- c("HID", paste("DFBC_", period2, sep =""))
dfbc_data$HID <- hospital_id
rownames(dfbc_data) <- dfbc_data$HID
head(dfbc_data)

for(year in period2) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- DFBC_count(endo[[idname]], variable1, variable2)
    dfbc_data[idname, paste0("DFBC_", year)] <- count
  }
}
dfbc_data$HID <- as.factor(dfbc_data$HID)
dfbc_data_endo_p2 <- dfbc_data

######### combine the data ###
endo_data_p1 <- fac_data_endo_p1
endo_data_p1 <- left_join(fac_data_endo_p1, dfbc_data_endo_p1, by = "HID")
for(i in period1){
  endo_data_p1[[paste0("Total_", i)]] <-  endo_data_p1[[paste0("FAC_", i)]] + endo_data_p1[[paste0("DFBC_", i)]]
  print(dim(endo_data_p1))
}
rownames(endo_data_p1) <- endo_data_p1$HID
#### calculate the OS drug for each year and hospital ###
names(endo)
for(i in period1){
  for(idname in hospital_id){
    variable1 <- paste0("OS_", idname, "_", i)
    print(variable1)
    count <- os_count(endo[[idname]], variable1)
    endo_data_p1[idname, paste0("OS_no_", i)] <- count
  }
}
View(endo_data_p1)
###### calculate prevalance ####### 
for(i in period1){
  endo_data_p1[[paste0("prevalance_", i)]] <- round((endo_data_p1[[paste0("OS_no_", i)]]/endo_data_p1[[paste0("Total_", i)]])*100, 2)
}
View(endo_data_p1)
endo_data_p1$class <- "Endocrine drugs"

### period 2 ###
endo_data_p2 <- fac_data_endo_p2
endo_data_p2 <- left_join(fac_data_endo_p2, dfbc_data_endo_p2, by = "HID")
for(i in period2){
  endo_data_p2[[paste0("Total_", i)]] <-  endo_data_p2[[paste0("FAC_", i)]] + endo_data_p2[[paste0("DFBC_", i)]]
  print(dim(endo_data_p2))
}
rownames(endo_data_p2) <- endo_data_p2$HID
#### calculate the OS drug for each year and hospital ###
names(endo)
for(i in period2){
  for(idname in hospital_id){
    variable1 <- paste0("OS_", idname, "_", i)
    print(variable1)
    count <- os_count(endo[[idname]], variable1)
    endo_data_p2[idname, paste0("OS_no_", i)] <- count
  }
}
View(endo_data_p2)
###### calculate prevalance ####### 
for(i in period2){
  endo_data_p2[[paste0("prevalance_", i)]] <- round((endo_data_p2[[paste0("OS_no_", i)]]/endo_data_p2[[paste0("Total_", i)]])*100, 2)
}
View(endo_data_p2)

endo_data_p2$class <- "Endocrine drugs"

############### do with total data without spliting the drug label #####
head(obje1_result)
col1 <- grep("^FAC_", colnames(obje1_result), value = T)
col1
col2 <- grep("DFBC_", colnames(obje1_result), value = T)
col2
req_col <- c("HID", col1, col2)
obje1_result <- as.data.frame(obje1_result)

total_set <- obje1_result[,colnames(obje1_result) %in% req_col] ### subset the required columns ####
dim(total_set)

for(i in years){
  total_set[[paste0("Total_", i)]] <-  total_set[[paste0("FAC_", i)]] + total_set[[paste0("DFBC_", i)]]
  print(dim(total_set))
}
rownames(total_set) <- total_set$HID

###### combine the OS data from the total dataset #####
for(i in years){
  for(idname in hospital_id){
    variable1 <- paste0("OS_", idname, "_", i)
    print(variable1)
    count <- os_count(hospital_wide[[idname]], variable1)
    total_set[idname, paste0("OS_no_", i)] <- count
  }
}

View(total_set)
###### calculate prevalance ####### 
for(i in years){
  total_set[[paste0("prevalance_", i)]] <- round((total_set[[paste0("OS_no_", i)]]/total_set[[paste0("Total_", i)]])*100, 2)
}
View(total_set)
total_set$class <- "Total"
#total_set$HID <- paste0("total_", total_set$HID)
colnames(total_set)

# Columns for the first data frame (2015 to 2019)
cols_2015_to_2019 <- c("HID", "class", paste0("FAC_", 2015:2019), paste0("DFBC_", 2015:2019), paste0("Total_", 2015:2019), paste0("OS_no_", 2015:2019), paste0("prevalance_", 2015:2019))
period1 <- total_set[, cols_2015_to_2019]

# Columns for the second data frame (2020 to 2022)
cols_2020_to_2022 <- c("HID", "class", paste0("FAC_", 2020:2022), paste0("DFBC_", 2020:2022), paste0("Total_", 2020:2022), paste0("OS_no_", 2020:2022), paste0("prevalance_", 2020:2022))
period2 <- total_set[, cols_2020_to_2022]
period2

########### combine the period 1 all data #####
cardio_endo_p1 <- rbind(cardio_data_p1, endo_data_p1)
cardio_endo_total_p1 <- rbind(cardio_endo_p1, period1)

#View(cardio_endo_total_p1)

col_names <- colnames(cardio_endo_total_p1)[-c(1,27)]
col_names
sorted_col_names <- col_names[order(as.numeric(sub(".*_(\\d+)$", "\\1", col_names)))]
sorted_col_names

cardio_endo_total_p1 <- cardio_endo_total_p1[,c("HID", "class", sorted_col_names)]

cardio_endo_total_p1$row <- paste0(cardio_endo_total_p1$HID, "_", cardio_endo_total_p1$class)
rownames(cardio_endo_total_p1) <- cardio_endo_total_p1$row
View(cardio_endo_total_p1)
class <- unique(cardio_endo_total_p1$class)
class
row_order <- unlist(lapply(hospital_id, function(x) rep(paste(x, class, sep="_"), each = 1)))
row_order
cardio_endo_total_p1 <- cardio_endo_total_p1[row_order,]
View(cardio_endo_total_p1)
cardio_endo_total_p1 <- cardio_endo_total_p1[,-ncol(cardio_endo_total_p1)]


head(cardio_endo_total_p1)
cardio_endo_total_p1 <- as.data.table(cardio_endo_total_p1)
head(cardio_endo_total_p1)
fac <- paste("FAC", 2016:2019, sep = "_")
cardio_endo_total_p1[,Average_FAC:= ceiling(rowSums(.SD, na.rm = TRUE)/4), .SDcols = fac]
dfbc <- paste("DFBC", 2016:2019, sep = "_")
cardio_endo_total_p1[,Average_DFBC:= ceiling(rowSums(.SD, na.rm = TRUE)/4), .SDcols = dfbc]
os <- paste("OS_no", 2016:2019, sep = "_")
cardio_endo_total_p1$AVerage_total <- cardio_endo_total_p1$Average_FAC + cardio_endo_total_p1$Average_DFBC
cardio_endo_total_p1[,Average_OS:= ceiling(rowSums(.SD, na.rm = TRUE)/4), .SDcols = os]
head(cardio_endo_total_p1)
cardio_endo_total_p1$Avergae_prevalance <- round(cardio_endo_total_p1$Average_OS/cardio_endo_total_p1$AVerage_total*100, 2)
write.csv(cardio_endo_total_p1, paste0("Output/task_5_prevalence_period1_", format(Sys.Date(),"%b_%d_%y"), ".csv"), row.names = F, quote = F)


########### combine the period 2 all data #####
cardio_endo_p2 <- rbind(cardio_data_p2, endo_data_p2)
cardio_endo_total_p2 <- rbind(cardio_endo_p2, period2)

View(cardio_endo_total_p1)
ncol(cardio_endo_total_p2)
col_names <- colnames(cardio_endo_total_p2)[-c(1,17)]
col_names
sorted_col_names <- col_names[order(as.numeric(sub(".*_(\\d+)$", "\\1", col_names)))]
sorted_col_names

cardio_endo_total_p2 <- cardio_endo_total_p2[,c("HID", "class", sorted_col_names)]
cardio_endo_total_p2$row <- paste0(cardio_endo_total_p2$HID, "_", cardio_endo_total_p2$class)
rownames(cardio_endo_total_p2) <- cardio_endo_total_p2$row
View(cardio_endo_total_p2)
class <- unique(cardio_endo_total_p2$class)
class
row_order <- unlist(lapply(hospital_id, function(x) rep(paste(x, class, sep="_"), each = 1)))
row_order
cardio_endo_total_p2 <- cardio_endo_total_p2[row_order,]
cardio_endo_total_p2 <- cardio_endo_total_p2[,-ncol(cardio_endo_total_p2)]
View(cardio_endo_total_p2)

head(cardio_endo_total_p2)
cardio_endo_total_p2 <- as.data.table(cardio_endo_total_p2)
head(cardio_endo_total_p2)
fac <- paste("FAC", 2020:2022, sep = "_")
fac
cardio_endo_total_p2[,Average_FAC:= ceiling(rowSums(.SD, na.rm = TRUE)/4), .SDcols = fac]
dfbc <- paste("DFBC", 2020:2022, sep = "_")
cardio_endo_total_p2[,Average_DFBC:= ceiling(rowSums(.SD, na.rm = TRUE)/4), .SDcols = dfbc]
os <- paste("OS_no", 2020:2022, sep = "_")
cardio_endo_total_p2$AVerage_total <- cardio_endo_total_p1$Average_FAC + cardio_endo_total_p1$Average_DFBC
cardio_endo_total_p2[,Average_OS:= ceiling(rowSums(.SD, na.rm = TRUE)/4), .SDcols = os]
head(cardio_endo_total_p2)
cardio_endo_total_p2$Avergae_prevalance <- round(cardio_endo_total_p2$Average_OS/cardio_endo_total_p2$AVerage_total*100, 2)
head(cardio_endo_total_p2)
write.csv(cardio_endo_total_p2, paste0("Output/task_5_prevalence_period2_", format(Sys.Date(),"%b_%d_%y"), ".csv"), row.names = F, quote = F)




