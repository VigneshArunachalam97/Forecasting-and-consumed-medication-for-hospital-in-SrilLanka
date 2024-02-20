### Task 1 in data analysis #####
### The summary of the medicines that have been forecasted and consumed by all the hospital need to be presented in a table (Table) #####

# import the required library # 
library(data.table)
library(readxl)
library(dplyr)
########## Function created for the analysis ####
FAC_count <- function(data, var1, var2) {
    count <- 0  ## This function will calculate the FAC count for all the hospital year wise and save as a dataframe###
  
  for(i in 1:nrow(data)) {
    if (!is.na(data[i,var1]) && 
        !is.na(data[i,var2]) &&
        data[i,var1] != 0 && 
        data[i,var2] != 0) {
      count <- count + 1
    }
  }
  
  return(count)
}

DFBC_count <- function(data, var1, var2) {
  count <- 0 #####This function calculated the DFBC count for all hopsital year wise and save as a data##
  
  for(i in 1:nrow(data)) {
    if (!is.na(data[i,var1]) && 
        !is.na(data[i,var2]) &&
        data[i,var1] == 0 && 
        data[i,var2] > 0) {
      count <- count + 1
    }
  }
  
  return(count)
}
  
FBDC_count <- function(data, var1, var2) {
  count <- 0 #####This function calculated the FBDC count for all hopsital year wise and save as a data##
  
  for(i in 1:nrow(data)) {
    if (!is.na(data[i,var1]) && 
        !is.na(data[i,var2]) &&
        data[i,var1] > 0 && 
        data[i,var2] == 0) {
      count <- count + 1
    }
  }
  
  return(count)
}


DFAC_count <- function(data, var1, var2) {
  count <- 0
  for(i in 1:nrow(data)) {
    if ((is.na(data[i, var1]) || data[i, var1] == 0) &&
        (is.na(data[i, var2]) || data[i, var2] == 0)) {
      count <- count + 1
    }
  }
  
  return(count)
}

### set the working directory 
wd <- "C:/Users/n11142006/OneDrive - Queensland University of Technology/PhD/Nimmi_data/"
setwd(wd)
### To get a sheet names##
sheetname <- excel_sheets("data/Data_Hospital wise_18.01.2024combined_toVignesh.xlsx")[1:35] ### #to get the sheet name and we will use to import the later
print(sheetname)

### import the files in loop ######
hospital_wide <- list()
for(i in sheetname) {
  data <- read_excel("data/Data_Hospital wise_18.01.2024combined_toVignesh.xlsx", sheet = i)
  data <- data[1:57,]
  data[,3:ncol(data)] <- lapply(data[,3:ncol(data)], as.numeric)
  hospital_wide[[i]] <- as.data.frame(data)
}
saveRDS(hospital_wide, "hospital_Wide_data_all_hospitals_unedited.RDS") ### Check point 1 - save the data in RDS object
#hospital_wide <- readRDS("hospital_Wide_data_all_hospitals.RDS")
### print the structure of each data ####
#str(hospital_wide)
#View(hospital_wide[[2]])
#print(names(hospital_wide))

colnames(hospital_wide[[4]])[colnames(hospital_wide[[4]]) == "FA_H3_0070_2015"] <- "FA_H4_0080_2015"
colnames(hospital_wide[[15]])[colnames(hospital_wide[[15]]) == "FA15_H15_0230_2015"] <- "FA_H15_0230_2015"
hospital_wide[[22]]$FA_H22_0300_2015 <- NA
colnames(hospital_wide[[15]]) <- gsub("FA15", "FA", colnames(hospital_wide[[15]]))
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2016"] <- "FA_H31_2020_2016"
colnames(hospital_wide[[31]])[colnames(hospital_wide[[31]]) == "FA_H31_2010_2015"] <- "FA_H31_2020_2015"
colnames(hospital_wide[[32]])[colnames(hospital_wide[[32]]) == "CS-H32_6010_2016"] <- "CS_H32_6010_2016"
colnames(hospital_wide[[4]])[colnames(hospital_wide[[4]]) == "FA_H3_0070_2019"] <- "FA_H4_0080_2019"
colnames(hospital_wide[[32]])[colnames(hospital_wide[[32]]) == "CS-H32_6010_2019"] <- "CS_H32_6010_2019"
colnames(hospital_wide[[32]])[colnames(hospital_wide[[32]]) == "CS-H32_6010_2022"] <- "CS_H32_6010_2022"

#### i observed few blank columns in the data and want to remove those blank columns ####
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

saveRDS(hospital_wide, "hospital_Wide_data_all_hospitals_edited_ReadyforAnalysis.RDS") ###check point 2 - save the data in RDS object

### calculate the required variables for each data ####
hospital_id <- sheetname ### hospital name #####
years <- seq(2015, 2022, 1) ### years data we have
FA_colnames <- paste("FA", "_", hospital_id, "_", years, sep = "")
CS_colnames <- paste("CS", "_", hospital_id, "_", years, sep = "")
## FAC - should present in both FA and CS ####


#### calculate fac for the hospitals ####
fac_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(years)+1))
colnames(fac_data) <- c("HID", paste("FAC_", years, sep =""))
fac_data$HID <- hospital_id
rownames(fac_data) <- fac_data$HID
head(fac_data)

for(year in years) {
 for(idname in hospital_id) {
  variable1 <- paste0("FA", "_", idname, "_", year)
  print(variable1)
  variable2 <- paste0("CS", "_", idname, "_", year)
  print(variable2)
  count <- FAC_count(hospital_wide[[idname]], variable1, variable2)
  fac_data[idname, paste0("FAC_", year)] <- count
 }
}
fac_data$HID <- as.character(fac_data$HID)
saveRDS(fac_data, "FAC_data_year_hospital.RDS") #### save as R object ####

#### calculate DFBC for the hospitals ####
dfbc_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(years)+1))
colnames(dfbc_data) <- c("HID", paste("DFBC_", years, sep =""))
dfbc_data$HID <- hospital_id
rownames(dfbc_data) <- dfbc_data$HID
head(dfbc_data)

for(year in years) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- DFBC_count(hospital_wide[[idname]], variable1, variable2)
    dfbc_data[idname, paste0("DFBC_", year)] <- count
  }
}
dfbc_data$HID <- as.factor(dfbc_data$HID)
saveRDS(dfbc_data, "DFBC_data_year_hospital.RDS") #### save as R object ####

#### calculate FBDC for the hospitals ####
fbdc_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(years)+1))
colnames(fbdc_data) <- c("HID", paste("FBDC_", years, sep =""))
fbdc_data$HID <- hospital_id
rownames(fbdc_data) <- fbdc_data$HID
head(fbdc_data)

for(year in years) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- FBDC_count(hospital_wide[[idname]], variable1, variable2)
    fbdc_data[idname, paste0("FBDC_", year)] <- count
  }
}
fbdc_data$HID <- as.character(fbdc_data$HID)
saveRDS(fbdc_data, "FBDC_data_year_hospital.RDS") #### save as R object ####

######### calculate the DFAC for all the hospitals ####
dfac_data <- as.data.frame(matrix(NA, nrow = length(hospital_id), ncol = length(years)+1))
colnames(dfac_data) <- c("HID", paste("DFAC_", years, sep =""))
dfac_data$HID <- hospital_id
rownames(dfac_data) <- dfac_data$HID
head(dfac_data)


for(year in years) {
  for(idname in hospital_id) {
    variable1 <- paste0("FA", "_", idname, "_", year)
    print(variable1)
    variable2 <- paste0("CS", "_", idname, "_", year)
    print(variable2)
    count <- DFAC_count(hospital_wide[[idname]], variable1, variable2)
    dfac_data[idname, paste0("DFAC_", year)] <- count
  }
}
dfac_data$HID <- as.character(dfac_data$HID)
saveRDS(dfac_data, "DFAC_data_year_hospital.RDS") #### save as R object ####


### sort those data and save it in a single excel file ####
#data names - fac_data, dfbc_data, fbdc_data, dfac_data 

data_Whole <- merge(fac_data, dfbc_data, by = "HID")
colnames(data_Whole)
data_Whole <- merge(data_Whole, fbdc_data, by = "HID")
colnames(data_Whole)
data_Whole <- merge(data_Whole, dfac_data, by = "HID")
colnames(data_Whole)

### sort those columns ###
col_names <- colnames(data_Whole)[-1]
col_names
sorted_col_names <- col_names[order(as.numeric(sub(".*_(\\d+)$", "\\1", col_names)))] ### this command line will order the variables based on the year ###
sorted_col_names <- c("HID", sorted_col_names)
data_Whole <- data_Whole[,sorted_col_names]
write.csv(data_Whole, "task_1_forecasted_consumed_hospitals.csv", row.names = F, quote = F)
