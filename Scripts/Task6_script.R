# import the required library # 
library(data.table)
library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)

source("Script/Functions.R")
##### get all data ####
hospital_wide <- readRDS("hospital_wide_edited_subset_final_Mar_02_24.RDS")
length(hospital_wide)
lapply(hospital_wide, dim)


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

###### calculate the AAC #######
years <- seq(2015, 2022, 1)
period1 <- seq(2015, 2019, 1)
period2 <- seq(2020, 2022, 1)


for(i in 1:length(hospital_wide)) {
  data <- hospital_wide[[i]]
  idname <- hospital_id[i]
   for(y in years) {
     var1 <- paste0("CS_", idname, "_", y)
     var2 <- paste0("OS_", idname, "_", y)
     out <- paste0("AAC_", y)
     data[[out]] <- data[[var1]]/(12 - data[[var2]]) 
   }
  hospital_wide[[i]] <- data
}

AAC_data <- list()
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  cols <- grep("AAC", colnames(data), value = T)
  data <- data[,c("STOCK_CODE", "Drug_Name", "class", cols)]
  data <- as.data.table(data)
  AAC_data[[i]] <- data
  
}
names(AAC_data) <- hospital_id


######## subset by medicine and merge it #####
drug_dataframes <- vector("list", length = 31)
drugs <- os_dataset[[1]]$Drug_Name
drug_order <- paste("drugs_", 1:31, sep = "")
ord <- data.frame(sheetname = drug_order, Drugs = drugs)
ord$STOCK_cODE <- merge(ord, hospital_wide[[1]], by.y = "Drug_Name", by.x = "Drugs")$STOCK_CODE
write.table(ord, "Output/druglist_order.tsv", sep = "\t", row.names = F, quote = F)
# Iterate over each drug
aac_drug_dataframes <- list()
for (drug_index in 1:31) {
  drug_data <- data.frame()
  for (df_index in 1:length(AAC_data)) {
    colnames(AAC_data[[df_index]]) <- c("Drug_Name", paste("AAC_", seq(2015, 2022,1), sep = ""))
    drug_data_df <- AAC_data[[df_index]][drug_index, ]
    drug_data <- rbind(drug_data, drug_data_df)
  }
  drug_data <- drug_data[,-1]
  drug_data$HID <- hospital_id
  drug_data <- drug_data[,c("HID", paste("AAC_", seq(2015, 2022,1), sep = ""))]
  drug_data <- as.data.frame(drug_data)
  rownames(drug_data) <- hospital_id
  drug_data <- drug_data[,-1]
  aac_drug_dataframes[[drug_index]] <- drug_data
}

############# calculate Average, minimum, maximum ###########
addition <- c("Minimum", "Maximum", "Average", "StandardDeviation", "Quartile1", "Median", "Quartile3")
wb <- createWorkbook()
calcuated_aac_dataframes <- list()
for(i in 1:length(aac_drug_dataframes)){
  data <- aac_drug_dataframes[[i]]
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
  #data[, c(paste0("AAC_per_", 2015:2022)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("AAC_", 2015:2022)]
  #data[14:20, 17:24] <- NA
  colo <- colnames(data)[-c(1, 10:16)]
  ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
  data <- as.data.frame(data)
  data <- data[,ordered]
  calcuated_aac_dataframes[[i]] <- data
  addWorksheet(wb = wb, sheetName = paste0("drugs_", i))
  writeData(wb = wb, sheet = i, x = calcuated_aac_dataframes[[i]])
}

saveWorkbook(wb, "Output/AAC_drug_wise_all_Year_and_hospitals.xlsx", overwrite = TRUE)

########## subset the data form 2015 to 2019 ##### period 1 ###
period1 <- createWorkbook()
for(i in 1:length(aac_drug_dataframes)) {
  data <- aac_drug_dataframes[[i]]
  data <- data[,paste("AAC_", 2015:2019, sep = "")]
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
  #data[, c(paste0("Y1_per_", 2015:2019)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2015:2019)]
  #data[14:20, 14:18] <- NA
  colo <- colnames(data)[-c(1, 7:13)]
  ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
  data <- as.data.frame(data)
  data <- data[,ordered]
  
  calcuated_aac_dataframes[[i]] <- data
  addWorksheet(wb = period1, sheetName = paste0("drugs_", i))
  writeData(wb = period1, sheet = i, x = calcuated_aac_dataframes[[i]])
}

tail(calcuated_aac_dataframes[[1]], 9)
saveWorkbook(period1, "Output/AAC_drug_wise_all_Year_and_hospitals_subset_period1.xlsx", overwrite = TRUE)

########## subset the data form 2020 to 2022 ##### period 2 ###
period2 <- createWorkbook()
for(i in 1:length(aac_drug_dataframes)) {
  data <- aac_drug_dataframes[[i]]
  data <- data[,paste("AAC_", 2020:2022, sep = "")]
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
  #data[, c(paste0("Y1_per_", 2020:2022)) := lapply(.SD, function(x) x/12*100), .SDcols = paste0("Y1_", 2020:2022)]
  #data[14:20, 12:14] <- NA
  colo <- colnames(data)[-c(1, 5:11)]
  ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
  data <- as.data.frame(data)
  data <- data[,ordered]
  calcuated_aac_dataframes[[i]] <- data
  addWorksheet(wb = period2, sheetName = paste0("drugs_", i))
  writeData(wb = period2, sheet = i, x = calcuated_aac_dataframes[[i]])
}
tail(calcuated_aac_dataframes[[1]], 9)
saveWorkbook(period1, "Output/AAC_drug_wise_all_Year_and_hospitals_subset_period2.xlsx", overwrite = TRUE)


########### plots ################
for(i in 1:length(AAC_data)){
  data <- AAC_data[[i]]
  #data <- data[,-12]
  colnames(data) <- c("STOCK_CODE", "Drug_Name", "class", paste(hospital_id[i], 2015:2022, sep="_"))
  data <- data[,-c(1,3)]
  AAC_data[[i]] <- data
}

head(AAC_data[[1]])

#### merge the data #####
complete_aac <- AAC_data[[1]]
for(i in 2:length(AAC_data)){
  complete_aac <- merge(complete_aac, AAC_data[[i]], by = "Drug_Name", all = TRUE)
  print(dim(complete_aac))
}

write.csv(complete_aac, "data/AAC_data_all_hospitals_drugs.csv", row.names = F, quote = F)
######## convert into long data #####

long_data <- melt(complete_aac, id.vars = "Drug_Name", variable.name = "Year", value.name = "Stock")
long_data$Stock_code <- left_join(long_data, hospital_wide[[1]], by = "Drug_Name")$STOCK_CODE
spl <- str_split_fixed(long_data$Year, "_", 3)
#long_data$HID_full <- paste0(spl[,1], "_", spl[,2])
#long_data$HID <- spl[,1]
#long_data$Years <- spl[,3]
long_data$Years <- gsub(".*_(\\d{4})$", "\\1", long_data$Year)
long_data$HID_full <- paste0(spl[,1],"_", spl[,2])
long_data <- as.data.frame(long_data)
#colnames(long_data)[colnames(long_data)  == "HID"] <- "HID_old"
colnames(long_data)[colnames(long_data)  == "HID_full"] <- "HID"
long_data$HID <- as.character(long_data$HID)
long_data$HID_new <- left_join(long_data, hos_id, by = "HID")$new_id
head(long_data)
long_data$log_AAC <- log10(long_data$Stock)

write.csv(complete_aac, "data/AAC_data_all_hospitals_drugs_longFormat.csv", row.names = F, quote = F)


####### set up the color and drug names ####
colr <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
          "#9467bd","#8c564b","#e377c2","#7f7f7f",
          "#bcbd22","#17becf","#aec7e8","#ffbb78",
          "#98df8a")

names(colr) <- unique(long_data$HID_new)
drug_name <- gsub("\\/", "_", gsub("Tab.", "", gsub(" ", "", drugs)))



############ plot drugs x axis is year and y axis is out of stock across the plot #######
##### with title #############
if(dir.exists("Plots/AAC_figures_title") == FALSE){
  dir.create("Plots/AAC_figures_title")
}
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
    ylab("Adjusted Average consumptions") +
    ggtitle(paste0("Adjusted Average Consumption in ", drug_name[drug], " from 2015 to 2022")) +
    theme(legend.position = "bottom", 
          plot.title = element_text(size = 14, hjust = 0.5)) 
  png(paste0("Plots/AAC_figures_title/AAC_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}
##### without title #############
if(dir.exists("Plots/AAC_figures_Wotitle") == FALSE){
  dir.create("Plots/AAC_figures_Wotitle")
}
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
    ylab("Adjusted Average consumptions") +
    #ggtitle(paste0("Adjusted Average Consumption in ", drug_name[drug], " from 2015 to 2022")) +
    theme(legend.position = "bottom", 
          plot.title = element_blank()) 
  png(paste0("Plots/AAC_figures_Wotitle/AAC_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}








