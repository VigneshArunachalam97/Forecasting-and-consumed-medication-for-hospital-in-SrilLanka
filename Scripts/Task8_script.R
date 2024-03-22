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


###################### export the FA data from the year #########
FA_data <- list()
for(i in 1:length(hospital_wide)){
  data <- hospital_wide[[i]]
  colns <- grep("FA_",colnames(data), value = T)
  data <- data[,c("Drug_Name", "STOCK_CODE", colns)]
  print(dim(data))
  FA_data[[i]] <- data
}

head(FA_data[[1]])

########### plots ################
for(i in 1:length(FA_data)){
  data <- FA_data[[i]]
  #data <- data[,-12]
  colnames(data) <- c("Drug_Name", "STOCK_CODE", paste(hospital_id[i], 2015:2023, sep="_"))
  data <- data[,-2]
  FA_data[[i]] <- data
}

head(AAC_data[[1]])

#### merge the data #####
complete_fa <- FA_data[[1]]
for(i in 2:length(FA_data)){
  complete_fa <- merge(complete_fa, FA_data[[i]], by = "Drug_Name", all = TRUE)
  print(dim(complete_fa))
}

write.csv(complete_fa, "data/FA_data_all_hospitals_drugs.csv", row.names = F, quote = F)
######## convert into long data #####

long_data <- melt(complete_fa, id.vars = "Drug_Name", variable.name = "Year", value.name = "Stock")
long_data$Stock_code <- left_join(long_data, hospital_wide[[1]], by = "Drug_Name")$STOCK_CODE
spl <- str_split_fixed(long_data$Year, "_", 3)
#long_data$HID_full <- paste0(spl[,1], "_", spl[,2])
#long_data$HID <- spl[,1]
#long_data$Years <- spl[,3]
long_data$Years <- gsub(".*_(\\d{4})$", "\\1", long_data$Year)
long_data$HID_full <- paste0(spl[,1],"_", spl[,2])
long_data <- as.data.frame(long_data)
head(long_data)
#colnames(long_data)[colnames(long_data)  == "HID"] <- "HID_old"
colnames(long_data)[colnames(long_data)  == "HID_full"] <- "HID"
long_data$HID <- as.character(long_data$HID)
long_data$HID_new <- left_join(long_data, hos_id, by = "HID")$new_id
head(long_data)
#long_data$log_AAC <- log10(long_data$Stock)
write.csv(long_data, "data/FA_data_all_hospitals_drugs_longFormat.csv", row.names = F, quote = F)

#### remove the 2023 data ####
long_data <- subset(long_data, Years != 2023)


####### set up the color and drug names ####
colr <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
          "#9467bd","#8c564b","#e377c2","#7f7f7f",
          "#bcbd22","#17becf","#aec7e8","#ffbb78",
          "#98df8a")

names(colr) <- unique(long_data$HID_new)
drug_name <- gsub("\\/", "_", gsub("Tab.", "", gsub(" ", "", drugs)))

############ plot drugs x axis is year and y axis is out of stock across the plot #######
##### with title #############
if(dir.exists("Plots/FA_figures_title") == FALSE){
  dir.create("Plots/FA_figures_title")
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
    ylab("Forecasted Amount") +
    ggtitle(paste0("Forecasted Amount in ", drug_name[drug], " from 2015 to 2022")) +
    theme(legend.position = "bottom", 
          plot.title = element_text(size = 14, hjust = 0.5)) 
  png(paste0("Plots/FA_figures_title/FA_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}
##### without title #############
if(dir.exists("Plots/FA_figures_Wotitle") == FALSE){
  dir.create("Plots/FA_figures_Wotitle")
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
    ylab("Forecasted Amount") +
    #ggtitle(paste0("Adjusted Average Consumption in ", drug_name[drug], " from 2015 to 2022")) +
    theme(legend.position = "bottom", 
          plot.title = element_blank()) 
  png(paste0("Plots/FA_figures_Wotitle/FA_plot_", drug_name[drug], ".png"),  w = 4000, h = 2500, unit = "px", res = 400)
  print(plot)
  dev.off()
}
