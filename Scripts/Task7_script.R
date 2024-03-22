#### import required packages ####
library(data.table)
library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(cowplot)

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


### read the excel file only the required hospital id #### 
#### Assign the column names ###
nam <- list()
for(i in 2015:2022){
  nam[[i]] <- paste("Y1", i,paste0("M", 1:12), sep="_") ## create a column names ##
}
nam <- unlist(nam)
print(nam)

######### import the data 
wb <- createWorkbook()
monthly_data <- list()
for(i in 1:length(hospital_id)){
  data <- read.xlsx("data/Monthly Consumption_substituuted medicines.xlsx", sheet = hospital_id[i])
  data <- data[-1,]
  colnames(data) <- c("STOCK_CODE", "Drug_Name", nam)
  data <- data[,-1]
  monthly_data[[i]] <- data
  addWorksheet(wb = wb, sheetName = hospital_id[i])
  writeData(wb = wb, sheet = i, x = monthly_data[[i]])
}
saveWorkbook(wb, "data/Monthly_consumption.xlsx", overwrite = TRUE)

################# create a different set of the plots ######
drug_sub <- trimws(monthly_data[[1]]$Drug_Name)
drug_sub

pair1 <- c(drug_sub[1], drug_sub[2])
pair2 <- c(drug_sub[3], drug_sub[4])
pair3 <- c(drug_sub[5], drug_sub[6])
pair4 <- c(drug_sub[7:9])
pairs <- list(pair1, pair2, pair3, pair4)

######## convert the data into long format ###########
monthly_long <- list()
for(i in 1:length(monthly_data)) {
  data <- monthly_data[[i]]
  long_data <- melt(data, id.vars = "Drug_Name", variable.name = "Year", value.name = "Stock")
  ad <- str_split_fixed(long_data$Year, pattern = "_", n = 3)
  long_data$Years <- ad[,2]
  long_data$Months <- ad[,3]
  long_data$Months <- gsub("M", "", long_data$Months)
  long_data$Months <- factor(long_data$Months, levels = 1:12)
  long_data$Drug_Name <- trimws(long_data$Drug_Name)
  monthly_long[[i]] <- long_data
}

########### create a plot #
colr <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
          "#9467bd","#8c564b","#e377c2","#7f7f7f",
          "#bcbd22","#17becf","#aec7e8","#ffbb78",
          "#98df8a")

length(drug_sub)
colr_sub <- colr[1:9]
names(colr_sub) <- drug_sub


try <- ggplot(data = monthly_long[[1]], aes(x = Months, y = Stock,  color = Drug_Name, group = Drug_Name)) + 
  geom_point(size = 1.2) + 
  geom_line(size = 1.5, alpha = 0.7) + 
  #facet_grid(cols =  vars(Years), scales = "fixed") + 
  #facet_wrap(~Years, ncol = 8, scales = "fixed", dir = "h", strip.position = "bottom") + 
  scale_color_manual(values = colr_sub, name = "Paired Drugs") + 
  theme_grey() + 
  xlab("Months") +
  ylab("Consumption of Prescribed & Substituted") +
  #ggtitle(paste0("Forecasted Amount in ", drug_name[drug], " from 2015 to 2022")) +
  theme(legend.position = "bottom", 
        plot.title = element_blank())
all_guide_boxes <- get_plot_component(try, "guide-box", return_all = TRUE)

legend_for_plot <- get_legend(try)


for(j in 1:length(monthly_long)){
      plot_list <- list()
      for(i in 1:(length(pairs)-1)){
          subs <- subset(monthly_long[[j]], Drug_Name %in% pairs[[i]])
          #sub <- subset(monthly_long[[1]], Drug_Name == drug_sub[7] | Drug_Name == drug_sub[8] | Drug_Name == drug_sub[9])
          plot <- ggplot(data = subs, aes(x = Months, y = Stock,  color = Drug_Name, group = Drug_Name)) + 
            geom_point(size = 1) + 
            geom_line(size = 1.5, alpha = 0.7) + 
            #facet_grid(cols =  vars(Years), scales = "fixed") + 
            facet_wrap(~Years, ncol = 8, scales = "fixed", dir = "h", strip.position = "bottom") + 
            scale_color_manual(values = colr_sub, name = "Paired Drugs") + 
            theme_grey() + 
            xlab("Months") +
            ylab("Consumption of Prescribed & Substituted") +
            #ggtitle(paste0("Forecasted Amount in ", drug_name[drug], " from 2015 to 2022")) +
            theme(legend.position = "bottom", 
                  plot.title = element_blank(), 
                  axis.text.x = element_blank(), 
                  axis.title.y = element_blank(), 
                  axis.title.x = element_blank(), 
                  axis.ticks = element_blank()) 
          plot_list[[i]] <- plot
      }  
    print("done")
    subs <- subset(monthly_long[[j]], Drug_Name %in% pairs[[4]])
    plot <- ggplot(data = subs, aes(x = Months, y = Stock,  color = Drug_Name, group = Drug_Name)) + 
      geom_point(size = 1) + 
      geom_line(size = 1.5, alpha = 0.7) + 
      #facet_grid(cols =  vars(Years), scales = "fixed") + 
      facet_wrap(~Years, ncol = 8, scales = "fixed", dir = "h", strip.position = "bottom") + 
      scale_color_manual(values = colr_sub, name = "Paired Drugs") + 
      theme_grey() + 
      xlab("Months & Years") +
      ylab("Consumption of Prescribed & Substituted") +
      #ggtitle(paste0("Forecasted Amount in ", drug_name[drug], " from 2015 to 2022")) +
      theme(legend.position = "bottom", 
            plot.title = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 6)) 
    plot1 <- plot_grid(plotlist = plot_list, nrow = 3, rel_widths = 1, rel_heights = 1)
    plot_final <- plot_grid(plot1, plot, nrow = 2, rel_heights = c(10,4), rel_widths = 1)
    png(paste0("Plots/Task7/Task7_", hospital_id[j], ".png"), w = 4000, h = 4000, units = "px", res = 350)
    print(plot_final)
    dev.off()
    print(hospital_id[j])
}





