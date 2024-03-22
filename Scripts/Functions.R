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



c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf","#aec7e8","#ffbb78","#98df8a")


####### calculate OS for the data ########
os_count <- function(data, var1) {
  count <- 0
  for(i in 1:nrow(data)) {
  if(!is.na(data[i, var1]) && data[i, var1] > 0) {
    count <- count + 1
   }
 } 
return(count)
}

##### Calculate ADE summary percentage ######
calculate_ade_summary <- function(dataframes, addition) {
  calculated_dataframes <- list() 
  for(i in seq_along(dataframes)) {
    data <- dataframes[[i]]
    data <- data[,grep("%ADE_", colnames(data), value = T)]
    data <- as.data.frame(t(data))
    data$Years <- rownames(data)
    data <- as.data.table(data[, c(14, 1:13)])
    data[, Minimum := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
    print("Min")
    data[, Maximum := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,15)]]
    print("Max")
    data[, Average := round(rowSums(.SD, na.rm = TRUE) / 13, 2), .SDcols = names(data)[-c(1,15,16)]]
    print("Ave")
    data[, StandardDeviation := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17)]]
    print("SD")
    data[, Quartile1 := apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17,18)]]
    print("Q1")
    data[, Median := apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19)]]
    print("Q2")
    data[, Quartile3 := apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19,20)]]
    print("Q3")
    data <- as.data.frame(data)
    rownames(data) <- data$Years
    data <- data[, -1]
    print("transposing")
    data <- as.data.frame(t(data))
    data$HID <- rownames(data)
    data <- data[, c(8, 1:7)]
    print("subsetting")
    data <- as.data.table(data)
    data[, Minimum := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
    print("Min2")
    data[, Maximum := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,9)]]
    print("Max2")
    data[, Average := round(rowSums(.SD, na.rm = TRUE) / 7, 2), .SDcols = names(data)[-c(1,9,10)]]
    print("Ave2")
    data[, StandardDeviation := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11)]]
    print("SD2")
    data[, Quartile1 := apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11,12)]]
    print("Q1-2")
    data[, Median := apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11,12,13)]]
    print("Q2-2")
    data[, Quartile3 := apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11,12,13,14)]]
    print("Q3-2")
    data[14:20, 9:15] <- NA
    colo <- colnames(data)[-c(1, 9:15)]
    ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
    data <- as.data.frame(data)
    data <- data[, ordered]
    calculated_dataframes[[i]] <- data
  }
  return(calculated_dataframes)
}

calculate_fac_summary <- function(dataframes, addition) {
  calculated_dataframes <- list() 
  for(i in seq_along(dataframes)) {
    data <- dataframes[[i]]
    data <- data[,grep("FAC_", colnames(data), value = T)]
    data <- as.data.frame(t(data))
    data$Years <- rownames(data)
    data <- as.data.table(data[, c(14, 1:13)])
    data[, Minimum := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
    print("Min")
    data[, Maximum := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,15)]]
    print("Max")
    data[, Average := round(rowSums(.SD, na.rm = TRUE) / 13, 2), .SDcols = names(data)[-c(1,15,16)]]
    print("Ave")
    data[, StandardDeviation := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17)]]
    print("SD")
    data[, Quartile1 := apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1, 15,16,17,18)]]
    print("Q1")
    data[, Median := apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19)]]
    print("Q2")
    data[, Quartile3 := apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,15,16,17,18,19,20)]]
    print("Q3")
    data <- as.data.frame(data)
    rownames(data) <- data$Years
    data <- data[, -1]
    print("transposing")
    data <- as.data.frame(t(data))
    data$HID <- rownames(data)
    data <- data[, c(8, 1:7)]
    print("subsetting")
    data <- as.data.table(data)
    data[, Minimum := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-1]]
    print("Min2")
    data[, Maximum := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = names(data)[-c(1,9)]]
    print("Max2")
    data[, Average := round(rowSums(.SD, na.rm = TRUE) / 7, 2), .SDcols = names(data)[-c(1,9,10)]]
    print("Ave2")
    data[, StandardDeviation := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11)]]
    print("SD2")
    data[, Quartile1 := apply(.SD, 1, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11,12)]]
    print("Q1-2")
    data[, Median := apply(.SD, 1, quantile, probs = 0.5, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11,12,13)]]
    print("Q2-2")
    data[, Quartile3 := apply(.SD, 1, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(data)[-c(1,9,10,11,12,13,14)]]
    print("Q3-2")
    data[14:20, 9:15] <- NA
    colo <- colnames(data)[-c(1, 9:15)]
    ordered <- c("HID", colo[order(as.numeric(sub(".*_(\\d+)$", "\\1", colo)))], addition)
    data <- as.data.frame(data)
    data <- data[, ordered]
    calculated_dataframes[[i]] <- data
  }
  return(calculated_dataframes)
}



######### calculate ade overall status ######

calculate_ade_status <- function(dataframes){
  ade_status_cal <- list()
  for(i in seq_along(dataframes)){
    data <- dataframes[[i]]
    data <- data[,grep("DE_status", colnames(data), value = T)]
    data <- as.data.frame(t(data))
    data$Years <- rownames(data)
    data <- as.data.table(data[,c(14, 1:13)])
    data[, overall_status := apply(.SD, 1, function(x) {tbl <- table(x) 
    names(tbl)[which.max(tbl)]})]
    data <- as.data.frame(data)
    rownames(data) <- data$Years
    data <- data[, -1]
    print("transposing")
    data <- as.data.frame(t(data))
    data$HID <- rownames(data)
    data <- as.data.table(data[, c(8, 1:7)])
    data[, overall_status := apply(.SD, 1, function(x) {tbl <- table(x) 
    names(tbl)[which.max(tbl)]})]
    ade_status_cal[[i]] <- data
  }
  return(ade_status_cal)
}

# Function to calculate the maximum occurred category in each row for a list of data frames
calculate_max_category <- function(dataframes) {
  max_categories <- lapply(dataframes, function(data) {
    max_category <- apply(data[, -1], 1, function(row) {
      table_row <- table(row)
      names(table_row)[which.max(table_row)]
    })
    return(max_category)
  })
  return(max_categories)
}


calculate_max_category <- function(dataframes) {
  max_categories <- lapply(dataframes, function(data) {
    max_category <- apply(data[, -1], 1, function(row) {
      table_row <- table(row)
      if (length(table_row) == 0) {
        return(NA)  # Replace NULL with NA
      }
      names(table_row)[which.max(table_row)]
    })
    return(max_category)
  })
  return(max_categories)
}



calculate_fac_status <- function(dataframes){
  status_cal <- list()
  for(i in seq_along(dataframes)){
    data <- dataframes[[i]]
    #data <- data[,grep("Deviataion", colnames(data), value = T)]
    data
    data <- as.data.frame(t(data))
    cols <- rownames(data)
    #data$Years <- rownames(data)
    #data <- as.data.table(data[,c(14, 1:13)])
    data <- as.data.table(data)
    data[, overall_status := apply(.SD, 1, function(x) {tbl <- table(x) 
                                names(tbl)[which.max(tbl)]})]
    data <- as.data.frame(data)
    rownames(data) <- cols
    print("transposing")
    data <- as.data.frame(t(data))
    #data$HID <- rownames(data)
    #data <- as.data.table(data[, c(9, 1:8)])
    data <- as.data.table(data)
    data$overall_status <- NA
    data[, overall_status := apply(.SD, 1, function(x) {tbl <- table(x) 
                                  names(tbl)[which.max(tbl)]})]
    status_cal[[i]] <- data
  }
  return(status_cal)
}





extract_ADE_data <- function(hospital_wide, hospital_id) {
  ADE_data <- list()
  for(j in 1:length(hospital_wide)) {
    data <- hospital_wide[[j]]
    col1 <- grep("DE_status", colnames(data), value = TRUE)
    col3 <- grep("ADE_", colnames(data), value = TRUE)
    col_need <- c("Drug_Name", "STOCK_CODE", col1, col3)
    data <- data[, col_need]
    colnames(data) <- gsub(paste0(hospital_id[j], "_"), "", colnames(data))
    ADE_data[[j]] <- data
  }
  names(ADE_data) <- hospital_id
  return(ADE_data)
}



extract_FI_data <- function(dataframes) {
  out_data <- list()
  for(j in seq_along(dataframes)) {
    data <- as.data.frame(dataframes[[j]])
    col1 <- grep("FI_", colnames(data), value = TRUE)
    col3 <- grep("FA_WHO_", colnames(data), value = TRUE)
    col4 <- grep("OS_status_", colnames(data), value = TRUE)
    col_need <- c("Drug_Name", "STOCK_CODE", col1, col3, col4)
    print(col_need)
    data <- data[,col_need]
    out_data[[j]] <- data
  }
  return(out_data)
}


############# create a FAC status #############
### It has four inputs namely list of dataframes, years, breaks, labels #####
categorize_data <- function(data_list, hospital_ids, years, breaks, labels) {
  for (i in seq_along(data_list)) {
    data <- data_list[[i]]
    hos <- hospital_ids[i]
    print(hos)
    #### run the loop for all the years in each dataframe
    for (j in seq_along(years)) {
      #### create the input variable 
      print(years[j])
      var1 <- paste0("FAC_per_", years[j])
      print(var1)
      #### create the output variable
      out <- paste0("FAC_status_", years[j])
      print(out)
      #### categorise the variable from A to E
      ### change the NA into _Inf
      data[[var1]][is.na(data[[var1]])] <- Inf
      data[[out]] <- cut(data[[var1]], breaks = breaks, labels = labels, include.lowest = TRUE)
      ### change the -Inf back to NA
      data[[var1]][data[[var1]] == Inf] <- NA
    }
    data_list[[i]] <- data
  }
  return(data_list)
}

######### this function will create hospital centred data into drug centered data ###########
process_drug_data <- function(data_list, hospital_id, drugs_id) {
  Drug_list <- list()
  for (drug_index in 1:31) {
    drug_data <- data.frame()
    for (df_index in 1:length(data_list)) {
      drug_data_df <- data_list[[df_index]][drug_index, ]
      drug_data <- rbind(drug_data, drug_data_df)
    }
    drug_data <- drug_data[, -c(grep("STOCK_CODE", colnames(drug_data)))]
    drug_data$HID <- hospital_id
    drug_data <- drug_data[, c(ncol(drug_data), 2:ncol(drug_data) - 1)]
    drug_data <- as.data.frame(drug_data)
    rownames(drug_data) <- hospital_id
    drug_data$class <- left_join(drug_data, drugs_id, by = "Drug_Name")$class
    drug_data <- drug_data[, -1]
    Drug_FI[[drug_index]] <- drug_data
  }
  return(Drug_FI)
}

########## This will calculate FAC and gives the drug Name as well ##
FAC_count_sep <- function(data, var1, var2) {
  count <- 0
  first_col_values <- c()  # Initialize an empty vector to store row names
  
  for(i in 1:nrow(data)) {
    if (!is.na(data[i, var1]) && 
        !is.na(data[i, var2]) &&
        data[i, var1] != 0 && 
        data[i, var2] != 0) {
      count <- count + 1
      first_col_values[i] <- data[i, 2]  # Store row names meeting the condition
    }
  }
  first_col_values <- as.vector(na.omit(first_col_values))
  return(list(count = count, Drug_Name = first_col_values))
}




