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

