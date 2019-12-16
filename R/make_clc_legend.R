############
make_clc_legend <- function(rough = "default", label = 1) {
  stopifnot(label %in% 1:3)
  
  legend_url <- "https://www.eea.europa.eu/data-and-maps/data/corine-land-cover-3/corine-land-cover-classes-and/clc_legend.csv/at_download/file"
  data <- read.csv(legend_url)
  
  if (label == 1) {
    if (rough == "default") {
      rough <- c(0.8, 0.3, 0.2, 0.0024, 0.0002, 1)
    }
    clc_levels <- data.frame(
      "LABEL1" = as.character(unique(data$LABEL1)),
      "Rauhigkeit_z" = rough
    )    
    data <- merge(data, clc_levels, by = "LABEL1", all.x = TRUE)
  }
  else if (label == 2) {
    if (rough == "default") {
      rough <- c(1, 0.3, 0.3, 0.3, 0.3, 0.8, 0.8, 0.8, 0.8, 0.2, 0.2, 0.2, 2e-04, 
                 2e-04, 0.0024, 0.0024)
    }
    clc_levels <- data.frame(
      "LABEL2" = as.character(unique(data$LABEL2)),
      "Rauhigkeit_z" = rough
    )    
    data <- merge(data, clc_levels, by = "LABEL2", all.x = TRUE)
  }
  else if (label == 3) {
    if (rough == "default") {
      rough <- c(1, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.8, 
          0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.2, 0.2, 0.2, 
          0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 2e-04, 2e-04, 2e-04, 
          2e-04, 2e-04, 0.0024, 0.0024, 0.0024, 0.0024, 0.0024)
    }
    clc_levels <- data.frame(
      "LABEL3" = as.character(unique(data$LABEL3)),
      "Rauhigkeit_z" = rough
    )
    data <- merge(data, clc_levels, by = "LABEL3", all.x = TRUE)
  } 
  
  data <- data[!is.na(data$CLC_CODE),]
  data <- data[!is.na(data$GRID_CODE),]
  return(data)
}


