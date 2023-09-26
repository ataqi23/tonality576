# ========================
#       Wrangle data
# ========================
wrangle_data <- function(data_raw){
  # Rename columns in raw data
  colnames(data_raw)[1] <- "code"; colnames(data_raw)[3] <- "tonality_type"
  # Remove columns for working data
  data <- subset(data_raw, select = -c(description, area))
  # Define a new variable, distance from equator (hence approximate humidity)
  data <- data %>% mutate(dist_equator = abs(latitude))
    # Create new dataset with simplified binary variable
    # 0: Atonal/Simple Tonal
    # 1: Complex Tonal
  data <- data %>% mutate(complex_tonal = (tonality_type == 3))
    # Create new dataset with simplified binary variable
    # 0: Atonal
    # 1: Simple/Complex Tonal
  data <- data %>% mutate(is_tonal = (tonality_type != 1))
  # Make binary variables numerical
  data$complex_tonal <- as.numeric(data$complex_tonal)
  data$is_tonal <- as.numeric(data$is_tonal)
  # Add isAustro categorical variable
  AustroFams <- c("Austronesian","Trans-New Guinea","Pama-Nyungan")
  data <- data %>% mutate(isAustro = (family %in% AustroFams))
  # Make tonality type a factor
  data$tonality_type <- as.factor(data$tonality_type)
  # Name tonality type factor levels
  levels(data$tonality_type)[levels(data$tonality_type) == 1]  <- "Atonal"
  levels(data$tonality_type)[levels(data$tonality_type) == 2]  <- "Simple"
  levels(data$tonality_type)[levels(data$tonality_type) == 3]  <- "Complex"
  # Return wrangled dataset
  return(data)
}
