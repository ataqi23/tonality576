#==============================================================
# Read data
humid_raw <- read_tsv("humidity.tsv", col_names = F, show_col_types = F)
#==============================================================
# Static values
RNG <- 2:95
Ny <- 94
Nx <- 193
ys <- as.numeric(humid_raw[RNG,1][[1]])
#==============================================================
wrangle_humidity <- function(months = 890){  
  # Combine all the month chunks
  humidity_df <- purrr::map_dfr(.x = 1:months, .f = month_chunk)
  # Transform longitude
  humidity_df <- humidity_df %>% mutate(x = transform_longitude(x))
}
#==============================================================
# Obtain x_i chunk
xi_chunk <- function(i, humidity_month){
  # Obtain the x and f values
  xi <- as.numeric(humidity_month[1,i+1])
  xi_col <- rep(xi, Ny)
  f_vals <- humidity_month[RNG, i+1][[1]]
  
  # Aggregate the data in a dataframe
  df <- data.frame(x = xi_col, y = ys, f = f_vals)
  return(df)
}
#==============================================================
# Obtain dataframe chunk for month
month_chunk <- function(j){
  # Create start and end indices for month chunk
  si <- 95*j + 1
  ei <- si + 94
  # Grab month chunk
  humidity_month <- humid_raw[si:ei, ]
  
  # Finally, get the df containing humidity values
  month_df <- purrr::map_dfr(.x = 1:(Nx-1), .f = xi_chunk, humidity_month)  
  
  # Attach date and year information
  date_cell <- as.character(humidity_month[1,1])
  month_df$mo <- str_sub(date_cell, end = 3)
  month_df$year <- str_sub(date_cell, start = -4)
  # Return dataframe
  return(month_df)
}
#==============================================================
# Transform a vector
transform_longitude <- function(vec){ purrr::map_dbl(.x = 1:length(vec), .f = compute_longitude, vec) }
# Computes longitude shifted
compute_longitude <- function(i, vec){
  y <- vec[i]
  if(y < 180){return(y)}
  return(y - 360)
}

