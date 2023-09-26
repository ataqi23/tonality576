
humid_df <- humidity_df

# List possible coordinates
poss_xs <- unique(humid_df$x)
poss_ys <- unique(humid_df$y)

nx <- function(x){ poss_xs[which.min(abs(x - poss_xs))] }
ny <- function(y){ poss_ys[which.min(abs(y - poss_ys))] }

# Applies to whole column
n_long <- function(xs){ purrr::map_dbl(.x = xs, .f = nx) }
n_lat <- function(ys){ purrr::map_dbl(.x = ys, .f = ny) }

# Given two vectors of coordinates, get MH of nearest coordinate
df_MH <- function(longitude, latitude, fxn){
  purrr::map2_dbl(.x = longitude, .y = latitude, fxn)
}

# Get MH for an approximate pair
get_MH <- function(nx, ny){
  # Get humidity for those coords
  hum_coords <- humid_df %>% filter(x == nx) %>% filter(y == ny)
  MH <- mean(hum_coords$f)
  return(MH)
}

# Get top MH values
top_MH <- function(nx, ny){
  # Get humidity for those coords
  hum_coords <- humid_df %>% filter(x == nx) %>% filter(y == ny)
  quantiles <- unname(quantile(hum_coords$f))
  top_MH <- mean((hum_coords %>% filter(f > quantiles[4]))$f)
  return(top_MH)
}

# Get bottom MH values
bot_MH <- function(nx, ny){
  # Get humidity for those coords
  hum_coords <- humid_df %>% filter(x == nx) %>% filter(y == ny)
  quantiles <- unname(quantile(hum_coords$f))
  bot_MH <- mean((hum_coords %>% filter(f < quantiles[2]))$f)
  return(bot_MH)
}

# Get MH for an approximate pair
min_MH <- function(nx, ny){
  # Get humidity for those coords
  hum_coords <- humid_df %>% filter(x == nx) %>% filter(y == ny)
  MH <- min(hum_coords$f)
  return(MH)
}

# Get MH for an approximate pair
max_MH <- function(nx, ny){
  # Get humidity for those coords
  hum_coords <- humid_df %>% filter(x == nx) %>% filter(y == ny)
  MH <- max(hum_coords$f)
  return(MH)
}