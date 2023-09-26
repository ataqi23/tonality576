# ========================
#       Import data
# ========================
# Import data
tonal_df <- read.csv("tonal_df.csv")
# Make tonality type factor
tonal_df$tonality_type <- as.factor(tonal_df$tonality_type)
# Reorder factors
tonal_df$tonality_type <- factor(tonal_df$tonality_type, levels=c("Atonal","Simple","Complex"))
# Rename the humidity indices
colnames(tonal_df)[15:16] <- c("UHI","LHI")
colnames(tonal_df)[17:18] <- c("min_H","max_H")
# Upscale the humidity units
tonal_df[,14:18] <- 100 * tonal_df[,14:18]
# Create dryness variables
tonal_df$MD <-  (max(tonal_df$MH) - tonal_df$MH)
tonal_df$UDI <- (max(tonal_df$UHI) - tonal_df$UHI)
# Indicators
tonal_df$isnt_tonal <- !tonal_df$is_tonal
tonal_df$isnt_complex_tonal <- !tonal_df$complex_tonal

# Humidity data
humidity_df <- read.csv("humidity100.csv")


hum_cols_MM <- c(14,17,18,23:31)
hum_cols <- c(23:31)

# Import alternate data
# tonalH_df <- read.csv("tonal_df_H.csv")
# # Make tonality type factor
# tonalH_df$tonality_type <- as.factor(tonalH_df$tonality_type)
# # Reorder factors
# tonalH_df$tonality_type <- factor(tonalH_df$tonality_type, levels=c("Atonal","Simple","Complex"))
# # Upscale humidity units
# tonalH_df[,hum_cols] <- tonalH_df[,hum_cols]*100

# ========================================================================================================
# Import alternate data
# tonalL_df <- read.csv("tonal_df_L.csv")
# # Make tonality type factor
# tonalL_df$tonality_type <- as.factor(tonalL_df$tonality_type)
# # Reorder factors
# tonalL_df$tonality_type <- factor(tonalL_df$tonality_type, levels=c("Atonal","Simple","Complex"))
# # Upscale humidity units
# tonalL_df[,hum_cols] <- tonalL_df[,hum_cols]*100
# ========================================================================================================
# Import alternate data
tonalM_df <- read.csv("tonal_df_M.csv")
# Make tonality type factor
tonalM_df$tonality_type <- as.factor(tonalM_df$tonality_type)
# Reorder factors
tonalM_df$tonality_type <- factor(tonalM_df$tonality_type, levels=c("Atonal","Simple","Complex"))
# Upscale humidity units
tonalM_df[,hum_cols] <- tonalM_df[,hum_cols]*100

# ========================================================================================================
# Import alternate data
tonalS_df <- read.csv("tonal_df_stats.csv")
# Make tonality type factor
tonalS_df$tonality_type <- as.factor(tonalS_df$tonality_type)
# Reorder factors
tonalS_df$tonality_type <- factor(tonalS_df$tonality_type, levels=c("Atonal","Simple","Complex"))
# Upscale humidity units
tonalS_df[,23] <- tonalS_df[,23]*100 # Median
tonalS_df[,24] <- tonalS_df[,24]*100^2 # Variance Upscaling
# ========================================================================================================
tonalM_df <- cbind(tonalM_df, tonalS_df[,c("med_H","var_H")])
tonalM_df <- tonalM_df %>% mutate(sd_H = sqrt(var_H))
tonalM_df <- tonalM_df %>% 
  mutate(isAusAsi = (family == "Austro-Asiatic")) %>%
  mutate(isAustro = (family == "Austronesian")) %>%
  mutate(isTransGuinean = (family == "Trans-New Guinea"))
# ========================================================================================================

# Create Dryness Variables
tonalM_df$MD <-  (max(tonalM_df$MH) - tonalM_df$MH)
tonalM_df$MDI <-  (max(tonalM_df$M70) - tonalM_df$M70)
tonalM_df$UDI <- (max(tonalM_df$M90) - tonalM_df$M90)
tonalM_df$XDI <- (max(tonalM_df$max_H) - tonalM_df$max_H)

# ========================================================================================================
hum_cols_Q <- 23:42
# Import window quantile data
tonalQ_df <- read.csv("tonalQ_df.csv")
# Make tonality type factor
tonalQ_df$tonality_type <- as.factor(tonalQ_df$tonality_type)
# Reorder factors
tonalQ_df$tonality_type <- factor(tonalQ_df$tonality_type, levels=c("Atonal","Simple","Complex"))
# Upscale humidity units
tonalQ_df[,hum_cols_Q] <- tonalQ_df[,hum_cols_Q]*100
# Add sd
tonalQ_df$sd_H <- tonalM_df$sd_H
# ========================================================================================================
tonal_df_skewness <- read.csv("skewness.csv")
tonal_df_skewness$complex_tonal <- as.factor(tonal_df_skewness$complex_tonal)
langs <- c("Austro-Asiatic","Austronesian","Trans-New Guinea")
lang_labs <- c("Austro-\nAsiatic","Austro-\nnesian","Trans-New\n Guinea", "Mean")
#lang_labs[3] <- "Trans-New\n Guinea"
# ========================================================================================================
NCT <- function(df){
  return(df %>% filter(complex_tonal == 0))
}
CT <- function(df){
  return(df %>% filter(complex_tonal == 1))
}