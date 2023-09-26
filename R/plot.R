# =======================================
#    Discussion: Skewness Scatterplot
# =======================================

skewness_hist <-
  ggplot(data = tonal_df_skewness) + 
  geom_point(mapping = aes(x = skew_H, color = complex_tonal, alpha = complex_tonal)) +
  scale_color_manual("Tonality Type", labels = c("Atonal / Simple", "Complex"), values = c("dodgerblue3","gold2")) + 
  scale_alpha_manual("Tonality Type", values = c(0.075,0.8), guide = "none") + 
  theme(legend.position = "bottom") +
  labs(title = "Tonality by Humidity Skewness & MH", x = "Humidity Skewness", y = "MH")

# Obtain means
skew_NCT <- mean(NCT(tonal_df_skewness)$skew_H)
skew_CT <- mean(CT(tonal_df_skewness)$skew_H)

skewness_histogram <- ggplot(data = tonal_df_skewness) + 
  geom_histogram(mapping = aes(x = skew_H, fill = complex_tonal), bins = 35) +
  geom_vline(xintercept = skew_NCT, color = "dodgerblue4") +
  geom_vline(xintercept = skew_CT, color = "gold4") +
  scale_fill_manual("Tonality Type", labels = c("Atonal / Simple", "Complex"), values = c("dodgerblue3","gold2")) + 
  theme(legend.position = "bottom") +
  labs(title = "Distribution of Humidity Skewness by Tonality", x = "Humidity Skewness", y = "count")

skewness_scatterplot <-
  ggplot(data = tonal_df_skewness) + 
  geom_point(mapping = aes(x = skew_H, y = MH, color = complex_tonal, alpha = complex_tonal)) +
  scale_color_manual("Tonality Type", labels = c("Atonal / Simple", "Complex"), values = c("dodgerblue3","gold2")) + 
  scale_alpha_manual("Tonality Type", values = c(0.075,0.8), guide = "none") + 
  geom_vline(xintercept = 0, color = "darkred", alpha = 0.65, linetype = "dashed")+
  theme(legend.position = "bottom") +
  labs(title = "Tonality by Humidity Skewness & MH", x = "Humidity Skewness", y = "MH")

# =======================================
#        Discussion: SD Predictor
# =======================================
plot_df0 <- tonalM_df 
plot_df0$complex_tonal <- as.factor(plot_df0$complex_tonal)
plot_df0$skew_H <- tonal_df_skewness$skew_H

sd_boxplot <- 
  ggplot(data = plot_df0) + 
  geom_boxplot(mapping = aes(x = sd_H, group = tonality_type, color = tonality_type)) +
  scale_color_manual("Tonality", values = c("dodgerblue3","orange3","gold2")) + 
  theme(legend.position = "bottom") +
  labs(title = "Tonality by Humidity SD", x = "Humidity Standard Deviation") +
  theme(legend.position = "bottom", 
        axis.ticks.y=element_blank(), axis.text.y=element_blank())

sd_scatterplot <-
  ggplot(data = plot_df0) + 
  geom_point(mapping = aes(x = sd_H, y = MH, color = complex_tonal, alpha = complex_tonal)) +
  scale_color_manual("Tonality Type", labels = c("Atonal / Simple", "Complex"), values = c("dodgerblue3","gold2")) + 
  scale_alpha_manual("Tonality Type", values = c(0.075,0.8), guide = "none") + 
  theme(legend.position = "bottom") +
  labs(title = "Tonality by Humidity SD & Mean", x = "Humidity Standard Deviation", y = "MH")

skewness_sd_scatterplot <-
  ggplot(data = plot_df0) + 
  geom_point(mapping = aes(x = skew_H, y = sd_H, color = complex_tonal, alpha = complex_tonal)) + 
  geom_vline(xintercept = 0, color = "darkred", alpha = 0.65, linetype = "dashed")+
  scale_color_manual("Tonality Type", labels = c("Atonal / Simple", "Complex"), values = c("dodgerblue3","gold2")) + 
  scale_alpha_manual("Tonality Type", values = c(0.075,0.8), guide = "none") + 
  theme(legend.position = "bottom") +
  labs(title = "Skewness and SD by Tonality", y = "Humidity Standard Deviation", x = "Humidity Skewness")

# =======================================
#         Dispersion Predictors
# =======================================
range_boxplot <- 
  ggplot(data = plot_df0) + 
  geom_boxplot(mapping = aes(x = I(max_H - min_H), group = tonality_type, color = tonality_type)) +
  scale_color_manual("Tonality", values = c("dodgerblue3","orange3","gold2")) + 
  theme(legend.position = "bottom") +
  labs(title = "Humidity Range by Tonality", x = "Humidity Range") +
  theme(legend.position = "bottom", 
        axis.ticks.y=element_blank(), axis.text.y=element_blank())

range_scatterplot <-
  ggplot(data = plot_df0) + 
  geom_point(mapping = aes(x = I(max_H - min_H), y = MH, color = complex_tonal, alpha = complex_tonal)) +
  scale_color_manual("Tonality Type", values = c("dodgerblue3","gold2")) + 
  scale_alpha_manual("Tonality Type", values = c(0.075,0.8), guide = "none") + 
  theme(legend.position = "bottom") +
  labs(title = "Humidity Range by MH", x = "Humidity Range", y = "MH")

# *******************************************

normrange_boxplot <- 
  ggplot(data = plot_df0) + 
  geom_boxplot(mapping = aes(x = sd_H, group = tonality_type, color = tonality_type)) +
  scale_color_manual("Tonality", values = c("dodgerblue3","orange3","gold2")) + 
  theme(legend.position = "bottom") +
  labs(title = "Hum. Norm. Range by Tonality", x = "Humidity Normalized Range") +
  theme(legend.position = "bottom", 
        axis.ticks.y=element_blank(), axis.text.y=element_blank())

normrange_scatterplot <-
  ggplot(data = plot_df0) + 
  geom_point(mapping = aes(x = I((max_H - min_H)/max_H), y = MH, color = complex_tonal, alpha = complex_tonal)) +
  scale_color_manual("Tonality Type", values = c("dodgerblue3","gold2")) + 
  scale_alpha_manual("Tonality Type", values = c(0.075,0.8), guide = "none") + 
  theme(legend.position = "bottom") +
  labs(title = "Hum. Norm. Range by MH", x = "Humidity Normalized Range", y = "MH")


# =======================================
#            Humidity Statistic
# =======================================
# Boxplot: UHI index
UHI_boxplot <-
  tonal_df %>%
    ggplot() +
    geom_boxplot(mapping = aes(y = MH, group = tonality_type, color = tonality_type), alpha = 0.01) +
    scale_color_manual("Tonality Type", values = c("grey75","grey75","grey75")) +
    new_scale_color() +
    geom_boxplot(mapping = aes(y = UHI, group = tonality_type, color = tonality_type), alpha = 0.1) +
    scale_color_manual("Tonality Type", values = c("mediumblue","orange3","gold3")) + 
    labs(y = "Upper Humidity Index", title = "Tonal System by Upper Humidity Index") +
    coord_flip() +
    theme(legend.position = "bottom", 
          axis.ticks.y=element_blank(), axis.text.y=element_blank())
  

# ========================
#        Data Viz
# ========================
# Scatterplot: Equator distance & MH
EQ_scatter <-
  tonal_df %>%
    ggplot(mapping = aes(x = dist_equator, y = MH, color = tonality_type, alpha = tonality_type)) +
    geom_point() +
    scale_color_manual("Tonality Type", values = c("dodgerblue","orange","gold")) + 
    scale_alpha_manual(name = "Tonality Type", values = c(0.125,1,1), guide = "none") + 
    labs(title = "Tonality Type & Mean Humidity by Proximity to Equator", x = "Distance from Equator (Absolute Degrees of Latitude)")+
    theme(legend.position = "bottom")

# Density: Tonal System & MH
TH_density <-
  tonal_df %>%
  ggplot(mapping = aes(x = MH, group = tonality_type, color = tonality_type)) +
  geom_density() +
  scale_color_manual("Tonality Type", values = c("dodgerblue3","orange","gold")) + 
  labs(x = "Mean Humidity", title = "Tonal System by Humidity") +
  theme(legend.position = "none")

# Boxplot: Tonal System & MH
TH_boxplot <-
  tonal_df %>%
  ggplot(mapping = aes(y = MH, group = tonality_type, color = tonality_type)) +
  geom_boxplot() +
  scale_color_manual("Tonality Type", values = c("dodgerblue3","orange","gold")) + 
  labs(y = "Mean Humidity", title = "Tonal System by Humidity") +
  theme(legend.position = "none", 
        axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  coord_flip()

# ========================
#        World Maps
# ========================
# Obtain ggplot world coordinates
world_coordinates <- map_data("world")
# Plot tonal systems map
tonality_map <- function(){
  tonal_df %>%
    ggplot() +
    geom_map(data = world_coordinates, map = world_coordinates, aes(x = long, y = lat, map_id = region)) +
    geom_point(mapping = aes(x = longitude, y = latitude, color = tonality_type), size = 0.35) +   
    scale_color_manual("Tonality Type", values = c("dodgerblue3","orange","gold")) + 
    labs(x = "Longitude", y = "Latitude", title = "Distribution of Language Tonal Systems") +
    theme(legend.position = "bottom")
}
# First, create the humidity data
humidity_df_small <- wrangle_humidity(months = 5) %>% filter(mo == "Jun")
# Then, create the map
# Palette for tonality types
humidity_tonal_palette <- c("grey20","blue","red")
# Sizes that were good: 0.55-0.625
humidity_tonality_map <- function(){
  tonal_df %>%
    ggplot() +
    geom_map(data = world_coordinates, map = world_coordinates, aes(x = long, y = lat, map_id = region)) +
    # Add humidity points
    geom_point(data = humidity_df_small, mapping = aes(x = x, y = y, alpha = f, color = f), alpha = 0.65, size = 0.64) +
    scale_color_distiller(type = "div", palette = 7, guide = "none") +
    new_scale_color()+
    # Add tonality points
    geom_point(mapping = aes(x = longitude, y = latitude, color = tonality_type), size = 0.25) +   
    scale_color_manual("Tonality Type", values = humidity_tonal_palette) + 
    labs(x = "Longitude", y = "Latitude", title = "Language Tonal Systems & Humidity Map Overlayed") +
    theme(legend.position = "bottom")
}

#papua_map <- function(){}