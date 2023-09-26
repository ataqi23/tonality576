
# ===============================
#             Models
# ===============================

eval_points = 0:20/8

model_surv <- survfit2(Surv(MD, isnt_complex_tonal) ~ 1, data = tonal_df)
model_surv2 <- survfit2(Surv(UDI, isnt_complex_tonal) ~ 1, data = tonal_df)
model_surv3 <- survfit2(Surv(XDI, isnt_complex_tonal) ~ 1, data = tonalM_df)
model_surv_summary <- summary(survfit(Surv(UDI, isnt_complex_tonal) ~ 1, data = tonal_df), times = eval_points)

# ===============================
#         Visualizations
# ===============================

surv_plot_fn <- function(model, xlab){
  model %>% 
    ggsurvfit() + 
    labs(x = xlab, y = "Survival Prob.") +
    add_confidence_interval() +
    add_quantile(y_value = 0.5)
}

# =======================================
#        Window Averages CDF Plot
# =======================================

# Setup
DFM <- tonalM_df
sel_rows <- which(DFM$is_tonal == 1)

# Plot parameters
avec <- 1 - abs(1:9 - 5)/5
mavec <- c(0.5, 0.775, 0.5)
# Color parameters
# col0 <- "orange" # violet
# col_MH <- "darkorange3" # purple
# col_min <- "royalblue1" # turquoise
# col_max <- "deeppink2"
# 
# col0 <- "deepskyblue2" # violet
# col_MH <- "royalblue2" # purple
# col_min <- "orange2" # turquoise
# col_max <- "deeppink2"

col0 <- "orange" # violet
col_MH <- "brown" # purple
col_min <- "deepskyblue3" # turquoise
col_max <- "red3"

# Plot window average CDFs 
CDF_plot <- DFM %>% 
  filter(is_tonal == TRUE) %>% 
  ggplot() + 
  stat_ecdf(aes(x = DFM[,23][sel_rows]), alpha = avec[1], color = col0) + 
  stat_ecdf(aes(x = DFM[,24][sel_rows]), alpha = avec[2], color = col0) + 
  stat_ecdf(aes(x = DFM[,25][sel_rows]), alpha = avec[3], color = col0) + 
  stat_ecdf(aes(x = DFM[,26][sel_rows]), alpha = avec[4], color = col0) + 
  stat_ecdf(aes(x = DFM[,27][sel_rows]), alpha = avec[5], color = col0) + 
  stat_ecdf(aes(x = DFM[,28][sel_rows]), alpha = avec[6], color = col0) + 
  stat_ecdf(aes(x = DFM[,29][sel_rows]), alpha = avec[7], color = col0) + 
  stat_ecdf(aes(x = DFM[,30][sel_rows]), alpha = avec[8], color = col0) +    
  stat_ecdf(aes(x = DFM[,31][sel_rows]), alpha = avec[9], color = col0) +
  stat_ecdf(aes(x = DFM[,17][sel_rows]), alpha = mavec[1], color = col_min) +
  stat_ecdf(aes(x = DFM[,14][sel_rows]), alpha = mavec[2], color = col_MH) +
  stat_ecdf(aes(x = DFM[,18][sel_rows]), alpha = mavec[3], color = col_max) +
  facet_wrap(~ tonality_type) +
  theme(legend.position = "none") +
  labs(title = paste("Empirical CDF by q-Window Humidity Averages by Tonal Language"),
       x = "Humidity q-Window Average", y = "Cumulative Proportion")

# =======================================
#                  EDFs
# =======================================

# EDF: MH, faceted by tonality type (include isAustro?)
EDF_MH <- 
  tonal_df %>%
  filter(is_tonal == TRUE) %>%
  ggplot(aes(x = MH, color = tonality_type)) + 
  scale_color_manual(name = "Tonality Type", values = c("orange3","gold")) + 
  stat_ecdf() + 
  facet_wrap(~ tonality_type) +
  theme(legend.position = "none") +
  labs(title = "Empirical Cumulative Distribution of Tonal Languages by Humidity (MH)", 
       x = "MH", y = "Cumulative Proportion")
# EDF: UHI, faceted by tonality type (include isAustro?)
EDF_UHI <- 
  tonal_df %>%
  filter(is_tonal == TRUE) %>%
  ggplot(aes(x = UHI, color = tonality_type)) + 
  scale_color_manual(name = "Tonality Type", values = c("orange3","gold")) +
  stat_ecdf() + 
  facet_wrap(~ tonality_type) +
  theme(legend.position = "bottom") +
  labs(title = "Empirical Cumulative Distribution of Tonal Languages by Humidity (UHI)", 
       x = "UHI", y = "Cumulative Proportion")

