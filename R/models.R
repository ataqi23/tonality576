# =======================================
#             Models: q-Window
# =======================================

#family_columns <- tonalM_df[,c("isTransGuinean","isAusAsi")]
#tonalQ_df <- cbind(tonalQ_df, family_columns)
tonalQ_df$skew_H <- tonal_df_skewness$skew_H
tonalQ_df$isAustro <- tonalM_df$isAustro
tonalQ_df$isAusAsi <- tonalM_df$isAusAsi
tonalQ_df$isTransGuinean <- tonalM_df$isTransGuinean

get_formula_q <- function(q, response = "complex_tonal", family = T){
  # Create k-SD predictor 
  hum_pred <- paste("Q",q, sep = "")
  # Create response and family indicators
  response <- paste(response," ~ ",sep="")
  if(family){family_indicators <- paste("is", c("Austro","TransGuinean","AusAsi"), sep = "")}
  else{family_indicators <- c()}
  # Create and return formula
  formula <- as.formula(paste(response, paste(c(hum_pred,family_indicators), collapse= "+")))
  formula
}

get_model_q <- function(q, family){
  model <- glm(formula = get_formula_q(q, family = family), family = quasibinomial(link = "probit"), data = tonalQ_df)
  return(model)
}

QI <- function(i){50 + 2.5*(i-1)}
IQ <- function(i){-0.4*(47.5 - i)}
seq_probs <- seq(50,97.5,by = 2.5)
#mean_k <- function(k){mean(I(MH + k*sd_H))}

results_model_q <- function(q, family){
  model <- get_model_q(q, family = family)
  # Get statistics
  deviance <- model$deviance
  # Compute statistics
  dispersion <- round(sum(residuals(model, type = "pearson")^2) / model$df.residual, 3)
  # Get Q label
  Q <- 50 + 2.5*(q-1)
  # Aggregate result
  result <- c(q = Q, deviance = deviance, dispersion = dispersion, family = family)
  # Return result
  return(result)
  
}

# =======================================
#             Models: k-SD
# =======================================

get_formula_k <- function(k, response = "complex_tonal", family = T){
  # Create k-SD predictor 
  hum_pred <- paste("I(MH + ",k,"*sd_H)", sep = "")
  # Create response and family indicators
  response <- paste(response," ~ ",sep="")
  if(family){family_indicators <- paste("is", c("Austro","TransGuinean","AusAsi"), sep = "")}
  else{family_indicators <- c()}
  # Create and return formula
  formula <- as.formula(paste(response, paste(c(hum_pred,family_indicators), collapse= "+")))
  formula
}

get_model_k <- function(k, family){
  model <- glm(formula = get_formula_k(k, family = family), family = quasibinomial(link = "probit"), data = tonalM_df)
  return(model)
}

results_model_k <- function(k, family){
  model <- get_model_k(k, family = family)
  # Get statistics
  deviance <- model$deviance
  # Compute statistics
  dispersion <- round(sum(residuals(model, type = "pearson")^2) / model$df.residual, 3)
  # Aggregate result
  result <- c(k = k, deviance = deviance, dispersion = dispersion, family = family)
  # Return result
  return(result)
  
}
# =======================================
#             Models: Modular
# =======================================
get_formula <- function(hum_vars, family = T, response = "complex_tonal"){
  # Create response and family indicators
  response <- paste(response," ~ ",sep="")
  if(family){family_indicators <- paste("is", c("Austro","TransGuinean","AusAsi"), sep = "")}
  else{family_indicators <- c()}
  # Create and return formula
  formula <- as.formula(paste(response, paste(c(hum_vars,family_indicators), collapse= "+")))
  formula
}
get_model <- function(hum_vars, family = T){
  model <- glm(formula = get_formula(hum_vars, family), family = quasibinomial(link = "probit"), data = tonalM_df)
  return(model)
}
model_output <- function(formula, loud = F){
  model <- glm(formula = formula, family = quasibinomial(link = "probit"), data = tonalM_df)
  model_summary <- summary(model)
  if(!loud){
    model_summary
  } else{
    #print(formula)
    print(model_summary$coefficients)
    print(anova(model))
    dispersion <- round(sum(residuals(model, type = "pearson")^2) / model$df.residual, 3)
    print(paste("Dispersion: ", dispersion))
  }
}

# =======================================
#          Discussion: ANOVA SD
# =======================================

anova_glm <- get_model(c("MH", "sd_H"))

# =======================================
#          Discussion: q-Window Plots
# =======================================

# Generate results
df_WIN_q_T <- map_dfr(.x = 1:20, .f = results_model_q, T)
df_WIN_q_F <- map_dfr(.x = 1:20, .f = results_model_q, F)
# Combine
df_WIN <- rbind(df_WIN_q_T, df_WIN_q_F)
# Create factor for plotting
df_WIN$family <- ifelse(df_WIN$family == 1, "All","None")
df_WIN$family <- as.factor(df_WIN$family)
# Plot params
col_vline <- "grey32"
type_vline <- "solid"
q_ref <- 75
# Plots
disp_plot_Q <- 
  df_WIN %>% 
  ggplot(mapping = aes(x = q, color = q, y = dispersion, group = family, shape = family)) +
  scale_colour_distiller(type = "div", palette = 7, guide = "none") +
  scale_shape_manual("Family Predictors", values = c("circle","triangle")) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 0, intercept = 1, color = "darkred", alpha = 0.75, linetype = "dashed") +
  geom_vline(xintercept = q_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  labs(title = "Dispersion of q-Window Models", x = "q", y = "Dispersion") +
  theme(legend.position = "bottom")
dev_plot_Q <- 
  df_WIN %>% 
  ggplot(mapping = aes(x = q, color = q, y = deviance, group = family, shape = family)) +
  scale_shape_manual("Family Preds", values = c("circle","triangle"), guide = "none") +
  scale_colour_distiller(type = "div", palette = 7) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 0, intercept = 475.4, color = "grey32", alpha = 0.75, linetype = "dotted") +
  geom_vline(xintercept = q_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  labs(title = "Deviance of q-Window Models", x = "q", y = "Deviance") +
  theme(legend.position = "bottom") #+
#ylim(c(399, 475))



# =======================================
#          Discussion: k-SD Plots
# =======================================
seq_ks <- 1:30/8
# Generate results
df_sd_k_T <- map_dfr(.x = 1:30/8, .f = results_model_k, T)
df_sd_k_F <- map_dfr(.x = 1:30/8, .f = results_model_k, F)
# Combine
df_sd <- rbind(df_sd_k_T, df_sd_k_F)
# Create factor for plotting
df_sd$family <- ifelse(df_sd$family == 1, "All","None")
df_sd$family <- as.factor(df_sd$family)
# Plot params
col_vline <- "grey32"
type_vline <- "solid"
k_ref <- 2 # 1.88 for fam before
# Plots
disp_plot <- 
  df_sd %>% 
  filter(k <= 2.5) %>%
  ggplot(mapping = aes(x = k, y = dispersion, group = family, shape = family, color = k)) +
  scale_colour_distiller(type = "div", palette = 7, guide = "none") +
  scale_shape_manual("Family Predictors", values = c("circle","triangle")) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 0, intercept = 1, color = "darkred", alpha = 0.75, linetype = "dashed") +
  geom_vline(xintercept = k_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  labs(title = "Dispersion of k-SD Models", x = "k", y = "Dispersion") +
  theme(legend.position = "bottom")
dev_plot <- 
  df_sd %>% 
  filter(k <= 2.5) %>%
  ggplot(mapping = aes(x = k, y = deviance, group = family, shape = family, color = k)) +
  scale_shape_manual("Family Preds", values = c("circle","triangle"), guide = "none") +
  scale_colour_distiller(type = "div", palette = 7) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 0, intercept = 475.4, color = "grey32", alpha = 0.75, linetype = "dotted") +
  geom_vline(xintercept = k_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  labs(title = "Deviance of k-SD Models", x = "k", y = "Deviance") +
  theme(legend.position = "bottom") #+
  #ylim(c(399, 475))

# =======================================
#          Dispersion Discussion
# =======================================
NC_rows <- which(tonal_df$complex_tonal == 0)
C_rows <- which(tonal_df$complex_tonal == 1)

type_res <- "pearson"
q_C <- function(q, family = T, sq = 1){
  mean((residuals(get_model_q(q = q, family), type = type_res)[C_rows])^(2-sq))
}
q_NC <- function(q, family = T, sq = 1){
  mean((residuals(get_model_q(q = q, family), type = type_res)[NC_rows])^(2-sq))
}
diff_q <- function(q, family = T, sq = 1){
  q_C(q, family, sq) - q_NC(q, family, sq)
}

k_C <- function(k, family = T, sq = 1){
  var((residuals(get_model_k(k = k, family), type = type_res)[C_rows])^(2-sq))
}
k_NC <- function(k, family = T, sq = 1){
  mean((residuals(get_model_k(k = k, family), type = type_res)[NC_rows])^(2-sq))
}
diff_k <- function(k, family = T, sq = 1){ k_C(k, family, sq) - k_NC(k, family, sq) }


df_residuals_q <- data.frame(q = seq_probs, res_disparity = map_dbl(1:20, diff_q, T, 0))
# group_labs <- c(rep("Complex",20), rep("Non-complex",20))
# df_res_q2 <- data.frame(q = rep(seq_probs,2), group = group_labs)
# df_res_q2$res <- c(map_dbl(1:20, q_C), map_dbl(1:20, q_NC))

K_MAX <- 20
df_residuals_k <- data.frame(k = seq_ks[1:K_MAX], res_disparity = map_dbl(seq_ks[1:K_MAX], diff_k, T, 0))
#df_residuals_k2 <- data.frame(k = seq_ks[1:20], res_disparity = map_dbl(seq_ks[1:20], diff_k))

plot_res_k_fn <- function(fn, title_str, xval, ylims, sq){
  with_fam <- map_dbl(seq_ks[1:K_MAX], fn, T, sq)
  no_fam <- map_dbl(seq_ks[1:K_MAX], fn, F, sq)
  fam_labels <- c(rep("All", K_MAX), rep("None", K_MAX))
  df_residuals_k <- data.frame(k = rep(seq_ks[1:K_MAX],2), 
                               mean_res = c(with_fam, no_fam),
                               Family = fam_labels)
  df_residuals_k %>% 
    ggplot(aes(x = k, y = mean_res, group = Family, shape = Family, color = Family)) + 
    geom_point() +
    geom_line() +
    #geom_vline(xintercept = xval, color = "darkred", linetype = "dotted") +
    geom_vline(xintercept = k_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
    labs(title = paste(title_str," Group Mean Error by Score",sep =""),
         y = "Group Mean Residual") +
    ylim(ylims)
  
}

plot_res_q_fn <- function(fn, title_str, xval, ylims, sq){
  with_fam <- map_dbl(1:20, fn, T, sq)
  no_fam <- map_dbl(1:20, fn, F, sq)
  fam_labels <- c(rep("All", 20), rep("None", 20))
  df_residuals_q <- data.frame(q = rep(map_dbl(1:20, QI),2),
                               mean_res = c(with_fam, no_fam),
                               Family = fam_labels)
  df_residuals_q %>% 
    ggplot(aes(x = q, y = mean_res, group = Family, shape = Family, color = Family)) + 
    geom_point() +
    geom_line() +
    #geom_vline(xintercept = xval, color = "darkred", linetype = "dotted") +
    geom_vline(xintercept = q_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
    labs(title = paste(title_str," Group Mean Error by Quantile",sep =""),
         y = "Group Mean Residual") +
    ylim(ylims)
  
}

residual_scatterplot_q <- 
  df_residuals_q %>% 
  ggplot(aes(x = q, y = res_disparity)) + 
  geom_point() +
  geom_line() +
  labs(title = "Mean Residual Intra-Group Disparity by Quantile",
       y = "Group Mean Residual")

# residual_scatterplot_q2 <- 
#   df_res_q2 %>% 
#   ggplot(aes(x = q, y = abs(res), group = group, color = group)) + 
#   geom_point() +
#   geom_line() +
#   labs(title = "Mean Residual Intra-Group Disparity by Quantile Extremity",
#        y = "Mean Residual Disparity")

residual_scatterplot_k <- 
  df_residuals_k %>% 
  ggplot(aes(x = k, y = res_disparity)) + 
  geom_point() +
  geom_line() +
  labs(title = "Mean Residual Intra-Group Disparity by Score",
       y = "Mean Residual Disparity")


pearson <- 0
if(pearson == 0)
  {ylimc <- c(4.5,5.25);ylimnc <-c(0.1995,0.20425) } else
  {ylimc <- c(1.9,2.15);ylimnc <-c(-0.44,-0.3825) }
prck <- plot_res_k_fn(k_C, "Complex", 3, ylimc, pearson)
prnck <- plot_res_k_fn(k_NC, "Non-Complex", 2.375, ylimnc, pearson) 
prcq <- plot_res_q_fn(q_C, "Complex", QI(20), ylimc, pearson)
prncq <- plot_res_q_fn(q_NC, "Non-Complex", QI(15), ylimnc, pearson) 
