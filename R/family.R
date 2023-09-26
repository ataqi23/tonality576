
# ===============================
#             k-SD 
# ===============================
get_k_coefs <- function(k){
  coefs <- summary(get_model_k(k, T))$coefficients[3:5]
  c(bf1 = coefs[1], bf2 = coefs[2], bf3 = coefs[3])
}

fam_coefs_k <- map_dfr(.x = seq_ks, get_k_coefs)
df_fam_coefs_k <- data.frame(k = rep(seq_ks), 
                             beta = c(fam_coefs_k$bf1, fam_coefs_k$bf2, fam_coefs_k$bf3), 
                             family = c(rep("Austro-Asiatic",30), 
                                        rep("Austronesian",30), 
                                        rep("Trans-New Guinea",30)))
df_sum_k <- df_fam_coefs_k %>% group_by(k) %>% summarize(sum_k = sum((beta)))
sum_rows_k <- data.frame(k = seq_ks,
                         beta = (df_sum_k$sum_k)/3,
                         family = rep("Average",30))
df_fam_coefs_k <- rbind(df_fam_coefs_k, sum_rows_k)

# Make tonality type factor
df_fam_coefs_k$family <- as.factor(df_fam_coefs_k$family)
# Reorder factors
df_fam_coefs_k$family <- factor(df_fam_coefs_k$family, 
                                levels=c("Austro-Asiatic",
                                         "Austronesian",
                                         "Trans-New Guinea",
                                         "Average"))
# =========================
get_k_mean <- function(k){
  filt <- tonalM_df %>% 
    filter(isTransGuinean | isAusAsi | isAustro) %>% 
    group_by(family) %>% 
    summarize(mean_fam = mean(MH + k * sd_H))
  return(data.frame(mean = filt$mean_fam, fam = filt$family, k = k))
}
kmean_df <- map_dfr(.x = seq_ks, .f = get_k_mean)
# ========================
#           PLOTS
# ========================
# Plot params
col_vline <- "grey32"
type_vline <- "solid"
k_ref <- 2 # 1.88 for fam before
# Plot params
PAL_famcoefs <- c("orange","steelblue3","forestgreen","grey15")
coef_plot_k <-df_fam_coefs_k %>%
  filter(k <= 2.5) %>%
  ggplot(mapping = aes(x = k, y = beta, group = family, color = family, alpha = family)) +
  geom_point() + 
  geom_line() + ylim(-0.72, -1.21) +
  geom_vline(xintercept = k_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  scale_alpha_manual("Family", values = c(1,1,1,0.3), guide = "none") + 
  scale_color_manual(name = "Family", labels = lang_labs, values =  PAL_famcoefs) +
  theme(legend.position = "bottom", 
        legend.text = element_text(lineheight = .8),
        legend.key.height = unit(0.8, "cm")) +
  labs(title = "k-SD Model Coefficients by Family")
mean_plot_k <- kmean_df %>% 
  filter(k <= 2.5) %>%
  ggplot(mapping = aes(x = k, y = mean, group = fam, color = fam)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = k_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  scale_color_manual(name = "Family", labels = lang_labs, values =  PAL_famcoefs) +
  theme(legend.position = "none", 
        legend.text = element_text(lineheight = .8),
        legend.key.height = unit(0.8, "cm")) +
  labs(title = "k-SD Mean Humidity by Family")

# ===============================
#             q-WIN
# ===============================

get_q_coefs <- function(q){
  coefs <- summary(get_model_q(q, T))$coefficients[3:5]
  c(bf1 = coefs[1], bf2 = coefs[2], bf3 = coefs[3])
}

fam_coefs_q <- map_dfr(.x = 1:20, get_q_coefs)
df_fam_coefs_q <- data.frame(q = rep(seq_probs), 
                             beta = c(fam_coefs_q$bf1, fam_coefs_q$bf2, fam_coefs_q$bf3), 
                             family = c(rep("Austro-Asiatic",20), 
                                        rep("Austronesian",20), 
                                        rep("Trans-New Guinea",20)))
df_sum_q <- df_fam_coefs_q %>% group_by(q) %>% summarize(sum_q = sum(beta))
sum_rows <- data.frame(q = seq_probs,
                       beta = (df_sum_q$sum_q)/3,
                       family = rep("Average",20))
df_fam_coefs_q <- rbind(df_fam_coefs_q, sum_rows)

# Make tonality type factor
df_fam_coefs_q$family <- as.factor(df_fam_coefs_q$family)
# Reorder factors
df_fam_coefs_q$family <- factor(df_fam_coefs_q$family, 
                                levels=c("Austro-Asiatic",
                                         "Austronesian",
                                         "Trans-New Guinea",
                                         "Average"))
# ========================
QFM <- function(var, q){
  res <- (tonalQ_df %>% 
            filter(isTransGuinean | isAusAsi | isAustro) %>% 
            group_by(family) %>% 
            summarize(mean_fam = mean( {{ var }} )))
  return(data.frame(q = QI(q), mean = res$mean_fam, fam = res$family))
}
df_QFM <- rbind(QFM(Q1, 1), QFM(Q2, 2), QFM(Q3, 3), QFM(Q4, 4),
                QFM(Q5, 5), QFM(Q6, 6), QFM(Q7, 7), QFM(Q8, 8),
                QFM(Q9, 9), QFM(Q10, 10), QFM(Q11, 11), QFM(Q12, 12),
                QFM(Q13, 13), QFM(Q14, 14), QFM(Q15, 15), QFM(Q16, 16),
                QFM(Q17, 17), QFM(Q18, 18), QFM(Q19, 19), QFM(Q20, 20))
# ========================
#           PLOTS
# ========================
# Plot params
col_vline <- "grey32"
type_vline <- "solid"
q_ref <- 75
# Plot
coef_plot_q <- df_fam_coefs_q %>%
  ggplot(mapping = aes(x = q, y = beta, group = family, color = family, alpha = family)) +
  geom_point() +
  geom_line() + ylim(-0.95,-1.2) +
  geom_vline(xintercept = q_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  scale_alpha_manual("Family", values = c(1,1,1,0.3), guide = "none") +
  scale_color_manual(name = "Family", labels = lang_labs, values =  PAL_famcoefs) +
  theme(legend.position = "bottom", 
        legend.text = element_text(lineheight = .8),
        legend.key.height = unit(0.8, "cm")) +
  labs(title = "q-Window Model Coefficients by Family")
mean_plot_q <- df_QFM %>%
  ggplot(mapping = aes(x = q, y = mean, group = fam, color = fam)) +
  geom_point()+
  geom_line() +
  geom_vline(xintercept = q_ref, linetype = type_vline, color = col_vline, alpha = 0.3) +
  scale_color_manual(name = "Family", labels = lang_labs[1:3], values =  PAL_famcoefs[1:3]) +
  theme(legend.position = "none", 
        legend.text = element_text(lineheight = .8),
        legend.key.height = unit(0.8, "cm")) +
  labs(title = "q-Window Mean Humidity by Family")
