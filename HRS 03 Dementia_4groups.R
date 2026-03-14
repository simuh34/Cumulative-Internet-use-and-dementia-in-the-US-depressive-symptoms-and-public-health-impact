#event analysis
df_cognition$hhid <- sub("^0", "", df_cognition$hhid)
df_cognition$hhidpn <- paste(as.character(df_cognition$hhid), as.character(df_cognition$pn), sep = "")

df$hhidpn <- as.numeric(df$hhidpn)
df_cognition$hhidpn <- as.numeric(df_cognition$hhidpn)

df_dementia <- left_join(
  df, df_cognition %>% select("hhidpn", "cogfunction2010", "cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018","cogfunction2020"),
  by = "hhidpn")

nrow(df_dementia)
table(df_dementia$cogfunction2010)
df_dementia %>%
  select("cogfunction2010", "cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018","cogfunction2020") %>%
  summary()

#delete participants had dimented problem in 2010 
df_dementia <- subset(df_dementia, cogfunction2010 != 3)

#construct outcome dementia event
for (i in seq(2012, 2020, by = 2)) {
  df_dementia[[paste0("dementia_", i)]] <- ifelse(df_dementia[[paste0("cogfunction", i)]] == 3, 1, 0)
}
df_dementia %>%
  select("dementia_2012","dementia_2014","dementia_2016","dementia_2018","dementia_2020") %>%
  summary()

#events_status
df_dementia <- df_dementia %>%
  mutate(
    event_status = case_when(
      (dementia_2012 == 1 | dementia_2014 == 1 | dementia_2016 == 1 | dementia_2018 == 1| dementia_2018 == 2| dementia_2020 == 2) ~ 1,
      is.na(dementia_2012) & is.na(dementia_2014) & is.na(dementia_2016) & is.na(dementia_2018) & is.na(dementia_2020)~ NA_real_,
      TRUE ~ 0  
    )
  )
#drop rows do not have any dementia records (n = 8490)
df_dementia <- df_dementia[!is.na(df_dementia$event_status),]


#NOTE:The logic behind calculating event_date is as follows: For event_status == 0 (no event), if inv_date14 is not missing, the event_date is set to inv_date14. If inv_date14 is missing but death_date exists and is earlier than the minimum of inv_date14, the event_date is set to death_date. If inv_date14 is missing and the death_date condition does not apply, the event_date is imputed using the median of inv_date14 from available non-missing values. If neither condition applies, the event_date is set to NA. For event_status == 1 (event occurred), if dementia_2012 == 1, the event_date is set to inv_date11; if dementia_2014 == 1, the event_date is set to inv_date12; if dementia_2016 == 1, the event_date is set to inv_date13; and if dementia_2018 == 1, the event_date is set to inv_date14. If none of these conditions apply, the event_date is set to NA. This approach ensures the event_date is logically determined based on available data and handles missing values appropriately. Temporary columns used for the calculation of the minimum and median of inv_date14 are removed at the end.
df_dementia <- df_dementia %>%
  mutate(
    min_inv_date15 = min(inv_date15, na.rm = TRUE),
    median_inv_date15 = median(inv_date15, na.rm = TRUE),
    event_date = case_when(
      event_status == 0 & !is.na(inv_date15) ~ as.Date(inv_date15, origin = "1960-01-01"),  
      event_status == 0 & !is.na(death_date) & death_date < min_inv_date15 ~ as.Date(death_date, origin = "1960-01-01"),  
      event_status == 0 & is.na(inv_date15) ~ as.Date(median_inv_date15, origin = "1960-01-01"),  
      event_status == 0 ~ as.Date(NA),  
      event_status == 1 ~ as.Date(
        coalesce(
          ifelse(dementia_2012 == 1, inv_date11, NA), 
          ifelse(dementia_2014 == 1, inv_date12, NA), 
          ifelse(dementia_2016 == 1, inv_date13, NA), 
          ifelse(dementia_2018 == 1, inv_date14, NA),
          ifelse(dementia_2020 == 1, inv_date15, NA)
        ), origin = "1960-01-01"
      ),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-min_inv_date15, -median_inv_date15)


df_dementia <- df_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, as.Date(inv_date10, origin = "1960-01-01"), units = "days")) / 30.44
  )


#reference group
df_dementia$internet_use <- factor(df_dementia$internet_use, 
                               levels = c("never", "1", "2", ">=3"))
df_dementia$internet_use <- relevel(df_dementia$internet_use, ref = "never")
df_dementia$Race <- factor(df_dementia$Race,levels = c("Non-Hispanic White" ,"Non-Hispanic Black","Hispanic","Other"))
df_dementia$Race <- relevel(df_dementia$Race,ref = "Non-Hispanic White" )
df_dementia$H_education <- factor(df_dementia$H_education)
df_dementia$H_education <- relevel(df_dementia$H_education, ref = "less than upper secondary")
#df_dementia$health_condition <- as.numeric(df_dementia$health_condition)
df_dementia$residence_rural <- factor(df_dementia$residence_rural)
df_dementia$residence_rural <- relevel(df_dementia$residence_rural, ref = "rural")
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "Men")
df_dementia$equivalized_wealth_quantile <- relevel(factor(df_dementia$equivalized_wealth_quantile), ref = "1")

####table

t1 = CreateTableOne(vars = c("age","gender","Race", "H_education","marital_status", "employment_status", "residence_rural","equivalized_wealth_quantile"),   
                    data=df_dementia,strata = "internet_use")
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE, nonnormal = "age") %>%
  knitr::kable(caption = "Descriptive charateristics by Internet usage", 
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

t2 = CreateTableOne(vars = c("age","gender","Race", "H_education","marital_status", "employment_status", "residence_rural","equivalized_wealth_quantile"),   
                    data=df_dementia)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE, nonnormal = "age") %>%
  knitr::kable(caption = "Descriptive charateristics", 
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

table(df_dementia[df_dementia$Race == "Other", "event_status"])

####model
library(survival)
surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia <- coxph(surv_obj ~ internet_use + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth_quantile, data = df_dementia, weights = iptw_weight)
summary(model_dementia)
exp(confint(model_dementia))

model_dementia_unadjusted <- coxph(surv_obj ~ internet_use, data = df_dementia, weights = iptw_weight)

#interaction test
#### Interaction Analysis ####

# Base model
model_dementia <- coxph(Surv(event_time, event_status) ~ 
                          internet_use + gender + age + age_sq + Race +
                          residence_rural + H_education + marital_status +
                          equivalized_wealth_quantile,
                        data = df_dementia, weights = iptw_weight)
summary(model_dementia)
conf_int <- confint(model_dementia)
round(exp(conf_int), 2)

# Gender interaction
model_dementia_gender_inter <- coxph(Surv(event_time, event_status) ~
                                       internet_use * gender + age + age_sq + Race +
                                       residence_rural + H_education + marital_status +
                                       equivalized_wealth_quantile,
                                     data = df_dementia, weights = iptw_weight)
summary(model_dementia_gender_inter)
conf_int <- confint(model_dementia_gender_inter)
round(exp(conf_int), 2)

# Race interaction
model_dementia_race_inter <- coxph(Surv(event_time, event_status) ~
                                     internet_use * Race + gender + age + age_sq +
                                     residence_rural + H_education + marital_status +
                                     equivalized_wealth_quantile,
                                   data = df_dementia, weights = iptw_weight)
summary(model_dementia_race_inter)
conf_int <- confint(model_dementia_race_inter)
round(exp(conf_int), 2)


library(ggplot2)
library(cowplot)

internet_labels <- c("1 time point", "2 time points", "≥3 time points")
groups <- c("Never", internet_labels)

colors <- c("#BDD7E7", "#6BAED6", "#3182BD", "#08519C")

blue_palette <- setNames(colors, groups)

coef_matrix <- summary(model_dementia)$coefficients
conf_matrix <- exp(confint(model_dementia))
internet_rows <- grep("^internet_use", rownames(coef_matrix))
internet_hr <- exp(coef_matrix[internet_rows, "coef"])
internet_lower <- conf_matrix[internet_rows, 1]
internet_upper <- conf_matrix[internet_rows, 2]
internet_p <- coef_matrix[internet_rows, "Pr(>|z|)"]

hr <- c(1.00, internet_hr)
lower <- c(1.00, internet_lower)
upper <- c(1.00, internet_upper)
p_values <- c(NA, internet_p)

plot_data <- data.frame(
  group = groups,
  hr = hr,
  lower = lower,
  upper = upper,
  p_value = p_values,
  row = length(groups):1,
  is_ref = c(TRUE, rep(FALSE, length(internet_labels)))
)

plot_data$group <- factor(plot_data$group, levels = groups)

plot_data$hr_text <- ifelse(plot_data$is_ref, "Ref", sprintf("%.2f", plot_data$hr))
plot_data$ci_text <- ifelse(plot_data$is_ref, "Ref", sprintf("%.2f - %.2f", plot_data$lower, plot_data$upper))
plot_data$p_text <- sapply(1:nrow(plot_data), function(i) {
  if (plot_data$is_ref[i]) return("Ref")
  p <- plot_data$p_value[i]
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.2f", p))
})

y_breaks <- plot_data$row
y_limits <- c(min(y_breaks) - 0.5, max(y_breaks) + 0.5)

theme_table <- theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.margin = margin(5, 5, 5, 5)
  )

p_labels <- ggplot(plot_data, aes(y = row)) +
  geom_text(aes(x = 1, label = group), size = 4, hjust = 1) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
  theme_table +
  theme(plot.margin = margin(10, 1, 10, 10))

p_forest <- ggplot(plot_data, aes(x = hr, y = row, color = group)) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.6) +
  
  geom_errorbarh(data = plot_data[!plot_data$is_ref, ],
                 aes(xmin = lower, xmax = upper), height = 0.2, 
                 linewidth = 0.8) +

  geom_point(size = 4, shape = 18) +

  scale_color_manual(values = blue_palette) +
  
  scale_x_log10(limits = c(0.3, 1.2), breaks = c(0.3, 0.5, 1.0), expand = c(0, 0)) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
  labs(x = "Hazard Ratio (95% CI)") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.6),
    axis.line.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 5, 10, 1),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length.x = unit(3, "pt"),
    legend.position = "none"
  )

p_hr <- ggplot(plot_data, aes(y = row)) +
  geom_text(aes(x = 0, label = hr_text), size = 4, hjust = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
  ggtitle("HR") + theme_table

p_ci <- ggplot(plot_data, aes(y = row)) +
  geom_text(aes(x = 0, label = ci_text), size = 4, hjust = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
  ggtitle("95% CI") + theme_table

p_pval <- ggplot(plot_data, aes(y = row)) +
  geom_text(aes(x = 0, label = p_text), size = 4, hjust = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
  ggtitle("P value") + theme_table

final_plot <- plot_grid(
  p_labels, p_forest, p_hr, p_ci, p_pval,
  ncol = 5,
  rel_widths = c(1.5, 2.2, 0.5, 0.9, 0.5),  
  align = "h"
)

####sensitivity analysis####
#lagged analysis
df_dementia_lag <- df_dementia[df_dementia$inw11 == 1,]
df_dementia_lag <- df_dementia[df_dementia_lag$death_year > 2012 | is.na(df_dementia_lag$death_year), ]

model_dementia_lag <- coxph(Surv(event_time, event_status) ~ 
                               internet_use + age + age_sq + gender + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth_quantile, 
                             data = df_dementia_lag, weights = iptw_weight)
summary(model_dementia_lag)
conf_int <- confint(model_dementia_lag)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_lag <- as.numeric(logLik(model_dementia_lag))

lr_stat <- 2 * (logLik_model_dementia_lag - logLik_model_dementia)
df_diff <- df.residual(model_dementia_lag) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#complete information n = 9891
df_complete <- read.csv("D:\\R project\\Digital\\dementia\\analysis\\2002-2010hrs_complete_4groups.csv")
df_complete <- df_complete[,-1]
df_complete_cognition <- read_dta("D:\\R project\\data\\hrs\\cogfinalimp_9520wide.dta")
table(df_complete_cognition$cogfunction2010)
df_complete$age_sq <- (df_complete$age)^2

#event analysis
df_complete_cognition$hhid <- sub("^0", "", df_complete_cognition$hhid)
df_complete_cognition$hhidpn <- paste(as.character(df_complete_cognition$hhid), as.character(df_complete_cognition$pn), sep = "")

df_complete$hhidpn <- as.numeric(df_complete$hhidpn)
df_complete_cognition$hhidpn <- as.numeric(df_complete_cognition$hhidpn)

df_complete_dementia <- left_join(
  df_complete, df_complete_cognition %>% select("hhidpn", "cogfunction2010", "cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018"),
  by = "hhidpn")

nrow(df_complete_dementia)
table(df_complete_dementia$cogfunction2010)
df_complete_dementia %>%
  select("cogfunction2010", "cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018") %>%
  summary()

#delete participants had dimented problem in 2010 
df_complete_dementia <- subset(df_complete_dementia, cogfunction2010 != 3)

#construct outcome dementia event
for (i in seq(2012, 2019, by = 2)) {
  df_complete_dementia[[paste0("dementia_", i)]] <- ifelse(df_complete_dementia[[paste0("cogfunction", i)]] == 3, 1, 0)
}
df_complete_dementia %>%
  select("dementia_2012","dementia_2014","dementia_2016","dementia_2018") %>%
  summary()

#events_status
df_complete_dementia <- df_complete_dementia %>%
  mutate(
    event_status = case_when(
      (dementia_2012 == 1 | dementia_2014 == 1 | dementia_2016 == 1 | dementia_2018 == 1) ~ 1,
      is.na(dementia_2012) & is.na(dementia_2014) & is.na(dementia_2016) & is.na(dementia_2018) ~ NA_real_,
      TRUE ~ 0  
    )
  )
#drop rows do not have any dementia records (n = 8490)
df_complete_dementia <- df_complete_dementia[!is.na(df_complete_dementia$event_status),]


#NOTE:The logic behind calculating event_date is as follows: For event_status == 0 (no event), if inv_date14 is not missing, the event_date is set to inv_date14. If inv_date14 is missing but death_date exists and is earlier than the minimum of inv_date14, the event_date is set to death_date. If inv_date14 is missing and the death_date condition does not apply, the event_date is imputed using the median of inv_date14 from available non-missing values. If neither condition applies, the event_date is set to NA. For event_status == 1 (event occurred), if dementia_2012 == 1, the event_date is set to inv_date11; if dementia_2014 == 1, the event_date is set to inv_date12; if dementia_2016 == 1, the event_date is set to inv_date13; and if dementia_2018 == 1, the event_date is set to inv_date14. If none of these conditions apply, the event_date is set to NA. This approach ensures the event_date is logically determined based on available data and handles missing values appropriately. Temporary columns used for the calculation of the minimum and median of inv_date14 are removed at the end.
df_complete_dementia <- df_complete_dementia %>%
  mutate(
    min_inv_date14 = min(inv_date14, na.rm = TRUE),
    median_inv_date14 = median(inv_date14, na.rm = TRUE),
    event_date = case_when(
      event_status == 0 & !is.na(inv_date14) ~ as.Date(inv_date14, origin = "1960-01-01"),  
      event_status == 0 & !is.na(death_date) & death_date < min_inv_date14 ~ as.Date(death_date, origin = "1960-01-01"),  
      event_status == 0 & is.na(inv_date14) ~ as.Date(median_inv_date14, origin = "1960-01-01"),  
      event_status == 0 ~ as.Date(NA),  
      event_status == 1 ~ as.Date(
        coalesce(
          ifelse(dementia_2012 == 1, inv_date11, NA), 
          ifelse(dementia_2014 == 1, inv_date12, NA), 
          ifelse(dementia_2016 == 1, inv_date13, NA), 
          ifelse(dementia_2018 == 1, inv_date14, NA)
        ), origin = "1960-01-01"
      ),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-min_inv_date14, -median_inv_date14)


df_complete_dementia <- df_complete_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, as.Date(inv_date10, origin = "1960-01-01"), units = "days")) / 30.44
  )


#reference group
df_complete_dementia$internet_use <- factor(df_complete_dementia$internet_use, 
                                   levels = c("never", "1", "2", ">=3"))
df_complete_dementia$internet_use <- relevel(df_complete_dementia$internet_use, ref = "never")
df_complete_dementia$Race <- factor(df_complete_dementia$Race,levels = c("Non-Hispanic White" ,"Other"))
df_complete_dementia$Race <- relevel(df_complete_dementia$Race,ref = "Non-Hispanic White" )
df_complete_dementia$H_education <- factor(df_complete_dementia$H_education)
df_complete_dementia$H_education <- relevel(df_complete_dementia$H_education, ref = "Less than upper secondary")
#df_complete_dementia$health_condition <- as.numeric(df_complete_dementia$health_condition)
df_complete_dementia$residence_rural <- factor(df_complete_dementia$residence_rural)
df_complete_dementia$residence_rural <- relevel(df_complete_dementia$residence_rural, ref = "rural")
df_complete_dementia$gender <- relevel(factor(df_complete_dementia$gender), ref = "Men")
df_complete_dementia$equivalized_wealth <- relevel(factor(df_complete_dementia$equivalized_wealth), ref = "Lower wealth")
model_dementia_complete <- coxph(Surv(event_time, event_status) ~ 
                                    internet_use + age + age_sq + gender + Race + 
                                    residence_rural + H_education + marital_status + 
                                    equivalized_wealth, 
                                  data = df_complete_dementia, weights = iptw_weight)
summary(model_dementia_complete)
conf_int <- confint(model_dementia_complete)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_complete <- as.numeric(logLik(model_dementia_complete))

lr_stat <- 2 * (logLik_model_dementia_complete - logLik_model_dementia)
df_diff <- df.residual(model_dementia_complete) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#sampling weight
df_dementia$WEIGHT <- as.numeric(df_dementia$r6weight)
df_dementia$iptw_weight[df_dementia$iptw_weight <= 0] <- 1e-6
df_dementia$WEIGHT[df_dementia$WEIGHT <= 0] <- 1e-6
model_dementia_sample <- coxph(Surv(event_time, event_status) ~ 
                                  internet_use + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth_quantile, 
                                data = df_dementia, weights = iptw_weight * WEIGHT)
summary(model_dementia_sample)
conf_int <- confint(model_dementia_sample)
round(exp(conf_int), 2)
round(exp(confint(model_dementia_sample)),2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_complete <- as.numeric(logLik(model_dementia_sample))

lr_stat <- 2 * (logLik_model_dementia_complete - logLik_model_dementia)
df_diff <- df.residual(model_dementia_complete) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#iptw < 15
df_dementia_iptw5 <- filter(df_dementia, df_dementia$iptw_weight < 15)
table(df_dementia_iptw5$internet_use)
model_dementia_iptw5 <- coxph(Surv(event_time, event_status) ~ 
                                 internet_use + age + age_sq + gender + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth_quantile, 
                               data = df_dementia_iptw5, weights = iptw_weight * WEIGHT)
summary(model_dementia_iptw5)
conf_int <- confint(model_dementia_iptw5)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_iptw5 <- as.numeric(logLik(model_dementia_iptw5))

lr_stat <- 2 * (logLik_model_dementia_iptw5 - logLik_model_dementia)
df_diff <- df.residual(model_dementia_iptw5) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#ipcw
df_dementia$ipcw_cum_inw14[df_dementia$ipcw_cum_inw14 <= 0] <- 1e-6
df_dementia$iptw_weight[df_dementia$iptw_weight <= 0] <- 1e-6
model_dementia_sample <- coxph(Surv(event_time, event_status) ~ 
                                  internet_use + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth_quantile, 
                                data = df_dementia, weights = iptw_weight * ipcw_cum_inw14)
summary(model_dementia_sample)
conf_int <- confint(model_dementia_sample)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_sample <- as.numeric(logLik(model_dementia_sample))

lr_stat <- 2 * (logLik_model_dementia_sample - logLik_model_dementia)
df_diff <- df.residual(model_dementia_sample) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#Pairwise Comparisons
library(emmeans)
emmeans_results <- emmeans(model_dementia, ~ internet_use)
comparisons <- contrast(emmeans_results, method = "revpairwise")
summary_result <- summary(comparisons, 
                          type = "response", 
                          infer = c(TRUE, TRUE))

summary_result$ratio <- as.numeric(summary_result$ratio)
summary_result$p.value <- as.numeric(summary_result$p.value)

summary_result$lower.CL <- exp(log(summary_result$ratio) - 1.96 * summary_result$SE)
summary_result$upper.CL <- exp(log(summary_result$ratio) + 1.96 * summary_result$SE)

summary_result$ratio <- sprintf("%.2f", round(summary_result$ratio, 2))
summary_result$lower.CL <- sprintf("%.2f", round(summary_result$lower.CL, 2))
summary_result$upper.CL <- sprintf("%.2f", round(summary_result$upper.CL, 2))
summary_result$p.value <- sprintf("%.2f", round(summary_result$p.value, 2))

final_output <- summary_result[, c("contrast", "ratio", "lower.CL", "upper.CL", "p.value")]
print(final_output)

#E-value
#extract rr
##define function
calc_e_value <- function(rr) {
  if (rr < 1) rr <- 1 / rr  # if rr < 1, take the reciprocal
  return(rr + rr * (rr - 1))
}

rr <- exp(coef(model_dementia))
variable_names <- names(rr)

e_values <- sapply(rr, calc_e_value)
results <- data.frame(Variable = variable_names, RiskRatio = rr, EValue = e_values)
print(results)
