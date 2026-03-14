
# Define sample inclusion criteria
df <- df[df$inw6 == 1,] 
df <- df[complete.cases(df$age),]
df <- df[df$age >= 50, ]
df <- df[df$death_year > 2010 | is.na(df$death_year), ]
df <- df[df$inw10 == 1,]

# Original sample size at this stage
print(paste("Sample size before imputation:", nrow(df)))

# =====================================================
# Create COMPLETE CASES dataset (no missing values)
# =====================================================
df_complete <- df[complete.cases(df[, c("w6", "w7", "w8", "w9", "w10")]), ]

# Construct internet_use_count for complete cases
df_complete$internet_use_count <- rowSums(df_complete[, c("w6", "w7", "w8", "w9", "w10")])

# Create internet_use categories for complete cases
library(dplyr)
df_complete$internet_use <- case_when(
  df_complete$internet_use_count == 0 ~ "never",
  df_complete$internet_use_count == 1 ~ "1",
  df_complete$internet_use_count == 2 ~ "2",
  df_complete$internet_use_count > 2  ~ ">=3",
  TRUE ~ NA_character_ 
)
df <- df_complete
# =====================================================
# Data cleaning
# =====================================================
#gender
df$gender <- as.factor(ifelse(df$gender == 1 , "Men", "Women"))

#Race
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
df$Race <- ifelse(df$race == 1 & 
                         df$ethnicity == 0, 0,
                       ifelse(df$race == 2 & 
                                df$ethnicity == 0, 1,
                              ifelse(df$ethnicity == 1, 2, 3)))

df$Race <- ifelse(df$Race == 0, "Non-Hispanic White" , 
                       ifelse(df$Race == 1, "Non-Hispanic Black",
                              ifelse(df$Race == 2, "Hispanic","Other")))

table(df$Race)

#marital_status
##1.Married 2.Married4.Separated 5.Divorced 6.null 7.Widowed 8.Never married 9.null
df <- df %>%
  mutate(marital_status = case_when(
    marital_status %in% c(1, 2) ~ "Married/Parterned",  # Married
    marital_status %in% c(4, 5, 7, 8) ~ "Other",      # Separated, Divorced, Widowed, Never married
    TRUE ~ NA_character_  # NA for any other values
  ))
table(df$marital_status)
df$marital_status <- factor(df$marital_status,level = c("Married/Parterned","Other"))

#employment_status
##1.Works FT 2.Works PT 3.Unemployed 4.Partly retired 5.Retired 6.Disabled 7.Not in LbrF 
#recode 1=employed, 2=retired, 3=other
df$employment_status <- ifelse(df$employment_status==1, "employed","other")
table(df$employment_status)
df$employment_status <- factor(df$employment_status,level = c("employed","retired", "other"))

#residence_region
#1.Northeast 2.Midwest 3.South 4.West 5.Other
df <- df %>%
  mutate(residence_region = case_when(
    residence_region == 1 ~ "Northeast",
    residence_region == 2 ~ "Midwest",
    residence_region == 3 ~ "South",
    residence_region == 4 ~ "West",
    residence_region == 5 ~ "Other",
    TRUE ~ NA_character_  # NA
  ))
table(df$residence_region)
df$residence_region <- factor(df$residence_region)

#rural
df <- df %>%
  mutate(residence_rural = case_when(
    residence_rural == 0 ~ "urban",
    residence_rural == 1 ~ "rural",
    TRUE ~ NA_character_  # NA
  ))
table(df$residence_rural)
df$residence_rural <- factor(df$residence_rural)

#health_condition
table(df$r10conds)

#living_with_others
df$living_with_others <- as.numeric(df$living_with_others)

#equivalized_wealth
df$equivalized_wealth <- df$wealth/sqrt(df$living_with_others)
df$equivalized_wealth <- round(df$equivalized_wealth/1000, 2)
summary(df$equivalized_wealth)

quantiles <- quantile(df$equivalized_wealth, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

df$equivalized_wealth_quantile <- cut(
  df$equivalized_wealth,
  breaks = quantiles,         
  labels = c(1, 2, 3, 4),    
  include.lowest = TRUE,     
  na.rm = FALSE              
)
table(df$equivalized_wealth_quantile, useNA = "ifany")


#education
summary(df$H_education)
df <- df %>%
  mutate(H_education = case_when(
    H_education == 1 ~ "less than upper secondary",
    H_education == 2 ~ "upper secondary and vocat",
    H_education == 3 ~ "tertiary",
    TRUE ~ NA_character_
  ))

table(df$H_education)

####exposure digital usage####
df$internet_use <- factor(df$internet_use, levels = c("never", "1", "2", ">=3"))
df$internet_use <- relevel(df$internet_use, ref = "never")

#imputation
df_converted <- df[, c("gender","Race", "H_education","marital_status", "employment_status", "residence_rural","equivalized_wealth_quantile")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c("gender","Race", "H_education","marital_status", "employment_status", "residence_rural","equivalized_wealth_quantile"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 1)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]
#write.csv(df,"D:\\R project\\Digital\\dementia\\analysis\\hrs_imputation.csv")

####iptw####
library(nnet) 
iptw_m <- multinom(internet_use ~ age + gender + Race + H_education + marital_status, 
                   data = df, 
                   trace = FALSE) 

df$propensity_scores <- predict(iptw_m, type = "probs")  

summary(df$propensity_scores)
head(df$propensity_scores) 

df$internet_use <- factor(df$internet_use, 
                          levels = c("never", "1", "2", ">=3"))

df$iptw_weight <- NA

for (i in 1:length(levels(df$internet_use))) {  

  group <- levels(df$internet_use)[i]
  df$iptw_weight[df$internet_use == group] <- 
    1 / df$propensity_scores[df$internet_use == group, i]
}

summary(df$iptw_weight)  

quantile_99 <- quantile(df$iptw_weight, 0.99, na.rm = TRUE)

df$iptw_weight <- ifelse(df$iptw_weight > quantile_99, quantile_99, df$iptw_weight)

summary(df$iptw_weight)  

####ipaw####
library(dplyr)
library(tidyr)
library(glm2)

df <- df %>%
  mutate(

    death_inw11 = ifelse(inw11 == 5, 1, ifelse(inw11 < 5, 0, NA)),
    death_inw12 = ifelse(inw12 == 5, 1, ifelse(inw12 < 5, 0, NA)),
    death_inw13 = ifelse(inw13 == 5, 1, ifelse(inw13 < 5, 0, NA)),
    death_inw14 = ifelse(inw14 == 5, 1, ifelse(inw14 < 5, 0, NA)),
    death_inw15 = ifelse(inw15 == 5, 1, ifelse(inw15 < 5, 0, NA)),
    death_inw16 = ifelse(inw16 == 5, 1, ifelse(inw16 < 5, 0, NA)),
    
  
    lost_inw11 = ifelse(inw11 == 7 | inw11 == 4, 1, ifelse(inw11 == 1, 0, NA)),
    lost_inw12 = ifelse(inw12 == 7 | inw12 == 4, 1, ifelse(inw12 == 1, 0, NA)),
    lost_inw13 = ifelse(inw13 == 7 | inw13 == 4, 1, ifelse(inw13 == 1, 0, NA)),
    lost_inw14 = ifelse(inw14 == 7 | inw14 == 4, 1, ifelse(inw14 == 1, 0, NA)),
    lost_inw15 = ifelse(inw15 == 7 | inw15 == 4, 1, ifelse(inw15 == 1, 0, NA)),
    lost_inw16 = ifelse(inw16 == 7 | inw16 == 4, 1, ifelse(inw16 == 1, 0, NA))
  )

library(dplyr)

for (wave in 11:16) {

  death_var <- paste0("death_inw", wave)
  lost_var <- paste0("lost_inw", wave)

  model_death <- glm(as.formula(paste(death_var, "~ age + gender + Race + H_education + marital_status")),
                     data = df, 
                     family = binomial, 
                     control = glm.control(maxit = 1000), 
                     na.action = na.exclude)  
  
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + Race + H_education + marital_status")),
                    data = df, 
                    family = binomial, 
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)  
  
  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")
  
  # Check if both prediction vectors are the same length as df
  if (length(pred_death) == nrow(df) && length(pred_lost) == nrow(df)) {
    df <- df %>%
      mutate(!!paste0("prob_alive_inw", wave) := 
               (1 - pred_death) * (1 - pred_lost))
  } else {
    warning("not match", wave)
  }
}

for (wave in 11:16) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)
  
  df <- df %>%
    mutate(!!ipcw_var := 1 / get(prob_var))
}

df <- df %>%
  mutate(
    ipcw_cum_inw11 = ipcw_inw11,
    ipcw_cum_inw12 = ifelse(is.na(ipcw_inw11), NA, ipcw_inw12 * ipcw_inw11),
    ipcw_cum_inw13 = ifelse(is.na(ipcw_cum_inw12), NA, ipcw_inw13 * ipcw_cum_inw12),
    ipcw_cum_inw14 = ifelse(is.na(ipcw_cum_inw13), NA, ipcw_inw14 * ipcw_cum_inw13),
    ipcw_cum_inw15 = ifelse(is.na(ipcw_cum_inw14), NA, ipcw_inw15 * ipcw_cum_inw14),
    ipcw_cum_inw16 = ifelse(is.na(ipcw_cum_inw15), NA, ipcw_inw16 * ipcw_cum_inw15)
  )



    
    lost_inw11 = ifelse(inw11 == 7 | inw11 == 4, 1, ifelse(inw11 == 1, 0, NA)),
    lost_inw12 = ifelse(inw12 == 7 | inw12 == 4, 1, ifelse(inw12 == 1, 0, NA)),
    lost_inw13 = ifelse(inw13 == 7 | inw13 == 4, 1, ifelse(inw13 == 1, 0, NA)),
    lost_inw14 = ifelse(inw14 == 7 | inw14 == 4, 1, ifelse(inw14 == 1, 0, NA)),
    lost_inw15 = ifelse(inw15 == 7 | inw15 == 4, 1, ifelse(inw15 == 1, 0, NA)),
    lost_inw16 = ifelse(inw16 == 7 | inw16 == 4, 1, ifelse(inw16 == 1, 0, NA))
  )

library(dplyr)

# Step 2: 
for (wave in 11:16) {
  
  death_var <- paste0("death_inw", wave)
  lost_var <- paste0("lost_inw", wave)
  
  model_death <- glm(as.formula(paste(death_var, "~ age + gender + Race + H_education + marital_status")),
                     data = df, 
                     family = binomial, 
                     control = glm.control(maxit = 1000), 
                     na.action = na.exclude)  
  
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + Race + H_education + marital_status")),
                    data = df, 
                    family = binomial, 
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)  
  
  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")
  
  # Check if both prediction vectors are the same length as df
  if (length(pred_death) == nrow(df) && length(pred_lost) == nrow(df)) {
    df <- df %>%
      mutate(!!paste0("prob_alive_inw", wave) := 
               (1 - pred_death) * (1 - pred_lost))
  } else {
    warning("not match", wave)
  }
}

# Step 3: 
for (wave in 11:16) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)
  
  df <- df %>%
    mutate(!!ipcw_var := 1 / get(prob_var))
}

# Step 4: 
df <- df %>%
  mutate(
    ipcw_cum_inw11 = ipcw_inw11,
    ipcw_cum_inw12 = ifelse(is.na(ipcw_inw11), NA, ipcw_inw12 * ipcw_inw11),
    ipcw_cum_inw13 = ifelse(is.na(ipcw_cum_inw12), NA, ipcw_inw13 * ipcw_cum_inw12),
    ipcw_cum_inw14 = ifelse(is.na(ipcw_cum_inw13), NA, ipcw_inw14 * ipcw_cum_inw13),
    ipcw_cum_inw15 = ifelse(is.na(ipcw_cum_inw14), NA, ipcw_inw15 * ipcw_cum_inw14),
    ipcw_cum_inw16 = ifelse(is.na(ipcw_cum_inw15), NA, ipcw_inw16 * ipcw_cum_inw15)
  )

df %>%
  select(ipcw_cum_inw11, ipcw_cum_inw12, ipcw_cum_inw13, ipcw_cum_inw14, ipcw_cum_inw15,ipcw_cum_inw16) %>%
  summary()








