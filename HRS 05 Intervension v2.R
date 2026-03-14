
coefs <- coef(model_dementia)
internet_coefs <- coefs[grep("internet_use", names(coefs))]

RR_vec <- c(1, exp(internet_coefs)) 
names(RR_vec) <- c("never", "1", "2", ">=3")

prevalence_df_all <- df_dementia %>%
  group_by(internet_use) %>%
  summarise(weighted_n = sum(WEIGHT, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(prop = weighted_n / sum(weighted_n))

P_current <- prevalence_df_all$prop[match(names(RR_vec), prevalence_df_all$internet_use)]
names(P_current) <- names(RR_vec)

annual_data <- data.frame(
  Year = 2010:2021,
 
  Incident_Cases = c(454000, 457700, 461400, 465100, 469000, 473000, 
                     476000, 480000, 484000, 487000, 491000, 910000),
  
  Excess_Cost = c(22404, 28557, 29968, 31205, 31897, 32637, 
                  33576, 33435, 34323, 35010, 35875, 37505)
)

total_cumulative_cases <- sum(annual_data$Incident_Cases)

base_total_burden_B <- sum(annual_data$Incident_Cases * annual_data$Excess_Cost) / 1e9

scenarios_list <- list()
val_never <- P_current["never"]
val_1     <- P_current["1"]
val_2     <- P_current["2"]
val_3     <- P_current[">=3"]

P_s1 <- P_current
P_s1["1"]     <- val_1 + val_never
P_s1["never"] <- 0
scenarios_list[["S1_Digital_Inclusion"]] <- P_s1

P_s2 <- c(0, val_never, val_1, val_3 + val_2)
names(P_s2) <- names(P_current)
scenarios_list[["S2_Population_Shift"]] <- P_s2

P_s3 <- P_current
P_s3["2"]     <- val_2 + val_1 + val_never
P_s3["1"]     <- 0
P_s3["never"] <- 0
scenarios_list[["S3_Target_Compliance"]] <- P_s3

P_s4 <- c(0, 0, 0, 1)
names(P_s4) <- names(P_current)
scenarios_list[["S4_Theoretical_Max"]] <- P_s4

results_table <- data.frame(
  Scenario = character(),
  PIF_Percent = numeric(),
  Total_Cumulative_Cases = numeric(),
  Prevented_Cases_Total = numeric(),
  Total_Burden_B = numeric(),
  Savings_Billions = numeric(),
  stringsAsFactors = FALSE
)

results_table <- rbind(results_table, data.frame(
  Scenario = "Base_Current",
  PIF_Percent = 0,
  Total_Cumulative_Cases = total_cumulative_cases,
  Prevented_Cases_Total = 0,
  Total_Burden_B = base_total_burden_B,
  Savings_Billions = 0
))

calc_pif <- function(curr, counter, rr) {
  mean_curr <- sum(curr * rr)
  mean_new  <- sum(counter * rr)
  return((mean_curr - mean_new) / mean_curr)
}

for (name in names(scenarios_list)) {
  pif <- calc_pif(P_current, scenarios_list[[name]], RR_vec)
  annual_prevented_cases <- annual_data$Incident_Cases * pif
  cases_prev_total <- sum(annual_prevented_cases)
  
  annual_savings <- annual_prevented_cases * annual_data$Excess_Cost
  savings_total_B <- sum(annual_savings) / 1e9

  results_table <- rbind(results_table, data.frame(
    Scenario = name,
    PIF_Percent = pif * 100,
    Total_Cumulative_Cases = total_cumulative_cases - cases_prev_total,
    Prevented_Cases_Total = cases_prev_total,
    Total_Burden_B = base_total_burden_B - savings_total_B,
    Savings_Billions = savings_total_B
  ))
}

