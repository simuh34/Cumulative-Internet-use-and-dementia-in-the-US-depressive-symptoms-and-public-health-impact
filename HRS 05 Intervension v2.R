#========================================================================
# Economic Burden Analysis (Cumulative 2010-2021 Annual Incident Cohort)
#========================================================================

library(dplyr)
library(survival)
library(knitr)

# --- 0 & 1(A)(B) 保持你原来的代码不变 ---
library(dplyr)
library(survival)
library(knitr)
library(ggplot2)

# --- 0. 关键修正：确保权重列存在 ---
if (!"WEIGHT" %in% names(df_dementia)) {
  df_dementia$WEIGHT <- as.numeric(df_dementia$r6weight)
  df_dementia$WEIGHT[is.na(df_dementia$WEIGHT) | df_dementia$WEIGHT <= 0] <- 1e-6
}

# --- 1. 准备基础数据 ---

# (A) 提取 HR (Hazard Ratios)
coefs <- coef(model_dementia)
internet_coefs <- coefs[grep("internet_use", names(coefs))]

# 构建 RR 向量 (Reference = never = 1)
RR_vec <- c(1, exp(internet_coefs)) 
names(RR_vec) <- c("never", "1", "2", ">=3")

# (B) 计算全人群分布
prevalence_df_all <- df_dementia %>%
  group_by(internet_use) %>%
  summarise(weighted_n = sum(WEIGHT, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(prop = weighted_n / sum(weighted_n))

P_current <- prevalence_df_all$prop[match(names(RR_vec), prevalence_df_all$internet_use)]
names(P_current) <- names(RR_vec)
# --- 1(C). 载入 2010-2021 年的真实历史序列数据 ---
# 根据图片表格数据整理
annual_data <- data.frame(
  Year = 2010:2021,
  # 历年新发病例数 (Incident Dementia Number)
  Incident_Cases = c(454000, 457700, 461400, 465100, 469000, 473000, 
                     476000, 480000, 484000, 487000, 491000, 910000),
  # 历年超额成本 (最后一列：有 Dementia - 无 Dementia 的差值)
  Excess_Cost = c(22404, 28557, 29968, 31205, 31897, 32637, 
                  33576, 33435, 34323, 35010, 35875, 37505)
)

# 计算 12 年的累积 Base Case (基线总发病人数与基线总经济负担)
total_cumulative_cases <- sum(annual_data$Incident_Cases)
# 每年的负担 = 当年的新发人数 * 当年的超额成本，然后求总和
base_total_burden_B <- sum(annual_data$Incident_Cases * annual_data$Excess_Cost) / 1e9

# --- 2. 定义 4 个标准 Scenarios (保持你原来的不变) ---
scenarios_list <- list()
val_never <- P_current["never"]
val_1     <- P_current["1"]
val_2     <- P_current["2"]
val_3     <- P_current[">=3"]

# 【Scenario 1: 扫盲 (Digital Inclusion)】
P_s1 <- P_current
P_s1["1"]     <- val_1 + val_never
P_s1["never"] <- 0
scenarios_list[["S1_Digital_Inclusion"]] <- P_s1

# 【Scenario 2: 全员晋级 (Population Shift)】
P_s2 <- c(0, val_never, val_1, val_3 + val_2)
names(P_s2) <- names(P_current)
scenarios_list[["S2_Population_Shift"]] <- P_s2

# 【Scenario 3: 达标 (Target Compliance)】
P_s3 <- P_current
P_s3["2"]     <- val_2 + val_1 + val_never
P_s3["1"]     <- 0
P_s3["never"] <- 0
scenarios_list[["S3_Target_Compliance"]] <- P_s3

# 【Scenario 4: 理论极值 (Theoretical Max)】
P_s4 <- c(0, 0, 0, 1)
names(P_s4) <- names(P_current)
scenarios_list[["S4_Theoretical_Max"]] <- P_s4

# --- 3. 计算并输出专门用于填表的数据 (升级为逐年累加逻辑) ---

cat("\n=======================================================\n")
cat("          DATA FOR MAIN TABLE EXPORT (2010-2021 CUMULATIVE)\n")
cat("=======================================================\n\n")

# 初始化结果表
results_table <- data.frame(
  Scenario = character(),
  PIF_Percent = numeric(),
  Total_Cumulative_Cases = numeric(),
  Prevented_Cases_Total = numeric(),
  Total_Burden_B = numeric(),
  Savings_Billions = numeric(),
  stringsAsFactors = FALSE
)

# 写入 Base Case 行
results_table <- rbind(results_table, data.frame(
  Scenario = "Base_Current",
  PIF_Percent = 0,
  Total_Cumulative_Cases = total_cumulative_cases,
  Prevented_Cases_Total = 0,
  Total_Burden_B = base_total_burden_B,
  Savings_Billions = 0
))

# 定义 PIF 计算函数 (保持你原来的不变)
calc_pif <- function(curr, counter, rr) {
  mean_curr <- sum(curr * rr)
  mean_new  <- sum(counter * rr)
  return((mean_curr - mean_new) / mean_curr)
}

# 核心改动：在循环里进行逐年相乘与累加
for (name in names(scenarios_list)) {
  # 1. 算出该场景下的 PIF
  pif <- calc_pif(P_current, scenarios_list[[name]], RR_vec)
  
  # 2. 向量化计算：每一年预防的 Case 数
  annual_prevented_cases <- annual_data$Incident_Cases * pif
  cases_prev_total <- sum(annual_prevented_cases)
  
  # 3. 向量化计算：每一年节省的钱 (当年预防人数 * 当年超额成本)
  annual_savings <- annual_prevented_cases * annual_data$Excess_Cost
  savings_total_B <- sum(annual_savings) / 1e9
  
  # 写入结果
  results_table <- rbind(results_table, data.frame(
    Scenario = name,
    PIF_Percent = pif * 100,
    Total_Cumulative_Cases = total_cumulative_cases - cases_prev_total,
    Prevented_Cases_Total = cases_prev_total,
    Total_Burden_B = base_total_burden_B - savings_total_B,
    Savings_Billions = savings_total_B
  ))
}

# 打印表格
print(knitr::kable(results_table, 
                   digits = 2,
                   format.args = list(big.mark = ",")))
