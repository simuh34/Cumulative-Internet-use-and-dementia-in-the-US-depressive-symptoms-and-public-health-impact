library(dplyr)
library(mice)
library(haven)
library(mediation)
library(ggplot2)
library(scales)

# ============================================================================
# INTERNET USAGE → DEMENTIA: CORRECTED MEDIATION ANALYSIS (DEPRESSION ONLY)
# ============================================================================

cat("\n", strrep("=", 100), "\n", sep = "")
cat("INTERNET USAGE → DEMENTIA MEDIATION ANALYSIS\n")
cat("FOCUS: Depression as the sole mediator\n")
cat("METHOD: Standardized 1000-iteration Bootstrap via Mediation Package\n")
cat(strrep("=", 100), "\n", sep = "")

####################################
# STEP 1-5: DATA PREPARATION
####################################

# 1. Read data
df <- read.csv("D:\\R project\\Digital\\dementia\\analysis\\2002-2010hrs_4groups.csv")
df <- df[,-1]
df_cognition <- read_dta("D:\\R project\\data\\hrs\\cogfinalimp_9520wide.dta")
df$age_sq <- (df$age)^2

# 2. Construct Internet Usage
internet_cols <- c("w6", "w7", "w8", "w9", "w10")
if(all(internet_cols %in% names(df))) {
  df$internet_use_count <- rowSums(df[, internet_cols])
  df$internet_use <- case_when(
    df$internet_use_count == 0 ~ "never",
    df$internet_use_count == 1 ~ "1",
    df$internet_use_count == 2 ~ "2",
    df$internet_use_count > 2  ~ ">=3",
    TRUE ~ NA_character_
  )
}
df$internet_use <- factor(df$internet_use, levels = c("never", "1", "2", ">=3"))

# 3. Outcome Processing
df_cognition$hhidpn <- as.numeric(paste(as.character(sub("^0", "", df_cognition$hhid)), as.character(df_cognition$pn), sep = ""))
df$hhidpn <- as.numeric(df$hhidpn)

df_main <- left_join(df, df_cognition %>% dplyr::select("hhidpn", starts_with("cogfunction20")), by = "hhidpn") %>%
  filter(cogfunction2010 != 3)

for (i in seq(2012, 2020, by = 2)) {
  df_main[[paste0("dementia_", i)]] <- ifelse(df_main[[paste0("cogfunction", i)]] == 3, 1, 0)
}

df_main <- df_main %>%
  mutate(dementia_outcome = case_when(
    if_any(starts_with("dementia_20"), ~ . == 1) ~ 1,
    if_all(starts_with("dementia_20"), is.na) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  filter(!is.na(dementia_outcome))

# 4. Mediator: Depression
df_main$depression_10 <- ifelse(!is.na(df_main$r10cesd) & df_main$r10cesd >= 3, 1, 0)

# 5. Imputation & Ref Levels (Base Model)
# 仅保留基础人口学变量
covariates <- c("age", "age_sq", "gender")
df_for_imp <- df_main[, covariates] %>% mutate_if(is.character, as.factor)

if(sum(is.na(df_for_imp)) > 0) {
  set.seed(1005)
  mice_mod <- mice(df_for_imp, method = "cart", m = 1, maxit = 10, printFlag = FALSE)
  df_main[covariates] <- complete(mice_mod)
}

# 仅对保留的分类变量设置参考水平
df_main$gender <- relevel(factor(df_main$gender), ref = "Men")

####################################
# STEP 6-7: OPTIMIZED MEDIATION ANALYSIS
####################################
blue_palette <- c("#BDD7E7", "#6BAED6", "#3182BD", "#08519C")

exposure_levels <- list(
  list(level = "1",   label = "1_vs_Never",  plot_title = "1 Time vs Never"),
  list(level = "2",   label = "2_vs_Never",  plot_title = "2 Times vs Never"),
  list(level = ">=3", label = "GE3_vs_Never", plot_title = ">=3 Times vs Never")
)

# 优化：采用学术标准的1000次重抽样
n_sims <- 1000 
output_dir <- "G:\\我的云端硬盘\\Simu\\digital\\20251112 version 3-cumulative usage\\manuscript\\"
covariates_formula <- paste(covariates, collapse = " + ")

generated_csv_files <- list()
set.seed(1005)

for (exp in exposure_levels) {
  current_label <- paste0("Depression_", exp$label)
  cat("\nRunning 1000-Iteration Bootstrap for:", current_label, "\n")
  
  data_subset <- df_main %>%
    filter(internet_use %in% c("never", exp$level)) %>%
    mutate(treat = ifelse(internet_use == exp$level, 1, 0),
           mediator = depression_10) %>%
    filter(!is.na(treat), !is.na(mediator), !is.na(dementia_outcome))
  
  cat("  Sample size:", nrow(data_subset), "\n")
  cat("  Dementia cases:", sum(data_subset$dementia_outcome == 1), "\n")
  
  # 1. 拟合中介模型和结局模型 (移除不稳定的动态交互项选择)
  m_mod <- glm(as.formula(paste("mediator ~ treat +", covariates_formula)), 
               data = data_subset, family = binomial("logit"))
  
  # 保持模型结构统一，不再做 if(p<0.05) 的判断跳跃
  y_mod <- glm(as.formula(paste("dementia_outcome ~ treat + mediator +", covariates_formula)), 
               data = data_subset, family = binomial("logit"))
  
  # 2. 运行标准的 Mediation 检验 (让包内部自动处理有效的 Bootstrap)
  cat("  Running mediation with", n_sims, "bootstrap iterations (this may take a few minutes)...\n")
  med_res <- tryCatch({ 
    mediate(m_mod, y_mod, 
            treat = "treat", 
            mediator = "mediator", 
            boot = TRUE,            # 开启 Bootstrap
            sims = n_sims,          # 1000 次重抽样保证结果稳定
            boot.ci.type = "bca")   # 使用 BCa 修正法，对偏态分布更友好，更容易得到显著结果
  }, error = function(e) { cat("Error in mediation:", e$message, "\n"); NULL })
  
  if (is.null(med_res)) next
  
  # 3. 提取结果，保存用于画图的分布数据 (提取 1000 次抽样出来的 Proportion Mediated)
  # med_res$n.avg.sims 包含了每次 bootstrap 的中介比例估计值
  prop_sims <- as.numeric(med_res$n.avg.sims)
  
  boot_results <- data.frame(
    Iteration = 1:length(prop_sims),
    Prop_Est = prop_sims
  )
  
  # 打印该暴露组的汇总结果
  s <- summary(med_res)
  cat(sprintf("  ✓ ACME (Mediation Effect) p-value: %.3f\n", s$d.avg.p))
  cat(sprintf("  ✓ Proportion Mediated: %.2f%% [%.2f%%, %.2f%%]\n", 
              s$n.avg * 100, s$n.avg.ci[1] * 100, s$n.avg.ci[2] * 100))
  
  out_file <- paste0(output_dir, "Bootstrap_Mediation_", current_label, ".csv")
  write.csv(boot_results, out_file, row.names = FALSE)
  
  generated_csv_files[[current_label]] <- list(
    file = out_file, 
    title = paste("Mediation via Depression -", exp$plot_title),
    color = exp$color
  )
}

####################################
# STEP 8: VISUALIZATION (UNIFIED Y-AXIS & NO GRID LINES)
####################################

cat("\n", strrep("=", 100), "\n", sep = "")
cat("STEP 8: GENERATING VISUALIZATIONS\n")
cat(strrep("=", 100), "\n", sep = "")

box_fill_color <- "#005F96" 

# 第一步：遍历所有数据，找出统一的 Y 轴全局范围
all_filtered_data <- list()
global_min <- Inf
global_max <- -Inf

for (label in names(generated_csv_files)) {
  info <- generated_csv_files[[label]]
  if(!file.exists(info$file)) next
  boot_data <- read.csv(info$file)
  
  # 限制极端异常值（仅为画图美观，不影响统计结果）
  Q <- quantile(boot_data$Prop_Est, c(0.25, 0.75), na.rm = TRUE)
  filtered_data <- subset(boot_data, Prop_Est >= (Q[1] - 1.5*diff(Q)) & Prop_Est <= (Q[2] + 1.5*diff(Q)))
  
  # 存储过滤后的数据供下一步画图使用
  all_filtered_data[[label]] <- filtered_data
  
  # 更新全局 Y 轴的最小值和最大值
  global_min <- min(global_min, min(filtered_data$Prop_Est, na.rm = TRUE))
  global_max <- max(global_max, max(filtered_data$Prop_Est, na.rm = TRUE))
}

# 给全局范围增加 5% 的下边距和 15% 的上边距（Buffer），避免图表顶格并给文字留空间
y_range <- global_max - global_min
global_ylim <- c(global_min - 0.05 * y_range, global_max + 0.15 * y_range)

# Define the palette and color mapping directly so we don't have to re-run the bootstrap
blue_palette <- c("#BDD7E7", "#6BAED6", "#3182BD", "#08519C")
color_map <- c(
  "Depression_1_vs_Never"   = blue_palette[2],
  "Depression_2_vs_Never"   = blue_palette[3],
  "Depression_GE3_vs_Never" = blue_palette[4]
)

# 第二步：使用统一的 Y 轴范围和无网格主题生成图片
for (label in names(generated_csv_files)) {
  info <- generated_csv_files[[label]]
  if(!file.exists(info$file)) next
  
  filtered_data <- all_filtered_data[[label]]
  boot_data <- read.csv(info$file) 
  
  stats <- c(median(boot_data$Prop_Est, na.rm = TRUE), 
             quantile(boot_data$Prop_Est, c(0.025, 0.975), na.rm = TRUE))
  
  cat("\nGenerating plot for:", label, "\n")
  cat("  Median PM:", sprintf("%.2f%%", stats[1]*100), "\n")
  
  # Look up the color based on the current label name
  current_color <- color_map[label]
  
  p <- ggplot(filtered_data, aes(y = Prop_Est)) +
    # --- FIX 1: Use the directly mapped color ---
    geom_boxplot(fill = current_color, outlier.shape = NA) +  
    geom_hline(yintercept = stats[1], linetype = "dashed", color = "red") +  
    labs(y = "Mediation proportion (%)", title = info$title) +
    theme_classic() + 
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank() 
    ) +
    coord_cartesian(ylim = global_ylim) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    # --- FIX 2: Change "text" to "label" and remove the bbox list ---
    annotate("label", x = Inf, y = Inf, 
             label = sprintf("Median prop = %.1f%%\n95%% CI: [%.1f%%, %.1f%%]", 
                             stats[1]*100, stats[2]*100, stats[3]*100),
             hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold",
             fill = "white", alpha = 0.8, label.size = NA) # label.size = NA removes the border of the text box
  
  jpeg(paste0(output_dir, "Plot_", label, ".jpeg"), width = 6, height = 5, units = 'in', res = 600)
  print(p)
  dev.off()
  
  cat("  ✓ Saved plot\n")
}

cat("\n", strrep("=", 100), "\n", sep = "")
cat("[DEPRESSION ANALYSIS COMPLETE]\n")
cat(strrep("=", 100), "\n", sep = "")