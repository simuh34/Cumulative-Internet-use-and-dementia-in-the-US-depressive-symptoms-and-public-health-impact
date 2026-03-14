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
} else {
  cat("WARNING: Internet variables w6-w10 not all found. Using existing columns...\n")
  df$internet_use <- df$internet_use
}

df$internet_use <- factor(df$internet_use, levels = c("never", "1", "2", ">=3"))

df_cognition$hhid <- sub("^0", "", df_cognition$hhid)
df_cognition$hhidpn <- paste(as.character(df_cognition$hhid), as.character(df_cognition$pn), sep = "")
df_cognition$hhidpn <- as.numeric(df_cognition$hhidpn)

df$hhidpn <- as.numeric(df$hhidpn)
df_cognition$hhidpn <- as.numeric(df_cognition$hhidpn)

df_main <- left_join(
  df, df_cognition %>% dplyr::select("hhidpn", "cogfunction2010", "cogfunction2012",
                                     "cogfunction2014", "cogfunction2016", 
                                     "cogfunction2018", "cogfunction2020"),
  by = "hhidpn")

df_main <- subset(df_main, cogfunction2010 != 3)
cat("After excluding baseline dementia:", nrow(df_main), "\n")

for (i in seq(2012, 2020, by = 2)) {
  df_main[[paste0("dementia_", i)]] <- ifelse(df_main[[paste0("cogfunction", i)]] == 3, 1, 0)
}

df_main <- df_main %>%
  mutate(
    dementia_outcome = case_when(
      (dementia_2012 == 1 | dementia_2014 == 1 | dementia_2016 == 1 | 
         dementia_2018 == 1 | dementia_2020 == 1) ~ 1,
      is.na(dementia_2012) & is.na(dementia_2014) & is.na(dementia_2016) & 
        is.na(dementia_2018) & is.na(dementia_2020) ~ NA_real_,
      TRUE ~ 0
    )
  )

df_main <- df_main[!is.na(df_main$dementia_outcome),]
df_main$depression_10 <- ifelse(!is.na(df_main$r10cesd) & df_main$r10cesd >= 3, 1, 0)

if("loneliness" %in% names(df_main)) {
  cat("   Found loneliness\n")
  cat("   loneliness distribution:\n")
  print(table(df_main$loneliness, useNA = "ifany"))
  df_main$loneliness_10 <- ifelse(!is.na(df_main$loneliness) & (df_main$loneliness == 1|df_main$loneliness == 2), 1, 0)
  cat("   Loneliness prevalence:", sum(df_main$loneliness_10 == 1, na.rm = TRUE), "\n")
} else {
  cat("   WARNING: loneliness variable not found, trying alternative names...\n")
  # Try alternative column names
  possible_names <- c("r10lonelyp", "r10lonely", "loneliness10", "lonely10")
  found <- FALSE
  for(name in possible_names) {
    if(name %in% names(df_main)) {
      cat("   Found alternative:", name, "\n")
      df_main$loneliness_10 <- ifelse(!is.na(df_main[[name]]) & df_main[[name]] == 1, 1, 0)
      found <- TRUE
      break
    }
  }
  if(!found) {
    cat("   ERROR: No loneliness variable found!\n")
    df_main$loneliness_10 <- NA
  }
}

covariates <- c("age", "age_sq", "gender", "Race", "residence_rural",
                "H_education", "marital_status", "equivalized_wealth")

vars_to_check <- c("dementia_outcome", "depression_10", "loneliness_10", covariates)

cat("\nMissing data summary:\n")
for(var in vars_to_check) {
  if(var %in% names(df_main)) {
    cat("  ", var, "- Missing:", sum(is.na(df_main[[var]])), "\n")
  }
}

df_for_imputation <- df_main[, covariates, drop = FALSE]
df_for_imputation <- df_for_imputation %>%
  mutate_if(names(.) %in% c("gender", "Race", "residence_rural",
                            "H_education", "marital_status"), as.factor)

covariate_missing <- sapply(df_for_imputation, function(x) sum(is.na(x)))

if(sum(covariate_missing) > 0) {
  set.seed(1005)
  mice_mod <- mice(df_for_imputation, method = "cart", m = 1, maxit = 5, printFlag = FALSE)
  imputed_data <- complete(mice_mod)
  
  common_cols <- intersect(names(df_main), names(imputed_data))
  df_main[common_cols] <- imputed_data[common_cols]
}

df_main$gender <- relevel(factor(df_main$gender), ref = "Men")
df_main$Race <- relevel(factor(df_main$Race, levels = c("Non-Hispanic White", "Other")),
                        ref = "Non-Hispanic White")
df_main$H_education <- relevel(factor(df_main$H_education), ref = "Less than upper secondary")
df_main$residence_rural <- relevel(factor(df_main$residence_rural), ref = "rural")
df_main$marital_status <- relevel(factor(df_main$marital_status), ref = "Married/Parterned")
df_main$equivalized_wealth <- relevel(factor(df_main$equivalized_wealth), ref = "Lower wealth")

run_mediation_analysis <- function(data, mediator_var, mediator_name,
                                   exposure_level, exposure_label, covariates,
                                   n_bootstrap = 50) {

  analysis_vars <- c("dementia_outcome", "internet_use", mediator_var, covariates)
  # Ensure we only check complete cases for existing columns
  valid_vars <- analysis_vars[analysis_vars %in% names(data)]
  data_clean <- data[complete.cases(data[valid_vars]), ]
  
  data_clean <- data_clean[data_clean$internet_use %in% c("never", exposure_level), ]
  data_clean$exposure_binary <- ifelse(data_clean$internet_use == exposure_level, 1, 0)
  
  if (nrow(data_clean) < 50) {
    cat("  ✗ Insufficient sample size:", nrow(data_clean), "\n")
    return(NULL)
  }
  
  data_clean$treat <- data_clean$exposure_binary
  data_clean$mediator <- data_clean[[mediator_var]]

  X_vars_str <- paste(covariates, collapse = " + ")

  f_med_txt <- paste("mediator ~ treat +", X_vars_str)
  f_out_inter_txt <- paste("dementia_outcome ~ treat + mediator + treat:mediator +", X_vars_str)
  f_out_nointer_txt <- paste("dementia_outcome ~ treat + mediator +", X_vars_str)

  f_med <- as.formula(f_med_txt)
  f_out_inter <- as.formula(f_out_inter_txt)
  f_out_nointer <- as.formula(f_out_nointer_txt)
  
  tryCatch({
    med_model <- glm(f_med, data = data_clean, family = binomial)
    # FIX: Overwrite the call formula so bootstrap doesn't look for 'X_vars'
    med_model$call$formula <- f_med 
    
    if(any(is.na(coef(med_model)))) {
      cat("  ✗ Mediator model has NA coefficients\n")
      return(NULL)
    }
    
    outcome_model_interact <- glm(f_out_inter, data = data_clean, family = binomial)
    
    if(any(is.na(coef(outcome_model_interact)))) {
      cat("  ✗ Outcome model with interaction has NA coefficients\n")
      return(NULL)
    }

    interact_coef <- summary(outcome_model_interact)$coefficients
    if("treat:mediator" %in% rownames(interact_coef)) {
      interact_p <- interact_coef["treat:mediator", "Pr(>|z|)"]
      cat("  Interaction p-value:", sprintf("%.4f", interact_p), "\n")
      use_interaction <- (interact_p < 0.05)
    } else {
      cat("  Interaction term not found, using model without interaction\n")
      use_interaction <- FALSE
    }
    
    # Step 2: Fit final outcome model
    if(use_interaction) {
      cat("  ✓ Using model WITH interaction (p < 0.05)\n")
      outcome_model <- outcome_model_interact
      # FIX: Overwrite call formula
      outcome_model$call$formula <- f_out_inter
    } else {
      cat("  ✓ Using model WITHOUT interaction (p >= 0.05)\n")
      outcome_model <- glm(f_out_nointer, data = data_clean, family = binomial)
      # FIX: Overwrite call formula
      outcome_model$call$formula <- f_out_nointer
      
      if(any(is.na(coef(outcome_model)))) {
        cat("  ✗ Outcome model has NA coefficients\n")
        return(NULL)
      }
    }
    
    set.seed(1005)
    med_results <- mediate(
      model.m = med_model,
      model.y = outcome_model,
      treat = "treat",        
      mediator = "mediator",  
      boot = TRUE,
      sims = n_bootstrap,
      boot.ci.type = "perc"
    )
    
    results_summary <- summary(med_results)
    
    TE_estimate <- results_summary$tau.coef
    TE_ci_lower <- results_summary$tau.ci[1]
    TE_ci_upper <- results_summary$tau.ci[2]
    TE_p <- results_summary$tau.p
    
    NDE_estimate <- results_summary$z.avg
    NDE_ci_lower <- results_summary$z.avg.ci[1]
    NDE_ci_upper <- results_summary$z.avg.ci[2]
    NDE_p <- results_summary$z.avg.p
    
    NIE_estimate <- results_summary$d.avg
    NIE_ci_lower <- results_summary$d.avg.ci[1]
    NIE_ci_upper <- results_summary$d.avg.ci[2]
    NIE_p <- results_summary$d.avg.p

    prop_mediated <- results_summary$n.avg * 100
    prop_ci_lower <- results_summary$n.avg.ci[1] * 100
    prop_ci_upper <- results_summary$n.avg.ci[2] * 100
    prop_p <- results_summary$n.avg.p
    
    return(list(
      mediator = mediator_name,
      comparison = exposure_label,
      sample_size = nrow(data_clean),
      n_events = sum(data_clean$dementia_outcome == 1),
      interaction_used = use_interaction,
      
      TE_estimate = TE_estimate,
      TE_ci_lower = TE_ci_lower,
      TE_ci_upper = TE_ci_upper,
      TE_p = TE_p,
      
      NDE_estimate = NDE_estimate,
      NDE_ci_lower = NDE_ci_lower,
      NDE_ci_upper = NDE_ci_upper,
      NDE_p = NDE_p,
      
      NIE_estimate = NIE_estimate,
      NIE_ci_lower = NIE_ci_lower,
      NIE_ci_upper = NIE_ci_upper,
      NIE_p = NIE_p,
      
      prop_mediated = prop_mediated,
      prop_ci_lower = prop_ci_lower,
      prop_ci_upper = prop_ci_upper,
      prop_p = prop_p
    ))
    
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
    return(NULL)
  })
}

exposure_levels <- list(
  list(level = "1", label = "1 time vs Never"),
  list(level = "2", label = "2 times vs Never"),
  list(level = ">=3", label = "≥3 times vs Never")
)

mediators <- list(
  list(var = "depression_10",    name = "Depression"),
  list(var = "loneliness_10",    name = "Loneliness")
)

all_results <- list()
for (med in mediators) {
  for (exp in exposure_levels) {
    res <- run_mediation_analysis(
      data = df_main,
      mediator_var = med$var,
      mediator_name = med$name,
      exposure_level = exp$level,
      exposure_label = exp$label,
      covariates = covariates,
      n_bootstrap = 50
    )
    if (!is.null(res)) {
      all_results[[paste(med$name, exp$label, sep = " | ")]] <- res
    }
  }
}

if(length(all_results) == 0) {
  cat("\n✗ No successful analyses to report!\n")
  cat("Please check the error messages above for debugging.\n")
} else {
  cat("\n", strrep("=", 100), "\n", sep = "")
  cat("STEP 8: FINAL RESULTS TABLE\n")
  cat(strrep("=", 100), "\n", sep = "")
  
  create_final_table <- function(res_list) {
    do.call(rbind, lapply(res_list, function(r) {
      data.frame(
        Mediator      = r$mediator,
        Comparison    = r$comparison,
        N             = r$sample_size,
        Events        = r$n_events,
        Interaction   = ifelse(r$interaction_used, "Yes", "No"),
        
        TE_Est        = sprintf("%.4f", r$TE_estimate),
        TE_CI         = sprintf("[%.4f, %.4f]", r$TE_ci_lower, r$TE_ci_upper),
        TE_P          = sprintf("%.4f", r$TE_p),
        
        NDE_Est       = sprintf("%.4f", r$NDE_estimate),
        NDE_CI        = sprintf("[%.4f, %.4f]", r$NDE_ci_lower, r$NDE_ci_upper),
        NDE_P         = sprintf("%.4f", r$NDE_p),
        
        NIE_Est       = sprintf("%.4f", r$NIE_estimate),
        NIE_CI        = sprintf("[%.4f, %.4f]", r$NIE_ci_lower, r$NIE_ci_upper),
        NIE_P         = sprintf("%.4f", r$NIE_p),
        
        PM_Percent    = sprintf("%.2f%%", r$prop_mediated),
        PM_CI         = sprintf("[%.2f%%, %.2f%%]", r$prop_ci_lower, r$prop_ci_upper),
        PM_P          = sprintf("%.4f", r$prop_p),
        
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
  } 
}

