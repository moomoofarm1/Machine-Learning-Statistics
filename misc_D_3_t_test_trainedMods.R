set.seed(42)
library(rsample)
library(yardstick)
library(tidyverse)
# library(tibble)
# library(dplyr)
# library(tidyr)
# library(purrr)
library(ggplot2)
library(text)

# data
df_train <- read.csv('2024_cleaned_data/online_train.csv')
df_online_heldout <- read.csv('2024_cleaned_data/online_heldout.csv')
df_offline_heldout <- read.csv('2024_cleaned_data/held-out-data.csv')
online_heldout_churchvsmall <- read.csv('2024_cleaned_data/online_heldout_churchvsmall.csv')
online_heldout_churchvspark <- read.csv('2024_cleaned_data/online_heldout_churchvspark.csv')
online_heldout_mallvspark <- read.csv('2024_cleaned_data/online_heldout_mallvspark.csv')
offline_heldout_churchvsmall <- read.csv('2024_cleaned_data/offline_heldout_churchvsmall.csv')
offline_heldout_churchvspark <- read.csv('2024_cleaned_data/offline_heldout_churchvspark.csv')
offline_heldout_mallvspark <- read.csv('2024_cleaned_data/offline_heldout_mallvspark.csv')
#df_online_heldout$pPlace <- as.factor(df_online_heldout$pPlace)

# panas mods 
panas_mods <- readRDS('2024_models/onlyPANASModels.rds')
panas_onbest <- panas_mods$model_output_indinegpos_prepost
panas_offbest <- panas_mods$model_output_compneg_post

# mods 3 cate online best
emb_onbest <- readRDS('2024_models/D_0_multinomial_output_concatdiff_phrase_mixed_bread_nomollan.rds')
comb_onbest <- readRDS('2024_models/multinomial_best_online.rds')

# mods 3 cate offline best
emb_offbest <- readRDS('2024_models/D_0_multinomial_output_concatdiff_phrase_rob_nomollan.rds')
comb_offbest <- readRDS('2024_models/multinomial_best_offline.rds')

# mods pair-wise online best
onbest_pair_churchvsmall <- readRDS('2024_models/churchvsmall_online_best.rds')
onbest_pair_churchvspark <- readRDS('2024_models/churchvspark_online_best.rds')
onbest_pair_mallvspark <- readRDS('2024_models/mallvspark_online_best.rds')

# mods pair-wise offline best
offbest_pair_churchvsmall <- readRDS('2024_models/churchvsmall_offline_best.rds')
offbest_pair_churchvspark <- readRDS('2024_models/churchvspark_offline_best.rds')
offbest_pair_mallvspark <- readRDS('2024_models/mallvspark_offline_best.rds')


# ---- test ----

t_test_trainedMods <- list()
test_func00000 <- function(prob_model1, prob_model2, truth, n_boot = 10000, seed = 42) {
  
  # prob_model1 is assumed to have columns .pred_ClassA, .pred_ClassB, .pred_ClassC, etc.
  # same for prob_model2
  # 'truth' is a factor with levels c("ClassA","ClassB","ClassC"), matching the predicted columns
  
  df <- bind_cols(
    tibble(truth = as.factor(truth)),
    prob_model1 %>% rename_with(~ paste0(.x, "_m1")),
    prob_model2 %>% rename_with(~ paste0(.x, "_m2"))
  )
  
  # Create bootstrap resamples
  set.seed(seed)
  boot_resamples <- rsample::bootstraps(df, times = n_boot)
  
  # A function to compute AUC for each model using hand_till estimator
  get_macro_auc <- function(split_obj) {
    df_boot <- rsample::assessment(split_obj)
    
    # For model1, use the "hand_till" estimator
    roc1 <- yardstick::roc_auc(
      df_boot,
      truth = truth,
      matches("_m1$"),
      estimator = "macro_weighted" # or "macro_weighted" if needed
    ) %>% mutate(model = "model1")
    
    # Similarly for model2
    roc2 <- yardstick::roc_auc(
      df_boot,
      truth = truth,
      matches("_m2$"),
      estimator = "macro_weighted" # or "macro_weighted" if needed
    ) %>% mutate(model = "model2")
    
    bind_rows(roc1, roc2)
  }
  
  # Compute AUCs
  auc_results <- boot_resamples %>%
    mutate(auc_data = purrr::map(splits, get_macro_auc)) %>%
    tidyr::unnest(auc_data) %>%
    select(id, model, .estimate)
  
  # Pivot wide for comparison
  auc_wide <- auc_results %>%
    tidyr::pivot_wider(names_from = model, values_from = .estimate)
  
  # Compute the differences for each bootstrap sample
  auc_wide <- auc_wide %>% mutate(diff = model1 - model2)
  
  # Plot distribution of differences
  plot <- ggplot(auc_wide, aes(x = diff)) +
    geom_density(alpha = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = "Distribution of AUC differences (model1 - model2)",
      x = "AUC Difference",
      y = "Density"
    )
  
  # Paired tests
  t_test <- t.test(auc_wide$model1, auc_wide$model2, paired = TRUE)
  wilcox_test <- wilcox.test(auc_wide$model1, auc_wide$model2, paired = TRUE)
  proportion_better <- mean(auc_wide$model1 > auc_wide$model2)
  
  # Adjust p-values using FDR method
  raw_pvalues <- c(t_test$p.value, wilcox_test$p.value)
  adjusted_pvalues <- p.adjust(raw_pvalues, method = "fdr")
  names(raw_pvalues) <- c("t_test", "wilcox_test")
  names(adjusted_pvalues) <- c("t_test", "wilcox_test")
  
  # Calculate effect size (Cohen's d for paired differences)
  effect_size <- effsize::cohen.d(auc_wide$model1, auc_wide$model2, mu = 0, paired = TRUE)$estimate#mean(auc_wide$diff) / sd(auc_wide$diff)
  
  list(
    t_test = t_test,
    wilcox_test = wilcox_test,
    raw_pvalues = raw_pvalues,
    adjusted_pvalues = adjusted_pvalues,
    proportion_model1_better = proportion_better,
    t_test_effect_size = effect_size,
    plot = plot
  )
}
test_func <- function(prob_mod1, prob_mod2, truth, n_boot = 2000, seed = 42){
  return (textPredictTest(
    y1 = truth, y2 = NULL, yhat1 = prob_mod1,yhat2=prob_mod2,
    method='bootstrap', statistic = 'auc',
    bootstraps_times = n_boot, seed=seed
  ))
}

# ---- table 2 ----
# TODO: the bug in textPredictTest
'
Error in `dplyr::mutate()`:
ℹ In argument:
  `corr_y2 =
  purrr::map(splits,
  stats_on_bootstrap)`.
Caused by error in `purrr::map()`:
ℹ In index: 1.
Caused by error in `yardstick::roc_auc_vec()` at textPlot-auctest/R/2_4_1_textPredictTextTrained.R:1085:11:
! The number of levels
  in `truth` (375) must match
  the number of columns
  supplied in `...` (2).
'

# table 2 online
table2 <- list()
table2$warrval <- list()
table2$plutchikcate <- list()
table2$online <- list()
#table2$online$panas <- list()
table2$online$emb <- list()
table2$online$comb <- list()
table2$offline <- list()
#table2$offline$panas <- list()
table2$offline$emb <- list()
table2$offline$comb <- list()

# first emb vs panas - removed  comb vs panas
# row 2
table2$warrval$train <- test_func(
  onlyPANASModels$model_output_comp_post$predictions[,4:6],
  warriner_mod_valence_arousal_r_panas$classify3$warr_valence$predictions[,4:6],
  df_train$pPlace
)
table2$warrval$online <-test_func(
  onlyPANASModels$model_output_comp_post$online_held_out_predictions$predictions[,2:4],
  warriner_mod_valence_arousal_r_panas$classify3$warr_valence$online_held_out_predictions[,2:4],
  df_online_heldout$pPlace
)
table2$warrval$offline <-test_func(
  onlyPANASModels$model_output_comp_post$offline_held_out_predictions$predictions[,2:4],
  warriner_mod_valence_arousal_r_panas$classify3$warr_valence$offline_held_out_predictions[,2:4],
  df_offline_heldout$pPlace
)
# row 3
table2$warrval$train <- test_func(
  onlyPANASModels$model_output_comp_post$predictions[,4:6],
  warriner_mod_valence_arousal_r_panas$classify3$warr_valence$predictions[,4:6],
  df_train$pPlace
)
table2$warrval$online <-test_func(
  onlyPANASModels$model_output_comp_post$online_held_out_predictions$predictions[,2:4],
  warriner_mod_valence_arousal_r_panas$classify3$warr_valence$online_held_out_predictions[,2:4],
  df_online_heldout$pPlace
)
table2$warrval$offline <-test_func(
  onlyPANASModels$model_output_comp_post$offline_held_out_predictions$predictions[,2:4],
  warriner_mod_valence_arousal_r_panas$classify3$warr_valence$offline_held_out_predictions[,2:4],
  df_offline_heldout$pPlace
)

# online
# row 5
table2$online$emb$train <- test_func(
  panas_onbest$predictions[,4:6],
  emb_onbest$predictions[,4:6],
  df_train$pPlace
)
table2$online$emb$onheld <- test_func(
  panas_onbest$online_held_out_predictions[,2:4], 
  emb_onbest$online_held_out_predictions[,2:4],
  df_online_heldout$pPlace)
table2$online$emb$offheld <- test_func(
  panas_offbest$offline_held_out_predictions[,2:4], 
  emb_offbest$offline_held_out_predictions[,2:4],
  df_offline_heldout$pPlace
)

# row X deleted
# table2$online$comb$train <- test_func(
#   prob_model1 =comb_onbest$predictions[,4:6],
#   prob_model2=emb_onbest$predictions[,4:6],
#   truth=df_train$pPlace
# )
# table2$online$comb$onheld <- test_func(
#   comb_onbest$online_held_out_predictions[,2:4], 
#   emb_onbest$online_held_out_predictions[,2:4],
#   df_online_heldout$pPlace)
# table2$online$comb$offheld <- test_func(
#   comb_offbest$offline_held_out_predictions[,2:4], 
#   emb_offbest$offline_held_out_predictions[,2:4],
#   df_offline_heldout$pPlace
# )

# offline
# row 6
table2$offline$emb$train <- test_func(
  panas_offbest$predictions[,4:6],
  emb_offbest$predictions[,4:6],
  df_train$pPlace
)
table2$offline$emb$onheld <- test_func(
  panas_offbest$online_held_out_predictions[,2:4], 
  emb_offbest$online_held_out_predictions[,2:4],
  df_online_heldout$pPlace)
table2$offline$emb$offheld <- test_func(
  panas_offbest$offline_held_out_predictions[,2:4], 
  emb_offbest$offline_held_out_predictions[,2:4],
  df_offline_heldout$pPlace
)

# row X deleted
# table2$offline$comb$train <- test_func(
#   comb_offbest$predictions[,4:6],
#   emb_offbest$predictions[,4:6],
#   df_train$pPlace
# )
# table2$offline$comb$onheld <- test_func(
#   comb_offbest$online_held_out_predictions[,2:4], 
#   emb_offbest$online_held_out_predictions[,2:4],
#   df_online_heldout$pPlace)
# table2$offline$comb$offheld <- test_func(
#   comb_offbest$offline_held_out_predictions[,2:4], 
#   emb_offbest$offline_held_out_predictions[,2:4],
#   df_offline_heldout$pPlace
# )

t_test_trainedMods$table2 <- table2
#saveRDS(t_test_trainedMods, 't_test_trainedMods.rds')


# ---- table S5 ----
cat4_online_best_onlyPANAS <- readRDS('2024_models/online_all_4category_onbest_onlyPANAS.rds') 
cat4_online_best_onlyEmb <- readRDS('2024_models/online_all_4category_onbest_onlyEmb.rds') 
t_test_trainedMods$class4_onlyEmb <- test_func(
  cat4_online_best_onlyPANAS$predictions[,4:7], 
  cat4_online_best_onlyEmb$predictions[,4:7],
  cat4_online_best_onlyPANAS$predictions$truth
)
saveRDS(t_test_trainedMods, 't_test_trainedMods.rds')


# ----misc ----

# test_func_oskar <- function(yhat1, yhat2, y1){
#   textPredictTest(
#     y1 = y1,
#     y2 = y1,
#     yhat1 = yhat1,
#     yhat2 = yhat2,
#     method = "t-test",
#     statistic = "AUC",
#     paired = TRUE,
#     event_level = "first",
#     bootstraps_times = 1000,
#     seed = 42,
#     y1= regression_only_google$predictions$truth,
#     y2 = regression_only_fb$predictions$truth,
#     yhat1 = as.numeric(regression_only_google$predictions$estimate),
#     yhat2 = as.numeric(regression_only_fb$predictions$estimate),
#     paired = TRUE,
#     bootstraps_times = 10000,
#     method = "bootstrap",
#     statistic = "auc",
#     seed = 42
#   )
# }
