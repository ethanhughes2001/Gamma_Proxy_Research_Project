# ============================================================
# MVE V1 — Public Results Reproduction
# ============================================================
# Reproduces the principal historical sample architecture,
# D-sample predictor standardisation, protected-H predictive
# results and frozen statistical inference used in the public
# MVE V1 findings note.
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGES AND SETTINGS
# ------------------------------------------------------------

library(tidyverse)
library(jsonlite)

development_n <- 300L
holdout_n <- 613L
common_valid_n <- 913L

hac_lag <- 5L
materiality_threshold <- 0.02
alpha <- 0.05

dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "outputs",
  showWarnings = FALSE,
  recursive = TRUE
)



# ------------------------------------------------------------
# 2. LOAD FROZEN ANALYTICAL INPUTS
# ------------------------------------------------------------

predictors <- read_csv(
  "data/M0_MVE_COMMON_VALID_PREDICTOR_MANIFEST_v1.csv",
  show_col_types = FALSE
)

frozen_model <- fromJSON(
  "data/M0_MVE_D_FROZEN_MODEL_PARAMETERS_v1.0.json"
)

ledger <- read_csv(
  "data/M0_MVE_H_DECISION_GRADE_SCORE_LEDGER_v1.csv",
  show_col_types = FALSE
)



# ------------------------------------------------------------
# 3. RECONSTRUCT AND VALIDATE D/H SAMPLE ALLOCATION
# ------------------------------------------------------------
# The first 300 common-valid sessions form the sole development
# sample (D). All later common-valid sessions form protected H.

predictors <- predictors |>
  arrange(common_valid_rank) |>
  mutate(
    sample = if_else(
      common_valid_rank <= development_n,
      "Development",
      "Protected holdout"
    )
  )





stopifnot(
  nrow(predictors) == common_valid_n,
  sum(predictors$sample == "Development") == development_n,
  sum(predictors$sample == "Protected holdout") == holdout_n,
  min(predictors$common_valid_rank) == 1,
  max(predictors$common_valid_rank) == common_valid_n
)



d_last_date <- predictors |>
  filter(sample == "Development") |>
  summarise(last_date = max(session_date)) |>
  pull(last_date)

h_first_date <- predictors |>
  filter(sample == "Protected holdout") |>
  summarise(first_date = min(session_date)) |>
  pull(first_date)

stopifnot(
  d_last_date < h_first_date
)







sample_summary <- predictors |>
  group_by(sample) |>
  summarise(
    observations = n(),
    start_date = min(session_date),
    end_date = max(session_date),
    .groups = "drop"
  ) |>
  mutate(
    sample = factor(
      sample,
      levels = c(
        "Protected holdout",
        "Development"
      )
    )
  )



stopifnot(
  min(
    predictors$session_date[
      predictors$sample == "Development"
    ]
  ) == as.Date("2018-04-04"),
  
  max(
    predictors$session_date[
      predictors$sample == "Development"
    ]
  ) == as.Date("2023-05-04"),
  
  min(
    predictors$session_date[
      predictors$sample == "Protected holdout"
    ]
  ) == as.Date("2023-05-05"),
  
  max(
    predictors$session_date[
      predictors$sample == "Protected holdout"
    ]
  ) == as.Date("2026-07-10")
)





# ------------------------------------------------------------
# 4. RECONSTRUCT D-ONLY PREDICTOR STANDARDISATION
# ------------------------------------------------------------
# Predictor scaling is estimated from D only and then frozen.
# H must not influence the mean or standard deviation.

d_predictors <- predictors |>
  filter(sample == "Development")

xs_mean_d <- mean(d_predictors$x_s)
xs_sd_d <- sd(d_predictors$x_s)

xf_mean_d <- mean(d_predictors$x_f)
xf_sd_d <- sd(d_predictors$x_f)

stopifnot(
  nrow(d_predictors) == development_n,
  is.finite(xs_mean_d),
  is.finite(xs_sd_d),
  is.finite(xf_mean_d),
  is.finite(xf_sd_d),
  xs_sd_d > 0,
  xf_sd_d > 0
)

# ------------------------------------------------------------
# 5. VERIFY SCALING AGAINST FROZEN MODEL PARAMETERS
# ------------------------------------------------------------
# Independently reconstructed D-sample scaling parameters must
# match the prospectively frozen model artifact.

stopifnot(
  frozen_model$standardization_frozen_from_D$ddof == 1,
  
  isTRUE(all.equal(
    xs_mean_d,
    frozen_model$standardization_frozen_from_D$x_s_mean
  )),
  
  isTRUE(all.equal(
    xs_sd_d,
    frozen_model$standardization_frozen_from_D$x_s_sample_sd
  )),
  
  isTRUE(all.equal(
    xf_mean_d,
    frozen_model$standardization_frozen_from_D$x_f_mean
  )),
  
  isTRUE(all.equal(
    xf_sd_d,
    frozen_model$standardization_frozen_from_D$x_f_sample_sd
  ))
)

stopifnot(
  frozen_model$training_sample$n == development_n,
  frozen_model$training_sample$partition == "D",
  frozen_model$training_sample$first_session == "2018-04-04",
  frozen_model$training_sample$last_session == "2023-05-04",
  frozen_model$scientific_role$D == "SOLE_PARAMETER_ESTIMATION_SAMPLE",
  frozen_model$scientific_role$H == "PROTECTED_CONFIRMATORY_SAMPLE_NO_REFIT"
)


# ------------------------------------------------------------
# 6. VALIDATE PROTECTED-H SCORE LEDGER
# ------------------------------------------------------------
# The ledger contains one protected-H record per session,
# including frozen predictions, squared losses and loss
# differentials for all three primary model comparisons.

stopifnot(
  nrow(ledger) == holdout_n,
  all(ledger$partition == "H"),
  min(ledger$common_valid_rank) == development_n + 1L,
  max(ledger$common_valid_rank) == common_valid_n,
  !anyDuplicated(ledger$session_date)
)

# ------------------------------------------------------------
# 7. RECONSTRUCT PROTECTED-H MSE RESULTS
# ------------------------------------------------------------
# Positive relative MSE gain means the candidate specification
# produced lower average squared prediction error than its
# relevant comparator on protected H.

m0s_reference_mse <- mean(ledger$m0s_reference_loss)
m0s_candidate_mse <- mean(ledger$m0s_model_loss)

m0f_reference_mse <- mean(ledger$m0f_reference_loss)
m0f_candidate_mse <- mean(ledger$m0f_model_loss)

m0sf_restricted_mse <- mean(ledger$sf_restricted_loss)
m0sf_augmented_mse <- mean(ledger$sf_augmented_loss)

m0s_gain <- (
  m0s_reference_mse - m0s_candidate_mse
) / m0s_reference_mse

m0f_gain <- (
  m0f_reference_mse - m0f_candidate_mse
) / m0f_reference_mse

m0sf_gain <- (
  m0sf_restricted_mse - m0sf_augmented_mse
) / m0sf_restricted_mse

stopifnot(
  isTRUE(all.equal(
    mean(ledger$d_m0s),
    m0s_reference_mse - m0s_candidate_mse
  )),
  
  isTRUE(all.equal(
    mean(ledger$d_m0f),
    m0f_reference_mse - m0f_candidate_mse
  )),
  
  isTRUE(all.equal(
    mean(ledger$d_m0sf),
    m0sf_restricted_mse - m0sf_augmented_mse
  ))
)

stopifnot(
  isTRUE(all.equal(
    m0s_gain,
    0.006693715122777746
  )),
  
  isTRUE(all.equal(
    m0f_gain,
    0.002225165526325804
  )),
  
  isTRUE(all.equal(
    m0sf_gain,
    0.002730621192840469
  ))
)



# ------------------------------------------------------------
# 8. RECONSTRUCT NEWEY-WEST / BARTLETT HAC INFERENCE
# ------------------------------------------------------------
# Tests whether the mean session-level loss differential is > 0.
# HAC lag is frozen at 5 sessions.

hac_mean_test <- function(d, lag = 5L) {
  
  n <- length(d)
  d_bar <- mean(d)
  centred <- d - d_bar
  
  gamma0 <- sum(centred^2) / n
  long_run_variance <- gamma0
  
  for (k in seq_len(lag)) {
    
    gamma_k <- sum(
      centred[(k + 1):n] *
        centred[1:(n - k)]
    ) / n
    
    weight <- 1 - k / (lag + 1)
    
    long_run_variance <- long_run_variance +
      2 * weight * gamma_k
  }
  
  se_mean <- sqrt(long_run_variance / n)
  z <- d_bar / se_mean
  p_one_sided <- 1 - pnorm(z)
  
  tibble(
    n = n,
    mean_d = d_bar,
    hac_se = se_mean,
    z = z,
    p_raw = p_one_sided
  )
}


m0s_test <- hac_mean_test(
  ledger$d_m0s,
  lag = hac_lag
)

m0f_test <- hac_mean_test(
  ledger$d_m0f,
  lag = hac_lag
)

m0sf_test <- hac_mean_test(
  ledger$d_m0sf,
  lag = hac_lag
)

stopifnot(
  isTRUE(all.equal(
    m0s_test$hac_se,
    0.00012585651317911176
  )),
  isTRUE(all.equal(
    m0s_test$p_raw,
    0.08363368504842839
  )),
  
  isTRUE(all.equal(
    m0f_test$hac_se,
    0.00015587777005023117
  )),
  isTRUE(all.equal(
    m0f_test$p_raw,
    0.33741125602667010
  )),
  
  isTRUE(all.equal(
    m0sf_test$hac_se,
    0.00015707396620767261
  )),
  isTRUE(all.equal(
    m0sf_test$p_raw,
    0.30446527979676341
  ))
)

# ------------------------------------------------------------
# 9. APPLY HOLM MULTIPLICITY ADJUSTMENT
# ------------------------------------------------------------
# Holm adjustment controls the familywise error rate across
# the three primary hypotheses.

raw_p_values <- c(
  "Structural positioning" = m0s_test$p_raw,
  "0DTE flow" = m0f_test$p_raw,
  "Structural + flow" = m0sf_test$p_raw
)

holm_p_values <- p.adjust(
  raw_p_values,
  method = "holm"
)


stopifnot(
  isTRUE(all.equal(
    unname(holm_p_values["Structural positioning"]),
    0.2509010551452852
  )),
  
  isTRUE(all.equal(
    unname(holm_p_values["0DTE flow"]),
    0.6089305595935268
  )),
  
  isTRUE(all.equal(
    unname(holm_p_values["Structural + flow"]),
    0.6089305595935268
  ))
)


stopifnot(
  all(holm_p_values > alpha),
  m0s_gain < materiality_threshold,
  m0f_gain < materiality_threshold,
  m0sf_gain < materiality_threshold
)


# ------------------------------------------------------------
# 10. BUILD FINAL RESEARCH-RESULTS TABLE
# ------------------------------------------------------------
# One row per primary hypothesis. This table combines predictive
# performance, uncertainty and the frozen decision criteria.

results <- tibble(
  branch = c(
    "Structural positioning",
    "0DTE flow",
    "Structural + flow"
  ),
  
  comparator = c(
    "Reference model",
    "Reference model",
    "Structural-only restricted model"
  ),
  
  reference_mse = c(
    m0s_reference_mse,
    m0f_reference_mse,
    m0sf_restricted_mse
  ),
  
  
  candidate_mse = c(
    m0s_candidate_mse,
    m0f_candidate_mse,
    m0sf_augmented_mse
  ),
  
  relative_mse_gain = c(
    m0s_gain,
    m0f_gain,
    m0sf_gain
  ),
  
  mean_loss_differential = c(
    m0s_test$mean_d,
    m0f_test$mean_d,
    m0sf_test$mean_d
  ),
  
  hac_se = c(
    m0s_test$hac_se,
    m0f_test$hac_se,
    m0sf_test$hac_se
  ),
  
  z_statistic = c(
    m0s_test$z,
    m0f_test$z,
    m0sf_test$z
  ),
  
  raw_p_value = c(
    m0s_test$p_raw,
    m0f_test$p_raw,
    m0sf_test$p_raw
  ),
  
  holm_adjusted_p = unname(
    holm_p_values[
      c(
        "Structural positioning",
        "0DTE flow",
        "Structural + flow"
      )
    ]
  )
) |>
  mutate(
    mse_gain_pct = relative_mse_gain * 100,
    materiality_screen_met =
      relative_mse_gain >= materiality_threshold,
    statistical_support =
      holm_adjusted_p < alpha
  )

stopifnot(
  nrow(results) == 3,
  
  all(results$mse_gain_pct > 0),
  
  all(!results$materiality_screen_met),
  
  all(!results$statistical_support)
)


paper_results <- results |>
  transmute(
    Test = branch,
    Comparator = comparator,
    `Reference MSE` = round(reference_mse, 6),
    `Candidate MSE` = round(candidate_mse, 6),
    `MSE gain (%)` = round(mse_gain_pct, 3),
    `Raw p-value` = round(raw_p_value, 4),
    `Holm-adjusted p` = round(holm_adjusted_p, 4),
    `2% screen met` = if_else(
      materiality_screen_met,
      "Yes",
      "No"
    ),
    `Statistically supported` = if_else(
      statistical_support,
      "Yes",
      "No"
    )
  )


paper_results


# ------------------------------------------------------------
# 11. CREATE PUBLICATION FIGURES
# ------------------------------------------------------------

# Figure 1 — Chronological D/H research design.
# Bar length represents calendar span, not observation count.
# The longer D calendar interval reflects sparser historical
# structural-options coverage; measurement rules were not
# relaxed to manufacture additional valid sessions.

sample_timeline <- ggplot(
  sample_summary,
  aes(y = sample)
) +
  geom_segment(
    aes(
      x = start_date,
      xend = end_date,
      yend = sample
    ),
    linewidth = 7,
    lineend = "butt"
  ) +
  geom_text(
    aes(
      x = start_date + (end_date - start_date) / 2,
      label = paste0(observations, " sessions")
    ),
    colour = "white",
    fontface = "bold"
  ) +
  labs(
    title = "Chronological research design",
    subtitle = "Development sample followed by protected out-of-sample evaluation",
    x = NULL,
    y = NULL,
    caption = paste(
      "Bar length represents calendar span, not observation count.",
      "The development period spans a longer interval because eligible historical",
      "structural-options coverage was sparser in the vendor data.",
      "Measurement rules were kept unchanged rather than relaxed to increase the sample."
    )
  ) +
  theme_minimal()

sample_timeline

# Figure 2 — Protected-H predictive performance.
# Positive values indicate lower MSE than the relevant comparator.
# The dashed line is the pre-specified 2% descriptive
# materiality screen.

mse_gain_plot <- ggplot(
  results,
  aes(
    x = branch,
    y = mse_gain_pct
  )
) +
  geom_col(
    width = 0.6
  ) +
  geom_hline(
    yintercept = materiality_threshold * 100,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      label = paste0(
        round(mse_gain_pct, 3),
        "%"
      )
    ),
    vjust = -0.5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(0, 2.2),
    breaks = seq(0, 2, 0.5)
  ) +
  labs(
    title = "Protected out-of-sample prediction improvement",
    subtitle = "Relative MSE gain across 613 protected holdout sessions",
    x = NULL,
    y = "Relative MSE improvement (%)",
    caption = "Dashed line = 2% pre-specified descriptive materiality screen"
  ) +
  theme_minimal()

mse_gain_plot


# ------------------------------------------------------------
# 12. EXPORT RESULTS
# ------------------------------------------------------------

ggsave(
  filename = "figures/sample_timeline.png",
  plot = sample_timeline,
  width = 8,
  height = 3,
  dpi = 300
)

ggsave(
  filename = "figures/protected_h_mse_results.png",
  plot = mse_gain_plot,
  width = 8,
  height = 5,
  dpi = 300
)

write_csv(
  results,
  "outputs/mve_v1_full_results.csv"
)

write_csv(
  paper_results,
  "outputs/mve_v1_paper_results.csv"
)

message("MVE V1 reproduction completed successfully.")