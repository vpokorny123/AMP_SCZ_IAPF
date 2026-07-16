library(dplyr)
library(tidyr)
library(lme4)
library(parallel)


permutations       <- 10000
set.seed(5, kind = "L'Ecuyer-CMRG")
converters = 22
nonconverters = 91
percent_converters <- converters / (converters+nonconverters)

percent_converters <- .1
#what is actual cohen's d
compute_cohens_d <- function(m1, m2, sd1, sd2) {
  (m1 - m2) / sqrt((sd1^2 + sd2^2) / 2)
}

#from van tricht 2014
cohens_d<- -compute_cohens_d(m1 = 8.98, m2 = 9.87, sd1 = .98, sd2 = 1.29)
n_cores            <- detectCores() - 1


df_long <- merged_df %>%
  pivot_longer(
    cols          = c(paf_eyes_closed_bl, paf_eyes_open_bl),
    names_to      = "eyes_condition",
    names_pattern = "paf_(eyes_(?:closed|open))_bl",
    values_to     = "paf"
  ) %>%
  mutate(
    eyes_condition = recode(eyes_condition,
                            "eyes_closed" = "closed",
                            "eyes_open"   = "open"
    ),
    paf       = as.numeric(scale(paf)),
    phenotype = factor(phenotype)
  )

df_long$phenotype <- relevel(df_long$phenotype, ref = "HC")

# ----------------------------
# Observed mixed model
# ----------------------------
fit_observed <- lmer(
  paf ~ phenotype + eyes_condition + (1 | src_subject_id),
  data    = df_long,
  REML    = FALSE
  #control = lmerControl(optimizer = "bobyqa")
)

observed_beta          <- fixef(fit_observed)["phenotypeCHR"]

# Variance split from observed model
# Rescaled so total simulated variance = 1
# paf is z-scored so SD = 1, meaning raw mean difference = Cohen's d

vc          <- as.data.frame(VarCorr(fit_observed))
subject_var <- vc$vcov[vc$grp == "src_subject_id"]
row_var     <- sigma(fit_observed)^2
total_var   <- subject_var + row_var
subject_sd  <- sqrt(subject_var / total_var)
row_sd      <- sqrt(row_var / total_var)

# Rescale observed beta to the unit-variance scale used in simulation
observed_beta_rescaled <- observed_beta / sqrt(total_var)

print("Variance partition from observed model:\n")
print("  Subject-level SD:", round(subject_sd, 4), "\n")
print("  Residual SD:     ", round(row_sd,     4), "\n")
print("  Total variance:  ", round(subject_sd^2 + row_sd^2, 4), "\n\n")

print("Observed beta (raw):      ", round(observed_beta,          4), "\n")
print("Observed beta (rescaled): ", round(observed_beta_rescaled, 4), "\n\n")

subj_df <- df_long %>%
  distinct(src_subject_id, phenotype) %>%
  filter(!is.na(phenotype))

chr_n  <- as.integer(nrow(filter(subj_df, phenotype == "CHR")))
hc_n   <- as.integer(nrow(filter(subj_df, phenotype == "HC")))
n_conv <- as.integer(round(chr_n * percent_converters))
n_non  <- as.integer(chr_n - n_conv)

print("Sample sizes:\n")
print("  CHR total:  ", chr_n,  "\n")
print("  HC total:   ", hc_n,   "\n")
print("  Converters: ", n_conv, "\n")
print("  Non-conv:   ", n_non,  "\n\n")

# Index vector built once: CHR rows carry subject index, HC rows get NA
full_subject_idx <- c(
  rep(seq_len(chr_n), each = 2),
  rep(NA_integer_,    hc_n * 2)
)

run_permutation <- function(i) {
  
  # Subject-level means: converters shifted by -cohens_d, rest at 0
  chr_mean <- c(rep(-cohens_d, n_conv), rep(0, n_non))
  hc_mean  <- rep(0, hc_n)
  
  # Subject-level random intercepts
  chr_subject <- chr_mean + rnorm(chr_n, mean = 0, sd = subject_sd)
  hc_subject  <- hc_mean  + rnorm(hc_n,  mean = 0, sd = subject_sd)
  
  # Long dataset: 2 rows per subject (eyes closed + open)
  sim <- data.frame(
    src_subject_id = c(
      rep(paste0("CHR_", seq_len(chr_n)), each = 2),
      rep(paste0("HC_",  seq_len(hc_n)),  each = 2)
    ),
    phenotype = factor(
      c(rep("CHR", chr_n * 2), rep("HC", hc_n * 2)),
      levels = c("HC", "CHR")
    ),
    eyes_condition = factor(
      rep(c("closed", "open"), times = chr_n + hc_n),
      levels = c("open", "closed")
    ),
    paf = c(rep(chr_subject, each = 2), rep(hc_subject, each = 2))
  )
  
  # Add residual noise
  sim$paf <- sim$paf + rnorm(nrow(sim), mean = 0, sd = row_sd)
  
  # Mixed model — mirrors the observed model exactly
  m <- lmer(
    paf ~ phenotype + eyes_condition + (1 | src_subject_id),
    data    = sim,
    REML    = FALSE,
    control = lmerControl(optimizer = "bobyqa")
  )
  
  # Empirical Cohen's d: converters vs HC using one row per subject
  sim_closed     <- sim[sim$eyes_condition == "closed", ]
  idx_closed     <- full_subject_idx[sim$eyes_condition == "closed"]
  is_conv_closed <- !is.na(idx_closed) & (idx_closed <= n_conv)
  is_hc_closed   <- sim_closed$phenotype == "HC"
  
  empirical_d <- mean(sim_closed$paf[is_conv_closed]) - mean(sim_closed$paf[is_hc_closed])
  
  list(
    beta        = fixef(m)["phenotypeCHR"],
    empirical_d = empirical_d,
    singular    = isSingular(m)
  )
}


print("Running", permutations, "permutations across", n_cores, "cores...\n\n")

start_time <- proc.time()

results <- mclapply(
  seq_len(permutations),
  run_permutation,
  mc.cores    = n_cores,
  mc.set.seed = TRUE
)

all_betas       <- sapply(results, `[[`, "beta")
all_empirical_d <- sapply(results, `[[`, "empirical_d")
n_singular      <- sum(sapply(results, `[[`, "singular"))




# Empirical Cohen's d
hist(all_empirical_d, breaks = 200)
abline(v = -cohens_d,             col = "red",    lwd = 2, lty = 2)
abline(v = mean(all_empirical_d), col = "orange", lwd = 2, lty = 1)
legend("topleft",
       legend = c(
         sprintf("Target d = -%.2f",        cohens_d),
         sprintf("Mean empirical d = %.3f", mean(all_empirical_d))
       ),
       col = c("red", "orange"),
       lty = c(2, 1),
       lwd = 2,
       bty = "n"
)


# Permutation betas vs observed (rescaled)
hist(all_betas, breaks = 200)
abline(v = observed_beta_rescaled, col = "red",  lwd = 2, lty = 2)
#abline(v = observed_beta,          col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c(
         sprintf("Obs. beta  = %.3f", observed_beta_rescaled)
         #sprintf("Observed beta (raw)      = %.3f", observed_beta)
       ),
       col = c("red"),
       lty = c(2, 3),
       lwd = 2,
       bty = "n"
)


# Summary stats
print("--- Empirical Cohen's d (converters - HC) ---\n")
print("  Target:  ", -cohens_d,                      "\n")
print("  Mean:    ", round(mean(all_empirical_d), 4), "\n")
print("  SD:      ", round(sd(all_empirical_d),   4), "\n\n")

print("--- Permutation betas ---\n")
print("  Observed beta (raw):      ", round(observed_beta,          4), "\n")
print("  Observed beta (rescaled): ", round(observed_beta_rescaled, 4), "\n")
print("  Mean sim beta:            ", round(mean(all_betas),        4), "\n")
print("  SD sim beta:              ", round(sd(all_betas),          4), "\n")
print("  Right-tail prob (rescaled): ", mean(all_betas >= observed_beta_rescaled), "\n\n")

elapsed <- proc.time() - start_time
print(sprintf("Completed in %.1f seconds (%.1f minutes)\n\n", elapsed["elapsed"], elapsed["elapsed"] / 60))
print("Singular fits:", n_singular, "/", permutations, "\n\n")