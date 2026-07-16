library(data.table)
library(tidyr)
source("~/Desktop/R_functions/funcs.R")
load('../../RData/merged_df_alternate_psychs.RData')

merged_df <-merged_df[merged_df$phenotype == 'CHR',]

# enter all at once
merged_df_long <- merged_df %>%
  pivot_longer(
    cols = c(paf_eyes_closed_bl, paf_eyes_open_bl),
    names_to = "eyes_condition",
    names_pattern = "paf_(eyes_(?:closed|open))_bl",
    values_to = "paf"
  ) %>%
  mutate(eyes_condition = recode(eyes_condition,
                                 "eyes_closed" = "closed",
                                 "eyes_open"   = "open"
  ))

vars_needed <- c('psychs_sips_p1', 'psychs_sips_p2', 'psychs_sips_p3', 
                 'psychs_sips_p4', 'psychs_sips_p5', 'chrnsipr_anhedonia_domain.x',  
                 'chrnsipr_asociality_domain.x', 'chrnsipr_avolition_domain.x',  
                 'chrnsipr_blunted_affect_domain.x', 'Age','race', 'chriq_fsiq',
                 'eyes_condition' )

#positive negative and FSIQ symptoms separately
merged_df_long <- merged_df_long[complete.cases(merged_df_long[, vars_needed]), ]


library(lme4); library(lmerTest); library(ggcorrplot)

sips_vars <- paste0("psychs_sips_p", 1:5)
vars <- c(sips_vars, "paf")
dat  <- as.data.frame(dplyr::mutate(merged_df_long,
                                    src_subject_id = factor(src_subject_id),
                                    site           = factor(site),
                                    eyes_condition = factor(eyes_condition)))

subj <- dat |>
  dplyr::group_by(src_subject_id) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(sips_vars), \(x) x[1]), .groups = "drop")

n <- length(vars)
R <- diag(n); P <- matrix(1, n, n); dimnames(R) <- dimnames(P) <- list(vars, vars)

# SIPS x SIPS: between-subject Pearson (one rating per subject)
for (i in 1:4) for (j in (i + 1):5) {
  ct <- cor.test(subj[[sips_vars[i]]], subj[[sips_vars[j]]])
  R[i, j] <- R[j, i] <- ct$estimate; P[i, j] <- P[j, i] <- ct$p.value
}

# PAF x SIPS: PAF repeated across conditions -> mixed model.
# cond fixed effect absorbs eyes-open/closed mean shift; random intercepts subject-in-site.
for (i in 1:5) {
  d <- data.frame(id = dat$src_subject_id, site = dat$site, cond = dat$eyes_condition,
                  sips = scale(dat[[sips_vars[i]]])[, 1], paf = scale(dat$paf)[, 1])
  s <- summary(lmerTest::lmer(paf ~ sips + cond + (1 | site/id), d))$coefficients["sips", ]
  R[i, 6] <- R[6, i] <- s[["Estimate"]]; P[i, 6] <- P[6, i] <- s[["Pr(>|t|)"]]
}

lt <- lower.tri(P)
P[lt] <- p.adjust(P[lt], method = "none")

nice <- c(psychs_sips_p1 = "P1 Unusual Thought Content",
          psychs_sips_p2 = "P2 Suspiciousness",
          psychs_sips_p3 = "P3 Grandiosity",
          psychs_sips_p4 = "P4 Perceptual Abnormalities",
          psychs_sips_p5 = "P5 Disorganized Communication",
          paf            = "IAPF")
dimnames(R) <- list(nice[rownames(R)], nice[colnames(R)]); dimnames(P) <- dimnames(R)

stars <- ifelse(P < .001, "***", ifelse(P < .01, "**", ifelse(P < .05, "*", "")))
lab <- matrix(paste0(sprintf("%.2f", R), stars), n, dimnames = dimnames(R))
lab[upper.tri(lab, diag = TRUE)] <- NA
lab_df <- reshape2::melt(lab, na.rm = TRUE)

# lower-triangle PAF cells = the mixed-model ones (PAF is the last variable, so its row)
paf_name <- nice["paf"]
box_df <- lab_df[lab_df$Var2 == paf_name | lab_df$Var1 == paf_name, ]

p <- ggcorrplot(R, type = "lower") +
  ggplot2::geom_tile(data = box_df, ggplot2::aes(Var1, Var2),
                     fill = NA, color = "grey20", linewidth = 0.6, inherit.aes = FALSE) +
  ggplot2::geom_text(data = lab_df, ggplot2::aes(Var1, Var2, label = value),
                     inherit.aes = FALSE, size = 3) +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0)) +
  ggplot2::labs(
    subtitle = "Unboxed cells: Pearson correlation coefficient\nBoxed cells: Standardized regression coefficient (linear mixed effects)")

p
ggplot2::ggsave("../../svgs/iapf_sips.svg", p, width = 8, height = 6.5)
p
