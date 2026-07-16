# we def want to show test-retest reliability
#maybe first figure is test-retest reliability and group differences in eyes open and eyes closed
# Usage
load('../../RData/merged_df_alternate_psychs.RData')

merged_df$Group<- merged_df$phenotype
# Base R


# Easier with dplyr
library(dplyr)
merged_df %>%
  group_by(phenotype) %>%
  summarise(eyes_closed_n = sum(!is.na(paf_eyes_closed_bl) & !is.na(paf_eyes_closed_fu)),
            eyes_open_n = sum(!is.na(paf_eyes_open_bl) & !is.na(paf_eyes_open_fu)))

# Or with complete.cases on just those two columns
sum(complete.cases(merged_df[ ,c("baseline", "followup")]))
compute_reliability(merged_df,t1_col = "paf_eyes_open_bl",
                    t2_col = "paf_eyes_open_fu",
                    x_label = "IAPF Time 1",
                    y_label = "IAPF Time 2",
                    title = "Eyes Open Reliability",
                    #covariate = 'days_diff',
                    plot_path = "../../svgs/reliability_eyes_open_days_diff.svg")
compute_reliability(merged_df,t1_col = "paf_eyes_closed_bl",
                    t2_col = "paf_eyes_closed_fu",
                    x_label = "IAPF Time 1",
                    y_label = "IAPF Time 2",
                    title = "Eyes Closed Reliability",
                    #covariate = 'days_diff',
                    plot_path = "../../svgs/reliability_eyes_closed_days_diff.svg")

# Or shortcut:
performance::icc(model)
# Fit mixed model accounting for diff_days
model <- lmer(paf ~ diff_days + (1 | src_subject_id), data = long_df)

# Extract ICC from variance components
var_components <- as.data.frame(VarCorr(model))
var_subject  <- var_components$vcov[1]  # between-person variance
var_residual <- var_components$vcov[2]  # within-person variance

icc <- var_subject / (var_subject + var_residual)
cat("ICC (adjusted for diff_days):", round(icc, 3), "\n")


#let's break them apart by group
merged_df_chr<-merged_df[merged_df$phenotype=='CHR',]
compute_reliability(merged_df_chr,t1_col = "paf_eyes_open_bl",
                    t2_col = "paf_eyes_open_fu",
                    x_label = "IAPF Time 1",
                    y_label = "IAPF Time 2",
                    title = "Eyes Open Reliability",
                    plot_path = "../../svgs/reliability_eyes_open.svg")
compute_reliability(merged_df_chr,t1_col = "paf_eyes_closed_bl",
                    t2_col = "paf_eyes_closed_fu",
                    x_label = "IAPF Time 1",
                    y_label = "IAPF Time 2",
                    title = "Eyes Closed Reliability",
                    plot_path = "../../svgs/reliability_eyes_closed.svg")

vjp_plot_ttest(merged_df, 'paf_eyes_open_bl', 'phenotype',point_alpha = 0.5,
               jitter_width = 0.10, x_label = '', y_label = 'IAPF', 
               title = 'Eyes Open', stats_type = 'bayes', bf_direction = '01',
               plot_save = "../../svgs/group_comparison_eyes_open.svg")
vjp_plot_ttest(merged_df, 'paf_eyes_closed_bl', 'phenotype',point_alpha = 0.5,
               jitter_width = 0.10, x_label = '', y_label = 'IAPF', 
               title = 'Eyes Closed', stats_type = 'bayes', bf_direction = '01',
               plot_save = "../../svgs/group_comparison_eyes_closed.svg")
 



