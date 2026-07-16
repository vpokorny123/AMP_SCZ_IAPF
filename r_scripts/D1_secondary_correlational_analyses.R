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

#firs just look at cognition
#w interaction
pub_ready_stats(lmer(paf ~ chriq_fsiq* eyes_condition + (1|src_subject_id),
                     data= merged_df_long))

#w/o interaction
pub_ready_stats(lmer(paf ~ chriq_fsiq + eyes_condition + (1|src_subject_id),
                     data= merged_df_long))

#all 5 positive symptoms separately
# does adding interaction term improve fit
model_a <- lmer(paf ~ (chriq_vocab_raw + chriq_matrix_raw)*eyes_condition +
                  (1 | src_subject_id), data = merged_df_long, REML = FALSE)

model_b <- lmer(paf ~ (chriq_vocab_raw + chriq_matrix_raw) + eyes_condition +
                  (1 | src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(anova(model_b, model_a))

# now interpret main effects
pub_ready_stats(lmer(paf ~ chriq_vocab_raw + chriq_matrix_raw + eyes_condition +
       (1 | src_subject_id), data = merged_df_long, REML = FALSE))

# now look at omnibus effect of adding all symptoms in
vars_needed <- c("paf", "chriq_vocab_raw","chriq_matrix_raw", "eyes_condition", "src_subject_id")
df_complete <- merged_df_long[complete.cases(merged_df_long[, vars_needed]), ]
model_a <- lmer(paf ~ (chriq_vocab_raw + chriq_matrix_raw)+eyes_condition +
                  (1 | src_subject_id), data = df_complete, REML = FALSE)

model_b <- lmer(paf ~ eyes_condition +
                  (1 | src_subject_id), data = df_complete, REML = FALSE)
pub_ready_stats(anova(model_b, model_a))

