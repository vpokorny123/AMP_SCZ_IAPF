library(data.table)
library(tidyr)
source("~/Desktop/R_functions/funcs.R")
load('../../RData/merged_df_alternate_psychs.RData')

#now look at higher symptom CHR
cut_score = 5
# get pre filtering count
table(merged_df$phenotype)
merged_df<-merged_df[merged_df$phenotype == 'HC' | 
                        merged_df$psychs_sips_p1>=cut_score | 
                        merged_df$psychs_sips_p2>=cut_score, ]

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

# get post filtering count
table(merged_df$phenotype)
pub_ready_stats(lmer(paf ~ phenotype*eyes_condition +
                       (1 | src_subject_id), data = merged_df_long, REML = FALSE))
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition +
                       (1 | src_subject_id), data = merged_df_long, REML = FALSE))

#does including covariates matter?
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition + Age + sex + chrdemo_education +
                       (1|src_subject_id),
                     data= merged_df_long))




