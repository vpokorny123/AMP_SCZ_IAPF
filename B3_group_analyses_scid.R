library(data.table)
library(tidyr)
source("~/Desktop/R_functions/funcs.R")
load('../../RData/merged_df_alternate_psychs.RData')


#convert to long
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

pub_ready_stats(anova(lmer(paf ~ site + (1|src_subject_id),
                     data= merged_df_long)))

#w/o covariates but w/ interactions
pub_ready_stats(lmer(paf ~ phenotype * eyes_condition +
                       (1|site/src_subject_id),  data= merged_df_long))

pub_ready_stats(lmer(paf ~ phenotype + eyes_condition +  (1|site/src_subject_id),
                     data= merged_df_long))
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition +  (1|site/src_subject_id),
                     data= merged_df_long,
                     subset = !((phenotype == 'HC' & chrscid_any_depression == 1)|
                                  (phenotype == 'HC' & chrscid_any_subst_use_disorder == 1 ))))
#w/o covariates but w/ interactions
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition +  (1|site/src_subject_id),
             data= merged_df_long,
             subset = !((phenotype == 'HC' & chrscid_any_depression == 1)|
                          (phenotype == 'HC' & chrscid_any_subst_use_disorder == 1 ))))

pub_ready_stats(lmer(paf ~ phenotype + eyes_condition +  (1|site/src_subject_id),
                     data= merged_df_long,
                     subset = !((phenotype == 'HC' & chrscid_any_depression == 1))))

pub_ready_stats(lmer(paf ~ phenotype*chrscid_any_mood + eyes_condition +  (1|site/src_subject_id),
                     data= merged_df_long))

pub_ready_stats(lmer(paf ~ phenotype + eyes_condition +  (1|site/src_subject_id),
                     data= merged_df_long))

pub_ready_stats(lmer(paf ~ chrscid_any_depression + eyes_condition + (1|site/src_subject_id),
                     data= merged_df_long))
pub_ready_stats(lmer(paf ~ chrscid_any_subst_use_disorder + eyes_condition + (1|site/src_subject_id),
                     data= merged_df_long))
pub_ready_stats(lmer(paf ~ chrscid_any_mood + eyes_condition + (1|site/src_subject_id),
                     data= merged_df_long))


#does including covariates matter?
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition + Age + sex +chriq_fsiq +
                       (1|src_subject_id),
                     data= merged_df_long))

# bayes factor for each condition
library(BayesFactor)
# Extract groups
x <- merged_df$paf_eyes_open_bl[merged_df$phenotype == "CHR"]
y <- merged_df$paf_eyes_open_bl[merged_df$phenotype == "HC"]

# Remove NAs/Infs from each
x <- x[is.finite(x)]
y <- y[is.finite(y)]

# Run Bayesian t-test
bf_two <- ttestBF(x = x, y = y, paired = FALSE)
bf_two

x <- merged_df$paf_eyes_closed_bl[merged_df$phenotype == "CHR"]
y <- merged_df$paf_eyes_closed_bl[merged_df$phenotype == "HC"]

# Remove NAs/Infs from each
x <- x[is.finite(x)]
y <- y[is.finite(y)]

# Run Bayesian t-test
bf_two <- ttestBF(x = x, y = y, paired = FALSE)
bf_two

#now look at higher symptom CHR
cut_score = 5
# get pre filtering count
table(merged_df$phenotype)
merged_df_cut<-merged_df[(merged_df$phenotype == 'HC' | 
                        merged_df$psychs_sips_p1>=cut_score | 
                        merged_df$psychs_sips_p2>=cut_score | 
                        merged_df$psychs_sips_p3>=cut_score | 
                        merged_df$psychs_sips_p4>=cut_score |
                        merged_df$psychs_sips_p5>=cut_score),]

# enter all at once
merged_df_long_cut <- merged_df_cut %>%
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
table(merged_df_cut$phenotype)
pub_ready_stats(lmer(paf ~ phenotype*eyes_condition + chriq_fsiq + 
                       (1 | site/src_subject_id), data = merged_df_long_cut, REML = FALSE))
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition + chriq_fsiq + 
                       (1 | site/src_subject_id), data = merged_df_long_cut, REML = FALSE))

#does including covariates matter?
pub_ready_stats(lmer(paf ~ phenotype + eyes_condition + Age + sex + chrdemo_education +
                       (1|src_subject_id),
                     data= merged_df_long_cut))




