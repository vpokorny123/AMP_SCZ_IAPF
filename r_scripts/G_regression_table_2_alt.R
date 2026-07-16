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



#all 5 positive symptoms separately
# does adding interaction term improve fit
model_a <- lmer(paf ~ (psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                  psychs_sips_p4 + psychs_sips_p5)*eyes_condition +
                  (1 | src_subject_id), data = merged_df_long, REML = FALSE)

model_b <- lmer(paf ~ (psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                         psychs_sips_p4 + psychs_sips_p5) + eyes_condition +
                  (1 | src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(anova(model_b, model_a))

# now interpret main effects
model_to_print<-lmer(paf ~ psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                   psychs_sips_p4 + psychs_sips_p5 + eyes_condition +
            (1 | site/src_subject_id), data = merged_df_long, REML = TRUE)
summary(model_to_print)
pub_ready_reg_table(model_to_print, save_file = '../../csvs/table_2_raw_alt.csv')


#make foil table of negative symptoms for supplement
pub_ready_stats(lmer(paf ~ chrnsipr_anhedonia_domain.x + chrnsipr_asociality_domain.x +
       chrnsipr_avolition_domain.x + 
       chrnsipr_blunted_affect_domain.x + eyes_condition +
       (1 | site/src_subject_id), data = merged_df_long, REML = FALSE))
model_b<-lmer(paf ~ chrnsipr_anhedonia_domain.x + chrnsipr_asociality_domain.x +
                       chrnsipr_avolition_domain.x + 
                       chrnsipr_blunted_affect_domain.x + eyes_condition +
                       (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_reg_table(model_b, save_file = '../../csvs/supp_table_1_raw.csv')

#make foil table of negative symptoms for supplement
pub_ready_stats(lmer(paf ~ chrnsipr_anhedonia_domain.x + chrnsipr_asociality_domain.x +
                       chrnsipr_avolition_domain.x + 
                       chrnsipr_blunted_affect_domain.x + eyes_condition +
                       (1 | site/src_subject_id), data = merged_df_long, REML = FALSE))
model_b<-lmer(paf ~ psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                psychs_sips_p4 + psychs_sips_p5+ 
                chrnsipr_anhedonia_domain.x + chrnsipr_asociality_domain.x +
                chrnsipr_avolition_domain.x + 
                chrnsipr_blunted_affect_domain.x + chriq_fsiq + eyes_condition +
                (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_reg_table(model_b, save_file = '../../csvs/supp_table_2_raw.csv')


model_b<-lmer(paf ~ sips_pos_tot + 
                chrnsipr_diminished_expression_dimension.y + 
                chrnsipr_motivation_and_pleasure_dimension.x+ chriq_fsiq + eyes_condition +
                (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
summary(model_b)
pub_ready_reg_table(model_b, save_file = '../../csvs/supp_table_2_raw.csv')



