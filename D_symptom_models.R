library(data.table)
library(tidyr)
library(ggcorrplot)
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

merged_df_long <- merged_df_long[complete.cases(merged_df_long[, vars_needed]), ]
mod_a<-lmer(paf ~ sips_pos_tot * eyes_condition + Age + race + chriq_fsiq +
                       (1|site/src_subject_id),
                     data= merged_df_long)

#w/o interaction
mod_b<-lmer(paf ~ sips_pos_tot + eyes_condition + Age + race + chriq_fsiq +
              (1|site/src_subject_id),
            data= merged_df_long)
pub_ready_stats(anova(mod_a,mod_b))
pub_ready_stats(mod_b)

plot_partial_regression_lmer(mod_b)

#all 5 positive symptoms separately
# does adding interaction term improve fit
model_a <- lmer(paf ~ (psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                         psychs_sips_p4 + psychs_sips_p5)*eyes_condition + chriq_fsiq +
                  Age + race + 
                  (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)

model_b <- lmer(paf ~ (psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                         psychs_sips_p4 + psychs_sips_p5) + eyes_condition + chriq_fsiq +
                  Age + race + 
                  (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
car::vif(model_b)
pub_ready_stats(anova(model_b, model_a))

#how much does adding positive symptoms improve prediction

mod_a<-lmer(paf ~ eyes_condition + Age + race + chriq_fsiq +
            (1 | site/src_subject_id), data = merged_df_long, REML = TRUE)
mod_b<-lmer(paf ~ (psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                   psychs_sips_p4 + psychs_sips_p5) + eyes_condition + 
              chriq_fsiq + Age + race +
            (1 | site/src_subject_id), data = merged_df_long, REML = TRUE)
pub_ready_stats(anova(mod_a,mod_b))
plot_partial_regression_lmer(res)

pub_ready_reg_table(mod_b, save_file = '../../csvs/table_2_raw_alt.csv')


# now interpret main effects
res<-lmer(paf ~ (psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                   psychs_sips_p4 + psychs_sips_p5) + eyes_condition + Age + race +
            (1 | site/src_subject_id), data = merged_df_long, REML = TRUE)
pub_ready_stats(res)
plot_partial_regression_lmer(res, save_dir = "../../svgs")

contrast <- multcomp::glht(res, linfct = c("psychs_sips_p4  - psychs_sips_p5 = 0"))
summary(contrast)



R <- cor(sips, use = "pairwise.complete.obs")
P <- cor_pmat(sips)
P[] <- p.adjust(P, method = "BH") 

nice <- c(psychs_sips_p1 = "P1 Unusual Thought Content",
          psychs_sips_p2 = "P2 Suspiciousness",
          psychs_sips_p3 = "P3 Grandiosity",
          psychs_sips_p4 = "P4 Perceptual Abnormalities",
          psychs_sips_p5 = "P5 Disorganized Communication",
          paf            = "Peak Alpha Frequency")
dimnames(R) <- dimnames(P) <- list(nice[rownames(R)], nice[colnames(R)])

stars <- ifelse(P < .001, "***", ifelse(P < .01, "**", ifelse(P < .05, "*", "")))
lab <- matrix(paste0(sprintf("%.2f", R), stars), nrow(R), dimnames = dimnames(R))
lab[upper.tri(lab, diag = TRUE)] <- NA

ggcorrplot(R, type = "lower") +
  ggplot2::geom_text(data = reshape2::melt(lab, na.rm = TRUE),
                     ggplot2::aes(Var1, Var2, label = value), inherit.aes = FALSE, size = 3) +
  ggplot2::theme(panel.grid = ggplot2::element_blank())


#negative symptoms alone
#first test interactions
model_a<-lmer(paf ~  (chrnsiprx_anhedonia_domain.x + 
                       chrnsipr_asociality_domain.x + chrnsipr_avolition_domain.x + 
                       chrnsipr_blunted_affect_domain.x)*
                eyes_condition + Age + race + (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
model_b<-lmer(paf ~  (chrnsipr_anhedonia_domain.x + 
                        chrnsipr_asociality_domain.x + chrnsipr_avolition_domain.x + 
                        chrnsipr_blunted_affect_domain.x) +
                eyes_condition + (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(anova(model_b, model_a))
pub_ready_reg_table(model_b, save_file = '../../csvs/neg_symp_table.csv')


#omnibus test 
model_a<-lmer(paf ~  eyes_condition + Age + race + (1 | site/src_subject_id), 
              data = merged_df_long, REML = FALSE)
model_b<-lmer(paf ~  (chrnsipr_anhedonia_domain.x + 
                        chrnsipr_asociality_domain.x + chrnsipr_avolition_domain.x + 
                        chrnsipr_blunted_affect_domain.x) +
                eyes_condition + (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(anova(model_b, model_a))


#does adding negative to positive explain variance
model_a<-lmer(paf ~ psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                       psychs_sips_p4 + psychs_sips_p5 + eyes_condition + Age+ race + 
                        chriq_fsiq +
                       (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
model_b<-lmer(paf ~ psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
                              psychs_sips_p4 + psychs_sips_p5 + chrnsipr_anhedonia_domain.x + 
                       chrnsipr_asociality_domain.x + chrnsipr_avolition_domain.x + 
                       chrnsipr_blunted_affect_domain.x + Age+ race + chriq_fsiq +
                       eyes_condition + (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(anova(model_b, model_a))



model_b<-lmer(paf ~ psychs_sips_p1 + psychs_sips_p2 + psychs_sips_p3 +
       psychs_sips_p4 + psychs_sips_p5 + chrnsipr_anhedonia_domain.x + 
       chrnsipr_asociality_domain.x + chrnsipr_avolition_domain.x + 
       chrnsipr_blunted_affect_domain.x + Age + race + chriq_fsiq +
       eyes_condition + (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(model_b)


#look at just FSIQ

model_b<-lmer(paf ~ Age + race + chriq_fsiq +
                eyes_condition + (1 | site/src_subject_id), data = merged_df_long, REML = FALSE)
pub_ready_stats(model_b)


