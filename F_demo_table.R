## Data Cleaning for ORBITZ Data 
source("~/Desktop/R_functions/funcs.R")

#load data
load('../../RData/merged_df_alternate_psychs.RData')

#drop if no EEG data
merged_df <- merged_df[!(is.na(as.numeric(merged_df$paf_eyes_closed_bl)) &
                    is.na(as.numeric(merged_df$paf_eyes_open_bl)) &
                    is.na(as.numeric(merged_df$paf_eyes_open_fu)) &
                    is.na(as.numeric(merged_df$paf_eyes_closed_fu))), ]


#set grouping variable name
grouping_var = 'phenotype'
merged_df$chrscid_any_subst_use_disorder<-factor(
  merged_df$chrscid_any_subst_use_disorder,
  levels = c(0, 1,2,3),
  labels = c("None","Mild","Moderate","Severe")
)

merged_df$chrscid_sedhypanx_use_disorder<-factor(
  merged_df$chrscid_sedhypanx_use_disorder,
  levels = c(0, 1,2,3),
  labels = c("None","Mild","Moderate","Severe")
)



# create demographic variable list: first value is name of the variable, second 
#value is the "pretty" name to be printed, third value sets the variable as nominal
#(TRUE) or not (FALSE) fourth optional allows you to specify median instead of 
#mean (e.g., for income)
demo_vars <- list(
  c('Age', 'Age',FALSE),
  c('sex','Sex',TRUE),
  c('race','Race',TRUE),
  c('Ethnicity','Ethnicity',TRUE),
  c('chrscid_any_subst_use_disorder','Substance Use Disorder', TRUE),
  c('chrscid_sedhypanx_use_disorder','Anxiolytic Use', TRUE),
  c('chrdemo_education','Education (Years)',FALSE),
  c('chriq_fsiq', 'IQ',FALSE),
  c('sips_pos_tot','SIPS Total Positive',FALSE),
  c('chrnsipr_diminished_expression_dimension.x','NSIPR Dim. Expression', FALSE),
  c('chrnsipr_motivation_and_pleasure_dimension.x','NSIPR Motivation & Pleasure', FALSE)
  
  #c('chrdemo_income','Income',TRUE) this is NOT household income but SOURCE of income
  #cannot find household income currently so skipping 
  
)

demo_df <- vjp_build_demographics_table(merged_df, demo_vars, grouping_var)

write.csv(demo_df, '../../csvs/table_raw_test.csv')
