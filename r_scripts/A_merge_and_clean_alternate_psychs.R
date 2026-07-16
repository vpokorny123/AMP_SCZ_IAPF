library(data.table)
library(tidyr)
library(dplyr)

source("~/Desktop/R_functions/funcs.R")
IAPF_df <- fread('../../csvs/data_for_R.csv')
nda_df <- fread('../../csvs/nda.csv')
scid_df<- fread('../../csvs/scid_4.csv')
psychs_df_bl <- fread('../../csvs/psychs_bl.csv')
psychs_df_scr <- fread('../../csvs/psychs_scr.csv')
nsipr_df <- fread('../../csvs/nsipr_bl.csv')
wais_df <- fread('../../csvs/wais_bl.csv')
socdemo_df <- fread('../../csvs/socdemo.csv')
eeg_dates_bl_1 <- fread('../../csvs/eeg_features_baseline.csv')
eeg_dates_bl_2 <- fread('../../csvs/eeg_features_baseline_part2.csv')
eeg_dates_bl_all  <- rbind(eeg_dates_bl_1, eeg_dates_bl_2)
eeg_dates_bl_all$visit  <- rep('bl',nrow(eeg_dates_bl_all))
eeg_dates_fu_1 <- fread('../../csvs/eeg_features_2m.csv')
eeg_dates_fu_2 <- fread('../../csvs/eeg_features_2m_part2.csv')
eeg_dates_fu_all  <- rbind(eeg_dates_fu_1, eeg_dates_fu_2)
eeg_dates_fu_all$visit  <- rep('fu',nrow(eeg_dates_fu_all))
eeg_dates_all<- rbind(rbind(eeg_dates_bl_all, eeg_dates_fu_all))
eeg_dates_wide_df <- eeg_dates_all%>% pivot_wider(id_cols = 'src_subject_id',names_from = 'visit', values_from = 'interview_date')
eeg_dates_wide_df <- eeg_dates_wide_df |>
  mutate(
    bl = as.Date(bl, format = "%m/%d/%Y"),
    fu = as.Date(fu, format = "%m/%d/%Y"),
    days_diff = as.numeric(fu - bl)
  )

#first figure out psychs_df_bl psychs_df_scr nightmare
#basic idea is we want to include baseline values if they have them, but then 
#if baseline is missing, we can supplement with screening values
#danielle says a person only got baseline values if their baseline visit was more
#than 21 days after screening visit

cols_to_check <- c("psychs_pos_tot", "psychs_sips_p1", "psychs_sips_p2", "psychs_sips_p3",
                   "psychs_sips_p4", "psychs_sips_p5", "sips_pos_tot", "psychs_caarms_p1",
                   "psychs_caarms_p2", "psychs_caarms_p3", "psychs_caarms_p4", "caarms_pos_tot")
#first need to drop folks with no values
psychs_df_bl[psychs_df_bl == -900 | psychs_df_bl == -300] <- NA
psychs_df_scr[psychs_df_scr == -900 | psychs_df_scr == -300] <- NA
psychs_df_bl <- psychs_df_bl %>% dplyr::filter(!if_all(all_of(cols_to_check), is.na))
psychs_df_scr <- psychs_df_scr %>% dplyr::filter(!if_all(all_of(cols_to_check), is.na))

psychs_df <- bind_rows(
  psychs_df_bl,
  psychs_df_scr %>% dplyr::filter(!src_subject_id %in% psychs_df_bl$src_subject_id)
)


# Define the variable suffixes you want to combine
suffixes <- paste0(1:14, "d1")
suffixes <- suffixes[suffixes != "2d1"]

for (suf in suffixes) {
  bl_var  <- paste0("chrpsychs_bl_", suf)
  scr_var <- paste0("chrpsychs_scr_", suf)
  new_var <- paste0("chrpsychs_", suf)
  
  psychs_df[[new_var]] <- coalesce(psychs_df[[bl_var]], psychs_df[[scr_var]])
}


#before merging let's clean up data_for_R
#create visit variable
IAPF_df$visit<-ifelse(grepl('visit1',IAPF_df$subIDs),'bl','fu')
IAPF_df$src_subject_id<- substr(IAPF_df$subIDs, 1, 7)
IAPF_df$condition <- ifelse(grepl('eyes_open',IAPF_df$subIDs),'eyes_open','eyes_closed')
merged_df<-base::merge(IAPF_df, nda_df, by = 'src_subject_id',all.x = TRUE)

#
merged_df %>% group_by(phenotype, visit,condition) %>% 
  summarize(n = n())

#next let's drop folks with really bad preprocessing scores (it's easier to 
#do this before pivoting wide) 
#let's say less than five usable occipital channels for either condition
main_dvs = c('paf','pafStd','cog','cogStd')
bad_elecs_idx<- IAPF_df$occipital_channels_used <= 5
sum(bad_elecs_idx)
IAPF_df$subIDs[bad_elecs_idx]
IAPF_df<- IAPF_df[!bad_elecs_idx,] 
vjp_hist(IAPF_df$occipital_channels_used)

#drop if more than half of time segments removed
bad_ts_idx<- IAPF_df$percent_segments_removed >= .5
sum(bad_ts_idx)
IAPF_df<- IAPF_df[!bad_ts_idx,] 
vjp_hist(IAPF_df$percent_segments_removed)

#table(IAPF_df$phenotype, IAPF_df$visit)
IAPF_df$usable_bl <- ifelse(!is.na(IAPF_df$paf) & IAPF_df$visit == 'bl', 1, 0)
IAPF_df$usable_bl <- ifelse(!is.na(IAPF_df$paf) & IAPF_df$visit == 'bl', 1, 0)

#pivot wide for merging
IAPF_df_wide <- pivot_wider(IAPF_df,id_cols = c('src_subject_id'),
                            names_from = c('condition','visit'),
                            values_from = -c('src_subject_id','condition','subIDs'))

variables_to_merge <- c('IAPF_df_wide','nda_df','psychs_df','nsipr_df','wais_df',
                        'socdemo_df','eeg_dates_wide_df','scid_df')

dfs_to_merge = NULL
all_dicts = NULL
for (j in seq(variables_to_merge)) {
  name <- variables_to_merge[j]
  df<-base::get(name)
  
  # merge gets mad if some src_subjects are factors and others are numeric
  #df$src_subject_id<-as.numeric(as.character(df$src_subject_id))
  
  #get rid of columns share between dfs because any discrepancies can and will lead
  #to merging problems
  if (name != 'nda_df'){
    df <- df %>% dplyr::select(-any_of(
      c('phenotype','visit','interview_age', 'interview_date', 'sex',
        'SUBMISSION_FOLDER_NAME','ampscz_missing','subjectkey',
        'ampscz_missing_spec')))
  }
  dfs_to_merge[[name]]<-df
  #all_dicts[[name]]<-dict
}
#finally we can merge
merged_df <- Reduce(function(x, y) 
  base::merge(x, y, by = c("src_subject_id"),all.x = TRUE), dfs_to_merge)

# check for discrepant variables
discrepant <- merged_df %>% dplyr::select(ends_with(".x"),ends_with(".y")) 

#count how many subjects we have data for 

merged_df %>% group_by(phenotype, visit,condition) %>% 
  summarize(n = n())
#before merging nsipr drop a bunch of redundant columns
nsipr_df <- nsipr_df %>% dplyr::select(-c(subjectkey, interview_date, interview_age, sex, SUBMISSION_FOLDER_NAME))
merged_df<-base::merge(merged_df, nsipr_df, by = 'src_subject_id', all = TRUE)

#now let's do some cleaning 

#some clinical vars have -900 and -300 for missing data :(
merged_df[merged_df == -900 | merged_df == -300] <- NA

#clean up demo variables
merged_df$Age <- merged_df$interview_age/12
merged_df <- merged_df %>% 
  mutate(Ethnicity = recode(chrdemo_hispanic_latino, `1` = "Hispanic", 
                            `0` = "Non-Hispanic"))

#set hc to reference level
merged_df$phenotype <- relevel(as.factor(merged_df$phenotype), ref = "HC")

#create visit variable
merged_df$site<-as.factor(substr(merged_df$src_subject_id,1,2))

#group counts for different exclusions
table(merged_df$phenotype[merged_df$subIDs %in% IAPF_df$subIDs[bad_elecs_idx]])
table(merged_df$phenotype[merged_df$subIDs %in% IAPF_df$subIDs[bad_ts_idx]])

save(merged_df, file = '../../RData/merged_df_alternate_psychs.RData')

