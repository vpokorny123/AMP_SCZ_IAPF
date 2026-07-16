skip_if_already_made = 1;

addpath([main_dir,'matlab_scripts/funcs'])
main_dir = '/projects/b1240/pronet/Pronet_EEG_VJP/';
raw_data_dir = [main_dir,'Raw EEG Files/'];
preproc_save_dir = [main_dir,'preproc/'];

files = dir([raw_data_dir,'*visit*']);
subIDs = {files.name};
conditions = {'eyes_open','eyes_closed'};
error_subjects = {};
electrodes_of_interest = 'occipital';

if strcmp(electrodes_of_interest,'occipital')
   elecs_to_select = {'O1','Oz','O2','PO7','PO3','POz','PO4', 'PO8','P7', ...
       'P5','P3','P1','Pz','P2','P4','P6','P8'};
else
   elecs_to_select = 'all';
end

parfor j = 1:length(subIDs)
    subID = subIDs{j};
    for jj = 1:length(conditions)
        condition = conditions{jj};
        if skip_if_already_made == 1 && ...
            exist([preproc_save_dir,subID,'_',condition,'_preproc.mat'],'file')
            continue
        end
        try          
            preproc_EEG = preproc_function(subID, main_dir, eeglab_dir, elecs_to_select,condition);
            parsave([preproc_save_dir,subID,'_',condition,'_preproc.mat'],preproc_EEG);
        catch
            error_subjects = [error_subjects,[subID,'_',condition]];
        end
    end
end
