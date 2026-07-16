skip_if_already_made = 1;

main_dir = '/projects/b1240/pronet/Pronet_EEG_VJP/';
load_dir = [main_dir,'preproc/'];
iapf_save_dir = [main_dir,'iapf/'];
addpath([main_dir,'matlab_scripts/funcs'])
error_subjects = {};

files = dir([load_dir,'*.mat']);
filenames = {files.name};
subIDs = cellfun(@(x) x(1:end-12), filenames, 'UniformOutput', false);


for j = 1:length(subIDs)
    subID = subIDs{j};
%     if skip_if_already_made == 1 && ...
%             exist([iapf_save_dir,subID,'_',condition,'_preproc.mat'],'file')
%         continue
%     end
    try
        load([load_dir,subID,'_preproc.mat'])
        [pSum, pChans, f]= restingIAF(preproc_EEG.data,preproc_EEG.nbchan,3,[1,20], ...
                                      preproc_EEG.srate,[7,13],11,5);
        iapf.pSum = pSum;
        iapf.pChans = pChans;
        iapf.f = f;
        iapf.channels_removed = preproc_EEG.channels_removed;
        iapf.channels_removed_names_nice = preproc_EEG.channels_removed_names_nice;
        iapf.occipital_channels_used = preproc_EEG.occipital_channels_used;
        iapf.percent_segments_removed = preproc_EEG.percent_segments_removed;
        iapf.rejected_ics= preproc_EEG.rejected_ics;
        save([iapf_save_dir,subID,'_iapf.mat'],'iapf');
        disp(['done with ', subID])
    catch
        error_subjects = [error_subjects,[subID,'_',condition]];
    end
end
