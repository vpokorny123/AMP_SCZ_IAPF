close all
iapf_dir = '/projects/b1240/pronet/Pronet_EEG_VJP/iapf/';
save_dir = '/projects/b1240/pronet/Pronet_EEG_VJP/csvs/';
%have to redo subIDs to only select subIDs that made it through
%preprocessing
files = dir([iapf_dir,'*.mat']);
subIDs = {files.name};
subIDs_short = cellfun(@(x) x(1:end-9), names, 'UniformOutput', false);
data_for_R = subIDs_short';
iapf_vars_to_get = {'paf','pafStd','cog','cogStd'};
preproc_vars_to_get = {'channels_removed','occipital_channels_used', ...
                        'percent_segments_removed','rejected_ics'};
vars_to_get = [iapf_vars_to_get,preproc_vars_to_get];

for j = 1:length(subIDs)
    subID = subIDs{j};
    load([iapf_dir,subID])
    for jj = 1:length(vars_to_get)
        for jj =1:length(vars_to_get)
            var_to_get = vars_to_get{jj};
            if contains(var_to_get,iapf_vars_to_get)
                data_for_R(j,jj+1) = {iapf.pSum.(var_to_get)};
            elseif contains(var_to_get,preproc_vars_to_get)
                data_for_R(j,jj+1) = {iapf.(var_to_get)};
            end
        end

    end
    disp(['done with ',subID])
end

data_for_R = cell2table(data_for_R);
colnames = [{'subIDs'}, vars_to_get];
data_for_R.Properties.VariableNames = colnames;  % renames columns to A and B
writetable(data_for_R, [save_dir,'data_for_R.csv']);
