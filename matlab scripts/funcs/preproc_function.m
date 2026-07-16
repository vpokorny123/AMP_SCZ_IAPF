function [preproc_EEG] = preproc_function(subID, main_dir, eeglab_dir, elecs_to_select, condition)
%as best as I can tell S 20 = eyes_open and S 24 = eyes_closed
if strcmp(condition, 'eyes_open')
    file_specifier = 'EO';
    event_code = 'S 20';
else
    file_specifier = 'EC';
    event_code = 'S 24';
end

data_dir = [main_dir,'Raw EEG Files/',subID];
subfile = dir([data_dir,'/*',file_specifier,'*.vhdr']);

%skip if there is no resting data
if isempty(subfile)
    disp('no resting data')
    return
end
%import
EEG = pop_loadbv(data_dir, subfile.name);

EEG=pop_chanedit(EEG, 'lookup',[eeglab_dir, '/plugins/dipfit/standard_BEM/elec/standard_1005.elc']);
labels = {EEG.chanlocs.labels}.';



% subset to only the resting task
EEG = pop_rmdat( EEG, {event_code},[-1 1],0);

% VJP added downsample to 250hz
EEG = pop_resample( EEG, 250);


%save out plot of raw power spectra for QC
figure; pop_spectopo(EEG, 1, [0  EEG.pnts], 'EEG' , 'freq', [8 10 12], ...
                     'freqrange',[2 25],'electrodes','on');
sgtitle([subID,'_',condition],'Interpreter','None')
saveas(gcf, [main_dir,'QC/raw_plots/',subID,'_',condition,'.png'])
close(gcf)


% The following is part of the code used to generate the results presented in:
% Delorme A. EEG is better left alone. Sci Rep. 2023 Feb 9;13(1):2372. doi: 10.1038/s41598-023-27528-0. PMID: 36759667; PMCID: PMC9911389.
% https://pubmed.ncbi.nlm.nih.gov/36759667/
%
% This contains the code for the optimal EEGLAB pipeline in the paper above. 
% An example dataset is provided in the data folder.
% Simple plotting for one channel for the two conditions is provided at the end of the script.
%
% Requires to have EEGLAB installed and to install the Picard plugin
% Tested successfuly with EEGLAB 2023.0
%
% Arnaud Delorme, 2022



% filter data %VJP added 40hz high cutoff
EEG = pop_eegfiltnew(EEG, 'locutoff',0.5, 'hicutoff',40);


%EEG = pop_select( EEG, 'nochannel',removeChans); % list here channels to ignore
chanlocs = EEG.chanlocs;

% % remove bad channels 
% EEG = pop_clean_rawdata(EEG, ...
%    'FlatlineCriterion',5,'ChannelCriterion',0.85, ...
%    'LineNoiseCriterion',4,'Highpass','off', ...
%    'BurstCriterion','off','WindowCriterion','off', ...
%    'BurstRejection','off','Distance','Euclidian');


%7/20/25 after much deliberation VJP has decided to turn off BurstCriterion
% and BurstRejection to reduce discontinuities.
%when doing some comparisons the non-timesegment-deleted psds looks
%smoother and cleaner. Our safe guard against enormous bursts will be 
% inspecting psds.
EEG = pop_clean_rawdata(EEG, ...
   'FlatlineCriterion',5,'ChannelCriterion',0.85, ...
   'LineNoiseCriterion',4,'Highpass','off',...
   'BurstCriterion','off','WindowCriterion',0.25, ...
   'BurstRejection','off','Distance','Euclidian', ...
   'WindowCriterionTolerances',[-Inf 9] );

if ~isfield(EEG.etc,'clean_channel_mask')
    EEG.channels_removed = 0;
    EEG.channels_removed_names = {' '};
    EEG.channels_removed_names_nice = {' '};
else
    EEG.channels_removed = sum(~EEG.etc.clean_channel_mask);
    EEG.channels_removed_names = labels(~EEG.etc.clean_channel_mask);
    %just making the channels removed names variable a little prettier
    EEG.channels_removed_names_nice = strjoin(string(labels(~EEG.etc.clean_channel_mask)));

end
if ~isfield(EEG.etc,'clean_sample_mask')
    EEG.percent_segments_removed = 0;
else
    EEG.percent_segments_removed = sum(~EEG.etc.clean_sample_mask)/ ... 
                               length(EEG.etc.clean_sample_mask);
end


% Run ICA and IC Label
EEG = pop_runica(EEG, 'icatype', 'picard', 'maxiter', 500); % Use mode standard for Infomax
EEG = pop_iclabel(EEG, 'default');
EEG = pop_icflag(EEG, [NaN NaN;NaN NaN;0.9 1;NaN NaN;NaN NaN;NaN NaN;NaN NaN]);

%get number of deleted components
EEG.rejected_ics = sum(EEG.reject.gcompreject);
EEG = pop_subcomp( EEG, [], 0);


% average head made data a lot noisier looking on test subjects, so we
% won't do that
%EEG = pop_reref( EEG, []);

% Interpolate removed channels
%EEG = pop_interp(EEG, chanlocs);


%select chans of interest if not set to 'all'
if ~strcmp(elecs_to_select,'all')
    %figure out which elecs got deleted
    %and only select occipital elecs that haven't been deleted
    elecs_to_select = elecs_to_select(~ismember(elecs_to_select, ...
                      EEG.channels_removed_names));
    EEG.occipital_channels_used = length(elecs_to_select);
    EEG = pop_select( EEG, 'channel',elecs_to_select);
end



%save out plot of raw power spectra for QC
figure; pop_spectopo(EEG, 1, [0  EEG.pnts], 'EEG' , 'freq', [8 10 12], ...
                     'freqrange',[2 25],'electrodes','on');
sgtitle([subID,'_',condition],'Interpreter','None')
saveas(gcf, [main_dir,'QC/preprocessed_plots/',subID,'_',condition,'.png'])
close(gcf)
preproc_EEG = EEG;

end