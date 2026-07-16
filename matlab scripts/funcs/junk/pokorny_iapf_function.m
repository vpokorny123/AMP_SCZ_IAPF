function [pSum, pChans, f] = iapf_function(subID, data_dir, ...
                                                   eeglab_dir, event_code, ...
                                                   elecs_to_select)
%import
EEG = pop_loadbv(data_dir, [subID,'.vhdr']);
EEG=pop_chanedit(EEG, 'lookup',[eeglab_dir, '/plugins/dipfit/standard_BEM/elec/standard_1005.elc']);

%as best as I can tell S20 = eyes_open and S24 = eyes_closed
%so let's try to subset to only the resting task
EEG = pop_rmdat( EEG, {event_code},[-1 1],0);

%keeping this line in for debugging purposes but we will get rid of this
%before sending out because the goal is to run this all at once


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

% filter data
EEG = pop_eegfiltnew(EEG, 'locutoff',0.5);
%EEG = pop_select( EEG, 'nochannel',removeChans); % list here channels to ignore
chanlocs = EEG.chanlocs;

% remove bad channels 
EEG = pop_clean_rawdata(EEG, ...
    'FlatlineCriterion',5,'ChannelCriterion',0.85, ...
    'LineNoiseCriterion',4,'Highpass','off', ...
    'BurstCriterion','off','WindowCriterion','off', ...
    'BurstRejection','off','Distance','Euclidian');
%this is the code to also remove bad time segments, but I want to avoid
%this to try to avoid discontinuities in the four second epochs
%EEG = pop_clean_rawdata(EEG, ...
%    'FlatlineCriterion',4,'ChannelCriterion',0.85, ...
%    'LineNoiseCriterion',4,'Highpass','off',...
%    'BurstCriterion',20,'WindowCriterion',0.25, ..Ei 
%    'BurstRejection','on','Distance','Euclidian', ...
%    'WindowCriterionTolerances',[-Inf 7] );

% Run ICA and IC Label
EEG = pop_runica(EEG, 'icatype', 'picard', 'maxiter', 500); % Use mode standard for Infomax
EEG = pop_iclabel(EEG, 'default');
EEG = pop_icflag(EEG, [NaN NaN;NaN NaN;0.9 1;NaN NaN;NaN NaN;NaN NaN;NaN NaN]);
EEG = pop_subcomp( EEG, [], 0);

% Interpolate removed channels 
%EEG = pop_interp(EEG, chanlocs);

%select chans of interest if not set to 'all'
if ~strcmp(elecs_to_select,'all')
    EEG = pop_select( EEG, 'channel',elecs_to_select);
end
%now do PAF
[pSum, pChans, f]= restingIAF(EEG.data,EEG.nbchan,3,[1,20],EEG.srate,[7,13],11,5);

%save for now
%save_dir = '/Users/victorpokorny/Desktop/Pronet EEG Pokorny/Imported_Files';
%EEG = pop_saveset( EEG, 'filename',['imported',subID,'.set'], ...
%     'filepath',save_dir);

end