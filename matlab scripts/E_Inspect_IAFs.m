close all
iapf_dir = '/projects/b1240/pronet/Pronet_EEG_VJP/iapf/';
save_fig_dir = '/projects/b1240/pronet/Pronet_EEG_VJP/QC/iapf_plots/';
subplotIndex = 1;
page_count = 0;

%have to redo subIDs to only select subIDs that made it through
%preprocessing
files = dir([iapf_dir,'*.mat']);
names = {files.name};
subIDs = regexprep(names, '_eyes_(open|closed)_iapf.mat$', '');
%drop repeats created by removing condition info
subIDs = unique(subIDs);

figure;
for j = 1:length(subIDs)
    subID = subIDs{j};
    for jj = 1:length(conditions)
        rows2plot = 6;
        subplot(rows2plot,2,subplotIndex)
        
        condition = conditions{jj};
        try
            load([iapf_dir,subID,'_',condition,'_iapf.mat'])
        catch
            continue
        end
        for jjj = 1:length(iapf.pChans)
            plot(iapf.f, iapf.pChans(jjj).pxx); hold on;
        end
         xline(iapf.pSum.paf);
            title([subID, ' ', condition],'Interpreter', 'none' );
            text(0.90, 0.90, num2str(iapf.pSum.paf), 'Units', 'normalized', ...
                        'HorizontalAlignment', 'right', ...
                        'VerticalAlignment', 'top', ...
                        'FontSize', 12);
        set(gcf, 'Units', 'normalized', 'OuterPosition', [0 0 1 1]);
        subplotIndex = subplotIndex + 1;
        if mod(subplotIndex,(rows2plot*2)+1) == 0
            page_count = page_count +1;
            saveas(gcf, [save_fig_dir, 'page_', num2str(page_count),'.png'])
            figure;
            subplotIndex = 1;
        end   
    end
end
