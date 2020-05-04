% The following script take pre-processesed data from a study investigating
% investigating cognitive function and interventions in people suffering
% from chronic pain. The script was written by patrick skippen
% (p.skippen@neura.edu.au).

%% Set-up workspace
clc;clearvars
cwd = pwd; % Find current working directory. This should be where you find this script.

DATAIN    = [cwd filesep 'Data' filesep]; % Change string if the data is found i a different folder
FUNCTIONS = [cwd filesep 'Functions' filesep]; % Local functions to be used here.
PACKAGES =  [cwd filesep 'Functions' filesep]; % Package library containing eeglab, fieldtrip, etc
EEGRAW = ['EEG' filesep 'Raw' filesep];
EEGANALYSED = ['EEG' filesep 'Analysis' filesep];
COND = {'np','p'}; % Condtions, 'no pain' and 'pain'
subj = dir([DATAIN filesep 'CLP1*']); % change to however you identify participants

addpath([PACKAGES 'eeglab_current' filesep 'eeglab2019_1']);
eeglab

addpath([PACKAGES 'fieldtrip-20200215']);
ft_defaults

addpath([PACKAGES 'EEG-Clean-Tools-master' filesep 'PrepPipeline'])
%% New Preprocessing script to the following steps
% 1. Filter @ 1Hz
% 2. Line noise removal @ 60 Hz + harmonics up until (sample rate/2)-1
% 3. Automatic bad channel detection and removal using clean_artifacts
% 4. Epoching
% 5. Extreme epoch noise detection using EEGLab's threshold & joint probability procedures
% 6. Run ICA (bin_ica)
% 7. Automated detection of ICA artefacts using ICLabel
% 8. Review of component decisions and inspection of data
% 9. Removal of components
% 10. Re-load original data, high pass at .1 Hz, remove line noise, same bad channels, same bad epochs and then apply ICA weights to data.
% 11. Ready for Processing.

%% Load and Format Data - DONE!
% Load data: First file often has 'test' in name. Others do not.
% Code should accounts for this in it's curent form.

for px = 1:length(subj) % CLP116 does not have Pain data. Use try-catch.
    % Find filenames of RAW data files.
    % Not interested in Eyes Closed Resting state (at this stage)
    tic
    fprintf(['Working on participant: ' subj(px).name '\n'])
    try
        for condition = 1:length(COND)
            
            Filenames = dir([DATAIN subj(px).name filesep EEGRAW '*.vhdr']); % Find all files with no pain
            IDX = strfind(lower({Filenames.name}),'ec'); % find any instances of ec (eyes closed resting state)
            TF = cellfun('isempty', IDX); % find location of these files containing 'ec'
            Filenames = Filenames(TF); % remove any filenames containing 'ec'
            
            fprintf(['Working on condition: ' COND{condition} '\n'])
            % extract only filenames of interest to condition.
            IDX = strfind(lower({Filenames.name}),['_' COND{condition}]);
            TF = ~cellfun('isempty', IDX);
            Filenames = Filenames(TF);
            % Sort by creation date to keep files in order
            [~,index] = sortrows({Filenames.date}.');
            Filenames = Filenames(index);
            clear index % Clean as you go
            
            ALLEEG = []; % Clear ALLEEG in preparation for the next condtion/participant
            
            for block = 1:length(Filenames)
                EEG = pop_loadbv([DATAIN subj(px).name filesep EEGRAW], Filenames(block).name);
                EEG.setname = [subj(px).name '_' COND{condition} '_' Filenames(block).name(end-5)]; % Rename to the suffix denoting block number. Which ends the filename.
                EEG = eeg_checkset(EEG);
                [ALLEEG, ~, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'gui','off');
            end
            
            % Merge EEG blocks into one dataset
            EEG = pop_mergeset(ALLEEG, 1:length(Filenames), 0); % As the sorting above puts things in order, 1:length should work.
            EEG.setname = [subj(px).name '_' COND{condition}];
            EEG = eeg_checkset(EEG);
            
            % Drop externals
            EEG = pop_select(EEG,'nochannel',65:EEG.nbchan);
            
            % Add initial (AFz) reference
            EEG.nbchan = EEG.nbchan+1;
            EEG.data(end+1,:) = zeros(1, EEG.pnts);
            EEG.chanlocs(1,EEG.nbchan).labels = 'AFz';
            
            % Adde EEG channel locations from file
            EEG = pop_chanedit(EEG, 'lookup', ...
                [PACKAGES 'eeglab_current' filesep 'eeglab2019_1' filesep 'plugins\dipfit\standard_BESA\standard-10-5-cap385.elp']);
            EEG.chanlocs(EEG.nbchan).ref = 1;
            EEG = eeg_checkset(EEG);
            
            % Label chanlocs appropriately
            for i = 1:length(EEG.chanlocs)
                EEG.chanlocs(i).type = 'EEG';
            end
            
            % Save event codes for RT analysis in R
            EventCodes = EEG.event;
            save([DATAIN subj(px).name filesep 'Behavior' filesep subj(px).name '_' COND{condition} '_EventCodes.mat'],...
                'EventCodes');
            
            % save dataset
            savename = [subj(px).name '_' COND{condition} '.set'];
            pop_saveset(EEG, 'filename', savename,...
                'filepath', [DATAIN subj(px).name filesep],...
                'savemode','twofiles');
            
            ALLEEG = []; % Clear ALLEEG in preparation for the next condtion/participant
            EEG = []; % Clean as we go. Prevents absent participants getting through
            EventCodes = []; % Clean as we go
        end
    catch e
        e;
    end
    t = toc;
    fprintf(['Participant ' subj(px).name ' took %.2fs to process' '\n'],t)
end

%% Pre Processing Pipeline
for px = 1:length(subj)
    fprintf(['Working on participant: ' subj(px).name '\n'])
    for condition = 1:length(COND)
        tic;
        try
            % Load data
            EEG = pop_loadset('filename',[subj(px).name '_' COND{condition} '.set'],...
                'filepath',[DATAIN subj(px).name]);
            EEG = eeg_checkset(EEG);
            
            % Apply average reference and remove initial (AFz) reference
            EEG = pop_reref(EEG, []);
            EEG = pop_select( EEG,'nochannel',{'AFz'});
            
            % Remove data during block breaks
            
            % Mark boundary events
            newsegments = find(strcmp({EEG.event.code},'New Segment'));
            firstbound  = find(cellfun(@isempty,{EEG.event.code}));
            
            % Tag first stimulus of next block and last stimulus of
            % previous block
            firststim = newsegments +1;
            laststim  = firstbound -1;
            % Find latencies
            laststim_latency  = [EEG.event(laststim).latency];
            firststim_latency = [EEG.event(firststim).latency];
            % Create Structure of lantencies to be removed
            toremove    = {};
            toremove{1} = [1,firststim_latency(1)-3500];
            for i = 2:length(laststim_latency)
                toremove{i} = [laststim_latency(i)+1500,firststim_latency(i+1)-3500];
            end
            % Remove inter-block sections
            formattedremoval = cat(1,toremove{:});
            EEG = eeg_eegrej(EEG,formattedremoval);
            
            % save dataset on the go
            savename = [EEG.setname '_InterBlocksRemoved.set'];
            pop_saveset(EEG, 'filename', savename,...
                'filepath', [DATAIN filesep subj(px).name],...
                'savemode','twofiles');
            
            % Identify Mastoid channel for removal below
            mastoid = find(strcmp({EEG.chanlocs.labels},'A1'));
            
            % Part II:  HP the signal for detecting bad channels and ICA
            fprintf('Preliminary detrend to compute preprocessing\n');
            detrendIn = struct();
            defaults = getPrepDefaults(EEG, 'detrend');
            detrendOut = struct('detrendChannels', [], 'detrendType', [], ...
                'detrendCutoff', [], 'detrendStepSize', [], ...
                'detrendCommand', []);
            [detrendOut, errors] = checkDefaults(detrendIn, detrendOut, defaults);
            
            tic
            [p_EEG, detrend] = removeTrend(EEG, detrendOut);
            EEG.etc.noiseDetection.detrend = detrend;
            defaults = getPrepDefaults(EEG, 'detrend');
            params = checkDefaults(detrend, detrendIn, defaults);
            computationTimes.detrend = toc;
            
            % Part III: Remove line noise
            fprintf('Line noise removal\n');
            lineNoiseIn = struct();
            defaults = getPrepDefaults(EEG, 'linenoise');
            lineNoiseOut = struct('lineNoiseMethod', [], ...
                'lineNoiseChannels', [], 'Fs', [], ...
                'lineFrequencies', [], 'p', [], 'fScanBandWidth', [], ...
                'taperBandWidth', [], 'taperWindowSize', [], ...
                'taperWindowStep', [], 'tau', [], 'pad', [], ...
                'fPassBand', [], 'maximumIterations', []);
            [lineNoiseOut, errors] = checkDefaults(lineNoiseIn, lineNoiseOut, defaults);
            
            tic
            [EEGClean, lineNoise] = removeLineNoise(p_EEG, lineNoiseOut);
            EEG.etc.noiseDetection.lineNoise = lineNoise;
            lineChannels = lineNoise.lineNoiseChannels;
            EEG.data(lineChannels, :) = EEG.data(lineChannels, :) ...
                - p_EEG.data(lineChannels, :) + EEGClean.data(lineChannels, :);
            clear p_EEG;
            computationTimes.lineNoise = toc;
            
            % Run the Artifact removal to detect bad channels
            % Only on EEG channels
            EEGClean = pop_select(EEGClean,'nochannel',mastoid);
            
            % We set a channel correlation criterion of 0.6 (oppssed to
            % default 0.8
            % Line Noise of 6 means that the bad channel must be 8SD higher
            % Flatline criterion: we must have at least a block of the data flat lined.
            % I make the ratio 1/6 to account for breaks in the data.
            EEG_preproc = clean_artifacts(EEGClean,'ChannelCriterion',0.6,...
                'LineNoiseCriterion',6,'BurstCriterion','off',...
                'WindowCriterion','off','Highpass','off','FlatlineCriterion',EEGClean.xmax/6);
            % Save off Bad channels for later referal
            
            if length(EEG_preproc.chanlocs)==length(EEGClean.chanlocs)
            fileID = fopen([DATAIN subj(px).name filesep EEG_preproc.setname '_BadChannels.txt'],'w');
            fprintf(fileID,'%d\n',0);
            fclose(fileID);
            else
                            badchan_idx = find(EEG_preproc.etc.clean_channel_mask==0);
            fileID = fopen([DATAIN subj(px).name filesep EEG_preproc.setname '_BadChannels.txt'],'w');
            fprintf(fileID,'%d\n',badchan_idx);
            fclose(fileID);
            end
            
            % save dataset on the go
            savename = [EEG_preproc.setname '_PreProcessing.set'];
            pop_saveset(EEG_preproc, 'filename', savename,...
                'filepath', [DATAIN filesep subj(px).name],...
                'savemode','twofiles');
            
            % Run ICA on continuous data
            % Our paradigm has several triggers, overlapping, without the long
            % SOA required.
            EEG_ICA = pop_runica(EEG_preproc,'icatype','runica');
            
            % save dataset on the go
            savename = [EEG_ICA.setname '_ICA.set'];
            pop_saveset(EEG_ICA, 'filename', savename,...
                'filepath', [DATAIN filesep subj(px).name],...
                'savemode','twofiles');
        catch e
            e;
        end
    end
    t = toc;
    fprintf(['Participant ' subj(px).name ' took %.2fs to process' '\n'],t)
end

%% Save off as one .mat file
% for px = 1:length(subj) % CLP116 does not have Pain data. Use try-catch.
%     Find filenames of RAW data files.
%     Not interested in Eyes Closed Resting state (at this stage)
%     tic
%     fprintf(['Working on participant: ' subj(px).name '\n'])
%     for condition = 1:length(COND)
%         try
%             EEG = pop_loadset('filename',[subj(px).name '_' COND{condition} '.set'],...
%                 'filepath',[DATAOUT subj(px).name filesep EEGANALYSED]);
%             EEG = eeg_checkset(EEG);
%             [ALLEEG, ~, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'gui','off');
%
%             EEG = []; % Clean as we go. Prevents absent participants getting through
%
%         catch e
%             e;
%         end
%     end
%     t = toc;
%     fprintf(['Participant ' subj(px).name ' took %.2fs to process' '\n'],t)
% end
%
% save([DATAOUT 'ALL_RAW_EEG.mat'],'ALLEEG','-v7.3');


%% Tag Comps
for px = 1:length(subj)
    fprintf(['Working on participant: ' num2str(subj(px)) '\n'])
    for condition = 1:length(COND)
        if isfile([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica.mat'])
            fprintf('loading data');
            tic
            
            load([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica.mat']);
            t = toc;
            fprintf(['Participant ' num2str(subj(px)) ' took %.2fs to load' '\n'],t)
            cfg = [];
            cfg.layout='EEG1010.lay';
            cfg.viewmode = 'component';
            cfg.component = 1:length(ic_data.label);
            ft_databrowser(cfg,ic_data)
            prompt = 'Please Identify Component to reject in matrix form:';
            rejected = input(prompt);
            cfg.component = rejected; % rejected components
            data_pruned = ft_rejectcomponent(cfg,ic_data);
            % Write rejected componenets to file
            filename = [DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rejectedcomps.txt'];
            fileID = fopen(filename,'w');
            fprintf(fileID,'%d\n',rejected);
            fclose(fileID);
        else
            fprintf('Participants ICA not complete')
        end
    end   
    
end
%% Removed Comps
addpath(genpath('E:\eeglab_current\eeglab2019_1\plugins\CSDtoolbox\'));

for px = 1:length(subj)
    tic
    fprintf(['Working on participant: ' num2str(subj(px)) '\n'])
    for condition = 1:length(COND)
        if isfile([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica.mat'])
            fprintf('loading data');
            
            load([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica.mat']);
            
            fprintf('Reading in rejected components')
            fileID = fopen([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rejectedcomps.txt'],'r');
            formatspec = '%d\n';
            rejected = fscanf(fileID,formatspec);
            fclose(fileID);
            
            cfg = [];
            %             cfg.layout='EEG1010.lay';
            %             cfg.viewmode = 'component';
            %             cfg.component = 1:length(ic_data.label);
            %             ft_databrowser(cfg,ic_data)
            %             prompt = 'Please Identify Component to reject in matrix form:';
            %             rejected = input(prompt);
            cfg.component = rejected; % rejected components
            data_pruned = ft_rejectcomponent(cfg,ic_data);
            
            setname = ['CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic']; % ICA run
            savename = [setname '.mat'];
            save([DATAIN filesep savename],'data_pruned','-v7.3');
            % CSD transform
            M = ExtractMontage(['E:\eeglab_current\eeglab2019_1\plugins\CSDtoolbox\resource\10-5-System_Mastoids_EGI129.csd'],data_pruned.label);
            % MapMontage(M);
            [G,H] = GetGH(M);%get montage
            trials_out = cell(1,length(data_pruned.trial));
            for trial_i = 1:length(data_pruned.trial)
                %                 if trial_i == 1
                %                     fprintf('\t\t');
                %                 end
                fprintf('.');
                trials_out{trial_i} = CSD(data_pruned.trial{trial_i},G,H);
            end
            data_pruned.oldtrials = data_pruned.trial;
            data_pruned.trial = trials_out;
            
            setname = [setname '_CSD'];
            savename = [setname '.mat'];
            save([DATAIN filesep savename],'data_pruned','-v7.3');
            % Tf Transform- Andrews Script
            cfg=[];
            cfg.method = 'mtmfft';
            cfg.taper = 'hanning';
            cfg.foi = [.2:.2:50];
            cfg.keeptrials = 'yes';
            data_freq = ft_freqanalysis(cfg,data_pruned);
            
            setname = [setname '_Tf'];
            savename = [setname '.mat'];
            save([DATAIN filesep savename],'data_freq','-v7.3');
            
        else
            fprintf('Participants ICA not complete')
        end
    end
    t = toc;
    fprintf(['Participant ' num2str(subj(px)) ' took %.2fs to process' '\n'],t)
end

%% Andrews PAF Measures - Fix this to inlcude all possible channels
% Spectra = zeros(length(subj),length(COND),64);
% PeakFrequency = zeros(length(subj),length(COND),64);
% AlphaPower = zeros(length(subj),length(COND),64);
freq = 9:.2:11;
PAF = [];
Alpha = [];
Size = [];
PeakFrequency = [];
AlphaPower = [];
Spectra = [];

load([DATAIN filesep 'CLP' num2str(subj(1)) '_' COND{1} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD_Tf.mat']);
electrodes = data_freq.label;
save('E:\ElectrodeLabels.mat','electrodes')
for px = 1:length(subj)
    tic
    fprintf(['Working on participant: ' num2str(subj(px)) '\n'])
    for condition = 1:length(COND)
        if isfile([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD_Tf.mat'])
            fprintf('loading data');
            
            load([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD_Tf.mat']);
            
            
            
            for elect = 1:length(electrodes)
                temp = squeeze(data_freq.powspctrm(:,elect,:));
                temp4 = zscore(temp(:,10:end),[],2);
                temp2 = [];
                temp3 = [];
                for y = 1:size(temp,1)
                    temp2(y,1) = sum(freq.*temp(y,45:55))/sum(temp(y,45:55));
                    temp3(y,1) = sum(temp4(y,30:50));
                end
                Spectra(px,condition,elect,:) = mean(temp); %average spectra for subject s, at channel z, in dataset d
                PeakFrequency(px,condition,elect) = mean(temp2); %average PAF for subject s, at channel z, in dataset d
                AlphaPower(px,condition,elect) = mean(temp3); %average z-scored alpha power for subject s, at channel z, in dataset d
            end
        else
            fprintf('Participants ICA not complete')
        end
        
    end
    t = toc;
    fprintf(['Participant ' num2str(subj(px)) ' took %.2fs to process' '\n'],t)
    
    
end

save([DATAIN filesep 'ImpSpectra.mat'],'Spectra')
save([DATAIN filesep 'ImpPeakFrequency.mat'],'PeakFrequency')
save([DATAIN filesep 'ImpAlphaPower.mat'],'AlphaPower')

%% More PAF
foi = length(data_freq.powspctrm(1,1,:));
% a1 = plot(2:.2:30,mean(zscore(Spectra{3}{6}(:,10:150),[],2)),'black');

plot(foi,squeeze(mean(zscore(Spectra(:,1,1,:),[],1))),'black');


%% Extract Conditions
% See R code: ExtractRT_MATLAB.R for extraction of RT

%% Full Time Frequency analysis
elecOI = {'Fz','FCz','Cz','CPz','Pz','FC3','FC4','C3','C4','CP3','CP4'};
for i = 1:length(elecOI)
    sites(i) = find(strcmp(data_pruned.label,elecOI{i}));
end

fmin = 2;
fmax = 30;
fbins = 80;
frequencies = logspace(log10(fmin),log10(fmax),fbins);

% other wavelet parameters
times_x = -1:1/1024:1.249023437500000;
half_of_wavelet_size = (length(times_x)-1)/2;

Tfreq = zeros(length(subj),length(COND),length(sites),length(frequencies),length(times_x));
ERP = zeros(length(subj),length(COND),length(sites),length(times_x));
for px = 1:length(subj)
    tic
    fprintf(['Working on participant: ' num2str(subj(px)) '\n'])
    for condition = 1:length(COND)
        if isfile([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD.mat'])
            fprintf('loading data');
            
            load([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD.mat']);
            
            catdat = data_pruned.trial{1}([sites],:);
            % take a structure and concatenate each subsequent trial to the third dim -
            % PS 13/09/2018
            for trial_i = 2:length(data_pruned.trial)
                catdat = cat(3,catdat,data_pruned.trial{trial_i}([sites],:));
            end
            
            % FFT parameters
            n_wavelet     = length(times_x);
            n_data        = length(catdat(:,:,1))*length(catdat(:,1,:));
            n_convolution(1:2) = n_wavelet+n_data-1;
            % n_convolution(3)   = n_wavelet+length(catdat(:,:,1))-1; % For non-phase
            
            fprintf('starting electrode loop')
            for electrode_i = 1:length(sites)
                
                % compute ERP
                ERP(px,condition,electrode_i,:) = squeeze(mean(catdat(electrode_i,:,:),3));
                
                
                
                % FFT of data
                fft_EEG = fft(reshape(catdat(electrode_i,:,:),1,length(catdat(:,:,1))*length(catdat(:,1,:))),n_convolution(1)); % total
                
                % initialize output time-frequency data
                %                 tf = zeros(length(sites),length(frequencies),length(catdat(:,:,1)));
                fprintf('freq')
                for fi=1:length(frequencies)
                    fprintf('.\n')
                    
                    % create wavelet
                    wavelet = exp(2*1i*pi*frequencies(fi).*times_x) .* exp(-times_x.^2./(2*(4/(2*pi*frequencies(fi)))^2))/frequencies(fi);
                    
                    % take FFT of data
                    fft_wavelet = fft(wavelet,n_convolution(1));
                    
                    % convolution...
                    convolution_result_fft = ifft(fft_wavelet.*fft_EEG,n_convolution(1));
                    convolution_result_fft = convolution_result_fft(half_of_wavelet_size+1:end-half_of_wavelet_size);
                    
                    % reshaping and trial averaging is done only on all trials
                    convolution_result_fft = reshape(convolution_result_fft,length(catdat(:,:,1)),length(catdat(:,1,:)));
                    
                    % compute power
                    Tfreq(px,condition,electrode_i,fi,:) = mean(abs(convolution_result_fft).^2,2);
                    
                end % end frequency loop
            end
            
        else
            fprintf('Participants ICA not complete')
            
        end
        %         Tfreq(px,condition,:,:,:) = tf(:,:,:);
    end
    
    t = toc;
    fprintf(['Participant ' num2str(subj(px)) ' took %.2fs to process' '\n'],t)
end
save([DATAIN filesep 'Time_Freq_selectsites.mat'],'Tfreq');
save([DATAIN filesep 'ERP_selectsites.mat'],'ERP');

load([DATAIN filesep 'Time_Freq_selectsites.mat']);
load([DATAIN filesep 'ERP_selectsites.mat']);

%% Baseline and Plot
load([DATAIN filesep 'CLP' num2str(subj(1)) '_' COND{1} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD.mat']) % Dummy Data
times_x = -1:1/1024:1.249023437500000;

fmin = 2;
fmax = 30;
fbins = 80;
frequencies = logspace(log10(fmin),log10(fmax),fbins);

elecOI = {'Fz','FCz','Cz','CPz','Pz','FC3','FC4','C3','C4','CP3','CP4'};
for i = 1:length(elecOI)
    sites(i) = find(strcmp(data_pruned.label,elecOI{i}));
end
baselinestart = -.5;
baselineend = -.25;

baselineinds = [find(times_x==baselinestart):find(times_x==baselineend)];

BL_Tfreq = zeros(length(subj),length(COND),length(sites),length(frequencies),length(times_x),'single');

Tfreq(Tfreq==0) = NaN;

for px = 1:length(subj)
    tic
    for condition = 1:length(COND)
        fprintf(['working on Participant = ' num2str(subj(1)) ', Condition = ' COND{condition} '\n'])
        for electrodesofinterest = 1:length(sites)
            Pow = squeeze(Tfreq(px,condition,electrodesofinterest,:,:));
            BL_Tfreq(px,condition,electrodesofinterest,:,:) = bsxfun(@minus,10*log10(Pow),10*log10(nanmean(Pow(:,baselineinds),2)));
        end
    end
    toc
end

%% ERP
ERP = zeros(length(subj),length(COND),length(sites),length(times_x));
for px = 1:length(subj)
    tic
    fprintf(['Working on participant: ' num2str(subj(px)) '\n'])
    for condition = 1:length(COND)
        if isfile([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD.mat'])
            fprintf('loading data');
            
            load([DATAIN filesep 'CLP' num2str(subj(px)) '_' COND{condition} '_rs_lhn_rr_ce_crrr_bcr_ica_rmic_CSD.mat']);
            
            catdat = data_pruned.trial{1}([sites],:);
            % take a structure and concatenate each subsequent trial to the third dim -
            % PS 13/09/2018
            for trial_i = 2:length(data_pruned.trial)
                catdat = cat(3,catdat,data_pruned.trial{trial_i}([sites],:));
            end
            
            fprintf('starting electrode loop')
            for electrode_i = 1:length(sites)
                
                % compute ERP
                ERP(px,condition,electrode_i,:) = squeeze(mean(catdat(electrode_i,:,:),3));
            end
        else
            fprintf('not found')
        end
    end
end

%% Baseline ERP
BL_ERP = zeros(length(subj),length(COND),length(sites),length(times_x),'single');

ERP(ERP==0) = NaN;

for px = 1:length(subj)
    tic
    for condition = 1:length(COND)
        fprintf(['working on Participant = ' num2str(subj(1)) ', Condition = ' COND{condition} '\n'])
        for electrodesofinterest = 1:length(sites)
            erp = squeeze(ERP(px,condition,electrodesofinterest,:));
            BL_ERP(px,condition,electrodesofinterest,:) = bsxfun(@minus,erp,nanmean(erp(baselineinds)));
        end
    end
    toc
end

%% Plot ERP
tic
plot_count = 1;
FigH = figure('Position', get(0, 'Screensize'));
F    = getframe(FigH);
for electrode_i = 1:length(sites(1:5))
    for cond_i = 1:3
        fprintf(['Condition ' condtype{cond_i} '\n'])
        subplot(length(sites(1:5)),3,plot_count)
        if cond_i == 3
            data = squeeze(nanmean(BL_ERP(:,1,electrode_i,:),1)) - squeeze(nanmean(BL_ERP(:,2,electrode_i,:),1));
            
        else
            data = squeeze(nanmean(BL_ERP(:,cond_i,electrode_i,:),1));
        end
        plot(times_x(x_axis(1):x_axis(2)),data(x_axis(1):x_axis(2)))
        
        xlabel('Time (ms)')
        ylabel('Amplitude (uV)')
        title([condtype{cond_i} '  ' elecOI{electrode_i}])
        %         colormap jet
        plot_count = plot_count+1;
    end
end
toc
saveas(gcf, [DATAIN filesep 'ERP_MidlineSites.tiff'], 'tiff');

%% Plot ERP Lateral Sites
tic
plot_count = 1;
FigH = figure('Position', get(0, 'Screensize'));
F    = getframe(FigH);
for electrode_i = 1:length(sites(6:end))
    for cond_i = 1:3
        fprintf(['Condition ' condtype{cond_i} '\n'])
        subplot(length(sites(6:end)),3,plot_count)
        if cond_i == 3
            data = squeeze(nanmean(BL_ERP(:,1,electrode_i+5,:),1)) - squeeze(nanmean(BL_ERP(:,2,electrode_i+5,:),1));
            
        else
            data = squeeze(nanmean(BL_ERP(:,cond_i,electrode_i+5,:),1));
        end
        plot(times_x(x_axis(1):x_axis(2)),data(x_axis(1):x_axis(2)))
        
        xlabel('Time (ms)')
        ylabel('Amplitude (uV)')
        title([condtype{cond_i} '  ' elecOI{electrode_i+5}])
        %         colormap jet
        plot_count = plot_count+1;
    end
end
toc
saveas(gcf, [DATAIN filesep 'ERP_LateralSites.tiff'], 'tiff');

%% Plotting Time Freq
x_axis = [find(times_x==-.25),find(times_x==times_x(end))];
condtype = {'No-Pain','Pain','Diff (np-p)'};


tic
plot_count = 1;
FigH = figure('Position', get(0, 'Screensize'));
F    = getframe(FigH);
for electrode_i = 1:length(sites(1:5))
    for cond_i = 1:3
        fprintf(['Condition ' condtype{cond_i} '\n'])
        subplot(length(sites(1:5)),3,plot_count)
        if cond_i == 3
            data = squeeze(nanmean(BL_Tfreq(:,1,electrode_i,:,:),1)) - squeeze(nanmean(BL_Tfreq(:,2,electrode_i,:,:),1));
            
        else
            data = squeeze(nanmean(BL_Tfreq(:,cond_i,electrode_i,:,:),1));
        end
        contourf(times_x(x_axis(1):x_axis(2)),frequencies,data(:,x_axis(1):x_axis(2)),40,'linecolor','none')
        %             if PowerTypes{PowerType_i} == 'ITPCBL' % might adjust the scales for each powertype later
        %                 caxis([0 1]);
        %             else
        caxis([-1.5 2.5]);
        %             end
        xlabel('Time (ms)')
        ylabel('Frequency (Hz)')
        title([condtype{cond_i} '  ' elecOI{electrode_i}])
        %         colormap jet
        plot_count = plot_count+1;
    end
end
toc
saveas(gcf, [DATAIN filesep 'Tfreq_MidlineSites.tiff'], 'tiff');

%% Plot lateral sites

tic
plot_count = 1;
FigH = figure('Position', get(0, 'Screensize'));
F    = getframe(FigH);
for electrode_i = 1:length(sites(6:end))
    for cond_i = 1:3
        fprintf(['Condition ' condtype{cond_i} '\n'])
        subplot(length(sites(6:end)),3,plot_count)
        if cond_i == 3
            data = squeeze(nanmean(BL_Tfreq(:,1,electrode_i+5,:,:),1)) - squeeze(nanmean(BL_Tfreq(:,2,electrode_i+5,:,:),1));
            
        else
            data = squeeze(nanmean(BL_Tfreq(:,cond_i,electrode_i+5,:,:),1));
        end
        contourf(times_x(x_axis(1):x_axis(2)),frequencies,data(:,x_axis(1):x_axis(2)),40,'linecolor','none')
        %             if PowerTypes{PowerType_i} == 'ITPCBL' % might adjust the scales for each powertype later
        %                 caxis([0 1]);
        %             else
        caxis([-1.5 2.5]);
        %             end
        xlabel('Time (ms)')
        ylabel('Frequency (Hz)')
        title([condtype{cond_i} '  ' elecOI{electrode_i+5}])
        %         colormap jet
        plot_count = plot_count+1;
    end
end
toc
saveas(gcf, [DATAIN filesep 'Tfreq_LateralSites.tiff'], 'tiff');
