    %% Import data from text file.
    % Script for importing data from the following text file:
    %
    % /bml/Data/Bank5/PROS/Pilot_image/Pilot_Behavior/behavior.csv
    %
    % To extend the code to different selected data or a different text file,
    % generate a function instead of a script.

    % Auto-generated by MATLAB on 2017/08/03 14:31:09

    %% Initialize variables.
    filename = '/bml/Data/Bank5/PROS/Pilot_image/Pilot_Behavior/behavior.CSV';
    delimiter = ',';
    startRow = 2;

    %% Read columns of data as strings:
    % For more information, see the TEXTSCAN documentation.

    formatSpec = '%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%q%[^\n\r]';

    %% Open the text file.

    fileID = fopen(filename,'r');

    %% Read columns of data according to format string.
    % This call is based on the structure of the file used to generate this
    % code. If an error occurs for a different file, try regenerating the code
    % from the Import Tool.

    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);

    %% Close the text file.

    fclose(fileID);

    % Convert the contents of columns containing numeric strings to numbers.
    % Replace non-numeric strings with NaN.

    raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
    for col=1:length(dataArray)-1
        raw(1:length(dataArray{col}),col) = dataArray{col};
    end
    numericData = NaN(size(dataArray{1},1),size(dataArray,2));

    for col=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37]
        % Converts strings in the input cell array to numbers. Replaced non-numeric
        % strings with NaN.
        rawData = dataArray{col};
        for row=1:size(rawData, 1);
            % Create a regular expression to detect and remove non-numeric prefixes and
            % suffixes.
            regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
            try
                result = regexp(rawData{row}, regexstr, 'names');
                numbers = result.numbers;
                
                % Detected commas in non-thousand locations.
                invalidThousandsSeparator = false;
                if any(numbers==',');
                    thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                    if isempty(regexp(numbers, thousandsRegExp, 'once'));
                        numbers = NaN;
                        invalidThousandsSeparator = true;
                    end
                end
                % Convert numeric strings to numbers.
                if ~invalidThousandsSeparator;
                    numbers = textscan(strrep(numbers, ',', ''), '%f');
                    numericData(row, col) = numbers{1};
                    raw{row, col} = numbers{1};
                end
            catch me
            end
        end
    end


    %% Allocate imported array to column variable names

    SubjectN = cell2mat(raw(:, 1));
    SessionN = cell2mat(raw(:, 2));
    GroupN = cell2mat(raw(:, 3));
    SexN = cell2mat(raw(:, 4));
    TriggerS = cell2mat(raw(:, 5));
    Main_tag = cell2mat(raw(:, 6));
    remainM = cell2mat(raw(:, 7));
    giveM = cell2mat(raw(:, 8));
    RegM = cell2mat(raw(:, 9));
    MDFirstP = cell2mat(raw(:, 10));
    MDRT = cell2mat(raw(:, 11));
    EFirstP = cell2mat(raw(:, 12));
    EmoRT = cell2mat(raw(:, 13));
    EmoTag = cell2mat(raw(:, 14));
    SITtag = cell2mat(raw(:, 15));
    RegMtag = cell2mat(raw(:, 16));
    ITI = cell2mat(raw(:, 17));
    ISI = cell2mat(raw(:, 18));
    TOnsettime = cell2mat(raw(:, 19));
    fixOnsettime = cell2mat(raw(:, 20));
    MDOnsettime = cell2mat(raw(:, 21));
    ISIstart = cell2mat(raw(:, 22));
    EmoOnsettime = cell2mat(raw(:, 23));
    EmoEndtime = cell2mat(raw(:, 24));
    TrialEnd = cell2mat(raw(:, 25));
    resttimeDPlus = cell2mat(raw(:, 26));
    resttimeEPlus = cell2mat(raw(:, 27));
    MoneyD_RT = cell2mat(raw(:, 28));
    EmoD_RT = cell2mat(raw(:, 29));
    ITI_D = cell2mat(raw(:, 30));
    MoneyD = cell2mat(raw(:, 31));
    ISI_D = cell2mat(raw(:, 32));
    EmoD = cell2mat(raw(:, 33));
    DTriggerOnset = cell2mat(raw(:, 34));
    TrialD = cell2mat(raw(:, 35));
    LongD = cell2mat(raw(:, 36));
    DefaultT = cell2mat(raw(:, 37));


    %% Clear temporary variables

    clearvars filename delimiter startRow formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me;

    %% Start output the design condition model .mat file 

    colnum = 32;
    subject_num = 50; %% change subject number!!
 
    EmoOnsettime = EmoOnsettime - TriggerS;
    MDOnsettime  = MDOnsettime - TriggerS;

    for o = 2:subject_num
        
     onsets = cell(1,32);
     durations = cell(1,colnum);
     orth = cell(1,colnum);
     temp = cell(1,1);
     
         for run = 1:6
            names = {'S_Prosocail','S_Purchase','S_Neutral','S_Scam',...
                           'Pro_E_+300','Pro_E_+50','Pro_E_+20','Pro_E_Same','Pro_E_-20','Pro_E_-50','Pro_E_0',...
                           'Pur_E_+300','Pur_E_+50','Pur_E_+20','Pur_E_Same','Pur_E_-20','Pur_E_-50','Pur_E_0',...
                           'Neu_E_+300','Neu_E_+50','Neu_E_+20','Neu_E_Same','Neu_E_-20','Neu_E_-50','Neu_E_0',...
                           'Unc_E_+300','Unc_E_+50','Unc_E_+20','Unc_E_Same','Unc_E_-20','Unc_E_-50','Unc_E_0',...
                           };
            situation_condition_num = 4;
            emotion_condition_num = 7;
            k = 1;   
            
            for n = 1:situation_condition_num

                    switch run
                        case 1
                            for varnum = 1:length(MDOnsettime(SubjectN==o & SessionN==run & SITtag==n))
                                temp{1,1} = (MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)/1000);
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 2
                            for varnum = 1:length(MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)) 
                                temp{1,1} = (MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)/1000) + 362;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                                end 
                            end
                        case 3
                            for varnum = 1:length(MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)) 
                                 temp{1,1} = (MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)/1000) + 362*2;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 4
                            for varnum = 1:length(MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)) 
                                 temp{1,1} = (MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)/1000) + 362*3;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                                end 
                            end
                        case 5
                            for varnum = 1:length(MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)) 
                                temp{1,1} = (MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)/1000) + 362*4;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                                end 
                            end
                        case 6
                            for varnum = 1:length(MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)) 
                                temp{1,1} = (MDOnsettime(SubjectN==o & SessionN==run & SITtag==n)/1000) + 362*5;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                                end 
                            end
                    end
                    %if cellfun('isempty', onsets(1,k)) == 1 % if the cell is empty then delete the column
                    %    onsets{1,k} = [];
                    %end
                
                 durations{1,k} = 0;
                 orth{1,k} = 0;
                 k=k+1;
            end
            
            m = situation_condition_num+1;
            
            for iter = 1:4 
                 for j = 1:emotion_condition_num
                    switch run
                        case 1
                            for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                                temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000);
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,m}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 2
                            for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                                temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000) + 362;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,m}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 3
                            for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                                temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000) + 362*2;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,m}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 4
                            for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                                temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000) + 362*3;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,m}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 5
                            for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                                temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000) + 362*4;
                                if cellfun('isempty', temp( 1,1)) == 0
                                    onsets{1,m}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                        case 6
                            for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                                temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000) + 362*5;
                                if cellfun('isempty', temp(1,1)) == 0
                                    onsets{1,m}(end+varnum) = temp{1,1}(varnum); 
                                end
                            end
                    end
                durations{1,m} = 0;
                orth{1,m} = 0;
                m=m+1;
                end
            end
            
            for count = 1:32
               cell_len = length(onsets{1,count});
               i = 1;
               while i <= cell_len
                    if (onsets{1,count}(i) == 0) == 1
                        iszero = (onsets{1,count}(i) == 0);
                        if iszero == 1
                            onsets{1,count}(i) = [];
                            i = i - 1;
                            cell_len = cell_len - 1;
                        end
                    else
                        i = i +1; 
                    end
                end
            end
            
            keep = any(~cellfun('isempty',names), 1);
            names = names(:,keep);
            onsets = onsets(:,keep);
            durations = durations(:,keep);
            orth = orth(:,keep);
            
         end
         
    %      if cellfun('isempty', onsets(1,k)) == 1 % if the cell is empty then delete the column
    %                    names{1,k} = [];
    %                    onsets{1,k} = [];
    %                    durations{1,k} = [];
    %                    orth{1,k} = [];
    %                    situation_condition_num = situation_condition_num - 1;
    %      end
         
    %      if cellfun('isempty', onsets(1,m)) == 1  % if the cell is empty then delete the column
    %                    names{1,m} = [];
    %                    onsets{1,m} = [];
    %                    durations{1,m} = [];
    %                    orth{1,m} = [];
    %             end
    
         if o < 10
            filename = sprintf('pilot_0%d_firstlevel_parameters.mat', o);
         else
            filename = sprintf('pilot_%d_firstlevel_parameters.mat', o);
         end
         
         save(filename, 'names', 'onsets', 'durations', 'orth');
         
    end

    