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
    colnum = 21;
    subject_num = 51;
    EmoOnsettime = EmoOnsettime - TriggerS;
    
    for o = 2:subject_num
        
        tonsets = cell(1,colnum);
        onsets = cell(1,colnum);
        durations = cell(1,colnum);
        orth = cell(1,colnum);
        temp = cell(1,1);
     
        for run = 1:6

            names = {'PRO_+300','PRO_+50','PRO_+20','PRO_Same','PRO_-20','PRO_-50','PRO_0',...
                     'PUR_+300','PUR_+50','PUR_+20','PUR_Same','PUR_-20','PUR_-50','PUR_0',...
                     'NEU_+300','NEU_+50','NEU_+20','NEU_Same','NEU_-20','NEU_-50','NEU_0'};

            emotion_condition_num = 7;
            k = 1;

            for iter = 1:3
                for j = 1:emotion_condition_num
                    for varnum = 1:length(EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j))
                        temp{1,1} = (EmoOnsettime(SubjectN==o & SITtag==iter & SessionN==run & RegMtag==j)/1000) + 362*(run-1);
                        if cellfun('isempty', temp(1,1)) == 0
                            onsets{1,k}(end+varnum) = temp{1,1}(varnum); 
                        end
                    end
                    durations{1,k} = 0;
                    orth{1,k} = 0;
                    k = k + 1;
                end
            end
            
            for count = 1:21 
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
                        i = i + 1;
                    end
                end
            end
            
            keep = any(~cellfun('isempty',names), 1);
            names = names(:,keep);
            onsets = onsets(:,keep);
            durations = durations(:,keep);
            orth = orth(:,keep);
        end
    
        if o < 10
           filename = sprintf('pilot_0%d_firstlevel_parameters.mat', o);
        else
           filename = sprintf('pilot_%d_firstlevel_parameters.mat', o);
        end
         
        save(filename, 'names', 'onsets', 'durations', 'orth');
         
    end

    
%% import data from text file. % script for importing data from the following text file: %
