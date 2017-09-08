
clear all; %#ok<CLALL>

SubjectS = 4; % Start
SubjectE = 11; % End

%% 

for SubjN = SubjectS:SubjectE

    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
    final_dirin = dir(final_dir);
    final_dirin(1:4) = [];
    final_dirin(end) = [];
    target_output_dir = {};  %#ok<NASGU>
    target_output_dir = [final_dir filesep final_dirin(SubjN).name]; % define target file directory
    target_spmfile = [target_output_dir filesep 'SPM.mat']; % define target SPM.mat file
    target_covari = [target_output_dir filesep 'covari.mat']; % define target covari variable .mat file
    
    dirname = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data';
    dirinfo = dir(dirname); % define file list
    dirinfo(1:2) = [];          % delete the first two filenames
    dirinfo(end) = [];         % delete the last one filenames
    swae_targetEPI = {};   % define targetEPI
    EPIra = {}; 
    
    target_firstlevel_dir = {};
    
    if SubjN < 10
        target_firstlevel_dir = [target_firstlevel_dir, [dirname filesep 'PROS-Pilot-0' num2str(SubjN) filesep]];  %#ok<AGROW>
    else
        target_firstlevel_dir = [target_firstlevel_dir, [dirname filesep 'PROS-Pilot-' num2str(SubjN) filesep]];  %#ok<AGROW>
    end
    
    swae_EPITag = 'A':'F';
    
    cd(char(target_firstlevel_dir));
    
    swae_EPIpath = {};
    
    for run = 1:6
        if run <= 5
            swae_EPIfile = dir(strcat('swaep2dmoco4mm181*', swae_EPITag(run), '*a001.nii'));
            for n = 1:181
                swae_EPIpath = [swae_EPIpath; strcat(target_firstlevel_dir, swae_EPIfile.name, ',', num2str(n))];
            end
        else
            swae_EPIfile = dir(strcat('swaep2dmoco4mm15*', swae_EPITag(run), '*a001.nii'));
            for k = 1:151
                 swae_EPIpath = [swae_EPIpath; strcat(target_firstlevel_dir, swae_EPIfile.name, ',', num2str(k))];
            end
        end
    end
    
    matlabbatch{1}.spm.stats.factorial_design.dir = {target_output_dir};
    matlabbatch{1}.spm.stats.factorial_design.des.t1.scans = swae_EPIpath;
    matlabbatch{1}.spm.stats.factorial_design.cov = struct('c', {}, 'cname', {}, 'iCFI', {}, 'iCC', {});
    matlabbatch{1}.spm.stats.factorial_design.multi_cov.files = {target_covari};
    matlabbatch{1}.spm.stats.factorial_design.multi_cov.iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.multi_cov.iCC = 5;
    matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
    matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;
    
    spm_jobman('run', matlabbatch)
    
    % load SPM.mat file
    load(target_spmfile); 

    % delete first column
    SPM.xX.X(:,1) = []; 

    % change '19' to '1'
    SPM.xX.X(SPM.xX.X(:,:)==19) = 1;

    % save SPM.mat file
    filename = [target_output_dir filesep 'SPM.mat'];
    save(filename, 'SPM'); 

    %%

end