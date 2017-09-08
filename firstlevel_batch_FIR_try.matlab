%% First level batch

SubjectS = 10; % Start
SubjectE = 11; % End

%% start for loop to get the data path 

for SubjN = SubjectS:SubjectE

    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
    final_dirin = dir(final_dir);
    final_dirin(1:4) = [];
    final_dirin(end-2:end) = [];
    target_output_dir = {}; 
    target_output_dir = [final_dir filesep final_dirin(SubjN).name];
    target_output_spm = [target_output_dir filesep 'SPM.mat'];
    scans = [181 181 181 181 181 151];

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

    cd(char(target_firstlevel_dir));
    
    swae_EPITag = 'A':'F';
    
    % Get the swa* and *.txt files 
        for run = 1:6
               swae_EPIpath = {};
                    if run <= 5
                        swae_EPIfile = dir(strcat('swaep2dmoco4mm181*', swae_EPITag(run), '*a001.nii'));
                        rp_TXT_path = dir(strcat('rp_aep2dmoco4mm181*', swae_EPITag(run), '*a001.txt'));
                        target_TXT{run} = strcat(target_firstlevel_dir, rp_TXT_path.name);
                        for k = 1:181
                            swae_EPIpath = [swae_EPIpath; strcat(target_firstlevel_dir, swae_EPIfile.name, ',', num2str(k))];
                            swae_targetEPI{run} = swae_EPIpath;
                        end
                    else
                        swae_EPIfile = dir(strcat('swaep2dmoco4mm15*', swae_EPITag(6), '*a001.nii'));
                        rp_TXT_path = dir(strcat('rp_aep2dmoco4mm15*', swae_EPITag(6), '*a001.txt'));
                        target_TXT{run} = strcat(target_firstlevel_dir, rp_TXT_path.name);
                        for i = 1:151
                            swae_EPIpath = [swae_EPIpath; strcat(target_firstlevel_dir, swae_EPIfile.name, ',', num2str(i))];
                            swae_targetEPI{run} = swae_EPIpath;
                        end
                    end
        end
       
       firstlevel_matfile_dir = '/bml/Data/Bank5/PROS/Pilot_image/Pilot_Behavior/pilot_first_level_parameters/';
       firstlevel_dirin = dir(firstlevel_matfile_dir);
       firstlevel_dirin(1:2) = [];
       firstlevel_dirin(end) = [];
       
       com_txt = dir(strcat('combinedtxt.txt'));
       target_com_TXT = strcat(target_firstlevel_dir, com_txt.name);
       swae_targetEPI_all = {[swae_targetEPI{1};swae_targetEPI{2};swae_targetEPI{3};swae_targetEPI{4};swae_targetEPI{5};swae_targetEPI{6}]};
       
       for numr = SubjectS:SubjectE
          mat_path = firstlevel_dirin(numr-1).name;
          target_mat_path{numr} = strcat(firstlevel_matfile_dir, mat_path);
       end
    
    %% Start Batch
 
    matlabbatch{1}.spm.stats.fmri_spec.dir = cellstr(target_output_dir);
    matlabbatch{1}.spm.stats.fmri_spec.timing.units = 'secs';
    matlabbatch{1}.spm.stats.fmri_spec.timing.RT = 2;
    matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = 38;
    matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = 1;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess.scans = swae_targetEPI_all{1,1};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess.multi = target_mat_path(SubjN);
    
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(1).name = 'run1';
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(1).val = [ones(181,1); zeros((1056-181), 1)];
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(2).name = 'run2';
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(2).val = [zeros(181, 1); ones(181,1); zeros((1056-181*2), 1)];
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(3).name = 'run3';
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(3).val = [zeros(181*2, 1); ones(181,1); zeros((1056-181*3), 1)];
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(4).name = 'run4';
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(4).val = [zeros(181*3, 1); ones(181,1); zeros((1056-181*4), 1)];
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(5).name = 'run5';
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(5).val = [zeros(181*4, 1); ones(181,1); zeros((1056-181*5), 1)];
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(6).name = 'run6';
%     matlabbatch{1}.spm.stats.fmri_spec.sess.regress(6).val = [zeros(181*5, 1); ones(151, 1)];

    matlabbatch{1}.spm.stats.fmri_spec.sess.multi_reg = target_com_TXT;
    matlabbatch{1}.spm.stats.fmri_spec.sess.hpf = 128;

    %%
    matlabbatch{1}.spm.stats.fmri_spec.fact = struct('name', {}, 'levels', {});
    matlabbatch{1}.spm.stats.fmri_spec.bases.fir.length = 24;
    matlabbatch{1}.spm.stats.fmri_spec.bases.fir.order = 12;
    matlabbatch{1}.spm.stats.fmri_spec.volt = 1;
    matlabbatch{1}.spm.stats.fmri_spec.global = 'None';
    matlabbatch{1}.spm.stats.fmri_spec.mthresh = 0.8;
    matlabbatch{1}.spm.stats.fmri_spec.mask = {''};
    matlabbatch{1}.spm.stats.fmri_spec.cvi = 'AR(1)';
    
    spm_jobman('run', matlabbatch)
    
    spm_fmri_concatenate(target_output_spm, scans);
    
    %% pass estimate
%     matlabbatch{1}.spm.stats.fmri_est.spmmat = {target_output_spm};
%     matlabbatch{1}.spm.stats.fmri_est.write_residuals = 0;
%     matlabbatch{1}.spm.stats.fmri_est.method.Classical = 1;
% 
%     spm_jobman('run', matlabbatch)

end