%% First level batch

SubjectS = 2; % Start
SubjectE = 2; % End

%% start for loop to get the data path 

for SubjN = SubjectS:SubjectE

    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
    final_dirin = dir(final_dir);
    final_dirin(1:2) = [];
    final_dirin(end) = [];
    target_output_dir = {}; 
    target_output_dir = [final_dir filesep final_dirin(SubjN).name];
    

    dirname = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data';
    dirinfo = dir(dirname); % define file list
    dirinfo(1:2) = [];          % delete the first two filenames
    dirinfo(end) = [];         % delete the last one filenames
    swae_targetEPI = {};   % define targetEPI
    EPIra = {}; 

    target_firstlevel_dir = {};                                                                                                             % define file list under the directory
    target_firstlevel_dir = [target_firstlevel_dir, [dirname filesep 'PROS-Pilot-' num2str(SubjN) filesep]]; % delete the first two filenames

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
       
       for run = 1:6
          mat_path = firstlevel_dirin(run+(SubjN-2)*6).name;
          target_mat_path{run} = strcat(firstlevel_matfile_dir, mat_path);
       end
    
    %% Start Batch
 
    matlabbatch{1}.spm.stats.fmri_spec.dir = cellstr(target_output_dir);
    matlabbatch{1}.spm.stats.fmri_spec.timing.units = 'secs';
    matlabbatch{1}.spm.stats.fmri_spec.timing.RT = 2;
    matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = 38;
    matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = 1;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(1).scans = swae_targetEPI{1,1};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(1).cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(1).multi = target_mat_path(1);
    matlabbatch{1}.spm.stats.fmri_spec.sess(1).regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(1).multi_reg = target_TXT{1,1};
    matlabbatch{1}.spm.stats.fmri_spec.sess(1).hpf = 128;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(2).scans = swae_targetEPI{1,2};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(2).cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(2).multi = target_mat_path(2);
    matlabbatch{1}.spm.stats.fmri_spec.sess(2).regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(2).multi_reg = target_TXT{1,2};
    matlabbatch{1}.spm.stats.fmri_spec.sess(2).hpf = 128;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(3).scans = swae_targetEPI{1,3};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(3).cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(3).multi = target_mat_path(3);
    matlabbatch{1}.spm.stats.fmri_spec.sess(3).regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(3).multi_reg = target_TXT{1,3};
    matlabbatch{1}.spm.stats.fmri_spec.sess(3).hpf = 128;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(4).scans = swae_targetEPI{1,4};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(4).cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(4).multi = target_mat_path(4);
    matlabbatch{1}.spm.stats.fmri_spec.sess(4).regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(4).multi_reg = target_TXT{1,4};
    matlabbatch{1}.spm.stats.fmri_spec.sess(4).hpf = 128;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(5).scans = swae_targetEPI{1,5};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(5).cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(5).multi = target_mat_path(5);
    matlabbatch{1}.spm.stats.fmri_spec.sess(5).regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(5).multi_reg = target_TXT{1,5};
    matlabbatch{1}.spm.stats.fmri_spec.sess(5).hpf = 128;
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(6).scans = swae_targetEPI{1,6};
    %%
    matlabbatch{1}.spm.stats.fmri_spec.sess(6).cond = struct('name', {}, 'onset', {}, 'duration', {}, 'tmod', {}, 'pmod', {}, 'orth', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(6).multi = target_mat_path(6);
    matlabbatch{1}.spm.stats.fmri_spec.sess(6).regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess(6).multi_reg = target_TXT{1,6};
    matlabbatch{1}.spm.stats.fmri_spec.sess(6).hpf = 128;

    %%
    matlabbatch{1}.spm.stats.fmri_spec.fact = struct('name', {}, 'levels', {});
    matlabbatch{1}.spm.stats.fmri_spec.bases.fir.length = 24;
    matlabbatch{1}.spm.stats.fmri_spec.bases.fir.order = 12;
    matlabbatch{1}.spm.stats.fmri_spec.volt = 1;
    matlabbatch{1}.spm.stats.fmri_spec.global = 'None';
    matlabbatch{1}.spm.stats.fmri_spec.mthresh = 0.8;
    matlabbatch{1}.spm.stats.fmri_spec.mask = {''};
    matlabbatch{1}.spm.stats.fmri_spec.cvi = 'AR(1)';
    
    %%
    matlabbatch{2}.spm.stats.fmri_est.spmmat(1) = cfg_dep('fMRI model specification: SPM.mat File', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    matlabbatch{2}.spm.stats.fmri_est.write_residuals = 0;
    matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;
    
    %%
    % %     matlabbatch{3}.spm.stats.con.spmmat(1) = cfg_dep('Model estimation: SPM.mat File', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    
    % %     matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'PRO';
    % %     matlabbatch{3}.spm.stats.con.consess{1}.tcon.weights = [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{1}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{2}.tcon.name = 'PUR';
    % %     matlabbatch{3}.spm.stats.con.consess{2}.tcon.weights = [0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{2}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{3}.tcon.name = 'NEU';
    % %     matlabbatch{3}.spm.stats.con.consess{3}.tcon.weights = [0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{3}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{4}.tcon.name = 'SCAM';
    % %     matlabbatch{3}.spm.stats.con.consess{4}.tcon.weights = [0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{4}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{5}.tcon.name = 'PRO-NEU';
    % %     matlabbatch{3}.spm.stats.con.consess{5}.tcon.weights = [1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{5}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{6}.tcon.name = 'PRO - PUR';
    % %     matlabbatch{3}.spm.stats.con.consess{6}.tcon.weights = [1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{6}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{7}.tcon.name = 'SCAM - NEU';
    % %     matlabbatch{3}.spm.stats.con.consess{7}.tcon.weights = [0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{7}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{8}.tcon.name = 'SCAM - PUR';
    % %     matlabbatch{3}.spm.stats.con.consess{8}.tcon.weights = [0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{8}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{9}.tcon.name = 'PUR - NEU';
    % %     matlabbatch{3}.spm.stats.con.consess{9}.tcon.weights = [0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{9}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{10}.tcon.name = 'PRO - SCAM';
    % %     matlabbatch{3}.spm.stats.con.consess{10}.tcon.weights = [1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{10}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{11}.tcon.name = 'PUR - SCAM';
    % %     matlabbatch{3}.spm.stats.con.consess{11}.tcon.weights = [0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{11}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{12}.tcon.name = 'SCAM - PRO';
    % %     matlabbatch{3}.spm.stats.con.consess{12}.tcon.weights = [-1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{12}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{13}.tcon.name = 'NEU - PRO';
    % %     matlabbatch{3}.spm.stats.con.consess{13}.tcon.weights = [-1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{13}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{14}.tcon.name = 'NEU - SCAM';
    % %     matlabbatch{3}.spm.stats.con.consess{14}.tcon.weights = [0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{14}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.consess{15}.tcon.name = 'NEU - PUR';
    % %     matlabbatch{3}.spm.stats.con.consess{15}.tcon.weights = [0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0];
    % %     matlabbatch{3}.spm.stats.con.consess{15}.tcon.sessrep = 'none';
    % %     matlabbatch{3}.spm.stats.con.delete = 1;
    % %     
    %%
%     matlabbatch{3}.spm.stats.con.spmmat(1) = cfg_dep('Model estimation: SPM.mat File', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
%     matlabbatch{3}.spm.stats.con.consess{1}.fcon.name = 'Pros';
%     %%
%     matlabbatch{3}.spm.stats.con.consess{1}.fcon.weights = [1 0 0 0 0 0 0 0 0 0 0 0
%                                                         0 1 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 1 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 1 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 1 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 1 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 1 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 1 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 1 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 1 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 1 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 1];
%     %%
%     matlabbatch{3}.spm.stats.con.consess{1}.fcon.sessrep = 'repl';
%     matlabbatch{3}.spm.stats.con.consess{2}.fcon.name = 'Purc';
%     %%
%     matlabbatch{3}.spm.stats.con.consess{2}.fcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1];
%     %%
%     matlabbatch{3}.spm.stats.con.consess{2}.fcon.sessrep = 'repl';
%     matlabbatch{3}.spm.stats.con.consess{3}.fcon.name = 'Neu';
%     %%
%     matlabbatch{3}.spm.stats.con.consess{3}.fcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1];
%     %%
%     matlabbatch{3}.spm.stats.con.consess{3}.fcon.sessrep = 'repl';
%     matlabbatch{3}.spm.stats.con.consess{4}.fcon.name = 'Infrequent';
%     %%
%     matlabbatch{3}.spm.stats.con.consess{4}.fcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
%                                                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1];
% %%
%     matlabbatch{3}.spm.stats.con.consess{4}.fcon.sessrep = 'repl';
    
%     matlabbatch{3}.spm.stats.con.delete = 1;
   
     spm_jobman('run', matlabbatch)
    
end