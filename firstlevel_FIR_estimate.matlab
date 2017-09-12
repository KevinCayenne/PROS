    SubjectS = 2; % Start
    SubjectE = 11; % End

    for SubjN = SubjectS:SubjectE

        final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
        final_dirin = dir(final_dir);
        final_dirin(1:2) = [];
        final_dirin(end-2:end) = [];
        target_output_dir = {}; 
        target_output_dir = [final_dir filesep final_dirin(SubjN).name];
        target_output_spm = [target_output_dir filesep 'SPM.mat'];

        matlabbatch{1}.spm.stats.fmri_est.spmmat = {target_output_spm};
        matlabbatch{1}.spm.stats.fmri_est.write_residuals = 0;
        matlabbatch{1}.spm.stats.fmri_est.method.Classical = 1;
        
       spm_jobman('run', matlabbatch)
       
    end