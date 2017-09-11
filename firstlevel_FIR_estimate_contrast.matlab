    SubjectS = 4; % Start
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

        matlabbatch{2}.spm.stats.con.spmmat = {target_output_spm};
        matlabbatch{2}.spm.stats.con.consess{1}.fcon.name = 'PROSOCIAL';
        matlabbatch{2}.spm.stats.con.consess{1}.fcon.weights = [ones(1056, 12), zeros(1056, 120)];
        matlabbatch{2}.spm.stats.con.consess{1}.fcon.sessrep = 'none';
        matlabbatch{2}.spm.stats.con.consess{2}.fcon.name = 'PURCHASE';
        matlabbatch{2}.spm.stats.con.consess{2}.fcon.weights = [zeros(1056, 12), ones(1056, 12), zeros(1056, 108)];
        matlabbatch{2}.spm.stats.con.consess{2}.fcon.sessrep = 'none';
        matlabbatch{2}.spm.stats.con.consess{3}.fcon.name = 'NEUTRAL';
        matlabbatch{2}.spm.stats.con.consess{3}.fcon.weights = [zeros(1056, 24), ones(1056, 12), zeros(1056, 96)];
        matlabbatch{2}.spm.stats.con.consess{3}.fcon.sessrep = 'none';
        matlabbatch{2}.spm.stats.con.consess{4}.fcon.name = 'UNCOMMON';
        matlabbatch{2}.spm.stats.con.consess{4}.fcon.weights = [zeros(1056, 36), ones(1056, 12), zeros(1056, 84)];
        matlabbatch{2}.spm.stats.con.consess{4}.fcon.sessrep = 'none';
        matlabbatch{2}.spm.stats.con.delete = 0;

        spm_jobman('run', matlabbatch)

    end