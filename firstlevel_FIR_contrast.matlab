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
        
        matlabbatch{1}.spm.stats.con.spmmat = {target_output_spm};
        
        matlabbatch{1}.spm.stats.con.consess{1}.fcon.name = 'PROSOCIAL_F';
        matlabbatch{1}.spm.stats.con.consess{1}.fcon.weights = [eye(12, 12)];
        matlabbatch{1}.spm.stats.con.consess{1}.fcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{2}.fcon.name = 'PURCHASE_F';
        matlabbatch{1}.spm.stats.con.consess{2}.fcon.weights = [zeros(12,12), eye(12,12)];
        matlabbatch{1}.spm.stats.con.consess{2}.fcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{3}.fcon.name = 'NEUTRAL_F';
        matlabbatch{1}.spm.stats.con.consess{3}.fcon.weights = [zeros(12,24), eye(12,12)];
        matlabbatch{1}.spm.stats.con.consess{3}.fcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{4}.fcon.name = 'UNCOMMON_F';
        matlabbatch{1}.spm.stats.con.consess{4}.fcon.weights = [zeros(12,36), eye(12,12)];
        matlabbatch{1}.spm.stats.con.consess{4}.fcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{5}.tcon.name = 'PROSOCIAL_T';
        matlabbatch{1}.spm.stats.con.consess{5}.tcon.weights =[ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{5}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{6}.tcon.name = 'PURCHASE_T';
        matlabbatch{1}.spm.stats.con.consess{6}.tcon.weights =[zeros(1,12), ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{6}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{7}.tcon.name = 'NEUTRAL_T';
        matlabbatch{1}.spm.stats.con.consess{7}.tcon.weights =[zeros(1,24), ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{7}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{8}.tcon.name = 'UNCOMMON_T';
        matlabbatch{1}.spm.stats.con.consess{8}.tcon.weights =[zeros(1,36), ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{8}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{9}.tcon.name = 'PROSOCIAL_T-PURCHASE_T';
        matlabbatch{1}.spm.stats.con.consess{9}.tcon.weights =[ones(1,12), -ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{9}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{10}.tcon.name = 'PROSOCIAL_T-NEUTRAL_T';
        matlabbatch{1}.spm.stats.con.consess{10}.tcon.weights =[ones(1,12), zeros(1,12), -ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{10}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{11}.tcon.name = 'PROSOCIAL_T-UNCOMMON_T';
        matlabbatch{1}.spm.stats.con.consess{11}.tcon.weights =[ones(1,12), zeros(1,24), -ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{11}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{12}.tcon.name = 'PURCHASE_T-NEUTRAL_T';
        matlabbatch{1}.spm.stats.con.consess{12}.tcon.weights =[zeros(1,12), ones(1,12), -ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{12}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{13}.tcon.name = 'PURCHASE_T-UNCOMMON_T';
        matlabbatch{1}.spm.stats.con.consess{13}.tcon.weights =[zeros(1,12), ones(1,12), zeros(1,12), -ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{13}.tcon.sessrep = 'none';
        matlabbatch{1}.spm.stats.con.consess{14}.tcon.name = 'UNCOMMON_T-NEUTRAL_T';
        matlabbatch{1}.spm.stats.con.consess{14}.tcon.weights =[zeros(1,24), -ones(1,12), ones(1,12)];
        matlabbatch{1}.spm.stats.con.consess{14}.tcon.sessrep = 'none';
        
        matlabbatch{1}.spm.stats.con.delete = 1;
        
        spm_jobman('run', matlabbatch)

    end