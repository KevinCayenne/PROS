
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
        
        for columnNum = 1:132
            ConName = ['T_test_' int2str(columnNum)];
            matlabbatch{1}.spm.stats.con.consess{(columnNum+4)}.tcon.name = ConName;
            matlabbatch{1}.spm.stats.con.consess{(columnNum+4)}.tcon.weights =[zeros(1,(columnNum-1)), ones(1,1), zeros(1,(132-columnNum))];
            matlabbatch{1}.spm.stats.con.consess{(columnNum+4)}.fcon.sessrep = 'none';
        end

        matlabbatch{1}.spm.stats.con.delete = 1;
        
        spm_jobman('run', matlabbatch)
        
        clear matlabbatch;

    end