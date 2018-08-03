
    SubjectS = 36; % Start
    SubjectE = 36; % End
    colnum = 132;
     temp1 = [3, 5, 6, 7, 8, 11, 14, 18, 19, 20, 21, 22, 23, 24, 28, 29, 31, 32, 33, 4, 9, 12, 15, 16, 17, 25, 26, 27, 34, 36, 40, 41, 42, 43, 44, 45, 46, 47, 48, 50];
     
    for SubjN = temp1

        final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR/first_level_fir';
        final_dirin = dir(final_dir);
        final_dirin(1:2) = [];
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
        
        for columnNum = 1:colnum
            ConName = ['T_test_' int2str(columnNum)];
            matlabbatch{1}.spm.stats.con.consess{(columnNum+4)}.tcon.name = ConName;
            matlabbatch{1}.spm.stats.con.consess{(columnNum+4)}.tcon.weights =[zeros(1,(columnNum-1)), ones(1,1), zeros(1,(colnum-columnNum))];
            matlabbatch{1}.spm.stats.con.consess{(columnNum+4)}.fcon.sessrep = 'none';
        end

        matlabbatch{1}.spm.stats.con.delete = 1;
        
        spm_jobman('run', matlabbatch)
        
        clear matlabbatch;

    end