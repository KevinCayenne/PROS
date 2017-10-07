matlabbatch{1}.spm.stats.con.spmmat = {'/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis/SPM.mat'};

for i = 0:11
    matlabbatch{1}.spm.stats.con.consess{i+1}.tcon.name = ['MDstarttime_' int2str(2*i+1)];
    matlabbatch{1}.spm.stats.con.consess{i+1}.tcon.weights = repmat([repmat([repmat(-1/11,1,i) ones(1,1) repmat(-1/11,1,11-i)],1,4) zeros(1,12*7)],1,2);
    matlabbatch{1}.spm.stats.con.consess{i+1}.tcon.sessrep = 'none';
end

matlabbatch{1}.spm.stats.con.delete = 0;

spm_jobman('run', matlabbatch)
