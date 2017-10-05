matlabbatch{1}.spm.stats.con.spmmat = {'/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis/SPM.mat'};

for i = 1:6
    matlabbatch{1}.spm.stats.con.consess{i}.tcon.name = ['MDstarttime_' int2str(2*i) '_' int2str(2*i+4)];
    matlabbatch{1}.spm.stats.con.consess{i}.tcon.weights = repmat([repmat([repmat(-0.1,1,i) repmat(0.5,1,2) repmat(-0.1,1,10-i)],1,4) zeros(1,12*7)],1,2);
    matlabbatch{1}.spm.stats.con.consess{i}.tcon.sessrep = 'none';
end

for j = 1:6
    matlabbatch{1}.spm.stats.con.consess{j+i}.tcon.name = ['MDstarttime_2timebeans_' int2str(2*j) '_' int2str(2*j+6)];
    matlabbatch{1}.spm.stats.con.consess{j+i}.tcon.weights = repmat([repmat([repmat(-10/9,1,j) repmat(10/3,1,3) repmat(-9/10,1,9-j)],1,4) zeros(1,12*7)],1,2);
    matlabbatch{1}.spm.stats.con.consess{j+i}.tcon.sessrep = 'none';
end
matlabbatch{1}.spm.stats.con.delete = 0;

spm_jobman('run', matlabbatch)
