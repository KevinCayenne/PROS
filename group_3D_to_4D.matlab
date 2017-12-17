final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis';
final_dirin = dir(final_dir);
ContConNum = 89;
totalTcon = ContConNum*12;

for conditionC = 1:ContConNum
    
    targetdir = {};
    
    for TimeCount = 1:12
        targetdir = [targetdir; [final_dir filesep final_dirin(end-(totalTcon-TimeCount)).name ',1']]; %#ok<AGROW>
    end
    
    totalTcon = totalTcon - 12;
    %%
    matlabbatch{1}.spm.util.cat.vols = targetdir;
    %%
    matlabbatch{1}.spm.util.cat.name = [int2str(conditionC) '_4D.nii'];
    matlabbatch{1}.spm.util.cat.dtype = 0;
    
    spm_jobman('run', matlabbatch)
end

