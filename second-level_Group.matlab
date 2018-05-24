
dirname = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis';

matlabbatch{1}.spm.stats.factorial_design.dir = {dirname};
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).name = 'Group';
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).levels = 2;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).dept = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).variance = 1;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).ancova = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).name = 'Situation';
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).levels = 32;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).dept = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).variance = 1;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).ancova = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(3).name = 'TimeCourse';
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(3).levels = 12;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(3).dept = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(3).variance = 1;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(3).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(3).ancova = 0;

%%
group_num = 2;
situation_num = 32;
time_points = 12;

for group = 1:group_num
    for situation = 1:situation_num
        for time_course = 1:time_points
                matlabbatch{1}.spm.stats.factorial_design.des.fd.icell(situation_num*time_points*(group-1)+(situation-1)*time_points+time_course).levels = [group situation time_course];
                
                %% change the subject's number
                if group == 1 %% Young 
                    SubjectN = [2, 3, 5, 6, 7, 8, 11, 14, 18, 19, 20, 21, 22, 23, 24, 28, 29, 31, 32, 33, 41, 45, 46, 47, 48];
                    run = length(SubjectN);
                elseif group == 2 %% Old
                    SubjectN = [4, 9, 12, 15, 16, 17, 25, 26, 27, 34, 36, 40, 42, 43, 44, 50];
                    run = length(SubjectN);
                end
                
                target_confile = {};
                for SubjN = SubjectN
                    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR/first_level_fir';
                    final_dirin = dir(final_dir);
                    final_dirin(1:2) = [];
                    target_dir = [final_dir filesep final_dirin(SubjN).name];
                    target_dirin = dir(target_dir);
                    target_confile = [target_confile; target_dir filesep target_dirin(401+(situation-1)*time_points+time_course).name]; %#ok<AGROW>
                end
            matlabbatch{1}.spm.stats.factorial_design.des.fd.icell(situation_num*time_points*(group-1)+(situation-1)*time_points+time_course).scans = target_confile;
        end
    end
end

matlabbatch{1}.spm.stats.factorial_design.des.fd.contrasts = 1;
matlabbatch{1}.spm.stats.factorial_design.cov = struct('c', {}, 'cname', {}, 'iCFI', {}, 'iCC', {});
matlabbatch{1}.spm.stats.factorial_design.multi_cov = struct('files', {}, 'iCFI', {}, 'iCC', {});
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

%% run estimate
matlabbatch{2}.spm.stats.fmri_est.spmmat = {'/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis/SPM.mat'};
matlabbatch{2}.spm.stats.fmri_est.write_residuals = 0;
matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;

spm_jobman('run', matlabbatch)

clear matlabbatch;