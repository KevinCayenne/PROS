
dirname = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis';

matlabbatch{1}.spm.stats.factorial_design.dir = {dirname};
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).name = 'Group';
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).levels = 2;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).dept = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).variance = 1;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(1).ancova = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).name = 'Situation';
matlabbatch{1}.spm.stats.factorial_design.des.fd.fact(2).levels = 11;
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
for group = 1:2
    for situation = 1:11
        for time_course = 1:12 
                matlabbatch{1}.spm.stats.factorial_design.des.fd.icell(132*(group-1)+12*(situation-1)+time_course).levels = [group situation time_course];
                
                if group == 1 
                    SubjectN = [2, 3, 5, 6, 7, 8, 10, 11];
                    run = length(SubjectN);
                elseif group == 2
                    SubjectN = [4, 9, 12];
                    run = length(SubjectN);
                end
                
                target_confile = {};
                for SubjN = SubjectN
                    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
                    final_dirin = dir(final_dir);
                    final_dirin(1:2) = [];
                    final_dirin(end-2:end) = [];
                    target_dir = [final_dir filesep final_dirin(SubjN).name];
                    target_dirin = dir(target_dir);
                    target_confile = [target_confile; target_dir filesep target_dirin(150+(situation-1)*12+time_course).name];
                end
                
                matlabbatch{1}.spm.stats.factorial_design.des.fd.icell(132*(group-1)+12*(situation-1)+time_course).scans = target_confile;
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

spm_jobman('run', matlabbatch)

clear matlabbatch;