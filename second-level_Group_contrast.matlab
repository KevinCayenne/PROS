
counter = 1; %% Define contrast number 
timeCourseC = 11; %% Define time coureses for loop (0:11)
AllTC = 12; %% Define 12 time coureses
MDcondNum = 4; %% Define MD conditions num
EDcondNum = 11; %% Define ED conditions num
GroupNum = 2; %% Define Group conditons num

matlabbatch{1}.spm.stats.con.spmmat = {'/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis/SPM.mat'};

for Contrast_Cond = 1:6 %% contrast conditions

	switch Contrast_Cond 
		case 1 %% All MD contrasts
			for AllMDcount = 0:timeCourseC
			    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['ALLMD_' int2str(AllMDcount+1)];
			    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = repmat([repmat([repmat(-1/timeCourseC,1,AllMDcount) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-AllMDcount)],1,4) zeros(1,timeCourseC*7)],1,2);
			    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
			    counter = counter + 1;
			end
		case 2 %% All ED contrasts
			for AllEDcount = 0:timeCourseC
			    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['Group_MDstarttime_' int2str(AllEDcount+1)];
			    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [repmat([repmat(-1/11,1,AllEDcount) ones(1,1) repmat(-1/11,1,11-AllEDcount)],1,4) zeros(1,12*7) repmat([repmat(1/11,1,AllEDcount) repmat(-1,1,1) repmat(1/11,1,11-AllEDcount)],1,4) zeros(1,12*7)];
			    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
			    counter = counter + 1;
			end            
		case 3

	end

end

matlabbatch{1}.spm.stats.con.delete = 0;

spm_jobman('run', matlabbatch)