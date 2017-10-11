counter = 1; %% Define contrast number 
timeCourseC = 11; %% Define time coureses for loop (0:11)
AllTC = 12; %% Define 12 time coureses
MDcondNum = 4; %% Define MD conditions num
EDcondNum = 7; %% Define ED conditions num
GroupNum = 2; %% Define Group conditons num

matlabbatch{1}.spm.stats.con.spmmat = {'/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis/SPM.mat'};

for Contrast_Cond = 1:6 %% contrast conditions
	for TimeC = 0:timeCourseC

		switch Contrast_Cond 

			case 1 %% All MD contrasts (ALL_MD - Baseline)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['ALLMD_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = repmat([
				    																	 repmat([
				    																	 		 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																			],1,MDcondNum) 
				    																	 zeros(1,AllTC*EDcondNum)
				    															 		],1,GroupNum);
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
			case 2 %% All ED contrasts (ALL_ED - Baseline)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['ALLED_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = repmat([
						    															 zeros(1,AllTC*MDcondNum) 
						    															 repmat([
						    															 		 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
						    																	],1,EDcondNum)
				    															 		],1,GroupNum);
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;

			%% MD Group contrasts %%

			case 3 %% All GroupMD_1 (Y_MD - O_MD)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupMD_1_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights =[repmat([
				    																	 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																	],1,MDcondNum) 
				    															 zeros(1,AllTC*EDcondNum) 
				    															 repmat([
				    															 		 repmat(1/timeCourseC,1,TimeC) repmat(-1,1,1) repmat(1/timeCourseC,1,timeCourseC-TimeC)
				    															 		],1,MDcondNum) 
				    															 zeros(1,AllTC*EDcondNum)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
		    case 4 %% All GroupMD_2 (Y_MD - Baseline)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupMD_2_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [repmat([
				    																	  repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																	 ],1,MDcondNum)
				    															  zeros(1,AllTC*EDcondNum) 
				    															  zeros(1,AllTC*MDcondNum)
				    															  zeros(1,AllTC*EDcondNum)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
			case 5 %% All GroupMD_3 (O_MD - Baseline)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupMD_3_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [zeros(1,AllTC*MDcondNum)
				    															  zeros(1,AllTC*EDcondNum) 
				    															  repmat([
				    																	  repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																	 ],1,MDcondNum)
				    															  zeros(1,AllTC*EDcondNum)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
			case 6 %% All GroupMD_4 (O_MD - Y_MD)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupMD_4_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [repmat([
				    																	 repmat(1/timeCourseC,1,TimeC) repmat(-1,1,1) repmat(1/timeCourseC,1,timeCourseC-TimeC)
				    																	],1,MDcondNum) 
				    															 zeros(1,AllTC*EDcondNum) 
				    															 repmat([
				    															 		 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    															 		],1,MDcondNum) 
				    															 zeros(1,AllTC*EDcondNum)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;

			%% ED Group contrasts %%

		    case 7 %% All GroupED_1 (Y_ED - O_ED)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupED_1_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights =[zeros(1,AllTC*MDcondNum)
				    															 repmat([
				    																	 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																	],1,EDcondNum) 
				    															 zeros(1,AllTC*MDcondNum) 
				    															 repmat([
				    															 		 repmat(1/timeCourseC,1,TimeC) repmat(-1,1,1) repmat(1/timeCourseC,1,timeCourseC-TimeC)
				    															 		],1,EDcondNum) 
				    															 ];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
		    case 8 %% All GroupED_2 (Y_ED - Baseline)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupED_2_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [zeros(1,AllTC*MDcondNum)
				    															  repmat([
				    																	  repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																	 ],1,EDcondNum)
				    															  zeros(1,AllTC*MDcondNum)
				    															  zeros(1,AllTC*EDcondNum)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
			case 9 %% All GroupED_3 (O_ED - Baseline)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupED_3_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [zeros(1,AllTC*MDcondNum)
				    															  zeros(1,AllTC*EDcondNum) 
				    															  zeros(1,AllTC*MDcondNum)
				    															  repmat([
				    																	  repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																	 ],1,EDcondNum)
				    															  ];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
			case 10 %% All GroupED_4 (O_ED - Y_ED)
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['All GroupED_4_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = [zeros(1,AllTC*MDcondNum)
				    															  repmat([
				    																	 repmat(1/timeCourseC,1,TimeC) repmat(-1,1,1) repmat(1/timeCourseC,1,timeCourseC-TimeC)
				    																	 ],1,EDcondNum) 
				    															  zeros(1,AllTC*MDcondNum) 
				    															  repmat([
				    															 		 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    															 		 ],1,EDcondNum) 
				    															 ];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;

			%% Situations contrasts %%
			case 11 %% All situations contrasts (no grouping)
				for ConCount1= 1:(MDcondNum+EDcondNum)
					matlabbatch{1}.spm.stats.con.consess{counter}.tcon.name = ['ALLMD_' int2str(TimeC+1)];
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.weights = repmat([
				    																	 repmat([
				    																	 		 repmat(-1/timeCourseC,1,TimeC) ones(1,1) repmat(-1/timeCourseC,1,timeCourseC-TimeC)
				    																			],1,MDcondNum) 
				    																	 zeros(1,AllTC*EDcondNum)
				    															 		],1,GroupNum);
				    matlabbatch{1}.spm.stats.con.consess{counter}.tcon.sessrep = 'none';
				    counter = counter + 1;
				end


	end
end

matlabbatch{1}.spm.stats.con.delete = 0;

spm_jobman('run', matlabbatch)