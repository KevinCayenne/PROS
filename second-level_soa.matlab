SubjectS = 41; % Start
SubjectE = 45; % End
temp1 = [2, 3, 5, 6, 7, 8, 11, 14, 18, 19, 20, 21, 22, 23, 24, 28, 29, 31, 32, 33, 4, 9, 12, 15, 16, 17, 25, 26, 27, 34, 36, 40, 41, 42, 43 , 44, 45, 46, 47, 48, 50];
%% 

for SubjN = temp1

    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
    final_dirin = dir(final_dir);
    final_dirin(1:3) = [];
    final_dirin(end) = [];
    target_output_dir = {}; 
    target_output_dir = [final_dir filesep final_dirin(SubjN).name];
    target_spmfile = [target_output_dir filesep 'SPM.mat'];

    load(target_spmfile);
    
    R = SPM.xX.X;
    names = cell(1:1);
     
    filename = [target_output_dir filesep 'covari.mat'];
    save(filename, 'R');

end