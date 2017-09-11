
SubjectS = 2; % Start
SubjectE = 11; % End

for SubjN = SubjectS:SubjectE

    final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/tryFIR';
    final_dirin = dir(final_dir);
    final_dirin(1:2) = [];
    final_dirin(end) = [];
    target_output_dir = {}; 
    target_output_dir = [final_dir filesep final_dirin(SubjN).name];
    target_spmfile = [target_output_dir filesep 'SPM.mat'];

    load(target_spmfile);
    
    R = SPM.xX.X;
    names = cell(1:1);
     
    for con = 1:11
        for num = 1:12
                switch con 
                    case 1
                        names{1,(12*(con-1))+num} = sprintf('PROS-FIR-%d', num);
                    case 2
                        names{1,(12*(con-1))+num} = sprintf('PUR-FIR-%d', num);
                    case 3
                        names{1,(12*(con-1))+num} = sprintf('NEUT-FIR-%d', num);
                    case 4
                        names{1,(12*(con-1))+num} = sprintf('UNCO-FIR-%d', num);
                    case 5
                        names{1,(12*(con-1))+num} = sprintf('A300-FIR-%d', num);
                    case 6
                        names{1,(12*(con-1))+num} = sprintf('A50-FIR-%d', num);
                    case 7
                        names{1,(12*(con-1))+num} = sprintf('A20-FIR-%d', num);
                    case 8
                        names{1,(12*(con-1))+num} = sprintf('SAME-FIR-%d', num);
                    case 9
                        names{1,(12*(con-1))+num} = sprintf('M20-FIR-%d', num);
                    case 10
                        names{1,(12*(con-1))+num} = sprintf('M50-FIR-%d', num);
                    case 11
                        names{1,(12*(con-1))+num} = sprintf('ZERO-FIR-%d', num);
                end
        end
    end
     
    filename = [target_output_dir filesep 'covari.mat'];
    save(filename, 'R');

end