
SubjectS = 46; % Start
SubjectE = 48; % End
    
%% 

for SubjN = 40

    dirname = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data';
    dirinfo = dir(dirname); % define file list
    dirinfo(1:2) = []; % delete the first two filenames
    dirinfo(end) = []; % delete the last one filenames
    targetEPI = {}; % define targetEPI
    EPIra = {}; 

    dirin = dir([dirname filesep dirinfo(SubjN).name]); % define file list under the directory
    dirin(1:2) = []; % delete the first two filenames

    targetdir = {};
    
    if SubjN < 10
        targetdir = [targetdir, [dirname filesep 'PROS-Pilot-0' num2str(SubjN) filesep]];  %#ok<AGROW>
    else
        targetdir = [targetdir, [dirname filesep 'PROS-Pilot-' num2str(SubjN) filesep]];  %#ok<AGROW>
    end

    cd(char(targetdir));
    EPITag = 'A':'F';

        for run = 1:6
            EPIpath = {};
                if run <= 5
                    EPIfile = dir(strcat('ep2dmoco4mm181*', EPITag(run), '*a001.nii'));
                    for k = 1:181
                    EPIpath = [EPIpath; strcat(targetdir, EPIfile.name, ',', num2str(k))];
                    targetEPI{run} = EPIpath;
                    end
                else
                    EPIfile = dir(strcat('ep2dmoco4mm151*', EPITag(6), '*a001.nii'));
                    for i = 1:151
                    EPIpath = [EPIpath; strcat(targetdir, EPIfile.name, ',', num2str(i))];
                    targetEPI{run} = EPIpath;
                    end
                end
        end

    T1file = dir('MPRAGE*.nii');
    targetT1 = strcat(targetdir, T1file.name);

    T2file = dir('t2tsetra*.nii');
    targetT2 = strcat(targetdir, T2file.name);

    % Flowfield = dir('u_rc*.nii');
    % targetFF = strcat(targetdir, filesep, Flowfield.name);

    cd(char(dirname))
    %load('preprocessing_0804_1.mat')
    
    %% start preprocessing  
    %% slice timming
    matlabbatch{1}.spm.temporal.st.scans = {
                                        targetEPI{1,1}
                                        targetEPI{1,2}
                                        targetEPI{1,3}
                                        targetEPI{1,4}
                                        targetEPI{1,5}
                                        targetEPI{1,6}
                                       };
    matlabbatch{1}.spm.temporal.st.nslices = 38;
    matlabbatch{1}.spm.temporal.st.tr = 2;
    matlabbatch{1}.spm.temporal.st.ta = 1.94736842105263;
    matlabbatch{1}.spm.temporal.st.so = [2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37];
    matlabbatch{1}.spm.temporal.st.refslice = 1;
    matlabbatch{1}.spm.temporal.st.prefix = 'a';                               
    
    %% 
    
    matlabbatch{2}.spm.spatial.realign.estimate.data{1}(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 1)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{1}, '.','files'));
    matlabbatch{2}.spm.spatial.realign.estimate.data{2}(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 2)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{2}, '.','files'));
    matlabbatch{2}.spm.spatial.realign.estimate.data{3}(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 3)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{3}, '.','files'));
    matlabbatch{2}.spm.spatial.realign.estimate.data{4}(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 4)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{4}, '.','files'));
    matlabbatch{2}.spm.spatial.realign.estimate.data{5}(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 5)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{5}, '.','files'));
    matlabbatch{2}.spm.spatial.realign.estimate.data{6}(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 6)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{6}, '.','files'));
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.quality = 0.9;
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.sep = 4;
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.fwhm = 5;
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.rtm = 1;
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.interp = 2;
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.wrap = [0 0 0];
    matlabbatch{2}.spm.spatial.realign.estimate.eoptions.weight = '';
    
    %%
    
    matlabbatch{3}.spm.spatial.coreg.estimate.ref = targetEPI{1,1}(1);
    matlabbatch{3}.spm.spatial.coreg.estimate.source = targetT2;
    matlabbatch{3}.spm.spatial.coreg.estimate.other = {''};
    matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.cost_fun = 'nmi';
    matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.sep = [4 2];
    matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.tol = [0.02 0.02 0.02 0.001 0.001 0.001 0.01 0.01 0.01 0.001 0.001 0.001];
    matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.fwhm = [7 7];
    
    %%
    
    matlabbatch{4}.spm.spatial.coreg.estimate.ref = targetT1;
    matlabbatch{4}.spm.spatial.coreg.estimate.source = targetT2;
    matlabbatch{4}.spm.spatial.coreg.estimate.other(1) = cfg_dep('Realign: Estimate: Realigned Images (Sess 1)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{1}, '.','cfiles'));
    matlabbatch{4}.spm.spatial.coreg.estimate.other(2) = cfg_dep('Realign: Estimate: Realigned Images (Sess 2)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{2}, '.','cfiles'));
    matlabbatch{4}.spm.spatial.coreg.estimate.other(3) = cfg_dep('Realign: Estimate: Realigned Images (Sess 3)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{3}, '.','cfiles'));
    matlabbatch{4}.spm.spatial.coreg.estimate.other(4) = cfg_dep('Realign: Estimate: Realigned Images (Sess 4)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{4}, '.','cfiles'));
    matlabbatch{4}.spm.spatial.coreg.estimate.other(5) = cfg_dep('Realign: Estimate: Realigned Images (Sess 5)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{5}, '.','cfiles'));
    matlabbatch{4}.spm.spatial.coreg.estimate.other(6) = cfg_dep('Realign: Estimate: Realigned Images (Sess 6)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{6}, '.','cfiles'));
    matlabbatch{4}.spm.spatial.coreg.estimate.eoptions.cost_fun = 'nmi';
    matlabbatch{4}.spm.spatial.coreg.estimate.eoptions.sep = [4 2];
    matlabbatch{4}.spm.spatial.coreg.estimate.eoptions.tol = [0.02 0.02 0.02 0.001 0.001 0.001 0.01 0.01 0.01 0.001 0.001 0.001];
    matlabbatch{4}.spm.spatial.coreg.estimate.eoptions.fwhm = [7 7];
    
    
    %%
    
    matlabbatch{5}.spm.spatial.normalise.estwrite.subj.vol = targetT1;
    matlabbatch{5}.spm.spatial.normalise.estwrite.subj.resample = targetT1;
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.biasreg = 0.0001;
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.biasfwhm = 60;
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.tpm = {'/usr/local/spm12/tpm/TPM.nii'};
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.affreg = 'eastern';
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.reg = [0 0.001 0.5 0.05 0.2];
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.fwhm = 0;
    matlabbatch{5}.spm.spatial.normalise.estwrite.eoptions.samp = 3;
    matlabbatch{5}.spm.spatial.normalise.estwrite.woptions.bb = [-78 -112 -70 78 76 85];
    matlabbatch{5}.spm.spatial.normalise.estwrite.woptions.vox = [3 3 3];
    matlabbatch{5}.spm.spatial.normalise.estwrite.woptions.interp = 4;
    matlabbatch{5}.spm.spatial.normalise.estwrite.woptions.prefix = 'w';
    
    %%
    
    matlabbatch{6}.spm.spatial.normalise.write.subj.def(1) = cfg_dep('Normalise: Estimate & Write: Deformation (Subj 1)', substruct('.','val', '{}',{5}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{1}, '.','def'));
    matlabbatch{6}.spm.spatial.normalise.write.subj.resample(1) = cfg_dep('Coregister: Estimate: Coregistered Images', substruct('.','val', '{}',{4}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','cfiles'));
    matlabbatch{6}.spm.spatial.normalise.write.woptions.bb = [-78 -112 -70
                                                              78 76 85];
    matlabbatch{6}.spm.spatial.normalise.write.woptions.vox = [3 3 3];
    matlabbatch{6}.spm.spatial.normalise.write.woptions.interp = 4;
    matlabbatch{6}.spm.spatial.normalise.write.woptions.prefix = 'w';

    %% 
    
    matlabbatch{7}.spm.spatial.smooth.data(1) = cfg_dep('Normalise: Write: Normalised Images (Subj 1)', substruct('.','val', '{}',{6}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{1}, '.','files'));
    matlabbatch{7}.spm.spatial.smooth.fwhm = [8 8 8];
    matlabbatch{7}.spm.spatial.smooth.dtype = 0;
    matlabbatch{7}.spm.spatial.smooth.im = 0;
    matlabbatch{7}.spm.spatial.smooth.prefix = 's';
    
    %%
    spm_jobman('run', matlabbatch)
    
end
