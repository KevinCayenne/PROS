PROS
====

PROS scripts are the Matlab and R scripts for the analysis of PROS(Age differences in prosocial decision making) project 2017 Z.Y.C. . Matlab for brain image analysis(using SPM12 package), and R for behavioral analysis. 

raw data processing:
====================

-   Sign into the root

-   Rsync NCCUFTP to your own directory :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rsync -avhr myfile.gz /home/pi/tmp/
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-   Change the owner authority:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chown -R kevincayenne:kevincayenne
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-   Exit the root

-   Creat the directories: 
    
    1. Converted_files 
    2. tryFIR 

-   Unzip nii files:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dcm2nii -d N -g N -o /bml/Data/Bank5/PROS//Pilot_image/Convert_data/PROS-Pilot-4 /bml/Data/Bank5/PROS/Pilot_image/RAW_image/PROS-PILOT-4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

behavior analysis procedure:
============================

-   Behavior analysis R file:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_merge
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

spm fMRI analysis procedure:
============================

1.  preprocessing script

2.  first_level_generate_matfile script -\> generate SOA.mat for first level
    (just do once)  
    \* before run the file:

    1.  transport the behavior.csv to the server  
        

    2.  combine txt files into combinedtxt.txt  
        \* cat rp_aep2dmoco4mm181A\* rp_aep2dmoco4mm181B\* rp_aep2dmoco4mm181C\*
        rp_aep2dmoco4mm181D\* rp_aep2dmoco4mm181E\* rp_aep2dmoco4mm151F\* \>
        combinedtxt.txt

3.  firstlevel_FIR script -\> without estimate  
    \* % motion orthogonalize: spm_fmri_concatenate(target_output_spm, scans);

4.  covariance script -\> generate the .mat file for second level script

5.  second level script -\> generate design matrix for first level

    1.  % delete first column: SPM.xX.X(:,1) = [];

    2.  % change '19' to '1': SPM.xX.X(SPM.xX.X(:,:)==19) = 1;

6.  firstlevel model estimate script -\> model estimate for first level matrix

7.  firstlevel_contrast script -\> generate F-contrasts and T-contrast for group
    level analysis

8.  secondlevel_group_analysis -\> generate group design matrix and estiamte

9.  secondlevel_group_contrast -\> group contrast

10. group_contrast_masking -\> add mask to each Tcon file

11. group_3D_to_4D -\> combine 3D 12 time courses contrast to 4D contrast
