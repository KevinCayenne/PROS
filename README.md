PROS
====

 

raw data processing:
====================

-   rsync NCCUFTP to your own directory :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rsync -avhr myfile.gz /home/pi/tmp/
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-   Change the owner authority:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chown -R kevincayenne:kevincayenne
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

    1.  first_level_generate_matfile script -\> generate SOA.mat for first level
        (just do once)  
        \* before run the file:  
        1. transport the behavior.csv to the server  
        2. combine txt files into combinedtxt.txt  
        \* cat rp_aep2dmoco4mm181A\* rp_aep2dmoco4mm181B\* rp_aep2dmoco4mm181C\*
        rp_aep2dmoco4mm181D\* rp_aep2dmoco4mm181E\* rp_aep2dmoco4mm151F\* \>
        combinedtxt.txt

2.  firstlevel_FIR script -\> without estimate  
    \* % motion orthogonalize: spm_fmri_concatenate(target_output_spm, scans);

3.  covariance script -\> generate the .mat file for second level script

4.  second level script -\> generate design matrix for first level

    1.  % delete first column: SPM.xX.X(:,1) = [];

    2.  % change '19' to '1': SPM.xX.X(SPM.xX.X(:,:)==19) = 1;

5.  firstlevel model estimate script -\> model estimate for first level matrix

6.  firstlevel_contrast script -\> generate F-contrasts and T-contrast for group
    level analysis

7.  secondlevel_group_analysis -\> generate group design matrix and estiamte

8.  secondlevel_group_contrast -\> group contrast
