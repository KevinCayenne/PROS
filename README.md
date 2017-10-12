# PROS


# behavior analysis procedure:
	
	data_merge
	
# fMRI analysis procedure:

	1. preprocessing script
	2. first_level_generate_matfile script-> generate SOA.mat for first level (just do once)
	3. firstlevel_FIR script ->  without estimate

		1. % motion orthogonalize:  spm_fmri_concatenate(target_output_spm, scans); 
	4. covariance script -> generate the .mat file for second level script 
	5. second level script -> generate design matrix for first level

		1. % delete first column:   SPM.xX.X(:,1) = [];
		2. % change '19' to '1':      SPM.xX.X(SPM.xX.X(:,:)==19) = 1;
	6. firstlevel model estimate script -> model estimate for first level matrix
	7. firstlevel_contrast script -> generate F-contrasts and T-contrast
	8. secondlevel_group_analysis -> generate group design matrix
	9. grouplevel model estimate script -> model estimate for group matrix
	10. secondlevel_group_contrast -> group contrast





	
	

