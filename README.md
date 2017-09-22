# PROS


# behavior analysis procedure:
	
	data_merge
	
# fMRI analysis procedure:

	1. preprocessing script
	2. first_level_generate_matfile script-> generate SOA.mat for first level (just do once)
	3. firstlevel_FIR script ->  without estimate
	4. covariance script -> generate the .mat file for second level script 
	5. second level script -> generate the proper design matrix for contrast
	6. firstlevel_estimate script -> estimate
	7. firstlevel_contrast script -> generate F-contrasts and T-contrast
	8. secondlevel_group_analysis -> group analysis


	
	

