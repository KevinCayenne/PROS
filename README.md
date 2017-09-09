# PROS

#fMRI analysis procedure:


	1. preprocessing script
	2. first_level_generate_matfile script-> generate SOA.mat for first level (just do once)
	3. firstlevel_FIR script ->  without estimate
	4. covariance script -> generate the .mat file for second level script 
	5. second level FIR script -> generate the proper design matrix for contrast
	6. firstlevel_estimate_contrast script -> estimate and generate F-contrast

