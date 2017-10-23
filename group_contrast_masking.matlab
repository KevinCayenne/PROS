final_dir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/seconlevel_analysis';
maskmni = '/bml/Data/Bank5/PROS/Pilot_image/MNI_template/rMNI152_T1_1mm.nii,1';
outputdir = '/bml/Data/Bank5/PROS/Pilot_image/Convert_data/first_level_matrix/masked_image';
final_dirin = dir(final_dir);

for
	matlabbatch{1}.spm.util.imcalc.input = ;
	matlabbatch{1}.spm.util.imcalc.output = 'output';
	matlabbatch{1}.spm.util.imcalc.outdir = outputdir;
	matlabbatch{1}.spm.util.imcalc.expression = 'i2.*(i1> 2500)';
	matlabbatch{1}.spm.util.imcalc.var = struct('name', {}, 'value', {});
	matlabbatch{1}.spm.util.imcalc.options.dmtx = 0;
	matlabbatch{1}.spm.util.imcalc.options.mask = 0;
	matlabbatch{1}.spm.util.imcalc.options.interp = 1;
	matlabbatch{1}.spm.util.imcalc.options.dtype = 4;

	spm_jobman('run', matlabbatch)
end