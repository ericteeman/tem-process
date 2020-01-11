# tem-process

For use in determining the size and size distribution of superparamagnetic nanoparticles using Fiji and psa_r12 macro.

Size and size distribution are determined from a log-normal fit of the compiled histrograms of sizes exported from the psa_r12 macro.

ParticleSizeAnalyzer Optimized Settings
- Assuming proper calibration (set scale) has already been completed
- Settings for spherical nanoaparticles with expected average sizes from 20-30 nm
- Interactive scaling possible, but much slower

Help with scaling (interactive)     NONE
Background removal                  Rolling Ball (r = 50 px)
Smoothing filter                    Median r = 3 (7x7)
Thresholding mode                   Manual (interactive)
Separation of touching particles    Watershed filter
Minimum diameter                    10
Maximum diameter                    50
Minimum circularity                 0.850
Maximum circularity                 1
