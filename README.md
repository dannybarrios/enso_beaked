Attached are the R files and CSVs used throughout this project. A quick summary of each r project:

Sightings
1. calculatesightingrates_univariatecomparisons_gh.R: Calculating and analyzing sighting rates for beaked whale species in relation to ENSO, PDO, NPGO, and seasonal values. Additionally, those indices are compared with sea states. Univariate comparisons are used, alongside figures and summary statistics. 
2. groupsize_comparisons_gh.R: Comparing group size statistics between beaked whales. 
3. sightingrates_gam_gh: running Generalized Additive Models on the beaked whale sighting rate data. Residual plots are created at the end. 

Tags
1. Adding_mei_pdo_npgo_values_tags_gh.R: Adding mei, pdo, and npgo values to beaked whale tags
2. calculate_rangeuseratios_gh.R: Creating range use ratios for tagged goose-beaked whales. 
3. rangeuseratios_stats_gh.R: Summary and formal analysis of range use ratios for goose-beaked whales ~ ENSO and season


CSVs (if any are missing from the r projects, please let me know):
1. Allsightings_biasremoved_perbelow30hradj_oneffort_375m_HI_Island.csv: All beaked whale sightings from HI, but periods with less 
than 30 hours of effort have been combined if possible. Points with bias have also been removed, as well as any time spent off efort and shallower than 375 m. 
2. coastline files: coastline shapefiles for plotting
3. Effort_Oct2022_GIS_Enso_comb_beaked_oneffort_375m_HI_Island.csv: Effort file with months at HI similarly combined as mentioned above
4. MdTags_4hr_use_seafloor_geomorph_oceangographic_var_enso_MAY23_v1: Blainville's tags
5. ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv: goose-beaked whale tags
7. mei_values_csv: mei values by month and year
8. mei_values_csv_long.csv: mei monthly/yearly values in long format
9. pdo_value_monthly_long.csv: pdo period by month and year, in long format
11. NPGO_values_monthly.csv: npgo values and period by month and year
14. hawaii_sighting_rates_by_consecutive_times_justsearching.csv: monthly sighting rate csv for both species, with shorter consecutive periods combined. Only on effort. 
15. ZcTags_weekly_rangeuseratios_2024Aug07.csv: Goose-beaked whale range use ratios, as calculated from calculate_rangeuseratios_gh.R. 

- Danny
