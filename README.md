Attached are the r files and csvs used to get up to this point in the project. A quick summary of each r project:

Sightings
1. sighting_rates_mei_gh.R: Calculating and analyzing sighting rates for beaked whale species in relation to enso values
2. FigS1_justgam_sightingmei_gh.R: running Generalized Additive Models on the beaked whale sighting rate data

Tags
1. Adding_mei_pdo_npgo_values_tags_gh.R: Adding mei, pdo, and npgo values to beaked whale tags
2. tag_displacement_gh.R: Calculating displacement between tag points for beaked whale data
3. homerange_ratio_gh.R: Calculating range-use ratios for Cuvier's beaked whale data


CSVs (if any are missing from the r projects, please let me know):
1. Allsightings_biasremoved_monthsbelow40hrHIadjusted_beaked: All beaked whale sightings, but months at HI with less 
than 40 hours of effort have been combined if possible. Points with bias have also been removed. 
2. coastline files: coastline shapefiles for plotting
3. Effort_thruNov2022_GIS_2023MAY_Enso_comb_beaked: Effort file with months at HI similarly combined as mentioned above
4. hawaii_effort_beakedwhale_sightings_MEI_values_v1_MAY23: excel workbook with sheets of information and r data
5. MdTags_4hr_use_seafloor_geomorph_oceangographic_var_enso_MAY23_v1: Blainville's tags with all enso columns, 
oceanographic variables, etc.
6. 022_DouglasFiltered_r10d3lc2_ArgosGPS_2022DECv1.csv: Blainville's douglas filtered, original location tag data
7. mei_values_csv: mei values by month and year
8. pdo_period.csv: pdo period by month and year
9. pdo_value_monthly: pdo value by month and year
10. NPGO_values_monthly.csv: npgo values and period by month and year
11. ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1: Cuvier's tags with all enso columns, 
oceanographic variables, etc.
12. ZcTag002-044_DouglasFiltered_KS_r10d3lc2_2020MAYv2.csv: Cuvier's douglas filtered, original location tag data
13. hawaii_sighting_rates_by_consecutive_times.csv: monthly sighting rate csv for both species, with shorter consecutive periods combined
14. deployment_duration.csv: lists the deployment duration for each beaked whale tag


- Danny
