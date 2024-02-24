# Armor_across_scales
Materials to produce results for "Nearshore fish abundance in an urban estuary is weakly associated with shoreline conditions across scales" by Emily E. Bishop, Timothy E. Essington, Jameal F. Samhouri, Blake E. Feist, Genoa H. Sullaway, Jason D. Toft, and Tessa B. Francis. 

This repository is divided into three folders:

/analysis
  - contains R scripts used to generate results and figures
/data
  - contains data used in the analysis R scripts
/figures
  - contains image files generated in the analysis R scripts

To replicate the results of this analysis, run the scripts in the analysis folder in numeric order. The purpose of each script is as follows:

**01_tidy_data** - loads raw data files, conducts some simple data cleaning, saves an Rdata object for use in other scripts
**02_perc_armor_calc** - loads shapefiles to calculate the percent shoreline armor within different radii of fish sampling locations
**03_model_selection** - lists alternative models and compares results using AICc model selection
**04_compare_scales** - compares the estimated coefficient for percent armor in the full model across different spatial scales (also contains code to create Fig 4)
**05_model_diagnostics** - runs diagnostics for the model with the lowest AICc value for each species

There are also scripts to generate figures within the manuscript, labeled with the prefix "Fig". Finally, there is a script to generate the numbers used to describe the percent of shoreline armor that salmon might encounter on the east side of the US waters of the Salish Sea as used in an example in the discussion. 

