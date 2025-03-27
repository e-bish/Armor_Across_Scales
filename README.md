# Armor_Across_Scales

A repo to store results related to [Bishop et al. 2024](https://www.int-res.com/abstracts/meps/v750/p105-118/)

Bishop, E. E., Essington, T. E., Samhouri, J. F., Feist, B. E., Sullaway, G. H., Toft, J. D., & Francis, T. B. (2024). Nearshore fish abundance in an urban estuary is weakly associated with shoreline conditions across spatial scales. Marine Ecology Progress Series, 750, 105-118.

## Abstract
Understanding the scale at which species respond to habitat characteristics can improve detection of effects from anthropogenic alterations to marine shorelines. Spatial context matters because habitat alteration may have more of an effect when prevalent throughout a region, depending on the spatial scale at which species experience changes in habitat value. In this study, we examined the associations of 4 highly motile fish species with shorelines that are altered via shoreline armor. We sampled fish for 4 yr in the Salish Sea, Washington, USA, and used model selection to evaluate the weight of evidence for associations between fish abundance and shoreline armor extent within hierarchically nested spatial scales. We evaluated species that inhabit nearshore waters under different contexts during their life histories because we expected different associations with armor between them. Of the species that are beach-associated at multiple times during their life histories, surf smelt showed no association with armor while Pacific herring showed a negative association, particularly at larger spatial scales. Of the species that pass through nearshore habitats during outmigration to the ocean, juvenile Chinook and chum salmon both showed slightly positive associations with armor presence, but these associations were not strong. The stronger association we observed in herring suggests that they may avoid areas with greater armor presence throughout a region. Distributions of anadromous species during migration are likely governed by other factors. This work highlights the importance of maintaining ecological context when considering future research aimed at understanding the impacts of shoreline armor on fish populations.

## File structure

This repository is divided into three folders:

* `/analysis`: contains R scripts used to generate results and figures
    
* `/data`: contains data used in the analysis R scripts
    
* `/figures`: contains image files generated in the analysis R scripts

To replicate the results of this analysis, run the scripts in the analysis folder in numeric order. The purpose of each script is as follows:

**01_perc_armor_calc** - loads shapefiles to calculate the percent shoreline armor within different radii of fish sampling locations

**02_tidy_data** - loads raw data files, conducts some simple data cleaning, saves an Rdata object for use in other scripts

**03_model_selection** - lists alternative models and compares results using AICc model selection

**04_compare_scales** - compares the estimated coefficient for percent armor in the full model across different spatial scales (also contains code to create Fig 4)

**05_model_diagnostics** - runs diagnostics for the model with the lowest AICc value for each species

There are also scripts to generate figures within the manuscript, labeled with the prefix "Fig". Finally, there is a script to generate the numbers used to describe the percent of shoreline armor that salmon might encounter on the east side of the US waters of the Salish Sea as used in an example in the discussion. 

The 01_perc_armor_calc file uses data from the Beach Strategies database. Additional information about this data source and links to download the data directly can be found [here](https://beach-strategies-wdfw-hub.hub.arcgis.com/). 
