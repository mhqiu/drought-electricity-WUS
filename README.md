# Drought impacts on the electricity system, emissions, and air quality in the western US
Replication materials for Qiu et al. (2023) 'Drought impacts on the electricity system, emissions, and air quality in the western US'

The materials in this repository allow users to reproduce the main results and figures of the paper.

If you have questions or suggestions, please contact Minghao Qiu at mhqiu@stanford.edu.

## Organization of repository
- Scripts/analysis: scripts for calculation and analysis.
- Scripts/figures: scripts for producing the figures.
- Data/input: data inputs for analysis.
- Data/intermediate: intermediate data/results that are generated with scripts in "scripts/analysis" and used for producing the figures.

## Instructions
The analysis is performed using R (version 4.3.0). Packages needed are listed at the beginning of each script. 

### Scripts/analysis:

- *1_reg_runoff_generation.R*

Used to perform the regressions to estimate the impacts of runoff anomalies on fossil fuel generation. This script will perform two sets of regressions: 1) estimating the pooled regression by regions; and 2) estimating separate regressions for each BA-fuel pair (i.e. only using plants that are from the same Balancing Area region and using the same fuel type).

- *2_historical_damages.R*

Used to calculate the historical drought-induced emissions from each plant, and the damages of drought-induced fossil fuel emissions accounting for CO2, CH4 leakage, and PM2.5-related mortalities.

- *3_reg_air_quality.R*

Used to perform the regressions to estimate the impacts of drought-induced emissions on measured PM2.5 concentration from EPA monitors. 

- *4_future_damages.R*

Used to calculate the future damages of drought-induced fossil fuel emissions, under different climate and electricity sector scenarios.

### Scripts/figures:

scripts for producing Figure 1-5 in the main paper. 

Note for figure1.R:
The shapefile of the eGRID subregion is too large to fit in the github repo. Please access the shapefile with the following dropbox link. Download the folder and put it under ~/Data/input

https://www.dropbox.com/sh/l7kkcabox41umjt/AABaL-jcca3FQFM6ZQOdobWHa?dl=0

### Data/input

- *runoff_generation_month_final.rds*

The primary dataset to perform the regressions to estimate runoff impacts on fossil fuel generation.


- *runoff_month_1980_2021.rds*

The runoff anomalies from 1980 - 2021. Used to produce Figure 1.

- *region_monthly_covariates.rds* 

The regional and monthly level covariates used for regression analysis and various analyses. Covariates include demand and aggregated electricity generation from various sources.

- *fuel_gas_consumption_2001_2021.rds*

The plant-level natural gas consumption data from EIA. Used to calculate the plant-level CH4 usage and leakage.

- *monitor_drought_emis.rds* 

The dataset to perform regressions to estimate the impacts of drought-induced emissions on PM2.5 measured at monitors. The dataset contains drought-induced emissions from different distance bins for each EPA monitor, and the non-smoke PM2.5 measured at each monitor.

- *monthly_emis_cmip6_scenarios.rds*

The input data to estimate future damages of drought-induced fossil fuel generation. The data includes the drought-induced generation and emissions from each plant under different climate model projections and electricity sector scenarios.  

### Data/intermediate

- *pool_reg_results.rds*

Regression results that are generated using *1_reg_runoff_generation.R*. The regression coefficients from pooled regression of 500 bootstrap runs. Used to plot Figure 2.


- *BA_fuel_reg_results.rds*

Regression results that are generated using *1_reg_runoff_generation.R*. The regression coefficients from BA-fuel level of 500 bootstrap runs.


- *unit_drought_emis_historical.rds* 

The estimated drought-induced emissions and observed emissions from each fossil fuel unit from 2001 to 2021. Generated using *2_historical_damages.R*. Used in various analysis and plot Figure 3A.

-*pm25_reg_results.csv*

Regression coefficients generated using *3_reg_air_quality.R*. Used to plot Figure 3C.


-*historical_pm25_mortality.rds*

The estimated PM2.5 mortalities due to drought-indued emissions from 2001 to 2021. Used in *3_historical_damages.R*. The results are derived by combining historical drought-induced emissions with the PM2.5-emission estimates from regressions, and the mortality concentration-response function (the derivation process is not shown in this repo).

-*historical_pm25_mortality_CAdrought.rds*

Same as *historical_pm25_mortality.rds*, but only estimates impacts due to runoff anomalies in the CA region. Used to plot Figure 4C.


-*historical_env_damages_year_month.rds*

 The estimated monetized damages of drought-induced emissions from 2001 to 2021. Generated using *2_historical_damages.R*.  Used to plot Figure 4A.


-*env_damages_cmip6_scenarios.rds*

 The estimated monetized damages of drought-induced emissions under different climate model projections and future electricity sector scenarios. Generated using *4_future_damages.R*. Used to plot Figure 5A - 5C.



-*emis_highRE_2024_2050_nerc_bystate.rds*

The predicted drought-induced emissions under high RE scenarios at two conditions (average years and dry year). Used to plot Figure 5D.


