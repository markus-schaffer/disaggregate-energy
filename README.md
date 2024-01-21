# Disaggregate total energy 
This repository includes the code, used for this publication: 

**M. Schaffer, J. Widén, J.E. Vera-Valdés, A. Marszal-Pomianowska, T.S. Larsen, Disaggregation of total energy use into space heating and domestic hot water: A city-scale suited approach, Energy. 291 (2024) 130351. https://doi.org/10.1016/j.energy.2024.130351.**

If you use this code, please cite the above, mentioned publication. 
## Indented aim

## Code Overview

This GitHub repository contains the code used in the analysis and generation of figures for our scientific publication. The code is organized into several files, each serving specific purposes:

1. **[`01_data_analysis.R`](01_data_analysis.R)**: This script combines smart meter data with weather data (refer to [data](#data)) and conducts analysis, producing figures 3 to 5 in the publication.
2. **[`02_reg_analysis_HPO.R`](02_reg_analysis_HPO.R)**: Selecting data from 160 buildings, this script performs analysis on various regression models with and without hyperparameter optimization, generating figures 6 and 7.
3. **[`03_analyse_pac_pcc.R`](03_analyse_pac_pcc.R)**: Calculating partial auto and cross-correlation, this script produces plots 8 and 9 in the publication.
4. **[`04_reg_analysis_features.R`](04_reg_analysis_features.R)**: Creating five different features and analyzing their performance for the selected regression models using all data, this script generates figures 10 and 11.
5. **[`05_final_disaggregation.R`](05_final_disaggregation.R)**: Performing final disaggregation for all buildings and analyzing feature representativeness using two proposed methods, this script produces figure 12.
6. **[`06_validation_low_res.R`](06_validation_low_res.R)**: Loading low-resolution validation data (refer to [data](#data)) and conducting algorithm validation, this script produces figure 13 and provides data for table 3.
7. **[`07_validation_high_res.R`](07_validation_high_res.R)**: Loading high-resolution validation data (refer to [data](#data)) and conducting algorithm validation, this script produces figures 14 and 15, along with data for table 4.


## Data

The data used in our publication is not included in this repository.
The smart meter data used is described in this publication and can be accessed via the specified conditions:

M. Schaffer, M. Veit, A. Marszal-Pomianowska, M. Frandsen, M.Z. Pomianowski, E. Dichmann, C.G. Sørensen, J. Kragh, Dataset of smart heat and water meter data with accompanying building characteristics, Data Br. 52 (2024) 109964. https://doi.org/10.1016/j.dib.2023.109964.

The weather data can be obtained from the Danish Meteorological Institute (DMI) via the following link: [`DMI Api`](https://confluence.govcloud.dk/display/FDAPI).

The validation data cannot be shared.
The format of all data as expected by the code is described below. 


1. **[`01_data_analysis.R`](01_data_analysis.R)**

   Combined smart meter data as a .fst file with hourly readings for each building. The file should contain the following columns:

   | customer_id | time_rounded  | water_volume_m3 | demand_spms |
   | ----------- | ------------- | ---------------- | ------------ |
   | Customer ID uniquely identifying each building | Rounded hourly reading times | Cumulative Water volume in m3 | Total energy use processed with SPMS in kWh/h |

   Hourly weather data as a .csv file with the following columns:

   | time_rounded | ext_temp | dhi  | dni  | ghi  |
   | ------------ | -------- | ---- | ---- | ---- |
   | Rounded hourly reading times | External temperature in °C | Diffuse horizontal irradiance in W/m2 | Direct normal irradiance in W/m2 | Global horizontal irradiance in W/m2 |

2. **[`06_validation_low_res.R`](06_validation_low_res.R)**

   Hourly data as a .csv file with the following columns:

   | house_nr | group | reading_time | total_energy_kwh_demand | total_water_m3_demand | demand_spms | dhw_energy_kwh_demand | zero_water |
   | --------- | ----- | ------------ | ----------------------- | ---------------------- | ------------ | ----------------------- | ---------- |
   | House number identifying each building | Group number used as some buildings have missing data. One building can have multiple data sequences, i.e., groups which are not consecutive | Hourly reading times | Total energy use in kWh/h | Total water use in m3/h | Total energy use processed with SPMS in kWh/h | Total energy use for DHW in kWh/h | Zero water use indicator (1 if no water was used, 0 otherwise) |

3. **[`07_validation_high_res.R`](07_validation_high_res.R)**

   Hourly data as a .csv file with the same format as for [`06_validation_low_res.R`](06_validation_low_res.R). The only difference is that this data must be recorded with high resolution for energy use.
   
## Bibtext
```
@article{Schaffer2024,
author = {Schaffer, Markus and Wid{\'{e}}n, Joakim and Vera-Vald{\'{e}}s, J. Eduardo and Marszal-Pomianowska, Anna and Larsen, Tine Steen},
doi = {10.1016/j.energy.2024.130351},
issn = {03605442},
journal = {Energy},
mendeley-groups = {DW/Disagregartion},
month = {mar},
pages = {130351},
title = {{Disaggregation of total energy use into space heating and domestic hot water: A city-scale suited approach}},
url = {https://linkinghub.elsevier.com/retrieve/pii/S0360544224001221},
volume = {291},
year = {2024}
}
```
   