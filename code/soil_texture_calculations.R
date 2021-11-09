## This script contains functions to compute percent sand-silt-clay
## Equations taken from Gee & Bauder 1986. Particle-size Analysis. 
## In *Methods of Soil Analysis, Part I. Physical and Mineralogical Methods.*

## Kaizad F. Patel 
## 2021-11-09

###################### #
###################### #


library(tidyverse)

# I. METHOD ---------------------------------------------------------------

## Weigh approximately 40-50 g of dry soil into the reaction beaker. 
## Follow pre-processing steps (OM removal, chemical and physical de-aggregation, etc.) per the official protocol.
## Transfer the soil to the hydrometer flask and make up volume to 1000 mL with deionized water.
## Take hydrometer readings at 1.5 hours (90 minutes) and 24 hours (1440 minutes).
## Also take blank readings at the two time points.

## After the 24 hour reading, pass the suspension through a 53 μm sieve and collect the coarse fraction (> 53 μm).
## Oven-dry the >53μm fraction and weigh. 
### ^^ this can be done by rinsing the material into a beaker, 
### or by simply drying the entire sieve+soil in the oven.


#
# II. TEMPLATE FOR DATA ENTRY ---------------------------------------------

## This function will create a blank template that can be used for data entry,
## compatible with the compute_ function below.
## You may modify the column names here, but make sure you also update the compute_ function.

create_texture_data_template = function(){
  
  tribble(
    ~sample_name,
    ~wt_dry_soil_g,
    ~wt_sieve_g,
    ~wt_sieve_soil_53um_g,
    ~reading_90min,
    ~blank_90min,
    ~reading_1440min,
    ~blank_1440min,
    ~date_started,
  )  
}


#
# III. COMPUTING PERCENT SAND-SILT-CLAY -----------------------------------

## This function will use the equations provided in Gee & Bauder
## to compute % sand, clay, silt

## % sand = fraction weight of material collected on the 53 μm sieve.
## % clay = computed using 90 and 1440 minute hydrometer readings
## % silt = 100 - (% sand + % clay)

compute_soil_texture = function(dat){
  
  dat %>% 
    mutate(
      B = (30 * 0.0091) / (9.98 * (2.65 - 1)), # constant
      
      #h = 16.3 - (0.164 * R),
      h_90min = 16.3 - (0.164 * reading_90min),
      h_1440min = 16.3 - (0.164 * reading_1440min),
      
      theta_90min = 1000 * (B * h_90min)^0.5,
      theta_1440min = 1000 * (B * h_1440min)^0.5,
      
      # P = summation %
      P_90min = ((reading_90min - blank_90min)/wt_dry_soil_g) * 100, 
      P_1440min = ((reading_1440min - blank_1440min)/wt_dry_soil_g) * 100,
      
      # X = mean diameter
      X_90min = theta_90min * (90)^-0.5, 
      X_1440min = theta_1440min * (1440)^-0.5,
      
      m = (P_90min - P_1440min)/log(X_90min/X_1440min),
      
      # percent sand-silt-clay
      percent_clay = (m * log(2/X_1440min)) + P_1440min,
      percent_sand = ((wt_sieve_soil_53um_g - wt_sieve_g)/wt_dry_soil_g) * 100,
      percent_silt = 100 - (percent_sand + percent_clay)
    ) %>% 
    dplyr::select(sample_name, starts_with("percent_"))
  
}


#
# IV. USING THESE FUNCTIONS -----------------------------------------------

# first, create and export the template
template = create_texture_data_template()
template %>% write.csv(..., row.names = FALSE)

# then, enter the data manually in the .csv file
# next, import the data and run through the compute_ function

texture_data = read.csv(...)
compute_soil_texture(dat = texture_data)

#
# V. EXAMPLE --------------------------------------------------------------

x = tribble(
  ~sample_name,
  ~wt_dry_soil_g,
  ~wt_sieve_g,
  ~wt_sieve_soil_53um_g,
  ~reading_90min,
  ~blank_90min,
  ~reading_1440min,
  ~blank_1440min,
  ~date_started,
  
  "BC15",
  40,
  372.92,
  391.35,
  16,
  -3,
  11,
  -3,
  NA
) 

compute_soil_texture(dat = x)
