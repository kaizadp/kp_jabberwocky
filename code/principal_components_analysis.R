## Performing and visualizing PCA (Principal Components Analysis) 
## KFP, Nov 2023

## packages needed:
library(tidyverse) # for the data processing
library(ggbiplot) # to make the PCA biplot, installation code below:
### devtools::install_github("miraKlein/ggbiplot")


data = iris


# PERFORMING THE PCA
## The PCA must be done on wide-form dataframes
## with no blank cells.
## The PCA is done within the function below.

fit_pca_function = function(dat){
  
  # First, we drop any rows that have NAs. 
  # NAs f--- with the PCA.
  dat %>% 
    drop_na()
  
  # Then, we split the dataframe into numeric columns (for the PCA)
  # and non-numeric columns that can be used to group the samples
  num = 
    dat %>%       
    dplyr::select(where(is.numeric)) %>%
    dplyr::mutate(row = row_number()) %>% 
    drop_na()
  
  num_row_numbers = num %>% dplyr::select(row)
  
  grp = 
    dat %>% 
    dplyr::select(-where(is.numeric)) %>% 
    dplyr::mutate(row = row_number()) %>% 
    right_join(num_row_numbers)
  
  
  num = num %>% dplyr::select(-row)
  
  # Finally, perform the PCA using `stats::prcomp`
  pca_int = prcomp(num, scale. = T)
  
  # we need all three pieces to build the PCA biplot
  list(num = num,
       grp = grp,
       pca_int = pca_int)
}

data_pca = fit_pca_function(data) 

# PLOTTING THE PCA (biplot)
## detailed info for the `ggbiplot` function can be found in the package description
## try `?ggbiplot` for details

ggbiplot(data_pca$pca_int, 
         obs.scale = 1, var.scale = 1,
         groups = as.character(data_pca$grp$Species), 
         ellipse = TRUE, circle = FALSE, 
         var.axes = TRUE, alpha = 0) +
  geom_point(size = 3,
             aes(
               shape = data_pca$grp$Species, # set shape to a specific column
               color = groups # or simply use `groups` if it's the same grouping
               ))+ 
  NULL

## The code above is for a basic biplot.
## It uses ggplot structure, and you can modify the graph using the typical ggplot commands
  
 