
library(sf) # for map
library(ggspatial) # for north arrow
library(tidyverse)
theme_set(theme_bw(base_size = 14))

## Set CRS
common_crs <- 4326

## Set map size and point size
point_size <- 2
map_width = 9
map_height = 6

## Set regional and WLE/CB (inset) bounding boxes
us_bbox <- c(xmin = -125, xmax = -40, ymin = 20, ymax = 50)
rowan_bbox <- c(xmin = -76, xmax = -72, ymin = 38.5, ymax = 42)

## Make US states map cropped to GL/CB region
us <- 
  read_sf("rowan_resilience_figs/cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
  st_transform(., crs = common_crs) %>% 
  st_crop(., y = us_bbox)

region <- st_crop(us, y = region_bbox)

## Further crop states to WLE/CB region
cb_states <- st_crop(region, y = rowan_bbox)

## Get state labels
st_labels = st_centroid(us) %>% 
  filter(!STUSPS %in% c("TN", "NC", "DC")) # remove state labels that mess with the graph

## create a df with site coordinates and labels
sites = data.frame(
  y = c(39.94000),
  x = c(-74.75610))

## Make the base map
base_plot <- 
  ggplot() + 
  geom_sf(data = region) + 
  #   geom_sf_text(data = st_labels, aes(label = STUSPS))+
  geom_point(
    data = sites, aes(x, y),
    size = 2, color = "black")+
  # CB inset rectangle
  geom_rect(aes(xmin = -76, xmax = -72, ymin = 38.5, ymax = 42), 
            fill = NA, color = "black", lwd = 0.75) +
  geom_segment(aes(x = -72, xend = -70, y = 40, yend = 40), 
               color = "black", lwd = 0.75) + 
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  xlim(-80, -63)+
  ylim(35, 47.3)+
  theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
        legend.key = element_rect(fill = "transparent"), 
        legend.position = c(0.85, 0.1)) + 
  labs(x = "", y = "")+
  NULL


## Make the inset map with just CB sites
inset_plot_rowan <- 
  ggplot() + 
  geom_sf(data = cb_states) + 
  geom_point(
    data = sites, aes(x, y),
    size = 5, color = "black")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  cowplot::theme_map() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#48bfe3"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),
        axis.text = element_blank()) 

## Combine into single figure
base_plot + 
  annotation_custom(
    ggplotGrob(inset_plot_rowan), 
    xmin = -71, xmax = -63, ymin = 32, ymax = 46)+
#  theme_kp()+
  NULL


ggsave("rowan_resilience_figs/pine_barrens_map.jpg", height = 9.5, width = 9.5)

