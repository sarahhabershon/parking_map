pacman::p_load("tidyverse", "osmdata", "sf", "showtext", "purr")
# http://bboxfinder.com/#52.466373,-1.932838,52.500682,-1.879280

# background_colour <- "#083D77"
# street_colour <- "#F4D35E"
# small_street_colour <- "#EBEBD3"
# parking_colour <- "#F95738"
# water_colour <- "#AED6F1"


background_colour <- "#f6ebd1"
street_colour <- "#674024"
small_street_colour <- "#674024"
parking_colour <- "#c64c53"
water_colour <- "#a0cbbd"
park_colour <- "#b4d5a8"
text_colour <- "#050505"



a = -80.198822
b = 25.758310
c = -80.104752
d = 25.919080

ak_streets <- opq(bbox = c(a,b,c,d)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf() 

ak_small_streets <- opq(bbox = c(a,b,c,d)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

ak_parking <- opq(bbox = c(a,b,c,d)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

ak_parks <- opq(bbox = c(a,b,c,d)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()

ak_parking$osm_polygons <- ak_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")

ak_coast <- opq(bbox = c(a,b,c,d)) %>%
  add_osm_feature(key = 'natural', value = 'bay') %>%
  osmdata_sf()

ak_water <- opq(bbox = c(a,b,c,d)) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()


ak <- ggplot() +
  geom_sf(data = ak_streets$osm_lines,
          colour = street_colour,
          size = .7,
          alpha = .6) +
  geom_sf(data = ak_small_streets$osm_lines,
          colour = small_street_colour,
          size = .4,
          alpha = 0.6
  ) +
  geom_sf(data = ak_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = ak_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = ak_coast$osm_multipolygons,
          fill = water_colour) +
  geom_sf(data = ak_water$osm_multipolygons,
          fill = water_colour) +
  theme_void() +
  coord_sf(
    xlim = c(a,c),
    ylim = c(b,d),
    expand = FALSE
  ) +
  theme(plot.title = element_text(colour=text_colour,
                                  size = 20, face="bold", hjust=.5,
                                  vjust=2.5),
        plot.subtitle = element_text(colour=text_colour,
                                     vjust=0.1,
                                     size = 18, hjust=.5, margin=margin(2, 0, 12, 0)),
        plot.caption = element_text(colour = text_colour,
                                    size = 10),
        panel.background = element_rect(fill = background_colour),
        plot.margin=unit(c(0.6,1.6,1,1.6),"cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(title = "Miami Beach", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

ak

