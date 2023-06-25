pacman::p_load("tidyverse", "osmdata", "sf", "showtext")
# http://bboxfinder.com/#0.000000,0.000000,0.000000,0.000000

background_colour <- "#083D77"
street_colour <- "#F4D35E"
small_street_colour <- "#EBEBD3"
parking_colour <- "#F95738"
water_colour <- "#AED6F1"


ak_streets <- opq(bbox = c(174.749324,-36.860106,174.779971,-36.834977)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

ak_small_streets <- opq(bbox = c(174.749324,-36.860106,174.779971,-36.834977)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

ak_parking <- opq(bbox = c(174.749324,-36.860106,174.779971,-36.834977)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

ak_parking$osm_polygons <- ak_parking$osm_polygons %>%
  filter(parking != "underground")



ak <- ggplot() +
  geom_sf(data = ak_streets$osm_lines,
          colour = street_colour,
          size = .5,
          alpha = .6) +
  geom_sf(data = ak_small_streets$osm_lines,
          colour = small_street_colour,
          size = .3,
          alpha = 0.6
  ) +
  geom_sf(data = ak_parking$osm_polygons,
          fill = parking_colour) +
  theme_void() +
  coord_sf(
    xlim = c(174.749324,174.779971),
    ylim = c(-36.860106,-36.834977),
    expand = FALSE
  ) +
  theme(plot.title = element_text(colour=background_colour,
                                  size = 28, face="bold", hjust=.5,
                                  vjust=2.5),
        plot.margin=unit(c(0.6,1.6,1,1.6),"cm"),
        panel.border = element_rect(colour = "white", fill=NA, size=1),
        plot.subtitle = element_text(colour=background_colour,
                                     vjust=0.1,
                                     size = 18, hjust=.5, margin=margin(2, 0, 12, 0)),
        plot.caption = element_text(colour = background_colour,
                                    size = 10),
        panel.background = element_rect(fill = background_colour)) +
  labs(title = "Central Auckland", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

ak

