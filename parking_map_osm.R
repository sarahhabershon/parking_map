pacman::p_load("tidyverse", "osmdata", "sf", "showtext", "purr")
# http://bboxfinder.com/#0.000000,0.000000,0.000000,0.000000

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

ak_parks <- opq(bbox = c(174.749324,-36.860106,174.779971,-36.834977)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()

ak_parking$osm_polygons <- ak_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")

ak_coast <- opq(bbox = "Auckland, New Zealand") %>%
  add_osm_feature(key = 'natural', value = 'bay') %>%
  osmdata_sf()

ak_water <- opq(bbox = c(174.749324,-36.860106,174.779971,-36.834977)) %>%
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
    xlim = c(174.749324,174.779971),
    ylim = c(-36.860106,-36.834977),
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
  labs(title = "Central Auckland", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

ak



# Welly

welly_streets <- opq(bbox = c(174.763590,-41.300090,174.789339,-41.281054)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

welly_small_streets <- opq(bbox = c(174.763590,-41.300090,174.789339,-41.281054)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

welly_parking <- opq(bbox = c(174.763590,-41.300090,174.789339,-41.281054)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

welly_parking$osm_polygons <- welly_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")


welly_water <- opq(bbox = c(174.763590,-41.300090,174.789339,-41.281054)) %>%
  add_osm_feature(key = "natural",
                  value = "bay") %>%
  osmdata_sf()

welly_parks <- opq(bbox = c(174.763590,-41.300090,174.789339,-41.281054)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()


welly <- ggplot() +
  geom_sf(data = welly_streets$osm_lines,
          colour = street_colour,
          size = .7,
          alpha = .6) +
  geom_sf(data = welly_small_streets$osm_lines,
          colour = small_street_colour,
          size = .4,
          alpha = 0.6
  ) +
  geom_sf(data = welly_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = welly_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = welly_water$osm_multipolygons,
          fill = water_colour) +
  geom_sf(data = welly_water$osm_polygons,
          fill = water_colour) +
  theme_void() +
  coord_sf(
    xlim = c(174.763590,174.789339),
    ylim = c(-41.300090, -41.281054),
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
  labs(title = "Central Wellington", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

welly






# CHCH

chch_streets <- opq(bbox = c(172.610544,-43.541186,172.652172,-43.520079)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

chch_small_streets <- opq(bbox = c(172.610544,-43.541186,172.652172,-43.520079)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

chch_parking <- opq(bbox = c(172.610544,-43.541186,172.652172,-43.520079)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

chch_water <- opq(bbox = c(172.610544,-43.541186,172.652172,-43.520079)) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()

chch_parking$osm_polygons <- chch_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")

chch_parking$osm_polygons <- chch_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")


chch_parks <- opq(bbox = c(172.610544,-43.541186,172.652172,-43.520079)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()

chch <- ggplot() +
  geom_sf(data = chch_streets$osm_lines,
          colour = street_colour,
          size = .7,
          alpha = .6) +
  geom_sf(data = chch_small_streets$osm_lines,
          colour = small_street_colour,
          size = .4,
          alpha = 0.6
  ) +
  geom_sf(data = chch_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = chch_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = chch_water$osm_polygons,
          fill = water_colour) +
  geom_sf(data = chch_water$osm_multipolygons,
          fill = water_colour) +
  coord_sf(
    xlim = c(172.6199, 172.652072),
    ylim = c(-43.541296, -43.520189),
    expand = FALSE
  ) +
  theme_void() +
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
  labs(title = "Central Christchurch", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

chch





#  tron

tron_streets <- opq(bbox = c(175.264154,-37.797169,175.289732,-37.775586)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

tron_small_streets <- opq(bbox = c(175.264154,-37.797169,175.289732,-37.775586)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

tron_parking <- opq(bbox = c(175.264154,-37.797169,175.289732,-37.775586)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

tron_parking$osm_polygons <- tron_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")


tron_parks <- opq(bbox = c(175.264154,-37.797169,175.289732,-37.775586)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()

tron_water <- opq(bbox = c(175.264154,-37.797169,175.289732,-37.775586)) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()

tron <- ggplot() +
  geom_sf(data = tron_streets$osm_lines,
          colour = street_colour,
          size = .7,
          alpha = .6) +
  geom_sf(data = tron_small_streets$osm_lines,
          colour = small_street_colour,
          size = .4,
          alpha = 0.6
  ) +
  geom_sf(data = tron_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = tron_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = tron_water$osm_polygons,
          fill = water_colour) +
  geom_sf(data = tron_water$osm_multipolygons,
          fill = water_colour) +
  coord_sf(
    xlim = c(175.264154, 175.289732),
    ylim = c(-37.797169, -37.775586),
    expand = FALSE
  ) +
  theme_void() +
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
  labs(title = "Central Hamilton", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

tron




#  dunedin

dunedin_streets <- opq(bbox = c(170.4982,-45.8792,170.5277,-45.8612)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

dunedin_small_streets <- opq(bbox = c(170.4982,-45.8792,170.5277,-45.8612)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

dunedin_parking <- opq(bbox = c(170.4982,-45.8792,170.5277,-45.8612)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

dunedin_parking$osm_polygons <- dunedin_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")


dunedin_parks <- opq(bbox = c(170.4982,-45.8792,170.5277,-45.8612)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()


dunedin_coast <- opq(bbox = "Dunedin, New Zealand") %>%
  add_osm_feature(key = 'natural', value = 'bay') %>%
  osmdata_sf()

dunedin_water <- opq(bbox = c(170.4982,-45.8792,170.5277,-45.8612)) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()

dunedin <- ggplot() +
  coord_sf(ylim = c(175.264154,175.289732),
           expand = FALSE)+
  geom_sf(data = dunedin_streets$osm_lines,
          colour = street_colour,
          size = .7,
          alpha = .6) +
  geom_sf(data = dunedin_small_streets$osm_lines,
          colour = small_street_colour,
          size = .4,
          alpha = 0.6
  ) +
  geom_sf(data = dunedin_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = dunedin_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = dunedin_water$osm_polygons,
          fill = water_colour) +
  geom_sf(data = dunedin_coast$osm_multipolygons,
          fill = water_colour) +
  theme_void() +
  coord_sf(
    xlim = c(170.4982,170.5277),
    ylim = c(-45.8792,-45.8612),
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
  labs(title = "Central Dunedin", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

dunedin






#  Tauranga

tauranga_streets <- opq(bbox = c(		176.16066,-37.68654,176.17401,-37.67618)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

tauranga_small_streets <- opq(bbox = c(		176.16066,-37.68654,176.17401,-37.67618)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

tauranga_parking <- opq(bbox = c(		176.16066,-37.68654,176.17401,-37.67618)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

tauranga_parking$osm_polygons <- tauranga_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")


tauranga_parks <- opq(bbox = c(176.16066,-37.68654,176.17401,-37.67618)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()


tauranga_coast <- opq(bbox = "Tauranga, New Zealand") %>%
  add_osm_feature(key = 'natural', value = 'bay') %>%
  osmdata_sf()


tauranga_water <- opq(bbox = c(176.16066,-37.68654,176.17401,-37.67618)) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()

tauranga <- ggplot() +
  geom_sf(data = tauranga_streets$osm_lines,
          colour = street_colour,
          size = .7,
          alpha = .6) +
  geom_sf(data = tauranga_small_streets$osm_lines,
          colour = small_street_colour,
          size = .4,
          alpha = 0.6
  ) +
  geom_sf(data = tauranga_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = tauranga_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = tauranga_water$osm_polygons,
          fill = water_colour) +
  geom_sf(data = tauranga_coast$osm_multipolygons,
          fill = water_colour) +
  theme_void() +
  coord_sf(
    xlim = c(	176.16066,176.17401),
    ylim = c(-37.68654,-37.67618),
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
  labs(title = "Central Tauranga", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

tauranga





#  Leipzig


leipzig_streets <- opq(bbox = c(12.36691,51.33445,12.38447,51.34501)) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

leipzig_small_streets <- opq(bbox = c(12.36691,51.33445,12.38447,51.34501)) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "footway")) %>%
  osmdata_sf()

leipzig_parking <- opq(bbox = c(12.36691,51.33445,12.38447,51.34501)) %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

leipzig_parking$osm_polygons <- leipzig_parking$osm_polygons %>%
  mutate(parking = replace_na(parking, "unknown")) %>%
  filter(parking != "underground")


leipzig_parks <- opq(bbox = c(12.36691,51.33445,12.38447,51.34501)) %>%
  add_osm_feature(key = "leisure",
                  value = "park") %>%
  osmdata_sf()


leipzig_water <- opq(bbox = c(176.16066,-37.68654,176.17401,-37.67618)) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()



leipzig <- ggplot() +
  geom_sf(data = leipzig_streets$osm_lines,
          colour = street_colour,
          size = .5,
          alpha = .6) +
  geom_sf(data = leipzig_small_streets$osm_lines,
          colour = small_street_colour,
          size = .3,
          alpha = 0.6
  ) +
  geom_sf(data = leipzig_parking$osm_polygons,
          fill = parking_colour) +
  geom_sf(data = leipzig_parks$osm_polygons,
          fill = park_colour) +
  geom_sf(data = leipzig_water$osm_multipolygons,
          fill = water_colour) +
  theme_void() +
  coord_sf(
    xlim = c(12.36691,12.38447),
    ylim = c(51.33445,51.34501),
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
  labs(title = "Central Leipzig", 
       subtitle = 'Land used for car parking',
       caption = "Map Data from OpenStreetMap\n@eye_am_the_i")

leipzig