
####################### AUTHORSHIP MAPS FOR NATURE CLIMATE CHANGE #####################

# country lists and some visualisation

library(magrittr); library(dplyr); library(sf); library(countrycode); library(ggplot2)
setwd("C:/Users/roryj/Dropbox/Research/attriverse/figure_generation/")

# proj
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# geographical focus of analysis country list (n=101)
c1 = read.csv("./analysis_countries.csv") %>%
  dplyr::filter(!Study %in% c("Astrom et al 2013")) %>%
  dplyr::mutate(
  ISO = countrycode::countrycode(Country, origin="country.name", destination="iso3c")
  ) %>%
  dplyr::group_by(ISO) %>%
  dplyr::summarise(Country = head(Country, 1),
                   n_analysis = n_distinct(Study))

# analysis author list
c2 = read.csv("./author_countries.csv") %>%
  dplyr::filter(NCC == TRUE) %>%
  dplyr::rename("author_country"=2,
                "lead"=3,
                "senior"=4) %>%
  tidyr::separate_rows(author_country, sep=", ") %>%
  dplyr::mutate(
    ISO = countrycode::countrycode(author_country, origin="country.name", destination="iso3c")
  ) %>%
  dplyr::group_by(ISO) %>%
  dplyr::summarise(Country = head(author_country, 1),
                   n_authors = n_distinct(Study))

# senior or lead
c3 = read.csv("./author_countries.csv") %>%
  dplyr::filter(NCC == TRUE) %>%
  dplyr::rename("author_country"=2,
                "lead"=3,
                "senior"=4) %>%
  dplyr::select(1, 3) %>%
  tidyr::separate_rows(lead, sep=", ")

c4 = read.csv("./author_countries.csv") %>%
  dplyr::filter(NCC == TRUE) %>%
  dplyr::rename("author_country"=2,
                "lead"=3,
                "senior"=4) %>%
  dplyr::select(1, 4) %>%
  tidyr::separate_rows(senior, sep=", ")

c3 = rbind(c3, dplyr::rename(c4, "lead"=senior)) %>%
  dplyr::mutate(
  ISO = countrycode::countrycode(lead, origin="country.name", destination="iso3c")
  ) %>%
  dplyr::group_by(ISO) %>%
  dplyr::summarise(Country = head(lead, 1),
                   n_lead_or_senior = n_distinct(Study))

# shapefile
ne = sf::st_read("./ne_10m_admin_0_countries.shp") %>%
  dplyr::select(ADMIN, ADM0_ISO, ISO_A3) %>%
  dplyr::mutate(
    ISO = ISO_A3,
    ISO = replace(ISO, ADMIN=="France", "FRA"),
    ISO = replace(ISO, ADMIN=="Norway", "NOR")
  ) %>%
  left_join(dplyr::select(c1, -Country)) %>%
  #dplyr::mutate(n_analysis = replace(n_analysis, is.na(n_analysis), 0)) %>%
  left_join(dplyr::select(c2, -Country)) %>%
  #dplyr::mutate(n_authors = replace(n_authors, is.na(n_authors), 0)) %>%
  left_join(dplyr::select(c3, -Country)) %>%
  sf::st_transform(robinson)

# continents
ne2 = sf::st_read("./ne_10m_land.shp") %>%
  sf::st_transform(robinson)

# combine and plot 1
# p1 = ggplot() + 
#   geom_sf(data=ne, color=NA, aes(fill=n_analysis)) + 
#   geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
#   theme_void() + 
#   scale_fill_gradientn(colors=rev(viridisLite::mako(200)), name="Number\nof\nstudies", na.value="grey90") + 
#   ggtitle("Geographic focus of health impact attribution studies") +
#   theme(plot.title = element_text(size=14, hjust=0.5))
  
# combine and plot 1
p1 = ggplot() + 
  geom_sf(data=ne, color=NA, aes(fill=n_analysis)) + 
  geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
  theme_void() + 
  #scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Number\nof\nstudies", na.value="grey90") + 
  scale_fill_gradientn(colors=pals::brewer.pubugn(200)[75:200], name="Number\nof\nstudies", na.value="grey95") + 
  ggtitle("Geographic focus of health impact attribution studies") +
  theme(plot.title = element_text(size=14, hjust=0.5))

p2 = ggplot() + 
  geom_sf(data=ne, color=NA, aes(fill=n_authors)) + 
  geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
  theme_void() + 
  #scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Number\nof\nstudies", na.value="grey90") + 
  scale_fill_gradientn(colors=pals::brewer.pubugn(200)[75:200], name="Number\nof\nstudies", na.value="grey95") + 
  ggtitle("Authorship of health impact attribution studies")  +
  theme(plot.title = element_text(size=14, hjust=0.5))

p3 = ggplot() + 
  geom_sf(data=ne, color=NA, aes(fill=n_lead_or_senior)) + 
  geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
  theme_void() + 
  #scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Number\nof\nstudies", na.value="grey90") + 
  scale_fill_gradientn(colors=pals::brewer.pubugn(200)[75:200], name="Number\nof\nstudies", na.value="grey95") + 
  ggtitle("Lead or senior authorship only")  +
  theme(plot.title = element_text(size=14, hjust=0.5))

pc = gridExtra::grid.arrange(p1, p2, p3, ncol=1)
ggsave(pc, file="./attriverse_geomap_july2024.jpg", device="jpg", dpi=600, width=8, height=10.5, units="in")


# 
# 
# # OPTION 2: proportions
# nstudies =  read.csv("./analysis_countries.csv") %>%
#   dplyr::select(Study) %>%
#   distinct() %>%
#   nrow()
# 
# # combine and plot 1
# p1 = ggplot() + 
#   geom_sf(data=ne, color=NA, aes(fill=n_analysis/nstudies)) + 
#   geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
#   theme_void() + 
#   scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Prop.\nstudies", na.value="grey90") + 
#   ggtitle("Geographic focus of health impact attribution studies") +
#   theme(plot.title = element_text(size=14, hjust=0.5))
# 
# p2 = ggplot() + 
#   geom_sf(data=ne, color=NA, aes(fill=n_authors/nstudies)) + 
#   geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
#   theme_void() + 
#   scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Prop.\nstudies", na.value="grey90") + 
#   ggtitle("Authorship of health impact attribution studies")  +
#   theme(plot.title = element_text(size=14, hjust=0.5))
# 
# p3 = ggplot() + 
#   geom_sf(data=ne, color=NA, aes(fill=n_lead_or_senior/nstudies)) + 
#   geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
#   theme_void() + 
#   scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Prop.\nstudies", na.value="grey90") + 
#   ggtitle("Lead or senior authorship only")  +
#   theme(plot.title = element_text(size=14, hjust=0.5))
# 
# pc = gridExtra::grid.arrange(p1, p2, p3, ncol=1)
# ggsave(pc, file="./attriverse_geomap_proportions_20230911.jpg", device="jpg", dpi=600, width=8, height=10.5, units="in")
# 
# 
# 
# # ============= interview authors ===============
# 
# # interviews
# int = readxl::read_xlsx("./Interviews (1).xlsx") %>%
#   dplyr::mutate(ISO = countrycode::countrycode(Country, origin="country.name", destination = "iso3c")) %>%
#   dplyr::group_by(ISO) %>%
#   dplyr::summarise(ncount = length(Affiliation)) %>%
#   dplyr::mutate(ISO = replace(ISO, ISO=="FRA", "FRA1"))
# 
# # shapefile
# ne = sf::st_read("./ne_10m_admin_0_countries.shp") %>%
#   dplyr::select(ADMIN, ADM0_ISO, ISO_A3) %>%
#   dplyr::mutate(
#     ISO = ISO_A3,
#     ISO = replace(ISO, ADMIN=="France", "FRA"),
#     ISO = replace(ISO, ADMIN=="Norway", "NOR")
#   ) 
# 
# # fix france
# fr1 = ne %>% dplyr::filter(ISO == "FRA") %>%
#   sf::st_crop(fr1, ymin=41, ymax=53, xmin=-7, xmax=10)
# fr1$ISO = "FRA1"
# ne = rbind(ne, fr1)
# 
# ne = ne %>%
#   sf::st_transform(robinson) %>%
#   dplyr::left_join(int)
# 
# # continents
# ne2 = sf::st_read("./ne_10m_land.shp") %>%
#   sf::st_transform(robinson)
# 
# p1 = ggplot() + 
#   geom_sf(data=ne, color=NA, aes(fill=ncount)) + 
#   geom_sf(data=ne2, fill=NA, color="grey50", size=0.2) +
#   theme_void() + 
#   scale_fill_gradientn(colors=rev(MetBrewer::met.brewer("Greek", 5)), name="Num.\nparticipants", na.value="grey90") + 
#   ggtitle("Locations of interview participant institutions")  +
#   theme(plot.title = element_text(size=14, hjust=0.5))
# ggsave(p1, file="./attriverse_interviewee_map.jpg", device="jpg", dpi=600, width=8, height=3.5, units="in")
# 
