library(here)
library(tidyverse)
library(sf)
library(janitor)
library(tmap)


# read .csv数据
Report_CompositeData <- read.csv(here::here("HDR23-24_Composite_indices_complete_time_series.csv"),
                          header = TRUE, sep = ",",
                          encoding = "latin1")
Shape_World_countries <- st_read(here::here("World_Countries_Generalized.geojson"))

# 处理.csv数据
Gii_only <- Report_CompositeData %>%
  janitor::clean_names(.) %>%
  dplyr::select(contains("iso3"),
                contains("country"),
                starts_with("gii_")) %>%
  mutate(gii_difference = rowSums(select(.,starts_with("gii_2010")),na.rm = TRUE) - gii_2019) %>%
  dplyr::select(contains("iso3"),
                contains("country"),
                contains("gii_difference"),
                gii_2010:gii_2019)

# join .csv到spatial data
join_data <- Shape_World_countries %>%
  left_join(Gii_only, by = c("COUNTRY" = "country"))
head(join_data)

# 画图
tmap_mode("plot")
tm_shape(join_data) + 
  tm_polygons("gii_difference",palette = "RdYlGn", title = "GII Difference(2010-2019)")+ 
  tm_layout(main.title = "Global Gender Inequality Index Difference(2010-2019)")
