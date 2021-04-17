#' Shapes for Cape Fear
#'
#' @param geo_level default to "tract"
#' @param vintage default to 2010
#' @return
#' @export
#'

# 2010 shapefile: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2010.html
# 2010 geo id lookup: https://census.missouri.edu/geocodes/?state=37
# 2019 shapefiles (for 2020 census response rate data): https://www.census.gov/geo/partnerships/pvs/partnership19v2/st37_nc.html




shape_cfeaR <- function(geo_level = c("tract","blkgrp" ,"block", "county"), vintage = 2010){
  
  # library(zeallot)
  
  counties <- load_county_df()
  
  geo_level <- match.arg(geo_level)
  
  c(sf_file, county_fld, geo_fld) %<-% get_sf_file_param(geo_level, vintage)
  
  # load data
  df <- sf::st_read(sf_file, stringsAsFactors = FALSE) %>%
    sf::st_transform(4269) %>%
    rename(
      county_id := {county_fld},
      GEO_ID := {geo_fld}
    ) %>%
    dplyr::right_join(counties, by = "county_id") %>%
    dplyr::mutate(clean_geo_id = str_replace(GEO_ID, "(.*)US", "")) %>%
    janitor::clean_names()
  
  return(df)
  
}

get_sf_file_param <- function(geo_level_filt, vintage_filt){
  
  cfear_path <- here::here()#getwd()
  
  # print(cfear_path)
  
  sf_values <- data.frame(
    geo_level = c("tract", "tract", "tract", "blkgrp", "block", "county"),
    vintage = c(2010, 2018, 2019, 2010, 2010, 2010),
    file = c(
      "gz_2010_37_140_00_500k.shp",
      "cb_2018_37_tract_500k.shp",
      "PVS_19_v2_curtracts_cfc.shp",
      "gz_2010_37_150_00_500k.shp",
      "tabblock2010_37_pophu.shp",
      "gz_2010_us_050_00_500k_NC_ONLY.shp"
    ),
    geo_fld = c("GEO_ID", "GEOID", "TRACTID", "GEO_ID", "GEOID", "GEO_ID"),
    county_fld = c("COUNTY", "COUNTYFP", "COUNTYFP", "COUNTY", "COUNTYFP10", "COUNTY"),
    stringsAsFactors = FALSE
  ) %>%
    filter(geo_level == geo_level_filt & vintage == vintage_filt) %>%
    mutate(filepath = file.path(cfear_path, "data", glue::glue("NC-{vintage}-{geo_level}-sf"), file)) %>%
    select(filepath, county_fld, geo_fld)
  
  assertthat::assert_that(
    nrow(sf_values) == 1,
    msg = glue::glue("There are no shape files for at the {geo_level_filt} level for {vintage_filt}.")
  )
  
  return(zeallot::destructure(sf_values))
}

load_county_df <- function(){
  
  cfc <- data.frame(
    county_name = c("Bladen", "Brunswick", "Columbus", "New Hanover", "Onslow", "Pender", "Robeson", "Duplin"),
    county_id = c("017", "019", "047", "129", "133", "141", "155", "061"),
    stringsAsFactors = FALSE
  )
  
  return(cfc)
}
