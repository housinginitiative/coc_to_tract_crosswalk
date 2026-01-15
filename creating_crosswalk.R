# PURPOSE

# This script creates a CSV file that acts as a CoC to Census Tract crosswalk. 
# The end result matches Census Tracts uniquely to CoC's.  
# In cases where Census Tracts are slipt across CoC's, they are
# assigned to the CoC that contains the majority of the Tract by area. 

# We do this progressively, first matching CoC's whose boundaries align with 
# county or state boundaries before performing a match of the remaining CoC's 
# directly to tracts. We take this approach in order to leverage the nesting of 
# Census Geographies in order to reduce processing time. 


# Load libraries  

library(sf)
library(dplyr)
library(tidycensus)
library(readxl)
library(units)
library(purrr)

project_root <- here::here()

# Read in CoC Boundaries, operationalizes, and removes CoC's not included in crosswalk  


gdb_path <- file.path(project_root, "CoC_GIS_National_Boundary2024.gdb") # Sets location of CoC boundary file

layers <- st_layers(gdb_path)

territories <- c( "AS-500", "GU-500", "MP-500", "PR-502", "PR-503", "VI-500") # Lists CoC's within US territories

layer_name <- "FY24_CoC_National_Bnd" 
coc_boundaries <- st_read(gdb_path, layer = layer_name) %>%
  mutate(Shape= st_make_valid(Shape))  %>% 
  mutate(COCarea = st_area(Shape) %>% set_units(mi^2)) %>% 
  filter(!COCNUM %in% territories) %>%  # remove territories, not included in analysis 
  mutate(state_code = ST_1)


# Identify CoC's to match at the county level -- through a series of filters 
# first based on the CoC name and then through manual inspection 

# CoC's identified as likely composed of Whole Counties based on name 

coc_boundaries_county <- filter(coc_boundaries, grepl( "County", COCNAME, ignore.case = TRUE))

coc_boundaries_counties <- filter(coc_boundaries, grepl( "Counties", COCNAME, ignore.case = TRUE))

coc_boundaries_BOS <- filter(coc_boundaries, grepl( "State", COCNAME, ignore.case = TRUE))


## Manual inspection indicating that boundaries are aligned with counties even if the overlap between CoC
# and county based on st_intersection (see below) is not virtually 100% 

# these are coc's generaly with large amounts of coastline; 

manual_COCNUMS <- c("CA-501", "CA-502", "CA-507", "DC-500",
                    "FL-500", "FL-501", 
                    "FL-502",  "FL-504",     "FL-505",  "FL-509",
                    "FL-511", "FL-512",   "FL-513",   "FL-515", "FL-520",
                    "FL-600",  "FL-602",  "FL-603",  "FL-604",  "HI-501",
                    "IA-502", "IL-514", "IL-517", "LA-503", "LA-509",
                    "MD-503", "MD-511",   "MD-513", "MD-600", "ME-500",
                    "MI-515", "PA-500","VA-500", "VA-601", "VA-604",
                    "VI-500","RI-500", "SC-500", "NJ-500", "NJ-501",
                    "NJ-503", "NJ-506",  "NJ-507", "NJ-508", "NJ-510",
                    "PA-502", "NY-606", "NY-600", "NY-603", "NY-604",
                    "AK-501", "AL-501", "AS-500","GA-507") 



# Subsetting the boundaries of these CoC's identified above so that a first match can be 
# on a subset in order to reduce the processing time 

coc_bos_and_county <- coc_boundaries %>% filter( COCNUM %in% as.list(coc_boundaries_county$COCNUM) |
                                                   COCNUM %in% as.list(coc_boundaries_BOS$COCNUM) |
                                                   COCNUM %in% as.list(coc_boundaries_counties$COCNUM) |
                                                   COCNUM %in% manual_COCNUMS) %>% 
  st_set_crs("NAD83")


#Bring in boundaries from the Census

#census_api_key(census_key, overwrite = TRUE)

acsVariables <- c("B01003_001E") # Total population

# Set tigris option to use cache
options(tigris_use_cache = TRUE)

states <- unique(fips_codes$state)[1:51]

# Get ACS data
get_state_acs <- function(state) {
  get_acs(geography = "county", 
          variables = acsVariables, 
          survey = "acs5", 
          year = 2023, 
          state = state, 
          output = "wide", 
          geometry = TRUE)
}

us_2023 <- map_dfr(states, get_state_acs)

counties2023 <- us_2023 %>%
  select(-B01003_001M) %>%
  st_set_crs("NAD83")

# calculate the area of these counties 

counties2023 <- counties2023 %>% mutate(county_area = st_area(counties2023) %>% set_units(mi^2),
                                        geometry = st_make_valid(geometry)) 

# calculate overlap between the county boundaries and the CoC's identified as those to be matched at the county level 
# calculate intersection area and calculate the percent overlap between coc and county 

intersections <- st_intersection(counties2023, coc_bos_and_county) %>%  
  mutate(geometry = st_make_valid(geometry)) 

intersection_area <- intersections %>% 
  mutate(intersection_area = st_area(intersections) %>%  
           set_units(mi^2)) %>%
  mutate(percent_area = as.numeric(intersection_area/county_area))%>%
  st_drop_geometry()

# filter this list so it is either coc's where the overlap is virtually 100% OR
# a lower threshhold that has been manually inspected 

qualified_counties <- intersection_area  %>%
  filter(percent_area > 0.999 | 
           (COCNUM %in%  manual_COCNUMS & percent_area > 0.6)) 

# Lower threshhold here for these ones identified through manual inspection

# check that the area of the CoC is fully covered by these counties 

qualified_counties_check <- qualified_counties %>%
  group_by(COCNAME) %>%
  dplyr::summarise(total_intersection_area = sum(intersection_area),
                   number_of_counties = n()) %>%
  st_drop_geometry()

coc_boundaries_check <- coc_boundaries %>% st_drop_geometry()

qualified_counties_COC_check <- inner_join(qualified_counties_check, coc_boundaries_check, by = "COCNAME") %>%
  mutate(coc_county_coverage = drop_units(total_intersection_area/COCarea)) %>%
  filter(coc_county_coverage > 0.999 |
           COCNUM %in% manual_COCNUMS)
###

## Utilize nested geographies:
# identify the tracts that are contained within counties that have been identified as
# having boundaries that overlap with those of the CoC

matched_counties <- qualified_counties %>% st_drop_geometry() %>%
  inner_join(., qualified_counties_COC_check, by="COCNUM") %>% 
  st_drop_geometry() %>%
  select(COCNUM, county_code= GEOID )

matched_counties_LIST <- as.list(matched_counties$county)


# Get ACS data (TRACTS)
get_state_acs <- function(states) {
  get_acs(geography = "tract", 
          variables = acsVariables, 
          survey = "acs5", 
          year = 2023, 
          state = states, 
          output = "wide", 
          geometry = FALSE)
}

all_tracts <- map_dfr(states, get_state_acs) 

all_tracts <- all_tracts %>%
  mutate(county_code = strtrim(GEOID, 5))

## creates dataframe that is a crosswalk between tracts and CoCs matched thus far: 

tracts_matched_county_level <- all_tracts %>% 
  inner_join(., matched_counties, by = "county_code") %>%
  select(COCNUM, GEOID)

###

#184 COCs have been matched above way; 197 will still need to be spatially matched
# Identify what CoC's and tracts need to be included in this second round of spatial matching
#calculate spatial intersections between CoCs and tracts
#and then identify what CoC contains the majority of a tract. 


missing <- coc_boundaries %>% 
  st_drop_geometry() %>%
  filter(! COCNUM %in% as.list(qualified_counties_COC_check$COCNUM)) 


##

states_where_we_need_tracts <- unique(missing$state_code)

## Get tract geometry in states that we haven't already fully mapped onto coc's 

# Get ACS data (TRACTS)
get_state_acs <- function(states_where_we_need_tracts) {
  get_acs(geography = "tract", 
          variables = acsVariables, 
          survey = "acs5", 
          year = 2023, 
          state = states_where_we_need_tracts, 
          output = "wide", 
          geometry = TRUE)
}

us_2023 <- map_dfr(states, get_state_acs)

tracts2023 <- us_2023 %>%
  select(-B01003_001M) 

tracts2023 <- tracts2023 %>% mutate(geometry = st_make_valid(geometry),
                                    tract_area = st_area(tracts2023) %>% set_units(mi^2)) %>%
  filter(!GEOID %in% as.list(tracts_matched_county_level $GEOID))

# Filters tracts that we have not already matched at the county level

coc_tract_intersections <- coc_boundaries %>% filter(COCNUM %in% as.list(missing$COCNUM))

# Calculates intersections 

tract_intersections <- st_intersection(tracts2023, coc_tract_intersections) %>% 
  mutate(geometry = st_make_valid(geometry)) 

tract_areas <-  tract_intersections %>% mutate(intersection_area = st_area(tract_intersections ) %>%  
           set_units(mi^2)) %>%
  mutate(percent_area = (as.numeric(intersection_area)/as.numeric(tract_area)))

# Tracts whose area is at least 55% contained within a CoC are assigned to those CoCs, 
# tracts who have less intersection by area are treated as invalid matches. 

qualified_tracts <- tract_areas %>%
  filter(percent_area > 0.55) %>% 
  st_drop_geometry() %>%
  select(COCNUM, GEOID)


###

rbind_geoid <- rbind(qualified_tracts, tracts_matched_county_level) 

# write.csv(rbind_geoid , "tract_to_coc_crosswalk.csv")
