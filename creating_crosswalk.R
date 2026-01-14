# PURPOSE

# This script creates a CSV file that acts as a CoC to Census Tract crosswalk. 
# The end result matches Census Tracts uniquely to CoC's.  
# In cases where Census Tracts are slipt across CoC's, they are
# assigned to the CoC that contains the majority of the Tract by area. 

# We do this progressively, first matching CoC's whose boundaries align with 
# county or state boundaries before performing a match of the remaining CoC's 
# directly to tracts. We take this approach in order to leverage the nesting of 
# Census Geographies in order to reduce processing time. 

# Key Steps: 
# Load libraries and data 
# Create a subset of CoC's to match at the county level 
# Match these CoC's to Counties through a spatial join
# Check that these matches are valid 
# Create a subset 

# Load libraries and data 

library(sf)
library(dplyr)
library(tidycensus)
library(readxl)
library(units)

project_root <- here::here()

# Reads in Esri File Geodatabase with CoC Boundaries  
# Available through HUD Exchange > CoC GIS Tools

gdb_path <- file.path(project_root, "data/CoC_GIS_National_Boundary2024.gdb") # Read in CoC boundaries

layers <- st_layers(gdb_path)
print(layers)

layer_name <- "FY24_CoC_National_Bnd" 
coc_boundaries <- st_read(gdb_path, layer = layer_name) %>%
  mutate(Shape= st_make_valid(Shape))  %>% 
  mutate(COCarea = st_area(Shape) %>% set_units(mi^2))

territories <- c( "AS-500", "GU-500", "MP-500", "PR-502", "PR-503", "VI-500")

# remove territories, not included in analysis 

coc_boundaries <- coc_boundaries  %>% 
  filter(!COCNUM %in% territories)

#Just checking things out 
mapview(coc_boundaries)

# Create a subset of CoC's to match at the county level 
# Create filters so that CoC's we assume map neatly onto county boundaries 
# either based on their names or manual inspection
# we do this to do the match at county rather than tract level 
# for processing time

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

# CoC's identified as likely composed of Whole Counties based on name 

coc_boundaries_county <- filter(coc_boundaries, grepl( "County", COCNAME, ignore.case = TRUE))

coc_boundaries_counties <- filter(coc_boundaries, grepl( "Counties", COCNAME, ignore.case = TRUE))

coc_boundaries_BOS <- filter(coc_boundaries, grepl( "State", COCNAME, ignore.case = TRUE))

# Subsetting the boundaries of these CoC's identified above so that a first match can be 
# on a subset in order to reduce the processing time 

coc_BOS_and_County <- coc_boundaries %>% filter( COCNUM %in% as.list(coc_boundaries_county$COCNUM) |
                                                   COCNUM %in% as.list(coc_boundaries_BOS$COCNUM) |
                                                   COCNUM %in% as.list(coc_boundaries_counties$COCNUM) |
                                                   COCNUM %in% manual_COCNUMS) %>% 
  st_set_crs("NAD83")


#Bring in boundaries from the Census

census_api_key(census_key, overwrite = TRUE)

acs_2024 <- load_variables(year = 2023, dataset = "acs5")

acsVariables <- c("B01003_001E", # Total population
                  "B01001A_001E", # White population
                  "B01001I_001E") # Hispanic population estimate

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
  select(-B01003_001M, -B01001A_001M, -B01001I_001M) %>%
  st_set_crs("NAD83")

# calculate the area of these counties 

counties2023 <- counties2023 %>% mutate(county_area = st_area(counties2023) %>% set_units(mi^2),
                                        geometry = st_make_valid(geometry)) 

#calculate overlap between the county boundaries and the CoC's identified as those to be matched at the county level 
# calculate intersection area
# calculate the percent overlap between coc and county 

intersections <- st_intersection(counties2023, coc_BOS_and_County) %>%  
  mutate(geometry = st_make_valid(geometry)) %>% 
  mutate(intersection_area = st_area(intersections) %>%  
           set_units(mi^2)) %>%
  mutate(percent_area = as.numeric(intersection_area/county_area))%>%
  st_drop_geometry()

# filter this list so it is either coc's where the overlap is virtually 100% OR
# a lower threshhold that has been manually inspected 

qualified_counties <- intersections %>%
  filter(percent_area > 0.999 | 
           (COCNUM %in%  manual_COCNUMS & percent_area > 0.6)) 

# Lower threshhold here for these ones identified through manual inspection

length(unique(qualified_counties$COCNAME)) #253 

length(unique(qualified_counties$county)) #2353

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


accounted_for_COCs <- nrow(qualified_counties_COC_check) #158 COCs have been matched this way

# STEP 2: Identify the CoCs that haven't been matched: calculate spatial intersections with tracts
# and then idenfiy what CoC contains the majority of a tract. 

missing <- coc_boundaries %>% 
  st_drop_geometry() %>%
  filter(! COCNUM %in% as.list(qualified_counties_COC_check$COCNUM)) 

unaccounted_for_coc <- nrow(missing)

nrow(coc_boundaries)
length(unique(coc_boundaries$COCNUM))

accounted_for_COCs + unaccounted_for_coc  #  381

sum(qualified_counties_COC_check$number_of_counties) # 818 counties

matched_counties <- qualified_counties %>% st_drop_geometry() %>%
  inner_join(., qualified_counties_COC_check, by="COCNUM") %>% 
  st_drop_geometry() %>%
  select(COCNUM, county_code= GEOID )

matched_counties_LIST <- as.list(matched_counties$county)

##

length(unique(missing$STATE_NAME)) # 42

states_where_we_need_tracts <- unique(missing$ST_1)

## Get TRACTS of counties we have already matched 


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

## tracts in counties matched at the county level:

tracts_matched_county_level <- all_tracts %>% 
  inner_join(., matched_counties, by = "county_code")

nrow(tracts_matched_county_level ) #30040


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
  select(-B01003_001M, -B01001A_001M, -B01001I_001M) #%>%
# rename(TotalPop = B01003_001E, White = B01001A_001E, Hispanic = B01001I_001E) 


tracts2023 <- tracts2023 %>% mutate(geometry = st_make_valid(geometry),
                                    tract_area = st_area(tracts2023) %>% set_units(mi^2)) %>%
  filter(!GEOID %in% as.list(tracts_matched_county_level $GEOID))

# Filters tracts that we have not already matched at the county level

coc_tract_intersections <- coc_boundaries %>% filter(COCNUM %in% as.list(missing$COCNUM))

# Calculates intersections 

tract_intersections <- st_intersection(tracts2023, coc_tract_intersections) 

tract_intersections <- tract_intersections %>%  mutate(geometry = st_make_valid(geometry))

tract_intersections  <- tract_intersections  %>% mutate(intersection_area = st_area(tract_intersections ) %>%  set_units(mi^2))

tract_intersections <- tract_intersections %>%
  mutate(percent_area = (as.numeric(intersection_area)/as.numeric(tract_area)))

# filters tracts at a lower threshold--

# thinking here was that every tract should get assigned to a CoC

qualified_tracts <- tract_intersections %>%
  filter(percent_area > 0.55)

qualified_tracts_check <- qualified_tracts %>%
  st_drop_geometry() %>%
  group_by(COCNAME) %>%
  dplyr::summarise(total_intersection_area = sum(intersection_area),
                   number_of_tracts = n())


coc_boundaries_check <- coc_boundaries %>% st_drop_geometry()

qualified_tracts_COC_check <- inner_join(qualified_tracts_check, coc_boundaries_check, by = "COCNAME") %>%
  mutate(coc_tract_coverage = drop_units(total_intersection_area/COCarea)) %>%
  filter(coc_tract_coverage > 0.65)

length(unique(qualified_tracts_COC_check$COCNAME)) # 197


tracts_matched_at_tract_level <- qualified_tracts %>% 
  filter(COCNAME %in% as.list(qualified_tracts_COC_check$COCNAME)) %>% 
  st_drop_geometry()

still_missing <- coc_boundaries_check %>% filter(! COCNAME %in% as.list(qualified_tracts_COC_check$COCNAME) &
                                                   ! COCNAME %in% as.list(qualified_counties_COC_check$COCNAME))


nrow(still_missing) #0

###

rbind_geoid <- rbind.fill(tracts_matched_at_tract_level, tracts_matched_county_level) %>% 
  select(COCNUM, GEOID)

#write.csv(rbind_geoid , "tract_to_COC_crosswalk.csv")
