# CoC to Census Tract Crosswalk

This repository contains an R script that creates a CSV crosswalk between

Continuums of Care (CoCs) and U.S. Census Tracts. This code employees the most up to date boundaries available during its development; 2024 CoC boundaries and 2023 Census boundaries.  

The final output uniquely assigns each Census Tract to a single CoC. In cases where a Census Tract spans multiple CoCs, the tract is assigned to the CoC that contains the largest share of the tract’s land area.

---

## Methodology

The crosswalk is created using a progressive approach designed to reduce processing time by leveraging the nesting structure of Census geographies.

The process works as follows:

1. CoCs whose boundaries we expect to align exactly with county or state boundaries are identified first.
2. The intersection of these CoCs with counties is calculated.
3. The validity of these county-level matches is checked.
4. Remaining CoCs are matched directly to Census Tracts using a spatial interesection function.
5. For Census Tracts that intersect multiple CoCs, assignment is based on the CoC containing the majority of the tract’s area.
6. Two joins are then performed, first to enumerate the tracts that each county contains (for the CoC’s matched to counties) and then to create a full national crosswalk by combining the separate matches together. 

This approach reduces processing time compared to matching all CoCs directly to Census Tracts.

---

## Inputs

The script expects the following spatial inputs:

- 2024 CoC boundary Esri File Geodatabase (included in this repository, available through HUD Exchange > CoC GIS Tools) 
- Unique Census API KEY
- Census Tract boundary shapefile 
- County boundary shapefile

**Note:** Census data are not included in this repository. Users must load these through Tidy Census. 

---

## Output

The script produces a CSV file containing a unique mapping of Census Tracts
to CoCs.

Fields include:
- Census Tract GEOID
- CoC identifier

---

## Usage

1. Ensure all required input files are available locally.
2. Update file paths in the script as needed.
3. Run the script in R:

```r
source("R/create_crosswalk.R")