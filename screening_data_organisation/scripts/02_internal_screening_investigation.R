
# ==========================================================
# SCREENING DATA ORGANISATION
# Script 02: Internal Screening Investigation
# ==========================================================

# This script investigates the internal screening exercise
# conducted by Nisreen, Paula, OJ and Bhargavi.
#
# Screening decisions are recorded directly in the meta sheet.
# The aim is to identify assigned records, reviewer decisions,
# double-screening outcomes and records without decisions.
#
# Original Excel files remain unchanged.


# ==========================================================
# STEP 1: Load packages
# ==========================================================

library(readxl)
library(dplyr)
library(tidyverse)


# ==========================================================
# STEP 2: Import the meta sheet
# ==========================================================

meta_files <- list.files(
  path = "data_raw/meta_sheet",
  pattern = "^[^~].*\\.xlsx?$",
  full.names = TRUE,
  ignore.case = TRUE
)

print(meta_files)

meta_data <- read_excel(
  meta_files[1],
  sheet = 1,
  col_types = "text"
)


# ==========================================================
# STEP 3: Prepare reviewer assignment information
# ==========================================================

reviewer_assignments <- meta_data %>%
  transmute(
    rec_number,
    Nisreen = !is.na(Nisreen) &
      toupper(trimws(Nisreen)) == "TRUE",
    Paula = !is.na(Paula) &
      toupper(trimws(Paula)) == "TRUE",
    OJ = !is.na(OJ) &
      toupper(trimws(OJ)) == "TRUE",
    Bhargavi = !is.na(Bhargavi) &
      toupper(trimws(Bhargavi)) == "TRUE"
  )


# ==========================================================
# STEP 4: Identify the 443 internal reviewer records
# ==========================================================

four_reviewer_records <- reviewer_assignments %>%
  filter(Nisreen | Paula | OJ | Bhargavi) %>%
  select(rec_number, Nisreen, Paula, OJ, Bhargavi)

# Check number of unique records
nrow(four_reviewer_records)


# ==========================================================
# STEP 5: Create the internal screening master dataset
# ==========================================================

internal_screening_master <- four_reviewer_records %>%
  left_join(
    meta_data %>%
      select(
        rec_number,
        title,
        journal_name,
        year,
        Nisreen_include,
        Nisreen_comments,
        Paula_include,
        Paula_comments,
        OJ_include,
        OJ_comments,
        Bhargavi_include,
        Bhargavi_comments,
        `Additional Comments`,
        OJ_notes
      ),
    by = "rec_number"
  )

# Check dataset dimensions
nrow(internal_screening_master)
ncol(internal_screening_master)

# View the dataset
View(internal_screening_master)


# ==========================================================
# STEP 6: Explore internal reviewer decisions
# ==========================================================

table(
  internal_screening_master$Nisreen_include,
  useNA = "always"
)

table(
  internal_screening_master$Paula_include,
  useNA = "always"
)

table(
  internal_screening_master$OJ_include,
  useNA = "always"
)

table(
  internal_screening_master$Bhargavi_include,
  useNA = "always"
)


# ==========================================================
# STEP 7: Summarise decisions by reviewer
# ==========================================================

reviewer_decision_summary <- internal_screening_master %>%
  summarise(
    total_records = n(),
    
    Nisreen_included =
      sum(Nisreen_include == "INCLUDE", na.rm = TRUE),
    Nisreen_excluded =
      sum(Nisreen_include == "EXCLUDE", na.rm = TRUE),
    Nisreen_not_reviewed =
      sum(is.na(Nisreen_include)),
    
    Paula_included =
      sum(Paula_include == "INCLUDE", na.rm = TRUE),
    Paula_excluded =
      sum(Paula_include == "EXCLUDE", na.rm = TRUE),
    Paula_not_sure =
      sum(Paula_include == "NOT SURE", na.rm = TRUE),
    Paula_not_reviewed =
      sum(is.na(Paula_include)),
    
    OJ_included =
      sum(OJ_include == "INCLUDE", na.rm = TRUE),
    OJ_excluded =
      sum(OJ_include == "EXCLUDE", na.rm = TRUE),
    OJ_not_reviewed =
      sum(is.na(OJ_include)),
    
    Bhargavi_included =
      sum(Bhargavi_include == "INCLUDE", na.rm = TRUE),
    Bhargavi_excluded =
      sum(Bhargavi_include == "EXCLUDE", na.rm = TRUE),
    Bhargavi_not_reviewed =
      sum(is.na(Bhargavi_include))
  )

print(reviewer_decision_summary)


# ==========================================================
# STEP 8: Investigate double-screening pairs
# ==========================================================

# Nisreen and Paula
internal_screening_master %>%
  count(Nisreen_include, Paula_include)

# OJ and Bhargavi
internal_screening_master %>%
  count(OJ_include, Bhargavi_include)


# Inspect the internal screening dataset

names(internal_screening_master)

# Check its dimensions
dim(internal_screening_master)

# Check duplicate record numbers
sum(duplicated(internal_screening_master$rec_number))


# ==========================================================
# END OF INITIAL INTERNAL SCREENING INVESTIGATION
# ==========================================================
