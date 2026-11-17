# ==========================================
# SCREENING DATA ORGANISATION
# Script 01: Import Meta Sheet
# ==========================================

# Load the required package
library(readxl)
library(dplyr)
library(tidyverse)

# Find the the meta_sheet 
list.files("data_raw/meta_sheet")

meta_files <- list.files(
  path = "data_raw/meta_sheet",
  pattern = "^[^~].*\\.xlsx?$",
  full.names = TRUE,
  ignore.case = TRUE
)

print(meta_files)

# read the sheet
meta_data <- read_excel(
  meta_files[1],
  sheet = 1,
  col_types = "text"
)

#view the dataset
View(meta_data)

# Display column names
names(meta_data)

# Count the rows
nrow(meta_data)

# Count the columns
ncol(meta_data)

glimpse(meta_data)

# ==========================================
# STEP 2: Explore the Meta Sheet
# ==========================================

# Check the assigned column 
table(meta_data$Assigned, useNA = "always")

# Check the four reviewer columns
table(meta_data$Nisreen, useNA = "always")
table(meta_data$Paula, useNA = "always")
table(meta_data$OJ, useNA = "always")
table(meta_data$Bhargavi, useNA = "always")

# ==========================================
# STEP 4: Check reviewer assignment overlaps
# ==========================================

reviewer_assignments <- meta_data %>%
  transmute(
    rec_number,
    Nisreen = !is.na(Nisreen) & toupper(trimws(Nisreen)) == "TRUE",
    Paula = !is.na(Paula) & toupper(trimws(Paula)) == "TRUE",
    OJ = !is.na(OJ) & toupper(trimws(OJ)) == "TRUE",
    Bhargavi = !is.na(Bhargavi) & toupper(trimws(Bhargavi)) == "TRUE"
  )

# 1. Compare Nisreen and Paula

table(
  Nisreen = reviewer_assignments$Nisreen,
  Paula = reviewer_assignments$Paula
)

# 2. Compare OJ and Bhargavi

table(
  OJ = reviewer_assignments$OJ,
  Bhargavi = reviewer_assignments$Bhargavi
)

# 3. Calculate assignment overlaps and differences

reviewer_assignments %>%
  summarise(
    Nisreen_only = sum(Nisreen & !Paula),
    Paula_only = sum(Paula & !Nisreen),
    Nisreen_and_Paula = sum(Nisreen & Paula),
    OJ_only = sum(OJ & !Bhargavi),
    Bhargavi_only = sum(Bhargavi & !OJ),
    OJ_and_Bhargavi = sum(OJ & Bhargavi)
  )

# 4. Total unique records assigned to at least one reviewer

sum(
  reviewer_assignments$Nisreen |
    reviewer_assignments$Paula |
    reviewer_assignments$OJ |
    reviewer_assignments$Bhargavi
)


# ==========================================
# STEP 5: Check record number quality
# ==========================================

# Check for missing record numbers
sum(is.na(meta_data$rec_number) | trimws(meta_data$rec_number) == "")

# Check for duplicated record numbers
sum(duplicated(meta_data$rec_number))

# View duplicated record numbers, if any
meta_data %>%
  count(rec_number) %>%
  filter(n > 1)

# ==========

# ==========================================
# STEP 6: Create four-reviewer audit table
# ==========================================

# Identify records assigned to at least one reviewer

four_reviewer_records <- reviewer_assignments %>%
  filter(Nisreen | Paula | OJ | Bhargavi)

# Keep the record number and reviewer assignment columns
four_reviewer_records <- four_reviewer_records %>%
  select(rec_number, Nisreen, Paula, OJ, Bhargavi)

# Add basic study information from the meta sheet
four_reviewer_audit <- four_reviewer_records %>%
  left_join(
    meta_data %>%
      select(rec_number, title, journal_name, year),
    by = "rec_number"
  )

# Check the number of records
nrow(four_reviewer_audit)

# View the audit table
View(four_reviewer_audit)

# Check the assignment combinations
four_reviewer_audit %>%
  count(Nisreen, Paula, OJ, Bhargavi)


#====
# Individual screening files
list.files(
  "data_raw/individual_screening",
  recursive = TRUE
)

# Double-screening files
list.files(
  "data_raw/double_screening",
  recursive = TRUE
)


