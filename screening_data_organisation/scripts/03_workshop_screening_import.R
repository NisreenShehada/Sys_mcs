# ==========================================================
# SCREENING DATA ORGANISATION
# Script 03: Workshop Individual Screening
# ==========================================================

library(readxl)
library(dplyr)
library(tidyverse)

# STEP 1: Identify all individual screening Excel files

individual_files <- list.files(
  path = "data_raw/individual_screening",
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Exclude temporary Excel files
individual_files <- individual_files[
  !grepl("~\\$", basename(individual_files))
]

# Count files
length(individual_files)

# Display filenames
basename(individual_files)

# ==========================================================
# STEP 2: Inspect column names across all individual files
# ==========================================================

individual_file_structure <- lapply(
  individual_files,
  function(file) {
    
    data <- read_excel(file, col_types = "text")
    
    data.frame(
      file_name = basename(file),
      column_names = paste(names(data), collapse = " | "),
      n_columns = ncol(data),
      n_rows = nrow(data)
    )
  }
)

individual_file_structure <- bind_rows(
  individual_file_structure
)

# Display file structures
View(individual_file_structure)

# Check whether all files have the same number of columns
table(individual_file_structure$n_columns)

# Identify files with different column structures
individual_file_structure %>% 
  filter(n_columns !=13)

# Inspect column names for every individual screening file

individual_column_names <- lapply(
  individual_files,
  function(file) {
    data <- read_excel(file, col_types = "text")
    
    data.frame(
      file_name = basename(file),
      column_number = seq_along(names(data)),
      column_name = names(data)
    )
  }
)

individual_column_names <- bind_rows(
  individual_column_names
)

View(individual_column_names)

# Inspect the additional column in Sheet 11
sheet11_extra <- read_excel(
  individual_files[
    grepl("Sheet 11 - Individual screening", basename(individual_files))
  ],
  col_types = "text"
)

table(sheet11_extra$...14, useNA = "always")

# Inspect the additional column in Sheet 14
sheet14_extra <- read_excel(
  individual_files[
    grepl("Sheet 14 - Individual screening", basename(individual_files))
  ],
  col_types = "text"
)

table(sheet14_extra$...14, useNA = "always")


# ==========================================================
# STEP 3: Import and combine all individual screening files
# ==========================================================

individual_data_list <- lapply(
  seq_along(individual_files),
  function(i) {
    
    # Import each Excel file
    data <- read_excel(
      individual_files[i],
      col_types = "text"
    )
    
    # Rename the additional unnamed column, if present
    if ("...14" %in% names(data)) {
      data <- data %>%
        rename(additional_screening_notes = `...14`)
    }
    
    # Add source information
    data <- data %>%
      mutate(
        source_file = basename(individual_files[i]),
        source_path = individual_files[i]
      )
    
    data
  }
)

# Combine all files into one dataset
workshop_individual_raw <- bind_rows(individual_data_list)

# Check the combined dataset
dim(workshop_individual_raw)

# View the dataset
View(workshop_individual_raw)

# Check the column names
names(workshop_individual_raw)

# ==========================================================
# STEP 4: Inspect combined dataset columns

names(workshop_individual_raw)

# Check the screening decisions recorded

table(
  workshop_individual_raw$Decision,
  useNA = "always"
)

# Check missing record numbers

sum(
  is.na(workshop_individual_raw$rec_number) |
    trimws(workshop_individual_raw$rec_number) == ""
)


# ==========================================================
# STEP 5: Match individual screening entries to meta sheet
# ==========================================================

# Add meta-sheet information to each screening entry

workshop_individual_matched <- workshop_individual_raw %>%
  left_join(
    meta_data %>%
      select(
        rec_number,
        meta_title = title,
        meta_year = year,
        Assigned,
        screening_status
      ),
    by = "rec_number"
  ) %>%
  mutate(
    meta_match = !is.na(meta_title)
  )

# Check dimensions
dim(workshop_individual_matched)

# View matched dataset
View(workshop_individual_matched)

# ==========================================================
# STEP 6: Summarise matching results
# ==========================================================

# Matched versus unmatched entries
workshop_individual_matched %>%
  count(meta_match)

# Assignment status among matched entries
workshop_individual_matched %>%
  filter(meta_match) %>%
  count(Assigned, useNA = "always")

# Screening decisions among matched entries
workshop_individual_matched %>%
  filter(meta_match) %>%
  count(Decision, useNA = "always")

# ==========================================================
# STEP 7: Identify unmatched screening records
# ==========================================================

unmatched_individual <- workshop_individual_matched %>%
  filter(!meta_match) %>%
  select(
    rec_number,
    Title,
    Decision,
    `Screener number`,
    source_file
  )

# View unmatched records
View(unmatched_individual)

# Count unmatched records by source file
unmatched_individual %>%
  count(source_file, Decision)

# ==========================================================
# STEP 9: Set aside unmatched records
# ==========================================================

unmatched_removed_candidates <- workshop_individual_matched %>%
  filter(!meta_match) %>%
  mutate(
    unmatched_reason = "Potentially removed during HRP-country correction"
  )

# Check the records
nrow(unmatched_removed_candidates)

# Summarise by decision
unmatched_removed_candidates %>%
  count(Decision)

# Summarise by source file
unmatched_removed_candidates %>%
  count(source_file, Decision)

# ==========================================================
# STEP 8: Identify repeated record numbers
# ==========================================================

individual_record_frequency <- workshop_individual_matched %>%
  count(rec_number, name = "number_of_entries") %>%
  arrange(desc(number_of_entries))

# Identify records appearing more than once
repeated_individual_records <- individual_record_frequency %>%
  filter(number_of_entries > 1)

# Summary
nrow(repeated_individual_records)

# View repeated records
View(repeated_individual_records)

# ==========================================================
# STEP 10: Inspect repeated screening records
# ==========================================================

repeated_individual_details <- workshop_individual_matched %>%
  semi_join(
    repeated_individual_records,
    by = "rec_number"
  ) %>%
  arrange(rec_number, source_file) %>%
  select(
    rec_number,
    Title,
    Decision,
    `Screener number`,
    source_file,
    Assigned
  )

View(repeated_individual_details)

# Inspect the copied Sheet 31

copy_sheet31 <- workshop_individual_raw %>%
  filter(source_file == "Copy of Sheet 31 - Individual screening.xlsx")

# Check number of entries
nrow(copy_sheet31)

# Check decisions
table(copy_sheet31$Decision, useNA = "always")

# Check whether record numbers are duplicated
sum(duplicated(copy_sheet31$rec_number))

# ==========================================================
# STEP 11: Exclude empty duplicate copy of Sheet 31
# ==========================================================

workshop_individual_raw_clean <- workshop_individual_raw %>%
  filter(
    source_file != "Copy of Sheet 31 - Individual screening.xlsx"
  )

# Check the new dimensions
dim(workshop_individual_raw_clean)

# Check decisions after excluding the copy
table(
  workshop_individual_raw_clean$Decision,
  useNA = "always"
)

# ==========================================================
# STEP 12: Match cleaned individual screening data
# ==========================================================

workshop_individual_matched_clean <- workshop_individual_raw_clean %>%
  left_join(
    meta_data %>%
      select(
        rec_number,
        meta_title = title,
        meta_year = year,
        Assigned,
        screening_status
      ),
    by = "rec_number"
  ) %>%
  mutate(
    meta_match = !is.na(meta_title)
  )

# Check dimensions
dim(workshop_individual_matched_clean)

# Check matching
workshop_individual_matched_clean %>%
  count(meta_match)

# Check assignment status
workshop_individual_matched_clean %>%
  filter(meta_match) %>%
  count(Assigned, .drop = FALSE)

# Check decisions among matched entries
workshop_individual_matched_clean %>%
  filter(meta_match) %>%
  count(Decision)

# Count unique record numbers in the cleaned dataset

n_distinct(
  workshop_individual_matched_clean$rec_number
)

# Count unique matched record numbers

workshop_individual_matched_clean %>%
  filter(meta_match) %>%
  summarise(
    unique_matched_papers = n_distinct(rec_number)
  )

# ==========================================================
# STEP 13: Check remaining repeated records
# ==========================================================

repeated_records_clean <- workshop_individual_matched_clean %>%
  count(rec_number, name = "number_of_entries") %>%
  filter(number_of_entries > 1)

# Number of repeated record numbers
nrow(repeated_records_clean)

# Inspect their screening decisions and sources
repeated_details_clean <- workshop_individual_matched_clean %>%
  semi_join(repeated_records_clean, by = "rec_number") %>%
  arrange(rec_number, source_file) %>%
  select(
    rec_number,
    Title,
    Decision,
    `Screener number`,
    source_file,
    Assigned
  )

View(repeated_details_clean)

# ==========================================================
# STEP 14: Summarise records screened by multiple screeners
# ==========================================================

repeated_screening_summary <- repeated_details_clean %>%
  group_by(rec_number, Title) %>%
  summarise(
    number_of_screeners = n_distinct(`Screener number`),
    decisions_recorded = paste(
      sort(unique(Decision)),
      collapse = " / "
    ),
    agreement = if_else(
      n_distinct(Decision) == 1,
      "Agreement",
      "Disagreement"
    ),
    .groups = "drop"
  )

View(repeated_screening_summary)

# Summarise agreement
repeated_screening_summary %>%
  count(agreement)


# ==========================================================
# STEP 15: Create final workshop individual-screening dataset
# ==========================================================

# Summarise individual screening entries by unique record number

individual_screening_summary <- workshop_individual_raw_clean %>%
  group_by(rec_number) %>%
  summarise(
    individual_screening_entries = n(),

    screeners = paste(
      sort(unique(na.omit(`Screener number`))),
      collapse = " | "
    ),

    screening_decisions = paste(
      sort(unique(na.omit(Decision))),
      collapse = " | "
    ),

    include_entries = sum(Decision == "INCLUDE", na.rm = TRUE),
    exclude_entries = sum(Decision == "EXCLUDE", na.rm = TRUE),
    not_sure_entries = sum(Decision == "NOT SURE", na.rm = TRUE),

    source_files = paste(
      sort(unique(source_file)),
      collapse = " | "
    ),

    screening_title = first(na.omit(Title)),
    .groups = "drop"
  )

# Match the screening summary to the complete meta sheet
# A full join retains records from BOTH sources

workshop_individual_final <- full_join(
  meta_data %>%
    select(
      rec_number,
      meta_title = title,
      meta_year = year,
      Assigned,
      screening_status
    ),

  individual_screening_summary,

  by = "rec_number"
) %>%
  mutate(
    meta_match_status = case_when(
      !is.na(meta_title) ~ "Matched to meta sheet",
      TRUE ~ "Not found in meta sheet"
    ),

    individual_screening_status = case_when(
      !is.na(individual_screening_entries) ~
        "Individual screening record found",
      TRUE ~ "No individual screening entry found"
    ),

    paper_title = coalesce(meta_title, screening_title)
  )

# ==========================================================
# STEP 16: Check the final dataset
# ==========================================================

# Dimensions
dim(workshop_individual_final)

# Check unique record numbers
n_distinct(workshop_individual_final$rec_number)

# Check meta-sheet matching categories
workshop_individual_final %>%
  count(meta_match_status)

# Check individual screening presence
workshop_individual_final %>%
  count(individual_screening_status)

# View final dataset
View(workshop_individual_final)

dim(workshop_individual_final)

workshop_individual_final %>%
  count(meta_match_status, individual_screening_status)

# Check the seven unmatched records
workshop_individual_final %>%
  filter(meta_match_status == "Not found in meta sheet") %>%
  select(rec_number, paper_title, screening_decisions, source_files)


# Rebuild the final individual screening dataset

meta_data_marked <- meta_data %>%
  select(
    rec_number,
    meta_title = title,
    meta_year = year,
    Assigned,
    screening_status
  ) %>%
  mutate(meta_record_present = TRUE)

workshop_individual_final <- full_join(
  meta_data_marked,
  individual_screening_summary,
  by = "rec_number"
) %>%
  mutate(
    meta_match_status = case_when(
      !is.na(meta_record_present) ~ "Matched to meta sheet",
      TRUE ~ "Not found in meta sheet"
    ),
    
    individual_screening_status = case_when(
      !is.na(individual_screening_entries) ~
        "Individual screening record found",
      TRUE ~ "No individual screening entry found - check double screening"
    ),
    
    paper_title = coalesce(meta_title, screening_title)
  )

# Check the classification
workshop_individual_final %>%
  count(meta_match_status, individual_screening_status)

# Check dimensions
dim(workshop_individual_final)

# ==========================================================
# STEP 18: Save individual screening datasets
# ==========================================================

# Save the complete comparison dataset
write.csv(
  workshop_individual_final,
  "data_processed/workshop_individual_final.csv",
  row.names = FALSE,
  na = ""
)

# Save the seven unmatched records separately
unmatched_individual_final <- workshop_individual_final %>%
  filter(meta_match_status == "Not found in meta sheet")

write.csv(
  unmatched_individual_final,
  "data_processed/unmatched_individual_records.csv",
  row.names = FALSE,
  na = ""
)


# Check individual screening summary
dim(individual_screening_summary)
nrow(individual_screening_summary) == 

# Check duplicate record numbers
sum(duplicated(individual_screening_summary$rec_number))

# Inspect its columns
names(individual_screening_summary)


# Check whether screener information is missing
individual_screening_summary %>%
  summarise(
    total_records = n(),
    missing_screeners = sum(
      is.na(screeners) | trimws(screeners) == ""
    ),
    records_with_screeners = sum(
      !is.na(screeners) & trimws(screeners) != ""
    )
  )


workshop_individual_raw_clean %>%
  select(rec_number, `Screener number`, Decision, source_file) %>%
  head(15)

