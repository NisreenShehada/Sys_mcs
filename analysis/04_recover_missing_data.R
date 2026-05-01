# 1. Load packages
library(dplyr)
library(readr)

# 2. Source helper functions
source("R/endnote_helpers.R")

# 3. Define paths
included_rds_path <- "analysis/data-derived/included_studies.rds"
full_endnote_path <- "~/Desktop/PhD files /1st year/Systematic review /endnote/Mortality in Crisis.enl"
output_csv_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.csv"
output_rds_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.rds"

# 4. Read included studies
included_df <- readRDS(included_rds_path)

# 5. Read full EndNote library
refs_df_new <- read_endnote_refs("~/Desktop/PhD files /1st year/Systematic review /endnote/Mortality in Crisis.enl")

# 6. Extract DOI lookup table
doi_lookup_df_new <- extract_endnote_dois(refs_df_new)

# 7. Match included studies to full EndNote library by record number
included_df_id <- included_df %>%
  mutate(rec_number = as.character(rec_number))

doi_lookup_df_new_id <- doi_lookup_df_new %>%
  mutate(id = as.character(id))

included_with_doi_by_id <- included_df_id %>%
  left_join(
    doi_lookup_df_new_id,
    by = c("rec_number" = "id"),
    suffix = c("_screening", "_endnote")
  )

# 8. Check if matching worked
cat("Included rows:", nrow(included_df_id), "\n")
cat("Rows after ID join:", nrow(included_with_doi_by_id), "\n")
cat("Rows matched by ID:",
    sum(!is.na(included_with_doi_by_id$title_endnote)), "\n")
cat("Rows with DOI:",
    sum(!is.na(included_with_doi_by_id$doi)), "\n")

# 9. Identify studies still missing DOI after full EndNote match
missing_doi_after_full_endnote_df <- included_with_doi_by_id %>%
  filter(is.na(doi))

cat("Rows still missing DOI:",
    nrow(missing_doi_after_full_endnote_df), "\n")

# 10. Checking for duplicates:
sum(duplicated(included_with_doi_by_id$rec_number))
sum(duplicated(missing_doi_after_full_endnote_df$record_index))
included_with_doi_by_id <- included_with_doi_by_id %>%
  mutate(title_clean = tolower(trimws(title_screening)))
sum(duplicated(included_with_doi_by_id$title_clean))

# 11. Inspect duplicates
included_with_doi_by_id %>%
  count(title_clean) %>%
  filter(n > 1) %>%
  arrange(desc(n))

included_with_doi_by_id %>%
  filter(title_clean == "some_example_title")
# 10. Save updated DOI dataset
write_csv(included_with_doi_by_id, output_csv_path)
saveRDS(included_with_doi_by_id, output_rds_path)
