# analysis/02_fetch_dois.R

# 1. Load packages
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)

# 2. Source helper functions
source("R/endnote_helpers.R")

# 3. Define Paths
included_rds_path <- "analysis/data-derived/included_studies.rds"
endnote_enl_path <- "~/Desktop/PhD files /1st year/Systematic review /endnote/Imported References.enl"

output_csv_path <- "analysis/data-derived/included_studies_with_doi.csv"
output_rds_path <- "analysis/data-derived/included_studies_with_doi.rds"

# 4. Check .enl file tables

file.exists(endnote_enl_path)
file.info(endnote_enl_path)$size
con <- dbConnect(SQLite(), endnote_enl_path)
dbListTables(con)
dbDisconnect(con)

# 4. Read included studies
included_df <- readRDS(included_rds_path)

# 5. Read EndNote references
refs_df <- read_endnote_refs(endnote_enl_path)

# 6. Extract DOI lookup table
doi_lookup_df <- extract_endnote_dois(refs_df)

# 7. Check key columns before joining
names(included_df)
names(doi_lookup_df)

# 8. Join DOI data
included_df <- included_df %>%
  mutate(rec_number = as.character(rec_number))

doi_lookup_df <- doi_lookup_df %>%
  mutate(id = as.character(id))

included_with_doi_df <- included_df %>%
  left_join(
    doi_lookup_df,
    by = c("rec_number" = "id")
  )

# 9. Run checks
names(included_with_doi_df)
cat("Included rows:", nrow(included_df), "\n")
cat("Rows after join:", nrow(included_with_doi_df), "\n")
cat("Rows with DOI:", sum(!is.na(included_with_doi_df$doi)), "\n")
included_with_doi_df %>%
  select(rec_number, title_screening, doi, doi_source_field) %>%
  filter(!is.na(doi)) %>%
  head()

write_csv(included_with_doi_df, output_csv_path)
saveRDS(included_with_doi_df, output_rds_path)
