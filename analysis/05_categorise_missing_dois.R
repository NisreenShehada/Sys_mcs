# 1. Load packages
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# 2. Define paths
input_rds_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.rds"

missing_doi_csv_path <- "analysis/data-derived/missing_doi_categorised.csv"
missing_doi_url_type_summary_path <- "analysis/data-derived/missing_doi_url_type_summary.csv"
missing_doi_ovid_database_summary_path <- "analysis/data-derived/missing_doi_ovid_database_summary.csv"
missing_doi_study_type_summary_path <- "analysis/data-derived/missing_doi_study_type_summary.csv"

# 3. Read DOI dataset
df <- readRDS(input_rds_path)

# 4. Keep rows still missing DOI
missing_doi_df <- df %>%
  filter(is.na(doi))

cat("Rows missing DOI:", nrow(missing_doi_df), "\n")

# 5. Split URL field into one row per URL
missing_doi_urls_long <- missing_doi_df %>%
  mutate(url = str_replace_all(url, "\r", ";")) %>%
  separate_rows(url, sep = ";") %>%
  mutate(
    url = trimws(url),
    url_lower = str_to_lower(url)
  )

# 6. Categorise URL types
missing_doi_urls_long <- missing_doi_urls_long %>%
  mutate(
    url_type = case_when(
      is.na(url_lower) | url_lower == "" ~ "No URL recorded",
      str_detect(url_lower, "ovidsp") ~ "Ovid platform link",
      str_detect(url_lower, "primo-explore|exlibrisgroup") ~ "Library resolver link",
      str_detect(url_lower, "doi.org") ~ "Direct DOI link",
      str_detect(url_lower, "clinicaltrials.gov|isrctn|trialsearch|pactr|chictr|anzctr") ~ "Clinical trial registry",
      str_detect(url_lower, "who.int") ~ "WHO / report",
      str_detect(url_lower, "pdf") ~ "Direct PDF link",
      TRUE ~ "Other publisher / unknown"
    )
  )

# 7. Summarise URL types
missing_doi_url_type_summary <- missing_doi_urls_long %>%
  count(url_type, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

print(missing_doi_url_type_summary)

# 8. Extract Ovid database patterns
ovid_patterns_df <- missing_doi_urls_long %>%
  filter(str_detect(url_lower, "ovidsp")) %>%
  mutate(
    ovid_database = str_extract(url, "D=[^&]+"),
    ovid_accession_number = str_extract(url, "AN=[^&]+"),
    database_group = case_when(
      str_detect(ovid_database, "cagh") ~ "CAB Abstracts",
      str_detect(ovid_database, "emctr") ~ "Embase Conference Abstracts",
      str_detect(ovid_database, "emed") ~ "Embase",
      TRUE ~ "Other"
    )
  )

# 9. Summarise Ovid database groups
ovid_database_summary <- ovid_patterns_df %>%
  count(database_group, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

print(ovid_database_summary)

# 10. Optional: classify broad study type from title
missing_doi_study_type_summary <- missing_doi_df %>%
  mutate(
    title_lower = str_to_lower(coalesce(title_screening, "")),
    study_type = case_when(
      str_detect(title_lower, "protocol") ~ "Protocol",
      str_detect(title_lower, "trial") ~ "Trial",
      str_detect(title_lower, "randomised|randomized") ~ "Randomised study",
      str_detect(title_lower, "systematic review|meta-analysis") ~ "Systematic review",
      str_detect(title_lower, "review") ~ "Review",
      str_detect(title_lower, "modelling|modeling") ~ "Modelling study",
      str_detect(title_lower, "outbreak") ~ "Outbreak report",
      str_detect(title_lower, "epidemiolog") ~ "Epidemiological study",
      str_detect(title_lower, "case report") ~ "Case report",
      TRUE ~ "Other / unclear"
    )
  ) %>%
  count(study_type, sort = TRUE)

print(missing_doi_study_type_summary)

# 11. Create final categorised missing DOI table
missing_doi_categorised_df <- missing_doi_df %>%
  mutate(
    url_lower = str_to_lower(coalesce(url, "")),
    missing_doi_reason = case_when(
      str_detect(url_lower, "ovidsp") ~ "Ovid database record without DOI",
      str_detect(url_lower, "primo-explore|exlibrisgroup") ~ "Library resolver record without DOI",
      url_lower == "" ~ "No URL recorded",
      TRUE ~ "Other / needs manual review"
    )
  )

# 12. Save outputs
write_csv(missing_doi_categorised_df, missing_doi_csv_path)
write_csv(missing_doi_url_type_summary, missing_doi_url_type_summary_path)
write_csv(ovid_database_summary, missing_doi_ovid_database_summary_path)
write_csv(missing_doi_study_type_summary, missing_doi_study_type_summary_path)

cat("Saved missing DOI categorised file:", missing_doi_csv_path, "\n")
cat("Saved URL type summary:", missing_doi_url_type_summary_path, "\n")
cat("Saved Ovid database summary:", missing_doi_ovid_database_summary_path, "\n")
cat("Saved study type summary:", missing_doi_study_type_summary_path, "\n")

