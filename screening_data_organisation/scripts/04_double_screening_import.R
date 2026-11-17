library(janitor)

# ==========================================
# STEP 1: List all double-screening files
# ==========================================

double_files <- list.files(
  path = "data_raw/double_screening",
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Check the number of files
length(double_files)

# Display their names and folder paths
data.frame(
  file_name = basename(double_files),
  folder = dirname(double_files)
)


# ==========================================
# STEP 2: Inspect double-screening file structures
# ==========================================

double_file_structure <- lapply(double_files, function(file) {
  
  data <- read_excel(file, col_types = "text")
  
  data.frame(
    file_name = basename(file),
    folder = basename(dirname(file)),
    column_names = paste(names(data), collapse = " | "),
    n_columns = ncol(data),
    n_rows = nrow(data)
  )
}) %>%
  bind_rows()

# View the file structures
View(double_file_structure)

# Check variation in the number of columns
table(double_file_structure$n_columns)

# Identify files with different structures
double_file_structure %>%
  filter(n_columns != max(n_columns))

# Identify the largest files
double_file_structure %>%
  arrange(desc(n_rows)) %>%
  select(file_name, folder, n_rows)

# Identify the file with the most rows
largest_file <- double_files[
  which.max(double_file_structure$n_rows)
]

# Print its path
largest_file

# Import it for inspection
large_double_sheet <- read_excel(
  largest_file,
  col_types = "text"
)

# Inspect its first 10 rows
head(large_double_sheet, 10)

# Check its decisions
table(large_double_sheet$Decision, useNA = "always")

# Check the Decision Compared column
table(large_double_sheet$`Decision Compared`, useNA = "always")

# Import Sheet 1A
sheet1A <- read_excel(
  double_files[
    grepl("Sheet 1A.xlsx", double_files, fixed = TRUE)
  ],
  col_types = "text"
)

# Check dimensions
dim(sheet1A)

# Inspect the first 10 records
head(sheet1A, 10)

# Inspect the last 10 records
tail(sheet1A, 10)

# Count non-empty record numbers
sum(
  !is.na(sheet1A$rec_number) &
    trimws(sheet1A$rec_number) != ""
)

# Count non-empty decisions
sum(
  !is.na(sheet1A$Decision) &
    trimws(sheet1A$Decision) != ""
)

# Count rows with both a record number and a decision
sheet1A %>%
  filter(
    !is.na(rec_number),
    trimws(rec_number) != "",
    !is.na(Decision),
    trimws(Decision) != ""
  ) %>%
  nrow()

# Check if 1B and 1A have the same record, Import Sheet 1B
sheet1B <- read_excel(
  "data_raw/double_screening/Sheet 1/Sheet 1B.xlsx",
  col_types = "text"
)

# Keep only records with actual decisions
sheet1A_clean <- sheet1A %>%
  filter(
    !is.na(rec_number),
    trimws(rec_number) != "",
    !is.na(Decision),
    trimws(Decision) != ""
  )

sheet1B_clean <- sheet1B %>%
  filter(
    !is.na(rec_number),
    trimws(rec_number) != "",
    !is.na(Decision),
    trimws(Decision) != ""
  )

# Compare the number of records
nrow(sheet1A_clean)
nrow(sheet1B_clean)

# Compare record numbers
length(intersect(
  sheet1A_clean$rec_number,
  sheet1B_clean$rec_number
))

# Records in A but not B
only_in_A <- setdiff(
  sheet1A_clean$rec_number,
  sheet1B_clean$rec_number
)

# Records in B but not A
only_in_B <- setdiff(
  sheet1B_clean$rec_number,
  sheet1A_clean$rec_number
)

only_in_A
only_in_B

# Check duplicates within each reviewer sheet
sum(duplicated(sheet1A_clean$rec_number))
sum(duplicated(sheet1B_clean$rec_number))

sheet1A_clean %>%
  select(rec_number, decision_A = Decision) %>%
  inner_join(
    sheet1B_clean %>%
      select(rec_number, decision_B = Decision),
    by = "rec_number"
  ) %>%
  count(decision_A, decision_B)

# Standardise record numbers in both sheets

sheet1A_clean <- sheet1A_clean %>%
  mutate(
    rec_number = sub("\\.0$", "", trimws(rec_number))
  )

sheet1B_clean <- sheet1B_clean %>%
  mutate(
    rec_number = sub("\\.0$", "", trimws(rec_number))
  )

# Records in A but not B
only_in_A <- setdiff(
  sheet1A_clean$rec_number,
  sheet1B_clean$rec_number
)

# Records in B but not A
only_in_B <- setdiff(
  sheet1B_clean$rec_number,
  sheet1A_clean$rec_number
)

only_in_A
only_in_B

# Number of matching records
length(intersect(
  sheet1A_clean$rec_number,
  sheet1B_clean$rec_number
))

# Compare all 20 reviewer decisions

sheet1A_clean %>%
  select(rec_number, decision_A = Decision) %>%
  inner_join(
    sheet1B_clean %>%
      select(rec_number, decision_B = Decision),
    by = "rec_number"
  ) %>%
  count(decision_A, decision_B)

# Confirm that all 20 records are represented
sheet1A_clean %>%
  inner_join(
    sheet1B_clean,
    by = "rec_number"
  ) %>%
  summarise(
    matched_records = n_distinct(rec_number),
    reviewer_A_decisions = sum(!is.na(Decision.x)),
    reviewer_B_decisions = sum(!is.na(Decision.y))
  )


# ==========================================================
# STEP 3: Import and clean all double-screening files
# ==========================================================

# Import every Excel file
double_data_list <- lapply(double_files, function(file) {
  
  data <- read_excel(
    file,
    col_types = "text",
    .name_repair = "unique"
  )
  
  # Check that every file has the expected 14 columns
  if (ncol(data) != 14) {
    stop(paste(
      "Unexpected number of columns in:",
      basename(file),
      "- found", ncol(data), "columns"
    ))
  }
  
  # Standardise column names
  names(data) <- c(
    "rec_number",
    "title",
    "journal_name",
    "year",
    "abstract",
    "doi",
    "pdf_link",
    "pdf_link_2",
    "screener_number",
    "decision",
    "exclusion_reason",
    "notes",
    "lit_review",
    "decision_compared"
  )
  
  data %>%
    mutate(
      source_file = basename(file),
      screening_batch = basename(dirname(file)),
      reviewer_sheet = case_when(
        grepl("A\\.xlsx$", basename(file), ignore.case = TRUE) ~ "A",
        grepl("B\\.xlsx$", basename(file), ignore.case = TRUE) ~ "B",
        TRUE ~ "Check"
      )
    )
})

# Inspect the column names in Sheet 1B
sheet1B_check <- read_excel(
  double_files[grepl("Sheet 1B\\.xlsx$", double_files)],
  col_types = "text",
  .name_repair = "unique"
)

# Show column names and their positions
data.frame(
  position = seq_along(names(sheet1B_check)),
  column_name = names(sheet1B_check)
)

# Show the first few rows
head(sheet1B_check, 5)

double_data_list <- lapply(double_files, function(file) {
  
  data <- read_excel(
    file,
    col_types = "text",
    .name_repair = "unique"
  )
  
  # Standardise column names by position
  standard_names <- c(
    "rec_number",
    "title",
    "journal_name",
    "year",
    "abstract",
    "doi",
    "pdf_link",
    "pdf_link_2",
    "screener_number",
    "decision",
    "exclusion_reason",
    "notes",
    "lit_review",
    "decision_compared",
    "final_decision"
  )
  
  # Stop if a file has more columns than expected
  if (ncol(data) > length(standard_names)) {
    stop(paste("Unexpected extra columns in:", basename(file)))
  }
  
  # Assign names to the columns that exist
  names(data) <- standard_names[seq_len(ncol(data))]
  
  # Add missing columns where necessary
  for (column in setdiff(standard_names, names(data))) {
    data[[column]] <- NA_character_
  }
  
  data %>%
    mutate(
      source_file = basename(file),
      screening_batch = basename(dirname(file)),
      reviewer_sheet = case_when(
        grepl("A\\.xlsx$", basename(file), ignore.case = TRUE) ~ "A",
        grepl("B\\.xlsx$", basename(file), ignore.case = TRUE) ~ "B",
        TRUE ~ "Check"
      )
    )
})

double_screening_raw <- bind_rows(double_data_list)

dim(double_screening_raw)

names(double_screening_raw)

head(double_screening_raw)


# Check duplicate record numbers within each source file
double_screening_raw %>%
  filter(!is.na(rec_number), rec_number != "") %>%
  count(source_file, rec_number) %>%
  filter(n > 1)

# Check records appearing more than once across the dataset
double_screening_raw %>%
  filter(!is.na(rec_number), rec_number != "") %>%
  count(rec_number) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Identify files where the first column is not named rec_number
file_column_check <- lapply(double_files, function(file) {
  
  data <- read_excel(
    file,
    n_max = 3,
    col_types = "text",
    .name_repair = "unique"
  )
  
  data.frame(
    source_file = basename(file),
    first_column = names(data)[1],
    second_column = names(data)[2],
    number_of_columns = ncol(data)
  )
})

bind_rows(file_column_check) %>%
  filter(first_column != "rec_number" |
           number_of_columns != 14 &
           number_of_columns != 15)

double_screening_raw %>%
  filter(rec_number %in% c("585.0", "2144.0", "3444.0")) %>%
  select(
    rec_number, title, source_file,
    screening_batch, reviewer_sheet
  )

sheet17A_check <- read_excel(
  double_files[grepl("Sheet 17A\\.xlsx$", double_files)],
  col_types = "text",
  .name_repair = "unique"
)

# Inspect first 10 rows and first 5 columns
sheet17A_check %>%
  select(1:5) %>%
  head(10)

# Check the values in the first two columns
sheet17A_check %>%
  select(1:2) %>%
  head(20)

double_screening_raw <- double_screening_raw %>%
  mutate(
    rec_number = trimws(rec_number),
    rec_number = sub("\\.0$", "", rec_number)
  )

#================================================================

double_data_list <- lapply(double_files, function(file) {
  
  data <- read_excel(
    file,
    col_types = "text",
    .name_repair = "unique"
  )
  
  # Correct the missing rec_number header in Sheet 17A
  if (basename(file) == "Sheet 17A.xlsx") {
    names(data)[1] <- "rec_number"
  }
  
  standard_names <- c(
    "rec_number",
    "title",
    "journal_name",
    "year",
    "abstract",
    "doi",
    "pdf_link",
    "pdf_link_2",
    "screener_number",
    "decision",
    "exclusion_reason",
    "notes",
    "lit_review",
    "decision_compared",
    "final_decision"
  )
  
  if (ncol(data) > length(standard_names)) {
    stop(paste("Unexpected columns in:", basename(file)))
  }
  
  names(data) <- standard_names[seq_len(ncol(data))]
  
  for (column in setdiff(standard_names, names(data))) {
    data[[column]] <- NA_character_
  }
  
  data %>%
    mutate(
      source_file = basename(file),
      screening_batch = basename(dirname(file)),
      reviewer_sheet = case_when(
        grepl("A\\.xlsx$", basename(file), ignore.case = TRUE) ~ "A",
        grepl("B\\.xlsx$", basename(file), ignore.case = TRUE) ~ "B",
        TRUE ~ "Check"
      )
    )
})

double_screening_raw <- bind_rows(double_data_list)


double_data_list <- lapply(double_files, function(file) {
  
  data <- read_excel(
    file,
    col_types = "text",
    .name_repair = "unique"
  )
  
  # Correct the missing rec_number header in Sheet 17A
  if (basename(file) == "Sheet 17A.xlsx") {
    names(data)[1] <- "rec_number"
  }
  
  standard_names <- c(
    "rec_number",
    "title",
    "journal_name",
    "year",
    "abstract",
    "doi",
    "pdf_link",
    "pdf_link_2",
    "screener_number",
    "decision",
    "exclusion_reason",
    "notes",
    "lit_review",
    "decision_compared",
    "final_decision"
  )
  
  if (ncol(data) > length(standard_names)) {
    stop(paste("Unexpected columns in:", basename(file)))
  }
  
  names(data) <- standard_names[seq_len(ncol(data))]
  
  for (column in setdiff(standard_names, names(data))) {
    data[[column]] <- NA_character_
  }
  
  data %>%
    mutate(
      source_file = basename(file),
      screening_batch = basename(dirname(file)),
      reviewer_sheet = case_when(
        grepl("A\\.xlsx$", basename(file), ignore.case = TRUE) ~ "A",
        grepl("B\\.xlsx$", basename(file), ignore.case = TRUE) ~ "B",
        TRUE ~ "Check"
      )
    )
})

double_screening_raw <- bind_rows(double_data_list)

double_screening_raw <- double_screening_raw %>%
  mutate(
    rec_number = trimws(rec_number),
    rec_number = sub("\\.0$", "", rec_number)
  )

# Duplicates within each file
double_screening_raw %>%
  filter(!is.na(rec_number), rec_number != "") %>%
  count(source_file, rec_number) %>%
  filter(n > 1)

# Records appearing across multiple files
double_screening_raw %>%
  filter(!is.na(rec_number), rec_number != "") %>%
  count(rec_number) %>%
  filter(n > 1) %>%
  arrange(desc(n))

double_screening_raw %>%
  filter(!is.na(rec_number), rec_number != "") %>%
  count(source_file, rec_number) %>%
  filter(n > 1)

#Check repeated records across screening batches
duplicate_batch_check <- double_screening_raw %>%
  filter(!is.na(rec_number), rec_number != "") %>%
  group_by(rec_number) %>%
  summarise(
    total_entries = n(),
    number_of_batches = n_distinct(screening_batch),
    batches = paste(sort(unique(screening_batch)), collapse = ", "),
    reviewer_sheets = paste(sort(unique(reviewer_sheet)), collapse = ", "),
    source_files = paste(sort(unique(source_file)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(total_entries > 1) %>%
  arrange(desc(number_of_batches), desc(total_entries))

duplicate_batch_check %>%
  count(number_of_batches, total_entries)

duplicate_batch_check %>%
  filter(number_of_batches > 1)

duplicate_batch_check %>%
  filter(number_of_batches == 1) %>%
  count(reviewer_sheets, total_entries)

#=========================

# Records appearing in more than one batch
records_multiple_batches <- duplicate_batch_check %>%
  filter(number_of_batches > 1)

records_multiple_batches


# Show every screening entry for these records
double_screening_raw %>%
  semi_join(
    records_multiple_batches,
    by = "rec_number"
  ) %>%
  select(
    rec_number,
    screening_batch,
    reviewer_sheet,
    screener_number,
    decision,
    decision_compared,
    final_decision,
    source_file
  ) %>%
  arrange(rec_number, screening_batch, reviewer_sheet)

#Inspect the 18 records and their decisions

double_screening_raw %>%
  semi_join(
    records_multiple_batches,
    by = "rec_number"
  ) %>%
  select(
    rec_number,
    screening_batch,
    reviewer_sheet,
    screener_number,
    decision,
    decision_compared,
    final_decision,
    source_file
  ) %>%
  arrange(rec_number, screening_batch, reviewer_sheet)

#summarise all 18 records
multiple_batch_summary <- double_screening_raw %>%
  semi_join(records_multiple_batches, by = "rec_number") %>%
  group_by(rec_number, screening_batch) %>%
  summarise(
    number_of_entries = n(),
    reviewers = paste(reviewer_sheet, collapse = ", "),
    screener_numbers = paste(screener_number, collapse = ", "),
    decisions = paste(decision, collapse = ", "),
    decisions_compared = paste(unique(na.omit(decision_compared)), collapse = ", "),
    final_decisions = paste(unique(na.omit(final_decision)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(rec_number, screening_batch)

print(multiple_batch_summary, n = Inf)

#Check the two incomplete pairs

double_screening_raw %>%
  filter(rec_number %in% c("44304", "45265")) %>%
  select(
    rec_number,
    screening_batch,
    reviewer_sheet,
    screener_number,
    decision,
    decision_compared,
    final_decision
  ) %>%
  arrange(rec_number, screening_batch, reviewer_sheet)

#match double screening to the meta sheet

meta_data <- read_excel(
  "data_raw/meta_sheet/Causes of Mortality - PDF Screening Sheet.xlsx",
  col_types = "text"
)

#Standardise the record numbers in both datasets:
meta_data <- meta_data %>%
  mutate(
    rec_number = trimws(rec_number),
    rec_number = sub("\\.0$", "", rec_number)
  )

#check the record numbers 
meta_data %>%
  count(rec_number) %>%
  filter(n > 1)

# Create a reference list of records in the meta sheet
meta_reference <- meta_data %>%
  select(
    rec_number,
    meta_title = title,
    meta_year = year
  ) %>%
  mutate(meta_record_present = TRUE)

# Match double-screening entries to the meta sheet
double_screening_matched <- double_screening_raw %>%
  left_join(meta_reference, by = "rec_number") %>%
  mutate(
    meta_match_status = case_when(
      !is.na(meta_record_present) ~ "Matched to meta sheet",
      TRUE ~ "Not found in meta sheet"
    )
  )

#Check the results 

double_screening_matched %>%
  distinct(rec_number, meta_match_status) %>%
  count(meta_match_status)

#identify unmatched records:
unmatched_double_screening <- double_screening_matched %>%
  filter(meta_match_status == "Not found in meta sheet") %>%
  distinct(rec_number, title, screening_batch, source_file)

unmatched_double_screening

#calculate the number of unique records in the double-screening dataset
double_screening_raw %>%
  summarise(
    total_screening_entries = n(),
    unique_records = n_distinct(rec_number),
    missing_record_numbers = sum(is.na(rec_number) | rec_number == "")
  )


missing_rec_check <- double_screening_raw %>%
  group_by(source_file, screening_batch) %>%
  summarise(
    total_rows = n(),
    missing_rec_number = sum(
      is.na(rec_number) | trimws(rec_number) == ""
    ),
    available_rec_number = sum(
      !is.na(rec_number) & trimws(rec_number) != ""
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_rec_number))

# Display only files containing missing record numbers
missing_rec_check %>%
  filter(missing_rec_number > 0)

#==========================================
#Inspect missing decisions by file
missing_decisions <- double_screening_raw %>%
  group_by(source_file) %>%
  summarise(
    total_rows = n(),
    records_with_number = sum(
      !is.na(rec_number) & trimws(rec_number) != ""
    ),
    missing_decisions = sum(
      is.na(decision) | trimws(decision) == ""
    ),
    recorded_decisions = sum(
      !is.na(decision) & trimws(decision) != ""
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_decisions))
print(missing_decisions, n = Inf)

#===========================
# Exclude Sheet 22B and remove rows without actual screening decisions
double_screening_clean <- double_screening_raw %>%
  filter(
    source_file != "Sheet 22B.xlsx",
    !is.na(rec_number),
    trimws(rec_number) != "",
    !is.na(decision),
    trimws(decision) != ""
  )

double_screening_incomplete <- double_screening_raw %>%
  filter(
    source_file != "Sheet 22B.xlsx",
    !is.na(rec_number),
    trimws(rec_number) != "",
    is.na(decision) | trimws(decision) == "",
    source_file != "Sheet 1A.xlsx"
  )

#check results for Sheets 20A and 19

# Number of completed screening entries
nrow(double_screening_clean)

# Number of incomplete records
nrow(double_screening_incomplete)

# Completed decisions by source file
double_screening_clean %>%
  count(source_file, sort = TRUE)

# Incomplete records by source file
double_screening_incomplete %>%
  count(source_file)

#=====================================
#checking for duplicates 
double_screening_clean %>%
  count(source_file, rec_number) %>%
  filter(n > 1)

double_screening_clean %>%
  count(rec_number) %>%
  filter(n > 1) %>%
  arrange(desc(n))

#summarise 
double_screening_duplicates <- double_screening_clean %>%
  group_by(rec_number) %>%
  summarise(
    total_entries = n(),
    number_of_batches = n_distinct(screening_batch),
    batches = paste(sort(unique(screening_batch)), collapse = ", "),
    reviewer_sheets = paste(sort(unique(reviewer_sheet)), collapse = ", "),
    decisions = paste(decision, collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(total_entries > 1)

#match to meta sheet
meta_data %>%
  count(rec_number) %>%
  filter(n > 1)

meta_reference <- meta_data %>%
  select(
    rec_number,
    meta_title = title,
    meta_year = year
  ) %>%
  mutate(meta_record_present = TRUE)

double_screening_matched <- double_screening_clean %>%
  left_join(meta_reference, by = "rec_number") %>%
  mutate(
    meta_match_status = case_when(
      !is.na(meta_record_present) ~ "Matched to meta sheet",
      TRUE ~ "Not found in meta sheet"
    )
  )

double_screening_matched %>%
  distinct(rec_number, meta_match_status) %>%
  count(meta_match_status)

#identify unmatched records 
unmatched_double_screening <- double_screening_matched %>%
  filter(meta_match_status == "Not found in meta sheet") %>%
  distinct(rec_number, title, screening_batch, source_file)

unmatched_double_screening %>%
  summarise(
    unique_unmatched_records = n_distinct(rec_number),
    unmatched_entries = n()
  )

unmatched_unique <- double_screening_matched %>%
  filter(meta_match_status == "Not found in meta sheet") %>%
  distinct(rec_number, title) %>%
  arrange(rec_number)

print(unmatched_unique, n = Inf)

# Compare unmatched double-screening records
# with the earlier individual-screening unmatched records

unmatched_overlap <- unmatched_unique %>%
  inner_join(
    unmatched_removed_candidates %>%
      distinct(rec_number),
    by = "rec_number"
  )

unmatched_overlap

# Double-screening records not found in individual unmatched list
unmatched_unique %>%
  anti_join(
    unmatched_removed_candidates %>% distinct(rec_number),
    by = "rec_number"
  )

#combine the additional 15 paperes from workshops 
# Prepare unmatched individual-screening records

names(unmatched_removed_candidates)

head(unmatched_removed_candidates)

individual_unmatched_extra <- unmatched_removed_candidates %>%
  distinct(rec_number, .keep_all = TRUE) %>%
  transmute(
    rec_number = as.character(rec_number),
    title = as.character(Title),
    screening_source = "Workshop individual screening"
  )

# Prepare unmatched double-screening records
double_unmatched_extra <- unmatched_unique %>%
  transmute(
    rec_number = as.character(rec_number),
    title = as.character(title),
    screening_source = "Workshop double screening"
  )

# Combine both sources
unmatched_extra_records <- bind_rows(
  individual_unmatched_extra,
  double_unmatched_extra
) %>%
  arrange(rec_number)

# Total rows
nrow(unmatched_extra_records)

# Unique record numbers
n_distinct(unmatched_extra_records$rec_number)

# Records by source
unmatched_extra_records %>%
  count(screening_source)

# Check duplicate record numbers
unmatched_extra_records %>%
  count(rec_number) %>%
  filter(n > 1)

#save the dataset
write.csv(
  unmatched_extra_records,
  "data_processed/unmatched_extra_records.csv",
  row.names = FALSE,
  na = ""
)

#==================================================================
#double screening dataset: Step 1 – Create a one-row-per-record summary

double_screening_summary <- double_screening_matched %>%
  group_by(rec_number) %>%
  summarise(
    title = first(title),
    
    number_of_screening_entries = n(),
    
    screening_batches = paste(
      sort(unique(screening_batch)),
      collapse = "; "
    ),
    
    reviewer_decisions = paste(
      paste0(
        screening_batch, " / ",
        reviewer_sheet, " / screener ",
        coalesce(screener_number, "NA"), ": ",
        coalesce(decision, "NA")
      ),
      collapse = "; "
    ),
    
    decisions_compared = paste(
      unique(decision_compared[
        !is.na(decision_compared) & decision_compared != ""
      ]),
      collapse = "; "
    ),
    
    recorded_final_decisions = paste(
      unique(final_decision[
        !is.na(final_decision) & final_decision != ""
      ]),
      collapse = "; "
    ),
    
    source_files = paste(
      sort(unique(source_file)),
      collapse = "; "
    ),
    
    meta_match_status = first(meta_match_status),
    
    .groups = "drop"
  )

#check the summary 
# Dimensions
dim(double_screening_summary)

# Unique record numbers
n_distinct(double_screening_summary$rec_number)

# Check duplicates
double_screening_summary %>%
  count(rec_number) %>%
  filter(n > 1)

# Matching status
double_screening_summary %>%
  count(meta_match_status)


meta_with_double_screening <- meta_data %>%
  left_join(
    double_screening_summary,
    by = "rec_number"
  )

dim(meta_data)
dim(meta_with_double_screening)

meta_with_double_screening %>%
  summarise(
    total_meta_records = n(),
    records_with_double_screening = sum(
      !is.na(number_of_screening_entries)
    ),
    records_without_double_screening = sum(
      is.na(number_of_screening_entries)
    )
  )

#==============================================================

meta_with_individual_and_double <- meta_with_double_screening %>%
  left_join(
    individual_screening_summary,
    by = "rec_number"
  )

#===================================================================
# Summarise incomplete double-screening records
double_incomplete_summary <- double_screening_incomplete %>%
  group_by(rec_number) %>%
  summarise(
    double_incomplete_entries = n(),
    double_incomplete_files = paste(
      unique(source_file),
      collapse = "; "
    ),
    .groups = "drop"
  )

# Combine completed and incomplete double-screening information
double_screening_all <- full_join(
  double_screening_summary,
  double_incomplete_summary,
  by = "rec_number"
)


