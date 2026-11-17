# ==========================================================
# SCRIPT 05: CREATE CONSOLIDATED SCREENING DATASET
# ==========================================================

# STEP 1: PREPARE DOUBLE-SCREENING DATA

# Summarise incomplete double-screening records

double_incomplete_summary <- double_screening_incomplete %>%
  group_by(rec_number) %>%
  summarise(
    double_incomplete_entries = n(),
    
    double_incomplete_files = paste(
      sort(unique(source_file)),
      collapse = "; "
    ),
    
    .groups = "drop"
  )


# Combine completed and incomplete double-screening records

double_screening_all <- full_join(
  double_screening_summary %>%
    rename(
      double_meta_match_status = meta_match_status
    ),
  double_incomplete_summary,
  by = "rec_number"
)

# ==========================================================
# STEP 3: PREPARE INTERNAL SCREENING DATA
# ==========================================================

internal_screening_for_join <- internal_screening_master %>%
  
  # Remove paper information already available elsewhere
  select(-title, -journal_name, -year) %>%
  
  mutate(
    
    # Identify papers included in internal screening
    internal_screening_record = TRUE,
    
    # Count reviewers assigned to each paper
    internal_reviewers_assigned = rowSums(
      across(c(Nisreen, Paula, OJ, Bhargavi)),
      na.rm = TRUE
    )
  )

# ==========================================================
# STEP 4: COMBINE ALL SCREENING PROCESSES
# ==========================================================

screening_consolidated <- workshop_individual_final %>%
  
  # Add double-screening information
  full_join(
    double_screening_all,
    by = "rec_number"
  ) %>%
  
  # Add internal screening information
  full_join(
    internal_screening_for_join,
    by = "rec_number"
  ) %>%
  
  # Identify which screening processes involved each paper
  mutate(
    
    individual_screening_record =
      !is.na(individual_screening_entries),
    
    double_screening_record =
      !is.na(number_of_screening_entries) |
      !is.na(double_incomplete_entries),
    
    internal_screening_record =
      !is.na(internal_screening_record)
    
  ) %>%
  
  # Retain only records with evidence of screening
  filter(
    individual_screening_record |
      double_screening_record |
      internal_screening_record
  )

# ==========================================================
# STEP 5: DATASET VALIDATION
# ==========================================================


# Count unique papers
n_distinct(screening_consolidated$rec_number)


# Check for duplicated record numbers
screening_consolidated %>%
  count(rec_number) %>%
  filter(n > 1)


# Count papers by screening process

screening_consolidated %>%
  summarise(
    
    total_unique_papers = n(),
    
    individual_screened = sum(
      individual_screening_record
    ),
    
    double_screened = sum(
      double_screening_record
    ),
    
    internally_screened = sum(
      internal_screening_record
    ),
    
    individual_and_double = sum(
      individual_screening_record &
        double_screening_record
    ),
    
    screened_by_all_three = sum(
      individual_screening_record &
        double_screening_record &
        internal_screening_record
    )
  )

# ==========================================================
# STEP 6: CHECK UNMATCHED RECORDS
# ==========================================================

screening_consolidated %>%
  filter(
    meta_match_status == "Not found in meta sheet" |
      double_meta_match_status == "Not found in meta sheet"
  ) %>%
  distinct(rec_number, .keep_all = TRUE) %>%
  count(rec_number)

# ==========================================================
# STEP 7: ADD IN INDIVIDUAL SCREENING DECISIONS
# ==========================================================

individual_decisions <- workshop_individual_raw_clean %>%
  transmute(
    rec_number,
    screening_source = "Individual",
    screening_batch = source_file,
    reviewer_id = if_else(
      is.na(`Screener number`) | trimws(`Screener number`) == "",
      NA_character_,
      str_remove(trimws(`Screener number`), "\\.0$")
    ),
    decision = Decision
  )

# ==========================================================
# STEP 8: DOUBLE SCREENING DECISIONS
# ==========================================================

double_decisions <- double_screening_clean %>%
  transmute(
    rec_number,
    screening_source = "Double",
    screening_batch = screening_batch,
    reviewer_id = if_else(
      is.na(screener_number) | trimws(screener_number) == "",
      paste(screening_batch, reviewer_sheet),
      str_remove(trimws(screener_number), "\\.0$")
    ),
    decision = decision
  )

# ==========================================================
# STEP 9: INTERNAL SCREENING DECISIONS
# ==========================================================

internal_decisions <- bind_rows(

  internal_screening_master %>%
    filter(Nisreen) %>%
    transmute(
      rec_number,
      screening_source = "Internal",
      screening_batch = "Internal",
      reviewer_id = "Nisreen",
      decision = Nisreen_include
    ),

  internal_screening_master %>%
    filter(Paula) %>%
    transmute(
      rec_number,
      screening_source = "Internal",
      screening_batch = "Internal",
      reviewer_id = "Paula",
      decision = Paula_include
    ),

  internal_screening_master %>%
    filter(OJ) %>%
    transmute(
      rec_number,
      screening_source = "Internal",
      screening_batch = "Internal",
      reviewer_id = "OJ",
      decision = OJ_include
    ),

  internal_screening_master %>%
    filter(Bhargavi) %>%
    transmute(
      rec_number,
      screening_source = "Internal",
      screening_batch = "Internal",
      reviewer_id = "Bhargavi",
      decision = Bhargavi_include
    )
)

# ==========================================================
# STEP 10: COMBINE ALL SCREENING DECISIONS
# ==========================================================

all_screening_decisions <- bind_rows(
  individual_decisions,
  double_decisions,
  internal_decisions
) %>%
  mutate(
    decision = na_if(toupper(trimws(decision)), "")
  )
# ==========================================================
# STEP 11: OVERALL DECISION COUNTS
# ==========================================================

all_screening_decisions %>%
  summarise(
    total_screening_entries = n(),
    
    include_decisions = sum(decision == "INCLUDE", na.rm = TRUE),
    
    exclude_decisions = sum(decision == "EXCLUDE", na.rm = TRUE),
    
    not_sure_decisions = sum(decision == "NOT SURE", na.rm = TRUE),
    
    missing_decisions = sum(is.na(decision))
  )
print(all_screening_decisions)
view(all_screening_decisions)

all_screening_decisions %>%
  count(screening_source, decision, sort = TRUE)

# ==========================================================
# STEP 12: PAPER-LEVEL DECISION SUMMARY
# ==========================================================

paper_decision_summary <- all_screening_decisions %>%
  group_by(rec_number) %>%
  summarise(
    total_screening_entries = n(),
    
    number_of_identified_reviewers = n_distinct(
      reviewer_id[!is.na(reviewer_id)]
    ),
    
    include_decisions = sum(decision == "INCLUDE", na.rm = TRUE),
    
    exclude_decisions = sum(decision == "EXCLUDE", na.rm = TRUE),
    
    not_sure_decisions = sum(decision == "NOT SURE", na.rm = TRUE),
    
    missing_decisions = sum(is.na(decision)),
    
    .groups = "drop"
  ) %>%
  mutate(
    more_than_two_reviewers =
      number_of_identified_reviewers > 2,
    
    conflicting_decisions =
      include_decisions > 0 & exclude_decisions > 0
  )

# ==========================================================
# STEP 13: ADD PAPER-LEVEL DECISION SUMMARY
# ==========================================================

screening_consolidated <- screening_consolidated %>%
  left_join(
    paper_decision_summary,
    by = "rec_number"
  )

view(screening_consolidated)

# ==========================================================
# STEP 14: final checks
# ==========================================================

# Check unique papers
n_distinct(screening_consolidated$rec_number)

# Check duplicated record numbers
screening_consolidated %>%
  count(rec_number) %>%
  filter(n > 1)

# Check papers with more than two reviewers
screening_consolidated %>%
  count(more_than_two_reviewers)

# Check papers with conflicting INCLUDE/EXCLUDE decisions
screening_consolidated %>%
  filter(conflicting_decisions) %>%
  select(
    rec_number,
    total_screening_entries,
    number_of_identified_reviewers,
    include_decisions,
    exclude_decisions,
    not_sure_decisions
  )

# ==========================================================
# STEP 15: OVERALL SCREENING FLOW SUMMARY
# ==========================================================

screening_flow_summary <- all_screening_decisions %>%
  group_by(rec_number) %>%
  summarise(
    total_entries = n(),
    
    has_recorded_decision = any(
      !is.na(decision) & decision != ""
    ),
    
    has_include = any(decision == "INCLUDE", na.rm = TRUE),
    
    has_exclude = any(decision == "EXCLUDE", na.rm = TRUE),
    
    has_not_sure = any(decision == "NOT SURE", na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  summarise(
    total_unique_papers_screened = n(),
    
    papers_with_recorded_decisions =
      sum(has_recorded_decision),
    
    papers_with_no_recorded_decision =
      sum(!has_recorded_decision),
    
    papers_with_include =
      sum(has_include),
    
    papers_with_exclude =
      sum(has_exclude),
    
    papers_with_not_sure =
      sum(has_not_sure)
  )

screening_flow_summary
view(screening_flow_summary)



all_screening_decisions %>%
  group_by(rec_number) %>%
  summarise(
    total_entries = n(),
    recorded_decisions = sum(!is.na(decision)),
    missing_decisions = sum(is.na(decision)),
    .groups = "drop"
  ) %>%
  filter(recorded_decisions == 0)

all_screening_decisions %>%
  count(screening_source, decision, .drop = FALSE)


# ==========================================================
# STEP 9:  PAPER-LEVEL DECISION SUMMARY
# ==========================================================

paper_final_outcomes <- all_screening_decisions %>%
  group_by(rec_number) %>%
  summarise(
    has_include = any(decision == "INCLUDE", na.rm = TRUE),
    has_exclude = any(decision == "EXCLUDE", na.rm = TRUE),
    has_not_sure = any(decision == "NOT SURE", na.rm = TRUE),
    
    missing_decisions = sum(is.na(decision)),
    
    .groups = "drop"
  ) %>%
  mutate(
    final_paper_outcome = case_when(
      
      has_include & !has_exclude & !has_not_sure ~ "Include only",
      
      has_exclude & !has_include & !has_not_sure ~ "Exclude only",
      
      has_not_sure & !has_include & !has_exclude ~ "Not sure only",
      
      has_include & has_exclude & has_not_sure ~
        "Include, Exclude and Not sure",
      
      has_include & has_exclude ~
        "Include and Exclude disagreement",
      
      has_include & has_not_sure ~
        "Include and Not sure",
      
      has_exclude & has_not_sure ~
        "Exclude and Not sure",
      
      TRUE ~ "No recorded decision"
    )
  )

paper_final_outcomes %>%
  count(final_paper_outcome, name = "number_of_unique_papers")

paper_final_outcomes %>%
  summarise(
    total_unique_papers = n(),
    include_only = sum(final_paper_outcome == "Include only"),
    exclude_only = sum(final_paper_outcome == "Exclude only"),
    not_sure_only = sum(final_paper_outcome == "Not sure only"),
    conflicting_decisions = sum(grepl(
      "disagreement|and Not sure|and Exclude",
      final_paper_outcome
    )),
    no_recorded_decision = sum(
      final_paper_outcome == "No recorded decision"
    )
  )

# ==========================================================
# STEP 10: Keep only papers matched to the original meta sheet

# ==========================================================

screening_matched_only <- screening_consolidated %>%
  filter(
    meta_match_status == "Matched to meta sheet"
  )

nrow(screening_matched_only)

n_distinct(screening_matched_only$rec_number)

matched_paper_outcomes <- paper_final_outcomes %>%
  semi_join(
    screening_matched_only %>% select(rec_number),
    by = "rec_number"
  )

matched_paper_outcomes %>%
  count(
    final_paper_outcome,
    name = "number_of_unique_papers"
  )

matched_paper_outcomes %>%
  summarise(
    total_unique_papers = n(),
    
    include_only = sum(
      final_paper_outcome == "Include only"
    ),
    
    exclude_only = sum(
      final_paper_outcome == "Exclude only"
    ),
    
    not_sure_only = sum(
      final_paper_outcome == "Not sure only"
    ),
    
    conflicting_decisions = sum(
      grepl(
        "disagreement|and Not sure|and Exclude",
        final_paper_outcome
      )
    ),
    
    no_recorded_decision = sum(
      final_paper_outcome == "No recorded decision"
    )
  )

write.csv(
  screening_matched_only,
  "data_processed/screening_matched_only.csv",
  row.names = FALSE,
  na = ""
)

#Save the matched-only dataset

write.csv(
  screening_matched_only,
  "data_processed/screening_matched_only.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  matched_paper_outcomes,
  "data_processed/matched_paper_outcomes.csv",
  row.names = FALSE,
  na = ""
)
