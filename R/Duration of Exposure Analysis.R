# =============================================================================
# DURATION OF EXPOSURE  SAFETY ANALYSIS SET
# =============================================================================
# Purpose      : Summarize recorded and net treatment exposure duration,
#                accounting for interruptions between exposure periods.
# Programmer   : Javan Mukunzi
# Program date : 28-Jul-2026
# Output       : Formatted PDF summary table

# =============================================================================


# 1. LOAD REQUIRED LIBRARIES -------------------------------------------------

# tidyverse : Data manipulation, summaries, and reshaping.
# haven     : Read SAS datasets into R.
# gt        : Create and format the summary table.
# docorator : Add report headers, footers, and render the PDF.

library(tidyverse)
library(haven)
library(gt)
library(docorator)

# Clear existing objects from the R environment to avoid accidental
# reuse of objects from previous analyses.
rm(list = ls())


# 2. READ SOURCE DATA --------------------------------------------------------

# Define the locations of the ADSL and ADEX datasets.
adsl <- read_sas("C:/Users/USER/Downloads/ADAM/adsl.sas7bdat")

adex_1 <- read_sas("C:/Users/USER/Downloads/ADAM/adam/adex.sas7bdat")


# SAFFL = "Y" identifies subjects included in the safety population.

adsl_saff <- adsl %>%
  filter(SAFFL == "Y")


# 3. PREPARE EXPOSURE DATA ---------------------------------------------------

# Retain the subject identifier and assigned treatment from ADSL.
# Exclude a duplicate TRT01A from ADEX, if present, so the assigned
# treatment variable is not accidentally overwritten during the join.
#
# An inner join retains only exposure records belonging to subjects
# in the Safety Analysis Set.

ex_dur <- adsl_saff %>%
  select(USUBJID, TRT01A) %>%
  inner_join(
    adex_1 %>% select(-any_of("TRT01A")),
    by = "USUBJID"
  ) %>%
  mutate(
    # Convert exposure dates to Date objects before calculating duration.
    EXSTDTC = as.Date(as.character(EXSTDTC)),
    EXENDTC = as.Date(as.character(EXENDTC)),

    # Calculate exposure duration in days, including both the start
    # and end dates. Divide by 30 to express duration in approximate
    # 30-day months.
    EXDUR = as.numeric(EXENDTC - EXSTDTC) + 1,
    EXDUR = EXDUR / 30
  ) %>%
  # Exclude records with missing treatment information or invalid dates.
  filter(
    !is.na(TRT01A),
    !is.na(TRTA),
    !is.na(EXSTDTC),
    !is.na(EXENDTC),
    EXENDTC >= EXSTDTC
  ) %>%
  select(
    USUBJID,
    TRT01A,
    TRTA,
    EXDUR,
    EXSTDTC,
    EXENDTC
  )

# Stop execution if no usable exposure records remain.
if (nrow(ex_dur) == 0) {
  stop("No valid exposure records found for the safety population.")
}


# 4. SUMMARIZE RECORDED EXPOSURE DURATION -----------------------------------

# Calculate descriptive statistics for the recorded exposure duration.
# TRT01A represents assigned treatment; each exposure record contributes
# its calculated duration to the summary.

total_dur <- ex_dur %>%
  group_by(TRT01A) %>%
  summarise(
    Mean   = mean(EXDUR, na.rm = TRUE),
    SD     = sd(EXDUR, na.rm = TRUE),
    Median = median(EXDUR, na.rm = TRUE),
    Min    = min(EXDUR, na.rm = TRUE),
    Max    = max(EXDUR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Apply the display precision required for the report.
    Mean   = round(Mean, 1),
    SD     = round(SD, 1),
    Median = round(Median, 1),
    Min    = round(Min, 0),
    Max    = round(Max, 0),

    # Convert the rounded mean duration from months to years.
    Total_treatment_years = round(Mean / 12, 1)
  ) %>%
  # Reshape the statistics so that each statistic becomes a row.
  pivot_longer(
    cols = -TRT01A,
    names_to = "name",
    values_to = "value"
  ) %>%
  # Place each treatment group in its own column.
  pivot_wider(
    id_cols = name,
    names_from = TRT01A,
    values_from = value
  ) %>%
  mutate(
    # Convert values to character so the report can contain both
    # numeric summaries and text labels.
    across(everything(), as.character),

    # Assign the section title and display order.
    col2 = "Actual treatment duration (months)a",
    ord = 1L
  )


# 5. IDENTIFY TREATMENT INTERRUPTIONS ---------------------------------------

# Sort records chronologically within each subject and administered
# treatment (TRTA).
#
# lead(EXSTDTC) obtains the next exposure start date.
# The gap is the number of calendar days between the current exposure
# end date and the next exposure start date.
# Only positive gaps are counted as interruptions.

interruptions <- ex_dur %>%
  arrange(USUBJID, TRTA, EXSTDTC, EXENDTC) %>%
  group_by(USUBJID, TRTA) %>%
  mutate(
    next_start = lead(EXSTDTC),
    gap_days = as.numeric(next_start - EXENDTC - 1)
  ) %>%
  summarise(
    interruption_days = sum(
      gap_days[gap_days > 0],
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# 6. CALCULATE SUBJECT-LEVEL NET TREATMENT DURATION -------------------------

# Determine the first exposure start date and last exposure end date
# for each subject and administered treatment.
#
# The calendar span includes all days between the first and last dates.
# Subtracting the identified gaps estimates the net exposure duration.
#
# IMPORTANT: These calculations must be grouped by subject and TRTA.
# Calculating min() and max() without grouping would incorrectly use
# dates across multiple subjects.

actual_duration <- ex_dur %>%
  group_by(USUBJID, TRTA) %>%
  summarise(
    first_start = min(EXSTDTC, na.rm = TRUE),
    last_end = max(EXENDTC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    interruptions,
    by = c("USUBJID", "TRTA")
  ) %>%
  mutate(
    # Calculate the complete calendar span, including both endpoints.
    calendar_days = as.numeric(last_end - first_start) + 1,

    # Assign zero interruption days if no gaps were identified.
    interruption_days = replace_na(interruption_days, 0),

    # Estimate the number of days on treatment after subtracting gaps.
    actual_days = pmax(
      calendar_days - interruption_days,
      0
    ),

    # Convert net exposure days into approximate months.
    actual_months = actual_days / (365.25 / 12)
  )


# 7. SUMMARIZE NET TREATMENT DURATION ---------------------------------------

# Summarize the subject-level net treatment duration by TRTA.
# Unlike Section 4, this summary uses one calculated duration per
# subject and administered treatment.

actual_trt_dur <- actual_duration %>%
  group_by(TRTA) %>%
  summarise(
    Mean   = mean(actual_months, na.rm = TRUE),
    SD     = sd(actual_months, na.rm = TRUE),
    Median = median(actual_months, na.rm = TRUE),
    Min    = min(actual_months, na.rm = TRUE),
    Max    = max(actual_months, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mean   = round(Mean, 1),
    SD     = round(SD, 1),
    Median = round(Median, 1),
    Min    = round(Min, 0),
    Max    = round(Max, 0),

    # Convert the mean duration to years.
    Total_treatment_years = round(Mean / 12, 1)
  ) %>%
  # Convert the summary statistics into report rows.
  pivot_longer(
    cols = -TRTA,
    names_to = "name",
    values_to = "value"
  ) %>%
  # Put administered treatment groups into separate columns.
  pivot_wider(
    id_cols = name,
    names_from = TRTA,
    values_from = value
  ) %>%
  mutate(
    across(everything(), as.character),
    col2 = "Total treatment duration (months)a",
    ord = 2L
  )


# 8. COMBINE THE SUMMARY SECTIONS -------------------------------------------

# Combine the recorded exposure-duration and net treatment-duration
# summaries into one dataset.
tot_act <- bind_rows(
  actual_trt_dur,
  total_dur
)

# Rename treatment columns to stable internal report identifiers.
# Backticks are required because the source treatment labels contain
# spaces and special characters.

tot_act <- tot_act %>%
  rename(
    tt_ac001 = "treatment A",
    tt_ac002 = "treatment B"
  )

# Define the expected source treatment labels.
# These are mapped to TREATMENT A and TREATMENT B in the final headers.
expected_trt <- c(
  "TREATMENT A",
  "TREATMENT B"
)

# Confirm that the expected treatment columns exist.
if (!all(c("tt_ac001", "tt_ac002") %in% names(tot_act))) {
  stop("One or more expected treatment summary columns are missing.")
}


# 9. CALCULATE SAFETY POPULATION DENOMINATORS -------------------------------

# Create a copy of the safety population for the overall Total column.
# TRT01AN = 99 is used as the internal code for Total.

adsl_total <- adsl_saff %>%
  mutate(
    TRT01A = "Total",
    TRT01AN = 99
  )

# Combine the treatment-specific subjects with the overall population.
bb <- bind_rows(
  adsl_saff,
  adsl_total
)

# Count distinct subjects within each treatment group.
# These values are displayed as N in the column headers.

bigN_hdr <- bb %>%
  distinct(USUBJID, TRT01A) %>%
  count(TRT01A, name = "BIG_N") %>%
  mutate(
    TRT01A = as.character(TRT01A)
  )

# Create a named vector to retrieve denominators by treatment label.
denom_vec <- setNames(
  bigN_hdr$BIG_N,
  bigN_hdr$TRT01A
)


# 10. MAP REPORT COLUMNS TO TREATMENTS --------------------------------------

# Identify the internal treatment columns in the summary dataset.
qc_trt_cols <- names(tot_act)[
  grepl("^tt_ac\\d{3}$", names(tot_act))
]

trt_qc_cols <- qc_trt_cols

# Stop if no treatment columns are available for the report.
if (length(qc_trt_cols) == 0) {
  stop("No tt_ac### treatment columns found in tot_act.")
}

# Build a treatment lookup using the treatment sequence number.
# This assumes TRT01AN = 1 identifies Durva + Olaparib and
# TRT01AN = 2 identifies Durva + Placebo.
trt_map <- bb %>%
  distinct(TRT01A, TRT01AN) %>%
  filter(!is.na(TRT01AN)) %>%
  distinct(TRT01A, .keep_all = TRUE) %>%
  mutate(
    TRT01AN = as.integer(TRT01AN)
  ) %>%
  arrange(TRT01AN)

# Extract the numeric suffix from each internal column name.
# For example, tt_ac001 produces sequence number 1.
# Join the sequence number to the treatment lookup.
qc_col_map <- tibble(
  qc_col = qc_trt_cols
) %>%
  mutate(
    TRT01AN = as.integer(
      stringr::str_extract(qc_col, "\\d{3}$")
    )
  ) %>%
  left_join(
    trt_map,
    by = "TRT01AN"
  ) %>%
  select(
    qc_col,
    TRT01A,
    TRT01AN
  )

# Validate the treatment mapping before creating the report.
if (any(is.na(qc_col_map$TRT01A))) {
  stop(
    "Some report columns could not be mapped to treatment groups. ",
    "Check TRT01AN in ADSL and the treatment column names."
  )
}

# Ensure both expected source treatment labels were mapped.
if (!all(expected_trt %in% qc_col_map$TRT01A)) {
  stop(
    "Expected treatment names not found after mapping.\n",
    "Expected: ",
    paste(expected_trt, collapse = " | "),
    "\nMapped: ",
    paste(unique(qc_col_map$TRT01A), collapse = " | ")
  )
}

# Set the intended display order for the treatment columns.
qc_col_map <- qc_col_map %>%
  mutate(
    TRT01A = factor(
      TRT01A,
      levels = expected_trt
    )
  ) %>%
  arrange(TRT01A)


# 11. BUILD THE REPORT BODY --------------------------------------------------

# Define the indentation for individual statistic rows.
indent <- "  "

# Create the body rows.
# Each statistic receives a row label, while ord determines its section.
body_rows <- tot_act %>%
  mutate(
    STUB = paste0(indent, name),
    seg = ord
  ) %>%
  select(
    seg,
    col2,
    STUB,
    all_of(qc_col_map$qc_col)
  )

# Create section heading rows for the two duration summaries.
header_rows <- tot_act %>%
  distinct(ord, col2) %>%
  rename(seg = ord) %>%
  arrange(seg) %>%
  transmute(
    seg,
    col2,
    STUB = col2
  )

# Leave treatment values blank on section heading rows.
for (cc in qc_col_map$qc_col) {
  header_rows[[cc]] <- ""
}

# Combine the headings and statistic rows.
# row_id = 0 places each section heading before its statistics.
report_df <- bind_rows(
  header_rows %>%
    mutate(row_id = 0L),

  body_rows %>%
    group_by(seg) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
) %>%
  arrange(seg, row_id) %>%
  select(
    STUB,
    all_of(qc_col_map$qc_col)
  )


# 12. PRESERVE INDENTATION IN THE PDF ---------------------------------------

# Replace leading spaces with nonbreaking spaces so the table renderer
# does not collapse the indentation in statistic labels.

make_leading_spaces_visible <- function(x) {

  x <- ifelse(
    is.na(x) | x == "",
    "\u00A0",
    as.character(x)
  )

  # Identify leading whitespace and separate it from the remaining text.
  m <- regexpr("^\\s+", x)
  has <- m > 0

  lead <- ifelse(
    has,
    regmatches(x, m),
    ""
  )

  rest <- ifelse(
    has,
    substring(
      x,
      pmax(attr(m, "match.length"), 0) + 1
    ),
    x
  )

  # Convert leading whitespace into nonbreaking spaces.
  lead_nbsp <- vapply(
    lead,
    function(s) {
      paste(
        rep("\u00A0", nchar(s)),
        collapse = ""
      )
    },
    character(1)
  )

  paste0(lead_nbsp, rest)
}

# Apply indentation handling to row labels and replace missing
# display values with nonbreaking spaces.
report_df <- report_df %>%
  mutate(
    STUB = make_leading_spaces_visible(STUB),
    across(
      all_of(qc_col_map$qc_col),
      ~ ifelse(
        is.na(.x) | .x == "",
        "\u00A0",
        as.character(.x)
      )
    )
  )


# 13. CREATE TREATMENT COLUMN HEADERS ---------------------------------------

# Translate source treatment labels into the standardized report labels.
# Append the safety population denominator to each treatment header.
hdr_labels <- setNames(
  paste0(
    recode(
      as.character(qc_col_map$TRT01A),
      "TREATMENT A" = "TREATMENT A",
      "TREATMENT B" = "TREATMENT B"
    ),
    " (N=",
    as.integer(
      denom_vec[as.character(qc_col_map$TRT01A)]
    ),
    ")"
  ),
  qc_col_map$qc_col
)

# Replace missing denominator text with zero for display.
hdr_labels <- gsub(
  "N=NA",
  "N=0",
  hdr_labels
)


# 14. FORMAT THE GT TABLE ----------------------------------------------------

# Create the report table using the prepared body and treatment headers.
# Align the row labels to the left and treatment values to the center.
# Adjust font sizes and column widths for a compact report layout.

trt_qc_cols <- qc_col_map$qc_col

DC_T01 <- gt(report_df) %>%
  cols_label(
    .list = c(
      list(STUB = "\u00A0"),
      as.list(hdr_labels)
    )
  ) %>%
  cols_align(
    align = "left",
    columns = "STUB"
  ) %>%
  cols_align(
    align = "center",
    columns = all_of(trt_qc_cols)
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      columns = all_of(trt_qc_cols)
    )
  ) %>%
  tab_style(
    # Set header font size to help treatment labels fit on one line.
    style = cell_text(size = gt::px(14)),
    locations = cells_column_labels(
      columns = all_of(trt_qc_cols)
    )
  ) %>%
  tab_options(
    table.font.size = gt::px(9)
  ) %>%
  cols_width(
    STUB ~ gt::px(240),
    all_of(trt_qc_cols) ~ gt::px(190)
  )


# 15. RENDER THE PDF REPORT --------------------------------------------------

# Render the formatted table as a PDF with protocol details,
# analysis population, data cutoff, table number, and report title.
# The footer includes the report name and the generation timestamp.
#
# Update display_loc if you want to save the PDF in another directory.

DC_T01 %>%
  as_docorator(
    display_name = "duration of exposure R_1",
    display_loc = "C:/Users/USER/OneDrive/Documents",

    header = fancyhead(
      fancyrow(
        left = "Protocol: D123XXXYZV",
        center = NA,
        right = doc_pagenum()
      ),
      fancyrow(
        left = "Population: Safety Analysis Set",
        center = NA,
        right = "Data as of 29JAN2024"
      ),
      fancyrow(
        left = NA,
        center = "Table 11.3.1.1.1",
        right = NA
      ),
      fancyrow(
        left = NA,
        center = "Duration of Exposure (Safety Analysis Set)",
        right = NA
      )
    ),

    footer = fancyfoot(
      fancyrow(
        left = "Javan Mukunzi - Duration of Exposure",
        center = NA,
        right = toupper(
          format(
            Sys.time(),
            "%d%b%Y %H:%M"
          )
        )
      )
    )
  ) %>%
  render_pdf()


# ============================================================================
# END OF PROGRAM
# ============================================================================
