# Load the package required to read ABS Excel workbooks.
library(readxl)


# Define the folder containing the raw ABS files and the output report.
data_dir <- "Data"
report_file <- "audit_report.txt"

age_files <- c(
  "3101051.xlsx" = "New South Wales",
  "3101052.xlsx" = "Victoria",
  "3101053.xlsx" = "Queensland",
  "3101054.xlsx" = "South Australia",
  "3101055.xlsx" = "Western Australia",
  "3101056.xlsx" = "Tasmania",
  "3101057.xlsx" = "Northern Territory",
  "3101058.xlsx" = "Australian Capital Territory"
)


# Convert ABS Excel serial dates into standard R dates.
excel_date <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  as.Date(x, origin = "1899-12-30")
}


# Read an ABS worksheet entirely as text so metadata and observations
# can be inspected consistently.
read_abs_sheet <- function(file, sheet) {
  read_excel(
    file,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
}


# Produce a compact summary of one ABS data worksheet.
audit_sheet <- function(file, sheet, show_series = FALSE) {

  x <- read_abs_sheet(file, sheet)

  series_count <- ncol(x) - 1

  series_names <- trimws(
    as.character(unlist(x[1, -1, drop = TRUE]))
  )

  dates_raw <- x[[1]][11:nrow(x)]
  dates <- excel_date(dates_raw)
  dates <- dates[!is.na(dates)]

  data_matrix <- as.matrix(
    x[11:nrow(x), -1, drop = FALSE]
  )

  missing_count <- sum(
    is.na(data_matrix) | data_matrix == ""
  )

  total_cells <- length(data_matrix)

  cat("\n  Sheet:", sheet, "\n")
  cat("  Rows:", nrow(x), "\n")
  cat("  Columns:", ncol(x), "\n")
  cat("  Series:", series_count, "\n")

  if (length(dates) > 0) {
    cat(
      "  Observation period:",
      format(min(dates)),
      "to",
      format(max(dates)),
      "\n"
    )
  }

  cat(
    "  Frequency:",
    paste(unique(na.omit(as.character(unlist(x[5, -1])))),
          collapse = ", "),
    "\n"
  )

  cat(
    "  Missing observation cells:",
    missing_count,
    "of",
    total_cells,
    sprintf("(%.2f%%)", 100 * missing_count / total_cells),
    "\n"
  )

  if (show_series) {

    cat("\n  SERIES NAMES:\n")

    for (i in seq_along(series_names)) {
      cat(
        sprintf("  %02d. %s\n", i, series_names[i])
      )
    }

  } else {

    cat("\n  Example series:\n")

    examples <- unique(series_names)
    examples <- examples[!is.na(examples)]

    for (item in head(examples, 6)) {
      cat("   -", item, "\n")
    }
  }
}


# Send the audit output to both the terminal and a text report.
sink(report_file, split = TRUE)

cat("========================================\n")
cat("GEOM90007 ASSIGNMENT 2 - ABS DATA AUDIT\n")
cat("========================================\n")


# Audit the state and territory population-change workbook.
cat("\n\n----------------------------------------\n")
cat("310102.xlsx - COMPONENTS OF CHANGE\n")
cat("----------------------------------------\n")

audit_sheet(
  file.path(data_dir, "310102.xlsx"),
  "Data1",
  show_series = TRUE
)


# Audit the state and territory population workbook.
cat("\n\n----------------------------------------\n")
cat("310104.xlsx - POPULATION\n")
cat("----------------------------------------\n")

audit_sheet(
  file.path(data_dir, "310104.xlsx"),
  "Data1",
  show_series = TRUE
)


# Audit both worksheets in every state and territory age/sex workbook.
cat("\n\n========================================\n")
cat("AGE AND SEX WORKBOOKS\n")
cat("========================================\n")

for (file_name in names(age_files)) {

  state <- age_files[[file_name]]
  path <- file.path(data_dir, file_name)

  cat("\n\n----------------------------------------\n")
  cat(file_name, "-", state, "\n")
  cat("----------------------------------------\n")

  sheets <- excel_sheets(path)

  for (sheet in intersect(c("Data1", "Data2"), sheets)) {
    audit_sheet(path, sheet, show_series = FALSE)
  }
}


# Compare the structure of all state and territory age/sex workbooks.
cat("\n\n========================================\n")
cat("AGE/SEX STRUCTURE CHECK\n")
cat("========================================\n")

reference_file <- file.path(data_dir, "3101051.xlsx")
reference <- read_abs_sheet(reference_file, "Data1")
reference_headers <- as.character(unlist(reference[1, -1]))

for (file_name in names(age_files)) {

  current <- read_abs_sheet(
    file.path(data_dir, file_name),
    "Data1"
  )

  current_headers <- as.character(
    unlist(current[1, -1])
  )

  same_structure <- identical(
    reference_headers,
    current_headers
  )

  cat(
    age_files[[file_name]],
    ":",
    ifelse(same_structure,
           "Data1 structure matches NSW",
           "STRUCTURE DIFFERENCE DETECTED"),
    "\n"
  )
}


# Finish the audit report.
cat("\n\n========================================\n")
cat("AUDIT COMPLETE\n")
cat("========================================\n")
cat("Report saved as:", report_file, "\n")

sink()