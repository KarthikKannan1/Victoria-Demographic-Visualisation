# Load packages required to read and transform the ABS data.
library(readxl)


# Define the location of the raw ABS data.
data_dir <- "Data"


# Convert ABS Excel serial dates to standard R dates.
excel_date <- function(x) {
  as.Date(as.numeric(x), origin = "1899-12-30")
}


# Read the population components workbook without using the ABS metadata rows as column names.
raw_components <- read_excel(
  file.path(data_dir, "310102.xlsx"),
  sheet = "Data1",
  col_names = FALSE,
  col_types = "text",
  .name_repair = "minimal"
)


# Extract the series descriptions and split each description into population-change component and geographic area.
series_names <- trimws(
  as.character(unlist(raw_components[1, -1]))
)

series_parts <- strsplit(series_names, ";", fixed = TRUE)

component <- sapply(series_parts, function(x) trimws(x[1]))
state <- sapply(series_parts, function(x) trimws(x[2]))


# Extract the quarterly observations beginning after the ABS metadata rows.
dates <- excel_date(raw_components[[1]][11:nrow(raw_components)])

values <- raw_components[
  11:nrow(raw_components),
  -1,
  drop = FALSE
]


# Convert the wide ABS spreadsheet into a tidy long-format dataset.
components <- data.frame(
  date = rep(dates, times = ncol(values)),
  state = rep(state, each = length(dates)),
  component = rep(component, each = length(dates)),
  value = as.numeric(unlist(values)),
  stringsAsFactors = FALSE
)


# Order the dataset consistently for later use in Shiny.
components <- components[
  order(components$state, components$date, components$component),
]


# Display basic validation information.
cat("Rows:", nrow(components), "\n")
cat(
  "Dates:",
  format(min(components$date)),
  "to",
  format(max(components$date)),
  "\n"
)
cat("States/areas:", length(unique(components$state)), "\n")
cat("Components:", paste(unique(components$component), collapse = " | "), "\n")
cat("Missing values:", sum(is.na(components$value)), "\n")

# Validate whether the three population-change drivers reconcile with the
# ABS-reported change over the previous quarter for each state and quarter.
state_components <- components[
  components$state != "Australia",
]

wide_components <- reshape(
  state_components,
  idvar = c("date", "state"),
  timevar = "component",
  direction = "wide"
)

names(wide_components) <- sub(
  "^value\\.",
  "",
  names(wide_components)
)

wide_components$calculated_change <-
  wide_components$`Natural Increase` +
  wide_components$`Net Overseas Migration` +
  wide_components$`Net Interstate Migration`

wide_components$difference <-
  wide_components$calculated_change -
  wide_components$`Change Over Previous Quarter`


# Report how closely the component sum matches the published ABS change.
valid_differences <- wide_components$difference[
  !is.na(wide_components$difference)
]

cat("\n--- Reconciliation check ---\n")
cat("Comparable quarters:", length(valid_differences), "\n")
cat("Exact matches:", sum(valid_differences == 0), "\n")
cat(
  "Maximum absolute difference:",
  max(abs(valid_differences)),
  "\n"
)
cat(
  "Mean absolute difference:",
  mean(abs(valid_differences)),
  "\n"
)

# Identify where reconciliation differences are largest to determine whether
# discrepancies are isolated or systematic across the ABS time series.
wide_components$absolute_difference <- abs(
  wide_components$difference
)

largest_differences <- wide_components[
  order(-wide_components$absolute_difference),
  c(
    "date",
    "state",
    "Natural Increase",
    "Net Overseas Migration",
    "Net Interstate Migration",
    "Change Over Previous Quarter",
    "calculated_change",
    "difference"
  )
]

cat("\n--- Largest reconciliation differences ---\n")
print(head(largest_differences, 10), row.names = FALSE)

# Save the cleaned dataset in an efficient R format for later use by Shiny.
saveRDS(
  components,
  file = "Data/components.rds"
)

# Read the quarterly estimated resident population workbook.
raw_population <- read_excel(
  file.path(data_dir, "310104.xlsx"),
  sheet = "Data1",
  col_names = FALSE,
  col_types = "text",
  .name_repair = "minimal"
)


# Extract sex and geographic area from each ABS population series description.
population_series <- trimws(
  as.character(unlist(raw_population[1, -1]))
)

population_parts <- strsplit(
  population_series,
  ";",
  fixed = TRUE
)

sex <- sapply(
  population_parts,
  function(x) trimws(x[2])
)

population_state <- sapply(
  population_parts,
  function(x) trimws(x[3])
)


# Extract quarterly dates and population observations.
population_dates <- excel_date(
  raw_population[[1]][11:nrow(raw_population)]
)

population_values <- raw_population[
  11:nrow(raw_population),
  -1,
  drop = FALSE
]


# Convert the ABS population workbook into tidy long format.
population <- data.frame(
  date = rep(population_dates, times = ncol(population_values)),
  state = rep(population_state, each = length(population_dates)),
  sex = rep(sex, each = length(population_dates)),
  population = as.numeric(unlist(population_values)),
  stringsAsFactors = FALSE
)

population <- population[
  order(population$state, population$date, population$sex),
]


# Validate the cleaned quarterly population dataset.
cat("\n--- Population dataset ---\n")
cat("Rows:", nrow(population), "\n")
cat(
  "Dates:",
  format(min(population$date)),
  "to",
  format(max(population$date)),
  "\n"
)
cat("States/areas:", length(unique(population$state)), "\n")
cat(
  "Sex categories:",
  paste(unique(population$sex), collapse = " | "),
  "\n"
)
cat(
  "Missing values:",
  sum(is.na(population$population)),
  "\n"
)


# Check whether male and female totals reconcile with the published Persons total.
population_check <- reshape(
  population,
  idvar = c("date", "state"),
  timevar = "sex",
  direction = "wide"
)

population_check$difference <-
  population_check$population.Male +
  population_check$population.Female -
  population_check$population.Persons

cat(
  "Maximum Male + Female reconciliation difference:",
  max(abs(population_check$difference), na.rm = TRUE),
  "\n"
)


# Save the cleaned quarterly population dataset for later use by Shiny.
saveRDS(
  population,
  file = "Data/population.rds"
)


# Define the eight ABS age and sex workbooks and their corresponding states and territories.
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


# Read and tidy one worksheet from an ABS age and sex workbook.
read_age_sheet <- function(file_name, state_name, sheet_name) {

  raw <- read_excel(
    file.path(data_dir, file_name),
    sheet = sheet_name,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )

  series_names <- trimws(
    as.character(unlist(raw[1, -1]))
  )

  series_parts <- strsplit(
    series_names,
    ";",
    fixed = TRUE
  )

  sex <- sapply(
    series_parts,
    function(x) trimws(x[2])
  )

  age <- sapply(
    series_parts,
    function(x) trimws(x[3])
  )

  dates <- excel_date(
    raw[[1]][11:nrow(raw)]
  )

  values <- raw[
    11:nrow(raw),
    -1,
    drop = FALSE
  ]

  data.frame(
    date = rep(dates, times = ncol(values)),
    state = state_name,
    sex = rep(sex, each = length(dates)),
    age = rep(age, each = length(dates)),
    population = as.numeric(unlist(values)),
    stringsAsFactors = FALSE
  )
}


# Combine Data1 and Data2 from every state and territory workbook into one consistent age-profile dataset.
age_profiles_list <- list()

for (file_name in names(age_files)) {

  state_name <- age_files[[file_name]]

  data1 <- read_age_sheet(
    file_name,
    state_name,
    "Data1"
  )

  data2 <- read_age_sheet(
    file_name,
    state_name,
    "Data2"
  )

  age_profiles_list[[state_name]] <- rbind(
    data1,
    data2
  )
}

age_profiles <- do.call(
  rbind,
  age_profiles_list
)

rownames(age_profiles) <- NULL


# Order the combined dataset consistently for later use in Shiny.
age_profiles <- age_profiles[
  order(
    age_profiles$state,
    age_profiles$date,
    age_profiles$sex,
    age_profiles$age
  ),
]


# Validate the combined age and sex dataset before saving it.
cat("\n--- Age profile dataset ---\n")
cat("Rows:", nrow(age_profiles), "\n")
cat(
  "Dates:",
  format(min(age_profiles$date)),
  "to",
  format(max(age_profiles$date)),
  "\n"
)
cat(
  "States/territories:",
  length(unique(age_profiles$state)),
  "\n"
)
cat(
  "Sex categories:",
  paste(unique(age_profiles$sex), collapse = " | "),
  "\n"
)
cat(
  "Age categories:",
  length(unique(age_profiles$age)),
  "\n"
)
cat(
  "Age range:",
  paste(head(sort(unique(age_profiles$age)), 1)),
  "to",
  paste(tail(sort(unique(age_profiles$age)), 1)),
  "\n"
)
cat(
  "Missing population values:",
  sum(is.na(age_profiles$population)),
  "\n"
)


# Check that every state and territory contains the same number of observations.
cat("\nObservations by state/territory:\n")
print(
  table(age_profiles$state)
)


# Check for duplicate combinations of state, date, sex and age after combining Data1 and Data2.
duplicate_rows <- duplicated(
  age_profiles[
    c("state", "date", "sex", "age")
  ]
)

cat(
  "\nDuplicate state/date/sex/age combinations:",
  sum(duplicate_rows),
  "\n"
)


# Save the cleaned age-profile dataset for later use by Shiny.
saveRDS(
  age_profiles,
  file = "Data/age_profiles.rds"
)