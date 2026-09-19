# ============================================================
# AUSTRALIA'S POPULATION ENGINE
# GEOM90007 - Assignment 2
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(tidyr)
library(scales)
library(sf)
library(ozmaps)


# ------------------------------------------------------------
# 2. DATA LOADING
# ------------------------------------------------------------

# Locate each file either inside Data/ (development layout) or beside app.R
# (submission ZIP layout). The working directory must contain app.R.
find_data_file <- function(filename) {
  candidates <- c(file.path("Data", filename), filename)
  matches <- candidates[file.exists(candidates)]

  if (length(matches) == 0) {
    stop(
      sprintf(
        "Required data file '%s' was not found. Place it beside app.R or inside the Data folder.",
        filename
      ),
      call. = FALSE
    )
  }

  matches[[1]]
}


read_data_file <- function(filename) {
  path <- find_data_file(filename)

  tryCatch(
    readRDS(path),
    error = function(error) {
      stop(
        sprintf("Could not read '%s': %s", path, conditionMessage(error)),
        call. = FALSE
      )
    }
  )
}


validate_columns <- function(data, dataset_name, required_columns) {
  if (!is.data.frame(data)) {
    stop(sprintf("'%s' must contain a data frame.", dataset_name), call. = FALSE)
  }

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "'%s' is missing required column(s): %s.",
        dataset_name,
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}


components <- read_data_file("components.rds")
population <- read_data_file("population.rds")
age_profiles <- read_data_file("age_profiles.rds")

validate_columns(
  components,
  "components.rds",
  c("date", "state", "component", "value")
)

validate_columns(
  population,
  "population.rds",
  c("date", "state", "sex", "population")
)

validate_columns(
  age_profiles,
  "age_profiles.rds",
  c("date", "state", "sex", "age", "population")
)


# ------------------------------------------------------------
# 3. DATA PREPARATION
# ------------------------------------------------------------

normalise_date <- function(values, dataset_name) {
  converted <- if (inherits(values, "Date")) {
    values
  } else {
    suppressWarnings(as.Date(as.character(values)))
  }

  if (any(is.na(converted))) {
    stop(
      sprintf("'%s' contains an invalid or missing date.", dataset_name),
      call. = FALSE
    )
  }

  converted
}


components$date <- normalise_date(components$date, "components.rds")
population$date <- normalise_date(population$date, "population.rds")
age_profiles$date <- normalise_date(age_profiles$date, "age_profiles.rds")


# Convert the ABS age labels into numeric ages.
# This also handles labels such as "100+" if present.
age_profiles <- age_profiles %>%
  mutate(
    age_numeric = suppressWarnings(
      as.integer(
        gsub(
          "[^0-9]",
          "",
          age
        )
      )
    )
  )


# Aggregate single-year ages into five-year age groups.
# This improves readability in the population pyramid.
age_profiles_grouped <- age_profiles %>%

  filter(
    sex %in% c(
      "Male",
      "Female"
    ),
    !is.na(age_numeric)
  ) %>%

  mutate(

    age_group_start = ifelse(
      age_numeric >= 100,
      100,
      floor(age_numeric / 5) * 5
    ),

    age_group = ifelse(
      age_group_start == 100,
      "100+",
      paste0(
        age_group_start,
        "\u2013",
        pmin(
          age_group_start + 4,
          99
        )
      )
    )
  ) %>%

  group_by(
    state,
    date,
    sex,
    age_group_start,
    age_group
  ) %>%

  summarise(
    population = if (
      all(is.na(population))
    ) {
      NA_real_
    } else {
      sum(population, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%

  mutate(
    year = as.integer(format(date, "%Y"))
  )


# Define the chronological order of age groups.
age_group_levels <- age_profiles_grouped %>%

  distinct(
    age_group_start,
    age_group
  ) %>%

  arrange(
    age_group_start
  ) %>%

  pull(
    age_group
  )


# Extract available annual age-profile years.
age_years <- sort(
  unique(
    as.integer(
      format(
        age_profiles_grouped$date,
        "%Y"
      )
    )
  )
)

age_years <- age_years[!is.na(age_years)]

if (length(age_years) == 0) {
  stop("age_profiles.rds contains no usable age-profile years.", call. = FALSE)
}


age_year_choices <- setNames(
  as.character(age_years),
  as.character(age_years)
)


# Choose sensible default comparison years.
default_year_a <- if (
  1981 %in% age_years
) {
  "1981"
} else {
  as.character(
    min(age_years)
  )
}


default_year_b <- as.character(
  max(age_years)
)


# ------------------------------------------------------------
# PREPARE AUSTRALIAN MAP DATA
# ------------------------------------------------------------

map_states <- ozmap_data(
  "states",
  quiet = TRUE
) %>%

  filter(
    NAME != "Other Territories"
  ) %>%

  rename(
    state = NAME
  ) %>%

  st_transform(
    4326
  )


# ------------------------------------------------------------
# PREPARE DASHBOARD CHOICES
# ------------------------------------------------------------

state_choices <- sort(
  setdiff(
    unique(components$state),
    "Australia"
  )
)

state_choices <- state_choices[
  !is.na(state_choices) &
    nzchar(state_choices)
]


quarter_dates <- sort(
  unique(components$date)
)

quarter_dates <- quarter_dates[!is.na(quarter_dates)]

if (length(state_choices) == 0 || length(quarter_dates) == 0) {
  stop("components.rds contains no usable states or quarters.", call. = FALSE)
}

default_state <- if (
  "Victoria" %in% state_choices
) {
  "Victoria"
} else {
  state_choices[[1]]
}


quarter_labels <- format(
  quarter_dates,
  "%b %Y"
)


quarter_choices <- setNames(
  as.character(quarter_dates),
  quarter_labels
)


# ------------------------------------------------------------
# DEFINE CONSISTENT COLOURS AND LABELS
# ------------------------------------------------------------

driver_colours <- c(
  "Natural Increase" = "#2A9D8F",
  "Net Overseas Migration" = "#3A86FF",
  "Net Interstate Migration" = "#F4A261"
)


driver_labels <- c(
  "Natural Increase" = "Natural increase",
  "Net Overseas Migration" = "Overseas migration",
  "Net Interstate Migration" = "Interstate migration"
)


# Use a separate muted palette for the population pyramid.
sex_colours <- c(
  "Male" = "#577590",
  "Female" = "#B56576"
)


# ------------------------------------------------------------
# 4. HELPER FUNCTIONS
# ------------------------------------------------------------

# Return NA when a rate cannot be calculated safely.
safe_rate <- function(value, population_value) {

  result <- rep(NA_real_, length(value))

  valid <-
    is.finite(value) &
    is.finite(population_value) &
    population_value > 0

  result[valid] <-
    value[valid] /
    population_value[valid] *
    1000

  result
}


format_people <- function(value, show_plus = TRUE) {

  result <- rep("Not available", length(value))
  valid <- is.finite(value)

  if (any(valid)) {

    formatter <- if (show_plus) {
      label_number(
        accuracy = 1,
        big.mark = ",",
        style_positive = "plus"
      )
    } else {
      label_number(
        accuracy = 1,
        big.mark = ","
      )
    }

    result[valid] <- formatter(value[valid])
  }

  result
}


format_rate <- function(
  value,
  suffix = " per 1,000 residents"
) {

  result <- rep("Not available", length(value))
  valid <- is.finite(value)

  if (any(valid)) {
    result[valid] <- paste0(
      label_number(
        accuracy = 0.1,
        style_positive = "plus"
      )(
        value[valid]
      ),
      suffix
    )
  }

  result
}


format_metric <- function(
  value,
  population_value,
  value_mode,
  rate_suffix = " per 1,000 residents"
) {

  if (identical(value_mode, "rate")) {
    format_rate(
      safe_rate(value, population_value),
      suffix = rate_suffix
    )
  } else {
    format_people(value)
  }
}


format_population <- function(value) {

  result <- rep("Not available", length(value))
  valid <- is.finite(value) & value >= 0

  if (any(valid)) {
    result[valid] <- label_number(
      accuracy = 0.1,
      scale_cut = cut_short_scale()
    )(
      value[valid]
    )
  }

  result
}


first_value_or_na <- function(value) {
  if (length(value) == 0) NA_real_ else value[[1]]
}


first_finite_or_na <- function(value) {
  value <- value[is.finite(value)]
  if (length(value) == 0) NA_real_ else value[[1]]
}


tooltip_css <- paste(
  "background-color:#222222;",
  "color:white;",
  "padding:9px 11px;",
  "border-radius:6px;",
  "font-family:'Segoe UI',Arial,sans-serif;",
  "font-size:12px;",
  "line-height:1.45;",
  "text-align:left;",
  "box-shadow:0 4px 12px rgba(0,0,0,0.20);"
)


style_girafe <- function(
  widget,
  hover_css,
  selection_type = "none",
  selected = character(),
  selection_css = ""
) {

  girafe_options(
    widget,
    opts_sizing(
      rescale = TRUE
    ),
    opts_hover(
      css = hover_css
    ),
    opts_selection(
      type = selection_type,
      selected = selected,
      css = selection_css
    ),
    opts_tooltip(
      css = tooltip_css,
      placement = "doc",
      zindex = 99999
    )
  )
}


# ------------------------------------------------------------
# 5. UI
# ------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  bg = "#F5F5F2",
  fg = "#222222",
  primary = "#2C6EAA"
)


ui <- page_fluid(

  theme = app_theme,


  # ----------------------------------------------------------
  # CUSTOM CSS
  # ----------------------------------------------------------

  tags$head(

    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),

    tags$style(

      HTML(
        "

        body {
          background-color: #F5F5F2;
          font-family: \"Segoe UI\", Arial, sans-serif;
        }


        .dashboard-title {
          font-size: 2rem;
          font-weight: 700;
          margin-bottom: 0;
        }

        .dashboard-subtitle {
          color: #666666;
          margin-top: 0.3rem;
          margin-bottom: 1rem;
        }


        .control-label {
          font-size: 0.78rem;
          font-weight: 600;
          color: #555555;
          text-transform: uppercase;
          letter-spacing: 0.04em;
          margin-bottom: 0.35rem;
        }


        .card {
          border: none;
          border-radius: 10px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.06);
          overflow: visible !important;
        }

        .card-body {
          overflow: visible !important;
        }

        .card-header {
          background-color: white;
          border-bottom: 1px solid #EEEEEE;
          font-weight: 600;
        }


        .control-card .card-body {
          padding-top: 0.8rem;
          padding-bottom: 0.6rem;
        }

        .form-group {
          margin-bottom: 0;
        }

        .selectize-control {
          margin-bottom: 0;
          z-index: 1000;
        }

        .selectize-dropdown {
          z-index: 99999 !important;
        }


        .kpi-card .card-body {
          padding-top: 0.8rem;
          padding-bottom: 0.8rem;
        }

        .kpi-label {
          color: #666666;
          font-size: 0.75rem;
          text-transform: uppercase;
          letter-spacing: 0.04em;
          margin-bottom: 0.25rem;
        }

        .kpi-value {
          font-size: 1.45rem;
          font-weight: 700;
          margin-bottom: 0;
          line-height: 1.2;
        }


        .chart-note {
          color: #666666;
          font-size: 0.82rem;
          margin-bottom: 0.25rem;
        }


        .year-control {
          margin-bottom: 0.4rem;
        }

        .year-control .control-label {
          margin-bottom: 0.2rem;
        }


        #driver_map {
          min-height: 390px;
        }

        #driver_timeline {
          min-height: 390px;
        }

        #interstate_momentum {
          min-height: 370px;
        }

        .bslib-grid {
          gap: 0.8rem;
        }

        html, body {
          max-width: 100%;
          overflow-x: hidden;
        }

        body, .form-control, .selectize-input, .selectize-dropdown,
        .shiny-input-container, .btn, .card, .girafe {
          font-family: \"Segoe UI\", Arial, sans-serif;
        }

        body > .container-fluid {
          width: 100%;
          max-width: 1540px;
          margin: 0 auto;
          padding-bottom: 1rem;
        }

        .card, .card-body, .bslib-card, .bslib-grid > * {
          min-width: 0;
        }

        .form-group, .shiny-input-container {
          width: 100%;
        }

        .chart-note {
          line-height: 1.35;
        }

        .fingerprint-year-note {
          color: #4F4F4F;
          font-weight: 600;
          margin-bottom: 0.35rem;
        }

        .availability-note {
          color: #777777;
          font-size: 0.76rem;
          line-height: 1.35;
          margin: 0.05rem 0 0.25rem;
        }

        #population_fingerprint {
          min-height: 390px;
        }

        .driver-legend {
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
          gap: 0.45rem 1.1rem;
          margin-top: 0.15rem;
          color: #444444;
          font-size: 0.78rem;
        }

        .driver-legend-item {
          display: inline-flex;
          align-items: center;
          gap: 0.4rem;
          white-space: nowrap;
        }

        .driver-legend-swatch {
          display: inline-block;
          width: 0.9rem;
          height: 0.72rem;
          flex: 0 0 auto;
          border: 1px solid rgba(0, 0, 0, 0.08);
          border-radius: 2px;
        }

        .dashboard-footer {
          padding: 0.9rem 0.35rem 0.25rem;
          color: #777777;
          font-size: 0.74rem;
          line-height: 1.4;
        }

        @media (max-width: 767.98px) {
          .dashboard-title {
            font-size: 1.65rem;
          }

          .dashboard-subtitle {
            font-size: 0.92rem;
          }

          #driver_map, #driver_timeline, #interstate_momentum {
            min-height: 360px;
          }

        }

        "
      )
    )
  ),


  # ----------------------------------------------------------
  # DASHBOARD HEADER
  # ----------------------------------------------------------

  tags$div(

    class = "px-2 pt-3",

    tags$h1(
      class = "dashboard-title",
      "Australia's Population Engine"
    ),

    tags$p(
      class = "dashboard-subtitle",
      "Explore what is driving population change across Australian states and territories."
    )
  ),


  # ----------------------------------------------------------
  # CONTROL BAR
  # ----------------------------------------------------------

  layout_columns(

    col_widths = breakpoints(
      xs = 12,
      md = c(4, 4, 4)
    ),


    card(

      class = "control-card",

      card_body(

        tags$div(
          class = "control-label",
          "State or territory"
        ),

        selectInput(
          inputId = "state",
          label = NULL,
          choices = state_choices,
          selected = default_state,
          width = "100%"
        )
      )
    ),


    card(

      class = "control-card",

      card_body(

        tags$div(
          class = "control-label",
          "Quarter"
        ),

        selectInput(
          inputId = "quarter",
          label = NULL,
          choices = quarter_choices,
          selected = as.character(
            max(quarter_dates)
          ),
          width = "100%"
        )
      )
    ),


    card(

      class = "control-card",

      card_body(

        tags$div(
          class = "control-label",
          "Display values as"
        ),

        radioButtons(
          inputId = "value_mode",
          label = NULL,
          choices = c(
            "People" = "people",
            "Per 1,000 residents" = "rate"
          ),
          selected = "people",
          inline = TRUE
        )
      )
    )
  ),


  # ----------------------------------------------------------
  # KPI CARDS
  # ----------------------------------------------------------

  layout_columns(

    col_widths = breakpoints(
      xs = 12,
      sm = c(6, 6),
      lg = c(3, 3, 3, 3)
    ),


    card(

      class = "kpi-card",

      card_body(

        tags$div(
          class = "kpi-label",
          "Population"
        ),

        tags$p(
          class = "kpi-value",

          textOutput(
            "population_kpi",
            inline = TRUE
          )
        )
      )
    ),


    card(

      class = "kpi-card",

      card_body(

        tags$div(
          class = "kpi-label",
          "Quarterly change"
        ),

        tags$p(
          class = "kpi-value",

          textOutput(
            "change_kpi",
            inline = TRUE
          )
        )
      )
    ),


    card(

      class = "kpi-card",

      card_body(

        tags$div(
          class = "kpi-label",
          "Largest driver"
        ),

        tags$p(
          class = "kpi-value",

          textOutput(
            "driver_kpi",
            inline = TRUE
          )
        )
      )
    ),


    card(

      class = "kpi-card",

      card_body(

        tags$div(
          class = "kpi-label",
          "Selected area"
        ),

        tags$p(
          class = "kpi-value",

          textOutput(
            "state_kpi",
            inline = TRUE
          )
        )
      )
    )
  ),


  # ----------------------------------------------------------
  # MAIN VISUALISATION AREA
  # ----------------------------------------------------------

  layout_columns(

    col_widths = breakpoints(
      xs = 12,
      lg = c(6, 6)
    ),


    # --------------------------------------------------------
    # GROWTH DRIVER MAP
    # --------------------------------------------------------

    card(

      card_header(
        "Growth Driver Map"
      ),

      card_body(

        tags$p(
          class = "chart-note",
          "Colour shows the published component with the largest absolute contribution. Click a state to explore it."
        ),

        girafeOutput(
          outputId = "driver_map",
          width = "100%",
          height = "400px"
        ),

        tags$div(
          class = "driver-legend",

          lapply(names(driver_colours), function(driver) {
            tags$span(
              class = "driver-legend-item",

              tags$span(
                class = "driver-legend-swatch",
                style = paste0(
                  "background-color:",
                  unname(driver_colours[[driver]]),
                  ";"
                )
              ),

              unname(driver_labels[[driver]])
            )
          })
        )
      )
    ),


    # --------------------------------------------------------
    # DRIVER TIMELINE
    # --------------------------------------------------------

    card(

      card_header(
        "Driver Timeline"
      ),

      card_body(

        tags$p(
          class = "chart-note",
          "Hover for values. Click a quarter to update the entire dashboard."
        ),

        girafeOutput(
          outputId = "driver_timeline",
          width = "100%",
          height = "400px"
        )
      )
    ),


    # --------------------------------------------------------
    # INTERSTATE MOMENTUM
    # --------------------------------------------------------

    card(

      card_header(
        "Interstate Momentum"
      ),

      card_body(

        tags$p(
          class = "chart-note",
          "Compare net interstate migration across states and territories. Click a bar to explore that area."
        ),

        girafeOutput(
          outputId = "interstate_momentum",
          width = "100%",
          height = "380px"
        )
      )
    ),


    # --------------------------------------------------------
    # POPULATION FINGERPRINT
    # --------------------------------------------------------

    card(

      card_header(
        "Population Fingerprint"
      ),

      card_body(

        tags$p(
          class = "chart-note",
          "Compare the annual age and sex structure of the selected area."
        ),

        tags$p(
          class = "chart-note fingerprint-year-note",

          textOutput(
            "fingerprint_year_note",
            inline = TRUE
          )
        ),


        layout_columns(

          col_widths = breakpoints(
            xs = 12,
            sm = c(6, 6)
          ),


          tags$div(

            class = "year-control",

            tags$div(
              class = "control-label",
              "Year A"
            ),

            selectInput(
              inputId = "year_a",
              label = NULL,
              choices = age_year_choices,
              selected = default_year_a,
              width = "100%"
            )
          ),


          tags$div(

            class = "year-control",

            tags$div(
              class = "control-label",
              "Year B"
            ),

            selectInput(
              inputId = "year_b",
              label = NULL,
              choices = age_year_choices,
              selected = default_year_b,
              width = "100%"
            )
          )
        ),

        tags$p(
          class = "availability-note",
          paste0(
            "Annual age profiles available to ",
            max(age_years),
            "."
          )
        ),


        girafeOutput(
          outputId = "population_fingerprint",
          width = "100%",
          height = "390px"
        )
      )
    )
  ),

  tags$footer(
    class = "dashboard-footer",

    tags$div(
      "Source: Australian Bureau of Statistics."
    ),

    tags$div(
      "Population components are shown as published and may not sum exactly to reported quarterly population change."
    )
  )
)


# ------------------------------------------------------------
# 6. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {


  # ----------------------------------------------------------
  # SHARED REACTIVES AND KPIs
  # ----------------------------------------------------------

  selected_quarter <- reactive({
    req(input$quarter)
    as.Date(input$quarter)
  })


  selected_population <- reactive({
    population %>%
      filter(
        state == input$state,
        sex == "Persons",
        date == selected_quarter()
      )
  })


  selected_components <- reactive({
    components %>%
      filter(
        state == input$state,
        date == selected_quarter()
      )
  })


  output$population_kpi <- renderText({
    population_value <- first_finite_or_na(
      selected_population()$population
    )

    format_population(population_value)
  })


  output$change_kpi <- renderText({
    change_value <- selected_components() %>%
      filter(component == "Change Over Previous Quarter") %>%
      pull(value) %>%
      first_value_or_na()

    population_value <- first_finite_or_na(
      selected_population()$population
    )

    format_metric(
      change_value,
      population_value,
      input$value_mode,
      rate_suffix = " / 1,000"
    )
  })


  output$driver_kpi <- renderText({
    data <- selected_components() %>%
      filter(
        component %in% names(driver_colours),
        is.finite(value)
      )

    if (nrow(data) == 0) {
      return("Not available")
    }

    driver <- data %>%
      slice_max(
        order_by = abs(value),
        n = 1,
        with_ties = FALSE
      ) %>%
      pull(component) %>%
      first()

    unname(driver_labels[[driver]])
  })


  output$state_kpi <- renderText({
    if (is.null(input$state) || !nzchar(input$state)) {
      "Not available"
    } else {
      input$state
    }
  })


  output$fingerprint_year_note <- renderText({
    req(input$year_a, input$year_b)

    paste0(
      input$year_a,
      " shown in grey; ",
      input$year_b,
      " shown in colour"
    )
  })


  # Use the same guarded state-selection link for both clickable charts.
  link_chart_to_state <- function(input_id) {

    observeEvent(
      input[[input_id]],
      {
        selected_state <- input[[input_id]]

        if (
          length(selected_state) == 1 &&
          selected_state %in% state_choices &&
          !identical(selected_state, input$state)
        ) {
          updateSelectInput(
            session = session,
            inputId = "state",
            selected = selected_state
          )
        }
      },
      ignoreInit = TRUE
    )
  }


  link_chart_to_state("driver_map_selected")
  link_chart_to_state("interstate_momentum_selected")


  # ==========================================================
  # GROWTH DRIVER MAP
  # ==========================================================


  # ----------------------------------------------------------
  # PREPARE MAP DATA
  # ----------------------------------------------------------

  map_data <- reactive({

    quarter <- selected_quarter()


    driver_data <- components %>%

      filter(
        date == quarter,
        state != "Australia",
        component %in% names(driver_colours)
      )


    dominant_driver <- driver_data %>%

      filter(
        is.finite(value)
      ) %>%

      group_by(
        state
      ) %>%

      slice_max(
        order_by = abs(value),
        n = 1,
        with_ties = FALSE
      ) %>%

      ungroup() %>%

      transmute(
        state,
        driver = component
      )


    component_wide <- expand_grid(
      state = state_choices,
      component = names(driver_colours)
    ) %>%

      left_join(
        driver_data %>%
          select(state, component, value),
        by = c("state", "component")
      ) %>%

      group_by(state, component) %>%

      summarise(
        value = first_finite_or_na(value),
        .groups = "drop"
      ) %>%

      pivot_wider(
        names_from = component,
        values_from = value
      )


    quarterly_change <- components %>%

      filter(
        date == quarter,
        state != "Australia",
        component ==
          "Change Over Previous Quarter"
      ) %>%

      group_by(state) %>%

      summarise(
        quarterly_change = first_finite_or_na(value),
        .groups = "drop"
      )


    quarter_population <- population %>%

      filter(
        date == quarter,
        state != "Australia",
        sex == "Persons"
      ) %>%

      group_by(state) %>%

      summarise(
        population = first_finite_or_na(population),
        .groups = "drop"
      )


    data <- map_states %>%

      left_join(
        dominant_driver,
        by = "state"
      ) %>%

      left_join(
        component_wide,
        by = "state"
      ) %>%

      left_join(
        quarterly_change,
        by = "state"
      ) %>%

      left_join(
        quarter_population,
        by = "state"
      )


    # --------------------------------------------------------
    # FORMAT MAP VALUES
    # --------------------------------------------------------

    data$natural_display <- format_metric(
      data$`Natural Increase`,
      data$population,
      input$value_mode
    )


    data$overseas_display <- format_metric(
      data$`Net Overseas Migration`,
      data$population,
      input$value_mode
    )


    data$interstate_display <- format_metric(
      data$`Net Interstate Migration`,
      data$population,
      input$value_mode
    )


    data$change_display <- format_metric(
      data$quarterly_change,
      data$population,
      input$value_mode
    )


    data$population_display <- format_population(
      data$population
    )


    data$driver_display <- unname(
      driver_labels[
        as.character(
          data$driver
        )
      ]
    )


    data$driver_display[
      is.na(
        data$driver_display
      )
    ] <- "Not available"


    data$tooltip <- paste0(

      "<strong>",
      data$state,
      "</strong><br>",

      format(
        quarter,
        "%B %Y"
      ),

      "<br><br>",

      "<strong>Population</strong>: ",
      data$population_display,

      "<br>",

      "<strong>Quarterly change</strong>: ",
      data$change_display,

      "<br><br>",

      "Natural increase: ",
      data$natural_display,

      "<br>",

      "Overseas migration: ",
      data$overseas_display,

      "<br>",

      "Interstate migration: ",
      data$interstate_display,

      "<br><br>",

      "<strong>Largest driver</strong>: ",
      data$driver_display
    )


    data$driver <- factor(
      data$driver,
      levels = names(
        driver_colours
      )
    )


    data
  })


  # ----------------------------------------------------------
  # RENDER GROWTH DRIVER MAP
  # ----------------------------------------------------------

  output$driver_map <- renderGirafe({

    data <- map_data()

    validate(
      need(
        nrow(data) > 0,
        "Map data is not available for this quarter."
      )
    )


    map_plot <- ggplot(
      data = data
    ) +

      geom_sf_interactive(

        aes(
          fill = driver,
          tooltip = tooltip,
          data_id = state
        ),

        colour = "#FFFFFF",
        linewidth = 0.7
      ) +

      scale_fill_manual(

        values = driver_colours,

        breaks = names(
          driver_colours
        ),

        labels = unname(
          driver_labels
        ),

        drop = FALSE,

        na.value = "#D9D9D4",

        guide = "none"
      ) +

      coord_sf(
        xlim = c(
          112,
          154
        ),
        ylim = c(
          -44,
          -10
        ),
        expand = FALSE
      ) +

      theme_void(
        base_family = "Segoe UI"
      ) +

      theme(
        plot.margin = margin(
          5,
          5,
          5,
          5
        )
      )


    widget <- girafe(
      ggobj = map_plot,
      width_svg = 7,
      height_svg = 5
    )


    style_girafe(
      widget,
      hover_css = "stroke:#222222;stroke-width:2px;cursor:pointer;",
      selection_type = "single",
      selected = input$state,
      selection_css = "stroke:#111111;stroke-width:3px;"
    )
  })


  # ==========================================================
  # DRIVER TIMELINE
  # ==========================================================


  # ----------------------------------------------------------
  # PREPARE TIMELINE DATA
  # ----------------------------------------------------------

  timeline_data <- reactive({

    driver_history <- components %>%

      filter(
        state == input$state,
        component %in% names(driver_colours)
      )


    population_history <- population %>%

      filter(
        state == input$state,
        sex == "Persons"
      ) %>%

      select(
        date,
        population
      )


    data <- driver_history %>%

      left_join(
        population_history,
        by = "date"
      )


    if (
      input$value_mode == "rate"
    ) {

      data <- data %>%

        mutate(
          display_value = safe_rate(
            value,
            population
          )
        )

    } else {

      data <- data %>%

        mutate(
          display_value = value
        )
    }


    data$component <- factor(
      data$component,
      levels = names(
        driver_colours
      )
    )


    data$component_label <- factor(

      as.character(
        data$component
      ),

      levels = names(
        driver_colours
      ),

      labels = unname(
        driver_labels
      )
    )


    if (
      input$value_mode == "rate"
    ) {

      data$value_display <- format_rate(
        data$display_value
      )

    } else {

      data$value_display <- format_people(
        data$display_value
      )
    }


    data$tooltip <- paste0(

      "<strong>",
      as.character(
        data$component_label
      ),
      "</strong><br>",

      input$state,

      "<br>",

      format(
        data$date,
        "%B %Y"
      ),

      "<br><br>",

      "<strong>",
      data$value_display,
      "</strong>"
    )


    data
  })


  # ----------------------------------------------------------
  # RENDER DRIVER TIMELINE
  # ----------------------------------------------------------

  output$driver_timeline <- renderGirafe({

    data <- timeline_data()


    validate(
      need(
        nrow(data) > 0,
        "Timeline data is not available for this area."
      ),
      need(
        any(is.finite(data$display_value)),
        "Timeline values are not available for this selection."
      )
    )


    y_axis_label <- if (
      input$value_mode == "rate"
    ) {

      "People per 1,000 residents"

    } else {

      "People"
    }


    timeline_plot <- ggplot(

      data,

      aes(
        x = date,
        y = display_value,
        group = component
      )

    ) +

      geom_hline(
        yintercept = 0,
        colour = "#B5B5B5",
        linewidth = 0.45
      ) +

      geom_vline(
        xintercept = selected_quarter(),
        colour = "#444444",
        linewidth = 0.6,
        linetype = "dashed"
      ) +

      geom_line(
        aes(
          colour = component
        ),
        linewidth = 0.8,
        na.rm = TRUE
      ) +

      geom_point_interactive(

        aes(
          colour = component,
          tooltip = tooltip,
          data_id = as.character(
            date
          )
        ),

        size = 1.8,
        alpha = 0.55,
        na.rm = TRUE
      ) +

      facet_wrap(
        ~ component_label,
        ncol = 1,
        scales = "free_y"
      ) +

      scale_colour_manual(
        values = driver_colours,
        guide = "none"
      ) +

      scale_x_date(
        date_breaks = "10 years",
        date_labels = "%Y",
        expand = expansion(
          mult = c(
            0.01,
            0.02
          )
        )
      ) +

      labs(
        x = "Quarter",
        y = y_axis_label
      ) +

      theme_minimal(
        base_family = "Segoe UI"
      ) +

      theme(

        panel.grid.minor = element_blank(),

        panel.grid.major.x = element_blank(),

        panel.grid.major.y = element_line(
          colour = "#E5E5E5",
          linewidth = 0.35
        ),

        strip.text = element_text(
          size = 9.5,
          face = "bold",
          colour = "#333333",
          hjust = 0
        ),

        strip.background = element_blank(),

        axis.title = element_text(
          size = 9,
          colour = "#555555"
        ),

        axis.text = element_text(
          size = 8,
          colour = "#666666"
        ),

        plot.margin = margin(
          5,
          8,
          5,
          5
        )
      )


    widget <- girafe(
      ggobj = timeline_plot,
      width_svg = 7,
      height_svg = 5
    )


    style_girafe(
      widget,
      hover_css = paste0(
        "fill:#222222;stroke:#222222;",
        "stroke-width:2px;cursor:pointer;"
      ),
      selection_type = "single",
      selected = as.character(selected_quarter()),
      selection_css = paste0(
        "fill:#222222;stroke:#222222;",
        "stroke-width:2px;"
      )
    )
  })


  # ----------------------------------------------------------
  # TIMELINE → QUARTER LINK
  # ----------------------------------------------------------

  observeEvent(

    input$driver_timeline_selected,

    {

      selected_date <-
        input$driver_timeline_selected


      if (
        length(selected_date) == 1 &&
        selected_date %in%
          as.character(
            quarter_dates
          ) &&
        !identical(
          selected_date,
          input$quarter
        )
      ) {

        updateSelectInput(
          session = session,
          inputId = "quarter",
          selected = selected_date
        )
      }
    },

    ignoreInit = TRUE
  )


  # ==========================================================
  # INTERSTATE MOMENTUM
  # ==========================================================


  # ----------------------------------------------------------
  # PREPARE INTERSTATE DATA
  # ----------------------------------------------------------

  interstate_data <- reactive({

    quarter <- selected_quarter()


    interstate <- components %>%

      filter(
        date == quarter,
        state != "Australia",
        component ==
          "Net Interstate Migration"
      ) %>%

      select(
        state,
        value
      )


    quarter_population <- population %>%

      filter(
        date == quarter,
        state != "Australia",
        sex == "Persons"
      ) %>%

      select(
        state,
        population
      )


    data <- interstate %>%

      left_join(
        quarter_population,
        by = "state"
      )


    if (
      input$value_mode == "rate"
    ) {

      data <- data %>%

        mutate(
          display_value = safe_rate(
            value,
            population
          )
        )

    } else {

      data <- data %>%

        mutate(
          display_value = value
        )
    }


    data <- data %>%

      mutate(

        direction = case_when(
          display_value > 0 ~ "Net gain",
          display_value < 0 ~ "Net loss",
          is.finite(display_value) ~ "No net change",
          TRUE ~ "Not available"
        )
      )


    if (
      input$value_mode == "rate"
    ) {

      data$value_display <- format_rate(
        data$display_value
      )

    } else {

      data$value_display <- format_people(
        data$display_value
      )
    }


    data$population_display <- format_population(
      data$population
    )


    data$tooltip <- paste0(

      "<strong>",
      data$state,
      "</strong><br>",

      format(
        quarter,
        "%B %Y"
      ),

      "<br><br>",

      "<strong>Net interstate migration</strong><br>",

      data$value_display,

      "<br><br>",

      data$direction,

      "<br>",

      "Population: ",
      data$population_display
    )


    data <- data %>%

      filter(
        is.finite(display_value)
      ) %>%

      arrange(
        display_value
      )


    data$state_order <- factor(
      data$state,
      levels = data$state
    )


    data
  })


  # ----------------------------------------------------------
  # RENDER INTERSTATE MOMENTUM
  # ----------------------------------------------------------

  output$interstate_momentum <- renderGirafe({

    data <- interstate_data()


    validate(
      need(
        nrow(data) > 0,
        "Interstate migration data is not available for this selection."
      )
    )


    x_axis_label <- if (
      input$value_mode == "rate"
    ) {

      "Net interstate migration per 1,000 residents"

    } else {

      "Net interstate migration (people)"
    }


    interstate_plot <- ggplot(

      data,

      aes(
        x = display_value,
        y = state_order
      )

    ) +

      geom_vline(
        xintercept = 0,
        colour = "#666666",
        linewidth = 0.6
      ) +

      geom_col_interactive(

        aes(
          tooltip = tooltip,
          data_id = state,
          colour = state == input$state
        ),

        fill = unname(
          driver_colours[
            "Net Interstate Migration"
          ]
        ),

        linewidth = 1
      ) +

      scale_colour_manual(

        values = c(
          "FALSE" = "#FFFFFF",
          "TRUE" = "#111111"
        ),

        guide = "none"
      ) +

      scale_x_continuous(

        labels = if (
          input$value_mode == "rate"
        ) {

          label_number(
            accuracy = 0.1
          )

        } else {

          label_number(
            accuracy = 1,
            big.mark = ","
          )
        },

        expand = expansion(
          mult = c(
            0.08,
            0.08
          )
        )
      ) +

      labs(
        subtitle = "Net loss  <---        zero        --->  Net gain",
        x = x_axis_label,
        y = NULL
      ) +

      theme_minimal(
        base_family = "Segoe UI"
      ) +

      theme(

        panel.grid.minor = element_blank(),

        panel.grid.major.y = element_blank(),

        panel.grid.major.x = element_line(
          colour = "#E5E5E5",
          linewidth = 0.35
        ),

        axis.text.y = element_text(
          size = 9,
          colour = "#333333"
        ),

        axis.text.x = element_text(
          size = 8,
          colour = "#666666"
        ),

        axis.title.x = element_text(
          size = 9,
          colour = "#555555",
          margin = margin(
            t = 8
          )
        ),

        plot.subtitle = element_text(
          size = 9,
          colour = "#666666",
          hjust = 0.5,
          margin = margin(
            b = 4
          )
        ),

        plot.margin = margin(
          8,
          12,
          8,
          5
        )
      )


    widget <- girafe(
      ggobj = interstate_plot,
      width_svg = 7,
      height_svg = 4.5
    )


    style_girafe(
      widget,
      hover_css = paste0(
        "stroke:#111111;stroke-width:2px;",
        "cursor:pointer;opacity:0.85;"
      ),
      selection_type = "single",
      selected = input$state,
      selection_css = "stroke:#111111;stroke-width:3px;"
    )
  })


  # ==========================================================
  # POPULATION FINGERPRINT
  # ==========================================================


  # ----------------------------------------------------------
  # PREPARE POPULATION PYRAMID DATA
  # ----------------------------------------------------------

  fingerprint_data <- reactive({

    req(
      input$year_a,
      input$year_b
    )

    year_a <- as.integer(
      input$year_a
    )

    year_b <- as.integer(
      input$year_b
    )


    state_data <- age_profiles_grouped %>%

      filter(
        state == input$state
      )


    # Extract Year A values.
    data_a <- state_data %>%

      filter(
        year == year_a
      ) %>%

      select(
        sex,
        age_group_start,
        age_group,
        population
      ) %>%

      rename(
        population_a = population
      )


    # Extract Year B values.
    data_b <- state_data %>%

      filter(
        year == year_b
      ) %>%

      select(
        sex,
        age_group_start,
        age_group,
        population
      ) %>%

      rename(
        population_b = population
      )


    # Join the two years so each age and sex combination
    # can show both values in one tooltip.
    data <- full_join(

      data_a,
      data_b,

      by = c(
        "sex",
        "age_group_start",
        "age_group"
      )
    ) %>%

      mutate(

        change = ifelse(
          is.finite(population_a) &
            is.finite(population_b),
          population_b - population_a,
          NA_real_
        ),

        plot_a_value = coalesce(
          population_a,
          0
        ),

        plot_b_value = coalesce(
          population_b,
          0
        ),


        # Male values are plotted to the left of zero and
        # female values to the right.
        plot_a = ifelse(
          sex == "Male",
          -plot_a_value,
          plot_a_value
        ),

        plot_b = ifelse(
          sex == "Male",
          -plot_b_value,
          plot_b_value
        ),

        age_group = factor(
          age_group,
          levels = age_group_levels
        )
      )


    # --------------------------------------------------------
    # FORMAT TOOLTIP VALUES
    # --------------------------------------------------------

    data$population_a_display <- format_people(
      data$population_a,
      show_plus = FALSE
    )


    data$population_b_display <- format_people(
      data$population_b,
      show_plus = FALSE
    )


    data$change_display <- format_people(
      data$change
    )


    data$tooltip <- paste0(

      "<strong>",
      input$state,
      "</strong><br>",

      "Age ",
      as.character(
        data$age_group
      ),

      " · ",
      data$sex,

      "<br><br>",

      "<strong>",
      year_a,
      "</strong>: ",
      data$population_a_display,

      "<br>",

      "<strong>",
      year_b,
      "</strong>: ",
      data$population_b_display,

      "<br><br>",

      "<strong>Change</strong>: ",
      data$change_display
    )


    data$data_id <- paste(
      data$sex,
      data$age_group,
      sep = "_"
    )


    data
  })


  # ----------------------------------------------------------
  # RENDER POPULATION FINGERPRINT
  # ----------------------------------------------------------

  output$population_fingerprint <- renderGirafe({

    data <- fingerprint_data()


    validate(
      need(
        nrow(data) > 0,
        "Age-profile data is not available for this selection."
      ),
      need(
        any(is.finite(data$population_a)) ||
          any(is.finite(data$population_b)),
        "Age-profile values are not available for these years."
      )
    )


    fingerprint_plot <- ggplot(

      data,

      aes(
        x = age_group
      )

    ) +

      # Central reference line between male and female sides.
      geom_hline(
        yintercept = 0,
        colour = "#666666",
        linewidth = 0.5
      ) +


      # Year A is displayed as a wider grey silhouette.
      geom_col(

        aes(
          y = plot_a
        ),

        fill = "#D9D9D4",
        width = 0.82
      ) +


      # Year B sits on top in colour and provides the
      # interactive tooltip.
      geom_col_interactive(

        aes(
          y = plot_b,
          fill = sex,
          tooltip = tooltip,
          data_id = data_id
        ),

        width = 0.55
      ) +

      scale_fill_manual(
        values = sex_colours,
        breaks = c("Male", "Female"),
        drop = FALSE
      ) +

      scale_y_continuous(

        labels = function(x) {

          label_number(
            accuracy = 1,
            scale_cut = cut_short_scale()
          )(
            abs(x)
          )
        },

        expand = expansion(
          mult = c(
            0.08,
            0.08
          )
        )
      ) +

      coord_flip() +

      labs(
        x = "Age group",
        y = "Population",
        fill = NULL
      ) +

      theme_minimal(
        base_family = "Segoe UI"
      ) +

      theme(

        panel.grid.minor = element_blank(),

        panel.grid.major.y = element_blank(),

        panel.grid.major.x = element_line(
          colour = "#E5E5E5",
          linewidth = 0.35
        ),

        axis.text.y = element_text(
          size = 8.5,
          colour = "#444444"
        ),

        axis.text.x = element_text(
          size = 8,
          colour = "#666666"
        ),

        axis.title = element_text(
          size = 9,
          colour = "#555555"
        ),

        legend.position = "bottom",

        legend.title = element_blank(),

        legend.text = element_text(
          size = 9,
          colour = "#444444"
        ),

        plot.margin = margin(
          5,
          10,
          5,
          5
        )
      )


    widget <- girafe(
      ggobj = fingerprint_plot,
      width_svg = 7,
      height_svg = 4.8
    )


    style_girafe(
      widget,
      hover_css = paste0(
        "stroke:#111111;stroke-width:1.5px;",
        "opacity:0.85;cursor:pointer;"
      )
    )
  })
}


# ------------------------------------------------------------
# 7. APP LAUNCH
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
