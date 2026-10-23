# GEOM90007 Assignment 2 - AUSTRALIA'S POPULATION ENGINE
# Karthik Kannan (1739619)


# 1. LOAD PACKAGES - Load the packages used for the dashboard, charts and map.
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(tidyr)
library(scales)
library(sf)
library(ozmaps)


# 2. DATA LOADING - read in the RDS files and check they have the columns we need.

# Check both the Data folder and the app.R folder itself, since the data files could be in either place depending on how the project is set up.
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


# Quick sanity check that each dataset actually has the columns the dashboard uses.
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



# 3. DATA PREPARATION - tidy the dates, build the age bands, and set up the map and dropdown choices.


# Make sure every date column is a real Date before we filter or join on it.
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


# Pull the number out of the ABS age labels, which also handles labels like "100+".
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


# Bucket the single ages into five-year bands, otherwise the pyramid is far too cluttered.
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


# Age groups in youngest-to-oldest order, used as the factor levels on the pyramid axis.
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


# Only offer years that actually exist in the age data.
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


# Start on 1981 vs the latest year, but fall back to the earliest year if 1981 is missing.
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



# AUSTRALIAN MAP DATA PREP

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


# DASHBOARD CHOICES PREP

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


# CONSISTENT COLORING AND LABELS

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


# Muted tones for the male/female sides of the pyramid so it does not clash with the driver colours.
sex_colours <- c(
  "Male" = "#577590",
  "Female" = "#B56576"
)


# 4. HELPER FUNCTIONS - small formatting and chart-styling functions reused across the dashboard.

# Turn a raw count into a rate per 1,000 residents. Anything that cannot be worked out stays NA so the dashboard shows "Not available" instead of an invalid value.
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


# One tooltip style so all four charts look the same on hover.
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


# Shared hover, selection and tooltip options, so I am not repeating these for every chart.
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


# Sizes the SVG to the card it sits in, otherwise the labels get squashed when the window resizes, and tags the chart with the sidebar input its marks should drive when clicked.
card_girafe <- function(plot, output_id, session, selection_input = NULL) {
  plot_width <- session$clientData[[paste0("output_", output_id, "_width")]]
  plot_height <- session$clientData[[paste0("output_", output_id, "_height")]]
  req(plot_width > 0, plot_height > 0)

  widget <- girafe(
    ggobj = plot,
    width_svg = plot_width / 96,
    height_svg = plot_height / 96
  )

  if (!is.null(selection_input)) {
    # The link goes on the SVG itself rather than the container, so clicks still work when bslib moves the chart into its fullscreen overlay.
    widget <- htmlwidgets::onRender(
      widget,
      "function(el, x, data) {
        const svg = el.querySelector('.girafe_container_std > svg');
        if (svg) {
          svg.dataset.selectionInput = data.input_id;
          svg.dataset.outputId = el.id;
        }
        window.populationCharts.render(el);
      }",
      data = list(input_id = selection_input)
    )
  }

  widget
}


# Little "i" button next to a control label, for extra explanation without cluttering the sidebar.
control_info <- function(label, text) {
  bslib::tooltip(
    tags$button(
      type = "button",
      class = "control-info",
      `aria-label` = paste("About", label),
      tags$span(`aria-hidden` = "true", "i")
    ),
    text,
    options = list(trigger = "hover focus click", container = "body")
  )
}


# 5. UI - the sidebar controls, KPI cards, chart layout and all the custom styling.

app_theme <- bs_theme(
  version = 5,
  bg = "#F5F5F2",
  fg = "#222222",
  primary = "#2C6EAA"
)


ui <- page_fluid(
  theme = app_theme,


  # CUSTOM CSS

  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),

    # Clicking a mark on a chart sets the matching dropdown rather than the chart's own selection, so the dropdowns stay the single source of truth. It has to be a real click listener because ggiraph also fires selection events on redraw, and those are not user clicks.
    tags$script(HTML("
      document.addEventListener('click', function(event) {
        if (!(event.target instanceof Element)) return;
        const mark = event.target.closest('svg[data-selection-input] [data-id]');
        if (!mark) return;

        const svg = mark.closest('svg[data-selection-input]');
        const input = document.getElementById(svg.dataset.selectionInput);
        const value = mark.getAttribute('data-id');
        if (!input || !input.selectize ||
            !Object.prototype.hasOwnProperty.call(input.selectize.options, value)) return;

        // Set the dropdown and let Shiny push the change out to every chart. Stopping the event blocks ggiraph's own toggle, otherwise a stale chart can fire afterwards and undo a newer selection.
        event.stopPropagation();
        if (input.value !== value) input.selectize.setValue(value);
      }, true);
    ")),

    # Playback steps through the quarter dropdown on a timer. Only the playing state is kept here, the quarter itself stays in the dropdown so there is never a second copy to keep in sync.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        const button = document.getElementById('quarter_play');
        const quarter = document.getElementById('quarter');
        const endQuarter = document.getElementById('quarter_end');
        const boundaryHint = document.getElementById('quarter_boundary_hint');
        const interval = 1100;
        let playing = false;
        let timer = null;
        let advancing = false;

        function stopPlayback() {
          playing = false;
          window.clearTimeout(timer);
          timer = null;
          button.textContent = 'Play';
          button.setAttribute('aria-label', 'Play quarter animation');
          button.setAttribute('aria-pressed', 'false');
        }

        // The end quarter only sets where playback stops, it never moves the current quarter.
        function checkBoundary() {
          const canPlay = quarter.value && endQuarter.value && quarter.value < endQuarter.value;
          if (!canPlay) stopPlayback();
          button.disabled = !canPlay;
          boundaryHint.hidden = !!canPlay;
          if (canPlay) button.removeAttribute('aria-describedby');
          else button.setAttribute('aria-describedby', boundaryHint.id);
        }

        function showQuarter(value) {
          advancing = true;
          try {
            quarter.selectize.setValue(value);
          } finally {
            advancing = false;
          }
        }

        function advanceQuarter() {
          if (!playing) return;
          const choices = Object.keys(quarter.selectize.options).sort();
          const index = choices.indexOf(quarter.value);
          const endIndex = choices.indexOf(endQuarter.value);
          if (index < 0 || endIndex < 0 || index >= endIndex) {
            stopPlayback();
            return;
          }
          // Wait for the current redraw to finish, otherwise requests pile up on slower machines.
          if (document.documentElement.classList.contains('shiny-busy')) {
            timer = window.setTimeout(advanceQuarter, interval);
            return;
          }
          showQuarter(choices[index + 1]);
          if (index + 1 >= endIndex) stopPlayback();
          else timer = window.setTimeout(advanceQuarter, interval);
        }

        button.addEventListener('click', function() {
          if (playing) {
            stopPlayback();
            return;
          }
          if (!quarter.selectize) return;
          const choices = Object.keys(quarter.selectize.options).sort();
          const index = choices.indexOf(quarter.value);
          const endIndex = choices.indexOf(endQuarter.value);
          if (index < 0 || endIndex < 0 || index >= endIndex) return;
          playing = true;
          button.textContent = 'Pause';
          button.setAttribute('aria-label', 'Pause quarter animation');
          button.setAttribute('aria-pressed', 'true');
          timer = window.setTimeout(advanceQuarter, interval);
        });

        // If the user picks a quarter themselves (dropdown or timeline click), stop playback.
        $(quarter).on('change.quarterPlayback', function() {
          if (!advancing) stopPlayback();
          checkBoundary();
        });
        $(endQuarter).on('change.quarterPlayback', checkBoundary);
        $(document).on('shiny:disconnected.quarterPlayback', stopPlayback);
        checkBoundary();
      });
    ")),

    # Close the help tooltips with Escape or a click elsewhere, without touching the control underneath.
    tags$script(HTML("
      document.addEventListener('keydown', function(event) {
        if (event.key === 'Escape') {
          document.querySelectorAll('bslib-tooltip:has(.control-info)').forEach(tip => tip.hide());
        }
      });
      document.addEventListener('click', function(event) {
        document.querySelectorAll('bslib-tooltip:has(.control-info)').forEach(tip => {
          if (!tip.contains(event.target)) tip.hide();
        });
      });
    ")),

    # All the animation work. Slides the timeline playhead and eases the map and bars between quarters, so playback looks smooth instead of flicking between static frames.
    tags$script(HTML("
      window.populationCharts = (function() {
        const previous = new Map();
        const duration = () => window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 0 : 350;
        const svgFor = id => document.querySelector('#' + id + ' .girafe_container_std > svg') ||
          document.querySelector('svg[data-output-id=' + id + ']');

        function movePlayhead(svg, animate) {
          if (!svg) return;
          const quarter = document.getElementById('quarter').value;
          const points = Array.from(svg.querySelectorAll('circle[data-id]'));
          const anchors = points.slice().sort((a, b) => a.dataset.id.localeCompare(b.dataset.id));
          if (anchors.length < 2) return;
          const first = anchors[0], last = anchors[anchors.length - 1];
          const start = Date.parse(first.dataset.id), end = Date.parse(last.dataset.id);
          if (end === start || !Number.isFinite(Date.parse(quarter))) return;
          // Work out the playhead x position by interpolating between the first and last plotted points, based on where the selected date sits in that range.
          const x = Number(first.getAttribute('cx')) +
            (Date.parse(quarter) - start) / (end - start) *
            (Number(last.getAttribute('cx')) - Number(first.getAttribute('cx')));

          svg.querySelectorAll('[data-id=quarter-playhead-reference]').forEach(line => {
            line.style.pointerEvents = 'none';
            line.style.transition = animate ? 'transform ' + duration() + 'ms ease-out' : 'none';
            line.style.transform = 'translateX(' + (x - Number(line.getAttribute('x1'))) + 'px)';
          });
          // Highlight the points matching the quarter currently in the dropdown.
          points.forEach(point => point.classList.toggle('select_data_' + svg.id, point.dataset.id === quarter));
        }

        function remember(id) {
          const svg = svgFor(id);
          if (!svg) return;
          const marks = new Map(), labels = new Map();
          svg.querySelectorAll('[data-id]').forEach(mark => {
            const box = mark.getBBox();
            marks.set(mark.dataset.id, {
              x: box.x, y: box.y, width: box.width, height: box.height,
              fill: getComputedStyle(mark).fill
            });
          });
          svg.querySelectorAll('text').forEach(label => {
            if (marks.has(label.textContent)) labels.set(label.textContent, label.getBBox().y);
          });
          // Snapshot the current positions and colours so the next render can animate from them.
          previous.set(id, {viewBox: svg.getAttribute('viewBox'), marks, labels});
        }

        function render(el) {
          const svg = svgFor(el.id);
          if (!svg) return;
          if (el.id === 'driver_timeline') {
            movePlayhead(svg, false);
            return;
          }
          const old = previous.get(el.id);
          previous.delete(el.id);
          const zero = svg.querySelector('[data-id=interstate-zero-reference]');
          if (zero) zero.style.pointerEvents = 'none';
          if (!old || !duration() || old.viewBox !== svg.getAttribute('viewBox')) return;
          const timing = {duration: duration(), easing: 'ease-out'};

          if (el.id === 'driver_map') {
            svg.querySelectorAll('path[data-id]').forEach(mark => {
              const before = old.marks.get(mark.dataset.id);
              if (before) mark.animate([{fill: before.fill}, {fill: getComputedStyle(mark).fill}], timing);
            });
          } else if (el.id === 'interstate_momentum') {
            const beforeZero = old.marks.get('interstate-zero-reference');
            const zeroX = zero ? zero.getBBox().x : null;
            svg.querySelectorAll('rect[data-id]').forEach(mark => {
              const before = old.marks.get(mark.dataset.id);
              if (!before) return;
              const after = mark.getBBox();
              const frame = box => ({x: box.x + 'px', y: box.y + 'px', width: box.width + 'px', height: box.height + 'px'});
              const frames = [frame(before)];
              // If a bar flips from loss to gain it should shrink to zero and grow back out the other side, rather than sliding straight through the axis.
              if (beforeZero && zeroX !== null &&
                  (before.x < beforeZero.x - 0.1) !== (after.x < zeroX - 0.1) && before.width + after.width > 0) {
                const fraction = before.width / (before.width + after.width);
                frames.push({...frame({
                  x: beforeZero.x + (zeroX - beforeZero.x) * fraction,
                  y: before.y + (after.y - before.y) * fraction,
                  width: 0, height: before.height + (after.height - before.height) * fraction
                }), offset: fraction});
              }
              frames.push(frame(after));
              mark.animate(frames, timing);
            });
            // Bring the state labels along with their bars, and keep the zero line in step.
            svg.querySelectorAll('text').forEach(label => {
              const y = old.labels.get(label.textContent);
              if (y !== undefined) label.animate([
                {transform: 'translateY(' + (y - label.getBBox().y) + 'px)'},
                {transform: 'translateY(0px)'}
              ], timing);
            });
            if (zero && beforeZero) zero.animate([
              {transform: 'translateX(' + (beforeZero.x - zeroX) + 'px)'},
              {transform: 'translateX(0px)'}
            ], timing);
          }
        }

        $(document).on('shiny:value.quarterMotion', function(event) {
          if (['driver_map', 'interstate_momentum'].includes(event.name)) remember(event.name);
        });
        $(document).on('shiny:error.quarterMotion', function(event) {
          previous.delete(event.name);
        });
        $(document).on('change.quarterMotion', '#quarter', function() {
          movePlayhead(svgFor('driver_timeline'), true);
        });
        return {render};
      })();
    ")),
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
          background-color: white;
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


        .dashboard-shell {
          display: grid;
          grid-template-columns: 240px minmax(0, 1fr);
          gap: 1.25rem;
          padding-top: 1rem;
          align-items: start;
        }

        .global-controls {
          background: #EEEFEA;
          border: 1px solid #E0E1DA;
          border-radius: 10px;
          padding: 1.1rem;
          position: sticky;
          top: 1rem;
          overflow: visible;
        }

        .global-controls:focus-within {
          /* Lift the sidebar while a control is focused, so an open dropdown sits over the chart cards instead of behind them. */
          z-index: 1000;
        }

.quarter-label-row {
  display: flex;
  align-items: center;
  margin-bottom: 0.35rem;
}

.quarter-label-row .control-label {
  margin-bottom: 0;
}

#quarter_play {
  font-size: 0.78rem;
  padding: 0.1rem 0.4rem;
}

/* Puts the Play button on the same row as the end quarter label. */
.playback-end-label-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.35rem;
}

.playback-end-label-row .control-label {
  margin-bottom: 0;
}

.playback-end-label-row #quarter_play {
  flex: 0 0 auto;
}

        .control-info {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 1.1rem;
          height: 1.1rem;
          padding: 0;
          margin-left: 0.25rem;
          border: 1px solid #888888;
          border-radius: 50%;
          background: transparent;
          color: #666666;
          font-size: 0.72rem;
          line-height: 1;
          text-transform: none;
          letter-spacing: normal;
          vertical-align: middle;
        }

        .control-info:focus-visible {
          outline: 2px solid #2C6EAA;
          outline-offset: 2px;
        }

        .control-label label {
          font: inherit;
          margin: 0;
        }

        .playback-end {
          margin-top: 0.65rem;
        }

        #quarter_boundary_hint {
          color: #666666;
          font-size: 0.75rem;
          line-height: 1.3;
          margin: 0.35rem 0 0;
        }

        .global-controls .radio + .radio {
          margin-top: 0.65rem;
        }

        .sidebar-hint {
          color: #777777;
          font-size: 0.78rem;
          border-top: 1px solid #DADCD4;
          padding-top: 0.9rem;
          margin: 0;
        }

        .dashboard-main {
          min-width: 0;
        }

        .form-group {
          margin-bottom: 0;
        }

        .selectize-control {
          margin-bottom: 0;
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


        .bslib-grid {
          gap: 0.8rem;
        }

        .analysis-layout {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 0.8rem;
          margin-top: 0.8rem;
          align-items: stretch;
        }

        .analysis-layout > .card {
          margin-bottom: 0;
        }

        .analysis-layout .card-body {
          display: flex;
          flex-direction: column;
          gap: 0.4rem;
        }

        .analysis-layout .girafe {
          flex: 1 0 auto;
          min-width: 0;
          position: relative;
        }

        /* Stop Shiny fading these out while recalculating, which makes playback look smoother. */
        #driver_map.recalculating, #interstate_momentum.recalculating,
        #driver_timeline.recalculating {
          opacity: 1;
        }

        .analysis-layout #population_fingerprint {
          /* Fixed height so all 21 age bands fit; the timeline card stretches instead. */
          flex: 0 0 450px;
        }

        .analysis-layout .girafe_container_std {
          position: absolute;
          inset: 0;
          height: 100%;
        }

        .analysis-layout .girafe_container_std > svg {
          max-width: 100% !important;
          max-height: 100% !important;
        }

        .fingerprint-years {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 0.8rem;
        }

        body, .form-control, .selectize-input, .selectize-dropdown,
        .shiny-input-container, .btn, .card, .girafe {
          font-family: \"Segoe UI\", Arial, sans-serif;
        }

        body > .container-fluid {
          width: 100%;
          max-width: 1540px;
          margin: 0 auto;
          padding: 0 1rem 1rem;
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

        @media (max-width: 1199.98px) {
          .dashboard-shell {
            grid-template-columns: 230px minmax(0, 1fr);
            gap: 1rem;
          }

          .analysis-layout {
            grid-template-columns: minmax(0, 1fr);
          }
        }

        @media (max-width: 991.98px) {
          .dashboard-shell {
            grid-template-columns: minmax(0, 1fr);
          }

          .global-controls {
            position: static;
          }

          .sidebar-fields {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 1rem;
          }

          .sidebar-heading {
            margin-bottom: 0.8rem;
          }
        }

        @media (max-width: 575.98px) {
          body > .container-fluid {
            padding: 0 0.65rem 0.65rem;
          }

          .sidebar-fields {
            grid-template-columns: minmax(0, 1fr);
            gap: 0;
          }

          .dashboard-title {
            font-size: 1.65rem;
          }

          .dashboard-subtitle {
            font-size: 0.92rem;
          }

        }

        "
      )
    )
  ),


  # GLOBAL CONTROLS AND MAIN CONTENT

  tags$div(
    class = "dashboard-shell",
    tags$aside(
      class = "global-controls",
      `aria-label` = "Global dashboard controls",
      tags$h2(class = "sidebar-heading", "Explore"),
      tags$div(
        class = "sidebar-fields",
        tags$div(
          class = "control-group",
          tags$label(
            class = "control-label",
            `for` = "state",
            "State or territory"
          ),
          selectInput(
            inputId = "state",
            label = NULL,
            choices = state_choices,
            selected = default_state,
            width = "100%"
          )
        ),
        tags$div(
          class = "control-group",

          # The quarter everything is driven by, and where playback starts from.
          tags$div(
            class = "quarter-label-row",
            tags$div(
              class = "control-label",
              tags$label(
                `for` = "quarter",
                "Quarter"
              ),
              control_info(
                "Quarter",
                "Choose the quarter you want to explore. This is also the starting point when playback begins."
              )
            )
          ),
          selectInput(
            inputId = "quarter",
            label = NULL,
            choices = quarter_choices,
            selected = as.character(max(quarter_dates)),
            width = "100%"
          ),

          # Where playback stops, plus the Play button itself.
          tags$div(
            class = "playback-end",
            tags$div(
              class = "playback-end-label-row",
              tags$div(
                class = "control-label",
                tags$label(
                  `for` = "quarter_end",
                  "Play until"
                ),
                control_info(
                  "Play until",
                  "Choose the final quarter for playback. Press Play to move from the selected Quarter to this date."
                )
              ),
              tags$button(
                id = "quarter_play",
                type = "button",
                class = "btn btn-outline-secondary btn-sm",
                `aria-label` = "Play quarter animation",
                `aria-pressed` = "false",
                `aria-describedby` = "quarter_boundary_hint",
                "Play"
              )
            ),
            selectInput(
              inputId = "quarter_end",
              label = NULL,
              choices = quarter_choices,
              selected = as.character(max(quarter_dates)),
              width = "100%"
            ),
            tags$p(
              id = "quarter_boundary_hint",
              role = "status",
              hidden = NA,
              "Choose a Quarter before the end quarter to play."
            )
          )
        ),
        tags$div(
          class = "control-group",
          tags$div(
            class = "control-label",
            tags$span(
              id = "value-mode-label",
              "Display values as"
            ),
            control_info(
              "Display values as",
              "People shows the published count. Per 1,000 residents adjusts for population size, making states easier to compare."
            )
          ),
          tags$div(
            role = "group",
            `aria-labelledby` = "value-mode-label",
            radioButtons(
              inputId = "value_mode",
              label = NULL,
              choices = c(
                "People" = "people",
                "Per 1,000 residents" = "rate"
              ),
              selected = "people",
              inline = FALSE
            )
          )
        )
      ),
      tags$p(
        class = "sidebar-hint",
        "Click charts to explore."
      )
    ),
    tags$main(
      class = "dashboard-main",

      # DASHBOARD HEADER

      tags$div(
        class = "dashboard-header",
        tags$h1(
          class = "dashboard-title",
          "Australia's Population Engine"
        ),
        tags$p(
          class = "dashboard-subtitle",
          "Explore what is driving population change across Australian states and territories."
        )
      ),


      # KPI CARDS

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


      # MAIN VISUALISATION AREA

      tags$div(
        class = "analysis-layout",


        # GROWTH DRIVER MAP

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
              height = "360px"
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


        # INTERSTATE MOMENTUM

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
              height = "360px"
            )
          )
        ),


        # DRIVER TIMELINE

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
              height = "450px"
            )
          )
        ),


        # POPULATION FINGERPRINT

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
            tags$div(
              class = "fingerprint-years",
              tags$div(
                class = "year-control",
                tags$div(
                  class = "control-label",
                  tags$label(`for` = "year_a", "Year A"),
                  control_info("Year A", "Choose two annual population profiles to compare changes in age and sex structure.")
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
                  tags$label(`for` = "year_b", "Year B"),
                  control_info("Year B", "Choose two annual population profiles to compare changes in age and sex structure.")
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
              height = "450px"
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
  )
)


# 6. SERVER - reactive logic behind the KPI cards and the four charts.

server <- function(input, output, session) {
  
  # SHARED REACTIVES AND KPIs
  selected_quarter <- reactive({
    req(input$quarter)
    as.Date(input$quarter)
  })


  # Total population for whichever state and quarter is selected.
  selected_population <- reactive({
    population %>%
      filter(
        state == input$state,
        sex == "Persons",
        date == selected_quarter()
      )
  })


  # Natural increase and the two migration components for the same selection.
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


  # Biggest driver by absolute value, so a large outflow counts just as much as a large inflow.
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


  # ------------------------------------------------------------ GROWTH DRIVER MAP -------------------------------------------------------------


  # A. PREPARE MAP DATA

  map_data <- reactive({
    quarter <- selected_quarter()


    driver_data <- components %>%
      filter(
        date == quarter,
        state != "Australia",
        component %in% names(driver_colours)
      )


    # Work out which driver dominates in each state this quarter.
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


    # Expand to every state/driver pair first, then widen, so a state missing one component still gets a row and the tooltip can show "Not available" for it.
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


    # Join it all onto the state geometry, ready to plot.
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


    # B. FORMAT MAP VALUES

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


    # Hover text for each state, written as HTML so ggiraph renders the formatting.
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


  # C. RENDER GROWTH DRIVER MAP

  output$driver_map <- renderGirafe({
    data <- map_data()

    validate(
      need(
        nrow(data) > 0,
        "Map data is not available for this quarter."
      )
    )


    # Fill each state by its dominant driver, greying out anything without data.
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


    widget <- card_girafe(
      map_plot, "driver_map", session,
      selection_input = "state"
    )

    style_girafe(
      widget,
      hover_css = "stroke:#222222;stroke-width:2px;cursor:pointer;",
      selection_type = "single",
      selected = input$state,
      selection_css = "stroke:#111111;stroke-width:3px;"
    )
  })


  # ---------------------------------------------------------------- DRIVER TIMELINE -----------------------------------------------------------------

  # A. PREPARE TIMELINE DATA

  # The whole history for the selected state, not just one quarter, since the timeline only needs redrawing when the state or the people/rate toggle changes.
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


  # B. RENDER DRIVER TIMELINE

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

      # Drawn once at the current quarter, then moved in the browser by the JS above, so changing quarter never re-renders the full history.
      geom_vline_interactive(
        xintercept = isolate(selected_quarter()),
        data_id = "quarter-playhead-reference",
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
        ~component_label,
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

    widget <- card_girafe(
      timeline_plot, "driver_timeline", session,
      selection_input = "quarter"
    )

    style_girafe(
      widget,
      hover_css = paste0(
        "fill:#222222;stroke:#222222;",
        "stroke-width:2px;cursor:pointer;"
      ),
      selection_type = "single",
      # Starting highlight only, later quarter changes are handled client-side.
      selected = isolate(as.character(selected_quarter())),
      selection_css = paste0(
        "fill:#222222;stroke:#222222;",
        "stroke-width:2px;"
      )
    )
  })



  # -------------------------------------------------------------- INTERSTATE MOMENTUM --------------------------------------------------------------

  # A. PREPARE INTERSTATE DATA

  # Always shows every state for the quarter. Selecting a state just outlines its bar rather than filtering, otherwise there is nothing left to compare against.
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


  # B. RENDER INTERSTATE MOMENTUM

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

    compact_plot <- isTRUE(
      session$clientData$output_interstate_momentum_width < 420
    )

    # Diverging bars around zero, so losses and gains are easy to compare at a glance.
    interstate_plot <- ggplot(
      data,
      aes(
        x = display_value,
        y = state_order
      )
    ) +
      geom_vline_interactive(
        xintercept = 0,
        data_id = "interstate-zero-reference",
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
        x = paste(
          strwrap(
            x_axis_label,
            width = if (compact_plot) 12 else 80
          ),
          collapse = "\n"
        ),
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
          colour = "#666666",
          angle = if (compact_plot) 45 else 0,
          hjust = if (compact_plot) 1 else 0.5
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

    widget <- card_girafe(
      interstate_plot, "interstate_momentum", session,
      selection_input = "state"
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


  # --------------------------------------------------------------POPULATION FINGERPRINT --------------------------------------------------------------


  # A. PREPARE POPULATION PYRAMID DATA

  # Driven by the state and the two year dropdowns. The quarter selector does not apply here because the age profiles are only published annually.
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


    # Full join so every age band survives even if one of the two years is missing it, which also lets the tooltip show both values and the change between them.
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


        # Flip the male counts negative so they plot to the left of zero, females to the right.
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


    # FORMAT TOOLTIP VALUES

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


  # B. RENDER POPULATION FINGERPRINT

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

      # Centre line splitting the male and female sides.
      geom_hline(
        yintercept = 0,
        colour = "#666666",
        linewidth = 0.5
      ) +

      # Year A sits behind in grey as the reference, drawn wider so it stays visible.
      geom_col(
        aes(
          y = plot_a
        ),
        fill = "#B8BAB5",
        width = 0.82
      ) +

      # Year B goes on top in colour, and is the layer that carries the tooltips.
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

    widget <- card_girafe(
      fingerprint_plot, "population_fingerprint", session
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


# 7. APP LAUNCH - put the UI and server together and start the app.

# Run it.
shinyApp(               
  ui = ui,
  server = server
)