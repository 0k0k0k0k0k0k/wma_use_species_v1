# Change audit: Made the Heat Map species metric label update dynamically with the selected species.
# Change audit: Kept Reset Filters inside the sidebar by letting the panel grow with Heat Map controls.
# Change audit: Tightened the left sidebar spacing and removed the sidebar scroll bar; updated the species note wording.

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(stringr)
library(tibble)

# Use GEOS for county/city boundary repair and simplification.
sf::sf_use_s2(FALSE)

sgcn_species <- c(
  "American Barn Owl",
  "American Black Duck",
  "American Herring Gull",
  "American Kestrel",
  "American Oystercatcher",
  "American Woodcock",
  "Winter Wren",
  "Bachman's Sparrow",
  "Bank Swallow",
  "Belted Kingfisher",
  "Bicknell's Thrush",
  "Black Skimmer",
  "Black-and-white Warbler",
  "Black-bellied Plover",
  "Boat-tailed Grackle",
  "Bobolink",
  "Brant",
  "Canada Warbler",
  "Cerulean Warbler",
  "Chimney Swift",
  "Chuck-will's-widow",
  "Clapper Rail",
  "Common Grackle",
  "Common Nighthawk",
  "Common Tern",
  "Dickcissel",
  "Dunlin",
  "Eastern Black Rail",
  "Eastern Kingbird",
  "Eastern Meadowlark",
  "Eastern Screech-Owl",
  "Eastern Towhee",
  "Eastern Whip-poor-will",
  "Eastern Wood-Pewee",
  "Field Sparrow",
  "Forster's Tern",
  "Glossy Ibis",
  "Golden Eagle",
  "Golden-winged Warbler",
  "Grasshopper Sparrow",
  "Gray Catbird",
  "Green Heron",
  "Gull-billed Tern",
  "Henslow's Sparrow",
  "Hermit Thrush",
  "Horned Lark",
  "Hudsonian Whimbrel",
  "Kentucky Warbler",
  "King Rail",
  "Laughing Gull",
  "Least Flycatcher",
  "Least Tern",
  "Little Blue Heron",
  "Loggerhead Shrike",
  "Marbled Godwit",
  "Marsh Wren",
  "Nelson's Sparrow",
  "Northern Bobwhite",
  "Northern Flicker",
  "Northern Gannet",
  "Northern Harrier",
  "Northern Mockingbird",
  "Northern Rough-winged Swallow",
  "Peregrine Falcon",
  "Piping Plover",
  "Prairie Warbler",
  "Purple Sandpiper",
  "Red-cockaded Woodpecker",
  "Red-throated Loon",
  "Red-winged Blackbird",
  "Red Knot",
  "Ruffed Grouse",
  "Rusty Blackbird",
  "Saltmarsh Sparrow",
  "Sanderling",
  "Savannah Sparrow",
  "Savannah Sparrow Ipswich ssp",
  "Short-billed Dowitcher",
  "Snowy Egret",
  "Tricolored Heron",
  "Veery",
  "Vesper Sparrow",
  "Virginia Rail",
  "Wayne's Warbler (Setophaga virens waynei)",
  "Western Willet",
  "Wilson's Plover",
  "Wood Thrush",
  "Yellow Warbler",
  "Yellow-breasted Chat"
)

species_list_file <- "data_raw/species_list2.csv"

if (!file.exists(species_list_file)) {
  stop("Species list file not found at: ", species_list_file)
}

species_list <- read.csv(
  species_list_file,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

required_species_cols <- c("common_name", "scientific_name")
missing_species_cols <- setdiff(required_species_cols, names(species_list))

if (length(missing_species_cols) > 0) {
  stop(
    "Missing required columns in species_list2.csv: ",
    paste(missing_species_cols, collapse = ", ")
  )
}

approved_species <- sort(unique(na.omit(species_list$common_name)))
approved_species <- approved_species[approved_species != ""]

# =========================
# PATHS
# =========================
app_data_file <- "data_processed/app_ready_data.rds"
county_boundaries_dir <- "data_raw/shapefiles/county_boundaries"

message("Working directory: ", getwd())
message("Using prebuilt app data: ", normalizePath(app_data_file, mustWork = FALSE))

# =========================
# HELPERS
# =========================
first_non_missing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

first_existing_col <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]
  if (length(matches) == 0) return(NA_character_)
  matches[1]
}

clean_county_city_label <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- str_replace(x, regex("\\s+city$", ignore_case = TRUE), "")
  x
}

row_month_values <- function(df) {
  month_col <- first_existing_col(df, c("month", "MONTH", "Month"))
  
  if (!is.na(month_col)) {
    month_vals <- suppressWarnings(as.integer(df[[month_col]]))
    return(month_vals)
  }
  
  if ("OBSERVATION DATE" %in% names(df)) {
    return(suppressWarnings(as.integer(format(as.Date(df[["OBSERVATION DATE"]]), "%m"))))
  }
  
  rep(NA_integer_, nrow(df))
}

apply_county_month_filters <- function(df, county_values, month_values, county_col) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  if (!is.na(county_col) &&
      !is.null(county_values) &&
      length(county_values) > 0 &&
      !("__all__" %in% county_values)) {
    df <- df[df[[county_col]] %in% county_values, , drop = FALSE]
  }
  
  if (!is.null(month_values) &&
      length(month_values) > 0 &&
      !("__all__" %in% month_values)) {
    selected_months <- suppressWarnings(as.integer(month_values))
    selected_months <- selected_months[!is.na(selected_months)]
    
    if (length(selected_months) > 0) {
      df_months <- row_month_values(df)
      df <- df[df_months %in% selected_months, , drop = FALSE]
    }
  }
  
  df
}

make_species_popup_html <- function(df) {
  paste0(
    "<b>WMA:</b> ", df$site_name, "<br>",
    "<b>Date:</b> ", df$`OBSERVATION DATE`, "<br>",
    "<b>Species:</b> ", df$`COMMON NAME`, "<br>",
    "<b>Count:</b> ", ifelse(is.na(df$display_count), "", df$display_count), "<br>",
    "<b>Checklist ID:</b> ", df$`SAMPLING EVENT IDENTIFIER`
  )
}

draw_annual_plot <- function(df) {
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No annual data available")
    return(invisible(NULL))
  }
  
  par(mar = c(3.5, 4, 1, 1))
  
  plot(
    x = df$year,
    y = df$checklists,
    type = "b",
    pch = 16,
    lwd = 2,
    col = "#2A5235",
    xlab = "Year",
    ylab = "Total Checklists",
    main = "",
    xaxt = "n"
  )
  
  axis(1, at = df$year, labels = df$year)
  
  text(
    x = df$year,
    y = df$checklists,
    labels = df$checklists,
    pos = 3,
    cex = 0.8,
    col = "#2A5235"
  )
}

common_heat_gradient <- c(
  "0.10" = "#51748A",
  "0.30" = "#A19857",
  "0.50" = "#DFC9A2",
  "0.70" = "#F36C21",
  "0.88" = "#79441C",
  "1.00" = "#8C2332"
)

prepare_heatmap_points <- function(df, intensity_col = "heat_intensity", coord_digits = 4) {
  if (nrow(df) == 0 ||
      !("LATITUDE" %in% names(df)) ||
      !("LONGITUDE" %in% names(df)) ||
      !(intensity_col %in% names(df))) {
    return(
      tibble(
        LATITUDE = numeric(),
        LONGITUDE = numeric(),
        heat_intensity = numeric()
      )
    )
  }
  
  df %>%
    filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
    mutate(
      heat_latitude = round(as.numeric(LATITUDE), coord_digits),
      heat_longitude = round(as.numeric(LONGITUDE), coord_digits),
      heat_value = suppressWarnings(as.numeric(.data[[intensity_col]])),
      heat_value = ifelse(is.na(heat_value) | heat_value <= 0, 1, heat_value)
    ) %>%
    group_by(heat_latitude, heat_longitude) %>%
    summarise(
      LATITUDE = first(heat_latitude),
      LONGITUDE = first(heat_longitude),
      heat_intensity = sum(heat_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# =========================
# LOAD PREBUILT APP DATA
# =========================
if (!file.exists(app_data_file)) {
  stop(
    "Prebuilt app data file not found: ",
    normalizePath(app_data_file, mustWork = FALSE),
    "\nRun scripts/build_app_data.R first."
  )
}

app_data <- readRDS(app_data_file)

site_lookup <- app_data$site_lookup
cache_list <- app_data$cache_list
polygon_list <- app_data$polygon_list
all_sites_cache <- app_data$all_sites_cache
all_polygons <- app_data$all_polygons
global_min_year <- app_data$global_min_year
global_max_year <- app_data$global_max_year
global_year_choices <- app_data$global_year_choices

county_boundary_layer <- NULL
county_boundary_name_col <- NA_character_
county_boundary_display_col <- "county_city_display"
county_col <- first_existing_col(
  all_sites_cache,
  c("county_city", "county", "COUNTY_CITY", "COUNTY", "County")
)

if (is.na(county_col)) {
  stop(
    "Rebuilt county/city field not found in app_ready_data.rds. ",
    "Run rebuild_wma_app_ready_data_county_city_display_names.R, then restart the app."
  )
}

all_sites_cache <- all_sites_cache %>%
  mutate(
    county_city_app = clean_county_city_label(.data[[county_col]])
  )

county_col <- "county_city_app"
cache_list <- split(all_sites_cache, all_sites_cache$site_key)
cache_list <- lapply(cache_list, tibble::as_tibble)

# Load the boundary layer only for drawing selected County / City outlines.
# Filtering now uses the rebuilt county_city field already saved in app_ready_data.rds.
if (!dir.exists(county_boundaries_dir)) {
  stop(
    "County boundary folder not found: ",
    normalizePath(county_boundaries_dir, mustWork = FALSE)
  )
}

county_shp_files <- list.files(
  county_boundaries_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(county_shp_files) == 0) {
  stop(
    "No .shp file found in county boundary folder: ",
    normalizePath(county_boundaries_dir, mustWork = FALSE)
  )
}

preferred_county_shp <- county_shp_files[
  tolower(basename(county_shp_files)) == "va_counties.shp"
]

county_shp_file <- if (length(preferred_county_shp) > 0) {
  preferred_county_shp[1]
} else {
  county_shp_files[1]
}

message("County / City boundary file used by app: ", county_shp_file)
county_boundaries <- sf::st_read(county_shp_file, quiet = TRUE)

county_name_col <- first_existing_col(
  county_boundaries,
  c(
    "NAMELSAD", "NAMELSAD20", "COUNTY_NAM", "COUNTY_NA",
    "COUNTY_NAME", "NAME", "NAME20",
    "COUNTY", "County", "county"
  )
)

if (is.na(county_name_col)) {
  stop(
    "Could not identify county/city name field in county boundary shapefile. Available fields: ",
    paste(names(county_boundaries), collapse = ", ")
  )
}

if (is.na(sf::st_crs(county_boundaries))) {
  sf::st_crs(county_boundaries) <- 4326
}

county_boundary_layer <- sf::st_transform(county_boundaries, 4326) %>%
  mutate(
    county_city_raw = as.character(.data[[county_name_col]]),
    county_city_display = clean_county_city_label(.data[[county_name_col]])
  )

# Repair/simplify county-city boundaries in a projected CRS.
# This avoids s2 duplicate-vertex failures from the raw shapefile while keeping
# the display layer light enough for Leaflet.
county_boundary_layer <- county_boundary_layer %>%
  sf::st_transform(3857) %>%
  sf::st_make_valid() %>%
  sf::st_simplify(dTolerance = 100, preserveTopology = TRUE) %>%
  sf::st_transform(4326)

county_boundary_name_col <- county_name_col

# Dropdown choices must come from the full county/independent-city boundary layer,
# not just from WMA records. This keeps independent cities visible even when
# a selected app dataset has few or no rows for that unit.
county_values <- county_boundary_layer[[county_boundary_display_col]]
county_values <- county_values[!is.na(county_values) & str_trim(as.character(county_values)) != ""]
county_values <- sort(unique(as.character(county_values)))
county_choices <- c(
  "All counties / cities" = "__all__",
  setNames(county_values, county_values)
)

independent_city_raw <- county_boundary_layer$county_city_raw[
  str_detect(county_boundary_layer$county_city_raw, regex("\\scity$", ignore_case = TRUE))
]
independent_city_display <- sort(unique(clean_county_city_label(independent_city_raw)))

message("County / City boundary units loaded: ", length(unique(county_boundary_layer$county_city_raw)))
message("Independent city boundary units loaded: ", length(independent_city_display))
message("Hampton present in County / City dropdown: ", "Hampton" %in% county_values)

if (length(independent_city_display) < 30) {
  stop(
    "Independent cities were not loaded correctly from the county/city boundary layer. ",
    "Expected about 38 Virginia independent cities, found ", length(independent_city_display), ". ",
    "Boundary field used: ", county_name_col, "."
  )
}

if (!("Hampton" %in% county_values)) {
  stop(
    "Hampton is missing from the County / City dropdown choices. ",
    "The app is not reading the full Virginia county/independent-city boundary layer correctly. ",
    "Boundary field used: ", county_name_col, "."
  )
}

month_choices <- c("All months" = "__all__", setNames(as.character(1:12), month.name))

if (nrow(site_lookup) == 0) {
  stop("No sites found in prebuilt app data.")
}

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f7f4ec;
        color: #222222;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .container-fluid {
        max-width: 1420px;
      }
      .wma-hero {
        background: linear-gradient(135deg, #2A5235 0%, #51748A 100%);
        color: white;
        padding: 12px 18px;
        border-radius: 10px;
        margin: 8px 0 12px 0;
        box-shadow: 0 8px 22px rgba(42, 82, 53, 0.22);
        border: 1px solid rgba(223, 201, 162, 0.55);
      }
      .wma-hero h2 {
        margin: 0;
        font-weight: 700;
        letter-spacing: 0.2px;
        font-size: 24px;
      }
      .wma-hero .subtitle {
        margin-top: 2px;
        color: #DFC9A2;
        font-size: 13px;
      }
      .well {
        background: #ffffff;
        border: 1px solid rgba(42, 82, 53, 0.22);
        border-radius: 12px;
        box-shadow: 0 5px 18px rgba(42, 82, 53, 0.12);
      }
      .sidebar-panel-compact {
        border-top: 5px solid #2A5235 !important;
      }
      .sidebar-panel-compact .form-group {
        margin-bottom: 4px;
      }
      .sidebar-panel-compact h4 {
        color: #2A5235;
        font-size: 14px;
        font-weight: 700;
        margin-top: 7px;
        margin-bottom: 2px;
        letter-spacing: 0.2px;
      }
      .sidebar-panel-compact .radio,
      .sidebar-panel-compact .checkbox {
        margin-top: 0;
        margin-bottom: 2px;
      }
      .sidebar-panel-compact .shiny-input-radiogroup {
        margin-top: 0;
        margin-bottom: 2px;
      }
      .selectize-input, .form-control {
        border-color: rgba(106, 97, 87, 0.45);
        border-radius: 6px;
      }
      .selectize-input.focus, .form-control:focus {
        border-color: #51748A;
        box-shadow: 0 0 0 2px rgba(81, 116, 138, 0.18);
      }
      .selectize-control.multi .selectize-input > div {
        background: #EDE3CC;
        color: #2A5235;
        border: 1px solid rgba(42, 82, 53, 0.22);
        border-radius: 5px;
      }
      .btn-default, #reset_filters {
        background: #2A5235 !important;
        border-color: #2A5235 !important;
        color: white !important;
        border-radius: 8px;
        font-weight: 700;
        box-shadow: 0 3px 10px rgba(42, 82, 53, 0.22);
      }
      .btn-default:hover, #reset_filters:hover {
        background: #1f3e28 !important;
        border-color: #1f3e28 !important;
      }
      .nav-tabs {
        border-bottom: 1px solid rgba(42, 82, 53, 0.25);
      }
      .nav-tabs > li > a {
        color: #2A5235;
        font-weight: 700;
        border-radius: 8px 8px 0 0;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: white;
        background: #2A5235;
        border-color: #2A5235;
      }
      .wma-card {
        border: 1px solid rgba(42, 82, 53, 0.25);
        border-radius: 12px;
        padding: 6px;
        background: white;
        box-shadow: 0 5px 18px rgba(42, 82, 53, 0.12);
        margin-top: 10px;
      }
      .leaflet-container {
        border-radius: 9px;
      }
      .shiny-text-output {
        margin-top: 5px;
      }
      .shiny-text-output pre {
        margin: 0;
        min-height: 42px;
        padding: 6px 8px;
        font-size: 12px;
        line-height: 1.25;
        color: #2A5235;
        word-break: break-word;
        white-space: pre-wrap;
        background-color: #fbfaf6;
        border: 1px solid rgba(42, 82, 53, 0.20);
        border-radius: 8px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-weight: 500;
      }
      .wma-note {
        font-size: 12px;
        color: #6A6157;
        margin-top: -4px;
        margin-bottom: 10px;
        line-height: 1.35;
      }
      "))
  ),
  
  div(
    class = "wma-hero",
    tags$h2("Virginia WMA eBird Species Analysis"),
    div(class = "subtitle", "Explore species records by WMA, county/city, season, and year.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel-compact",
      style = "height: auto; min-height: 600px; overflow-y: visible; padding-top: 6px; padding-bottom: 12px;",
      
      tags$h4("Search By"),
      radioButtons(
        "search_mode",
        NULL,
        choices = c("WMA" = "wma", "County / City" = "county"),
        selected = "wma",
        inline = TRUE
      ),
      
      conditionalPanel(
        condition = "input.search_mode == 'wma'",
        tags$h4("Select WMA"),
        selectizeInput(
          "site",
          NULL,
          choices = c(
            "All WMAs" = "__all__",
            setNames(site_lookup$site_key, site_lookup$site_name)
          ),
          selected = character(0),
          multiple = TRUE,
          options = list(
            placeholder = "Select one or more WMAs"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.search_mode == 'county'",
        tags$h4("County / City"),
        selectizeInput(
          "county",
          NULL,
          choices = county_choices,
          selected = character(0),
          multiple = TRUE,
          options = list(
            placeholder = "Select one or more counties or cities"
          )
        ),
        div(
          class = "wma-note",
          "Only counties and cities with WMA checklist data will have species results."
        )
      ),
      
      tags$h4("Month"),
      selectizeInput(
        "month",
        NULL,
        choices = month_choices,
        selected = character(0),
        multiple = TRUE,
        options = list(placeholder = "Select one or more")
      ),
      
      tags$h4("Year Range"),
      div(
        style = "display: flex; gap: 2%;",
        div(
          style = "width: 49%;",
          selectInput(
            "start_year",
            label = NULL,
            choices = global_year_choices,
            selected = as.character(global_min_year),
            selectize = FALSE,
            width = "100%"
          )
        ),
        div(
          style = "width: 49%;",
          selectInput(
            "end_year",
            label = NULL,
            choices = global_year_choices,
            selected = as.character(as.integer(format(Sys.Date(), "%Y"))),
            selectize = FALSE,
            width = "100%"
          )
        )
      ),
      
      tags$h4("Select Species"),
      div(
        class = "wma-note",
        "The species list is based on the selected location."
      ),
      
      checkboxInput(
        "sgcn_only",
        "Show Virginia SGCN birds only",
        value = FALSE
      ),
      
      uiOutput("species_ui"),
      
      verbatimTextOutput("species_summary_text", placeholder = TRUE),
      
      tags$h4("Map Style", style = "margin-bottom: 10px;"),
      div(
        style = "margin-top: 0; margin-bottom: 12px;",
        radioButtons(
          "map_type",
          NULL,
          choices = c("Points" = "points", "Heat Map" = "heat"),
          selected = "points"
        )
      ),
      
      conditionalPanel(
        condition = "input.map_type == 'heat'",
        tags$h4("Map Based On", style = "margin-top: 12px; margin-bottom: 10px;"),
        div(
          style = "margin-top: 0; margin-bottom: 12px;",
          uiOutput("heat_metric_ui")
        )
      )
      ,
      actionButton(
        "reset_filters",
        "Reset Filters",
        style = "margin-top: 8px; width: 100%;"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Map",
          div(
            class = "wma-card",
            leafletOutput("map", height = "500px")
          )
        ),
        tabPanel(
          "Checklists Over Time",
          div(
            class = "wma-card",
            plotOutput("annual_plot", height = 450, width = "100%")
          )
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  active_search_mode <- reactive({
    if (is.null(input$search_mode) || input$search_mode == "") {
      return("wma")
    }
    
    input$search_mode
  })
  
  output$heat_metric_ui <- renderUI({
    species_label <- input$selected_species
    
    species_reports_label <- if (
      is.null(species_label) ||
      species_label == "" ||
      species_label == "__none__"
    ) {
      "Selected species reports"
    } else {
      paste(species_label, "reports")
    }
    
    heat_choices <- setNames(
      c("species_density", "all_checklists"),
      c(species_reports_label, "All checklist activity")
    )
    
    current_heat_metric <- input$heat_metric
    if (is.null(current_heat_metric) ||
        !(current_heat_metric %in% c("species_density", "all_checklists"))) {
      current_heat_metric <- "species_density"
    }
    
    radioButtons(
      "heat_metric",
      NULL,
      choices = heat_choices,
      selected = current_heat_metric
    )
  })
  
  active_county_values <- reactive({
    if (active_search_mode() != "county") {
      return(character(0))
    }
    
    selected_counties <- input$county
    
    if (is.null(selected_counties) || length(selected_counties) == 0) {
      return(character(0))
    }
    
    selected_counties[selected_counties != ""]
  })
  
  observeEvent(input$reset_filters, {
    last_selected_species("")
    
    updateRadioButtons(session, "search_mode", selected = "wma")
    updateSelectizeInput(session, "site", selected = character(0))
    updateSelectizeInput(session, "county", selected = character(0))
    updateSelectizeInput(session, "month", selected = character(0))
    updateSelectInput(session, "start_year", selected = as.character(global_min_year))
    updateSelectInput(session, "end_year", selected = as.character(as.integer(format(Sys.Date(), "%Y"))))
    updateCheckboxInput(session, "sgcn_only", value = FALSE)
    updateSelectizeInput(session, "selected_species", selected = "")
    updateRadioButtons(session, "map_type", selected = "points")
    updateRadioButtons(session, "heat_metric", selected = "species_density")
  })
  
  selected_site_keys <- reactive({
    if (active_search_mode() == "county") {
      selected_counties <- active_county_values()
      
      if (is.null(selected_counties) || length(selected_counties) == 0) {
        return(character(0))
      }
      
      if ("__all__" %in% selected_counties) {
        return(site_lookup$site_key)
      }
      
      if (is.na(county_col)) {
        return(character(0))
      }
      
      return(
        all_sites_cache %>%
          filter(.data[[county_col]] %in% selected_counties) %>%
          distinct(site_key) %>%
          pull(site_key)
      )
    }
    
    selected_sites <- input$site
    
    if (is.null(selected_sites) || length(selected_sites) == 0) {
      return(character(0))
    }
    
    selected_sites <- selected_sites[selected_sites != ""]
    
    if (length(selected_sites) == 0) {
      return(character(0))
    }
    
    if ("__all__" %in% selected_sites) {
      return(site_lookup$site_key)
    }
    
    selected_sites[selected_sites %in% site_lookup$site_key]
  })
  
  selected_site_label <- reactive({
    keys <- selected_site_keys()
    
    if (length(keys) == 0) {
      return("")
    }
    
    if (active_search_mode() == "county") {
      selected_counties <- active_county_values()
      
      if ("__all__" %in% selected_counties) {
        return("All counties / cities")
      }
      
      county_names <- selected_counties[selected_counties != "__all__"]
      
      if (length(county_names) == 1) {
        return(county_names)
      }
      
      return(paste(length(county_names), "selected counties / cities"))
    }
    
    if (!is.null(input$site) && "__all__" %in% input$site) {
      return("All WMAs")
    }
    
    selected_names <- site_lookup$site_name[match(keys, site_lookup$site_key)]
    selected_names <- selected_names[!is.na(selected_names)]
    
    if (length(selected_names) == 1) {
      return(selected_names)
    }
    
    paste(length(selected_names), "selected WMAs")
  })
  
  last_selected_species <- reactiveVal("")
  
  observeEvent(input$selected_species, {
    if (!is.null(input$selected_species) &&
        input$selected_species != "" &&
        input$selected_species != "__none__") {
      last_selected_species(input$selected_species)
    }
  }, ignoreInit = TRUE)
  
  current_cache <- reactive({
    keys <- selected_site_keys()
    req(length(keys) > 0)
    
    if (active_search_mode() == "county") {
      selected_counties <- active_county_values()
      
      if ("__all__" %in% selected_counties || is.na(county_col)) {
        return(all_sites_cache)
      }
      
      return(
        all_sites_cache %>%
          filter(.data[[county_col]] %in% selected_counties)
      )
    }
    
    all_sites_cache %>%
      filter(site_key %in% keys)
  })
  
  current_polygon <- reactive({
    keys <- selected_site_keys()
    req(length(keys) > 0)
    
    if (active_search_mode() == "county") {
      selected_counties <- active_county_values()
      
      if ("__all__" %in% selected_counties) {
        return(all_polygons)
      }
    } else if (!is.null(input$site) && "__all__" %in% input$site) {
      return(all_polygons)
    }
    
    if ("site_key" %in% names(all_polygons)) {
      return(all_polygons %>% filter(site_key %in% keys))
    }
    
    selected_polygons <- polygon_list[keys]
    selected_polygons <- selected_polygons[!vapply(selected_polygons, is.null, logical(1))]
    
    if (length(selected_polygons) == 0) {
      return(all_polygons[0, ])
    }
    
    do.call(rbind, selected_polygons)
  })
  
  current_site_name <- reactive({
    req(length(selected_site_keys()) > 0)
    selected_site_label()
  })
  
  heat_settings <- reactive({
    keys <- selected_site_keys()
    req(length(keys) > 0)
    
    heat_metric <- input$heat_metric
    if (is.null(heat_metric) || length(heat_metric) == 0 || !nzchar(heat_metric)) {
      heat_metric <- "species_density"
    }
    
    county_values <- active_county_values()
    county_all_selected <- !is.null(county_values) && length(county_values) > 0 && "__all__" %in% county_values
    site_all_selected <- !is.null(input$site) && length(input$site) > 0 && "__all__" %in% input$site
    
    is_all <- length(keys) > 1 ||
      (active_search_mode() == "wma" && site_all_selected) ||
      (active_search_mode() == "county" && county_all_selected)
    
    if (identical(heat_metric, "all_checklists")) {
      if (is_all) {
        return(list(radius = 18, blur = 14, max = 0.12, min_opacity = 0.35))
      } else {
        return(list(radius = 20, blur = 16, max = 0.22, min_opacity = 0.35))
      }
    }
    
    if (is_all) {
      return(list(radius = 18, blur = 14, max = 0.10, min_opacity = 0.35))
    } else {
      return(list(radius = 20, blur = 16, max = 0.20, min_opacity = 0.35))
    }
  })
  
  observeEvent(list(input$search_mode, input$site, input$county), {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (length(selected_site_keys()) == 0) {
      updateSelectInput(
        session,
        "start_year",
        choices = global_year_choices,
        selected = as.character(global_min_year)
      )
      
      updateSelectInput(
        session,
        "end_year",
        choices = global_year_choices,
        selected = as.character(as.integer(format(Sys.Date(), "%Y")))
      )
      
      return()
    }
    
    site_df <- current_cache()
    
    if (nrow(site_df) == 0) {
      site_year_choices <- as.character(seq(2002, current_year, by = 1))
      
      updateSelectInput(
        session,
        "start_year",
        choices = site_year_choices,
        selected = "2002"
      )
      
      updateSelectInput(
        session,
        "end_year",
        choices = site_year_choices,
        selected = as.character(current_year)
      )
      
      return()
    }
    
    site_years_num <- site_df %>%
      filter(!is.na(year)) %>%
      pull(year)
    
    if (length(site_years_num) > 0) {
      min_site_year <- min(site_years_num, na.rm = TRUE)
      max_site_year_actual <- max(site_years_num, na.rm = TRUE)
      max_site_year_for_ui <- max(max_site_year_actual, current_year)
      graph_year_choices <- as.character(seq(min_site_year, max_site_year_for_ui, by = 1))
      
      updateSelectInput(
        session,
        "start_year",
        choices = graph_year_choices,
        selected = as.character(min_site_year)
      )
      
      updateSelectInput(
        session,
        "end_year",
        choices = graph_year_choices,
        selected = as.character(max_site_year_for_ui)
      )
    }
  }, ignoreInit = FALSE)
  
  species_state <- reactive({
    if (length(selected_site_keys()) == 0) {
      return(list(
        mode = "select",
        choices = character(0),
        selected = ""
      ))
    }
    
    # Species choices are curated for the selected location only.
    # Year and month filters affect the map/summary after a species is selected,
    # but they should not empty the species dropdown for narrow date windows.
    site_df <- current_cache()
    
    if (nrow(site_df) == 0) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
    }
    
    available_species <- site_df %>%
      filter(!is.na(`COMMON NAME`), `COMMON NAME` != "") %>%
      distinct(`COMMON NAME`) %>%
      pull(`COMMON NAME`)
    
    species_choices <- approved_species[approved_species %in% available_species]
    
    if (isTRUE(input$sgcn_only)) {
      species_choices <- species_choices[species_choices %in% sgcn_species]
    }
    
    if (length(species_choices) == 0) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
    }
    
    target_species <- if (!is.null(input$selected_species) &&
                          input$selected_species != "__none__") {
      input$selected_species
    } else {
      ""
    }
    
    selected_species <- if (!is.null(target_species) &&
                            target_species != "" &&
                            target_species %in% species_choices) {
      target_species
    } else {
      ""
    }
    
    list(
      mode = "choices",
      choices = species_choices,
      selected = selected_species
    )
  })
  
  output$species_ui <- renderUI({
    st <- species_state()
    
    if (st$mode == "none") {
      return(
        selectInput(
          "selected_species",
          NULL,
          choices = c("No data available" = "__none__"),
          selected = "__none__"
        )
      )
    }
    
    if (st$mode == "select") {
      return(
        selectizeInput(
          "selected_species",
          NULL,
          choices = c("Select Species" = ""),
          selected = "",
          multiple = FALSE,
          options = list(
            placeholder = "Type to search species",
            allowEmptyOption = TRUE
          )
        )
      )
    }
    
    selectizeInput(
      "selected_species",
      NULL,
      choices = c("Select Species" = "", st$choices),
      selected = st$selected,
      multiple = FALSE,
      options = list(
        placeholder = "Type to search species",
        allowEmptyOption = TRUE
      )
    )
  })
  
  filtered_data <- reactive({
    req(length(selected_site_keys()) > 0)
    req(input$start_year, input$end_year)
    
    df <- current_cache()
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    start_year <- as.integer(input$start_year)
    end_year <- as.integer(input$end_year)
    
    if (is.na(start_year) || is.na(end_year)) {
      return(df[0, , drop = FALSE])
    }
    
    if (start_year > end_year) {
      return(df[0, , drop = FALSE])
    }
    
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31"))
    
    df <- df %>%
      filter(
        !is.na(`OBSERVATION DATE`),
        `OBSERVATION DATE` >= start_date,
        `OBSERVATION DATE` <= end_date
      )
    
    apply_county_month_filters(
      df,
      active_county_values(),
      input$month,
      county_col
    )
  })
  
  year_filtered_data <- reactive({
    req(length(selected_site_keys()) > 0)
    req(input$start_year, input$end_year)
    
    df <- current_cache()
    
    if (nrow(df) == 0) {
      return(df[0, , drop = FALSE])
    }
    
    start_year <- as.integer(input$start_year)
    end_year <- as.integer(input$end_year)
    
    if (is.na(start_year) || is.na(end_year) || start_year > end_year) {
      return(df[0, , drop = FALSE])
    }
    
    df <- df %>%
      filter(year >= start_year, year <= end_year)
    
    apply_county_month_filters(
      df,
      active_county_values(),
      input$month,
      county_col
    )
  })
  
  species_filtered_data <- reactive({
    req(length(selected_site_keys()) > 0)
    req(input$start_year)
    req(input$end_year)
    req(input$selected_species)
    
    df <- year_filtered_data()
    
    if (input$selected_species == "" ||
        input$selected_species == "__none__" ||
        nrow(df) == 0) {
      return(df[0, , drop = FALSE])
    }
    
    df %>%
      filter(`COMMON NAME` == input$selected_species)
  })
  
  species_checklist_points <- reactive({
    df <- species_filtered_data()
    
    if (nrow(df) == 0) {
      return(
        tibble(
          checklist_group_id = character(),
          `SAMPLING EVENT IDENTIFIER` = character(),
          `OBSERVATION DATE` = as.Date(character()),
          LATITUDE = numeric(),
          LONGITUDE = numeric(),
          `COMMON NAME` = character(),
          display_count = character(),
          individuals_num = numeric(),
          site_name = character()
        )
      )
    }
    
    df %>%
      mutate(
        sampling_id_chr = as.character(`SAMPLING EVENT IDENTIFIER`),
        obs_count_num = suppressWarnings(as.numeric(`OBSERVATION COUNT`))
      ) %>%
      arrange(checklist_group_id, `OBSERVATION DATE`, sampling_id_chr) %>%
      group_by(checklist_group_id) %>%
      summarise(
        `SAMPLING EVENT IDENTIFIER` = first(`SAMPLING EVENT IDENTIFIER`),
        `OBSERVATION DATE` = first(`OBSERVATION DATE`),
        LATITUDE = first(LATITUDE),
        LONGITUDE = first(LONGITUDE),
        `COMMON NAME` = first(`COMMON NAME`),
        display_count = first_non_missing(`OBSERVATION COUNT`),
        individuals_num = {
          vals <- obs_count_num[!is.na(obs_count_num)]
          if (length(vals) == 0) 0 else max(vals)
        },
        site_name = first(site_name),
        .groups = "drop"
      )
  })
  
  all_checklist_points <- reactive({
    df <- year_filtered_data()
    
    if (nrow(df) == 0) {
      return(
        tibble(
          checklist_group_id = character(),
          `SAMPLING EVENT IDENTIFIER` = character(),
          `OBSERVATION DATE` = as.Date(character()),
          LATITUDE = numeric(),
          LONGITUDE = numeric(),
          site_name = character(),
          heat_intensity = numeric()
        )
      )
    }
    
    df %>%
      mutate(
        sampling_id_chr = as.character(`SAMPLING EVENT IDENTIFIER`)
      ) %>%
      arrange(checklist_group_id, `OBSERVATION DATE`, sampling_id_chr) %>%
      group_by(checklist_group_id) %>%
      summarise(
        `SAMPLING EVENT IDENTIFIER` = first(`SAMPLING EVENT IDENTIFIER`),
        `OBSERVATION DATE` = first(`OBSERVATION DATE`),
        LATITUDE = first(LATITUDE),
        LONGITUDE = first(LONGITUDE),
        site_name = first(site_name),
        heat_intensity = 1,
        .groups = "drop"
      )
  })
  
  annual_counts <- reactive({
    req(length(selected_site_keys()) > 0)
    req(input$start_year, input$end_year)
    
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(data.frame(year = integer(0), checklists = integer(0)))
    }
    
    yearly_counts <- df %>%
      mutate(
        sampling_id_chr = as.character(`SAMPLING EVENT IDENTIFIER`)
      ) %>%
      arrange(checklist_group_id, year, sampling_id_chr) %>%
      group_by(checklist_group_id) %>%
      summarise(
        year = first(year),
        .groups = "drop"
      ) %>%
      count(year, name = "checklists") %>%
      arrange(year)
    
    start_year <- as.integer(input$start_year)
    end_year <- as.integer(input$end_year)
    
    all_years <- data.frame(year = seq(start_year, end_year))
    
    all_years %>%
      left_join(yearly_counts, by = "year") %>%
      mutate(checklists = ifelse(is.na(checklists), 0, checklists))
  })
  
  annual_checklist_total <- reactive({
    req(length(selected_site_keys()) > 0)
    req(input$start_year)
    req(input$end_year)
    
    df <- year_filtered_data()
    
    if (nrow(df) == 0) {
      return(0)
    }
    
    df %>%
      summarise(n = n_distinct(checklist_group_id)) %>%
      pull(n)
  })
  
  species_summary <- reactive({
    df <- species_checklist_points()
    
    if (nrow(df) == 0) {
      return(list(individuals = 0, checklists = 0))
    }
    
    individuals_total <- sum(df$individuals_num, na.rm = TRUE)
    checklist_total <- nrow(df)
    
    list(
      individuals = individuals_total,
      checklists = checklist_total
    )
  })
  
  output$species_summary_text <- renderText({
    if (length(selected_site_keys()) == 0 ||
        is.null(input$selected_species) || input$selected_species == "" ||
        input$selected_species == "__none__") {
      return("Number Reported:\nUnique checklists:")
    }
    
    sp <- species_summary()
    
    paste0(
      "Number Reported: ", format(sp$individuals, big.mark = ","), "\n",
      "Unique Checklists: ", format(sp$checklists, big.mark = ",")
    )
  })
  
  output$annual_plot <- renderPlot(
    height = 450,
    res = 96,
    {
      if (length(selected_site_keys()) == 0) {
        plot.new()
        return()
      }
      
      df <- annual_counts()
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      draw_annual_plot(df)
    }
  )
  
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomSnap = 0,
        zoomDelta = 1
      )
    ) %>%
      addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      setView(lng = -79.5, lat = 37.8, zoom = 6.3)
  })
  
  observe({
    county_poly <- NULL
    
    if (active_search_mode() == "county" &&
        !is.null(county_boundary_layer) &&
        county_boundary_display_col %in% names(county_boundary_layer)) {
      selected_counties <- active_county_values()
      
      if (!is.null(selected_counties) &&
          length(selected_counties) > 0 &&
          !("__all__" %in% selected_counties)) {
        county_poly <- county_boundary_layer %>%
          filter(.data[[county_boundary_display_col]] %in% selected_counties)
      }
    }
    
    proxy <- leafletProxy("map") %>%
      clearShapes()
    
    if (length(selected_site_keys()) == 0) {
      proxy <- proxy %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() %>%
        clearHeatmap()
      
      if (!is.null(county_poly) && nrow(county_poly) > 0) {
        bb <- st_bbox(county_poly)
        
        proxy %>%
          addPolygons(
            data = county_poly,
            color = "#51748A",
            weight = 2,
            fillColor = "#51748A",
            fillOpacity = 0.07,
            opacity = 0.9,
            popup = county_poly[[county_boundary_display_col]]
          ) %>%
          fitBounds(
            lng1 = unname(bb["xmin"]),
            lat1 = unname(bb["ymin"]),
            lng2 = unname(bb["xmax"]),
            lat2 = unname(bb["ymax"])
          )
      } else {
        proxy %>%
          setView(lng = -79.5, lat = 37.8, zoom = 6.3)
      }
      
      return()
    }
    
    poly <- current_polygon()
    
    bb <- if (!is.null(county_poly) && nrow(county_poly) > 0) {
      st_bbox(county_poly)
    } else if (!is.null(poly) && nrow(poly) > 0) {
      st_bbox(poly)
    } else {
      NULL
    }
    
    popup_values <- if (!is.null(poly) && "site_name" %in% names(poly)) {
      poly$site_name
    } else {
      current_site_name()
    }
    
    if (!is.null(county_poly) && nrow(county_poly) > 0) {
      proxy <- proxy %>%
        addPolygons(
          data = county_poly,
          color = "#51748A",
          weight = 2,
          fillColor = "#51748A",
          fillOpacity = 0.07,
          opacity = 0.9,
          popup = county_poly[[county_boundary_display_col]]
        )
    }
    
    if (!is.null(poly) && nrow(poly) > 0) {
      proxy <- proxy %>%
        addPolygons(
          data = poly,
          color = "#2A5235",
          weight = 2,
          fillColor = "#2A5235",
          fillOpacity = 0.18,
          popup = popup_values
        )
    }
    
    if (!is.null(bb)) {
      proxy %>%
        fitBounds(
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        )
    }
  })
  
  observe({
    if (length(selected_site_keys()) == 0 ||
        is.null(input$start_year) || input$start_year == "" ||
        is.null(input$end_year) || input$end_year == "") {
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() %>%
        clearHeatmap()
      
      return()
    }
    
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearPopups() %>%
      clearHeatmap()
    
    settings <- heat_settings()
    
    if (input$map_type == "points") {
      if (is.null(input$selected_species) || input$selected_species == "" ||
          input$selected_species == "__none__") {
        return()
      }
      
      df_points <- species_checklist_points() %>%
        filter(!is.na(LATITUDE), !is.na(LONGITUDE))
      
      if (nrow(df_points) == 0) {
        return()
      }
      
      popup_values <- make_species_popup_html(df_points)
      
      proxy %>%
        addCircleMarkers(
          data = df_points,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 4,
          stroke = FALSE,
          fillColor = "#F36C21",
          fillOpacity = 0.7,
          popup = popup_values
        )
      
      return()
    }
    
    heat_metric <- input$heat_metric
    if (is.null(heat_metric) || length(heat_metric) == 0 || !nzchar(heat_metric)) {
      heat_metric <- "species_density"
    }
    
    if (identical(heat_metric, "all_checklists")) {
      df_heat <- prepare_heatmap_points(all_checklist_points(), "heat_intensity")
      
      if (nrow(df_heat) == 0) {
        return()
      }
      
      proxy %>%
        addHeatmap(
          data = df_heat,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = ~heat_intensity,
          blur = settings$blur,
          max = settings$max,
          radius = settings$radius,
          minOpacity = settings$min_opacity
        )
      
      return()
    }
    
    if (is.null(input$selected_species) || input$selected_species == "" ||
        input$selected_species == "__none__") {
      return()
    }
    
    df_points <- species_checklist_points() %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE))
    
    if (nrow(df_points) == 0) {
      return()
    }
    
    if (identical(heat_metric, "species_density")) {
      df_heat <- prepare_heatmap_points(
        df_points %>% mutate(heat_intensity = 1),
        "heat_intensity"
      )
      
      proxy %>%
        addHeatmap(
          data = df_heat,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = ~heat_intensity,
          blur = settings$blur,
          max = settings$max,
          radius = settings$radius,
          minOpacity = settings$min_opacity
        )
      
      return()
    }
    
    df_heat <- df_points %>%
      mutate(
        heat_intensity = ifelse(
          is.na(individuals_num) | individuals_num <= 0,
          1,
          pmax(1.5, sqrt(individuals_num) * 3.5)
        )
      ) %>%
      prepare_heatmap_points("heat_intensity")
    
    proxy %>%
      addHeatmap(
        data = df_heat,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        intensity = ~heat_intensity,
        blur = settings$blur,
        max = settings$max,
        radius = settings$radius,
        minOpacity = settings$min_opacity
      )
  })
}

# =========================
# RUN APP
# =========================
shinyApp(ui, server)