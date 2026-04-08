# WMA SPECIES v1 TESTING
# SPECIES v1 FINAL
Sys.setenv(CHROMOTE_CHROME_ARGS = "--no-sandbox")

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(stringr)
library(tibble)
library(rmarkdown)
library(htmlwidgets)
library(webshot2)

# =========================
# PATHS
# =========================
dwr_shp_file <- "data_raw/shapefiles/DWR_WMA_Boundaries/DWR_WMA_Boundaries.shp"
cache_dir    <- "data_processed/caches"
output_dir   <- "outputs"
report_template_file <- "report_template.Rmd"

message("Working directory: ", getwd())
message("Using DWR shapefile: ", normalizePath(dwr_shp_file, mustWork = FALSE))
message("Using cache directory: ", normalizePath(cache_dir, mustWork = FALSE))
message("Using output directory: ", normalizePath(output_dir, mustWork = FALSE))
message("Using report template: ", normalizePath(report_template_file, mustWork = FALSE))

# =========================
# HELPERS
# =========================
clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("wildlife management area", "wma") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

make_site_key <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("wildlife management area", "") %>%
    str_replace_all("\\bwma\\b", "") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    str_replace_all("_+", "_")
}

make_site_name_from_key <- function(site_key) {
  paste0(
    site_key %>%
      str_replace_all("_", " ") %>%
      str_squish() %>%
      str_to_title(),
    " WMA"
  )
}

read_cache_file <- function(site_name, path) {
  if (is.na(path) || !file.exists(path)) {
    stop("Cache file not found for ", site_name, ": ", path)
  }
  
  df <- readRDS(path)
  
  needed_cols <- c(
    "SAMPLING EVENT IDENTIFIER",
    "OBSERVATION DATE",
    "LATITUDE",
    "LONGITUDE",
    "COMMON NAME",
    "OBSERVATION COUNT"
  )
  missing_cols <- setdiff(needed_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Cache file for ", site_name, " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!"GROUP IDENTIFIER" %in% names(df)) {
    df$`GROUP IDENTIFIER` <- NA_character_
  }
  
  df %>%
    mutate(
      `OBSERVATION DATE` = as.Date(`OBSERVATION DATE`),
      checklist_group_id = if_else(
        !is.na(`GROUP IDENTIFIER`) & str_trim(`GROUP IDENTIFIER`) != "",
        as.character(`GROUP IDENTIFIER`),
        as.character(`SAMPLING EVENT IDENTIFIER`)
      )
    ) %>%
    filter(!is.na(`OBSERVATION DATE`), `OBSERVATION DATE` >= as.Date("2002-01-01"))
}

find_name_column <- function(wmas) {
  candidates <- c("WMA_NAME", "MANAME", "LABEL", "NAME", "AREA_NAME", "UNIT_NAME")
  hits <- candidates[candidates %in% names(wmas)]
  
  if (length(hits) == 0) {
    stop(
      "Could not find a likely WMA name column in shapefile.\n",
      "Available columns are:\n",
      paste(names(wmas), collapse = ", ")
    )
  }
  
  hits[1]
}

match_wma_features <- function(wmas, site_key) {
  name_col <- find_name_column(wmas)
  
  wmas2 <- wmas %>%
    mutate(
      wma_name_raw = as.character(.data[[name_col]]),
      site_key_tmp = make_site_key(wma_name_raw)
    )
  
  hit <- wmas2 %>%
    filter(site_key_tmp == site_key)
  
  if (nrow(hit) == 0) {
    stop("Could not find polygon in shapefile for site_key = ", site_key)
  }
  
  hit
}

match_wma_polygon <- function(wmas, site_key, site_name) {
  hit <- match_wma_features(wmas, site_key)
  
  geom <- hit %>%
    st_make_valid() %>%
    st_union()
  
  st_sf(
    site_key = site_key,
    site_name = site_name,
    geometry = st_sfc(geom, crs = st_crs(hit))
  ) %>%
    st_transform(4326)
}

first_non_missing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
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
  
  par(mar = c(4, 4, 1, 1))
  
  plot(
    x = df$year,
    y = df$checklists,
    type = "b",
    pch = 16,
    lwd = 2,
    col = "#22422a",
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
    col = "#22422a"
  )
}

save_annual_plot_png <- function(df, file) {
  png(filename = file, width = 1200, height = 700, res = 120)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)
  
  draw_annual_plot(df)
}

save_report_map_png <- function(poly,
                                file,
                                map_type = "points",
                                point_data = NULL,
                                heat_data = NULL,
                                heat_settings = NULL,
                                heat_gradient = NULL) {
  tmp_html <- tempfile(fileext = ".html")
  bb <- st_bbox(poly)
  
  map_widget <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    addPolygons(
      data = poly,
      color = "#22422a",
      weight = 2,
      fillColor = "#2A5235",
      fillOpacity = 0.25,
      popup = NULL
    )
  
  if (identical(map_type, "points")) {
    if (!is.null(point_data) && nrow(point_data) > 0) {
      map_widget <- map_widget %>%
        addCircleMarkers(
          data = point_data,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 4,
          stroke = FALSE,
          fillColor = "#F36C21",
          fillOpacity = 0.7,
          popup = NULL
        )
    }
  } else if (identical(map_type, "heat")) {
    if (!is.null(heat_data) &&
        nrow(heat_data) > 0 &&
        !is.null(heat_settings) &&
        !is.null(heat_gradient)) {
      map_widget <- map_widget %>%
        addHeatmap(
          data = heat_data,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = ~heat_intensity,
          blur = heat_settings$blur,
          max = heat_settings$max,
          radius = heat_settings$radius,
          minOpacity = heat_settings$min_opacity,
          gradient = heat_gradient
        )
    }
  }
  
  map_widget <- map_widget %>%
    fitBounds(
      lng1 = unname(bb["xmin"]),
      lat1 = unname(bb["ymin"]),
      lng2 = unname(bb["xmax"]),
      lat2 = unname(bb["ymax"])
    )
  
  htmlwidgets::saveWidget(
    widget = map_widget,
    file = tmp_html,
    selfcontained = TRUE
  )
  
  webshot2::webshot(
    url = tmp_html,
    file = file,
    vwidth = 1200,
    vheight = 850,
    zoom = 2
  )
}

sanitize_filename <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

common_heat_gradient <- c(
  "0.10" = "#2c7bb6",
  "0.25" = "#00a6ca",
  "0.40" = "#00ccbc",
  "0.55" = "#90eb9d",
  "0.70" = "#ffff8c",
  "0.82" = "#f9d057",
  "0.92" = "#f29e2e",
  "1.00" = "#e76818"
)

# =========================
# FIND CACHE FILES
# =========================
if (!dir.exists(cache_dir)) {
  stop("Cache directory not found: ", normalizePath(cache_dir, mustWork = FALSE))
}

cache_files <- list.files(
  cache_dir,
  pattern = "_checklists_cache\\.rds$",
  full.names = TRUE
)

if (length(cache_files) == 0) {
  stop("No cache files found in: ", normalizePath(cache_dir, mustWork = FALSE))
}

site_keys <- make_site_key(str_remove(basename(cache_files), "_checklists_cache\\.rds$"))

site_lookup_raw <- tibble(
  cache_file = cache_files,
  cache_file_name = basename(cache_files),
  site_key_raw = str_remove(cache_file_name, "_checklists_cache\\.rds$"),
  site_key = site_keys,
  site_name = make_site_name_from_key(site_keys)
)

site_lookup <- site_lookup_raw %>%
  mutate(
    is_preferred = !str_detect(site_key_raw, "_wma$")
  ) %>%
  arrange(desc(is_preferred), cache_file_name) %>%
  distinct(site_key, .keep_all = TRUE) %>%
  select(cache_file, cache_file_name, site_key, site_name) %>%
  arrange(site_name)

message("Found cache files after de-duplication:")
print(site_lookup %>% select(site_name, cache_file))

# =========================
# READ ALL CACHES
# =========================
cache_list <- list()

for (i in seq_len(nrow(site_lookup))) {
  sk <- site_lookup$site_key[i]
  sn <- site_lookup$site_name[i]
  cf <- site_lookup$cache_file[i]
  
  message("Reading cache for ", sn, ": ", cf)
  
  this_cache <- tryCatch(
    read_cache_file(sn, cf),
    error = function(e) {
      message("Skipping bad cache for ", sn, ": ", e$message)
      NULL
    }
  )
  
  if (is.null(this_cache)) next
  
  cache_list[[sk]] <- this_cache
}

site_lookup <- site_lookup %>%
  filter(site_key %in% names(cache_list)) %>%
  arrange(site_name)

if (length(cache_list) == 0) {
  stop("No usable cache files were loaded.")
}

nonempty_cache_keys <- names(cache_list)[vapply(cache_list, nrow, integer(1)) > 0]

if (length(nonempty_cache_keys) > 0) {
  global_min_date <- max(
    as.Date("2002-01-01"),
    min(
      unlist(lapply(cache_list[nonempty_cache_keys], function(x) x$`OBSERVATION DATE`)),
      na.rm = TRUE
    )
  )
  
  global_max_date <- max(
    unlist(lapply(cache_list[nonempty_cache_keys], function(x) x$`OBSERVATION DATE`)),
    na.rm = TRUE
  )
} else {
  global_min_date <- as.Date("2002-01-01")
  global_max_date <- Sys.Date()
}

global_min_year <- as.integer(strftime(as.Date(global_min_date), "%Y"))
global_max_year <- max(
  as.integer(strftime(as.Date(global_max_date), "%Y")),
  as.integer(format(Sys.Date(), "%Y"))
)
global_year_choices <- as.character(seq(2002, global_max_year, by = 1))

# =========================
# READ SHAPEFILE
# =========================
if (!file.exists(dwr_shp_file)) {
  stop("DWR shapefile not found: ", normalizePath(dwr_shp_file, mustWork = FALSE))
}

dwr_wmas <- st_read(dwr_shp_file, quiet = TRUE) %>%
  st_make_valid()

message("Columns in DWR shapefile:")
print(names(dwr_wmas))

# =========================
# BUILD POLYGONS
# =========================
polygon_list <- setNames(vector("list", nrow(site_lookup)), site_lookup$site_key)

for (i in seq_len(nrow(site_lookup))) {
  sk <- site_lookup$site_key[i]
  sn <- site_lookup$site_name[i]
  
  message("Building polygon for ", sn, " (", sk, ")")
  
  poly_try <- tryCatch(
    {
      match_wma_polygon(
        wmas = dwr_wmas,
        site_key = sk,
        site_name = sn
      )
    },
    error = function(e) {
      message("Polygon build failed for ", sn, ": ", e$message)
      NULL
    }
  )
  
  polygon_list[[sk]] <- poly_try
}

valid_site_keys <- names(polygon_list)[!vapply(polygon_list, is.null, logical(1))]

site_lookup <- site_lookup %>%
  filter(site_key %in% valid_site_keys) %>%
  arrange(site_name)

cache_list <- cache_list[site_lookup$site_key]
polygon_list <- polygon_list[site_lookup$site_key]

if (nrow(site_lookup) == 0) {
  stop("No sites remain after matching caches to polygons.")
}

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-text-output {
        margin-top: 5px;
      }
      .shiny-text-output pre {
        margin: 0;
        min-height: 58px;
        padding: 8.5px;
        font-size: 14px;
        line-height: 1.42857143;
        color: #333333;
        word-break: break-all;
        word-wrap: break-word;
        background-color: #f5f5f5;
        border: 1px solid #cccccc;
        border-radius: 4px;
        font-family: Menlo, Monaco, Consolas, 'Courier New', monospace;
        font-weight: 400;
      }
      "))
  ),
  
  titlePanel("Virginia WMA eBird Species Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Select WMA"),
      
      selectInput(
        "site",
        NULL,
        choices = c(
          "Select WMA" = "",
          "All WMAs" = "__all__",
          setNames(site_lookup$site_key, site_lookup$site_name)
        ),
        selected = ""
      ),
      
      tags$div(style = "margin-top:20px;"),
      
      tags$h4("Select Year"),
      selectInput(
        "selected_year",
        NULL,
        choices = c("Select Year" = ""),
        selected = ""
      ),
      
      verbatimTextOutput("summary_text", placeholder = TRUE),
      
      tags$div(style = "margin-top:20px;"),
      
      tags$h4("Select Species"),
      uiOutput("species_ui"),
      
      verbatimTextOutput("species_summary_text", placeholder = TRUE),
      
      tags$div(style = "margin-top:20px;"),
      
      tags$h4("Map Style"),
      div(
        style = "margin-top: 15px;",
        radioButtons(
          "map_type",
          NULL,
          choices = c("Points" = "points", "Heat Map" = "heat"),
          selected = "points"
        )
      ),
      
      tags$div(style = "margin-top:20px;"),
      
      conditionalPanel(
        condition = "input.map_type == 'heat'",
        tags$h4("Map Based On"),
        div(
          style = "margin-top: 15px;",
          radioButtons(
            "heat_metric",
            NULL,
            choices = c(
              "Species density" = "species_density",
              "Individual density" = "individuals_density",
              "All checklist activity" = "all_checklists"
            ),
            selected = "species_density"
          )
        )
      ),
      
      tags$div(style = "margin-top:20px;"),
      
      tags$h4("View Checklists Over Time"),
      tags$div(style = "margin-top:8px;"),
      
      selectInput(
        "year_min",
        "Start year",
        choices = global_year_choices,
        selected = as.character(global_min_year)
      ),
      
      selectInput(
        "year_max",
        "End year",
        choices = global_year_choices,
        selected = as.character(global_min_year)
      ),
      
      tags$div(style = "margin-top:5px;"),
      
      tags$div(style = "margin-top:20px;"),
      downloadButton("download_report", "Download Report")
    ),
    
    mainPanel(
      div(
        style = "border: 1px solid black; border-radius: 4px; padding: 2px;",
        leafletOutput("map", height = 650)
      ),
      
      tags$div(style = "margin-top:20px;"),
      
      div(
        style = "border: 1px solid black; border-radius: 4px; padding: 6px; background-color: white;",
        plotOutput("annual_plot", height = 250)
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  last_selected_year <- reactiveVal("")
  last_selected_species <- reactiveVal("")
  
  observeEvent(input$selected_year, {
    if (!is.null(input$selected_year) && input$selected_year != "") {
      last_selected_year(input$selected_year)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$selected_species, {
    if (!is.null(input$selected_species) &&
        input$selected_species != "" &&
        input$selected_species != "__none__") {
      last_selected_species(input$selected_species)
    }
  }, ignoreInit = TRUE)
  
  current_cache <- reactive({
    req(input$site)
    req(input$site != "")
    
    if (input$site == "__all__") {
      if (length(cache_list) == 0) {
        return(tibble(
          `SAMPLING EVENT IDENTIFIER` = character(),
          `GROUP IDENTIFIER` = character(),
          checklist_group_id = character(),
          `OBSERVATION DATE` = as.Date(character()),
          LATITUDE = numeric(),
          LONGITUDE = numeric(),
          `COMMON NAME` = character(),
          `OBSERVATION COUNT` = character(),
          site_key = character(),
          site_name = character()
        ))
      }
      
      all_cache <- bind_rows(
        lapply(seq_along(cache_list), function(i) {
          sk <- names(cache_list)[i]
          sn <- site_lookup$site_name[match(sk, site_lookup$site_key)]
          cache_list[[i]] %>%
            mutate(
              site_key = sk,
              site_name = sn
            )
        })
      )
      
      return(all_cache)
    }
    
    cache_list[[input$site]] %>%
      mutate(
        site_key = input$site,
        site_name = site_lookup$site_name[match(input$site, site_lookup$site_key)]
      )
  })
  
  current_polygon <- reactive({
    req(input$site)
    req(input$site != "")
    
    if (input$site == "__all__") {
      return(do.call(rbind, polygon_list))
    }
    
    polygon_list[[input$site]]
  })
  
  current_site_name <- reactive({
    req(input$site)
    req(input$site != "")
    
    if (input$site == "__all__") {
      return("All WMAs")
    }
    
    site_lookup$site_name[match(input$site, site_lookup$site_key)]
  })
  
  heat_settings <- reactive({
    req(input$site)
    req(input$site != "")
    
    is_all <- identical(input$site, "__all__")
    
    if (input$heat_metric == "all_checklists") {
      if (is_all) {
        return(list(radius = 18, blur = 14, max = 0.12, min_opacity = 0.35))
      } else {
        return(list(radius = 20, blur = 16, max = 0.22, min_opacity = 0.35))
      }
    }
    
    if (input$heat_metric == "species_density") {
      if (is_all) {
        return(list(radius = 18, blur = 14, max = 0.10, min_opacity = 0.35))
      } else {
        return(list(radius = 20, blur = 16, max = 0.20, min_opacity = 0.35))
      }
    }
    
    if (is_all) {
      return(list(radius = 18, blur = 14, max = 0.18, min_opacity = 0.35))
    } else {
      return(list(radius = 20, blur = 16, max = 0.30, min_opacity = 0.35))
    }
  })
  
  observeEvent(input$site, {
    remembered_year <- last_selected_year()
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (is.null(input$site) || input$site == "") {
      updateSelectInput(
        session,
        "selected_year",
        choices = c("Select Year" = ""),
        selected = ""
      )
      
      updateSelectInput(
        session,
        "year_min",
        choices = global_year_choices,
        selected = as.character(global_min_year)
      )
      
      updateSelectInput(
        session,
        "year_max",
        choices = global_year_choices,
        selected = as.character(global_min_year)
      )
      
      return()
    }
    
    site_df <- current_cache()
    
    if (nrow(site_df) == 0) {
      updateSelectInput(
        session,
        "selected_year",
        choices = c("No data available" = "__none__"),
        selected = "__none__"
      )
      
      site_year_choices <- as.character(seq(2002, current_year, by = 1))
      
      updateSelectInput(
        session,
        "year_min",
        choices = site_year_choices,
        selected = "2002"
      )
      
      updateSelectInput(
        session,
        "year_max",
        choices = site_year_choices,
        selected = "2002"
      )
      
      return()
    }
    
    site_year_choices <- site_df %>%
      filter(!is.na(`OBSERVATION DATE`)) %>%
      mutate(year = as.integer(strftime(as.Date(`OBSERVATION DATE`), "%Y"))) %>%
      distinct(year) %>%
      arrange(desc(year)) %>%
      pull(year) %>%
      as.character()
    
    if (length(site_year_choices) == 0) {
      updateSelectInput(
        session,
        "selected_year",
        choices = c("No data available" = "__none__"),
        selected = "__none__"
      )
      
      site_year_choices_graph <- as.character(seq(2002, current_year, by = 1))
      
      updateSelectInput(
        session,
        "year_min",
        choices = site_year_choices_graph,
        selected = "2002"
      )
      
      updateSelectInput(
        session,
        "year_max",
        choices = site_year_choices_graph,
        selected = "2002"
      )
      
      return()
    }
    
    year_prompt <- if (!is.null(remembered_year) &&
                       remembered_year != "" &&
                       !(remembered_year %in% site_year_choices)) {
      "No data available"
    } else {
      "Select Year"
    }
    
    new_year <- if (!is.null(remembered_year) &&
                    remembered_year != "" &&
                    remembered_year %in% site_year_choices) {
      remembered_year
    } else {
      ""
    }
    
    updateSelectInput(
      session,
      "selected_year",
      choices = c(setNames("", year_prompt), site_year_choices),
      selected = new_year
    )
    
    site_years_num <- site_df %>%
      filter(!is.na(`OBSERVATION DATE`)) %>%
      mutate(year = as.integer(strftime(as.Date(`OBSERVATION DATE`), "%Y"))) %>%
      pull(year)
    
    if (length(site_years_num) > 0) {
      min_site_year <- min(site_years_num, na.rm = TRUE)
      max_site_year_actual <- max(site_years_num, na.rm = TRUE)
      max_site_year_for_ui <- max(max_site_year_actual, current_year)
      graph_year_choices <- as.character(seq(min_site_year, max_site_year_for_ui, by = 1))
      
      updateSelectInput(
        session,
        "year_min",
        choices = graph_year_choices,
        selected = as.character(min_site_year)
      )
      
      updateSelectInput(
        session,
        "year_max",
        choices = graph_year_choices,
        selected = as.character(min_site_year)
      )
    }
  }, ignoreInit = FALSE)
  
  species_state <- reactive({
    if (is.null(input$site) || input$site == "") {
      return(list(
        mode = "select",
        choices = character(0),
        selected = ""
      ))
    }
    
    site_df <- current_cache()
    
    if (nrow(site_df) == 0) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
    }
    
    site_year_choices <- site_df %>%
      filter(!is.na(`OBSERVATION DATE`)) %>%
      mutate(year = as.integer(strftime(as.Date(`OBSERVATION DATE`), "%Y"))) %>%
      distinct(year) %>%
      arrange(desc(year)) %>%
      pull(year) %>%
      as.character()
    
    if (length(site_year_choices) == 0) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
    }
    
    target_year <- if (!is.null(input$selected_year) && input$selected_year != "") {
      input$selected_year
    } else {
      last_selected_year()
    }
    
    if (is.null(target_year) || target_year == "") {
      return(list(
        mode = "select",
        choices = character(0),
        selected = ""
      ))
    }
    
    if (!(target_year %in% site_year_choices)) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
    }
    
    df_year <- site_df %>%
      mutate(year = strftime(as.Date(`OBSERVATION DATE`), "%Y")) %>%
      filter(year == target_year)
    
    species_choices <- df_year %>%
      filter(!is.na(`COMMON NAME`), `COMMON NAME` != "") %>%
      distinct(`COMMON NAME`) %>%
      arrange(`COMMON NAME`) %>%
      pull(`COMMON NAME`)
    
    if (length(species_choices) == 0) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
    }
    
    target_species <- if (!is.null(input$selected_species) &&
                          input$selected_species != "" &&
                          input$selected_species != "__none__") {
      input$selected_species
    } else {
      last_selected_species()
    }
    
    if (!is.null(target_species) &&
        target_species != "" &&
        !(target_species %in% species_choices)) {
      return(list(
        mode = "none",
        choices = character(0),
        selected = "__none__"
      ))
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
          options = list(
            placeholder = "Select Species"
          )
        )
      )
    }
    
    selectizeInput(
      "selected_species",
      NULL,
      choices = c("Select Species" = "", st$choices),
      selected = st$selected,
      options = list(
        placeholder = "Select Species"
      )
    )
  })
  
  filtered_data <- reactive({
    req(input$site)
    req(input$site != "")
    req(input$year_min, input$year_max)
    
    df <- current_cache()
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    start_year <- as.integer(input$year_min)
    end_year <- as.integer(input$year_max)
    
    if (is.na(start_year) || is.na(end_year)) {
      return(df[0, , drop = FALSE])
    }
    
    if (start_year > end_year) {
      return(df[0, , drop = FALSE])
    }
    
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31"))
    
    df %>%
      filter(
        !is.na(`OBSERVATION DATE`),
        `OBSERVATION DATE` >= start_date,
        `OBSERVATION DATE` <= end_date
      )
  })
  
  year_filtered_data <- reactive({
    req(input$site)
    req(input$site != "")
    req(input$selected_year)
    
    df <- current_cache()
    
    if (input$selected_year == "" || input$selected_year == "__none__") {
      return(df[0, , drop = FALSE])
    }
    
    if (nrow(df) == 0) {
      return(df[0, , drop = FALSE])
    }
    
    df %>%
      mutate(year = strftime(as.Date(`OBSERVATION DATE`), "%Y")) %>%
      filter(year == input$selected_year)
  })
  
  species_filtered_data <- reactive({
    req(input$site)
    req(input$site != "")
    req(input$selected_year)
    req(input$selected_species)
    
    df <- year_filtered_data()
    
    if (input$selected_year == "" ||
        input$selected_year == "__none__" ||
        input$selected_species == "" ||
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
    req(input$site)
    req(input$site != "")
    req(input$year_min, input$year_max)
    
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(data.frame(year = integer(0), checklists = integer(0)))
    }
    
    yearly_counts <- df %>%
      mutate(
        year = as.integer(strftime(as.Date(`OBSERVATION DATE`), "%Y")),
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
    
    start_year <- as.integer(input$year_min)
    end_year <- as.integer(input$year_max)
    
    all_years <- data.frame(year = seq(start_year, end_year))
    
    all_years %>%
      left_join(yearly_counts, by = "year") %>%
      mutate(checklists = ifelse(is.na(checklists), 0, checklists))
  })
  
  annual_checklist_total <- reactive({
    req(input$site)
    req(input$site != "")
    req(input$selected_year)
    
    df <- year_filtered_data()
    
    if (input$selected_year == "" || input$selected_year == "__none__" || nrow(df) == 0) {
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
  
  output$summary_text <- renderText({
    if (is.null(input$site) || input$site == "" ||
        is.null(input$selected_year) || input$selected_year == "" ||
        input$selected_year == "__none__") {
      return("Checklists:")
    }
    
    paste0(
      "Checklists in ", input$selected_year, ": ",
      format(annual_checklist_total(), big.mark = ",")
    )
  })
  
  output$species_summary_text <- renderText({
    if (is.null(input$site) || input$site == "" ||
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
    width = 1000,
    height = 250,
    res = 96,
    {
      if (is.null(input$site) || input$site == "") {
        plot.new()
        return()
      }
      
      df <- annual_counts()
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      draw_annual_plot(df)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      site_part <- if (is.null(input$site) || input$site == "") {
        "wma"
      } else {
        sanitize_filename(current_site_name())
      }
      
      year_part <- if (is.null(input$selected_year) || input$selected_year == "") {
        "no_year"
      } else {
        input$selected_year
      }
      
      species_part <- if (!is.null(input$selected_species) &&
                          input$selected_species != "" &&
                          input$selected_species != "__none__") {
        sanitize_filename(input$selected_species)
      } else {
        "all_species"
      }
      
      map_part <- if (!is.null(input$map_type) && input$map_type == "heat") {
        paste0("heat_", input$heat_metric)
      } else {
        "points"
      }
      
      paste0(site_part, "_", year_part, "_", species_part, "_", map_part, "_report.pdf")
    },
    
    content = function(file) {
      req(input$site)
      req(input$site != "")
      req(input$selected_year)
      req(input$selected_year != "")
      req(input$selected_year != "__none__")
      
      if (!file.exists(report_template_file)) {
        stop("Could not find report template: ", normalizePath(report_template_file, mustWork = FALSE))
      }
      
      tmp_dir <- tempdir()
      report_rmd <- file.path(tmp_dir, "report_template.Rmd")
      report_pdf <- file.path(tmp_dir, "report_output.pdf")
      map_png <- file.path(tmp_dir, "report_map.png")
      annual_png <- file.path(tmp_dir, "annual_plot.png")
      
      file.copy(report_template_file, report_rmd, overwrite = TRUE)
      
      site_name_val <- current_site_name()
      selected_year_val <- input$selected_year
      selected_species_val <- if (is.null(input$selected_species)) "" else input$selected_species
      checklist_total_val <- annual_checklist_total()
      species_summary_val <- species_summary()
      annual_counts_val <- annual_counts()
      poly_val <- current_polygon()
      report_date_val <- format(Sys.time(), "%Y-%m-%d %H:%M")
      
      report_map_type <- input$map_type
      report_heat_metric <- input$heat_metric
      report_heat_settings <- heat_settings()
      
      report_point_data <- NULL
      report_heat_data <- NULL
      
      if (report_map_type == "points") {
        if (!is.null(input$selected_species) &&
            input$selected_species != "" &&
            input$selected_species != "__none__") {
          report_point_data <- species_checklist_points() %>%
            filter(!is.na(LATITUDE), !is.na(LONGITUDE))
        } else {
          report_point_data <- tibble(
            LONGITUDE = numeric(),
            LATITUDE = numeric()
          )
        }
      } else {
        if (report_heat_metric == "all_checklists") {
          report_heat_data <- all_checklist_points() %>%
            filter(!is.na(LATITUDE), !is.na(LONGITUDE))
        } else if (!is.null(input$selected_species) &&
                   input$selected_species != "" &&
                   input$selected_species != "__none__") {
          df_points <- species_checklist_points() %>%
            filter(!is.na(LATITUDE), !is.na(LONGITUDE))
          
          if (report_heat_metric == "species_density") {
            report_heat_data <- df_points %>%
              mutate(heat_intensity = 1)
          } else {
            report_heat_data <- df_points %>%
              mutate(
                heat_intensity = ifelse(
                  is.na(individuals_num) | individuals_num <= 0,
                  1,
                  pmax(1.5, sqrt(individuals_num) * 3.5)
                )
              )
          }
        } else {
          report_heat_data <- tibble(
            LONGITUDE = numeric(),
            LATITUDE = numeric(),
            heat_intensity = numeric()
          )
        }
      }
      
      save_report_map_png(
        poly = poly_val,
        file = map_png,
        map_type = report_map_type,
        point_data = report_point_data,
        heat_data = report_heat_data,
        heat_settings = report_heat_settings,
        heat_gradient = common_heat_gradient
      )
      
      save_annual_plot_png(
        df = annual_counts_val,
        file = annual_png
      )
      
      rendered_file <- rmarkdown::render(
        input = report_rmd,
        output_file = report_pdf,
        params = list(
          site_name = site_name_val,
          selected_year = selected_year_val,
          selected_species = selected_species_val,
          checklist_total = checklist_total_val,
          species_individuals = species_summary_val$individuals,
          species_checklists = species_summary_val$checklists,
          annual_plot_path = annual_png,
          map_path = map_png,
          report_date = report_date_val,
          heat_metric = if (is.null(input$heat_metric)) "" else input$heat_metric
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      if (!file.exists(rendered_file)) {
        stop("PDF was not created.")
      }
      
      file.copy(rendered_file, file, overwrite = TRUE)
    }
  )
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      setView(lng = -78.5, lat = 37.5, zoom = 7)
  })
  
  observe({
    if (is.null(input$site) || input$site == "") {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() %>%
        clearHeatmap() %>%
        setView(lng = -78.5, lat = 37.5, zoom = 7)
      return()
    }
    
    poly <- current_polygon()
    bb <- st_bbox(poly)
    
    popup_values <- if ("site_name" %in% names(poly)) poly$site_name else current_site_name()
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = poly,
        color = "#22422a",
        weight = 2,
        fillColor = "#2A5235",
        fillOpacity = 0.25,
        popup = popup_values
      ) %>%
      fitBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"])
      )
  })
  
  observe({
    if (is.null(input$site) || input$site == "" ||
        is.null(input$selected_year) || input$selected_year == "" ||
        input$selected_year == "__none__") {
      
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
    
    if (input$heat_metric == "all_checklists") {
      df_heat <- all_checklist_points() %>%
        filter(!is.na(LATITUDE), !is.na(LONGITUDE))
      
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
          minOpacity = settings$min_opacity,
          gradient = common_heat_gradient
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
    
    if (input$heat_metric == "species_density") {
      df_heat <- df_points %>%
        mutate(heat_intensity = 1)
      
      proxy %>%
        addHeatmap(
          data = df_heat,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = ~heat_intensity,
          blur = settings$blur,
          max = settings$max,
          radius = settings$radius,
          minOpacity = settings$min_opacity,
          gradient = common_heat_gradient
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
        minOpacity = settings$min_opacity,
        gradient = common_heat_gradient
      )
  })
}

# =========================
# RUN APP
# =========================
shinyApp(ui, server)