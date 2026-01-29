# --- app.R ( UPDATES: Optimized, no flicker. Use MapStatus column. With interaction logging.
#             All drop-down lists update upon selection of any filter. 
#             When filtered down to 1 record, all categories get populated in banner.
#             Collapsed legend. Map does not scroll on page scroll. Added an hovering information box.    
#             Made sure the +/- and full screen buttons on the map work                                ) ---

# --- Load logger ---
source("scripts/logger.R")
init_logger()

# --- Load libraries ---
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(sf)
library(spData)
library(leaflet)
library(leaflet.extras)
library(DT)

# Load world map
data(world)

# ---- CONFIG (LIVE DATA LOADER) ----
#github_raw_url1 <- "https://raw.githubusercontent.com/palmycology/Robigus/main/docs/data/Data_massaged.tsv"
#github_raw_url2 <- "https://raw.githubusercontent.com/palmycology/Robigus/main/docs/data/precomputed_zscores.tsv"

local_file1 <- "data/Data_massaged.tsv"
local_file2 <- "data/precomputed_zscores.tsv"

# --- Function to load TSV with remote fallback ---
#load_tsv_fallback <- function(remote_url, local_file) {
#  src <- "remote"
#  df <- tryCatch(
#    read_tsv(remote_url, show_col_types = FALSE),
#    error = function(e) {
#      message("Remote load failed, using local file: ", local_file)
#      src <<- "local"
#      read_tsv(local_file, show_col_types = FALSE)
#    }
#  )
#  attr(df, "source") <- src
#  df
#}

# --- Optimized cleaning function ---
clean_data <- function(df) {
  names(df) <- trimws(names(df))
  names(df) <- sub("^\ufeff", "", names(df))
  
  char_cols <- c("Country","Year","Pathogen","Disease","Host","name_long","MapStatus")
  num_cols <- c("RecordCount","Z_all_years","Z_all_years_clamped","Z_by_year","Z_by_year_clamped")
  all_cols <- c(char_cols, num_cols)
  
  for (col in all_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  
  df[char_cols] <- lapply(df[char_cols], function(x) trimws(as.character(x)))
  df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
  
  # Ensure MapStatus has valid values
  df$MapStatus <- ifelse(df$MapStatus %in% c("Mappable","Unmappable"), df$MapStatus, "Unmappable")
  
  df
}

# --- Load and clean datasets ---
#full_data_cleaned <- clean_data(load_tsv_fallback(github_raw_url1, local_file1))
#df2_cleaned <- clean_data(load_tsv_fallback(github_raw_url2, local_file2))

full_data_cleaned <- clean_data(read_tsv(local_file1, show_col_types = FALSE))
df2_cleaned       <- clean_data(read_tsv(local_file2, show_col_types = FALSE))

# Source of full data
#data_source <- attr(full_data_cleaned, "source")
#print(data_source)

# --- Filterable columns ---
filter_columns <- c("Pathogen","Disease","Host","Country","Year")

# --- All countries list ---
all_countries <- sort(unique(trimws(na.omit(full_data_cleaned$Country))))
mappable_countries <- all_countries[full_data_cleaned$MapStatus[match(all_countries, full_data_cleaned$Country)]=="Mappable"]
unmappable_countries <- all_countries[full_data_cleaned$MapStatus[match(all_countries, full_data_cleaned$Country)]=="Unmappable"]


ui <- fluidPage(
  theme = shinytheme("readable"),
  tags$head(
    # --- Custom JS handler for dynamic country choices ---
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update_country_choices', function(message) {
        var select = $('#input_Country').selectize()[0].selectize;
        select.clearOptions(); 
        select.clearOptionGroups();
        var groupedChoices = message.choices;
        groupedChoices.forEach(function(item) {
          if (item.value && item.label) {
            select.addOption({ value: item.value, label: item.label, text: item.label });
          } else if (item.optgroup && Array.isArray(item.options)) {
            select.addOptionGroup(item.optgroup, { label: item.optgroup });
            item.options.forEach(function(opt) { 
              select.addOption({ value: opt.value, label: opt.label, text: opt.label, optgroup: item.optgroup }); 
            });
          }
        });
        select.refreshOptions(false);
        if (message.selected) select.setValue(message.selected);
      });
    
      Shiny.addCustomMessageHandler('blur_selectize', function(message) {
        var select = $('#' + message.inputId).selectize()[0].selectize;
        select.blur();  // remove focus so dropdown closes
      });
    
      document.addEventListener('DOMContentLoaded', function() {
        var container = document.getElementById('infoContainer');
        var tab = document.getElementById('infoTab');

      // Start with box open
      tab.textContent = \"✕\";  // thinner cross
      tab.classList.remove(\"hamburger\");

      tab.onclick = function(e) {
      //  e.stopPropagation();
        container.classList.toggle('collapsed');

        if(container.classList.contains('collapsed')) {
          tab.textContent = \"≡\";       // hamburger
          tab.classList.add(\"hamburger\");
          } else {
          tab.textContent = \"✕\";       // cross
          tab.classList.remove(\"hamburger\");
        }
      };
    });
  ")),
    
    # --- CSS ---
  tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background-color: #f8f9fa; }
    
    .main-title { font-size: 48px; font-weight: bold; color: #0f5132; padding:5px 0; margin-bottom: 0; }
    .subtitle { font-size: 20px; color: #6c757d; margin: 0; padding: 0; }

    /* --- Filter Bar Styling --- */
    .filter-bar { 
      display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 5px;
      position: relative; z-index: 10001; transition: margin-left 0.3s ease;
    }

    #infoContainer:not(.collapsed) ~ .filter-bar { margin-left: 330px; }
    #infoContainer.collapsed ~ .filter-bar { margin-left: 10px; }

    .filter-item { flex: 1 1 200px; min-width: 180px; }

    /* --- Category Box --- */
    .filter-box {
      border: 1px solid #ccc; border-radius: 6px; padding: 6px 8px; background-color: #fff;
    }
    
    /* --- Info Box Styling --- */
    #infoContainer { position: fixed; top: 120px; left: 0; z-index: 9999; }

    /* Info box */
    #infoBox {
      width: 320px; height: 520px; background: rgba(240,240,240,0.85);
      border: none; border-radius: 0 8px 8px 0; padding: 10px;
      box-shadow: 2px 2px 6px rgba(0,0,0,0.2); 
      transition: transform 0.3s ease; position: relative; left: 0;
    }

    /* Content */
    #infoBox .content { font-size: 13px; opacity: 1; transition: opacity 0.2s ease; }

    /* Tab */
    #infoTab {
      width: 30px; height: 60px; background: rgba(240,240,240,0.9); border: none;
      border-radius: 0 4px 4px 0; position: fixed; top: 150px; left: 320px; display: flex; 
      justify-content: center; align-items: center; font-size: 18px; cursor: pointer;
      user-select: none; transition: left 0.3s ease; z-index: 3;
    }
    
    /* Cross symbol thinner (✕) */
    #infoTab.collapsed, infoTab.open { font-family: 'Arial', sans-serif; font-weight: 400; font-size: 18px; }

    /* Hamburger spacing */
    #infoTab.hamburger { letter-spacing: 4px; font-weight: 700; }

    /* Collapsed state */
    #infoContainer.collapsed #infoBox { transform: translateX(-320px); }
    #infoContainer.collapsed #infoBox .content { opacity: 0; pointer-events: none; }
    #infoContainer.collapsed #infoTab { left: 0; }
    
  "))
  ),
  
  # --- Title ---
  div(
    style = "text-align:center; margin-top:-20px;",
    h1("Robigus", class="main-title"),
    p("An initiative from the UF/IFAS Department of Plant Pathology to catalog and map plant diseases globally", class="subtitle"),
    h6("Plant Disease Notes (1980 - 2024) published by American Phytopathological Society Press", 
       style="color: black; margin-top:5px;")
  ),
  
  # --- Info Popup ---
  tags$div(
    id = "infoContainer",
    tags$div(
      id = "infoBox",
        tags$div(class = "content",
          tags$h4("Information"),
          "This ", tags$strong("interactive map")," showing ", tags$strong("the distribution of plant diseases"),
          " is based on an analysis of 9,609 Plant Disease Note (PDN) titles 
          published by ", tags$strong("American Phytopathological Society (APS)")," over 45 years.", 
          tags$br(), tags$br(), 
          "The current map shows an ", tags$strong("'AAA summary'")," i.e., ALL PDNs published over ALL years for ALL countries. 
          ", tags$strong("Intensity of green color")," is proportional to the number of PDNs published from that country. 
          As you hover over a region, the cumulative number of published PDNs can be seen.", 
          tags$br(), tags$br(), 
          tags$strong("HOW IT WORKS: "),"Use the filter panel to explore the data. 
          Filtering is ", tags$strong("hierarchical")," i.e., each selection refines the available values in other categories. 
          Three categories - ", tags$strong("Pathogen, Disease")," and ", tags$strong("Host")," - have alphabetical pre-filters. 
          The ", tags$strong("Country")," category is split into ", tags$i("Mappable")," and ", tags$i("Unmappable")," data, 
          while the ", tags$strong("Year")," filter highlights countries that published PDNs in a given year. When a single record 
          remains, all details appear in the summary banner.", 
          tags$br(), tags$br(), 
          "Below the map is a ", tags$strong("Table of Citations")," for the data being plotted."
        )
      ),
    tags$div(id = "infoTab", "✖")
  ),
  
  uiOutput("banner_ui"),
  
  # --- FILTER BAR (with grouped boxes) ---
  div(class = "filter-bar",
    # Pathogen box
    div(class="filter-item",
      selectInput("letter_Pathogen", "Pathogen: A-Z", choices = c("All","#",LETTERS), selected = "All"),
      selectInput("input_Pathogen", "Select Pathogen", choices = c("All"), selected = "All")
    ),
    # Disease box
    div(class="filter-item",
      selectInput("letter_Disease", "Disease: A-Z", choices = c("All","#",LETTERS), selected = "All"),
      selectInput("input_Disease", "Select Disease", choices = c("All"), selected = "All")
    ),
    # Host box
    div(class="filter-item",
      selectInput("letter_Host", "Host: A-Z", choices = c("All","#",LETTERS), selected = "All"),
      selectInput("input_Host", "Select Host", choices = c("All"), selected = "All")
    ),
    # Country and Year
    div(class="filter-item",
      selectizeInput("input_Country", "Select Country", choices = NULL, selected = "All",
                      options = list(placeholder = "All", labelField = "label", valueField = "value", optgroupField = "optgroup")),
      selectInput("input_Year", "Select Year", choices = c("All"), selected = "All")
    ),
    # Reset
    div(class="filter-item", 
      style="display:flex; align-items:center; justify-content:center;",
      actionButton("reset_btn", "Reset All", class="btn btn-outline-primary"))
  ),
  
  # --- FULL-WIDTH MAP ---
  fluidRow(
    column(12, leafletOutput("map", height = 550))
  ),
  br(),
  
  # --- ABOUT PANEL + TABLE SIDE BY SIDE ---
  fluidRow(
    column(
      4,
      wellPanel(
        style = "
                background-color: #e6f7e6;
                border: 1px solid #b7dfb7;
                border-radius: 12px;
                padding: 12px 15px;
                box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
                text-align: left;
              ",
        tags$img(src = "logo.png", height = "50px", 
                 style = "float: right; margin-left: 10px; border-radius: 3px;"),
        h5("About this App"),
        tags$p(HTML("<b>Creator:</b> Braham Dhillon,<br>Assistant Professor")),
        tags$p(HTML("<b>Affiliation:</b><br>
                    University of Florida<br>
                    Institute of Food and Agricultural Sciences<br>
                    Department of Plant Pathology<br>
                    Fort Lauderdale Research and Education Center<br>
                    Davie, FL")),
        tags$p(HTML("<b>Contact:</b><br>dhillonb *at* ufl *dot* edu"))
      )
    ),
    column(
      8,
      DTOutput("filtered_table")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # --- Log session start ---
  log_interaction(session, "SESSION_START", "User connected")
  # Initialize info box behavior
  
  observe({
    session$sendCustomMessage("initInfoBox", list())
  })
  
  # --- Make sure to log when session ends ---
  session$onSessionEnded(function() {
    log_interaction(session, "SESSION_END", "User disconnected")
  })
  
  filter_state <- reactiveValues(Pathogen="All", Disease="All", Host="All", Country="All", Year="All")
  
  # --- Update filter_state on input + log changes ---
  lapply(filter_columns, function(col) {
    observeEvent(input[[paste0("input_", col)]], { 
      filter_state[[col]] <- input[[paste0("input_", col)]]
      log_interaction(session, "FILTER_CHANGE", paste0(col, " = ", input[[paste0("input_", col)]]))
    }, ignoreInit=TRUE)
  })
  
  # --- Reactive filtered dataset ---
  filtered_data <- reactive({
    df <- full_data_cleaned
    for(col in filter_columns) {
      val <- filter_state[[col]]
      if(!is.null(val) && val != "All") df <- df[!is.na(df[[col]]) & df[[col]]==val,]
    }
    if(nrow(df)==0) df <- full_data_cleaned
    df
  })
  
  # --- Helper functions ---
  get_filtered_choices <- function(df, col, letter) {
    vals <- unique(na.omit(df[[col]]))
    if (!is.null(letter) && letter != "All") {
      if (letter == "#") {
        vals <- vals[grepl("^[^A-Za-z]", vals)]
      } else {
        vals <- vals[grepl(paste0("^", letter), vals, ignore.case = TRUE)]
      }
    }
    sort(as.character(vals))
  }
  
  safe_update_selectize <- function(inputId, choices, selected) {
    current_choices <- isolate(input[[inputId]])
    if (!identical(sort(choices), sort(current_choices)) || !identical(selected, input[[inputId]])) {
      updateSelectizeInput(session, inputId, choices = choices, selected = selected, server = TRUE)
    }
  }
  
  safe_update_select <- function(inputId, choices, selected) {
    current_choices <- isolate(input[[inputId]])
    if (!identical(choices, current_choices) || !identical(selected, input[[inputId]])) {
      updateSelectInput(session, inputId, choices = choices, selected = selected)
    }
  }
  
  # --- Reactive filtered dataset ---
  filtered_data <- reactive({
    df <- full_data_cleaned
    for(col in c("Pathogen","Disease","Host","Country","Year")) {
      val <- filter_state[[col]]
      if(!is.null(val) && val != "All") df <- df[!is.na(df[[col]]) & df[[col]]==val, ]
    }
    if(nrow(df) == 0) df <- full_data_cleaned
    df
  })
  
  # --- Pathogen / Disease / Host (A-Z + select) ---
  for(col in c("Pathogen","Disease","Host")) {
    local({
      this_col <- col
      
      # --- Reactive update whenever filtered_data changes ---
      observe({
        df <- filtered_data()
        
        # --- Update available letters for A-Z dropdown ---
        vals <- unique(na.omit(df[[this_col]]))
        letters_available <- c("All", "#", LETTERS[LETTERS %in% toupper(substr(vals,1,1))])
        selected_letter <- input[[paste0("letter_", this_col)]]
        if(is.null(selected_letter) || !selected_letter %in% letters_available) selected_letter <- "All"
        updateSelectInput(session, paste0("letter_", this_col),
                          choices = letters_available, selected = selected_letter)
        
        # --- Update category select based on selected letter ---
        choices <- c("All", get_filtered_choices(df, this_col, selected_letter))
        selected_val <- filter_state[[this_col]]
        if(!selected_val %in% choices) selected_val <- "All"
        safe_update_selectize(paste0("input_", this_col), choices, selected_val)
        session$sendCustomMessage("blur_selectize", list(inputId = paste0("input_", this_col)))
      })
      
      # --- Observe user selection changes ---
      observeEvent(input[[paste0("input_", this_col)]], {
        filter_state[[this_col]] <- input[[paste0("input_", this_col)]]
      }, ignoreInit=TRUE, ignoreNULL=TRUE)
      
      observeEvent(input[[paste0("letter_", this_col)]], {
        df <- filtered_data()
        letter <- input[[paste0("letter_", this_col)]]
        choices <- c("All", get_filtered_choices(df, this_col, letter))
        selected_val <- filter_state[[this_col]]
        if(!selected_val %in% choices) selected_val <- "All"
        safe_update_selectize(paste0("input_", this_col), choices, selected_val)
      }, ignoreInit=TRUE, ignoreNULL=TRUE)
    })
  }
  
  # --- Country dropdown ---
  observe({
    df <- filtered_data()
    valid_vals <- sort(unique(na.omit(df$Country)))
    mappable <- valid_vals[df$MapStatus[match(valid_vals, df$Country)]=="Mappable"]
    unmappable <- setdiff(valid_vals, mappable)
    
    choices_list <- list(list(value="All", label="All"))
    if(length(mappable)>0) {
      choices_list <- c(choices_list,
                        list(list(optgroup="Mappable Countries",
                                  options=lapply(mappable, function(x) list(value=x, label=x)))))
    }
    if(length(unmappable)>0) {
      choices_list <- c(choices_list,
                        list(list(optgroup="Unmappable / General Regions",
                                  options=lapply(unmappable, function(x) list(value=x, label=x)))))
    }
    
    selected_val <- filter_state$Country
    if(!selected_val %in% valid_vals) selected_val <- "All"
    
    session$sendCustomMessage("update_country_choices",
                              list(choices=choices_list, selected=selected_val))
  })
  
  observeEvent(input$input_Country, {
    filter_state$Country <- input$input_Country
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  # --- Year dropdown ---
  observe({
    df <- filtered_data()
    valid_vals <- sort(unique(na.omit(df$Year)))
    choices <- c("All", valid_vals)
    
    selected_val <- filter_state$Year
    if(!selected_val %in% choices) selected_val <- "All"
    
    safe_update_select("input_Year", choices, selected_val)
  })
  
  observeEvent(input$input_Year, {
    filter_state$Year <- input$input_Year
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  # --- Banner ---
  output$banner_ui <- renderUI({
    df <- filtered_data()
    n <- nrow(df)
    
    filter_texts <- sapply(filter_columns, function(col){
      val <- filter_state[[col]]
      lbl <- if(!is.null(val) && val!="All") 
        paste0("<strong>",val,"</strong>") 
      else "<strong>All</strong>"
      paste0(col, ": ", lbl)
    })
    
    # --- If exactly one record, populate actual values in placeholders ---
    if(n == 1) {
      rec <- df[1, ]
      filter_texts <- sapply(filter_columns, function(col){
        paste0(col, ": <strong>", rec[[col]], "</strong>")
      })
    }
    
    count_text <- paste0(
      "<span style='background-color:#FFFF00; padding:2px 4px; border-radius:4px; font-weight:bold; font-size:1.1em;'>",
      "Showing ", n, " record", ifelse(n == 1, "", "s"),
      "</span>"
    )
    
    banner_text <- if(n==0) {
      "No records available for current selection" 
    } else { 
      paste0(count_text, " for ", paste(filter_texts, collapse="; "))
    }
    
    tags$div(
      class="card",
      #style = "padding:5px 10px; margin-top:5px; margin-bottom:10px;",  # compact padding & margin
      style = "border: 1px solid #d1e7dd; background-color: #e9f7ef; padding:10px; margin-top:5px; margin-bottom:10px; border-radius:8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
      tags$h3("FILTER SUMMARY",
              style = "font-weight:bold;color:#0f5132;text-align:center;margin:2px 0; padding:0;"),
      HTML(paste0(
        "<p style='font-weight:500;color:#0f5132;text-align:center;margin:2px 0; padding:0;'>",
        banner_text,
        "</p>"
      ))
    )
  })
  
  # --- Precomputed map ---
  precomputed_map <- reactive({
    df <- df2_cleaned
    selected_year <- filter_state$Year
    
    if(selected_year == "All") {
      # Default map: all years
      df_summary <- df %>%
        group_by(Country) %>%
        summarise(
          RecordCount_all = sum(RecordCount, na.rm = TRUE),
          Z_value = first(Z_all_years_clamped, default = 0),
          MapStatus = first(MapStatus, default = "Unmappable"),
          .groups = "drop"
        )
    } else {
      # Map for selected year
      df_summary <- df %>%
        filter(Year == selected_year) %>%
        group_by(Country) %>%
        summarise(
          RecordCount_all = sum(RecordCount, na.rm = TRUE),
          Z_value = first(Z_by_year_clamped, default = 0),
          MapStatus = first(MapStatus, default = "Unmappable"),
          .groups = "drop"
        )
    }
    
    # Join to world map
    map_sf <- world %>%
      left_join(df_summary, by = c("name_long" = "Country"))
    
    # Fill NAs
    map_sf$RecordCount_all[is.na(map_sf$RecordCount_all)] <- 0
    map_sf$Z_value[is.na(map_sf$Z_value)] <- 0
    map_sf$MapStatus[is.na(map_sf$MapStatus)] <- "Unmappable"
    
    map_sf
  })
  
  # --- Reactive filtered map ---
  filtered_map <- reactive({
    map_sf <- precomputed_map()  # already has Z_value, RecordCount_all, MapStatus
    
    selected_country <- filter_state$Country
    any_filter <- filter_state$Pathogen != "All" || filter_state$Disease != "All" || filter_state$Host != "All"
    
    # --- recompute counts for the active filtered dataset ---
    filtered_df <- filtered_data()
    counts <- filtered_df %>%
      count(Country, name = "RecordCount")
    
    # join counts to map, fallback to 0 if missing
    map_sf <- map_sf %>%
      left_join(counts, by = c("name_long" = "Country")) %>%
      mutate(RecordCount = ifelse(is.na(RecordCount), 0, RecordCount))
    
    # Fixed palette and breaks for Z-scores
    z_palette <- rev(hcl.colors(7, "Greens 3"))  # 7 colors for -3:3
    z_breaks <- seq(-3, 3, length.out = length(z_palette) + 1)
    missing_color <- "#ffdada"  # light red for missing/unmappable
    
    map_sf <- map_sf %>% mutate(
      fillColor = case_when(
        # --- Priority 1: Single country selected ---
        selected_country != "All" ~ if_else(name_long == selected_country, "blue", missing_color),
        
        # --- Priority 2: Any Pathogen/Disease/Host filter ---
        any_filter & MapStatus == "Mappable" ~ if_else(name_long %in% filtered_data()$Country, "blue", missing_color),
        
        # Priority 3: Z-score coloring for mapped countries
        MapStatus == "Mappable" ~ z_palette[cut(pmin(pmax(Z_value, -3), 3), breaks = z_breaks, include.lowest = TRUE)],
        
        # Default: unmappable
        TRUE ~ missing_color
      ),
      fillOpacity = ifelse(fillColor == missing_color, 0.5, 0.7)
    )
    
    # Debugging: count of countries per branch
    #debug_counts <- map_sf %>%
    #  mutate(branch = case_when(
    #    fillColor == "blue" ~ "blue",
    #    fillColor == missing_color ~ "unmappable",
    #    TRUE ~ "zscore"
    #  )) %>%
    #  group_by(branch) %>%
    #  summarise(n = n(), .groups = "drop")
    #print(paste0("Branch counts for selection (year=", filter_state$Year,
    #             ", Pathogen=", filter_state$Pathogen,
    #             ", Disease=", filter_state$Disease,
    #             ", Host=", filter_state$Host, "):"))
    #print(debug_counts)
    
    map_sf
  })
  
  # --- Leaflet base ---
  output$map <- renderLeaflet({
    leaflet(world, options = leafletOptions(zoomControl = TRUE, scrollWheelZoom = FALSE)) %>% 
      addTiles() %>% 
      setView(lng = 0, lat = 26, zoom = 2) %>%
      addFullscreenControl(pseudoFullscreen = TRUE, position = "topright") %>% 
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        // Move zoom controls to top-right
        map.zoomControl.setPosition('topright');

        // instruction overlay
        var info = document.createElement('div');
        info.id = 'map-scroll-info';
        info.innerHTML = 'Hold <b>Ctrl</b> + scroll to zoom map';
        info.style.cssText = 'position:absolute;top:95%;left:50%;transform:translate(-50%, -50%);background:white;padding:5px 8px;border:1px solid #999;border-radius:4px;font-size:12px;opacity:0.8;z-index:1000;';
        map.getContainer().appendChild(info);

        // persistent wheel listener
        map.scrollWheelZoom.disable();
        
        map.getContainer().addEventListener('wheel', function(e) {
          if(e.ctrlKey) {
            e.preventDefault();               // prevent page scroll
            map.scrollWheelZoom._enabled || map.scrollWheelZoom.enable();
          } else {
            map.scrollWheelZoom._enabled && map.scrollWheelZoom.disable();
          }
        }, { passive: false });
      }
    ")
  })
  
  # --- Leaflet observer ---
  observe({
    map_sf <- filtered_map()
    
    leafletProxy("map", data = map_sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addFullscreenControl(pseudoFullscreen = TRUE, position = "topleft") %>%
      addPolygons(
        fillColor = ~fillColor,
        fillOpacity = ~fillOpacity,
        weight = 0.5,
        color = "#444444",
        label = ~paste0("<b>", name_long, "</b><br/>", "Record(s): ", RecordCount) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      )
    
    # --- Dynamic Z-score Legend ---
    show_z_legend <- !((filter_state$Pathogen != "All") || 
                       (filter_state$Disease  != "All") || 
                       (filter_state$Host     != "All") ||
                       (filter_state$Country  != "All")) 
    
    if(show_z_legend) {
      legend_title <- if(filter_state$Year != "All") {
        paste0("Z-score (", filter_state$Year, ")")
      } else {
        "Z-score (all years)"
      }
      
      z_vals <- -3:3
      z_colors <- rev(hcl.colors(length(z_vals), "Greens 3"))
#      legend_colors <- c(z_colors, "#ffdada")  # add missing color
#      legend_labels <- c(as.character(z_vals), "No data")

      # Build HTML for the legend (initially hidden)
      legend_html <- paste0(
        "<div id='legend-box' style='background:white;padding:5px;border:1px solid #999;border-radius:5px;'>",
        "<strong style='cursor:pointer;' onclick='var x=document.getElementById(\"legend-content\"); if(x.style.display==\"none\"){x.style.display=\"block\";} else{x.style.display=\"none\";}'>", 
        legend_title, "</strong>",
        "<div id='legend-content' style='display:none;margin-top:5px;'>",
        paste0(
          "<div style='display:flex;align-items:center;'>",
          paste0(
            "<div style='width:20px;height:20px;margin-right:3px;background:", z_colors, ";border:1px solid #444;'></div>",
            "<span style='margin-right:5px;'>", z_vals, "</span>",
            collapse=""
          ),
          "</div>"
        ),
        "<div style='display:flex;align-items:center;'><div style='width:20px;height:20px;margin-right:3px;background:#ffdada;border:1px solid #444;'></div><span>No data</span></div>",
        "</div></div>"
      )
      
      leafletProxy("map") %>% addControl(html = legend_html, position = "bottomleft")
    }
  })
  
  # --- Reset ---
  observeEvent(input$reset_btn,{
    log_interaction(session, "RESET_FILTERS", "User reset all filters")
    
    lapply(filter_columns,function(col) filter_state[[col]]<-"All")
    
    updateSelectInput(session,"letter_Pathogen",selected="All")
    updateSelectizeInput(session,"input_Pathogen",selected="All",choices=c("All"),server=TRUE)
    
    updateSelectInput(session,"letter_Disease",selected="All")
    updateSelectizeInput(session,"input_Disease",selected="All",choices=c("All"),server=TRUE)
    
    updateSelectInput(session,"letter_Host",selected="All")
    updateSelectizeInput(session,"input_Host",selected="All",choices=c("All"),server=TRUE)
    
    updateSelectInput(session,"input_Year",selected="All")
    updateSelectizeInput(session,"input_Country",selected="All")
  })
  
  # --- Data table ---
  output$filtered_table <- renderDT({
    df <- filtered_data()
    remove_cols <- c("Pathogen", "Host", "Disease", "Info", "State", "Country", "Year", "name_long", "MapStatus", 
                     "RecordCount", "Z_all_years", "Z_all_years_clamped", "Z_by_year", "Z_by_year_clamped")
    df <- df %>% select(-intersect(names(df),remove_cols))
    datatable(df, options=list(pageLength=10,scrollX=TRUE))
  })
}

# --- Run App ---
shinyApp(ui, server)
