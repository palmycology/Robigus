# --- app.R (Optimized, no flicker. Use MapStatus column. With interaction logging) ---

# --- Load logger ---
source("scripts/logger.R")
init_logger()

# --- Load libraries ---
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(spData)
library(leaflet)
library(DT)
library(stringr)
library(shinythemes)

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
    # --- Custom JS & CSS ---
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

      // Force Leaflet map to resize & refit after load
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          if (window.myLeafletMap) {
            window.myLeafletMap.invalidateSize();
            window.myLeafletMap.fitBounds(window.myLeafletMap.getBounds());
          }
        }, 500);
      });
    ")),
    tags$style(HTML("
      body { font-family: 'Segoe UI', sans-serif; background-color: #f8f9fa; }

      .main-title { font-size: 48px; font-weight: bold; color: #0f5132;
                    padding:5px 0 5px 0; margin-bottom: 0; }
      .subtitle   { font-size: 20px; color: #6c757d; margin: 0; padding: 0; }

      .card, .sidebar, .dataTables_wrapper, .leaflet-container {
        border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.1);
      }
      .card   { border: 1px solid #d1e7dd; background-color: #e9f7ef;
                padding:10px; margin-bottom: 15px; }
      .sidebar { background-color: #ffffff; padding: 15px; }

      .dataTables_wrapper { background-color: #ffffff; padding: 10px; }
      .leaflet-container  { border: 1px solid #ddd; }

      /* Logo responsiveness */
      .app-logo {
        max-width: 100%;
        height: auto;
        border-radius: 3px;
        margin-bottom: 10px;
      }

      /* Legend defaults */
      .leaflet-control { font-size: 14px; }

      /* --- Responsive adjustments for smaller screens --- */
      @media (max-width: 768px) {
        body { font-size: 12px; }
        .main-title { font-size: 28px; }
        .subtitle   { font-size: 16px; }
        h4 { font-size: 16px; }
        .sidebar { padding: 10px; margin-bottom: 15px; }
        .leaflet-container { height: 300px !important; }

        /* Legend tweaks: hide legend completely on narrow screens */
        .leaflet-control-layers,
        .leaflet-bottom.leaflet-right {
          display: none !important;
        }
      }
    "))
  ),
  
  # --- App Title & Subtitle ---
  div(
    class = "title-block",
    style = "text-align:center; margin-top:-20px;",
    h1("Robigus", class="main-title"),
    p("An initiative from the UF/IFAS Department of Plant Pathology to catalog and map plant diseases globally",
      class="subtitle"),
    h6("Plant Disease Notes (1980 - 2024) published by American Phytopathological Society Press",
       style="color: black; margin-top:5px;")
  ),
  
  uiOutput("banner_ui"),
  
  # --- Responsive Layout ---
  fluidRow(
    # Sidebar: full width on small screens, 4/12 on medium+
    column(
      width = 12, class = "col-md-4",
      div(class = "sidebar",
          # Logo on top of sidebar
          #tags$img(src = "logo.png", class = "app-logo"),
          
          # --- Filter Controls ---
          fluidRow(
            column(4, selectInput("letter_Pathogen", "Pathogen: A-Z", 
                                  choices = c("All","#",LETTERS), selected = "All")),
            column(8, selectInput("input_Pathogen", "Select Pathogen", 
                                  choices = c("All"), selected = "All"))
          ),
          fluidRow(
            column(4, selectInput("letter_Disease", "Disease: A-Z", 
                                  choices = c("All","#",LETTERS), selected = "All")),
            column(8, selectInput("input_Disease", "Select Disease", 
                                  choices = c("All"), selected = "All"))
          ),
          fluidRow(
            column(4, selectInput("letter_Host", "Host: A-Z", 
                                  choices = c("All","#",LETTERS), selected = "All")),
            column(8, selectInput("input_Host", "Select Host", 
                                  choices = c("All"), selected = "All"))
          ),
          selectizeInput(
            "input_Country", "Select Country", choices = NULL, selected = "All", width = "100%",
            options = list(
              placeholder = "All", 
              labelField = "label", 
              valueField = "value", 
              optgroupField = "optgroup"
            )
          ),
          selectInput("input_Year", "Select Year", 
                      choices = c("All"), selected = "All", width = "100%"),
          tags$div(style="margin-top:15px;", 
                   actionButton("reset_btn", "Reset All Filters", 
                                class="btn btn-outline-primary")),
          
          # --- About Panel ---
          hr(),
          wellPanel(
            style = "
                background-color: #e6f7e6;
                border: 1px solid #b7dfb7;
                border-radius: 12px;
                padding: 12px 15px;
                box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
                text-align: left;
              ",
            tags$img(src = "logo.png", height = "60px",
                     style = "float: right; margin-left: 10px; border-radius: 3px;"),
            h4("About this App"),
            tags$p(HTML("<b>Creator:</b> ,<br>Assistant Professor")),
            tags$p(HTML("<b>Affiliation:</b><br>
                        University of Florida<br>
                        Institute of Food and Agricultural Sciences<br>
                        Department of Plant Pathology<br>
                        Fort Lauderdale Research and Education Center<br>
                        Davie, FL")),
            tags$p(HTML("<b>Contact:</b> <br> "))
          )
      )
    ),
    
    # Main content: full width on small screens, 8/12 on medium+
    column(
      width = 12, class = "col-md-8",
      leafletOutput("map", height = 500),
      br(),
      DTOutput("filtered_table")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # --- Log session start ---
  log_interaction(session, "SESSION_START", "User connected")
  
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
  
  # --- Letter filtering ---
  get_filtered_choices <- function(df, col, letter) {
    vals <- unique(na.omit(df[[col]]))
    if(!is.null(letter) && letter != "All") {
      if(letter=="#") vals <- vals[grepl("^[^A-Za-z]", vals)]
      else vals <- grepl(paste0("^", letter), vals, ignore.case=TRUE)
    }
    sort(as.character(vals))
  }
  
  # --- Dynamic dropdowns ---
  for(col in c("Pathogen","Disease","Host")) {
    local({
      this_col <- col
      observeEvent(input[[paste0("letter_", this_col)]], {
        log_interaction(session, "LETTER_FILTER", paste0(this_col, " letter = ", input[[paste0("letter_", this_col)]]))
        
        df <- filtered_data()
        choices <- c("All", get_filtered_choices(df, this_col, input[[paste0("letter_", this_col)]]))
        selected_val <- filter_state[[this_col]]
        if(!selected_val %in% choices) selected_val <- "All"
        updateSelectizeInput(session, paste0("input_", this_col), choices=choices, selected=selected_val, server=TRUE)
      }, ignoreInit=TRUE)
    })
  }
  
  # --- Country dropdown ---
  observe({
    df <- filtered_data()
    valid_vals <- sort(unique(na.omit(df$Country)))
    mappable <- valid_vals[df$MapStatus[match(valid_vals, df$Country)]=="Mappable"]
    unmappable <- setdiff(valid_vals, mappable)
    
    choices_list <- list(list(value="All", label="All"))
    if(length(mappable)>0) choices_list <- c(choices_list, list(list(optgroup="Mappable Countries", options=lapply(mappable,function(x) list(value=x,label=x)))))
    if(length(unmappable)>0) choices_list <- c(choices_list, list(list(optgroup="Unmappable / General Regions", options=lapply(unmappable,function(x) list(value=x,label=x)))))
    
    selected_val <- filter_state$Country
    if(!selected_val %in% valid_vals) selected_val <- "All"
    session$sendCustomMessage("update_country_choices", list(choices=choices_list, selected=selected_val))
  })
  
  # --- Year dropdown ---
  observe({
    df <- filtered_data()
    valid_vals <- sort(unique(na.omit(df$Year)))
    choices <- c("All", valid_vals)
    selected_val <- filter_state$Year
    if(!selected_val %in% choices) selected_val <- "All"
    updateSelectInput(session, "input_Year", choices=choices, selected=selected_val)
  })
  
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
    
    tags$div(class="card",
             style = "padding:5px 10px; margin-top:5px; margin-bottom:10px;",
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
      df_summary <- df %>%
        group_by(Country) %>%
        summarise(
          RecordCount_all = sum(RecordCount, na.rm = TRUE),
          Z_value = first(Z_all_years_clamped, default = 0),
          MapStatus = first(MapStatus, default = "Unmappable"),
          .groups = "drop"
        )
    } else {
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
    
    map_sf <- world %>%
      left_join(df_summary, by = c("name_long" = "Country"))
    
    map_sf$RecordCount_all[is.na(map_sf$RecordCount_all)] <- 0
    map_sf$Z_value[is.na(map_sf$Z_value)] <- 0
    map_sf$MapStatus[is.na(map_sf$MapStatus)] <- "Unmappable"
    
    map_sf
  })
  
  # --- Reactive filtered map ---
  filtered_map <- reactive({
    map_sf <- precomputed_map()
    
    selected_country <- filter_state$Country
    any_filter <- filter_state$Pathogen != "All" || filter_state$Disease != "All" || filter_state$Host != "All"
    
    filtered_df <- filtered_data()
    counts <- filtered_df %>%
      count(Country, name = "RecordCount")
    
    map_sf <- map_sf %>%
      left_join(counts, by = c("name_long" = "Country")) %>%
      mutate(RecordCount = ifelse(is.na(RecordCount), 0, RecordCount))
    
    z_palette <- rev(hcl.colors(7, "Greens 3"))
    z_breaks <- seq(-3, 3, length.out = length(z_palette) + 1)
    missing_color <- "#ffdada"
    
    map_sf <- map_sf %>% mutate(
      fillColor = case_when(
        selected_country != "All" ~ if_else(name_long == selected_country, "blue", missing_color),
        any_filter & MapStatus == "Mappable" ~ if_else(name_long %in% filtered_data()$Country, "blue", missing_color),
        MapStatus == "Mappable" ~ z_palette[cut(pmin(pmax(Z_value, -3), 3), breaks = z_breaks, include.lowest = TRUE)],
        TRUE ~ missing_color
      ),
      fillOpacity = ifelse(fillColor == missing_color, 0.5, 0.7)
    )
    
    map_sf
  })
  
  # --- Leaflet base: Mobile-friendly initial view ---
  output$map <- renderLeaflet({
    leaflet(df2_cleaned) %>% addTiles() %>%
      {
        if (session$clientData$output_map_width < 768) {
          setView(., lng = 0, lat = 20, zoom = 2)  # Mobile: show world
        } else {
          fitBounds(.,
                    lng1 = min(df2_cleaned$lon, na.rm = TRUE),
                    lat1 = min(df2_cleaned$lat, na.rm = TRUE),
                    lng2 = max(df2_cleaned$lon, na.rm = TRUE),
                    lat2 = max(df2_cleaned$lat, na.rm = TRUE)
          )
        }
      } %>%
      htmlwidgets::onRender("function(el, x) { window.myLeafletMap = this; }")
  })
  
  # --- Leaflet observer ---
  observe({
    map_sf <- filtered_map()
    
    leafletProxy("map", data = map_sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = map_sf,
        fillColor = ~fillColor,
        fillOpacity = ~fillOpacity,
        weight = 0.5,
        color = "#444444",
        label = ~paste0("<b>", name_long, "</b><br/>Record(s): ", RecordCount) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      )
    
    show_z_legend <- !((filter_state$Pathogen != "All") || 
                         (filter_state$Disease != "All") || 
                         (filter_state$Host != "All"))
    
    if(show_z_legend) {
      legend_title <- if(filter_state$Year != "All") {
        paste0("Z-score (", filter_state$Year, ")")
      } else {
        "Z-score (all years)"
      }
      
      z_vals <- -3:3
      z_colors <- rev(hcl.colors(length(z_vals), "Greens 3"))
      legend_colors <- c(z_colors, "#ffdada")
      legend_labels <- c(as.character(z_vals), "No data")
      
      leafletProxy("map") %>%
        addLegend(
          position = "bottomleft",
          colors = legend_colors,
          labels = legend_labels,
          opacity = 0.7,
          title = legend_title
        )
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
