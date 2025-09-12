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
    ")),
    tags$style(HTML("
      body { font-family: 'Segoe UI', sans-serif; background-color: #f8f9fa; }
      .main-title { font-size: 48px; font-weight: bold; color: #0f5132; padding:5px 0 5px 0; margin-bottom: 0; }
      .subtitle { font-size: 20px; color: #6c757d; margin: 0; padding: 0; }
      .card { border: 1px solid #d1e7dd; background-color: #e9f7ef; padding:10px; margin-bottom: 15px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
      .sidebar { background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 1px 4px rgba(0,0,0,0.1); }
      .dataTables_wrapper { background-color: #ffffff; padding: 10px; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.1); }
      .leaflet-container { border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.1); }
      
      /* --- Responsive adjustments for smaller screens --- */
      @media (max-width: 768px) {
        .main-title { font-size: 28px; }
        .subtitle { font-size: 16px; }
        .sidebar { padding: 10px; margin-bottom: 15px; }
        .leaflet-container { height: 300px !important; }
      }
    "))
  ),
  
  # --- App Title & Subtitle ---
  div(
    style = "text-align:center; margin-top:-20px;",
    h1("Robigus", class="main-title"),
    p("An initiative from the UF/IFAS Department of Plant Pathology to catalog and map plant diseases globally", class="subtitle"),
    h6("Plant Disease Notes (1980 - 2024) published by American Phytopathological Society Press", style="color: black; margin-top:5px;")
  ),
  
  uiOutput("banner_ui"),
  
  # --- Responsive Layout ---
  fluidRow(
    # Sidebar: full width on small screens, 4/12 on medium+
    column(
      width = 12, class = "col-md-4",
      div(class = "sidebar",
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
            tags$img(src = "logo.png", height = "60px", style = "float: right; margin-left: 10px; border-radius: 3px;"),
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
      else vals <- vals[grepl(paste0("^", letter), vals, ignore.case=TRUE)]
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
             style = "padding:5px 10px; margin-top:5px; margin-bottom:10px;",  # compact padding & margin
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
    leaflet(world) %>% addTiles() %>% setView(lng = 0, lat = 20, zoom = 2)
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
        #label = ~paste0(name_long, ": ", RecordCount_all, " record(s)"),
        label = ~paste0(
          "<b>", name_long, "</b><br/>",
          "Record(s): ", RecordCount  # <-- use dynamic field
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      )
    
    # --- Legend ---
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
      legend_colors <- c(z_colors, "#ffdada")  # add missing color
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
