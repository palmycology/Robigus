# --- app_v2.R ( UPDATES: Using shinyWidgets now
#                Added multiple select options for four filters Pathogen/Host/Disease/Country      
#                When multiple values are selected, zscore is computed on the fly                   ) ---

# --- Load logger ---
source("scripts/logger.R")
init_logger()

# --- Load libraries ---
library(shiny)
library(shinythemes)
library(shinyWidgets)
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


library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(leaflet)

ui <- fluidPage(
  theme = shinytheme("readable"),
  
  tags$head(
    # --- JS handlers (keep existing behavior) ---
    tags$script(HTML("
      Shiny.addCustomMessageHandler('blur_selectize', function(message) {
        var select = $('#' + message.inputId).selectize()[0].selectize;
        select.blur();  // remove focus so dropdown closes
      });

      document.addEventListener('DOMContentLoaded', function() {
        var container = document.getElementById('infoContainer');
        var tab = document.getElementById('infoTab');
        tab.textContent = '✕';
        tab.classList.remove('hamburger');

        tab.onclick = function(e) {
          container.classList.toggle('collapsed');
          if(container.classList.contains('collapsed')) {
            tab.textContent = '≡';
            tab.classList.add('hamburger');
          } else {
            tab.textContent = '✕';
            tab.classList.remove('hamburger');
          }
        };
      });
    ")),
    
    # --- CSS (keep your styles) ---
    tags$style(HTML("
      body { font-family: 'Segoe UI', sans-serif; background-color: #f8f9fa; }
      .main-title { font-size: 48px; font-weight: bold; color: #0f5132; padding:5px 0; margin-bottom: 0; }
      .subtitle { font-size: 20px; color: #6c757d; margin: 0; padding: 0; }
      .filter-bar { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 5px; position: relative; z-index: 10001; transition: margin-left 0.3s ease; }
      #infoContainer:not(.collapsed) ~ .filter-bar { margin-left: 330px; }
      #infoContainer.collapsed ~ .filter-bar { margin-left: 10px; }
      .filter-item { flex: 1 1 200px; min-width: 180px; }
      #infoContainer { position: fixed; top: 120px; left: 0; z-index: 9999; }
      #infoBox { width: 320px; height: 520px; background: rgba(240,240,240,0.85); border: none; border-radius: 0 8px 8px 0; padding: 10px; box-shadow: 2px 2px 6px rgba(0,0,0,0.2); transition: transform 0.3s ease; position: relative; left: 0; }
      #infoBox .content { font-size: 13px; opacity: 1; transition: opacity 0.2s ease; }
      #infoTab { width: 30px; height: 60px; background: rgba(240,240,240,0.9); border: none; border-radius: 0 4px 4px 0; position: fixed; top: 150px; left: 320px; display: flex; justify-content: center; align-items: center; font-size: 18px; cursor: pointer; user-select: none; transition: left 0.3s ease; z-index: 3; }
      #infoTab.collapsed, infoTab.open { font-family: 'Arial', sans-serif; font-weight: 400; font-size: 18px; }
      #infoTab.hamburger { letter-spacing: 4px; font-weight: 700; }
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
  
  # --- FILTER BAR ---
  div(class = "filter-bar",
      
      # Pathogen box
      div(class="filter-item",
          selectInput("letter_Pathogen", "Pathogen: A-Z", choices = c("All","#",LETTERS), selected = "All"),
          pickerInput("input_Pathogen", "Select Pathogen", 
                      choices = c("All"), 
                      selected = "All", multiple = TRUE, 
                      options = list(`actions-box`=TRUE, `live-search`=TRUE))
      ),
      
      # Disease box
      div(class="filter-item",
          selectInput("letter_Disease", "Disease: A-Z", choices = c("All","#",LETTERS), selected = "All"),
          pickerInput("input_Disease", "Select Disease", 
                      choices = c("All"), 
                      selected = "All", multiple = TRUE, 
                      options = list(`actions-box`=TRUE, `live-search`=TRUE))
      ),
      
      # Host box
      div(class="filter-item",
          selectInput("letter_Host", "Host: A-Z", choices = c("All","#",LETTERS), selected = "All"),
          pickerInput("input_Host", "Select Host", 
                      choices = c("All"), 
                      selected = "All", multiple = TRUE, 
                      options = list(`actions-box`=TRUE, `live-search`=TRUE))
      ),
      
      # Country and Year
      div(class="filter-item",
          pickerInput("input_Country", "Select Country", 
                      choices = c("All"), 
                      selected = "All", multiple = TRUE, 
                      options = list(`actions-box`=TRUE, `live-search`=TRUE)),
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
  
  # --- Initialize info box ---
  observe({
    session$sendCustomMessage("initInfoBox", list())
  })
  
  # --- Log session end ---
  session$onSessionEnded(function() {
    log_interaction(session, "SESSION_END", "User disconnected")
  })
  
  # --- Reactive filter state ---
  filter_state <- reactiveValues(Pathogen="All", Disease="All", Host="All", Country="All", Year="All")
  
  # --- Update filter_state on picker changes ---
  lapply(filter_columns, function(col) {
    observeEvent(input[[paste0("input_", col)]], { 
      selected <- input[[paste0("input_", col)]]
      if(is.null(selected) || length(selected) == 0) selected <- "All"
      filter_state[[col]] <- selected
      log_interaction(session, "FILTER_CHANGE", paste0(col, " = ", paste(selected, collapse=",")))
    }, ignoreInit=TRUE)
  })
  
  # --- Reactive: filter data based on filter_state ---
  filtered_data <- reactive({
    df <- full_data_cleaned
    for(col in filter_columns){
      val <- filter_state[[col]]
      if(!is.null(val) && !("All" %in% val)){
        df <- df[!is.na(df[[col]]) & df[[col]] %in% val, ]
      }
    }
    if(nrow(df) == 0) df <- full_data_cleaned
    df
  })
  
  filtered_data_no_country <- reactive({
    
    df <- full_data_cleaned
    
    # Apply Pathogen filter
    if (!("All" %in% filter_state$Pathogen)) {
      df <- df %>% filter(Pathogen %in% filter_state$Pathogen)
    }
    
    # Apply Disease filter
    if (!("All" %in% filter_state$Disease)) {
      df <- df %>% filter(Disease %in% filter_state$Disease)
    }
    
    # Apply Host filter
    if (!("All" %in% filter_state$Host)) {
      df <- df %>% filter(Host %in% filter_state$Host)
    }
    
    # Apply Year filter (keep this — matches your hierarchy rules)
    if (filter_state$Year != "All") {
      df <- df %>% filter(Year == filter_state$Year)
    }
    
    df
  })
  
  # --- Helper: filter choices by letter ---
  get_filtered_choices <- function(df, col, letter){
    vals <- unique(na.omit(df[[col]]))
    if(!is.null(letter) && letter != "All"){
      if(letter == "#") vals <- vals[grepl("^[^A-Za-z]", vals)]
      else vals <- vals[grepl(paste0("^", letter), vals, ignore.case=TRUE)]
    }
    sort(as.character(vals))
  }
  
  # --- Reactive: update Pathogen/Disease/Host letters and pickerInput choices ---
  for(col in c("Pathogen","Disease","Host")) {
    local({
      this_col <- col
      other_cols <- setdiff(c("Pathogen","Disease","Host"), this_col)
      
      # --- Observe letters and update current picker choices ---
      observe({
        all_vals <- unique(na.omit(full_data_cleaned[[this_col]]))  # ALWAYS full data
        selected_letter <- input[[paste0("letter_", this_col)]]
        
        if(selected_letter == "All") {
          choices <- sort(all_vals)
        } else if(selected_letter == "#") {
          choices <- sort(all_vals[grepl("^[^A-Za-z]", all_vals)])
        } else {
          choices <- sort(all_vals[grepl(paste0("^", selected_letter), all_vals, ignore.case=TRUE)])
        }
        
        choices <- c("All", choices)
        
        # Keep selected values that are still valid
        selected_val <- filter_state[[this_col]]
        selected_val <- selected_val[selected_val %in% choices]
        if(length(selected_val) == 0) selected_val <- "All"
        
        updatePickerInput(session, paste0("input_", this_col),
                          choices = choices,
                          selected = selected_val,
                          options=list(`actions-box`=TRUE, `live-search`=TRUE))
      })
      
      # --- Observe selection for current column ---
      observeEvent(input[[paste0("input_", this_col)]], {
        selected_val <- input[[paste0("input_", this_col)]]
        if("All" %in% selected_val && length(selected_val) > 1) {
          selected_val <- setdiff(selected_val, "All")
        }
        if(length(selected_val) == 0) selected_val <- "All"
        filter_state[[this_col]] <- selected_val
      }, ignoreInit=TRUE)
      
      # --- Apply hierarchical filtering to other columns ---
      observe({
        df_hier <- filtered_data()  # This applies Pathogen/Disease/Host filters
        for(other_col in other_cols){
          other_vals <- unique(na.omit(df_hier[[other_col]]))
          other_selected <- filter_state[[other_col]]
          new_selected <- other_selected[other_selected %in% c("All", other_vals)]
          if(length(new_selected) == 0) new_selected <- "All"
          
          updatePickerInput(session, paste0("input_", other_col),
                            choices = c("All", sort(other_vals)),
                            selected = new_selected,
                            options=list(`actions-box`=TRUE, `live-search`=TRUE))
        }
      })
      
    })
  }
  
  # --- Country pickerInput with proper optgroups ---
  observe({
    df <- filtered_data_no_country()
    
    valid_vals <- sort(unique(na.omit(df$Country)))
    
    mappable <- valid_vals[df$MapStatus[match(valid_vals, df$Country)] == "Mappable"]
    unmappable <- setdiff(valid_vals, mappable)
    
    # Build choices with optgroups
    choices <- list()
    
    if (length(mappable) > 0) {
      choices[["Mappable Countries"]] <- mappable
    }
    
    if (length(unmappable) > 0) {
      choices[["Unmappable / General Regions"]] <- unmappable
    }
    
    # Always include All
    choices <- c("All" = "All", choices)
    
    # Preserve current selection
    selected_val <- filter_state$Country
    selected_val <- selected_val[selected_val %in% unlist(choices)]
    
    if (length(selected_val) == 0) {
      selected_val <- "All"
    }
    
    updatePickerInput(
      session,
      "input_Country",
      choices = choices,
      selected = selected_val,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE
      )
    )
  })
  
  
  # --- Country selection logic (match Pathogen/Disease/Host behavior) ---
  observeEvent(input$input_Country, {
    
    selected_val <- input$input_Country
    
    # Remove "All" if real selections exist
    if ("All" %in% selected_val && length(selected_val) > 1) {
      selected_val <- setdiff(selected_val, "All")
    }
    
    # Restore All if empty
    if (is.null(selected_val) || length(selected_val) == 0) {
      selected_val <- "All"
    }
    
    filter_state$Country <- selected_val
    
  }, ignoreInit = TRUE)
  
  # --- Year filter ---
  observe({
    df <- filtered_data()
    valid_vals <- sort(unique(na.omit(df$Year)))
    choices <- c("All", valid_vals)
    selected_val <- filter_state$Year
    if(!selected_val %in% choices) selected_val <- "All"
    updateSelectInput(session,"input_Year",choices=choices,selected=selected_val)
  })
  
  observeEvent(input$input_Year,{
    filter_state$Year <- input$input_Year
  }, ignoreInit=TRUE)
  
  # --- Banner ---
  output$banner_ui <- renderUI({
    df <- filtered_data()
    n <- nrow(df)
    
    filter_texts <- sapply(filter_columns, function(col){
      val <- filter_state[[col]]
      if(!is.null(val) && !("All" %in% val)) lbl <- paste0("<strong>",paste(val, collapse=","),"</strong>")
      else lbl <- "<strong>All</strong>"
      paste0(col, ": ", lbl)
    })
    
    if(n==1){
      rec <- df[1,]
      filter_texts <- sapply(filter_columns, function(col){
        paste0(col, ": <strong>", rec[[col]], "</strong>")
      })
    }
    
    count_text <- paste0("<span style='background-color:#FFFF00; padding:2px 4px; border-radius:4px; font-weight:bold; font-size:1.1em;'>",
                         "Showing ", n, " record", ifelse(n==1,"","s"), "</span>")
    banner_text <- if(n==0) "No records available for current selection" else paste0(count_text, " for ", paste(filter_texts, collapse="; "))
    
    tags$div(class="card",
             style="border:1px solid #d1e7dd; background-color:#e9f7ef; padding:10px; margin-top:5px; margin-bottom:10px; border-radius:8px; box-shadow:0 1px 3px rgba(0,0,0,0.1);",
             tags$h3("FILTER SUMMARY", style="font-weight:bold;color:#0f5132;text-align:center;margin:2px 0; padding:0;"),
             HTML(paste0("<p style='font-weight:500;color:#0f5132;text-align:center;margin:2px 0; padding:0;'>",banner_text,"</p>"))
    )
  })
  
  # --- Precomputed map ---
  precomputed_map <- reactive({
    df <- df2_cleaned
    selected_year <- filter_state$Year
    
    if(selected_year=="All"){
      df_summary <- df %>% group_by(Country) %>% summarise(
        RecordCount_all=sum(RecordCount, na.rm=TRUE),
        Z_value=first(Z_all_years_clamped, default=0),
        MapStatus=first(MapStatus, default="Unmappable"), .groups="drop"
      )
    } else {
      df_summary <- df %>% filter(Year==selected_year) %>% group_by(Country) %>% summarise(
        RecordCount_all=sum(RecordCount, na.rm=TRUE),
        Z_value=first(Z_by_year_clamped, default=0),
        MapStatus=first(MapStatus, default="Unmappable"), .groups="drop"
      )
    }
    
    map_sf <- world %>% left_join(df_summary, by=c("name_long"="Country"))
    map_sf$RecordCount_all[is.na(map_sf$RecordCount_all)] <- 0
    map_sf$Z_value[is.na(map_sf$Z_value)] <- 0
    map_sf$MapStatus[is.na(map_sf$MapStatus)] <- "Unmappable"
    
    map_sf
  })
  
  # --- Filtered map ---
  filtered_map <- reactive({
    map_sf <- precomputed_map()
    selected_country <- filter_state$Country
    any_filter <- !("All" %in% filter_state$Pathogen) ||
      !("All" %in% filter_state$Disease) ||
      !("All" %in% filter_state$Host)
    
    filtered_df <- filtered_data()
    counts <- filtered_df %>% count(Country, name="RecordCount")
    
    map_sf <- map_sf %>%
      left_join(counts, by=c("name_long"="Country")) %>%
      mutate(RecordCount = ifelse(is.na(RecordCount), 0, RecordCount))
    
    z_palette <- rev(hcl.colors(7, "Greens 3"))
    z_breaks <- seq(-3,3,length.out=length(z_palette)+1)
    missing_color <- "#ffdada"
    
    # Determine which Z to use
    use_precomputed_all <- all(c("All" %in% filter_state$Pathogen,
                                 "All" %in% filter_state$Disease,
                                 "All" %in% filter_state$Host,
                                 "All" %in% filter_state$Country,
                                 filter_state$Year == "All"))
    
    use_precomputed_year <- all(c("All" %in% filter_state$Pathogen,
                                  "All" %in% filter_state$Disease,
                                  "All" %in% filter_state$Host,
                                  "All" %in% filter_state$Country,
                                  filter_state$Year != "All"))
    
    # Calculate dynamic Z only if filters other than Year/Country are applied
    map_sf <- map_sf %>%
      rowwise() %>%
      mutate(
        Z_value_dynamic = case_when(
          use_precomputed_all ~ Z_value,
          use_precomputed_year ~ Z_value,
          TRUE ~ {
            # Only consider mappable countries in filtered_df
            vals <- filtered_df$Country %>% table() %>% as.numeric()
            if(length(vals) > 1 && sd(vals) != 0){
              z <- (RecordCount - mean(vals)) / sd(vals)
              pmin(pmax(z, -3), 3)
            } else 0
          }
        )
      ) %>% ungroup()
    
    n_countries <- length(unique(filtered_df$Country))
    
    map_sf <- map_sf %>% mutate(
      
      fillColor = case_when(
        
        # ---- SINGLE COUNTRY FOCUS (blue highlight) ----
        n_countries == 1 & MapStatus=="Mappable" ~
          if_else(name_long %in% filtered_df$Country, "blue", missing_color),
        
        # ---- DYNAMIC Z (any Pathogen/Disease/Host OR Country filter active) ----
        (any_filter | !("All" %in% selected_country)) &
          MapStatus=="Mappable" &
          RecordCount > 0 ~
          z_palette[
            cut(
              pmin(pmax(Z_value_dynamic, -3), 3),
              breaks = z_breaks,
              include.lowest = TRUE
            )
          ],
        
        # ---- PRECOMPUTED Z (default map + Year only) ----
        MapStatus=="Mappable" &
          RecordCount > 0 ~
          z_palette[
            cut(
              pmin(pmax(Z_value, -3), 3),
              breaks = z_breaks,
              include.lowest = TRUE
            )
          ],
        
        # ---- DEFAULT ----
        TRUE ~ missing_color
      ),
      
      fillOpacity = ifelse(fillColor == missing_color, 0.5, 0.7)
      
    )
    map_sf
  })
  
  
  # --- Leaflet base ---
  output$map <- renderLeaflet({
    leaflet(world, options=leafletOptions(zoomControl=TRUE, scrollWheelZoom=FALSE)) %>%
      addTiles() %>% setView(lng=0, lat=26, zoom=2) %>%
      addFullscreenControl(pseudoFullscreen=TRUE, position="topright") %>%
      htmlwidgets::onRender("
      function(el,x){
        var map=this;
        map.zoomControl.setPosition('topright');
        var info=document.createElement('div');
        info.id='map-scroll-info';
        info.innerHTML='Hold <b>Ctrl</b> + scroll to zoom map';
        info.style.cssText='position:absolute;top:95%;left:50%;transform:translate(-50%, -50%);background:white;padding:5px 8px;border:1px solid #999;border-radius:4px;font-size:12px;opacity:0.8;z-index:1000;';
        map.getContainer().appendChild(info);
        map.scrollWheelZoom.disable();
        map.getContainer().addEventListener('wheel', function(e){if(e.ctrlKey){e.preventDefault(); map.scrollWheelZoom._enabled||map.scrollWheelZoom.enable();} else {map.scrollWheelZoom._enabled&&map.scrollWheelZoom.disable();}}, {passive:false});
      }")
  })
  
  # --- Leaflet proxy ---
  observe({
    map_sf <- filtered_map()
    leafletProxy("map", data=map_sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~fillColor,
        fillOpacity = ~fillOpacity,
        weight = 0.5,
        color = "#444444",
        label = ~paste0("<b>", name_long, "</b><br/>", "Record(s): ", RecordCount) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight=2, color="#666", fillOpacity=0.9, bringToFront=TRUE)
      )
    
    show_z_legend <- all(c("All" %in% filter_state$Pathogen,
                           "All" %in% filter_state$Disease,
                           "All" %in% filter_state$Host,
                           "All" %in% filter_state$Country))
    
    if(show_z_legend){
      legend_title <- if(filter_state$Year!="All") paste0("Z-score (",filter_state$Year,")") else "Z-score (all years)"
      z_vals <- -3:3
      z_colors <- rev(hcl.colors(length(z_vals), "Greens 3"))
      legend_html <- paste0(
        "<div id='legend-box' style='background:white;padding:5px;border:1px solid #999;border-radius:5px;'>",
        "<strong style='cursor:pointer;' onclick='var x=document.getElementById(\"legend-content\"); if(x.style.display==\"none\"){x.style.display=\"block\";} else{x.style.display=\"none\";}'>", 
        legend_title, "</strong>",
        "<div id='legend-content' style='display:none;margin-top:5px;'>",
        paste0("<div style='display:flex;align-items:center;'>",
               paste0("<div style='width:20px;height:20px;margin-right:3px;background:", z_colors, ";border:1px solid #444;'></div>",
                      "<span style='margin-right:5px;'>", z_vals, "</span>", collapse=""),"</div>"),
        "<div style='display:flex;align-items:center;'><div style='width:20px;height:20px;margin-right:3px;background:#ffdada;border:1px solid #444;'></div><span>No data</span></div>",
        "</div></div>"
      )
      leafletProxy("map") %>% addControl(html=legend_html, position="bottomleft")
    }
  })
  
  # --- Reset button ---
  observeEvent(input$reset_btn,{
    log_interaction(session, "RESET_FILTERS", "User reset all filters")
    lapply(filter_columns, function(col) filter_state[[col]] <- "All")
    
    for(col in c("Pathogen","Disease","Host")){
      updateSelectInput(session, paste0("letter_", col), selected="All")
      updatePickerInput(session, paste0("input_", col), selected="All")
    }
    updatePickerInput(session, "input_Country", choices="All", selected="All")
    updateSelectInput(session,"input_Year", selected="All")
  })
  
  # --- Data table ---
  output$filtered_table <- renderDT({
    df <- filtered_data()
    remove_cols <- c("Pathogen", "Host", "Disease", "Info", "State", "Country", "Year", "name_long", "MapStatus", 
                     "RecordCount", "Z_all_years", "Z_all_years_clamped", "Z_by_year", "Z_by_year_clamped")
    df <- df %>% select(-intersect(names(df), remove_cols))
    datatable(df, options=list(pageLength=10, scrollX=TRUE))
  })
  
}


# --- Run App ---
shinyApp(ui, server)
