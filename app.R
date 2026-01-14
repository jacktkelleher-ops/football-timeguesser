# ------------------------------
# 1) Setup and Libraries
# ------------------------------
library(shiny)
library(leaflet)
library(shinyWidgets)
library(geosphere)
library(dplyr)
library(readr)
library(shinyjs)
library(bslib)

# ----------------------------------------------------
# 2) UI Logic
# ----------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "darkly"),
  
  tags$head(
    tags$style(HTML("
      .leaflet-container { background: #1a1a1a; }
      #matchImage img { 
        max-height: 500px; 
        width: auto;
        max-width: 100%;
        display: block;
        margin: 0 auto;
        border-radius: 4px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.5);
      }
      .score-box {
        background-color: #2c3e50;
        padding: 15px;
        border-radius: 10px;
        text-align: center;
        margin-bottom: 15px;
      }
    "))
  ),
  
  titlePanel("⚽ Football TimeGuesser"),
  
  fluidRow(
    # --- Left Column: Map ---
    column(width = 8,
           leafletOutput("map", height = "750px")
    ),
    
    # --- Right Column: Game Controls ---
    column(width = 4,
           wellPanel(
             # Score Display
             tags$div(class = "score-box",
                      tags$h3(textOutput("score_display"), style = "color: #f39c12; margin: 0;"),
                      tags$span("Total Score", style = "color: #bdc3c7; font-size: 0.9em;")
             ),
             
             # The Image
             uiOutput("matchImage"),
             tags$br(),
             
             # Controls
             tags$div(id = "game_controls",
                      sliderInput("year_guess", "📅 Guess the Year:", 
                                  min = 2000, max = 2025, value = 2015, sep = "", step = 1),
                      
                      tags$p("📍 Click on the map to guess the location!", style = "color: #aaa; font-size: 0.9em;"),
                      
                      actionButton("submit", "Submit Guess", class = "btn-primary btn-lg btn-block", width = "100%"),
                      hidden(actionButton("next_round", "Next Round ➡️", class = "btn-success btn-lg btn-block", width = "100%"))
             ),
             
             tags$hr(),
             
             # Feedback Area
             tags$div(
               tags$h4(textOutput("feedback_title"), style = "text-align: center; color: #00bc8c; font-weight: bold;"),
               tags$p(textOutput("feedback_details"), style = "text-align: center; color: #fff;"),
               tags$p(textOutput("attribution"), style = "color: #888; font-size: 0.8em; text-align: center; font-style: italic;")
             )
           )
    )
  )
)

# ----------------------------------------------------
# 3) Server Logic
# ----------------------------------------------------
server <- function(input, output, session) {
  
  # --- Load Data ---
  matches_data <- tryCatch({
    read_csv("matches.csv", show_col_types = FALSE)
  }, error = function(e) {
    NULL
  })
  
  # Game State Values
  rv <- reactiveValues(
    matches = matches_data,
    round = 1,
    total_score = 0,
    guess_lat = NA,
    guess_lon = NA,
    game_over = FALSE
  )
  
  # --- FIX: Wrapped this check in observe() ---
  observe({
    if (is.null(rv$matches)) {
      showModal(modalDialog(
        title = "Error",
        "Could not find 'matches.csv'. Please run the builder script first!",
        easyClose = FALSE
      ))
    }
  })
  
  # --- Map Initialization ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # --- Map Click Logic ---
  observeEvent(input$map_click, {
    if(rv$game_over) return()
    
    rv$guess_lat <- input$map_click$lat
    rv$guess_lon <- input$map_click$lng
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = rv$guess_lon, lat = rv$guess_lat)
  })
  
  # --- Render Image ---
  output$matchImage <- renderUI({
    req(rv$matches)
    # Ensure we don't go out of bounds
    if (rv$round > nrow(rv$matches)) return(NULL)
    
    tags$img(src = rv$matches$Image_URL[rv$round])
  })
  
  # --- Render Score ---
  output$score_display <- renderText(paste(rv$total_score))
  
  output$attribution <- renderText({
    req(rv$matches)
    if (rv$round > nrow(rv$matches)) return("")
    return("") 
  })
  
  # --- Submit Guess Logic ---
  observeEvent(input$submit, {
    if (is.na(rv$guess_lat)) {
      showNotification("📍 You need to click the map first!", type = "warning")
      return()
    }
    
    m <- rv$matches[rv$round, ]
    
    # 1. Calculate Distance
    dist_km <- distHaversine(c(rv$guess_lon, rv$guess_lat), c(m$Real_Lon, m$Real_Lat)) / 1000
    
    # 2. Score Calculation
    loc_score <- 4000 * exp(-dist_km / 1500) 
    
    year_diff <- abs(input$year_guess - m$Correct_Year)
    year_score <- max(0, 1000 - (year_diff * 200))
    
    round_score <- round(loc_score + year_score)
    rv$total_score <- rv$total_score + round_score
    
    # 3. Update Map
    leafletProxy("map") %>%
      addMarkers(lng = m$Real_Lon, lat = m$Real_Lat, 
                 icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", 25, 41)) %>%
      addPolylines(lng = c(rv$guess_lon, m$Real_Lon), lat = c(rv$guess_lat, m$Real_Lat), 
                   color = "yellow", weight = 3, dashArray = "5, 10")
    
    # 4. Feedback
    output$feedback_title <- renderText(paste("+", round_score, "Points!"))
    output$feedback_details <- renderText(
      sprintf("It was %s (%d). You were %dkm away.", 
              m$Attribution, m$Correct_Year, round(dist_km))
    )
    
    output$attribution <- renderText(m$Attribution)
    
    shinyjs::hide("submit")
    shinyjs::show("next_round")
  })
  
  # --- Next Round Logic ---
  observeEvent(input$next_round, {
    if (rv$round >= nrow(rv$matches)) {
      rv$game_over <- TRUE
      output$feedback_title <- renderText("🏆 GAME OVER")
      output$feedback_details <- renderText(paste("Final Score:", rv$total_score))
      shinyjs::hide("next_round")
      shinyjs::hide("game_controls")
      return()
    }
    
    rv$round <- rv$round + 1
    rv$guess_lat <- NA
    rv$guess_lon <- NA
    output$feedback_title <- renderText("")
    output$feedback_details <- renderText("")
    output$attribution <- renderText("")
    
    leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    shinyjs::show("submit")
    shinyjs::hide("next_round")
  })
}

shinyApp(ui, server)