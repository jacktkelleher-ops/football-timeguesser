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
      .breakdown-box {
        background-color: #1a252f;
        border: 1px solid #2c3e50;
        border-radius: 10px;
        padding: 12px;
        margin-bottom: 10px;
        display: flex;
        justify-content: space-around;
        text-align: center;
      }
      .breakdown-item span.label {
        display: block;
        color: #bdc3c7;
        font-size: 0.8em;
      }
      .breakdown-item span.value {
        font-size: 1.5em;
        font-weight: bold;
      }
      .summary-grid {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 15px;
        padding: 20px 0;
      }
      .summary-card {
        background: #2c3e50;
        border-radius: 8px;
        overflow: hidden;
        width: 180px;
        text-align: center;
      }
      .summary-card img {
        width: 100%;
        height: 130px;
        object-fit: cover;
      }
      .summary-card .card-score {
        padding: 10px;
        color: #f39c12;
        font-size: 1.2em;
        font-weight: bold;
      }
      .final-score-box {
        background: #2c3e50;
        border-radius: 12px;
        padding: 25px;
        text-align: center;
        margin: 20px auto;
        max-width: 400px;
      }

      /* ---- Mobile styles ---- */
      @media (max-width: 767px) {

        /* Reduce map height on small screens */
        #map { height: 350px !important; }

        /* Show controls panel above map on mobile by reversing column order */
        #game_area > .row {
          display: flex;
          flex-direction: column-reverse;
        }

        /* Ensure both columns go full width when stacked */
        #game_area > .row > div {
          width: 100% !important;
        }

        /* Limit photo height on mobile */
        #matchImage img { max-height: 220px; }

        /* Stack score breakdown vertically on narrow screens */
        .breakdown-box {
          flex-direction: column;
          gap: 6px;
        }

        /* Smaller summary cards on mobile */
        .summary-card { width: 120px; }
        .summary-card img { height: 90px; }
        .summary-card .card-score { font-size: 1em; padding: 6px; }

        /* Tighten summary grid gap */
        .summary-grid { gap: 10px; }

        /* Final score box full width */
        .final-score-box { max-width: 100%; margin: 10px; }
      }
    "))
  ),
  
  titlePanel("⚽ Football TimeGuesser"),

  # --- Main Game Area ---
  tags$div(id = "game_area",
  fluidRow(
    # --- Left Column: Map ---
    column(width = 8,
           leafletOutput("map", height = "750px", width = "100%")
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
                                  min = 1900, max = as.integer(format(Sys.Date(), "%Y")), value = 2000, sep = "", step = 1),
                      
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
             ),

             # Score Breakdown (shown after each guess)
             hidden(tags$div(id = "score_breakdown",
               tags$div(class = "breakdown-box",
                 tags$div(class = "breakdown-item",
                   tags$span("📍 Location", class = "label"),
                   tags$span(textOutput("loc_score_display"), class = "value", style = "color: #3498db;")
                 ),
                 tags$div(class = "breakdown-item",
                   tags$span("📅 Year", class = "label"),
                   tags$span(textOutput("year_score_display"), class = "value", style = "color: #e74c3c;")
                 ),
                 tags$div(class = "breakdown-item",
                   tags$span("Round Total", class = "label"),
                   tags$span(textOutput("round_score_display"), class = "value", style = "color: #f39c12;")
                 )
               )
             ))
           )
    )
  )
  ), # end game_area

  # --- Summary Page (shown after final round) ---
  hidden(tags$div(id = "summary_page",
    tags$h2("🏆 Game Over!", style = "text-align: center; color: #f39c12; padding-top: 20px;"),
    tags$div(class = "summary-grid",
      uiOutput("summary_grid")
    ),
    tags$div(class = "final-score-box",
      tags$div("Final Score", style = "color: #bdc3c7; font-size: 0.95em;"),
      tags$div(textOutput("final_score_text"), style = "color: #f39c12; font-size: 2.5em; font-weight: bold;"),
      tags$div("/ 50,000", style = "color: #bdc3c7; font-size: 1em;")
    )
  ))
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
    game_over = FALSE,
    round_results = list()
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
    loc_score <- 5000 * exp(-dist_km / 1500)

    year_diff <- abs(input$year_guess - m$Correct_Year)
    year_score <- round(5000 * exp(-year_diff / 3))

    round_score <- round(loc_score + year_score)
    rv$total_score <- rv$total_score + round_score

    # Store result for summary page
    rv$round_results[[rv$round]] <- list(
      image_url = as.character(m$Image_URL),
      round_score = round_score
    )
    
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

    output$loc_score_display  <- renderText(round(loc_score))
    output$year_score_display <- renderText(round(year_score))
    output$round_score_display <- renderText(round_score)

    shinyjs::show("score_breakdown")
    shinyjs::hide("submit")
    shinyjs::show("next_round")
  })
  
  # --- Next Round Logic ---
  observeEvent(input$next_round, {
    if (rv$round >= nrow(rv$matches)) {
      rv$game_over <- TRUE
      shinyjs::hide("game_area")
      shinyjs::show("summary_page")
      return()
    }
    
    rv$round <- rv$round + 1
    rv$guess_lat <- NA
    rv$guess_lon <- NA
    output$feedback_title <- renderText("")
    output$feedback_details <- renderText("")
    output$attribution <- renderText("")

    updateSliderInput(session, "year_guess", value = 2015)
    shinyjs::hide("score_breakdown")

    leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    shinyjs::show("submit")
    shinyjs::hide("next_round")
  })

  # --- Summary Page Outputs ---
  output$summary_grid <- renderUI({
    results <- rv$round_results
    cards <- lapply(seq_along(results), function(i) {
      r <- results[[i]]
      tags$div(class = "summary-card",
        tags$img(src = r$image_url),
        tags$div(class = "card-score", paste("+", format(r$round_score, big.mark = ",")))
      )
    })
    tagList(cards)
  })

  output$final_score_text <- renderText({
    format(rv$total_score, big.mark = ",")
  })
}

shinyApp(ui, server)