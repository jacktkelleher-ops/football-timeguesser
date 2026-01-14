library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(tidygeocoder)

# --------------------------------------------------------
# 1. CONFIGURATION
# --------------------------------------------------------
api_key <- Sys.getenv("FLICKR_API_KEY")
if (api_key == "") {
  api_key <- "c12de9baf7b727bedddc5f0b51bf15e1" 
}target_profile <- "https://www.flickr.com/photos/footpicshd/"
target_count <- 5 

# --------------------------------------------------------
# 2. FLICKR FETCH
# --------------------------------------------------------
get_flickr_id <- function(profile_url, key) {
  url <- "https://www.flickr.com/services/rest/"
  res <- GET(url, query = list(method = "flickr.urls.lookupUser", api_key = key, url = profile_url, format = "json", nojsoncallback = 1))
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  return(data$user$id)
}

fetch_metadata <- function(user_id, key) {
  base_url <- "https://www.flickr.com/services/rest/"
  all_photos <- list()
  page <- 1
  max_pages <- 20 
  
  message("🚀 Downloading photo list...")
  
  repeat {
    params <- list(
      method = "flickr.people.getPublicPhotos",
      api_key = key,
      user_id = user_id,
      extras = "date_taken,url_l,url_o", 
      per_page = 100,
      page = page,
      format = "json",
      nojsoncallback = 1
    )
    
    res <- GET(base_url, query = params)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (length(data$photos$photo) == 0) break
    
    photos <- data$photos$photo
    
    try({
      df <- photos %>%
        as_tibble() %>%
        mutate(
          title = if("title" %in% names(.)) title else "",
          url = case_when(
            "url_o" %in% names(.) ~ url_o,
            "url_l" %in% names(.) ~ url_l,
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(url), nchar(title) > 5) %>%
        select(Image_URL = url, Date_Meta = datetaken, Title = title)
      
      all_photos[[length(all_photos) + 1]] <- df
    }, silent = TRUE)
    
    if (page >= max_pages) break
    page <- page + 1
  }
  return(bind_rows(all_photos))
}

# --------------------------------------------------------
# 3. SMART PARSER (LOCATION + YEAR)
# --------------------------------------------------------
user_id <- get_flickr_id(target_profile, api_key)
full_list <- fetch_metadata(user_id, api_key)

candidates <- full_list %>% sample_n(min(n(), 15))

message(paste("🌍 Parsing & Geocoding", nrow(candidates), "photos..."))

results <- list()

for (i in 1:nrow(candidates)) {
  row <- candidates[i, ]
  raw_title <- row$Title
  
  # Split title by " - "
  parts <- str_split(raw_title, " - ")[[1]]
  
  location_query <- NA
  year_extracted <- NA
  
  # --- LOGIC: EXTRACT LOCATION & YEAR ---
  if (length(parts) >= 3) {
    # 1. Location is usually SECOND TO LAST
    location_part <- parts[length(parts) - 1]
    location_query <- location_part 
    
    # 2. Date is usually the LAST part (e.g., "June 23, 2024")
    date_part <- parts[length(parts)]
    # Extract the 4 digits from that string
    year_extracted <- as.integer(str_extract(date_part, "\\d{4}"))
    
  } else {
    # Fallback for simple titles
    location_query <- paste(str_remove_all(raw_title, "<.*?>"), "Stadium")
  }
  
  # Fallback: If title didn't have a year, use the camera metadata
  if (is.na(year_extracted)) {
    year_extracted <- as.integer(str_extract(row$Date_Meta, "^\\d{4}"))
  }
  
  message(paste("   Title:", str_trunc(raw_title, 40)))
  message(paste("      📍 Loc:", location_query))
  message(paste("      📅 Year:", year_extracted))
  
  # Geocode
  geo <- geo(address = location_query, method = "arcgis", verbose = FALSE)
  
  if (!is.na(geo$lat)) {
    message("      ✅ Found!")
    row$Real_Lat <- geo$lat
    row$Real_Lon <- geo$long
    row$Correct_Year <- year_extracted # Assign the extracted year
    results[[length(results) + 1]] <- row
  } else {
    message("      ❌ Location lookup failed.")
  }
  
  if (length(results) >= target_count) break
}

# --------------------------------------------------------
# 4. SAVE
# --------------------------------------------------------
final_df <- bind_rows(results) %>%
  mutate(Attribution = str_trunc(Title, 80)) %>%
  select(Image_URL, Real_Lat, Real_Lon, Correct_Year, Attribution)

if (nrow(final_df) == 0) stop("❌ No valid matches found.")

write_csv(final_df, "matches.csv")

message("------------------------------------------------")
message(paste("✅ SUCCESS! Saved", nrow(final_df), "matches with parsed Years."))

message("------------------------------------------------")
