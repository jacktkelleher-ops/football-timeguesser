library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(tidygeocoder)

api_key <- Sys.getenv("FLICKR_API_KEY")
if (api_key == "") {
  stop("FLICKR_API_KEY is not set")
}

target_profile <- "https://www.flickr.com/photos/footpicshd/"
target_count <- 5

get_flickr_id <- function(profile_url, key) {
  url <- "https://www.flickr.com/services/rest/"
  res <- GET(
    url,
    query = list(
      method = "flickr.urls.lookupUser",
      api_key = key,
      url = profile_url,
      format = "json",
      nojsoncallback = 1
    )
  )
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  data$user$id
}

fetch_metadata <- function(user_id, key) {
  base_url <- "https://www.flickr.com/services/rest/"
  all_photos <- list()
  page <- 1
  max_pages <- 20
  message("Downloading photo list...")
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
          title = if ("title" %in% names(.)) title else "",
          url = case_when(
            "url_o" %in% names(.) ~ url_o,
            "url_l" %in% names(.) ~ url_l,
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(url), nchar(title) > 5) %>%
        select(
          Image_URL = url,
          Date_Meta = datetaken,
          Title = title
        )
      all_photos[[length(all_photos) + 1]] <- df
    }, silent = TRUE)
    if (page >= max_pages) break
    page <- page + 1
  }
  bind_rows(all_photos)
}

user_id <- get_flickr_id(target_profile, api_key)
full_list <- fetch_metadata(user_id, api_key)

# ------------------------------
# Parse and filter all photos upfront
# Only keep photos whose titles match the expected "Match - Location - Year" format
# ------------------------------
full_list <- full_list %>%
  mutate(
    n_parts = sapply(str_split(Title, " - "), length),
    location_query = sapply(str_split(Title, " - "), function(p) {
      if (length(p) >= 3) p[length(p) - 1] else NA_character_
    }),
    title_year = as.integer(str_extract(
      sapply(str_split(Title, " - "), function(p) {
        if (length(p) >= 3) p[length(p)] else ""
      }),
      "\\d{4}"
    )),
    meta_year  = as.integer(str_extract(Date_Meta, "^\\d{4}")),
    photo_year = ifelse(!is.na(title_year), title_year, meta_year)
  ) %>%
  filter(n_parts >= 3, !is.na(photo_year), !is.na(location_query))

message(paste("Valid photos after format filtering:", nrow(full_list)))

# ------------------------------
# Stratified sampling across year buckets
# Ensures candidate pool has photos from a spread of eras,
# not just whatever is most common (typically recent years)
# ------------------------------
year_buckets <- list(
  c(2000, 2014),
  c(2015, 2018),
  c(2019, 2021),
  c(2022, 2023),
  c(2024, 2030)
)

candidates <- lapply(year_buckets, function(rng) {
  bucket <- full_list %>% filter(photo_year >= rng[1], photo_year <= rng[2])
  n_available <- nrow(bucket)
  if (n_available == 0) {
    message(paste("  Bucket", rng[1], "-", rng[2], ": no photos available"))
    return(NULL)
  }
  n_sample <- min(n_available, 3)
  message(paste("  Bucket", rng[1], "-", rng[2], ":", n_available, "available, sampling", n_sample))
  bucket %>% slice_sample(n = n_sample)
}) %>%
  bind_rows() %>%
  slice_sample(n = n())  # shuffle so bucket order doesn't bias geocoding

message(paste("Total stratified candidates:", nrow(candidates)))
message("Geocoding candidates...")

results <- list()
germany_2024_count <- 0

for (i in seq_len(nrow(candidates))) {
  row <- candidates[i, ]
  location_query <- row$location_query
  year_extracted <- row$photo_year

  message(paste("   Title:", str_trunc(row$Title, 40)))
  message(paste("      Location:", location_query))
  message(paste("      Year:", year_extracted))

  geo <- geo(
    address = location_query,
    method = "arcgis",
    verbose = FALSE
  )
  if (!is.na(geo$lat)) {
    # Limit Germany 2024 photos to at most 1 per game
    # Germany bounding box: lat 47-55, lon 6-15
    is_germany_2024 <- !is.na(year_extracted) && year_extracted == 2024 &&
                       geo$lat >= 47 && geo$lat <= 55 &&
                       geo$long >= 6 && geo$long <= 15
    if (is_germany_2024 && germany_2024_count >= 1) {
      message("      Skipping: Germany 2024 limit reached.")
    } else {
      if (is_germany_2024) germany_2024_count <- germany_2024_count + 1
      message("      Found!")
      row$Real_Lat <- geo$lat
      row$Real_Lon <- geo$long
      row$Correct_Year <- year_extracted
      results[[length(results) + 1]] <- row
    }
  } else {
    message("      Location lookup failed.")
  }
  if (length(results) >= target_count) break
}

final_df <- bind_rows(results) %>%
  mutate(Attribution = str_trunc(Title, 80)) %>%
  select(
    Image_URL,
    Real_Lat,
    Real_Lon,
    Correct_Year,
    Attribution
  )

if (nrow(final_df) == 0) {
  stop("No valid matches found.")
}

write_csv(final_df, "matches.csv")

message("------------------------------------------------")
message(paste("SUCCESS! Saved", nrow(final_df), "matches with parsed Years."))
message("------------------------------------------------")
