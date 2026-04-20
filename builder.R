library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(tidygeocoder)

api_key <- Sys.getenv("FLICKR_API_KEY")
if (api_key == "") stop("FLICKR_API_KEY is not set")

target_profile <- "https://www.flickr.com/photos/footpicshd/"

# ------------------------------
# Flickr helpers
# ------------------------------

get_flickr_id <- function(profile_url, key) {
  res <- GET("https://www.flickr.com/services/rest/",
    query = list(
      method        = "flickr.urls.lookupUser",
      api_key       = key,
      url           = profile_url,
      format        = "json",
      nojsoncallback = 1
    )
  )
  fromJSON(content(res, "text", encoding = "UTF-8"))$user$id
}

# Search a user's photos by text and return a tidy tibble (or NULL)
search_photos <- function(user_id, key, query, per_page = 100) {
  res <- GET("https://www.flickr.com/services/rest/",
    query = list(
      method         = "flickr.photos.search",
      api_key        = key,
      user_id        = user_id,
      text           = query,
      extras         = "date_taken,url_l,url_o",
      per_page       = per_page,
      format         = "json",
      nojsoncallback = 1
    )
  )
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  if (is.null(data$photos$photo) || length(data$photos$photo) == 0) return(NULL)

  tryCatch({
    data$photos$photo %>%
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
      select(Image_URL = url, Date_Meta = datetaken, Title = title)
  }, error = function(e) NULL)
}

# ------------------------------
# Title parsing
# ------------------------------

# Parse titles, extract location and year from the last two " - " segments.
# Only keeps photos where the extracted year matches expected_year.
parse_and_filter <- function(df, expected_year) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  result <- df %>%
    mutate(
      n_parts = sapply(str_split(Title, " - "), length),
      location_query = sapply(str_split(Title, " - "), function(p) {
        if (length(p) >= 3) p[length(p) - 1] else NA_character_
      }),
      photo_year = as.integer(str_extract(
        sapply(str_split(Title, " - "), function(p) {
          if (length(p) >= 3) p[length(p)] else ""
        }),
        "\\d{4}"
      ))
    ) %>%
    filter(
      n_parts >= 3,
      !is.na(location_query),
      !is.na(photo_year),
      photo_year == expected_year
    )

  if (nrow(result) == 0) return(NULL)
  result
}

# ------------------------------
# Bucket candidate fetching
# ------------------------------

# For a given era bucket, search years in order until we have enough valid
# candidates. Searches for "- YEAR" to match the title date format specifically.
get_bucket_candidates <- function(user_id, key, bucket_name, search_years, target = 3) {
  message(paste0("\n--- Bucket: ", bucket_name, " ---"))
  all_valid <- list()

  for (year in search_years) {
    query <- as.character(year)
    message(paste("  Searching for:", query))

    raw   <- search_photos(user_id, key, query)
    valid <- parse_and_filter(raw, year)

    if (!is.null(valid)) {
      message(paste("   ", nrow(valid), "confirmed photos for year", year))
      all_valid[[length(all_valid) + 1]] <- valid
    } else {
      message(paste("   No confirmed photos for year", year))
    }

    total_so_far <- if (length(all_valid) > 0) nrow(bind_rows(all_valid)) else 0
    if (total_so_far >= target) break
  }

  result <- if (length(all_valid) > 0) bind_rows(all_valid) else NULL

  if (is.null(result) || nrow(result) == 0) {
    message(paste("  No candidates found for bucket:", bucket_name))
    return(NULL)
  }

  message(paste("  Total candidates:", nrow(result)))
  result %>% slice_sample(prop = 1)
}

# ------------------------------
# Era bucket definitions
# Years listed in priority order (most likely to have content first).
# Pre-2015 has the longest fallback list to ensure we find content.
# ------------------------------
era_buckets <- list(
  list(name = "pre-2015",  years = c(2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003)),
  list(name = "2015-2018", years = c(2018, 2017, 2016, 2015)),
  list(name = "2019-2021", years = c(2021, 2020, 2019)),
  list(name = "2022-2023", years = c(2023, 2022)),
  list(name = "2024+",     years = c(2025, 2024, 2026))
)

# ------------------------------
# Main
# ------------------------------

user_id <- get_flickr_id(target_profile, api_key)
message(paste("Flickr user ID:", user_id))

results          <- list()
germany_2024_count <- 0

for (bucket in era_buckets) {
  candidates <- get_bucket_candidates(user_id, api_key, bucket$name, bucket$years)

  if (is.null(candidates)) {
    message(paste("Skipping bucket:", bucket$name, "(no valid candidates)"))
    next
  }

  # Try candidates in this bucket until ONE geocodes successfully
  found_for_bucket <- FALSE

  for (i in seq_len(nrow(candidates))) {
    row            <- candidates[i, ]
    location_query <- row$location_query
    year_extracted <- row$photo_year

    message(paste("  Geocoding:", str_trunc(row$Title, 55)))
    message(paste("    Location:", location_query, "| Year:", year_extracted))

    geo <- geo(address = location_query, method = "arcgis", verbose = FALSE)

    if (!is.na(geo$lat)) {
      # Germany 2024 cap
      is_germany_2024 <- !is.na(year_extracted) && year_extracted == 2024 &&
                         geo$lat >= 47 && geo$lat <= 55 &&
                         geo$long >= 6 && geo$long <= 15
      if (is_germany_2024 && germany_2024_count >= 1) {
        message("    Skipping: Germany 2024 limit reached.")
        next
      }
      if (is_germany_2024) germany_2024_count <- germany_2024_count + 1

      message(paste("    Geocoded! Year:", year_extracted))
      row$Real_Lat     <- geo$lat
      row$Real_Lon     <- geo$long
      row$Correct_Year <- year_extracted
      results[[length(results) + 1]] <- row
      found_for_bucket <- TRUE
      break

    } else {
      message("    Location lookup failed, trying next candidate.")
    }
  }

  if (!found_for_bucket) {
    message(paste("Warning: no geocodeable photo found for bucket:", bucket$name))
  }
}

# ------------------------------
# Output
# ------------------------------

final_df <- bind_rows(results) %>%
  mutate(Attribution = str_trunc(Title, 80)) %>%
  select(Image_URL, Real_Lat, Real_Lon, Correct_Year, Attribution)

message("\n--- Final match selection ---")
for (i in seq_len(nrow(final_df))) {
  message(sprintf("  Round %d | %d | %s", i, final_df$Correct_Year[i],
                  str_trunc(final_df$Attribution[i], 55)))
}

if (nrow(final_df) == 0) stop("No valid matches found.")

write_csv(final_df, "matches.csv")
message(paste("\nSUCCESS! Saved", nrow(final_df), "matches."))
