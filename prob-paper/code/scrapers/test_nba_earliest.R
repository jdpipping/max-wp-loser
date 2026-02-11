# Test to find earliest NBA win probability data available from ESPN
library(hoopR)
library(tidyverse)

message("Testing NBA win probability data availability...")

# Test years from recent to older
test_years = 2025:2010

earliest_year = Inf

for (year in test_years) {
  message(sprintf("Testing NBA year %d...", year))
  tryCatch({
    schedule = load_nba_schedule(seasons = year)
    if (!is.null(schedule) && nrow(schedule) > 0 && "id" %in% names(schedule)) {
      # Try first 20 games to find one with WP data
      found_wp = FALSE
      for (i in 1:min(20, nrow(schedule))) {
        test_game_id = schedule$id[i]
        tryCatch({
          wp_result = espn_nba_wp(test_game_id)
          # Check if it's a list with winprobability element
          if (is.list(wp_result) && "winprobability" %in% names(wp_result)) {
            wp_data = wp_result$winprobability
            if (!is.null(wp_data) && (is.data.frame(wp_data) || is_tibble(wp_data)) && nrow(wp_data) > 0) {
              found_wp = TRUE
              earliest_year = min(earliest_year, year)
              message(sprintf("  ✓ Year %d: Available! (tested game %d, ID: %s, %d rows)", 
                             year, i, test_game_id, nrow(wp_data)))
              break
            }
          } else if (is.data.frame(wp_result) || is_tibble(wp_result)) {
            # Maybe it returns a tibble directly
            if (nrow(wp_result) > 0 && ("home_win_percentage" %in% names(wp_result) || 
                                       "homeWinPercentage" %in% names(wp_result))) {
              found_wp = TRUE
              earliest_year = min(earliest_year, year)
              message(sprintf("  ✓ Year %d: Available! (tested game %d, ID: %s, %d rows)", 
                             year, i, test_game_id, nrow(wp_result)))
              break
            }
          }
        }, error = function(e) {
          # Try next game
        })
      }
      if (!found_wp) {
        message(sprintf("  ✗ Year %d: No WP data found in first 20 games", year))
      }
    } else {
      message(sprintf("  ✗ Year %d: No schedule found", year))
    }
  }, error = function(e) {
    message(sprintf("  ✗ Year %d: Error - %s", year, e$message))
  })
}

message("\n", paste(rep("=", 50), collapse = ""))
if (!is.infinite(earliest_year)) {
  message(sprintf("NBA earliest year with WP data: %d", earliest_year))
} else {
  message("NBA: No years with WP data found in tested range")
}
message(paste(rep("=", 50), collapse = ""))

