# Test to find earliest NFL win probability data available from ESPN
library(espnscrapeR)
library(tidyverse)

message("Testing NFL win probability data availability...")

# Test years from recent to older
test_years = 2024:2000

earliest_year = Inf

for (year in test_years) {
  message(sprintf("Testing NFL year %d...", year))
  tryCatch({
    schedule = get_nfl_schedule(season = year)
    if (!is.null(schedule) && nrow(schedule) > 0 && "game_id" %in% names(schedule)) {
      # Try first 10 games to find one with WP data
      found_wp = FALSE
      for (i in 1:min(10, nrow(schedule))) {
        test_game_id = schedule$game_id[i]
        tryCatch({
          wp_data = get_espn_win_prob(test_game_id)
          if (!is.null(wp_data) && nrow(wp_data) > 0) {
            # Check if it has the expected columns
            if ("home_wp" %in% names(wp_data)) {
              found_wp = TRUE
              earliest_year = min(earliest_year, year)
              message(sprintf("  ✓ Year %d: Available! (tested game %d, ID: %s, %d rows)", 
                             year, i, test_game_id, nrow(wp_data)))
              break
            }
          }
        }, error = function(e) {
          # Try next game
        })
      }
      if (!found_wp) {
        message(sprintf("  ✗ Year %d: No WP data found in first 10 games", year))
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
  message(sprintf("NFL earliest year with WP data: %d", earliest_year))
} else {
  message("NFL: No years with WP data found in tested range")
}
message(paste(rep("=", 50), collapse = ""))

