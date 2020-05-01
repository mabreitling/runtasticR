table_runs <- function(runs, desc = TRUE){
  runs %>%
    group_by(id) %>%
    summarise("date" = min(lubridate::date(time)), "dist_m" = max(dist_cumsum), "pace_mean" = max(duration/60)/(max(dist_cumsum)/1000),
              "pace_median" = median(pace, na.rm = TRUE)) %>%
    arrange(desc(date))

}

table_runs(runs)

