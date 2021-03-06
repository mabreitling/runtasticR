#' Import Running Data
#'
#' Parse single .gpx file from runtastic export zipfile.
#'
#' @param path Relative path to single .gpx file in subfolder or export directory.
#' @return Tibble with gps-position data (lat, lon, elevation), time_span,
#' @return (cumulative) distance (time_span, duration, dist, dist_cumsum).
#' @examples
#' parse_gps(path = 'cad66c60-474d-4c2c-baa0-699d784de564.gpx')
parse_gps <- function(path) {
  xml <- xml2::read_xml(path)

  trkpt <- xml2::xml_find_all(xml, "/*/*[2]/*[3]/*") #read trackpoints by xpath
  lat <- trkpt %>% #extract longitude, elevation, time
    xml2::xml_attr("lat") %>%
    as.double()

  lon <- trkpt %>%
    xml2::xml_attr("lon") %>%
    as.double()

  ele <- trkpt  %>%
    xml2::xml_child("d1:ele") %>%
    xml2::xml_text() %>%
    as.double()

 time <- trkpt %>%
    xml2::xml_child("d1:time") %>%
    xml2::xml_text() %>%
    lubridate::ymd_hms() %>%
    c()

  track_tibble <- tibble::tibble(lat, lon, ele, time)

  track_tibble <- track_tibble %>%
    dplyr::mutate(time_span = time - dplyr::lag(time)) %>% #compute time_span between single gps points
    tidyr::replace_na(list(time_span = 0)) %>%
    dplyr::mutate(duration = cumsum(as.double(time_span)))

  track_tibble <- track_tibble %>%
    dplyr::rename(lat1 = lat, lon1 = lon) %>%
    dplyr::mutate(lat2 = dplyr::lag(lat1), lon2 = dplyr::lag(lon1)) %>%
    dplyr::mutate(dist = purrr::pmap_dbl(.,
            function(lon1, lat1, lon2, lat2, ...) {
              geosphere::distm(c(lon1, lat1), c(lon2, lat2), fun = geosphere::distHaversine)
              }))

  track_tibble <- track_tibble %>%
    tidyr::replace_na(list(dist = 0)) %>%
    dplyr::mutate(dist_cumsum =  cumsum(dist))

  track_tibble <- track_tibble %>%
    #dplyr::mutate(pace = (as.double(time_span) / 60) / (dist / 1000))%>%
    dplyr::mutate(pace = ifelse(dist == 0, 0, (as.double(time_span) / 60) / (dist / 1000)))%>%
    dplyr::mutate(pace_5 = zoo::rollmedian(pace, 5, na.pad = TRUE),
           pace_15 = zoo::rollmedian(pace, 15, na.pad = TRUE))

  track_tibble$pace[is.nan(track_tibble$pace)] <- 0

  return(track_tibble)

}

#' Import Running Data
#'
#' Creates one tidy tibble from zipfile exported via
#' https://www.runtastic.com/de/benutzer/[...]/einstellungen/account-and-data.
#' All runs from ./Sport-sessions/GPS-data are imported via the parse_gps()
#' function.
#'
#' @param zipfile Relative path to zipfile.
#' @return Tibble with gps-position data (lat, lon, elevation), time_span, (cumulative)
#'     distance, time_span, duration, distace (cumulative).
#' @examples
#' import_runs(zipfile = 'export-20200424-000.zip')
import_runs <- function(zipfile) {
  unzip(zipfile, exdir = "export")
  file_list <- list.files("./export/Sport-sessions/GPS-data/", pattern = "\\.gpx$")
  all_runs <- tibble::tibble()
  for (i in seq_along(file_list)) {
    run <- cbind(parse_gps(paste0("./export/Sport-sessions/GPS-data/", file_list[i])), id = i)
    all_runs <- rbind(all_runs, run)
  }
  unlink('export', recursive = TRUE)
  all_runs <- all_runs %>%
    dplyr::group_by(lubridate::date(time)) %>%
    dplyr::arrange(lubridate::date(time))%>%
    dplyr::mutate(id = dplyr::group_indices())

  return(tibble::tibble(all_runs))
}
