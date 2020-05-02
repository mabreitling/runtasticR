#' Map running data using leaflet
#'
#' Filter running tibble and map runs.
#'
#' @param runs tibble
#' @param runId Vector of runId, runs is filtered by. Returned runs are mapped afterwards
#' @return leaflet object
#' @examples
#' run_map(runs_tibble, 2)
run_map <- function(runs, runId){
  run_filt <- runs %>% filter(id %in% runId)
   map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider = 'Stamen.TonerLite')%>%
    leaflet::setView(lng = mean(run_filt$lon1, na.rm = TRUE), lat = mean(run_filt$lat1, na.rm = TRUE), zoom = 14)
    for(group in levels(as.factor(run_filt$id))){
      map <- leaflet::addPolylines(map, lng=~lon1,lat=~lat1, data=run_filt[run_filt$id==group,])
    }
  return(map)
}






