library(ggplot2)
library(ggthemes)
runs <- runtasticR::import_runs('/home/marius/VL_Unterlagen/R/runtastic/export-20200424-000.zip')

runs %>% filter(id == 1) %>%
  ggplot(aes(x = duration, y = pace_5)) +
  geom_path()+
  theme_tufte()


runs %>% filter(pace_5 < 15, pace_5 > 1) %>%
  ggplot(aes(x = duration, y = pace_5)) +
  geom_line()+
  theme_minimal()+
  labs(title = 'Pace for distinct runs', subtitle = 'measured in min/km')+
  xlab('Meters')+
  ylab('Pace, min/km')+
  facet_wrap(~ as.factor(lubridate::date(time)), scale = "free_x")

runs %>%
  filter(pace < 20, time > "2020-01-01") %>%
  #filter(id == 7, pace < 20) %>%
  ggplot(aes(x = pace))+
  geom_density( alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~ as.factor(lubridate::date(time)))



run_map <- function(runs, runId, mapshot = TRUE){
  run_filt <- runs %>% filter(id %in% runId)
   map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider = 'Stamen.TonerLite')%>%
    leaflet::setView(lng = mean(run_filt$lon1, na.rm = TRUE), lat = mean(run_filt$lat1, na.rm = TRUE), zoom = 14)
    for(group in levels(as.factor(run_filt$id))){
      map <- leaflet::addPolylines(map, lng=~lon1,lat=~lat1, data=run_filt[run_filt$id==group,])
    }
  return(map)
}

run_map(runs, runId = c(1), mapshot = TRUE)









