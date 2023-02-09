vio <- read.csv("/Users/gregorymatthews/Downloads/Ordinance_Violations.csv")
library(tidyverse)

ggplot(aes(x = IMPOSED.FINE), data = vio) + geom_histogram(aes(y = ..density..),color = "pink") + geom_density(color = "navy")
ggplot(aes(x = IMPOSED.FINE), data = vio) + geom_boxplot(color = "chartreuse") + xlim(0, 5000)

vio %>% filter(IMPOSED.FINE > 0) %>% ggplot(aes(x = IMPOSED.FINE)) + geom_boxplot(color = "darkgreen") +  xlim(0, 5000)

table(vio$)
as.character(vio$CASE.DISPOSITION[1:10])
table(vio$CASE.DISPOSITION)


nrow(vio)


vio %>% group_by(RESPONDENTS) %>% summarize(bad = n()) %>% arrange(-bad) %>% head(20)

#DIANE GOTTLIEB section of code. 
vio %>% filter(RESPONDENTS == "DIANE GOTTLIEB") %>% group_by(VIOLATION.DESCRIPTION) %>% summarize(howmany =n(), nowmuch = sum(IMPOSED.FINE)) %>% arrange(-howmany)
vio %>% filter(RESPONDENTS == "DIANE GOTTLIEB") %>% summarise(howmuch = sum(IMPOSED.FINE))


#Make a map
vio %>% filter(IMPOSED.FINE > 0) %>% mutate(logfine = log(IMPOSED.FINE)) %>% head(10000) %>% ggplot(aes(x = LONGITUDE, y = LATITUDE, color = logfine)) + geom_point() + scale_color_gradientn(colors = c("red","darkblue","orange","darkgreen","black"))


vio %>% filter(IMPOSED.FINE > 0) %>% head(10000) %>% mutate(finerange = cut(IMPOSED.FINE,breaks = c(0,499,999,1499,100000),labels = c("small","medium","large","Diane"))) %>%  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = finerange)) + facet_grid(~finerange) +geom_point(alpha = 0.1)
  
  
  %mutate(logfine = log(IMPOSED.FINE)) %>% head(10000) %>% ggplot(aes(x = LONGITUDE, y = LATITUDE, color = logfine)) + geom_point() + scale_color_gradientn(colors = c("red","darkblue","orange","darkgreen","black"))


library(ggmap)
41.8781° N, 87.6298° W
chi <- get_map(location = c(lon = -87.6298, lat = 41.8781), source = "stamen")



library(leaflet)

diane <- vio %>% filter(RESPONDENTS == "DIANE GOTTLIEB")
m <- leaflet() %>% addTiles()
m <- m %>% setView(-87.6298, 41.8781, zoom = 10) 
m %>% addMarkers(diane$LONGITUDE[1:10],diane$LATITUDE[1:10])
# a map with the default OSM tile layer





