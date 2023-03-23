add2 <- function(x = 100){
  out <- x + 2
  return(out)
}

add2(1000)


fn <- function(x,y){
  cos(x) + exp(-y)
}




add2 <- function(x){
  out <- x + 2
  return(as.integer(out))
}

add2 <- function(x){
   (x + 2)
}

library(tidyverse)
map_int(1:10000,add2)


library(babynames)

topname <- function(yr){
  babynames %>% filter(year == yr) %>% arrange(-n) %>% head(1) %>% pull(name)
}

top <- map_chr(1880:2017,topname)

data.frame(year = 1880:2017, name = top)




topname <- function(yr, data){
  data %>% filter(year == yr) %>% arrange(-n) %>% head(1) %>% pull(name)
}

topname(1950, babynames)
top <- map_chr(1880:2017,topname,data = babynames)
data.frame(year = 1880:2017, name = top)




#Two lists returned one for male and 1 for female
topname_both <- function(yr, data){
  #female name 
  fname <- data %>% filter(year == yr & sex == "F") %>% arrange(-n) %>% head(1) %>% pull(name)
  #male name
  mname <- data %>% filter(year == yr & sex == "M") %>% arrange(-n) %>% head(1) %>% pull(name)
  data.frame(year = yr, fname,mname)
}

top <- map_dfr(1880:2017, topname_both, data = babynames)
#pmap
pmap(list(top$fname,top$year), viceversa, sx = "F",data = babynames)

#Old timey
#do.call(rbind,greg)


viceversa <- function(nm, yr, sx, data){
  data %>% filter(year == yr & name == nm & sex != sx) %>% pull(n)
}

viceversa("Emma",2017,"F",babynames)


map(top$fname, viceversa, yr = 2017, sx = "F", data = babynames)


viceversa <- function(nm, yr, sx , data ){
  data %>% filter(year == yr & name == nm & sex != sx) %>% pull(n)
}
map2(top$fname,top$year, viceversa, sx = "F",data = babynames)


pmap(list(top$fname,top$year), viceversa, sx = "F",data = babynames)








#Two lists returned one for male and 1 for female
topname_both <- function(yr, data){
  #female name 
  fname <- data %>% filter(year == yr & sex == "F") %>% arrange(-n) %>% head(1) %>% pull(name)
  #male name
  mname <- data %>% filter(year == yr & sex == "M") %>% arrange(-n) %>% head(1) %>% pull(name)
  c(year = yr, fname)
}

viceversa <- function(x, sx , data ){
  data %>% filter(year == as.numeric(x[1]) & name == x[2] & sex != sx) %>% pull(n)
}

top <- map(1880:2017, topname_both, data = babynames)
#map
#This works with a list as input
map(top, viceversa, sx = "F",data = babynames)

viceversa(x = top[[100]],sx = "F",data = babynames)
