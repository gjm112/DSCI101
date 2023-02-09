dirtybeach <- read.csv("/Users/gregorymatthews/Downloads/Beach_E._coli_Predictions.csv")

summary(dirtybeach)


library(tidyverse)

#By latitude
ggplot(aes(x = Latitude, y = Predicted.Level), data = dirtybeach) + geom_point() + geom_smooth()


dirtybeach %>% filter(Latitude > 42) %>% select(Beach.Name) %>% table()


factor(dirtybeach$Beach.Name)
ggplot(aes(x = Beach.Name, y = Predicted.Level), data = dirtybeach) + geom_boxplot() + coord_flip()


#By beach
orderedbeach <- dirtybeach %>% group_by(Beach.Name) %>% summarize(lat = head(Latitude,1)) %>% arrange(lat) %>% select(Beach.Name)
dirtybeach$Beach.Name <- factor(dirtybeach$Beach.Name, levels = orderedbeach$Beach.Name)
ggplot(aes(x = Beach.Name, y = Predicted.Level), data = dirtybeach) + geom_boxplot() + coord_flip()


#Lets look at seaonality
dirtybeach$Date <- as.Date(dirtybeach$Date, format = "%m/%d/%Y")
sub <- dirtybeach %>% filter(Beach.Name == "Margaret T Burroughs (31st)")
ggplot(aes(x = Date, y = Predicted.Level), data = sub) + geom_line()

library(lubridate)
sub$Year <- factor(year(sub$Date))
sub$Dayofyear <- yday(sub$Date)
sub <- sub %>% filter(Year != 2020)
ggplot(aes(x = Dayofyear, y = Predicted.Level, color = Year), data = sub)  + geom_point()  + geom_smooth()











