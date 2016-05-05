library(dplyr)
library(tidyr)



## Cleaning
source("str_helpers.R")


# Start Analysis
d0 <- read.csv("Data+for+TreefortBnB+Puzzle.csv", stringsAsFactors = FALSE)
names(d0) <- c("id","city","state","price","nreviews")
d0$originalCities <- paste(d0$city, d0$state, sep = " - ")

# check if clean
sort((unique(d0$city)))
sort((unique(d0$state)))
length((unique(d0$state)))


# cities and states
# City original from: http://simplemaps.com/resources/us-cities-data (cities.csv)
# with some manual tweaking
cityNames <- read.csv("us-cities.csv")
dict <- cityNames[c('name','alternativeNames')]

originalCities <- d0$originalCities
originalCities <- unique(originalCities)
# Let's make an approximate replace

cleanCities <- dictionaryMatch(originalCities,dict, maxDistance = 2)
cities <- data.frame(originalCities = originalCities, cleanCities = cleanCities)

# “Which Are the Most Expensive Cities in America to Book a Tree Fort?”

d <- merge(d0, cities, by = "originalCities")
names(d)
d <- d[c("id","cleanCities","price","nreviews","state")]
names(d)[2] <- "city"


## Analysis

# median price of a tree fort in each of the top 100 cities that have the most units on the market.
#Just send us back a table with a list of the median price in each city, ranked from most to least expensive. Restrict your analysis to just to the "top 100" cities that have the most units on the market.

t <- d %>%
  group_by(city) %>%
  #filter(!is.na(city)) %>%
  summarise(medianPrice = median(price), units = n()) %>%
  top_n(100) %>%
  arrange(medianPrice) %>%
  select(city,medianPrice)

write.csv(t,"output.csv",row.names = FALSE)



# Get additional data

source("scrape_helpers.R")
url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
population <- getWikipediaTable(url)
population <- population[c("City","State[5]","2014 estimate")]
names(population) <- c("city","state","population")
population$city <- removeNotes(population$city)
population$state <- removeNotes(population$state)
population$population <- as.numeric(gsub("[^[:digit:]]","",population$population))

url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States"
states <- getWikipediaTable(url,1)
states <- states[c("State","Abbr.")]
names(states) <- c("state","abbr")
write.csv(states,"states.csv",row.names = FALSE)
states$state <- trim_spaces(states$state) %>% removeNotes()
population <- merge(population,states, by = "state",all.x = TRUE)
population$city <- paste(population$city, population$abbr, sep=" - ")

d2 <- merge(d,population[c("population","city")])


# Cities with the most units percapita (top 10)

t2 <- d2 %>%
  select(city,population) %>%
  group_by(city,population) %>%
  summarise(units = n()) %>%
  mutate(unitsPerCapita = units/population) %>%
  ungroup() %>%
  arrange(desc(unitsPerCapita)) %>%
  top_n(10)

#

t3 <- d %>%
  select(state,price) %>%
  group_by(state) %>%
  summarise(units = n(), meanPrice = mean(price))

write.csv(t3,"treeforbnb-states.csv",row.names = FALSE)

library(dmaps)
# Experimental package: htmlwidgets wrapper for datamaps in R
# devtools::install_github("jpmarindiaz/dmaps")


## Sort of hacky bivariate Choropleth
d <- read.csv("treeforbnb-states.csv")
var1 <- cut2(d[,2],g=3)
levels(var1) <- c("x1","x2","x3")
var2 <- cut2(d[,3],g=3)
levels(var2) <- c("y1","y2","y3")

d$group <- paste(var1,var2,sep="")

groups2d <- apply(expand.grid(paste0("x",1:3),paste0("y",1:3)),1,
                  function(r)paste0(r[1],r[2]))
colors2d <- c("#e8e8e8","#e4acac","#c85a5a","#b0d5df","#ad93a5","#985356","#64acbe","#62718c","#574249")
customPalette <- data.frame(group = groups2d, color = colors2d)

opts <- list(
  defaultFill = "#FFFFFF",
  borderColor = "#CCCCCC",
  borderWidth = 0.3,
  highlightFillColor = "#999999",
  highlightBorderWidth = 1,
  palette = "PuBu",
  customPalette = customPalette,
  choroLegend = list(show = FALSE),
  bivariateLegend = list(show = TRUE, var1Label = "Units", var2Label = "Median Price")
)
dmaps("us_states", data = d,
      groupCol = "group",
      regionCols = "state",
      opts = opts)





