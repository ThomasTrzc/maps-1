library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
devtools::install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(tidyverse)


files <- list.files("C:/Users/Thomas/Desktop/zoology2") #change directory as appropriate 

setwd("C:/Users/Thomas/Desktop/zoology2") 

f <- import_results(file = files[21:length("zoology2")])

view(f)

#Extract the addresses of the authors
raw <- f$address
#Removing punctuation
raw <- gsub("[[:punct:]\n]","",raw)

#Create a list of country names
library(maps)
data(world.cities)
c1 <- unique(world.cities$country.etc)
#Add synonyms in case we're not sure how some countries might be represented
c1 <- c(c1, "UK", "U.K.", "England", "Scotland", 
        "Wales", "Northern Ireland", "Britain",
        "U.S.A.", "United States")
#Lengthen c1 to include all caps cases
c1 <- unique(c(c1, toupper(c1)))

#Create a matrix of results and countries and look for matches
matches <- matrix(nrow = 98, ncol = length(c1))
colnames(matches) <- c1
for(i in 1:length(raw)){
  for(j in 1:length(c1)){
    if(grepl(pattern = c1[[j]], x = raw[[i]])){
      matches[[i,j]] <- TRUE
    }
  }
}
#Extract the represented countries
CountryList_raw_multi <- c()
for(i in 1:length(matches[,1])){
  CountryList_raw_multi[[i]] <- names(which(matches[i,] == TRUE))
}
rm(i,j,matches)
#create a single list of all countries represented
x <- as.factor(unlist(CountryList_raw_multi))
#Bring the UK together
x <- gsub("ENGLAND", "UNITED KINGDOM", x)
x <- gsub("Scotland", "United Kingdom", x)
x <- gsub("Wales", "United Kingdom", x)
x <- gsub("Northern Ireland", "United Kingdom", x)

#Unify the US 
x <- gsub("USA", "UNITED STATES", x)
x <- gsub("U.S.A.", "UNITED STATES", x)

countries_represented <- data.frame(x)
y <- data.frame(table(countries_represented))
y <- y[order(y$Freq, decreasing = T),]
knitr::kable(y, row.names = FALSE, 
             col.names = c("Nation", "n"), align = 'c') %>% 
  kableExtra::kable_styling(full_width = F)

#Calculate the proportional representation of each country
y <- data.frame(table(countries_represented$x))
y$alpha <- y$Freq/max(y$Freq)
y$rep <- rep(2, length(y[,1]))

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(ggplot2) # tidyverse data visualization package
library(imputeTS)# na_replace

#change all caps to sentence case (for matching with spatial data)
y$Var1 <- str_to_title(y$Var1)

#join publication data to world data 
z <- world %>% 
  left_join(y, by = c("name_long" = "Var1")) 

z %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot()+
  geom_sf(aes(fill = -alpha))+
  theme_minimal()+
  scale_fill_continuous(na.value="white")+
  labs(title = "Zoology reading list",
       subtitle = "Countries highlighted for researcher country of origin",
       X = NULL, Y = NULL,
       caption = "University of Liverpool")+
  scale_fill_gradient(name = "Country of origin", low = "red", high = "yellow", na.value = "grey50")