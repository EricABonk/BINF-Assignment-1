# Assignment 1
# Version:  2023.09.0+463
# Author: Eric Bonk
library(tidyverse)
# Load Data----
Oncorhynchus <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Oncorhynchus&format=tsv")
Lepomis <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Lepomis&format=tsv")
# Preliminary filtering of data ----
# NA Values need to be removed. Due to the nature of the analysis only high quality and complete data will be used.For this assignment we care about the country, bins, and species.
# Remove values with no country data for both data sets  
Oncorhynchus<- Oncorhynchus%>% filter(! country %in% c(NA))
Lepomis<- Lepomis%>% filter(! country %in% c(NA))
# Remove values with no bin 
Oncorhynchus<- Oncorhynchus%>% filter(! bin_uri %in% c(NA))
Lepomis<- Lepomis%>% filter(! bin_uri %in% c(NA))
# Remove NA for species 
Oncorhynchus<- Oncorhynchus%>% filter(!species_name %in% c(NA))
Lepomis<- Lepomis%>% filter(! species_name %in% c(NA))
# Data Check----
# For this assignment we are working with the number of records and geological data
# We need to check to make sure there is not errors 
# Check to make sure both genera have at least 10 bins
bins_Oncorhynchus<-unique(Oncorhynchus$bin_uri) 
length(bins_Oncorhynchus)
bins_Lepomis<-unique(Lepomis$bin_uri) 
length(bins_Lepomis)
# Check to make sure that species are actually species in the genus 
unique(Oncorhynchus$species_name)
unique(Lepomis$species_name)
# For the geological data need to make sure the countries are actually countries 
# Determine the countries for each genus 
unique(Oncorhynchus$country)
unique(Lepomis$country)
# Oncorhynchus has "Exception - Culture" which needs to be removed 
Oncorhynchus<- Oncorhynchus%>% filter(!country %in% c('Exception - Culture'))
# It is important to note that "Exception - Culture" was deliberately and critically removed now in order to ensure that both the records per species analysis and geographic analysis use the same data
# Number of records analysis----
# Filter data
# Determine the number of records per species 
Oncorhynchus_simplified<-Oncorhynchus %>%
  group_by(species_name) %>%
  summarise(count = length(processid))
Lepomis_simplified<-Lepomis %>%
  group_by(species_name) %>%
  summarise(count = length(processid))

# Summarization of key variables for analysis----
# Determine the total number of records for each genus 
sum(Oncorhynchus_simplified$count)
sum(Lepomis_simplified$count)
# Determine mean number of records for each genus 
mean(Oncorhynchus_simplified$count)
mean(Lepomis_simplified$count)
# Determine standard error for the mean number of records for each genus 
library(plotrix)
std.error(Oncorhynchus_simplified$count)
std.error(Lepomis_simplified$count)
# Determine range of records for each genus 
range(Oncorhynchus_simplified$count)
range(Lepomis_simplified$count)
# Determine if there is a statistically significant difference----
# Test assumptions
shapiro.test(Lepomis_simplified$count)
shapiro.test(Oncorhynchus_simplified$count)
# Levene test
library(car)
leveneTest(Lepomis_simplified$count,Lepomis_simplified$species_name)
leveneTest(Oncorhynchus_simplified$count,Oncorhynchus_simplified$species_name)
# The Levene's test is NA due to there being only one replicate per species so in order to further assess the distribution of the data a histogram needs to be made 
hist(Oncorhynchus_simplified$count) # Positive skew
hist(Lepomis_simplified$count) # Positive skew
# Data is Non-parametric so Wilcox test
wilcox.test(Lepomis_simplified$count, Oncorhynchus_simplified$count)
# Statistically significant difference occurs 
# Make bar plot for the mean number of records per species for each genus----
# Make data frame to conform to ggplot requirements
records_per_species<-data.frame(count=c(Lepomis_simplified$count,Oncorhynchus_simplified$count ),
                                Genus=c(rep("Lepomis", length(Lepomis_simplified$count)), 
                                        rep("Oncorhynchus", length(Oncorhynchus_simplified$count))))
# Plot 
library(ggplot2)
ggplot(records_per_species, aes(x=factor(Genus), y=count)) +
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="The Mean Number of Records per Species", x="Genus")+
  theme(axis.text.x = element_text(size=7,face = "bold"))+
  theme(axis.title = element_text(size = 8,face = "bold"))

# Geological analysis ----
# Determine contributing countries for each genus
countries_Oncorhynchus<-unique(Oncorhynchus$country)
countries_Lepomis<-unique(Lepomis$country)
# Filter data
# Determine the number of contributing countries per species 
Oncorhynchus_simplified_geo<-Oncorhynchus %>%
  group_by(species_name) %>%
  summarise(count = length(unique(country)))
Lepomis_simplified_geo<-Lepomis %>%
  group_by(species_name) %>%
  summarise(count = length(unique(country)))

# Summarization of key variables for analysis----
# Determine how many contries have contributed to each genus 
length(countries_Oncorhynchus)
length(countries_Lepomis)
# Are there any countries shared between the 2 genera
intersect(countries_Oncorhynchus,countries_Lepomis) # 11 Countries in common 
# Determine mean number of countries per species for each genus
mean(Oncorhynchus_simplified_geo$count)
mean(Lepomis_simplified_geo$count)
# Determine standard error for the mean number of records for each genus 
std.error(Oncorhynchus_simplified_geo$count)
std.error(Lepomis_simplified_geo$count)
# Determine range of number of countries for each genus 
range(Oncorhynchus_simplified_geo$count)
range(Lepomis_simplified_geo$count)
# Determine if there is a statistically significant difference----
# Test assumptions
shapiro.test(Lepomis_simplified_geo$count)
shapiro.test(Oncorhynchus_simplified_geo$count)
# Levene test
leveneTest(Lepomis_simplified_geo$count,Lepomis_simplified_geo$species_name)
leveneTest(Oncorhynchus_simplified_geo$count,Oncorhynchus_simplified_geo$species_name)
# The Levene's test is NA due to there being only one replicate per species so in order to further assess the distribution of the data a histogram needs to be made 
hist(Oncorhynchus_simplified_geo$count) # Positive skew
hist(Lepomis_simplified_geo$count) # Positive skew
# Data is Non-parametric so Wilcox test
wilcox.test(Lepomis_simplified_geo$count, Oncorhynchus_simplified_geo$count)
# Statistically significant difference occurs 
# Make map of countries that have contributed to data----
library(RgoogleMaps)
library(ggmap)
# In order to increase the readability of the map the coordinates in the Bold Data sets can not be used as each country would have many data points in slightly different locations 
# Need to use a central coordinate for each country 
# Must retrieve general country coordinates from the function map_data
country.maps <- map_data("world")
# Filter the data frame for the countries needed for each genus 
country_cord_Oncorhynchus<- country.maps%>% filter(region %in% c(countries_Oncorhynchus))
country_cord_Lepomis<- country.maps%>% filter(region %in% c(countries_Lepomis))
# These coordinates are the border coordinates so we need to take the mean of the lat and long for each country to get a roughly central coordinate
country_cord_Oncorhynchus<-country_cord_Oncorhynchus%>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
country_cord_Lepomis<-country_cord_Lepomis%>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
#Change name of region column for convienve 
country_cord_Oncorhynchus<-country_cord_Oncorhynchus%>% 
  rename(Country=region)
country_cord_Lepomis<-country_cord_Lepomis%>% 
  rename(Country=region)
# Load base map
# Note: Depending on the computer and the experience someone has with the get_map function, an initial attempt to load a world map using this function may not work. The function get_map archives map tiles as you use them and since I use the package frequently I have many tiles already archived. The package often won't run on a map this size if there are no archives. What you need to do is start with a smaller range of coordinates and slowly increase the range of coordinates. For me I simply had to use these lines of code:
# base = get_map(location=c(-180,-85,180,85), zoom=3, maptype="terrain-background")
# map1 = ggmap(base)
# But in order to make sure the base map loads properly I have uploaded the ggmap file to my GitHub. For convenience I will walk through how to access the file. Note: This file was created by me using the code in lines 156 and 157 and then using the code save(map1, file = "map.RData"). I then uploaded and published the file on my github.
load(url("https://github.com/EricABonk/BINF-Assignment-1/releases/download/v1.0.0/map.RData"))
map1
# Check to make sure base map has loaded correctly and zoom is at an appropriate level
map1

# Apply genus data to base map for each genus
library(viridis) # For colors
map_Oncorhynchus<-map1 + geom_point(data=country_cord_Oncorhynchus, aes(x=long, y=lat, colour= Country), shape=20, size=2)+
  scale_color_viridis(discrete = TRUE,option = "H")+
  labs(x="Latitude (\u00B0)", y="Longitude (\u00B0)") + 
  theme_bw() + theme(axis.text = element_text(size = rel(0.75),face = "bold"), 
                     axis.text.x = element_text(angle=45, vjust=0.5,face = "bold"),
                     axis.title =  element_text(face = "bold"))+
  theme(legend.text =element_text(size = 9,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(face = "bold"))+
  guides(colour = guide_legend(ncol = 1))

map_Lepomis<-map1 + geom_point(data=country_cord_Lepomis, aes(x=long, y=lat,colour=Country), shape=20, size=2)+
  scale_color_viridis(discrete = TRUE,option = "H")+
  labs(x="Latitude (\u00B0)", y="Longitude (\u00B0)") + 
  theme_bw() + theme(axis.text = element_text(size = rel(0.75),face = "bold"), 
                     axis.text.x = element_text(angle=45, vjust=0.5,face = "bold"),
                     axis.title =  element_text(face = "bold"))+
  theme(legend.text =element_text(size = 9,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(face = "bold"))


# Place the two maps side by side 
library(ggpubr)
ggarrange(map_Oncorhynchus,map_Lepomis, ncol=1,labels = c("A", "B"),font.label = list(size=10))

