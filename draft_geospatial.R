rm(list=ls(all=TRUE))

library(RSocrata, quietly = TRUE)
library(zoo, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(TSstudio, quietly = TRUE)
library(forecast, quietly = TRUE)
library(texreg, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(ggmap,quietly = TRUE)
library(OpenStreetMap, quietly = TRUE) 



#Yeah this part can take a min, as of nov 17 2021. The df has ~1 million rows
df <- read.socrata(
  "https://data.seattle.gov/resource/tazs-3rd5.json",
  app_token = "Token",
  email     = "Email",
  password  = "Password")


###########################################
#### Good old data  cleaning and setup ####


glimpse(df)
df <- na.omit(df)
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)
df <- filter(df, longitude != 0)
df <- filter(df, latitude != 0)
#I should go in later and filter some of these extra vars out 
#before I filter by date
df2019 <- filter(df, offense_start_datetime >= ("2019-01-01"), offense_start_datetime <= ("2019-12-31"))
df2020 <- filter(df, offense_start_datetime >= ("2020-01-01"), offense_start_datetime <= ("2020-12-31"))
df2021<- filter(df, offense_start_datetime >= ("2021-01-01"))
rm(df)

#that does not feel like alot of cleaning...
#I removed the original df, just because it is so so large


glimpse(df2021)
#crime_against_category("SOCIETY") is an intresting one


ggplot() +
  geom_point(data=df2021, aes(x=longitude, y=latitude))


#using stamenmap to get my map, here is a link on how to use it and common issues that might come up
#https://rdrr.io/cran/ggmap/man/get_stamenmap.html

#Seattle, feel free to adjust the size of the map, I just kinda winged it, probably forgot some places
sea_fullish = get_stamenmap(bbox = c(left = -122.45, bottom = 47.55, right = -122.25, top = 47.7), maptype = c("toner-lite"), zoom = 13)
ggmap(sea_fullish)
map <- ggmap(sea_fullish)
map + 
  ggtitle("Seattle, toner-lite") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 

#oh fuck yeah it is on the map
map +
  geom_point(data=df2021, aes(x=longitude, y=latitude))


######################
#### Maps n' shit ####
#first is pretty normal, with semi-translucent dots
#three is a heat map, which is super cool actually

#This first one is pretty cool, uses semi-translucent dots
map + 
  geom_point(data=df2021, aes(x=longitude, y=latitude), color="dark green", alpha=.03, size=1.1) +
  ggtitle("Scatter Map, Seattle") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 
#seems to be more around major roads/highways 




# This second one is super cool, I did not write this and I am not sure it is the most helpful right now
#but very cool

map +
  stat_density2d(data=df2021_ARSON, aes(x=longitude
                                            , y=latitude
                                            ,color=..density..
                                            ,size=ifelse(..density..<=1,0,..density..)
                                            ,alpha=..density..)
                 ,geom="tile",contour=F) +
  scale_color_continuous(low="orange", high="red", guide = "none") +
  scale_size_continuous(range = c(0, 3), guide = "none") +
  scale_alpha(range = c(0,.5), guide="none") +
  
  #if you wanted to add a title I guess 
  ggtitle("Seattle Crime") +
  theme(plot.title = element_text(family="Trebuchet MS", size=36, face="bold", hjust=0, color="#777777")) 



# the third map is a heat map, and its very cool 
#not perfect though, not sure that lake union is a crime hotspot.... maybe that pirate ship in lake union is actually a priate ship and not a party boat...

map + 
  stat_density2d( data = df2021, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 12, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.1, .3), guide = "none") +
  ggtitle("Heat Map, Full") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 

#This one has much more blow up colors
# map + 
#   stat_density2d( data = df2021, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 30, geom = 'polygon') +
#   scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
#   scale_alpha(range = c(.2, .3), guide = "none") +
#   guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
#   ggtitle("Heat Map 2021, Seattle") +
#   theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 
# 

#If you wanted a topographical look at all
#map + stat_density2d( data = df2021, aes(x = longitude, y = latitude), size = 1, bins = 15) 



####################################################
#### Different types of crimes now ####



counts <- count(df2021,offense_parent_group)
View(counts)

#counts_off <- count(df2021,offense)
#View(counts_off)
#Large list going to use offense_parent_group going forward
#also intreting to note 59 listed crime types online 42 in this data set

#not going to keep looking into this one, not alot of info to pull from three vars
#counts_crime_against_categroy <- count(df2021,crime_against_category)


#only looking at events with >100 crimes, things some of the crimes that did not make the list 
#EMBEZZLEMENT,BRIBERY,ANIMAL CRUELTY. The total list is 24 
df2021_ARSON<- filter(df2021, offense_parent_group == "ARSON")
df2021_ASSAULT_OFFENSES<- filter(df2021, offense_parent_group == "ASSAULT OFFENSES")
df2021_BURGLARY_BREAKING_ENTERING<- filter(df2021, offense_parent_group == "BURGLARY/BREAKING&ENTERING")
df2021_COUNTERFEITING_FORGERY<- filter(df2021, offense_parent_group == "COUNTERFEITING/FORGERY")
df2021_DESTRUCTION_DAMAGE_VANDALISM_OF_PROPERTY<- filter(df2021, offense_parent_group == "DESTRUCTION/DAMAGE/VANDALISM OF PROPERTY")
df2021_DRIVING_UNDER_THE_INFLUENCE<- filter(df2021, offense_parent_group == "DRIVING UNDER THE INFLUENCE")
df2021_FRAUD_OFFENSES<- filter(df2021, offense_parent_group == "FRAUD OFFENSES")
df2021_MOTOR_VEHICLE_THEFT<- filter(df2021, offense_parent_group == "MOTOR VEHICLE THEFT")
df2021_LARCENY_THEFT<- filter(df2021, offense_parent_group == "LARCENY-THEFT")
df2021_ROBBERY<- filter(df2021, offense_parent_group == "ROBBERY")
df2021_STOLEN_PROPERTY_OFFENSES<- filter(df2021, offense_parent_group == "STOLEN PROPERTY OFFENSES")
df2021_TRESPASS_OF_REAL_PROPERTY<- filter(df2021, offense_parent_group == "TRESPASS OF REAL PROPERTY")
df2021_WEAPON_LAW_VIOLATIONS<- filter(df2021, offense_parent_group == "WEAPON LAW VIOLATIONS")


#Soooo ploting these is actually a bit of a pain, classes with ~2,500 or more I would use the heat map
#but for smaller classes just the plotted map might be better

#6118 ops - BURGLARY_BREAKING_ENTERING

map + 
  stat_density2d( data = df2021_BURGLARY_BREAKING_ENTERING, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 12, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.1, .3), guide = "none") +
  ggtitle("Heat Map, BURGLARY_BREAKING_ENTERING") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0))
#interesting that the "hotspot" has moved out of the city center
#wonder if I could so a side by side

#Going to add a few here that are intresting
map + 
  stat_density2d( data = df2021_MOTOR_VEHICLE_THEFT, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 12, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.1, .3), guide = "none") +
  ggtitle("Heat Map, MOTOR_VEHICLE_THEFT") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0))

map + 
  stat_density2d( data = df2021_ASSAULT_OFFENSES, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 12, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.1, .3), guide = "none") +
  ggtitle("Heat Map, ASSAULT_OFFENSES") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0))


#318 ops - DRIVING_UNDER_THE_INFLUENCE
map +
  geom_point(data=df2021_DRIVING_UNDER_THE_INFLUENCE, aes(x=longitude, y=latitude))

#maybe do some cluster analysis on this one, find out where the groups are
#eh I do it below, I am not good with cuslters. So spoiler alert, its not that good



#color coating different offence groups 
map + 
  geom_point(data=df2021_FRAUD_OFFENSES, aes(x=longitude, y=latitude, color=offense, shape=offense_parent_group))
#hum, might need to zoom in
#annnnd missing some classes


##### Zoomed in Maps #####
#Volunteer Park + The area Around 

sea_vp = get_stamenmap(bbox = c(left = -122.335, bottom = 47.625, right = -122.3, top = 47.645), maptype = c("toner-lite"), zoom = 16)
ggmap(sea_vp)
map_zoom_volunteer_park <- ggmap(sea_vp)

#Downtown, businessy part of town (I want to try 18)
sea_downtown = get_stamenmap(bbox = c(left = -122.340, bottom = 47.607, right = -122.3325, top = 47.6115), maptype = c("toner-lite"), zoom = 18)
ggmap(sea_downtown)
map_zoom_downtown <- ggmap(sea_downtown)


######################################################
#### Grouping smaller classes in an "other group" ####
#I wish I could write better code.... oh well here we are and it works 

df2021$offense_group_small <- ifelse(df2021$offense_parent_group=="BURGLARY/BREAKING&ENTERING" | df2021$offense_parent_group=="ASSAULT OFFENSES"
                                     | df2021$offense_parent_group=="DESTRUCTION/DAMAGE/VANDALISM OF PROPERTY" |  df2021$offense_parent_group=="FRAUD OFFENSES"
                                     | df2021$offense_parent_group=="LARCENY-THEFT" | df2021$offense_parent_group=="MOTOR VEHICLE THEFT"
                                     , df2021$offense_parent_group,
                                     ifelse(df2021$offense_parent_group == "ANIMAL CRUELTY" | df2021$offense_parent_group == "ARSON" 
                                            | df2021$offense_parent_group == "BAD CHECKS" |  df2021$offense_parent_group == "BRIBERY"
                                            |df2021$offense_parent_group == "COUNTERFEITING/FORGERY" | df2021$offense_parent_group == "CURFEW/LOITERING/VAGRANCY VIOLATIONS" 
                                            | df2021$offense_parent_group == "DRIVING UNDER THE INFLUENCE" | df2021$offense_parent_group == "DRUG/NARCOTIC OFFENSES" 
                                            | df2021$offense_parent_group == "DRUNKENNESS" | df2021$offense_parent_group == "EMBEZZLEMENT" 
                                            |df2021$offense_parent_group == "EXTORTION/BLACKMAIL"  |df2021$offense_parent_group == "LIQUOR LAW VIOLATIONS"  
                                            |df2021$offense_parent_group == "PORNOGRAPHY/OBSCENE MATERIAL" | df2021$offense_parent_group == "PROSTITUTION OFFENSES" 
                                            | df2021$offense_parent_group == "ROBBERY"| df2021$offense_parent_group == "STOLEN PROPERTY OFFENSES"  
                                            | df2021$offense_parent_group == "TRESPASS OF REAL PROPERTY"| df2021$offense_parent_group == "WEAPON LAW VIOLATIONS" 
                                      , "OTHER", NA))


#For Counts 
counts_offense_pg <- count(df2021,offense_group_small)
print(counts_offense_pg)
#lets try this again

#cool, unfortunately shaooe for "Other" so might need to group one more into that
map_zoom_volunteer_park +
  geom_point(data=df2021, aes(x=longitude, y=latitude, color=offense_group_small, shape=offense_group_small, size = 1.1)) +
  ggtitle("Offense Type, Vollenteer Park") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 

map_zoom_downtown +
  geom_point(data=df2021, aes(x=longitude, y=latitude, color=offense_group_small, shape=offense_group_small, size=1.8))
#well cool, that worked as well as I could have hoped for

#would be interesting to use a heat map on the zoomed in data
#that might be better to add after I clean all this code up

#and yes, I know this heat thing plays weird with water/areas that are not smooth
map_zoom_volunteer_park + 
  stat_density2d( data = df2021, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins =12, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.1, .3), guide = "none") +
  ggtitle("Heat Map, Volunteer Park") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 

# This is the "bare bones one, which might be harder to read quickly but might be more accurate
# map_zoom_volunteer_park + 
#   stat_density2d( data = df2021, aes(x = longitude, y = latitude), size = 1, bins = 15) +
#   ggtitle("Line Density Map, Downtown") +
#   theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 


#oh I actually really like this map.....
map_zoom_downtown + 
  stat_density2d( data = df2021, aes(x = longitude, y = latitude), size = 1, bins = 15) +
ggtitle("Line Density Map, Downtown") +
  theme(plot.title = element_text(family="Helvetica", size=18, face="bold", hjust=0)) 

############################################
#### Lets try some shit with clustering ####
#this sorta works with arson... not really sure how well it works for other classes


df2021_K_cuslter = data.frame(df2021_ARSON$latitude, df2021_ARSON$longitude)
TheVariance=apply(df2021_K_cuslter,2,var)
WithinClusterSumOfSquares = (nrow(df2021_K_cuslter)-1)*sum(TheVariance)
for (i in 2:15) {
  ClusterInfo=kmeans(df2021_K_cuslter, centers=i)
  WithinClusterSumOfSquares[i] = sum(ClusterInfo$withinss)
}
 plot(1:15, WithinClusterSumOfSquares, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
#looks like 5 or 6 for this one.... but I am going to go with 12....
 

 k = kmeans(df2021_K_cuslter, 12) 
 k$centers
 table(k$cluster)
k$cluster
plot(k$cluster)
 
  glimpse(ClusterInfo)
 
 
#oh shit that kinda worked
map +
   geom_point(data=df2021_K_cuslter, aes(x=df2021_ARSON$longitude, y=df2021_ARSON$latitude, color = k$cluster)) 
#I kinda like sending it on a ton of clusters... lets you isolate the two arsons in greenwood and west seattle from the rest
#but not really sure what to do with this inforamtion

#all data now, this is fun... but not useful haha


df2021_K_cuslter_2 = data.frame(df2021$latitude, df2021$longitude)
TheVariance=apply(df2021_K_cuslter_2,2,var)
WithinClusterSumOfSquares = (nrow(df2021_K_cuslter_2)-1)*sum(TheVariance)
for (i in 2:15) {
  ClusterInfo=kmeans(df2021_K_cuslter_2, centers=i)
  WithinClusterSumOfSquares[i] = sum(ClusterInfo$withinss)
}
plot(1:15, WithinClusterSumOfSquares, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
#looks like 5 or 6 for this one.... but I am going to try 15 


k = kmeans(df2021_K_cuslter_2, 15) 
k$centers
table(k$cluster)
k$cluster
plot(k$cluster)

glimpse(ClusterInfo)


### Size of the points has been funny before on this one, making the chart unreadable ###
map +
  geom_point(data=df2021_K_cuslter_2, aes(x=df2021.longitude, y=df2021.latitude, color = k$cluster, alpha=.02)) 

#kinda cool to see crime broken up into districts based on natural barriers (freeways, highways, waterway, etc.)
#but kinda hard to tell the differences here between colors 

##########################################
######### Final Takeaways ################

#Before I start I should mention that none of this is close to scientific, just a fun thing I did.
#This is not really meant to be some extensive crime analysis.

#First takeaway, would be that most crime is downtown. It really depends on the kinds of crime.
#Second might be that more crime seems to take place around major roads/highways
#I really need a pop density map to do crimes/capita in different parts of town
#It might be good to go into how time affects crimes, or using lat/long to filter crimes (so you are only looking at crimes in your local area)

#To get some really good info, you might need to dig in alot more to something like arson or a particular crime. See if there is some pattern there.

#I do plan on working with some time series stuff, that might be kinda cool







