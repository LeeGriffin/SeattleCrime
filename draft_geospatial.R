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


#################################
#Good old data  cleaning and setup
#and I will include the getting of maps in this section as well
#################################

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



glimpse(df2021)
#Just noticed crime_against_category["SOCIETY"] wow hum wonder what that was (Will come back to this later )


ggplot() +
  geom_point(data=df2021, aes(x=longitude, y=latitude))


#shit that worked fuck yeah,  but this is San Fran, Not important for this
#sf = get_stamenmap(bbox = c(left = -122.5164, bottom = 37.7066, right = -122.3554, top = 37.8103), maptype = c("toner-lite"), zoom = 13)
#ggmap(sf)

#using stamenmap, here is a link on how to use it and common issues that might come up
#https://rdrr.io/cran/ggmap/man/get_stamenmap.html

#Seattle, feel free to adjust the size of the map, I just kinda winged it, probably forgot some places
sea_test = get_stamenmap(bbox = c(left = -122.45, bottom = 47.55, right = -122.25, top = 47.7), maptype = c("toner-lite"), zoom = 13)
ggmap(sea_test)
map <- ggmap(sea_test)

#oh fuck yeah it is on the map
map +
  geom_point(data=df2021, aes(x=longitude, y=latitude))

#################################
#Maps n' shit
#first is pretty normal, with semi-translutant dots
#two is a heat map that might be cool in some places but is hard to read with this data
#three is a heat map, which is super cool actually
#################################

#oh fuck yeah again, that looks even better
map + 
geom_point(data=df2021, aes(x=longitude, y=latitude), color="dark green", alpha=.03, size=1.1)

#################################
#  tile border mapped to density
#thanks to the person who wrote this code out
# ah this is not all that readable
#################################
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



#################################
#####annnnd here is the heat map
#################################
map + 
  stat_density2d( data = df2021, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 50, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.2, .3), guide = "none") +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) 

#If you wanted a topagrpahical look at all
#map + stat_density2d( data = df2021, aes(x = longitude, y = latitude), size = 1, bins = 15) 



#################################
#Looking into different types of crimes now
#################################


counts <- count(df2021,offense_parent_group)

#not going to keep looking into this one, not alot of info to pull from three vars
#counts_crime_against_categroy <- count(df2021,crime_against_category)

#ooooooof this is a terible histogram. But it gvies me the information that I need sooooo there it is
ggplot(counts, aes(counts$offense_parent_group, counts$n)) +
  geom_boxplot() +
  coord_flip()
#So theft as a whole is most common
#Lets break these out


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

map + 
  stat_density2d( data = df2021_TRESPASS_OF_REAL_PROPERTY, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 50, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.2, .3), guide = "none") +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) 

map +
  geom_point(data=df2021_MOTOR_VEHICLE_THEFT, aes(x=longitude, y=latitude))

#These might not have been as useful as I thought
glimpse(df2021_MOTOR_VEHICLE_THEFT)


#colorcoatingmy diffence offence groups 
map_zoom + 
  geom_point(data=df2021_MOTOR_VEHICLE_THEFT, aes(x=longitude, y=latitude, color=offense_parent_group, shape=offense_parent_group))

#hum, might need to zoom in
#North Cap Hill, Volunteer park

sea_vp = get_stamenmap(bbox = c(left = -122.335, bottom = 47.625, right = -122.3, top = 47.645), maptype = c("toner-lite"), zoom = 16)
ggmap(sea_vp)
map_zoom_north_seattle <- ggmap(sea_vp)

#Downtown, close to where I work
sea_downtown = get_stamenmap(bbox = c(left = -122.340, bottom = 47.607, right = -122.3325, top = 47.6115), maptype = c("toner-lite"), zoom = 17)
ggmap(sea_downtown)
map_zoom_downtown <- ggmap(sea_downtown)

#Some good stuff here, but I think that I need to combine crimes into an "other group"
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

map +
  geom_point(data=df2021_MOTOR_VEHICLE_THEFT, aes(x=longitude, y=latitude))

#cool, unforunally "Other" so might need to group one more into that
map_zoom_north_seattle +
  geom_point(data=df2021, aes(x=longitude, y=latitude, color=offense_group_small, shape=offense_group_small, size = 1.1))

map_zoom_downtown +
  geom_point(data=df2021, aes(x=longitude, y=latitude, color=offense_group_small, shape=offense_group_small, size=1.8))
#well cool, that worked as well as I could have hoped for

#would be intresting to use a heat map on the zoomed in data
#that might be better to add after I clean all this code up



#Lets try some shit with clustering
#this sorta workd with arson... but might be more intresting to have other 
df2021_K_cuslter = data.frame(df2021_MOTOR_VEHICLE_THEFT$latitude, df2021_ARSON$longitude)
TheVariance=apply(df2021_K_cuslter,2,var)
WithinClusterSumOfSquares = (nrow(df2021_K_cuslter)-1)*sum(TheVariance)
for (i in 2:15) {
  ClusterInfo=kmeans(df2021_K_cuslter, centers=i)
  WithinClusterSumOfSquares[i] = sum(ClusterInfo$withinss)
}
 plot(1:15, WithinClusterSumOfSquares, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
#looks like 5 or 6 for this one
 

 k = kmeans(df2021_K_cuslter, 5) 
 k$centers
 table(k$cluster)
k$cluster
plot(k$cluster)
 
  glimpse(ClusterInfo)
 
 
 ###oh shit that kinda worked
map +
   geom_point(data=df2021_K_cuslter, aes(x=df2021_ARSON.longitude, y=df2021_ARSON.latitude, color = k$cluster, size=1)) 





df2021_K_cuslter_2 = data.frame(df2021$latitude, df2021$longitude)
TheVariance=apply(df2021_K_cuslter_2,2,var)
WithinClusterSumOfSquares = (nrow(df2021_K_cuslter_2)-1)*sum(TheVariance)
for (i in 2:15) {
  ClusterInfo=kmeans(df2021_K_cuslter_2, centers=i)
  WithinClusterSumOfSquares[i] = sum(ClusterInfo$withinss)
}
plot(1:15, WithinClusterSumOfSquares, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
#looks like 5 or 6 for this one


k = kmeans(df2021_K_cuslter_2, 5) 
k$centers
table(k$cluster)
k$cluster
plot(k$cluster)

glimpse(ClusterInfo)


###Lets try again here, hum size seems to be off. Maybe by refreshing I will fix stuff
map +
  geom_point(data=df2021_K_cuslter_2, aes(x=df2021.longitude, y=df2021.latitude, color = k$cluster, alpha=.02)) 


