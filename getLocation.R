#Libraries required for getting Location
library(sp)
library(rworldmap)
library(RgoogleMaps)
library(ggmap)
library(sp)
library(rgdal)
library(rworldxtra)



# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  #get map
  countriesSP <- getMap(resolution='low')
    
  #setting Coordinate Reference System directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}


#Get the Lat and Lon
kochLoc <- data.frame(na.omit(koch$longitude),na.omit(koch$latitude))
rajiniLoc <- data.frame(na.omit(rajini$longitude),na.omit(rajini$latitude))
soundaryaLoc <- data.frame(na.omit(soundarya$longitude),na.omit(soundarya$latitude))


#Rename to standard name
names(kochLoc)[1]<-paste("Lon")
names(kochLoc)[2]<-paste("Lat")

names(rajiniLoc)[1]<-paste("Lon")
names(rajiniLoc)[2]<-paste("Lat")

names(soundaryaLoc)[1]<-paste("Lon")
names(soundaryaLoc)[2]<-paste("Lat")

#Consolidating all location
Loc1 <- rbind(kochLoc, rajiniLoc)
Location <- rbind(Loc1,soundaryaLoc)

#head(Location)

# set up some points to test
points = data.frame(Location$Lon, Location$Lat)


# get a list of country names
countries <- coords2country(points)
countryList <-unique(data.frame(countries))

