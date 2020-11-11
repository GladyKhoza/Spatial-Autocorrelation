install.packages("RColorBrewer")
par(mfrow=c(1,1))
plot(Locations)
library(sp)
library(maptools)
library(classInt)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(RColorBrewer)
library(raster)

#Loading Shapefile
SHP<- shapefile("GautengSuburbs.shp")
SHP$Responses = as.numeric(SHP$Responses)


#Choropeth Map
par(mfrow = c(1,1))
tm_shape(SHP) + tm_fill("Responses", palette = "Blues", n = 5) + tm_borders(alpha = .4) + 
  tm_compass()+ tm_layout(title= "Locational Distribution of Substance Abusers", 
                          frame = FALSE) + tm_legend(legend.position = c("left", "top"))

#Calculating Neighbours

neighbours <-poly2nb(SHP)
listw<-nb2listw(neighbours)
listw

#Moran's I Test

moran.test(SHP$Responses, listw)
moran.plot(SHP$Responses, listw = nb2listw(neighbours, style = "W"))
local <-localmoran(x=SHP$Responses, listw = nb2listw(neighbours, style = "W"))

#Moran's I Map
moran.map<- cbind(SHP, local)
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "Local Moran Map") 

#Geary C Test
geary.test(SHP$Responses, listw)
geary.map<-cbind(SHP, local)
tm_shape(geary.map) + tm_fill(col = "Ii", style = "quantile", title = "Geary C Map")


hist(SHP$AG_MUN_ID, main = "Number of Substance Abusers", xlab = "Responders", col = "blue")
