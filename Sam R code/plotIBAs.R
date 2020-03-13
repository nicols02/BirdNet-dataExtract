#this script maps the important bird areas onto a global map

basedir <-  'C:/Users/nic24k/Dropbox/Migratory birds network reconstruction'
scriptdir <- './Sam R code'
datadir <- './Data files'
setwd(basedir)

species <- 'far eastern curlew'
speciesdir <- paste(datadir,'/eBird sightings data/', species, sep= "")

library(maps)
library(mapdata)
library(ggplot2)
library(ptinpoly)

#mapdat <- data(world2MapEnv)
#plot(mapdat)




wldmap <- map("world2", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0), asp= 1)

#stick in in GGplot where we can set the coordinates better
wldGGplot <- ggplot() + geom_polygon(data = wldmap, aes(x=long, y = lat, group = group), fill=NA, color= 'black') + 
  coord_fixed(1.3)
#crop to desired region by fixing xlim
wldGGplot2 <- wldGGplot + coord_fixed(xlim = c(85, 205),  ylim = c(-50,70), ratio = 1)
   
#import locations of IBAs
setwd(datadir)
IBA_locations <- read.csv(file= "EAA_ID_sites.csv", header= TRUE, sep= ",")
setwd(basedir)
plotLongs <- IBA_locations$Longitude
plotLongs[plotLongs < 0] <- plotLongs[plotLongs < 0]  +360  #adjust Alaskan sites with -ve longitude to coordinate system
IBA_locations$Adj_Longitude <- plotLongs
#points(plotLongs, IBA_locations$Latitude, col = "red", cex = .6)
wldGGplot3 <- wldGGplot2 + 
  geom_point(data = IBA_locations, aes(x = Adj_Longitude, y = Latitude), color = "red", size = 0.75)


#plot IBA locations of species
setwd(speciesdir)  #read in species location data
filename <- paste(species, '_IBAs.csv', sep="")
species_locations <- read.csv(filename, header= TRUE, sep= ',')
setwd(basedir)
#add in lat/long data to the species locations
sp_locations <- merge(species_locations, IBA_locations, by.x= "EAA_ID", by.y= "SiteID")

#check to make sure the merge picked up all the sites-- if not, need to cross-check using the line of code below
#this line identifies which locations are missing from the dataset
#species_locations[!species_locations$EAA_ID%in%sp_locations$EAA_ID,]

plotLongs_sp <- sp_locations$Longitude
plotLongs_sp[plotLongs_sp <0] <-  plotLongs_sp[plotLongs_sp <0] +360  #adjust AK sites
plotLongs_sp$Adj_Longitude <- plotLongs
sp_locations$BLOCK <- as.factor(sp_locations$BLOCK)  #factor
#points(plotLongs_sp, sp_locations$Latitude, col = "blue", cex = .6)
wldGGplot4 <- wldGGplot3 + 
  geom_point(data = sp_locations, aes(x = Adj_Longitude, y = Latitude, color = BLOCK), size = 2)

#manually add in polygons to match the images in Supp Info 1 of Tak's paper

regions <- c("BREED", "YS", "SK-JPN", "MSIA-IND","NWAUS","NAUS","SEAUS","SAUS", "NK-YS")

ids <- factor(levels(sp_locations$BLOCK))
values <- data.frame(
  id = ids,
  value = c("BREED", "YS", "SK-JPN", "MSIA-IND","NWAUS","NAUS","SEAUS","SAUS", "NK-YS")

)

positions <- data.frame(
  id = c( rep(ids[1], 4),
          rep(ids[2], 5),
          rep(ids[3], 5),
          rep(ids[4], 9),
          rep(ids[5], 4),
          rep(ids[6], 8),
          rep(ids[7], 5),
          rep(ids[8], 4),
          rep(ids[9],7)),
  yLat = c(45.72,65,65, 45.72,
           28,44,44,34,28,
           29, 36,38,38,29,
           -12,6.3,6.3,7.9,7.9,3,-2,-8.45,-12,
           -23.5,-13,-13,-23.5,
           -21,-10,-10,0,0,-17,-17,-21,
           -37.5, -21, -17,-17,-37.5,
           -45, -35, -37.5,-45,
           28,44,44,38,38,36,28),
    xLong = c(135, 135,170,170,  #breeding
          115, 115, 130,130,122, #yellow sea: southward route
          124,124,126,142,142,  #s korea border +japan: northward route
          93, 93, 105,105,120, 120,118,115.43, 115.43, #malaysia, w.indonesia to wallace line
          112,112, 129, 129,  #NW Australia: WA border to tropic of capricorn
          129,129,141,141,153,153,145.77,145.77, #N Australia to Cairns +PNG
          150,145.77,145.77,160,160, #SE Australia to Cairns and NSW/Victorian border at coast
          138.5, 138.5,150,150, #Southern Australia: Victorian coastline to Adelaide
          115,115,130,130,126,124,124) #North Korea and Yellow Sea: northward route

)

datapoly <- merge(values, positions, by=c("id"))

#plot the polygons to make sure they cover the correct areas
wldGGplot4+
  geom_polygon(data= datapoly, aes(x= xLong, y= yLat, group= id), colour= "red", alpha= 0.2)
  #geom_polygon(data=overlay, aes(x=long, y=lat, group=group), color="red", alpha=0)

#----------------------------
#now load eastern curlew eBird data
setwd(speciesdir)
eBirddat <- read.table(file= "ebd_faecur_relAug-2018.txt", header= TRUE, sep= '\t', quote="", fill=TRUE)
setwd(basedir)

#pull out the columns that we actually will use, dump the rest:
eBirddat <- eBirddat[, c("GLOBAL.UNIQUE.IDENTIFIER", "SCIENTIFIC.NAME","OBSERVATION.COUNT", 
                          "COUNTRY", "COUNTRY.CODE", "STATE", "LOCALITY", "LOCALITY.ID", "LATITUDE", 
                          "LONGITUDE", "OBSERVATION.DATE", "PROTOCOL.TYPE", "DURATION.MINUTES", 
                          "EFFORT.DISTANCE.KM", "EFFORT.AREA.HA", "NUMBER.OBSERVERS")] 
#remove records with no observation count and coerce from factor to numeric
eBirddat <- eBirddat[-which(eBirddat$OBSERVATION.COUNT=="X"),]
eBirddat$OBSERVATION.COUNT <- as.numeric(eBirddat$OBSERVATION.COUNT)

#extract year from date and remove very old records
eBirddat$year <- as.numeric(as.character(substr(eBirddat$OBSERVATION.DATE,0,4) ))
eBirddat$month <- as.numeric(as.character(substr(eBirddat$OBSERVATION.DATE,6,7) ))
eBirddat$day <- as.numeric(as.character(substr(eBirddat$OBSERVATION.DATE,9,10) ))

oldestyear <- 1980  #set oldest year of records that we want to include
eBirddat <-  eBirddat[-which(is.na(eBirddat$year)),]
eBirddat <- eBirddat[- which(eBirddat$year < oldestyear),]

#check for NAs in eBird.queries (i.e. unreported locations) 
NA_lat <- which(is.na(eBirddat$LATITUDE))
NA_long <- which(is.na(eBirddat$LONGITUDE))
NAs_all <- union(NA_lat, NA_long)
#remove these rows
if (length(NAs_all)>0){
  eBirddat <- eBirddat[-NAs_all,]
}


#create a matrix of lat and long query data points from eBirddat
eBird.queries <- cbind(eBirddat$LATITUDE, eBirddat$LONGITUDE)

valid.records <- list()
for (i in 1:length(regions)){
  vertices <- as.matrix(subset(positions, id== i)[,2:3])
  
  #test if the data are in the site, return the row numbers if so
  testif.in <- eBirddat[pip2d(vertices, eBird.queries)>=0,]
  valid.records[[i]] <- testif.in
}
##
