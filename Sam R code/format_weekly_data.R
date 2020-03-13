#format_weekly_data takes the filtered eBird datafile and assigns all records to geographical zones
#and weekly timesteps
#to get the filtered data, first run getData.R

library(ptinpoly)
library(plyr)
library(maps)
library(ggplot2)

basedir <-  'C:/Users/nic24k/Dropbox/Migratory birds network reconstruction'
scriptdir <- './Sam R code'
datadir <- './Data files'
plotdir <- './Plots'
setwd(basedir)

species <- 'far eastern curlew'
speciesdir <- paste(datadir,'/eBird sightings data/', species, sep= "")

##
#import locations of IBAs
setwd(basedir)
setwd(datadir)
IBA_locations <- read.csv(file= "EAA_ID_sites.csv", header= TRUE, sep= ",")
setwd(basedir)


#manually add in polygons to match the images in Supp Info 1 of Tak's paper

regionnames <- c("Breeding", "YS", "SK-JPN", "MSIA-INDO","NW_AUS","N_AUS","SE_AUS","S_AUS", "NK-YS")
values <- data.frame(
  id = seq(1,length(regionnames),1),
  value = regionnames
)

positions <- data.frame(
  id = c( rep(values$id[1], 4),
          rep(values$id[2], 5),
          rep(values$id[3], 5),
          rep(values$id[4], 9),
          rep(values$id[5], 4),
          rep(values$id[6], 8),
          rep(values$id[7], 5),
          rep(values$id[8], 4),
          rep(values$id[9],7)),
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

###
setwd(basedir)
setwd(scriptdir)
#get the eBird data
eBirddat <- readRDS("eastern_curlew_all_eBird")  #could also try read_ebd function here (from auk package)
####
eBirddat <- eBirddat[-which(eBirddat$observation_count== "X"),]
eBirddat$observation_count <- as.numeric(eBirddat$observation_count)
#find any NAs and remove:
#eBirddat <- eBirddat[-which(is.na(eBirddat$observation_count), arr.ind=TRUE),]

#extract year from date and remove very old records
eBirddat$year <- as.numeric(as.character(substr(eBirddat$observation_date,0,4) ))
eBirddat$month <- as.numeric(as.character(substr(eBirddat$observation_date,6,7) ))
eBirddat$day <- as.numeric(as.character(substr(eBirddat$observation_date,9,10) ))
eBirddat$week <- as.numeric(format(eBirddat$observation_date, format= "%U"))#"%Y-%U")  #assign the week number

#remove all records where species has never been sighted
oldestyear <- 1980  #set oldest year of records that we want to include
eBirddat <- eBirddat[- which(eBirddat$year < oldestyear),]

#check for NAs in eBird.queries (i.e. unreported locations) 
NA_lat <- which(is.na(eBirddat$latitude))
NA_long <- which(is.na(eBirddat$longitude))
NAs_all <- union(NA_lat, NA_long)
#remove these rows
if (length(NAs_all)>0){
  eBirddat <- eBirddat[-NAs_all,]
}

eBirddat$locality_id <- as.factor(eBirddat$locality_id)
#aggregate observation data by locality id, find zero entries (i.e. species never observed at locality) and remove
y <- aggregate(eBirddat$observation_count, by=list(Locality=eBirddat$locality_id), FUN=sum)
y <- y[y$x==0,]  #this is the list of locations where the species has never been recorded
y$Locality <- as.factor(y$Locality)  #y is a list of locations that we need to drop-- species never historically seen here

#crop the list so that we only keep the records at sites where birds have been observed at least once since observation started
eBirddat2 <-eBirddat[!(eBirddat$locality_id %in% y$Locality),]

#save eBirddat2, the full dataset relevant to the species
filename1 <- paste(species, "_all_records.csv", sep="")
setwd(basedir)
setwd(datadir)
  write.csv(eBirddat2, file = filename1)
setwd(basedir)
setwd(scriptdir)

#assign each record to a block based on position
#create a matrix of lat and long query data points from eBirddat
eBird.queries <- cbind(eBirddat2$latitude, eBirddat2$longitude)

valid.records <- list()
weekly.counts <- list()
for (i in 1:length(regionnames)){
  vertices <- as.matrix(subset(positions, id== i)[,2:3])
  
  #test if the data are in the site, return the row numbers if so
  testif.in <- eBirddat2[pip2d(vertices, eBird.queries)>=0,]
  valid.records[[i]] <- testif.in
  #summarise the results by getting the count of birds each week (and report the number of lists)
  weekly.counts[[i]] <- ddply(valid.records[[i]], .(week), summarize, obsCount=sum(observation_count), nLists= length(week))
  weekly.counts[[i]]$region <- regionnames[i]
  }

#find all records that are not in any of the polygons
invalid.records <- list()
invalid.records <- eBirddat2
for (i in 1:length(regionnames)){
  invalid.records <- invalid.records[!(invalid.records$locality_id %in% valid.records[[i]]$locality_id),]
}

#flatten weekly counts for export to csv
weekly.countsMat <- do.call("rbind", weekly.counts)
#save weekly counts data as csv, the full dataset relevant to the species
filename2 <- paste(species, "_weekly_regioncount.csv", sep="")
setwd(basedir)
setwd(datadir)
write.csv(weekly.countsMat , file = filename2)
setwd(basedir)
setwd(scriptdir)



#evaluate the proportion of sightings captured by each of the regions, and the proportion of sightings not in the regions
evaluate.regions <- list(regionID=integer(),
                                regionname =character(),
                                totbirds=integer(),
                                totlists=integer()) 
for (i in 1:length(regionnames)){
  evaluate.regions$regionID[i] <- i
  evaluate.regions$regionname[i] <- regionnames[i]
  evaluate.regions$totbirds[i] <- sum(valid.records[[i]]$observation_count)
  evaluate.regions$totlists[i] <- length(valid.records[[i]]$checklist_id)
}
evaluate.regions$regionID[(length(regionnames)+1)] <- NA
evaluate.regions$regionname[(length(regionnames)+1)] <- NA
#need to avoid double counting chinese birds >_<
evaluate.regions$totbirds[(length(regionnames)+1)] <- sum(invalid.records$observation_count)
evaluate.regions$totlists[(length(regionnames)+1)] <- length(invalid.records$checklist_id)
evaluate.regions <- as.data.frame(evaluate.regions)


############# map the remaining locations
wldmap <- map("world2", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0), asp= 1)
#stick in in GGplot where we can set the coordinates better
wldGGplot <- ggplot() + geom_polygon(data = wldmap, aes(x=long, y = lat, group = group), fill=NA, color= 'black') + 
  coord_fixed(1.3)
#crop to desired region by fixing xlim
wldGGplot2 <- wldGGplot + coord_fixed(xlim = c(85, 205),  ylim = c(-50,70), ratio = 1)

#plot(wldGGplot2)
eBirddat2$longitude_adjust <- eBirddat2$longitude
eBirddat2$longitude_adjust[eBirddat2$longitude_adjust < 0] <- eBirddat2$longitude_adjust[eBirddat2$longitude_adjust < 0]  +360  #adjust Alaskan sites with -ve longitude to coordinate system

invalid.records$longitude_adjust <- invalid.records$longitude
invalid.records$longitude_adjust[invalid.records$longitude_adjust < 0] <- invalid.records$longitude_adjust[invalid.records$longitude_adjust < 0]  +360  #adjust Alaskan sites with -ve longitude to coordinate system


wldGGplot3 <- wldGGplot2 + 
  geom_point(data = eBirddat2, aes(x = longitude_adjust, y = latitude), color = "red", size = 0.5)+
  geom_point(data = invalid.records, aes(x = longitude_adjust, y = latitude), color = "blue", size = 0.5)
  

#plot the polygons to make sure they cover the correct areas
wldGGplot4 <- wldGGplot3+
  geom_polygon(data= datapoly, aes(x= xLong, y= yLat, group= id), colour= "red", alpha= 0.2)

plot(wldGGplot4)

############ plot weekly data in each region

nplotrows <- 3 #specify how many rows for the plot (user defined based on number of regions)
nplotcols <- 3 # as above but for columns


setwd(basedir)
setwd(plotdir)

filename <- paste(species, "_total_count.png", sep="")
png(filename)
par(mfrow= c(nplotrows, nplotcols))
  for (n in 1:(length(regionnames))){
    xvals <- weekly.counts[[n]]$week
      plot(xvals, weekly.counts[[n]]$obsCount, pch=n, type= "b", xlab= "week number", ylab= "total count", main= regionnames[n])
  }
mtext("Total counts", side = 3, line = -1.5, outer = TRUE)
dev.off()

filename <- paste(species, "_total_lists.png", sep="")
png(filename)
par(mfrow= c(nplotrows, nplotcols))
for (n in 1:length(regionnames)){
  xvals <- weekly.counts[[n]]$week
  plot(xvals, weekly.counts[[n]]$nLists, pch=n, type= "b", xlab= "week number", ylab= "number of lists",main= regionnames[n])
}
mtext("Total Lists", side = 3, line = -1.5, outer = TRUE)
dev.off()

filename <- paste(species, "_avg_count_perList.png", sep="")
png(filename)
par(mfrow= c(nplotrows, nplotcols))
for (n in 1:length(regionnames)){
  xvals <- weekly.counts[[n]]$week
  plot(xvals, weekly.counts[[n]]$obsCount/weekly.counts[[n]]$nLists, pch=n, type= "b", xlab= "week number", ylab= "avg total count per list", main= regionnames[n])
}
dev.off()

setwd(basedir)
setwd(scriptdir)