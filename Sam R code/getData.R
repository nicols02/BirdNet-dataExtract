#getData is a script that downloads species data from eBird, including absence data

#get the auk package to manipulate the eBird data. 
#more info, see: https://cran.r-project.org/web/packages/auk/vignettes/auk.html
library(auk)

#speciesName <- "Far Eastern Curlew"
speciesName <- c("Bar-tailed godwit", "Far Eastern Curlew", "Curlew Sandpiper", "Great Knot", "Gray-tailed Tattler",
                  "Lesser Sand-Plover", "Red Knot", "Terek Sandpiper")

#Check species names in ebird_taxonomy dataframe-- i.e. Limosa lapponica baueri/menzbieri; Bar-tailed Godwit (Siberian)

datadir <- 'D:/eBird'
workingdir <- 'C:/Users/nic24k/Dropbox/Migratory birds network reconstruction/Sam R code'
setwd(workingdir)

#path to ebird data file
f_ebd <- paste(datadir, '/ebd_relAug-2018/ebd_relAug-2018.txt', sep="")
f_smp <- paste(datadir, '/ebd_sampling_relAug-2018/ebd_sampling_relAug-2018.txt', sep="")

filters <- auk_ebd(f_ebd, file_sampling = f_smp) %>% 
   auk_species(species= speciesName) %>% 
    auk_country(country = c("US","Russia", "Mongolia", "China","Hong Kong", "South Korea", "North Korea",
                            "Japan","Philippines", "Vietnam","Cambodia", "Laos", "Thailand", "Myanmar",
                            "Bangladesh", "India", "Malaysia", "Singapore", "Brunei", "Indonesia","Timor leste",
                            "Papua New Guinea", "Australia", "AC","CC", "New Zealand", "FM",
                            "MP","PW","SB","VU","WS") ) %>% 
    #auk_date(date = c("1970-01-01", "2018-09-01")) %>%
    auk_complete() 
 filters  #check filters via printing
#  #auk_bbox(c(80,85,-141,-60)) %>% #-141W is the AK-Canada border; doesn't work because of date line.

  
  #define output files and run filter-- takes ages so comment out if not using
ebd_sed_filtered <- auk_filter(filters, 
                                 file = "ebd-filtered.txt",
                                 file_sampling = "sampling-filtered.txt", overwrite = TRUE)
memory.limit(size=160000000000000)

#combine datasets to get zero-filled dataset
ebd_zf <- auk_zerofill("ebd-filtered.txt", "sampling-filtered.txt" )
ebd_zf


#collapse dataframes together

ebd_zf_df <- collapse_zerofill(ebd_zf)
class(ebd_zf_df)

#get rid of unnecessary columns to reduce the memory required to store the object: list columns we want to keep
ebd_zf_df <- ebd_zf_df[, c("checklist_id", "country", "country_code", "iba_code","state", "state_code","locality_id","locality_type",
                           "latitude", "longitude", "observation_date", "sampling_event_identifier",
                           "duration_minutes", "effort_distance_km", "effort_area_ha", "number_observers", "scientific_name",
                           "observation_count", "species_observed")] 
#dump all USA records that are not from AK
ebd_zf_df <- ebd_zf_df[!(ebd_zf_df$country_code== "US" & ebd_zf_df$state != "AK"), ]

#dump any records from western Russia by setting limits on the longitude
ebd_zf_df <- ebd_zf_df[!(ebd_zf_df$longitude <80 & ebd_zf_df$longitude>= -141),]

#set limits on dates (better to do this on initial filter)?

saveRDS(ebd_zf_df, "EAAF_species_all_eBird")

#saveRDS(ebd_zf_df, "eastern_curlew_all_eBird")

#---------------------
#


 


