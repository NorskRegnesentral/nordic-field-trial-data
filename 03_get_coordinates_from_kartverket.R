### OBS! This script use data files that are not publicly available ###
### To accsess the additional data files, please contact smvandeskog@nr.no ###

# get coordinates for Norwegian field trials from NIBIO based on 
# kartverket object with postal code polygons

datadir = "./barley-files-from-norway/"

library(data.table)

# load data
all_files = list.files(datadir)
data_list = list()
for (i in seq_along(all_files)){
  
  yr = paste0("20", substr(all_files[i],8,9))
  data = readxl::read_xlsx(paste0(datadir, all_files[i]),.name_repair = "minimal", col_names = FALSE)
  
  tmp = data[, 1:8]
  colnames(tmp) = tmp[3,]
  tmp = tmp[-c(1:3),]
  
  data_list2 = list()
  for(j in 9:ncol(data)){
    tmp2 = data.table::copy(tmp) |> data.table::as.data.table()
    tmp2$st = data[1,j]
    tmp2$P0 = data[2,j]
    tmp2$X = data[4:nrow(data),j]
    setnames(tmp2, "X", as.character(data[3,j]))
    
    data_list2[[length(data_list2)+1]] = tmp2
  }
  
  data = data.table::rbindlist(data_list2, fill = TRUE)
  
  data$year = as.integer(yr)
  
  data_list[[i]] = data
  
}

data = data.table::rbindlist(data_list, fill = TRUE)

# change from character to numeric
changeCols <- colnames(data)[which(as.vector(data[,lapply(.SD, class)]) == "character")]
changeCols = changeCols[!(changeCols %in% c("Forsøk", "Adresse", "Sort", "st", "P0"))]
data[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]

# update datadir
datadir = ""

# get postal code info
data[, postnr:=substr(Adresse, regexpr("\\d{4}", Adresse), regexpr("\\d{4}", Adresse) + 3)]
data[grep("Graminor AS", Adresse), postnr := "2322"]
data[grep("NLR Midt", Adresse), postnr := "7383"] # could be wrong - no exact address available
data[Nord == 715162, Nord := 7151620] # fix typo

# keep only location columns
data = unique(data[, .SD, .SDcols = c("Forsøk", "Felt", "Adresse", "UTM-sone" , "Øst", "Nord", "year", "postnr")])
data[, provided := ifelse(!is.na(Nord), "NIBIO", "NR")]

# Transform UTM32 to 33
ix32 = which(data$`UTM-sone` == 32)

# Define the CRS for UTM zone 32N (EPSG:25832)
utm32n <- sf::st_crs(25832)
# Define the CRS for UTM zone 33N (EPSG:32633)
utm33n <- sf::st_crs(32633)

# Create sf points
sf_points_32 <- sf::st_as_sf(data[ix32], coords = c("Øst", "Nord"), crs = utm32n)
# Transform the coordinates to UTM zone 33N
sf_points_33 <- sf::st_transform(sf_points_32, crs = utm33n)

# Extract coordinates from the sf object
coords_33 <- sf::st_coordinates(sf_points_33)

# Convert to a data frame
data[ix32, Øst := coords_33[,1]]
data[ix32, Nord := coords_33[,2]]

data$`UTM-sone` = 33

# load polygon info based on postal codes 
postnr = sf::st_read(paste0(datadir,"postnr-koord.geojson"))
keep = which(postnr$postnummer %in% data$postnr)

# find centroids of each polygon
centroids = sf::st_centroid(postnr[keep,]$geometry)

# Assuming sf_points is your sf object
coords <- sf::st_coordinates(centroids)

# Convert to a data frame
sf_data <- data.table::as.data.table(coords)

# add postnr
sf_data$postnr = postnr[keep,]$postnummer
data[sf_data, `:=` (Øst = i.X, Nord = i.Y), on = "postnr"]

# Create sf points
sf_points_utm33 <- sf::st_as_sf(data, coords = c("Øst", "Nord"), crs = utm33n)

# Define the CRS for WGS 84 (EPSG:4326)
wgs84 <- sf::st_crs(4326)

# Transform the coordinates to longitude and latitude
sf_points_lonlat <- sf::st_transform(sf_points_utm33, crs = wgs84)

# Extract coordinates from the sf object
coords_lonlat <- sf::st_coordinates(sf_points_lonlat)

data$long <- coords_lonlat[,1]
data$lat <- coords_lonlat[,2]

# add counties
fylker = data.frame(
  rad = 1:20,
  fylke = c("Østfold", "Akershus", "Oslo", "Hedmark",
            "Oppland", "Buskerud", "Vestfold", "Telemark", "Aust-Agder", 
            "Vest-Agder", "Rogaland", "Hordaland", "", 
            "Sogn og Fjordane", "Møre og Romsdal",
            "Sør-Trøndelag", "Nord-Trøndelag", "Nordland",
            "Troms", "Finnmark")
)

fylker$location_code = rep(NA,20)
fylker[1:9,]$location_code = paste0("county_nor0",1:9)
fylker[10:20,]$location_code = paste0("county_nor",10:20)

normap = csmaps::nor_county_map_b2017_default_sf

# Define the CRS for UTM zone 33N (EPSG:32633)
utm33n <- sf::st_crs(32633)
sf::st_crs(normap) = utm33n

data_sf <- sf::st_as_sf(data, coords = c("Øst", "Nord"), crs = utm33n)
data_with_regions <- sf::st_join(data_sf, normap, join = sf::st_within)
data[, location_code := data_with_regions$location_code]
data.table::setDT(fylker)
data[fylker, fylke := i.fylke, on = "location_code"]

# add municipalities -------------------------------------------------------------
normap = csmaps::nor_municip_map_b2024_default_sf
norloc = csdata::nor_locations_names()

# Define the CRS for UTM zone 33N (EPSG:32633)
utm33n <- sf::st_crs(32633)
normap = sf::st_transform(normap, crs = utm33n)

data_sf <- sf::st_as_sf(data, coords = c("Øst", "Nord"), crs = utm33n)
data_with_regions <- sf::st_join(data_sf, normap, join = sf::st_within)
data[, location_code := data_with_regions$location_code.y]
data[norloc, kommune := i.location_name, on = "location_code"]

data[is.na(fylke), fylke := "Nord-Trøndelag"] # missing 1 location (Steinkjer)
data[is.na(kommune)]

data.table::fwrite(data[, .(Forsøk, year, Felt, fylke, kommune, Adresse, postnr, `UTM-sone`, Øst, Nord, long, lat)] |> unique(), paste0(datadir, "norway-barley-coords-trials.csv"))
