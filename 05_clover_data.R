### NB! This script use data files that are not publicly available ###
### To accsess the additional data files, please contact smvandeskog@nr.no ###

### Red clover data processing script ###
# script for reading in all red-clover field trial data provided from
# Norway, Finland and NFTS (Sweden + Denmark), 
# processing and combining into one dataset

library(data.table)
rm(list = ls())
datadir = "./"

# functions ----------------------------------------------------

# turn numbers < 10000 into 4-digit strings with leading zeros
add_leading_zero = function(x){
    if(as.integer(x) > 9999) stop("x should be less than 10000")
    x_new = paste0("000",x)
    return(substr(x_new, nchar(x_new)-3, nchar(x_new)))
}

# extract numbers from strings
get_numbers = function(text){
    # Extract numbers using regular expressions
    numbers <- gregexpr("[0-9]+", text)
    result <- regmatches(text, numbers)
    # Convert to numeric
    numeric_result <- as.numeric(unlist(result))
    return(numeric_result)
}

# get harvest year from string
get_number = function(text){
    # Extract numbers using regular expressions
    match <- gregexpr(". BRUGSÅR", text)
    match[[1]] = match[[1]]-1
    attr(match[[1]], "match.length") = 1
    result <- regmatches(text, match)
    numeric_result <- as.numeric(result)
    return(numeric_result)
}

# get info on which cut it is from string
get_cut_number = function(text0){
    text = substr(text0, 5, nchar(text0))
    match <- gregexpr("\\d", text)
    result <- regmatches(text, match)
    numeric_result <- as.numeric(result)
    return(numeric_result)
}


# functions for variety name curation 
update_name = function(x0){
  x = tolower(x0)
  x = gsub("_", "", x)
  x = gsub(" ", "", x)
  x = gsub("\\(ss","", x)
  return(x)
}

add_spacing = function(x, text){
  gsub(text,paste0(text," "), x)
}

# Finnish data -------------------------------------------------

# load rawdata
rawdata = readxl::read_xlsx(paste0(datadir, "clover_finland.xlsx"), sheet = 2) |> as.data.table()

# load municipality and coordinates
tmp = readxl::read_xlsx(paste0(datadir, "clover_finland.xlsx"), sheet = 1, range = "B11:K34") |> as.data.table()
setnames(tmp, c("Municipality", "Municipality (2025)"), c("municipality", "municipality_2025"))
coords = strsplit(tmp$`Coordinates`, " ")
tmp[, `:=` (longitude = sapply(coords, function(x) get_numbers(x[2])),
            latitude = sapply(coords, function(x) get_numbers(x[4])),
             koep = sapply(coords, function(x) get_numbers(x[6])))]

rawdata[tmp, `:=` (municipality = i.municipality,
            municipality_2025 = i.municipality_2025,
            longitude_kkj = as.numeric(i.longitude),
            latitude_kkj = as.numeric(i.latitude),
            koep = as.numeric(i.koep)), on = c("site" = "Site")]


# Define the CRS for KKJ (EPSG:2393)
kkj <- sf::st_crs(2393)
# Define the CRS for WGS 84 (EPSG:4326)
wgs84 <- sf::st_crs(4326)

# Create sf points
sf_points_kkj <- sf::st_as_sf(rawdata[!is.na(longitude_kkj)], coords = c("longitude_kkj", "latitude_kkj"), crs = kkj)

# Transform the coordinates to longitude and latitude
sf_points_lonlat <- sf::st_transform(sf_points_kkj, crs = wgs84)

# Extract coordinates from the sf object
coords_lonlat <- sf::st_coordinates(sf_points_lonlat)

rawdata[!is.na(longitude_kkj), `:=` (longitude = coords_lonlat[,1],
                                  latitude = coords_lonlat[,2])]

# add country variable
rawdata[, country := "Finland"]

# adjust yield from kg/ha to hkg/ha
rawdata[, dry_matter_yield := yield/100]

# Construct harvest year
# Step 1: Sort the data
setorder(rawdata, trial, site, year)
tmp = rawdata[,.(trial, site, year)] |> unique()

# Step 2: Identify breaks in consecutive years
tmp[, year_diff := c(NA, diff(year)), by = .(trial, site)]
tmp[, group_id := cumsum(is.na(year_diff) | year_diff != 1), by = .(trial, site)]

setorder(tmp, year, site)
# construct new trial variable that is unique
tmp[, trial2 := paste0("XCLFI", add_leading_zero(.GRP), "X"), by = .(trial, site, group_id)]

# Step 3: Assign harvest year within each group of consecutive years
tmp[, harvest_year := seq_len(.N), by = .(trial, site, group_id)]
rawdata = merge(rawdata, tmp, by = c("trial", "site", "year"))
setnames(rawdata, c("trial", "trial2"), c("old_trial_name", "trial"))
totalfi = rawdata[, .(dry_matter_yield_total = sum(dry_matter_yield, na.rm = TRUE)), by = .(year, site, trial, old_trial_name, cultivar, cultivar_name, municipality, municipality_2025, longitude, latitude, koep, country, harvest_year)]
rawdata = rbindlist(list(rawdata, totalfi), fill = TRUE)

# set key
setkey(rawdata, year, site, trial, cultivar_name, cut)

# calculate establishment year as recorded year - harvest year
rawdata[, establishment_year := year-harvest_year]

# fix municipality names
rawdata[is.na(municipality), `:=` (municipality = municipality_2025, municipality_2025 = NA)]
rawdata[!is.na(municipality_2025), `:=` (municipality = paste0(municipality, " (", municipality_2025, ")"))]

# manually add missing coordinates 
rawdata[municipality == "Nikkilä", `:=` (longitude = 25.26899, latitude = 60.37711)]
rawdata[municipality == "Mildola (Tuusula)", `:=` (longitude = 24.40841, latitude = 60.17627)]
rawdata[municipality == "Muhos", `:=` (longitude = 26.01915, latitude = 64.98877)]
rawdata[municipality == "Rovaniemi", `:=` (longitude = 25.92255, latitude = 66.49621)]

dtfi = copy(rawdata)

# Norwegian data -------------------------------------------------

all_files = list.files(paste0(datadir, "/clover-files-from-norway/"))

data_list = list()
coord_list = list()

# load and combine all data files
for (i in seq_along(all_files)){
  data = readxl::read_xlsx(paste0(datadir, "/clover-files-from-norway/",all_files[i]), sheet = 2) |> as.data.table()
  data[, trial := substr(all_files[i], 1, 5)]
  tmp1 = readxl::read_xlsx(paste0(datadir, "/clover-files-from-norway/",all_files[i]), sheet = 1, range = "C9:D25", col_names = c("cultivar", "sort"), col_types = c("numeric", "text")) |> as.data.table()
  tmp2 = readxl::read_xlsx(paste0(datadir, "/clover-files-from-norway/",all_files[i]), sheet = 1, range = "C2:E6", col_names = c("locationID", "location", "coordinates")) |> as.data.table()

  data[tmp1, sort := i.sort, on = .(cultivar)]  
  
  data_list[[i]] = data
  coord_list[[i]] = tmp2  
  
}

rawdata = data.table::rbindlist(data_list, fill = TRUE)
rawdata[, rep := 1:.N, by = .(trial, location, cultivar, har.year)]

# fix coordinates
coord = data.table::rbindlist(coord_list, fill = TRUE) |> unique()
coord[, coordinates := gsub(",", ".", coordinates)]
coord[, coordinates := gsub("N", "", coordinates)]
coord[, coordinates := gsub("E", "", coordinates)]
coord_split = strsplit(coord$coordinates, " ", fixed = TRUE)
coord[, c("latitude", "longitude") := .(as.numeric(sapply(coord_split, function(x) x[1])),
                                 as.numeric(sapply(coord_split, function(x) x[2])))]
coord[, location := tolower(location)]

coord_unique = coord[, .(location, latitude, longitude)] |> unique()

# fix weather variables
rawdata[, `:=` (`Temp GS` = mean(c(`Temp GS`, `Temp. GS`), na.rm = TRUE),
             `Temp year` = mean(c(`Temp year`, `Temp. year`), na.rm = TRUE),
             `Prec year` = mean(c(`Prec. Year`, `Prec.year`), na.rm = TRUE))]
rawdata[, lapply(.SD, function(x){sum(is.na(x))/.N}), .SDcols= c("cal.year", "Temp. year", "Prec.year", "Prec. Year", "Temp. GS", "...14", "T/ha...18", "T/ha...22")]
rawdata[, c("cal.year", "Temp. year", "Prec.year", "Prec. Year", "Temp. GS", "...14", "T/ha...18", "T/ha...22"):= NULL]

# add coordinates
rawdata[, location := tolower(location)]
rawdata[coord_unique, `:=` (latitude = i.latitude,
                         longitude = i.longitude), on = .(location)]

# make ploidy level into numeric
rawdata[ploidy == "diploid", ploidy := 2]
rawdata[ploidy == "tetraploid", ploidy := 4]

# add country
rawdata[, country := "Norway"]

setorder(rawdata, year, location)
# add trial variable 
rawdata[, trial := paste0("XCLNO", add_leading_zero(.GRP), "X"), by = .(trial, location)]

# calculate establishment year as recorded year - harvest year
rawdata[, establishment_year := reg.year-har.year]

# add remark for two missing observations
rawdata[is.na(sort), remark := "variety name missing"]

# convert yield from tonn/ha to hkg/ha
rawdata[, `:=` (dry_matter_yield_total = DMY * 10,  
             fresh_matter_yield_total = FMY * 10)]
tmp = copy(rawdata)
rawdata[, `:=` (dry_matter_yield_sl1 = Dsl1 * 10, 
             dry_matter_yield_sl2 = Dsl2 * 10, 
             dry_matter_yield_sl3 = Dsl3 * 10,  
             fresh_matter_yield_sl1 = Fsl1 * 10,  
             fresh_matter_yield_sl2 = Fsl2 * 10,  
             fresh_matter_yield_sl3 = Fsl3 * 10)]

# convert to long format
rawdata[, c("DMY", "FMY", "DMC", paste0("Dsl",1:3), paste0("Fsl",1:3), "dry_matter_yield_total", "fresh_matter_yield_total") := NULL]
data_final = melt(rawdata, measure.vars = patterns("dry_matter_yield_sl", "fresh_matter_yield_sl", "datosl"))

setnames(data_final, c("variable", paste0("value",1:3)), c("cut", "dry_matter_yield", "fresh_matter_yield", "date"))
data_final = rbind(data_final, tmp[,.SD, .SDcols = c('location', 'cultivar', 'ploidy', 'rep', 'har.year', 'reg.year', 'Temp year', 'Temp GS', 'Prec. GS', 'trial', 'sort', 'Prec year', 'latitude', 'longitude', 'country', 'establishment_year', 'remark', 'dry_matter_yield_total', 'fresh_matter_yield_total')], fill = TRUE)

setkey(data_final, reg.year, trial, location, cultivar, rep, cut)

data_final[, date := as.Date(date, format = "%Y-%m-%d")]

dtno = copy(data_final)

# NFTS data -------------------------------------------------
info = data.table::fread(paste0(datadir, "/nfts/final/red-clover_info.csv"))
info2 = data.table::fread(paste0(datadir, "/nfts-address-geocoded.csv"))
info3 = data.table::fread(paste0(datadir, "/nfts-address.csv")) 
info3[info2, `:=` (longitude = i.lon, latitude = i.lat), on = c("full_address" = "full_address_str")]
info[info3, `:=` (lon = i.longitude, lat = i.latitude), on = c("id")]
info[is.na(longitude), `:=` (longitude = lon, latitude = lat)]
rawdata = data.table::fread(paste0(datadir, "/nfts/final/red-clover_data.csv"))
rawdata2 = readxl::read_xlsx(paste0(datadir, "nfts-clover-additional-info.xlsx")) |> as.data.table() ## from Sybil

info[!grepl("-", trial_number), trial_number := NA]
info[, run_nr := lapply(strsplit(trial_number,"-"), tail, n = 1)]
info[, plan_nr := lapply(strsplit(trial_number,"-"), function(x){paste(unlist(head(x, n = (length(x)-1))), collapse = "-")})]
info[, run_nr := as.integer(run_nr)]

# manually fix wrong date info for certain trials
info[id == 59570, `:=` (year = 2013)]
info[id == 66581, `:=` (year = 2022)]
info[id == 58826, `:=` (year = 2012)]
rawdata[id == 59570, `:=` (date = as.Date(paste0("2013", substr(date,5,10))))]
rawdata[id == 66581, `:=` (date = as.Date(paste0("2022", substr(date,5,10))))]
rawdata[id == 58826, `:=` (date = as.Date(paste0("2012", substr(date,5,10))))]

# get harvest year from experiment title 
for(i in 1:nrow(info)){
    info[i, harvest_year := get_number(info[i]$experiment_title)]
}

# get harvest year from plan_nr
info[country == "Sweden", plan_years := substr(plan_nr, nchar(plan_nr)-8, nchar(plan_nr))]
info[country == "Sweden", first_year := substr(plan_years, 1, 4)]
info[country == "Sweden", last_year := substr(plan_years, 6, 9)]

info[country == "Denmark", plan_years := substr(plan_nr, nchar(plan_nr)-3, nchar(plan_nr))]
info[country == "Denmark", first_year := paste0(20, substr(plan_years, 1, 2))]
info[country == "Denmark", last_year := paste0(20, substr(plan_years, 3, 4))]

info[year != last_year]

# construct harvest year
info[is.na(harvest_year), harvest_year := year-as.integer(first_year)]

# identify trials across years
info[country == "Denmark", plan_id := substr(as.character(plan_nr), 1,7)]
info[country == "Sweden", plan_id := substr(plan_nr, 1,13)]
info[, trial_id := .GRP, by = .(plan_id, run_nr)]

# get harvest year from another data file
info[rawdata2[!is.na(production_years)], production_years := i.production_years, on = "id"]
info[is.na(harvest_year) & !is.na(production_years), harvest_year := production_years]

# add info from info to full data
rawdata[info, `:=` (year = i.year,
                country = i.country,
                lfe = i.lfe,
                longitude = i.longitude,
                latitude = i.latitude,
                harvest_year = i.harvest_year,
                sowing_date = i.sowing_date,
                trial_id = i.trial_id,
                experiment_title = i.experiment_title,
                experiment_design = i.experiment_design), on = .(id)]

rawdata[rawdata2[!(is.na(longitude)|is.na(latitude))], `:=` (longitude = i.longitude, latitude = i.latitude), on = c("id")]
rawdata[,remark := paste0("NFTS experiment title: ",experiment_title)]

rawdata[, establishment_year := year-harvest_year]
rawdata[, date := as.Date(date, format = "%Y-%m-%d")]

# remove legume trials
rawdata[, keep := ifelse(grepl("baljväxter", experiment_title), FALSE, TRUE)]
rawdata = rawdata[keep == TRUE]
rawdata[, keep := NULL]

# fix outlier: id = 57457
rawdata[id == 57457, `:=` (longitude = 20.23723)]

# remove error in variety names
rawdata[, variety := gsub(" mk)", "", variety)]

# construct new trial variable that is unique
rawdata[country == "Sweden", trial := paste0("XCLSW", add_leading_zero(.GRP), "X"), by = .(trial_id)]
rawdata[country == "Denmark", trial := paste0("XCLDK", add_leading_zero(.GRP), "X"), by = .(trial_id)]

# Check for trials with more than one year between
# Step 1: Sort the data
setorder(rawdata, trial, lfe, year)
tmp = rawdata[,.(trial, lfe, year)] |> unique()

# Step 2: Identify breaks in consecutive years
tmp[, year_diff := c(NA, diff(year)), by = .(trial, lfe)]
tmp[year_diff > 1] # only one case, which looks reasonable so we don't change this

# get cut number
rawdata[, cut := get_cut_number(info)]

# make extra info column (free text)
rawdata[id %in% c(53671, 54464, 58851), remark := paste0("persistency trial; ", remark)]

# remove wrong trial
rawdata = rawdata[id != 55718]

# copmute total in the cases where this was missing
tmp = rawdata[,
    .(dmy_total = sum(`Yield (hkg drymatter (dm))`, na.rm = TRUE),
    fmy_total = sum(`Yield (hkg freshmatter (fm))`, na.rm = TRUE)),
    by = .(id, variety)]

tmp[,info := "T01: Sum af slæt"]
rawdata[tmp, `:=` (`Total yield (hkg freshmatter (fm)) computed` = i.fmy_total,
                `Total yield (hkg drymatter (dm)) computed` = i.dmy_total), on = c("id", "variety", "info")]

rawdata[is.na(`Total yield (hkg freshmatter (fm))`) & info == "T01: Sum af slæt", `Total yield (hkg freshmatter (fm))` := `Total yield (hkg freshmatter (fm)) computed`]
rawdata[is.na(`Total yield (hkg drymatter (dm))`) & info == "T01: Sum af slæt", `Total yield (hkg drymatter (dm))` := `Total yield (hkg drymatter (dm)) computed`]

dtnfts = copy(rawdata)

# Combine all data --------------------------------------------------------

setnames(dtnfts, 
    old = c("date","lfe", "ploidy_level", "Total yield (hkg drymatter (dm))", "Total yield (hkg freshmatter (fm))", "id","Yield (hkg freshmatter (fm))", "Yield (hkg drymatter (dm))"), 
    new = c("measure_date","location_info", "ploidy_level", "dry_matter_yield_total", "fresh_matter_yield_total", "nfts_id", "fresh_matter_yield", "dry_matter_yield"))
setnames(dtno, 
    old = c("sort","reg.year", "location", "har.year", "date"), 
    new = c("variety","year", "location_info", "harvest_year", "measure_date"))
setnames(dtfi, 
    old = c("municipality", "longitude", "latitude", "cultivar_name"), 
    new = c("location_info", "longitude", "latitude", "variety"))

col_keep = c("trial","nfts_id", "year", "country", "location_info",   
            "longitude", "latitude",
            "variety", "ploidy_level", "rep", "establishment_year","harvest_year", "measure_date", "cut",
            "dry_matter_yield", "fresh_matter_yield",
            "dry_matter_yield_total", "fresh_matter_yield_total", "experiment_design", "remark")


data_all = rbindlist(list(dtno, dtfi, dtnfts), fill = TRUE, ignore.attr = TRUE)
data_all = data_all[, ..col_keep]

# fix variety names
data_all[, variety := tolower(variety)]
data_all[, variety := gsub(",\\dx", "", variety)]
data_all[, variety := gsub(" \\dx", "", variety)]
data_all[, variety := gsub(" \\dn", "", variety)]
data_all[, variety := gsub("\\dx", "", variety)]
data_all[, variety := gsub("\\dn", "", variety)]
data_all[, variety := gsub(" ,", "", variety)]
data_all[, variety := gsub(",", "", variety)]
data_all[, variety := gsub(" ", "", variety)]
data_all[, variety := gsub("-", "", variety)]
data_all[variety == "", variety := NA]
data_all = data_all[!(is.na(variety)|variety=="utgått"|variety=="utgår")]
data_all[grep("nexus", variety), `:=` (remark = paste0(remark, "not red clover; "))]
data_all[grep("vitklöver hebe mätare sor", variety), `:=` (remark = paste0(remark, "not red clover; "))]
data_all[variety == "callistos", variety := "callisto"]
data_all[variety == "lars(gn/", variety := "lars"]
data_all[variety == "allyswrk1053", `:=` (variety = "ally", old_variety_name = "swrk1053")]
data_all[variety == "reipo(lgrk8802)", `:=` (variety = "reipo", old_variety_name = "lgrk8802")]
data_all[variety == "lgrk8802", `:=` (variety = "reipo", old_variety_name = "lgrk8802")]
data_all[variety == "betty", variety := "swbetty"]
data_all[variety == "swrk1053", `:=` (variety = "ally", old_variety_name = "swrk1053")]

# add ploidy level
rawdata3 = readxl::read_xlsx(paste0(datadir, "clover-ploidy-info.xlsx"), sheet = 1) |> as.data.table() ## ploidy_info from Sybil
rawdata3[, variety := tolower(variety)]
rawdata3[, variety := gsub(",\\dx", "", variety)]
rawdata3[, variety := gsub(" \\dx", "", variety)]
rawdata3[, variety := gsub(" \\dn", "", variety)]
rawdata3[, variety := gsub("\\dx", "", variety)]
rawdata3[, variety := gsub("\\dn", "", variety)]
rawdata3[, variety := gsub(" ,", "", variety)]
rawdata3[, variety := gsub(",", "", variety)]
rawdata3[, variety := gsub(" ", "", variety)]
rawdata3[, variety := gsub("-", "", variety)]
data_all[rawdata3[!is.na(ploidy)], ploidy := i.ploidy, on = c("variety")]
data_all[is.na(ploidy_level), ploidy_level := as.integer(ploidy)]
data_all[, ploidy := NULL]


data_all[, variety := add_spacing(variety, "swå")]
data_all[, variety := add_spacing(variety, "gn")]
data_all[, variety := add_spacing(variety, "lm")]
data_all[, variety := add_spacing(variety, "lø")]
data_all[, variety := add_spacing(variety, "vå")]
data_all[, variety := add_spacing(variety, "lg")]
data_all[substr(variety,1,3)!="swå", variety := add_spacing(variety, "sw")]

data_all[, old_variety_name := add_spacing(old_variety_name, "swå")]
data_all[, old_variety_name := add_spacing(old_variety_name, "gn")]
data_all[, old_variety_name := add_spacing(old_variety_name, "lm")]
data_all[, old_variety_name := add_spacing(old_variety_name, "lø")]
data_all[, old_variety_name := add_spacing(old_variety_name, "vå")]
data_all[, old_variety_name := add_spacing(old_variety_name, "lg")]
data_all[substr(old_variety_name,1,3)!="swå", old_variety_name := add_spacing(old_variety_name, "sw")]


data_all[old_variety_name == "", old_variety_name := NA]

data_all[variety == "selm a", variety := "selma"]
data_all[variety == "vilm a", variety := "vilma"]

# compute dry_matter_coefficient
data_all[, dry_matter_coefficient := dry_matter_yield/fresh_matter_yield*100]
data_all[, dry_matter_coefficient_total := dry_matter_yield_total/fresh_matter_yield_total*100]

# remove observations/trials that contain extreme values
data_all = data_all[!(trial == "XCLSW0052X" & year == 2020)]
data_all = data_all[!((trial == "XCLNO0010X" & year == 2013 & variety == "bjursele" & rep == 2)|(trial == "XCLNO0010X" & year == 2013 & variety == "katrin" & rep == 1))]

# format measure date
data_all[, `:=` (measure_date = as.Date(measure_date, format = "%Y-%m-%d"))]

# manually fix date typos for some trials 
data_all[trial == "XCLNO0015X" & year == 2015 & harvest_year == 2 & cut == 1, `:=` (
  measure_date = as.Date(paste0(2015, substr(as.character(measure_date),5,10)), format = "%Y-%m-%d"))]

# add Julian date
data_all[, measure_date_J := lubridate::yday(measure_date)]

data_all = data_all[!(is.na(dry_matter_yield) & is.na(fresh_matter_yield) & is.na(dry_matter_yield_total) & is.na(fresh_matter_yield_total))]

data_all[, trial_year := paste0(trial, "-", year)]

# set column order
setcolorder(data_all,
c("trial",  "trial_year", "nfts_id", "year",
"country", "longitude", "latitude", "location_info", "establishment_year","harvest_year", 
"variety", "old_variety_name", "ploidy_level", "rep", "cut", "measure_date", "measure_date_J",
"dry_matter_yield", "fresh_matter_yield", "dry_matter_coefficient",
"dry_matter_yield_total", "fresh_matter_yield_total","dry_matter_coefficient_total",
"experiment_design", "remark")
)

# set key
setkey(data_all, country, year, location_info, trial, variety, rep, cut)

setnames(data_all, "location_info", "region")

data_all[region == "12SLU Lövsta Fältforskningsstation" & is.na(longitude), `:=` (longitude=17.79025, latitude=59.84042)]
data_all[region == "12HS Sjuhärad" & is.na(longitude), `:=` (longitude=13.25571, latitude=57.60649)]
data_all[region == "4HS Halland L:a Böslid" & is.na(longitude), `:=` (longitude=12.95155, latitude=56.59678)]

# save data as csv
data.table::fwrite(data_all, paste0(datadir, "02_RedCloverData.csv"), row.names = FALSE)
