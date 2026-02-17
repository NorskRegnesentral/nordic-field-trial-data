### NB! This script use data files that are not publicly available ###
### To accsess the additional data files, please contact smvandeskog@nr.no ###

### Barley data processing script ###
# script for reading in all barley field trial data provided from
# Norway, Finland, Iceland and NFTS (Sweden + Denmark), 
# processing and combining into one dataset

library(data.table)
library(lubridate)
library(readxl)
library(sf)
rm(list = ls())
datadir = "./"

# functions ----------------------------------------------------
# change date from (d)dmmyyyy to yyyy-mm-dd
digit_to_date <- function(x) {
  x_str <- as.character(x)
  if (nchar(x_str) == 7) {
    x_str <- paste0("0", x_str)
  }
  day <- substr(x_str, 1, 2)
  month <- substr(x_str, 3, 4)
  year <- substr(x_str, 5, 8)

  date = paste0(year, "-", month, "-", day)

  return(date)

}


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

# Icelandic data -----------------------------------------------

# load rawdata
rawdata = read.delim(paste0(datadir, "barley_iceland.txt"), sep = " ") |> as.data.table()

# convert tons/ha to hkg/ha
rawdata[, dry_matter_yield := DM_yield_adj*10] 

# fix dates for sowing and harvest
rawdata[, sowing_date := as.Date(sapply(sowdata, digit_to_date), format = "%Y-%m-%d")]
rawdata[!is.na(harvdate), harvest_date := as.Date(sapply(harvdate, digit_to_date), format = "%Y-%m-%d")]

# construct trial variable (naming convention: XBA(rley)IC(eland)(four-digit number)X)
rawdata[, old_trial_name := trial]
rawdata[, trial := NULL]
rawdata[, trial := paste0("XBAIC", add_leading_zero(.GRP), "X"), by = .(old_trial_name, farm, year)]
rawdata[, country := "Iceland"]
setorder(rawdata, year, trial, blokk)

# Assign unique numbers per subgroup within each group
rawdata[, rep := as.factor(match(blokk, unique(blokk))), by = .(trial)]

dtic = copy(rawdata)

# Finnish data -------------------------------------------------

# load rawdata
rawdata = readxl::read_xlsx(path = paste0(datadir, "barley_finland.xlsx"), sheet =2) |> as.data.table()

# load municipality and coordinates
tmp = readxl::read_xlsx(paste0(datadir, "barley_finland.xlsx"), sheet = 1, range = "B11:K41") |> as.data.table()
setnames(tmp, c("Municipality", "Municipality (2025)"), c("municipality", "municipality_2025"))
coords = strsplit(tmp$`Coordinates`, " ")
tmp[, `:=` (longitude = sapply(coords, function(x) get_numbers(x[2])),
            latitude = sapply(coords, function(x) get_numbers(x[4])),
             koep = sapply(coords, function(x) get_numbers(x[6])))]

# add municipality and coordinates to rawdata
rawdata[tmp, `:=` (
    municipality = i.municipality,
    municipality_2025 = i.municipality_2025,
    longitude_kkj = as.numeric(i.longitude),
    latitude_kkj = as.numeric(i.latitude),
    koep = as.numeric(i.koep)
), on = c("site" = "Site")]


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

# fix swapped coordinates
rawdata[longitude > 50, `:=` (longitude = latitude, latitude = longitude)]

# add country variable
rawdata[, country := "Finland"]

# adjust yield from kg/ha to hkg/ha
rawdata[, dry_matter_yield := yield/100]

# construct trial variable 
rawdata[, old_trial_name := trial]
rawdata[, trial := as.character(trial)]
rawdata[, trial := paste0("XBAFI", add_leading_zero(.GRP), "X"), by = .(old_trial_name, site, year)]

# fix municipality names
rawdata[is.na(municipality), `:=` (municipality = municipality_2025, municipality_2025 = NA)]
rawdata[!is.na(municipality_2025), `:=` (municipality = paste0(municipality, " (", municipality_2025, ")"))]

# Manually add missing coordinates 
rawdata[municipality == "Nikkilä", `:=` (longitude = 25.26899, latitude = 60.37711)]
rawdata[municipality == "Mildola (Tuusula)", `:=` (longitude = 24.40841, latitude = 60.17627)]
rawdata[municipality == "Muhos", `:=` (longitude = 26.01915, latitude = 64.98877)]
rawdata[municipality == "Keski-pohjanmaa (Toholampi)", `:=` (longitude = 24.248753, latitude = 63.772451)]
rawdata[municipality == "Vimpeli", `:=` (longitude = 23.828837, latitude = 63.163935)]
rawdata[municipality == "Kotkaniemi", `:=` (longitude = 25.794996, latitude = 61.228601)]
rawdata[municipality == "Sokerijuurikas (Paimio)", `:=` (longitude = 22.688033, latitude = 60.457206)]

# remove 16 observations with missing location info
rawdata = rawdata[!is.na(longitude) & !is.na(latitude)]

dtfi = copy(rawdata)

# Norwegian data -----------------------------------------------

# combine all excel files
all_files = list.files(paste0(datadir, "/barley-files-from-norway/"))
data_list = list()

for (i in seq_along(all_files)){
  
  yr = paste0("20", substr(all_files[i],8,9))
  data = readxl::read_xlsx(paste0(datadir, "/barley-files-from-norway/", all_files[i]),.name_repair = "minimal", col_names = FALSE)
  
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
  
  common_cols = c(colnames(tmp), "st", "P0")

  for(j in seq_along(data_list2)){
    if(j==1){data = data_list2[[j]]}
    else{
      data = merge(data, data_list2[[j]], all = TRUE)
    }
  }
  data$year = as.integer(yr)
  
  data_list[[i]] = data
  
}

rawdata = data.table::rbindlist(data_list, fill = TRUE)

# set correct column type
changeCols <- colnames(rawdata)[which(as.vector(rawdata[,lapply(.SD, class)]) == "character")]
changeCols = changeCols[!(changeCols %in% c("Forsøk", "Adresse", "Sort", "st", "P0"))]
rawdata[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]

# join information on old variety name, early and row from another excel file
dtno2 = readxl::read_xlsx(path = paste0(datadir, "norway-barley-lines-additional-info.xlsx"), sheet =1, skip = 2) |> as.data.table()
setnames(dtno2, c("Sort/gamelt navn", "tidlig/sein", "6/2 radsbygg"), c("x","y","z"))
rawdata[dtno2, `:=` (sort_old_name = i.x,
                  early = i.y,
                  row = i.z), on = c("Forsøk", "Felt", "Sort")]


# join information about coordinates from another csv file
coord = data.table::fread(paste0(datadir, "norway-barley-coords-trials.csv"))
setnames(coord, "UTM-sone", "UTM")
rawdata[coord, `:=` (
  fylke = i.fylke,
  kommune = i.kommune,
  postnr = i.postnr,
  `UTM-sone` = i.UTM,
  Øst = i.Øst,
  Nord = i.Nord,
  longitude = i.long,
  latitude = i.lat
), on = c("Forsøk", "year", "Felt")]

# add country information
rawdata[, country := "Norway"]

# add trial variable
rawdata[, trial := paste0("XBANO", add_leading_zero(.GRP), "X"), by = .(Forsøk, Felt)]

# adjust yield from hkg/da to hkg/ha
rawdata[, `:=` (yield_in_seed = `Avling, kg korn(frø)/daa`/10, dry_matter_yield = `Avling, kg tørrstoff/daa`/10)]

# make early variable binary 0/1
rawdata[early == "", early := NA]
rawdata[early == "sein", early := 0]
rawdata[early %in% c("tidlig", "tildlig"), early := 1]

dtno = copy(rawdata)

# nfts data ----------------------------------------------------

# load data and metadata
rawdata = data.table::fread(paste0(datadir, "/nfts/final/spring-barley_data.csv"))
infonfts = data.table::fread(paste0(datadir, "/nfts/final/spring-barley_info.csv"))
info2 = data.table::fread(paste0(datadir, "/nfts-address-geocoded.csv"))
info3 = data.table::fread(paste0(datadir, "/nfts-address.csv")) 
info3[info2, `:=` (longitude = i.lon, latitude = i.lat), on = c("full_address" = "full_address_str")]
infonfts[info3, `:=` (longitude = i.longitude, latitude = i.latitude), on = c("id")]


interesting_danish_trials = c("Gamle vårbygsorter med normal + kraftig og uden svampebekæmpelse",
                              "Gamle vårbygsorter med normal, kraftig eller ingen svampebekæmpelse",
                              "Vårbygsorter",
                              "Vårbygsorter (A-plus sorter og B-sorter)",
                              "Vårbygsorter (A-plus)",
                              "Vårbygsorter (B sorter)",
                              "Vårbygsorter med og uden svampebekæmpelse",
                              "Vårbygsorter, A og B sorter",                                                     
                              "Vårbygsorter, A og B sorter (106 sorter)",                                        
                              "Vårbygsorter, A og B sorter (110 sorter)",                                        
                              "Vårbygsorter, A og B sorter (118 sorter)",                                        
                              "Vårbygsorter, A-plus og B-sorter, med og uden svampebekæmpelse",                  
                              "Vårbygsorter, A-plus sorter (2-fak)",                                             
                              "Vårbygsorter, A-plus, med og uden svampebekæmpelse",                              
                              "Vårbygsorter, A+ og B sorter, med og uden svampebek.",                            
                              "Vårbygsorter, A+ og B sorter, med og uden svampebek. (69 sorter)",                
                              "Vårbygsorter, A+ og B sorter, med og uden svampebekæmpelse",                      
                              "Vårbygsorter, A+ og B sorter, med og uden svampebekæmpelse (66 sorter)",    
                              "Vårbygsorter, A+ og B sorter, med og uden svampebekæmpelse (71 sorter)",    
                              "Vårbygsorter, alle.",                                                             
                              "Vårbygsorter, alle. Med og uden svampebekæmpelse",                                
                              "Vårbygsorter, B-sorter (2-fak)",                                                  
                              "Vårbygsorter, B-sorter, med og uden svampebekæmpelse",
                              "Vårbygsorter, supplerende sortsforsøg",
                              "Vårbygsorters reaktion på svampebekæmpelse",
                              "Demonstration af vårbygsorter")

# trials that were removed:
# "(Egen plan) 5. gentagelse ubehandlet 010052121 Vårbygsorter",
# "(Egen plan) 5. Gentagelse ubehandlet 010052222 vårbygsorter",
# "2 vårbygsorter + 3 fraktioner",
# "Ærtesorter, økologisk",
# "BEREGNINGSTESTFORSØG kopi 011051919 Vårbygsorter A og B sorter",
# "BEREGNINGSTESTFORSØG kopi 011551919 Vårbygsorter A og B med og uden svampebekæmpelse",
# "Bestemmelse af kvælstofbehov i 3 vårbygsorter ud fra dronemålinger",
# "Blandingsafgrøder af vårsæd",
# "Demonstration - Vintersæd udlagt i vårsæd",
# "Efterårssåning af vårbyg",
# "EGEN PLAN 2: Vårbygsorter, alle. Med og uden svampebekæmpelse",
# "EGEN PLAN: 2004.140 Vårbyg sorter",
# "EGEN PLAN: Maltbygsorter",
# "EGEN PLAN: Vårbyg sorter",
# "EGEN PLAN: Vårbygsorter",
# "EGEN PLAN: Vårbygsorter +/- svampebekæmpelse",
# "EGEN PLAN: Vårbygsorter med og uden svampebekæmpelse",
# "EGEN PLAN: Vårbygsorter, alle. Med og uden svampebekæmpelse",
# "EGEN PLAN: Vårbygsorter, økologisk (6 sorter)",
# "EGENPLAN Ytteborg Cup 2018, Maltbyg",
# "Foderbyg Cup Ytteborg 2014",
# "Foderbygsorters reaktion på svampebekæmpelse",
# "Forsøgsseminar 2008 SmartTrial",
# "Fungicidstrategier i foderbygsorter af vårbyg",
# "Fungicidstrategier i maltbygsorter af vårbyg",
# "Fungicidstrategier i typesorter af vårbyg",
# "Havresorter",
# "Havresorter og vårsædsorter ved økologisk/biodynamisk dyrkning",
# "Havresorter, økologisk",
# "Kontrolforsøg. Korn. FAK2. A-B-C-D.",
# "KURSUS Indberetning af forsøgsdata 2021",
# "Kvælstofmængder til typer af vårbygsorter",
# "Kvælstofoptagelse i sorter af vårbyg",
# "Kvælstofoptagelse i sorter af vårbyg (2 kvælstofniveauer)",
# "Maltbygsorter i to dyrkningssystemer",
# "Maltbygsorter ved regenerativ dyrkning",
# "Maltbygsorters reaktion på svampebekæmpelse",
# "OBS parceller vårbygsorter",
# "Øget udbytte i vårhvede og vårbyg",
# "Øget udbytte i vårhvede og vårbyg inklusive radrensning",
# "Optimering af produktion af foderbyg til svin",
# "Pløjning og såning i een arbejdsgang",
# "Såteknik ved etablering af vårbyg",
# "Screening af vårbygsorter",
# "Sorter af nøgenbyg",
# "Sorter af vårbyg",
# "Stresstest forsøg til kornlab",
# "Test af håndterminal SmartTrial 2008",
# "TEST KWS Vårbyg",
# "TEST Plantekongres 2009",
# "TESTPLAN Forsøgsseminar 2009 med og uden svampebekæmpelse",
# "Tidlige sorter af vårbyg, økologisk",
# "Triticalesorter, B-sorter, med og uden svampebekæmpelse",
# "Vårbygsorter med/uden svampebekæmpelse - småparcelteknik",
# "Vårbygsorter til malt, phenotyping projektet",
# "Vårbygsorter ved økologisk/biodynamisk dyrkning",
# "Vårbygsorter ved to N-mængder",
# "Vårbygsorter, A og B sorter (118 sorter) - KOPI TIL TEST",
# "Vårbygsorter, Limagrain",
# "Vårbygsorter, økologisk",
# "Vårbygsorter, økologisk (10 sorter)",
# "Vårbygsorter, økologisk (8 sorter)",
# "Vårbygsorter, økologisk (9 sorter)",
# "Vårbygsorter, økologisk. med og uden gylle",
# "Vårbygsorter, økologisk. Plan 1, med og uden gylle",
# "Vårbygsorter, økologisk. Plan 2, med og uden gylle",
# "Vårbygsorters følsomhed overfor manganmangel",
# "Vårbygsorters tolerance overfor manganmangel",
# "Vinterhvedesorter, økologisk",
# "Vinterrugsorter, B-sorter, med og uden vækstregulering"

interesting_danish_lfe = c("Ytteborg",
                           "LandboNord",
                           "Djursland Landboforening",
                           "Landsforsøg Østjylland",
                           "1Planteforsøg Sydjylland I/S",
                           "Ålborg",
                           "1VKST, Ringsted",
                           "1VKST, Holeby",
                           "1Teknologisk Institut",
                           "3Nordic Seed",
                           "3Sejet Planteforædling",
                           "4Afd. for Sortsafprøvning",
                           "1Bornholm", 
                           "1FORSØG FYN")
# lfe that were removed:
# "1Bornholm",
# "1FORSØG FYN",
# "2Hanne Justesen Bach",
# "2Kontrolforsøg",
# "2Software Test",
# "2Svensk test",
# "2Test Planlægningsteamet",
# "2Thomas Tester",
# "4KU - LIFE"

interesting_swedish_trials = c("Att undersöka olika fungiciders aktuella effekt i ett samspel mellan sorter, bekämpning, på olika platser.",   
                               #"Fyra kornsorter och två skördetidpunkter",                                                                     
                               "Korn Sorter för norra sverige",                                                                                 
                               "Korn Sorter för norra Sverige",                                                                                 
                               "Korn sortförsök",                                                                                               
                               "Tidig korn. Sort * behandling.",                                                                               
                               "Tidig vårkorn. Sortförsök avkastning",                                                                         
                               "Vårkorn",                                                                                                       
                               "Vårkorn. Sort * behandling.",                                                                                         
                               "Vårkorn. Sorter (2-radskorn) för norra Sverige",                                                               
                               "Vårkorn. Sorter (6-radskorn) för norra Sverige",                                                               
                               "Vårkorn. Sorter för Norra Sverige",                                                                             
                               "Vårkorn. Sortförsök avkastning",                                                                                 
                               "Vårkorn. Sortförsök gradering",
                               "Tidig vårkorn. Sortförsök gradering")                               
# trials that were removed:
# "Artförsök, Vårkorn, Vårhavre, Vårvete och Höstvete.",
# "Artjämförelse vårsäd",
# "Ekologisk spannmål",
# "Fyra kornsorter och två skördetidpunkter",
# "Kvävebehov hos olika maltkornsorter",
# "Sortförsök gradering vårvete (301G), vårkorn/tidigt (401G/402G), havre (501G)",
# "Tidig vårkorn. Sortförsök gradering",
# "Vårkorn i torra odlingslägen",
# "Vårkorn. Ekologisk odling"

interesting_swedish_lfe = c("10HS Konsult Brunnby",
                            "11HS Kalmar",
                            "11SLU Norrländsk Jordbruksvetenskap",
                            "12HS Gotland",
                            "13HS Östergötland Vreta Kloster",
                            "13HS-Jönköping Riddersberg",
                            "4HS Halland L:a Böslid",
                            "5HS Skåne",
                            "5HS Skaraborg Logården")
# lfe that were removed:
# "10SLU Växtproduktionsekologi"

# keep only interesting trials and lfe
infonfts = infonfts[experiment_title %in% c(interesting_danish_trials, interesting_swedish_trials) & lfe %in% c(interesting_danish_lfe, interesting_swedish_lfe)] # 1537 forsøk

# clean info on experiment design
infonfts$experiment_design2 = sapply(strsplit(infonfts$experiment_design,"[.]"),function(x){x[[1]]})

# get year information from sowing date if missing
infonfts[is.na(year), year := as.integer(substr(as.character(sowing_date),1,4))]

# add metadata to rawdata
rawdata[infonfts, `:=` (year = i.year,
                country = i.country,
                lfe = i.lfe,
                longitude = i.longitude,
                latitude = i.latitude,
                sowing_date = i.sowing_date,
                harvest_date = i.harvest_date,
                experiment_title = i.experiment_title,
                experiment_design = i.experiment_design), on = .(id)]

# fix coordinates that are clearly wrong
rawdata[id == 2375, `:=` (longitude = 9.030194, latitude = 55.041722)]

# Remove a bad trial
rawdata = rawdata[id != 3999]
infonfts = infonfts[id != 3999]

# categorize and remove trials based on registered treatments
rawdata = rawdata[id %in% infonfts$id]
rawdata[, obsID := 1:.N]
rawdata[, keep := TRUE]

# keep all 1 factor trials
obs_1_faktor = c(rawdata[id %in% infonfts[grep("1 faktor", experiment_design2)]$id]$obsID)
rawdata[obsID %in% obs_1_faktor, treatment_group := "1 factor"]

# find untreated observations from 2 factor trials
rawdata[grep("(ingen)|(ubehandlet)|(obehandlat)", tolower(treatment)), ubh := TRUE]
obs_2_faktor_ubh = c(rawdata[id %in% infonfts[grep("2 faktor", experiment_design2)]$id & ubh == TRUE]$obsID)
rawdata[obsID %in% obs_2_faktor_ubh, treatment_group := "2 factor (without fungicide treatment)"]
rawdata[is.na(treatment_group) & treatment == "", treatment_group := "2 factor (without fungicide treatment)"]

# find treated observations from 2 factor trials
rawdata[grep("(intensiv)", tolower(treatment)), bh := TRUE]
obs_2_faktor_bh = c(rawdata[id %in% infonfts[grep("2 faktor", experiment_design2)]$id & bh == TRUE]$obsID)
rawdata[obsID %in% obs_2_faktor_bh, treatment_group := "2 factor (with fungicide treatment)"]
rawdata[is.na(treatment_group), treatment_group := "2 factor (with fungicide treatment)"]

# remove trials that are unbalanced (not 50/50 treated/untreated). These trials are typically one of two things: 
# 1) They are treated with multiple different levels/types of fungicide (i.e. untreated, low dose, medium dose, high dose)
# 2) They are tested for something else than than fungicide (i.e. sowing date, row spacing, etc.)
tmp = table(rawdata[treatment_group != "1 factor"]$id, rawdata[treatment_group != "1 factor"]$treatment_group)
remove_ids = tmp[apply(tmp,1,function(x){any(x[1]!=x[2])}),] |> rownames() # 70 trials
rawdata[id %in% remove_ids, keep := FALSE]

# if all yield observations are NA for a full trial we remove them
rawdata[, all_na_yield := all(is.na(`Yield (hkg drymatter (dm))`)), by = .(id)]

# remove based on above criteria
data_final = rawdata[keep == TRUE & all_na_yield == FALSE] # 286593 -> 268216
infonfts_final = infonfts[id %in% data_final$id] # 1425 forsøk
data_final[, `:=` (obsID = NULL, keep = NULL)]

# make trial variable
data_final[country == "Sweden", trial := paste0("XBASW", add_leading_zero(.GRP), "X"), by = .(id)]
data_final[country == "Denmark", trial := paste0("XBADK", add_leading_zero(.GRP), "X"), by = .(id)]

# manual fix after looking up trial at NFTS web page !OBS - replace this with nfts_ids instead of year < 2010
data_final[id == 63554, `:=` (year = 2020, date = as.Date(paste0(2020, "-", month(date), "-", mday(date))))]

# fix date for trials with missing/wrong date (i.e. before 1980)
data_final[, tmp_date := max(date, na.rm = TRUE), by = .(id)]
data_final[date < as.Date("1980-01-01"), date := tmp_date]
data_final[is.na(year), year := as.integer(substr(as.character(date),1,4))]

dtnfts = copy(data_final)

# combine all data ------------------------------------------

# standardize variable names
setnames(dtnfts, 
    old = c("lfe", "id", "date", "Yield (hkg drymatter (dm))", "n_row", "experiment_title"), 
    new = c("location_info", "nfts_id", "measure_date", "dry_matter_yield", "row", "remark"))

dtnfts[, remark := paste0("NFTS experiment title: ", remark)]

setnames(dtno, 
    old = c("sort_old_name","Sort", "kommune", "Felt"), 
    new = c("old_variety_name","variety", "location_info", "site"))

setnames(dtfi, 
    old = c("municipality", "cultivar_name"), 
    new = c("location_info", "variety"))

setnames(dtic, 
    old = c("farm", "cultivar", "rowtype", "harvest_date", "long", "lat"), 
    new = c("location_info", "variety", "row", "measure_date", "longitude", "latitude"))

col_keep = c("trial", "year", "country", "location_info",
            "longitude", "latitude", "nfts_id","sowing_date", "measure_date", "treatment_group","treatment",
            "variety", "old_variety_name","row", "early", "dry_matter_yield", "experiment_design", "remark")

# combine data into one long dataset
data_all = rbindlist(list(dtno, dtfi, dtnfts, dtic), fill = TRUE)
data_all = data_all[, ..col_keep]


data_all[, variety := update_name(variety)]
data_all[, old_variety_name := update_name(old_variety_name)]
old_names = data_all[, .(variety, old_variety_name)] |> unique()

# update variety names
for(i in 1:nrow(old_names)){
  data_all[variety == old_names[i]$old_variety_name, variety := old_names[i]$variety]
}

# fix typos in variety names
data_all[variety == "", variety := NA]
data_all = data_all[!(is.na(variety)|variety=="svensk"|variety=="utgått")]
data_all[variety == "bentedubbl", variety := "bente"]
data_all[variety == "bor10661(kaarle)", `:=` (old_variety_name = "bor10661", variety = "kaarle")]
data_all[variety == "cb13-3047-b", variety := "cb13-3047"]
data_all[variety == "cb20-0954b", variety := "cb20-0954"]
data_all[variety == "canut", variety := "cbcanut"]
data_all[variety == "halvor(gn18559)", `:=` (old_variety_name = "gn18559", variety = "halvor")]
data_all[variety == "kwschrissie(kws15/2412", `:=` (old_variety_name = "kws15/2412", variety = "kwschrissie")]
data_all[variety == "caruso", variety := "lgcaruso"]
data_all[variety == "nabuco", variety := "lgnabuco"]
data_all[variety == "luhkasdubblett", variety := "luhkas"]
data_all[variety == "maire(bor15203", `:=` (old_variety_name = "bor15203", variety = "maire")]
data_all[variety == "myway(nos10006-52)", `:=` (old_variety_name = "nos10006-52", variety = "myway")]
data_all[variety == "nosupstairs(nos113.259", `:=` (old_variety_name = "nos113.259", variety = "nosupstairs")]
data_all[variety == "nosvalda(nos115.918-10", `:=` (old_variety_name = "nos115.918-10", variety = "nosvalda")]
data_all[variety == "rödhette", variety := "rødhette"]
data_all[variety == "soulmate(nos16111-55", `:=` (old_variety_name = "nos16111-55", variety = "soulmate")]
data_all[variety == "swcatrionadubblett", variety := "swcatriona"]
data_all[variety == "swå11019-ny", variety := "swå11019"]

# add information on old variety names, earliness and row
tmp = data_all[, .(variety, old_variety_name, row, early)] |> unique()
data_all[tmp, `:=` (old_variety_name = i.old_variety_name, row = i.row, early = i.early), on = "variety"]

# add spacing in variety names where needed
data_all[, variety := add_spacing(variety, "kws")]
data_all[, variety := add_spacing(variety, "nos")]
data_all[, variety := add_spacing(variety, "pf")]
data_all[, variety := add_spacing(variety, "sy")]
data_all[, variety := add_spacing(variety, "swc")]
data_all[, variety := add_spacing(variety, "swå")]
data_all[, variety := add_spacing(variety, "nord")]
data_all[, variety := add_spacing(variety, "cs")]
data_all[, variety := add_spacing(variety, "cb")]
data_all[, variety := add_spacing(variety, "sj")]
data_all[, variety := add_spacing(variety, "lg")]
data_all[, variety := add_spacing(variety, "nfc")]
data_all[, variety := add_spacing(variety, "nsl")]
data_all[, variety := add_spacing(variety, "bor")]
data_all[, variety := add_spacing(variety, "gn")]
data_all[, variety := add_spacing(variety, "rgt")]
data_all[, variety := add_spacing(variety, "cdc")]


data_all[, old_variety_name := add_spacing(old_variety_name, "kws")]
data_all[, old_variety_name := add_spacing(old_variety_name, "nos")]
data_all[, old_variety_name := add_spacing(old_variety_name, "pf")]
data_all[, old_variety_name := add_spacing(old_variety_name, "sy")]
data_all[, old_variety_name := add_spacing(old_variety_name, "swc")]
data_all[, old_variety_name := add_spacing(old_variety_name, "swå")]
data_all[, old_variety_name := add_spacing(old_variety_name, "nord")]
data_all[, old_variety_name := add_spacing(old_variety_name, "cs")]
data_all[, old_variety_name := add_spacing(old_variety_name, "cb")]
data_all[, old_variety_name := add_spacing(old_variety_name, "sj")]
data_all[, old_variety_name := add_spacing(old_variety_name, "lg")]
data_all[, old_variety_name := add_spacing(old_variety_name, "nfc")]
data_all[, old_variety_name := add_spacing(old_variety_name, "nsl")]
data_all[, old_variety_name := add_spacing(old_variety_name, "bor")]
data_all[, old_variety_name := add_spacing(old_variety_name, "gn")]
data_all[, old_variety_name := add_spacing(old_variety_name, "rgt")]
data_all[, old_variety_name := add_spacing(old_variety_name, "cdc")]

data_all[grep("chronos", variety), variety := "chronos"]
data_all[grep("kws jessie", variety), variety := "kws jessie"]
data_all[grep("cogn ac", variety), variety := "cognac"] 
data_all[grep("magn itude", variety), variety := "magnitude"] 
data_all[grep("nord 22/2522gs", variety), variety := "nord 22/2522"]
data_all[grep("sign ora", variety), variety := "signora"] 
data_all[grep("sy countour", variety), variety := "sy contour"]
data_all[grep("sy sign et", variety), variety := "sy signet"]
data_all[grep("vilg ott", variety), variety := "vilgott"]

# remove all observations with missing yield
data_all = data_all[!(is.na(dry_matter_yield))] # 306200 -> 64346 (this is because of long format of nfts data)

# fix date formats
data_all[, `:=` (sowing_date = as.Date(sowing_date, format = "%Y-%m-%d"),
                 measure_date = as.Date(measure_date, format = "%Y-%m-%d"))]


# manually fix date typos for some nfts trials based on looking up the trials at nfts web page         
data_all[nfts_id == 3503, `:=` (
  year = 1999,
  measure_date = as.Date(paste0(1999, substr(as.character(measure_date),5,10), format = "%Y-%m-%d")),
  sowing_date = as.Date(paste0(1999, substr(as.character(sowing_date),5,10), format = "%Y-%m-%d"))
  )]

data_all[nfts_id == 37495, `:=` (
  year = 2008,
  measure_date = as.Date(paste0(2008, substr(as.character(measure_date),5,10), format = "%Y-%m-%d")),
  sowing_date = as.Date(paste0(2008, substr(as.character(sowing_date),5,10), format = "%Y-%m-%d"))
  )]

data_all[nfts_id == 40942, `:=` (
  year = 2010,
  measure_date = as.Date(paste0(2010, substr(as.character(measure_date),5,10), format = "%Y-%m-%d")),
  sowing_date = as.Date(paste0(2010, substr(as.character(sowing_date),5,10), format = "%Y-%m-%d"))
  )]

data_all[nfts_id == 44115, `:=` (
  year = 2012,
  measure_date = as.Date(paste0(2012, substr(as.character(measure_date),5,10), format = "%Y-%m-%d")),
  sowing_date = as.Date(paste0(2012, substr(as.character(sowing_date),5,10), format = "%Y-%m-%d"))
  )]

data_all[nfts_id == 47626, `:=` (
  year = 2014,
  measure_date = as.Date(paste0(2014, substr(as.character(measure_date),5,10), format = "%Y-%m-%d")),
  sowing_date = as.Date(paste0(2014, substr(as.character(sowing_date),5,10), format = "%Y-%m-%d"))
  )]

data_all[nfts_id == 47627, `:=` (
  year = 2014,
  measure_date = as.Date(paste0(2014, substr(as.character(measure_date),5,10), format = "%Y-%m-%d")),
  sowing_date = as.Date(paste0(2014, substr(as.character(sowing_date),5,10), format = "%Y-%m-%d"))
  )]

# add Julian date
data_all[, sowing_date_J := lubridate::yday(sowing_date)]
data_all[, measure_date_J := lubridate::yday(measure_date)]

# fix wrong sowing date
data_all[nfts_id == 47627, sowing_date := as.Date("2014-03-24", format = "%Y-%m-%d")]
data_all[nfts_id == 47627, sowing_date_J := lubridate::yday(sowing_date)]

data_all[measure_date_J < 150 | measure_date_J > 325, `:=` (measure_date = NA, measure_date_J = NA)]

# reorder columns
setcolorder(data_all,
c("trial", "nfts_id", "year", 
"country", "longitude", "latitude", "location_info", 
"variety", "old_variety_name", "row", "early",
"sowing_date", "sowing_date_J", "measure_date", "measure_date_J", 
"dry_matter_yield",
"treatment_group","treatment",
"experiment_design", "remark")
)

# set key 
setkey(data_all, country, year, location_info, trial, variety)

setnames(data_all, "location_info", "region")

data_all[region == "3Sejet Planteforædling" & is.na(longitude), `:=` (longitude=9.94398, latitude=55.82324)]

# save as csv
data.table::fwrite(data_all, paste0(datadir, "01_BarleyData.csv"), row.names = FALSE)
