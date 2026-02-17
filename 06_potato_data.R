### NB! This script use data files that are not publicly available ###
### To accsess the additional data files, please contact smvandeskog@nr.no ###

### Potato data processing script ###
# script for reading in all potato field trial data provided from
# Norway and NFTS (Sweden + Denmark),
# processing and combining into one dataset

library(data.table)
rm(list = ls())
datadir = "./"

# functions -----------------------------------------------------

# construct new trial variable that is unique
add_leading_zero = function(x){
    x_new = paste0("000",x)
    return(substr(x_new, nchar(x_new)-3, nchar(x_new)))
}

# NFTS data -------------------------------------------------

data = data.table::fread(paste0(datadir, "/nfts/final/potato_data.csv"))
info = data.table::fread(paste0(datadir, "/nfts/final/potato_info.csv"))
info2 = data.table::fread(paste0(datadir, "/nfts-address-geocoded.csv"))
info3 = data.table::fread(paste0(datadir, "/nfts-address.csv")) 
info3[info2, `:=` (longitude = i.lon, latitude = i.lat), on = c("full_address" = "full_address_str")]
info[info3, `:=` (lon = i.longitude, lat = i.latitude), on = c("id")]
info[is.na(longitude), `:=` (longitude = lon, latitude = lat)]

interesting_trials = c(
#   "Afprøvning af kartoffelsorter til økologisk stivelsesproduktion",
  "Afprøvning af melkartoffelsorter, høst 1. oktober",
  "Afprøvning af melkartoffelsorter, høst 1. september",
#   "Afprøvning af spisesorter i økologisk produktion",
  "Afprøvning af spisesorter i spisekartofler",
  "Afprøvning af stivelseskartoffelsorter",
#   "Afprøvning af stivelseskartoffelsorter – vandet og uvandet",
#   "Afprøvning af stivelseskartoffelsorter ved forskellige kvælstof-tildelinger",
#   "Afprøvning af stivelseskartoffelsorter ved forskellige N-tildelinger",
  "Afprøvning af stivelseskartoffelsorter, høst 1. oktober",
  "Afprøvning af stivelseskartoffelsorter, høst 1. september",
  "Betydning af klorholdige gødninger i spisekartofler",
  "Delt gødskning til stivelseskartoffelsorter",
#   "DEMO: Skimmelbekæmpelse i højresistente kartoffelsorter",
  "Demonstration af kartoffelsorter",
  "Fabrikskartoffelsorter",
  "Fungicider mot bladmögel och brunröta i matpotatis.",
  "Havresorter",
#   "Kopi af 040181616-001 analyser ledniveau",
  "Kvælstofoptimum i stivelseskartofler (4 sorter)",
  "Matpotatis. Sortförsök. Ekologisk odling.",
  "Meget tidl. kartoffelsorter med plastdække, sen optagning",
  "Meget tidl. kartoffelsorter med plastdække, tidlig optagning",
  "Meget tidl. kartoffelsorter uden plastdække, sen optagning",
  "Meget tidlige kartoffelsorter med plastdække, sen optagning",
  "Meget tidlige kartoffelsorter uden plastdække, sen optagning",
  "Meget tidlige kartoffelsorter, med plast, sen optagning",
  "Meget tidlige kartoffelsorter, sen optagning, med plastdækning",
  "Meget tidlige kartoffelsorter, tidlig optagning, med plastdækning",
  "Meget tidlige kartoffelsorter, uden plast, sen optagning",
  "Meget tidlige kartofler",
  "Meget tidlige kartofler, med plast, sen optagn",
  "Meget tidlige kartofler, med plast, sen optagning",
  "Meget tidlige kartofler, med plast, tidlig optagning",
  "Meget tidlige kartofler, uden plast, sen optagning",
  "Middeltidlige kartoffelsorter med plastdække, sen optagning",
  "Middeltidlige kartoffelsorter med plastdække, tidlig optagning",
  "Middeltidlige kartoffelsorter uden plastdække, sen optagning",
  "Middeltidlige kartoffelsorter, med plast, sen optagning",
  "Middeltidlige kartoffelsorter, med plast, tidlig optagning",
  "Middeltidlige kartoffelsorter, sen optagning, med plastdækniing",
  "Middeltidlige kartoffelsorter, sen optagning, uden plastdækniing",
  "Middeltidlige kartoffelsorter, tidlig optagning, med plastdækning",
  "Middeltidlige kartoffelsorter, uden plast, sen optagning",
  "Middeltidlige kartofler med plastdække, sen optagning",
  "Middeltidlige kartofler med plastdække, tidlig optagning",
  "Middeltidlige kartofler uden plastdække, sen optagning",
  "Middeltidlige kartofler, med plast, sen optagn",
  "Middeltidlige kartofler, med plast, sen optagning",
  "Middeltidlige kartofler, med plast, tidlig optagning",
  "Middeltidlige kartofler, uden plast, sen optagning",
  "Mideltidlige kartofler, uden plast, sen optagning",
#   "N optimum og P følsomhed i forbindelse med lægning af stivelseskartofler i små parceller",
#   "N-optimum i nye resistente stivelsessorter",
  "Nematodresistente fabrikskartoffelsorter",
#   "Planteafstand ved dyrkning af Fontane",
#   "Provning av olika bekämpningsstrategier med fungicider mot potatis bladmögel och brunröta.",
  "Sommerkartofler",
  "Sommerkartofler middeltidlige",
  "Sommerkartofler-middeltidlige",
  "Sorter til kartoffelpulver og chipsproduktion",
  "Sorter til kartoffelpulverproduktion, høst først i august",
  "Sorter til kartoffelpulverproduktion, høst først september",
  "Tdlige kartofler, med plast, tidlig optagning",
#   "TEST BEREGNINGER Kartofler 040181818 LED Afprøvning af stivelseskartoffelsorter, høst 1. ",
  "Tidl. kartoffelsorter med plastdække, sen optagning",
  "Tidl. kartoffelsorter med plastdække, tidlig optagning",
  "Tidl. kartoffelsorter uden plastdække, sen optagning",
  "Tidlige kartoffelsorter med plastdække, sen optagning",
  "Tidlige kartoffelsorter med plastdække, tidlig optagning",
  "Tidlige kartoffelsorter uden plastdække, sen optagning",
  "Tidlige kartoffelsorter, med plast, sen optagning",
  "Tidlige kartoffelsorter, med plast, tidlig optagning",
  "Tidlige kartoffelsorter, sen optagning, med plastdækning",
  "Tidlige kartoffelsorter, sen optagning, uden plastdækning",
  "Tidlige kartoffelsorter, tidlig optagning, med plastdækning",
  "Tidlige kartoffelsorter, uden plast, sen optagning",
  "Tidlige kartofler",
  "Tidlige kartofler, med plast, sen optagning",
  "Tidlige kartofler, med plast, tidlig optagning",
  "Tidlige kartofler, med plast, tidlig optagningen",
  "Tidlige kartofler, uden plast, sen optagn",
  "Tidlige kartofler, uden plast, sen optagning"
)


interesting_lfe = c("11SLU Norrländsk Jordbruksvetenskap",
                    "12HS Gotland",
                    "4HS Halland L:a Böslid",
                    "5HS Skåne",
                    "5HS Skaraborg Logården",
                    "1Planteforsøg Sydjylland I/S",
                    "Landsforsøg Østjylland",
                    "4Jyndevad Forsøgstation",
                    "LandboNord",
                    "Ytteborg")

info = info[experiment_title %in% c(interesting_trials) & lfe %in% c(interesting_lfe)] # 335 forsøk
info[country == "", country := "Sweden"]
info[(experiment_design != ""), experiment_design2 := sapply(strsplit(info[(experiment_design != "")]$experiment_design,"[.]"),function(x){x[[1]]})]

data[info, `:=` (year = i.year,
                country = i.country,
                lfe = i.lfe,
                longitude = i.longitude,
                latitude = i.latitude,
                crop_type = i.crop_type,
                sowing_date = i.sowing_date,
                experiment_design = i.experiment_design,
                experiment_title = i.experiment_title), on = .(id)]

## fjern uinteressant forsøk
ids = info$id
data = data[id %in% ids]

## Fjerne disse (treatment er ikke sort, men gjødsel (eller tilsvarende))
data_final = data[!(id %in% (data[grep("(NS)|(NPK)", variety)]$id |> unique()))]
info_final = info[id %in% data_final$id] # 303 forsøk

data_final[country == "Denmark", trial := paste0("XPODK", add_leading_zero(.GRP), "X"), by = .(id)]
data_final[country == "Sweden", trial := paste0("XPOSW", add_leading_zero(.GRP), "X"), by = .(id)]

## To make all observations into absolute numbers we do as follows:
# Typically the first observations of each trial is the reference value (here called absolute_yield_value)
# We manually correct for exceptions, i.e. cases where it is not the first observations that is the reference
# We then compute yield = relative*absolute_yield_value

# Yield
data_final[!is.na(yield_and_yield_increase), nr := 1:.N, by = .(id)]
data_final[!is.na(yield_and_yield_increase), absolute_yield_value := yield_and_yield_increase[nr == 1], by = .(id)]

## fix for exceptions
data_final[!is.na(yield_and_yield_increase) & id == 31525, absolute_yield_value := yield_and_yield_increase[nr == 8], by = .(id)]
data_final[!is.na(yield_and_yield_increase) & id %in% c(28880, 29403, 29465,29564), absolute_yield_value := yield_and_yield_increase[nr == 3], by = .(id)]

# data_final[yield_and_yield_increase>absolute_yield_value]

data_final[, yield_relative_to_one_number := round(yield_and_yield_increase/absolute_yield_value*100)]
data_final[nr!=1 & !(id %in% c(28880, 29403, 29465, 31525,29564)), yield_relative_to_one_number := 100 + yield_relative_to_one_number]

## fix for exceptions
data_final[nr!=8 & id == 31525, yield_relative_to_one_number := 100 + yield_relative_to_one_number]
data_final[nr!=3 & id %in% c(28880, 29403, 29465,29564), yield_relative_to_one_number := 100 + yield_relative_to_one_number]

# data_final[, .(all(yield_relative_to_one_number>99)), by = .(trial, id)][V1==TRUE]
## only rounding errors remain
# data_final[yield_relative_number != yield_relative_to_one_number, .(trial, id, nr, yield_and_yield_increase, yield_relative_number, yield_relative_to_one_number)] |> View()

data_final[, yield := absolute_yield_value*yield_relative_to_one_number/100]
data_final[, `:=` (nr = NULL, absolute_yield_value = NULL, yield_relative_to_one_number = NULL)]

# Dry matter yield
data_final[!is.na(dry_matter_yield_and_yield_increase), nr := 1:.N, by = .(id)]
data_final[!is.na(dry_matter_yield_and_yield_increase), absolute_yield_value := dry_matter_yield_and_yield_increase[nr == 1], by = .(id)]

data_final[, yield_relative_to_one_number := round(dry_matter_yield_and_yield_increase/absolute_yield_value*100)]
data_final[nr!=1 & !(id %in% c()), yield_relative_to_one_number := 100 + yield_relative_to_one_number]

# data_final[dry_matter_yield_and_yield_increase>absolute_yield_value]

data_final[is.na(dry_matter_yield), dry_matter_yield := absolute_yield_value*yield_relative_to_one_number/100]
data_final[, `:=` (nr = NULL, absolute_yield_value = NULL, yield_relative_to_one_number = NULL)]

# Starch
data_final[!is.na(starch_yield_and_yield_increase), nr := 1:.N, by = .(id)]
data_final[!is.na(starch_yield_and_yield_increase), absolute_yield_value := starch_yield_and_yield_increase[nr == 1], by = .(id)]

## fix for exceptions
data_final[!is.na(starch_yield_and_yield_increase) & id %in% c(28880, 29403, 29465, 29564), absolute_yield_value := starch_yield_and_yield_increase[nr == 3], by = .(id)]

data_final[, yield_relative_to_one_number := round(starch_yield_and_yield_increase/absolute_yield_value*100)]
data_final[nr!=1 & !(id %in% c(28880, 29403, 29465, 29564)), yield_relative_to_one_number := 100 + yield_relative_to_one_number]

## fix for exceptions
data_final[nr!=3 & id %in% c(28880, 29403, 29465, 29564), yield_relative_to_one_number := 100 + yield_relative_to_one_number]

data_final[is.na(starch_yield), starch_yield := absolute_yield_value*yield_relative_to_one_number/100]
data_final[, `:=` (nr = NULL, absolute_yield_value = NULL, yield_relative_to_one_number = NULL)]

# data_final[id == 39492]

data_final[,remark := paste0("NFTS experiment title: ",experiment_title)]

data_final[grep(", Dyrkningssted A", variety) , treatment := "Dyrkningssted A"]
data_final[grep(", Dyrkningssted B", variety) , treatment := "Dyrkningssted B"]
data_final[grep("Obehandlat, ", variety) , treatment := "Obehandlat"]

data_final[, variety := gsub(", Dyrkningssted A", "", variety)]
data_final[, variety := gsub(", Dyrkningssted B", "", variety)]
data_final[, variety := gsub("Obehandlat, ", "", variety)]

data_final[is.na(size_40mm_60mm_in_percentage), size_40mm_60mm_in_percentage := size_40mm_50mm_in_percentage + size_50mm_60mm_in_percentage]

data_final[, early := 0]
data_final[grep("(tidl)|(sommer)", tolower(experiment_title)), early := 1]
tab = table(data_final$variety, data_final$early)
data_final[variety %in% rownames(tab[which(rowSums(tab !=0)>1),]), early := 0.5]

data_final[crop_type %in% c("Kartofler", "Matpotatis", "Spisekartofler"), crop_type := "table potato"]
data_final[crop_type == "Fabrikskartofler", crop_type := "industry potato"]

data_final[variety %in% c("Anna Bella"), crop_type := "table potato"]
data_final[variety %in% c("Fontane", "Liva", "Panda", "Saturna"), crop_type := "industry potato"]
setnames(data_final, "crop_type", "potato_type")

dtnfts = copy(data_final)

# Norwegian data --------------------------------------------------

dtno = readxl::read_xlsx(paste0(datadir, "potato_norway.xlsx"), skip = 8) |> as.data.table()
dtno_coords = readxl::read_xlsx(paste0(datadir, "norway-potato-coords-trials.xlsx")) |> as.data.table()

yield_keep = c(
"Totalavling kg/daa",
"Totalavling > 40 mm kg/daa",
"TS %",
"TS kg/daa",
"Stivelse, %",
"Knollansett, pr plante",
"Knollantall/plante (>40 mm)",
"% <40 mm",
"%40-50mm",
"%50-60mm",
"%>60mm"
)

meta_keep = c(
"Sort",
"År",
"Region",
"Felt",
"PLAN NR",
"Gjødsling",
"rep",
"rutenr",
"leddnr",
"Sort             GODKJENT"
)


dtno = dtno[, .SD, .SDcols = c(yield_keep, meta_keep)]
dtno[, country := "Norway"]

setnames(dtno, 
    old = c("Totalavling kg/daa", "Totalavling > 40 mm kg/daa", "TS %", "TS kg/daa", 
    "Stivelse, %", "Knollansett, pr plante", "Knollantall/plante (>40 mm)", 
    "% <40 mm", "%40-50mm", "%50-60mm", "%>60mm", "Sort", "År", "Region", "Felt", "rutenr", "Sort             GODKJENT"),
    new = c("yield", "yield_over40mm", "dry_matter_in_percentage", "dry_matter_yield",
    "starch_in_percentage", "tubers_per_plant", "tubers_per_plant_over40mm", 
    "size_under40mm_in_percentage", "size_40mm_50mm_in_percentage", "size_50mm_60mm_in_percentage", "size_over60mm_in_percentage",
    "old_variety_name", "year", "region", "location", "plotnr", "variety"))

dtno[, location_info := tolower(paste0(region, "(",location,")"))]
dtno_coords[, location_info := tolower(Stedsbeskrivelse)]
dtno[dtno_coords, `:=` (longitude = i.Longitude, latitude = i.Latitude), on = "location_info"]
dtno[is.na(variety), variety := old_variety_name]
dtno[old_variety_name == variety, old_variety_name := NA]
dtno[, size_40mm_60mm_in_percentage := size_40mm_50mm_in_percentage + size_50mm_60mm_in_percentage]

cols = c("yield", "yield_over40mm", "dry_matter_yield", "starch_in_percentage")
dtno[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
cols=cols[-4]
dtno[, (cols) := lapply(.SD, function(x){x/10}), .SDcols = cols]
dtno[, starch := starch_in_percentage*yield/100]

# remove trials with missing info
dtno = dtno[starch_in_percentage > 0]
dtno[, trial := paste0("XPONO", add_leading_zero(.GRP), "X"), by = .(year, region, location)]

# combine data -------------------------------------------------------

setnames(dtnfts, 
    old = c("lfe", "id", "date", "starch_yield"), 
    new = c("location_info", "nfts_id", "measure_date", "starch"))


col_keep = c("trial","nfts_id", "year", "country", "location_info",   
            "longitude", "latitude",
            "variety", "old_variety_name", "potato_type", "early", "sowing_date", "measure_date", "rep", "plotnr",
            "yield", "yield_over40mm", "dry_matter_in_percentage", "dry_matter_yield",
            "starch", "starch_in_percentage", "plant_population_1000_per_ha", "tubers_per_plant", "tubers_per_plant_over40mm", 
            "size_under40mm_in_percentage", "size_40mm_60mm_in_percentage", "size_over60mm_in_percentage", 
            "experiment_design", "remark")


data_all = rbindlist(list(dtnfts, dtno), fill = TRUE)
data_all = data_all[, ..col_keep]

setkey(data_all, trial, year, variety)
data_all[, variety := tolower(variety)]
data_all[, variety := gsub(" &", "", variety)]

# format date variable
data_all[, `:=` (measure_date = as.Date(measure_date, format = "%Y-%m-%d"))]
data_all[, `:=` (sowing_date = as.Date(sowing_date, format = "%Y-%m-%d"))]

# fix typo
data_all[nfts_id %in%  c(36253, 36254), `:=` (
  measure_date = as.Date(paste0(2007, substr(as.character(measure_date),5,10)), format = "%Y-%m-%d"))]

# add Julian date
data_all[, measure_date_J := lubridate::yday(measure_date)]
data_all[, sowing_date_J := lubridate::yday(sowing_date)]

yield_cols=c("yield", "yield_over40mm", 
            "dry_matter_yield","dry_matter_in_percentage", 
            "starch", "starch_in_percentage", 
            # "plant_population_1000_per_ha", "tubers_per_plant", "tubers_per_plant_over40mm", 
            "size_under40mm_in_percentage", "size_40mm_60mm_in_percentage", "size_over60mm_in_percentage")

data_all[starch > 300, starch := NA]
data_all[yield <= 0, yield := NA]
data_all[yield > 1500, yield := NA]
data_all[yield_over40mm > 1500, yield_over40mm := NA]
data_all[dry_matter_yield > 500, dry_matter_yield  := NA]
data_all[is.na(starch), starch := starch_in_percentage*yield/100]
data_all[is.na(dry_matter_yield), dry_matter_yield := dry_matter_in_percentage*yield/100]
# data_all[dry_matter_in_percentage < 0, dry_matter_in_percentage  := NA]
# data_all[starch_in_percentage < 0, starch_in_percentage  := NA]
summary(data_all[, .SD, .SDcols = yield_cols])

# remove rows where all yield variables are missing
rmv = which(data_all[, .SD, .SDcols = yield_cols] |> apply(1,is.na) |> apply(2, all))
data_all = data_all[-rmv]

data_all[year != data.table::year(measure_date) & measure_date_J > sowing_date_J, `:=` (
  measure_date = as.Date(paste0(year, substr(as.character(measure_date),5,10)), format = "%Y-%m-%d"))]
data_all[, measure_date_J := lubridate::yday(measure_date)]

tmp = data_all[country != "Norway",.(variety, potato_type, early)] |> unique()
data_all[tmp, `:=` (potato_type = i.potato_type, early = i.early), on = "variety"]

# set column order
setcolorder(data_all,
c("trial", "nfts_id", "year",
"country", "longitude", "latitude", "location_info", 
"variety", "old_variety_name", "potato_type", "early", "rep", "plotnr", 
"sowing_date", "sowing_date_J", "measure_date", "measure_date_J",
"yield", "yield_over40mm", 
"dry_matter_yield", "dry_matter_in_percentage",
"starch", "starch_in_percentage", "plant_population_1000_per_ha", "tubers_per_plant", 
"tubers_per_plant_over40mm", "size_under40mm_in_percentage", 
"size_40mm_60mm_in_percentage", "size_over60mm_in_percentage",
"experiment_design", "remark")
)

# set key
setkey(data_all, country, year, location_info, trial, variety, rep)

setnames(data_all, "location_info", "region")

## Fill inn manually based on average lon/lat values of other trials from the same region
data_all[is.na(longitude) & region=="4Jyndevad Forsøgstation", `:=`(longitude = 9.126143, latitude = 54.897989)]
data_all[is.na(longitude) & region=="Landsforsøg Østjylland", `:=`(longitude = 10.63393, latitude = 55.84769)]
data_all[
  is.na(longitude) & region == "11SLU Norrländsk Jordbruksvetenskap",
  `:=`(longitude = 20.24200, latitude = 63.81082)
]


# dave data as csv
data.table::fwrite(data_all, paste0(datadir, "03_PotatoData.csv"), row.names = FALSE)

