# get coordinates from ggmap for nfts field trials with missing coordinates

contains_numbers <- function(x) {
  x[grepl("\\d", x)]
}


datadir = "./"
potato = data.table::fread(paste0(datadir, "/nfts/final/potato_info.csv"))
clover = data.table::fread(paste0(datadir, "/nfts/final/red-clover_info.csv"))
barley = data.table::fread(paste0(datadir, "/nfts/final/spring-barley_info.csv"))


loc = rbind(potato, clover, barley)[is.na(longitude)|is.na(latitude),.(id, address, longitude, latitude)]


address_split = strsplit(loc$address, ",")

loc[which((lapply(lapply(address_split, contains_numbers),length) |> unlist()) == 1), only_address := lapply(address_split, contains_numbers) |> unlist()]


tmp = loc[!duplicated(only_address), .(id, only_address)]
tmp
tmp[, postcode := c(
    "9330, Dronninglund",
    "7540, Haderup",
    "7430, Ikast",
    "8305, Samsø",
    "6360, Tinglev",
    "",
    "6500, Vojens",
    "6240, Løgumkloster",
    "5700, Svendborg",
    "7400, Herning", 
    "90432",
    "8830, Tjele", 
    "4230, Skælskør", 
    "88195, UNDROM", 
    "", 
    "83694, ÅS", 
    "7870, Roslev", 
    "6270, Tønder", 
    "6240, Løgumkloster", 
    "7490, Aulum", 
    "7100, Vejle", 
    "7760, Hurup", 
    "8220, Brabrand", 
    "5853, Ørbæk", 
    "6715, Esbjerg", 
    "6753, Agerbæk", 
    "6650, Brørup", 
    "6091, Bjert", 
    "6870, Ølgod", 
    "8471, Sabro", 
    "7870, Roslev", 
    "4270, Høng", 
    "8700, Horsens", 
    "4920, Søllested", 
    "8700, Horsens", 
    "6240, Løgumkloster", 
    "6360, Tinglev", 
    "8700, Horsens", 
    "4960, Holeby", 
    "7800, Skive", 
    "4800, Nykøbing", 
    "2219, Brandval", 
    "2618, Lillehammer", 
    "4863, Eskilstrup", 
    "4653, Karise", 
    "4340, Bryne"
)]

loc[tmp, postcode := i.postcode, on = "only_address"]

loc[, full_address := paste0(only_address, ", ", postcode)]

data.table::fwrite(loc[, .(id, address, only_address, postcode, full_address)], paste0(datadir, "/nfts-address.csv"))
