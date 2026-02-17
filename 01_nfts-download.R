# This script is used to download field trial data from the NFTS web page

library(data.table)
library(rvest)
library(polite)
library(xml2)
library(readxl)
library(parallel)

n_cores = 10

url_base_compressed = "https://nfts.dlbr.dk/Forms/KomprimeretDokumentation.aspx"
url_base_full = "https://nfts.dlbr.dk/Forms/Dokumentation.aspx"

data_dir = "./nfts"
if (!dir.exists(data_dir)) dir.create(data_dir)

# folder for downloading all raw http data from the nfts website
raw_data_dir = file.path(data_dir, "raw")
if (!dir.exists(raw_data_dir)) dir.create(raw_data_dir)

# folder for all temporary processing of the raw http data
tmp_data_dir = file.path(data_dir, "tmp")
if (!dir.exists(tmp_data_dir)) dir.create(tmp_data_dir)

# folder for storing the final results
final_dir = file.path(data_dir, "final")
if (!dir.exists(final_dir)) dir.create(final_dir)

# Document for keeping track of the experiments that contain interesting crop types
meta_path = file.path(tmp_data_dir, "meta.csv")
# Document for keeping track of all info from the NFTS excel files
excel_meta_path = file.path(tmp_data_dir, "excel_meta.csv")

crop_types = c("Vårbyg", "Kartofler", "Spisekartofler", "Fabrikskartofler", "Rødkløver")

# ==============================================================================
# Helper functions
# ==============================================================================

source("./01.1-nfts-download-functions.R")

# ==============================================================================
# Loop over all available nfts websites and download info files
# ==============================================================================

max_id = 99999

if (!file.exists(meta_path)) {
  meta = data.table(
    id = seq_len(max_id),
    bad_experiment = FALSE
  )
  fwrite(meta, meta_path)
}

meta = fread(meta_path)

ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(ids))
session = polite::bow(url_base_compressed, times = 1)
for (i in seq_along(ids)) {
  pb$tick()
  id = sprintf("%05d", ids[i])
  out_path = file.path(raw_data_dir, "compressed", paste0(id, ".html"))
  if (file.exists(out_path)) next
  contents = polite::scrape(session, query = list(KardexID = id))
  if (!is.null(contents)) xml2::write_html(contents, out_path)
}
pb$terminate()

# ==============================================================================
# Update meta with information about ids with available data
# ==============================================================================

if (is.null(meta$missing_html_data)) meta$missing_html_data = FALSE

ids = sprintf("%05d", 1:99999)
out_paths = file.path(raw_data_dir, "compressed", paste0(ids, ".html"))
file_is_available = file.exists(out_paths)

meta[!file_is_available, let(
  bad_experiment = TRUE,
  missing_html_data = TRUE
)]

fwrite(meta, meta_path)

# ==============================================================================
# Loop over all the downloaded html files
# and extract the relevant info into data.tables that we write to disk
# ==============================================================================

meta = fread(meta_path)

ids = meta[bad_experiment == FALSE, id]
start_time = Sys.time()
success = parallel::mclapply(
  X = seq_along(ids),
  mc.preschedule = FALSE,
  mc.cores = n_cores,
  FUN = function(i) {
    if (i %% 100 == 0) {
      time_passed = Sys.time() - start_time
      message(
        i, " / ", length(ids), ", time passed: ",
        round(time_passed, 2), " ", attr(time_passed, "unit")
      )
    }
    id = sprintf("%05d", ids[i])
    in_path = file.path(raw_data_dir, "compressed", paste0(id, ".html"))
    info_path = file.path(tmp_data_dir, "info", paste0(id, ".csv"))
    if (!file.exists(info_path)) {
      contents = xml2::read_html(in_path)
      info = get_compressed_info(contents, language = "danish")
      if (is.null(info)) return(FALSE)
      info$experiment_id = id
      data.table::fwrite(info, info_path)
    }
    TRUE
  })

summary(unlist(success))

# Update meta with info about the downloaded info files
# ------------------------------------------------------------------------------

if (is.null(meta$country)) meta$country = NA_character_
if (is.null(meta$surely_no_variety_data)) meta$surely_no_variety_data = FALSE
if (is.null(meta$any_unknown_treatments)) meta$any_unknown_treatments = FALSE
if (is.null(meta$crop_type)) meta$crop_type = NA_character_
if (is.null(meta$available_info)) meta$available_info = TRUE

pb = get_progress_bar(nrow(meta))
for (i in seq_len(nrow(meta))) {
  pb$tick()
  id = sprintf("%05d", meta$id[i])
  info_path = file.path(tmp_data_dir, "info", paste0(id, ".csv"))
  if (!file.exists(info_path)) {
    meta[i, let(
      available_info = FALSE,
      bad_experiment = TRUE
    )]
    next
  }
  info = data.table::fread(info_path)
  meta[i, let(
    crop_type = unique(info$crop_type),
    country = unique(info$plan_email_country),
    any_unknown_treatments = any(info$any_unknown_treatments),
    surely_contains_variety_data = any(info$surely_contains_variety_data)
  )]
  if (meta[i, !any_unknown_treatments && !surely_contains_variety_data]) {
    meta[i, let(
      bad_experiment = TRUE,
      surely_no_variety_data = TRUE
    )]
  }
}
pb$terminate()

fwrite(meta, meta_path)

# ==============================================================================
# Identify which experiments that contain interesting crop types
# ==============================================================================

meta = fread(meta, meta_path)
table(meta$crop_type) |>
  sort(decreasing = TRUE) |>
  print(max = 5000)

meta[, let(
  good_crop_type = crop_type %in% crop_types,
  crop_category = data.table::fcase(
    grepl("kartofler", tolower(crop_type)), "potato",
    crop_type == "Vårbyg", "spring barley",
    crop_type == "Rødkløver", "red clover",
    default = NA
  )
)]
meta[good_crop_type == FALSE, let(bad_experiment = TRUE)]

fwrite(meta, meta_path)

# ==============================================================================
# Download info data with Swedish language settings for the Swedish experiments
# ==============================================================================

meta = fread(meta_path)
swedish_meta = meta[country == "Sweden" & good_crop_type == TRUE]

pb = get_progress_bar(nrow(swedish_meta))
session = polite::bow(url_base_compressed, times = 1)
for (i in seq_len(nrow(swedish_meta))) {
  pb$tick()
  id = sprintf("%05d", swedish_meta$id[i])
  path = file.path(raw_data_dir, "compressed_swedish", paste0(id, ".html"))
  if (file.exists(path)) next
  contents = polite::scrape(session, query = list(KardexID = id, applLangID = "sv"))
  if (!is.null(contents)) xml2::write_html(contents, path)
}
pb$terminate()

# ==============================================================================
# Loop over all the downloaded swedish html files
# and extract the relevant info into data.tables that we write to disk.
# Overwrite the danish info files for the swedish experiments
# ==============================================================================

start_time = Sys.time()
success = parallel::mclapply(
  X = seq_len(nrow(swedish_meta)),
  mc.preschedule = FALSE,
  mc.cores = n_cores,
  FUN = function(i) {
    if (i %% 100 == 0) {
      time_passed = Sys.time() - start_time
      message(
        i, " / ", nrow(swedish_meta),
        ", time passed: ", round(time_passed, 2), " ", attr(time_passed, "unit")
      )
    }
    id = sprintf("%05d", swedish_meta$id[i])
    in_path = file.path(raw_data_dir, "compressed_swedish", paste0(id, ".html"))
    info_path = file.path(tmp_data_dir, "info", paste0(id, ".csv"))
    contents = xml2::read_html(in_path)
    info = get_compressed_info(contents, language = "swedish")
    if (is.null(info)) return(FALSE)
    info$experiment_id = id
    data.table::fwrite(info, info_path)
    TRUE
  })

summary(unlist(success))

# Update meta with info about the downloaded info files
# ------------------------------------------------------------------------------

pb = get_progress_bar(nrow(swedish_meta))
for (i in seq_len(nrow(swedish_meta))) {
  pb$tick()
  my_id = swedish_meta$id[i]
  info_path = file.path(tmp_data_dir, "info", paste0(sprintf("%05d", my_id), ".csv"))
  if (!file.exists(info_path)) {
    meta[id == my_id, let(
      available_info = FALSE,
      bad_experiment = TRUE
    )]
    next
  }
  info = data.table::fread(info_path)
  meta[id == my_id, let(
    any_unknown_treatments = any(info$any_unknown_treatments),
    surely_contains_variety_data = any(info$surely_contains_variety_data)
  )]
  if (meta[id == my_id, !any_unknown_treatments && !surely_contains_variety_data]) {
    meta[id == my_id, let(
      bad_experiment = TRUE,
      surely_no_variety_data = TRUE
    )]
  }
}
pb$terminate()

fwrite(meta, meta_path)

# ==============================================================================
# Download variety trial data from all websites that either has any unknown
# treatments or that surely contains variety data.
# Use swedish language settings for the confirmed swedish experiments, and
# danish language settings for all other experiments
# ==============================================================================

meta = fread(meta_path)

all_ids = meta[bad_experiment == FALSE & country != "Sweden", id]
pb = get_progress_bar(length(all_ids))
session = polite::bow(url_base_full, times = 1)
for (i in seq_along(all_ids)) {
  pb$tick()
  id = sprintf("%05d", all_ids[i])
  path = file.path(raw_data_dir, "full", paste0(id, ".html"))
  if (file.exists(path)) next
  contents = polite::scrape(session, query = list(KardexID = id))
  if (!is.null(contents)) xml2::write_html(contents, path)
}
pb$terminate()

all_ids = meta[bad_experiment == FALSE & country == "Sweden", id]
pb = get_progress_bar(length(all_ids))
session = polite::bow(url_base_full, times = 1)
for (i in seq_along(all_ids)) {
  pb$tick()
  id = sprintf("%05d", all_ids[i])
  path = file.path(raw_data_dir, "full_swedish", paste0(id, ".html"))
  if (file.exists(path)) next
  contents = polite::scrape(session, query = list(KardexID = id, applLangID = "sv"))
  if (!is.null(contents)) xml2::write_html(contents, path)
}
pb$terminate()

# Update meta with information about experiments with missing data
meta = fread(meta_path)

if (is.null(meta$available_data)) meta$available_data = TRUE

all_ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(all_ids))
for (i in seq_along(all_ids)) {
  pb$tick()
  country = meta[id == all_ids[i], country]
  path = file.path(
    raw_data_dir,
    paste0("full", ifelse(country == "Sweden", "_swedish", "")),
    paste0(sprintf("%05d", all_ids[i]), ".html")
  )
  if (!file.exists(path)) {
    meta[id == all_ids[i], let(
      available_data = FALSE,
      bad_experiment = TRUE
    )]
  }
}
pb$terminate()

fwrite(meta, meta_path)

# ==============================================================================
# Loop over all the downloaded html files and extract the relevant data into
# data.tables that we write to disk
# ==============================================================================

my_melt = function(dt, id.vars) {
  dt1 = dt[1:2, -..id.vars]
  dt1 = data.table(
    info1 = names(dt1),
    info2 = unlist(dt1[1, ]),
    info3 = unlist(dt1[2, ]),
    variable = factor(seq_len(ncol(dt1)))
  )
  dt2 = copy(dt[-(1:2)])
  change_name_index = which(!names(dt2) %in% id.vars)
  names(dt2)[change_name_index] = seq_len(nrow(dt1))
  dt2 = melt(dt2, id.vars = id.vars)
  dt2[, let(value = as.numeric(sub(",", ".", value)))]
  res = merge(dt2, dt1, by = "variable")
  res[, let(variable = NULL)]
  res
}

meta = fread(meta_path)

if (is.null(meta$interesting_tables)) meta$interesting_tables = TRUE

all_ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(all_ids))
for (i in seq_along(all_ids)) {
  pb$tick()

  id = sprintf("%05d", all_ids[i])
  country = meta[id == all_ids[i], country]
  in_path = file.path(
    raw_data_dir,
    paste0("full", ifelse(country == "Sweden", "_swedish", "")),
    paste0(id, ".html")
  )
  info_path = file.path(tmp_data_dir, "info", paste0(id, ".csv"))
  out_path = file.path(tmp_data_dir, "data", paste0(id, ".csv"))
  common_treatment_path = file.path(tmp_data_dir, "common_treatment", paste0(id, ".csv"))
  treatment_info_path = file.path(tmp_data_dir, "treatment_info", paste0(id, ".csv"))
  if (file.exists(out_path)) next

  contents = xml2::read_html(in_path)
  tables = rvest::html_table(contents)
  text = rvest::html_text2(contents)

  # Get GPS information, and place it into the info data
  gps_string = sub(".*gps: *(\\d+.\\d+, *\\d+.\\d+)[^\\d].*", "\\1", tolower(text))
  longitude = suppressWarnings(as.numeric(strsplit(gps_string, ",")[[1]][2]))
  latitude = suppressWarnings(as.numeric(strsplit(gps_string, ",")[[1]][1]))
  if (!any(is.na(longitude), is.na(latitude))) {
    info = fread(info_path)
    info$longitude = longitude
    info$latitude = latitude
    fwrite(info, info_path)
  }

  # Get experimental design information, and place it into the info data
  design_string = if (country == "Sweden") {
    sub(".*Försökstyp: *([^\r\n]+).*", "\\1", text)
  } else {
    sub(".*Forsøgstype: *([^\r\n]+).*", "\\1", text)
  }
  if (design_string != text) {
    design_string = sub(" +$", "", design_string)
    info = fread(info_path)
    info$design = design_string
    fwrite(info, info_path)
  }

  # Get soil type information and place it into the info data
  soil_column_names = c(
    ifelse(country == "Sweden", "Mätparameter", "Måleparameter"),
    ifelse(country == "Sweden", "Registrerat värde", "Målt værdi")
  )
  soil_table_index = which(sapply(
    tables,
    function(x) {
      any(names(x) == soil_column_names[1]) && any(grepl("^ *JB NR *$", x[[soil_column_names[1]]]))
    }
  ))
  if (length(soil_table_index) > 1) stop("Weird stuff for soil data extraction")
  if (length(soil_table_index) == 1) {
    soil_row_index = which(grepl("^ *JB NR *$", tables[[soil_table_index]][[soil_column_names[1]]]))
    jb_soil_type = as.numeric(tables[[soil_table_index]][[soil_column_names[2]]][soil_row_index])
    if (length(jb_soil_type) > 1) {
      # Choose the latest soil quality measurement that is non-NA
      jb_soil_type = tail(jb_soil_type[!is.na(jb_soil_type)], 1)
      if (length(jb_soil_type) != 1) stop("Weird stuff for soil data extraction")
    }
    info = fread(info_path)
    info$jb_soil_type = jb_soil_type
    fwrite(info, info_path)
  }

  # Get common treatment information and place it into the info data
  common_treatment_index = if (country == "Sweden") {
    which(sapply(
      tables,
      function(x) all(c("datum", "kategori", "medel/beh.") %in% tolower(names(x)))
    ))
  } else {
    which(sapply(
      tables,
      function(x) all(c("dato", "kategori", "middel") %in% tolower(names(x)))
    ))
  }
  if (length(common_treatment_index) > 1) stop("Weird stuff for common treatment index")
  if (length(common_treatment_index) == 1) {
    common_treatment_table = tables[[common_treatment_index]]
    if (nrow(common_treatment_table) > 0) fwrite(common_treatment_table, common_treatment_path)
  }

  # Get specific treatment information and place it into the info data
  treatment_index = if (country == "Sweden") {
    which(sapply(
      tables,
      function(x) all(c("led", "beh.", "tid", "medel/beh") %in% tolower(names(x)))
    ))
  } else {
    which(sapply(
      tables,
      function(x) all(c("led", "beh.", "tid", "middel") %in% tolower(names(x)))
    ))
  }
  if (length(treatment_index) > 1) {
    all_identical = all(sapply(
      X = tables[treatment_index],
      FUN = function(x) identical(names(x), names(tables[[treatment_index[1]]]))
    ))
    if (all_identical) {
      treatment_table = do.call(rbind, tables[treatment_index])
      if (nrow(treatment_table) > 0) fwrite(treatment_table, treatment_info_path)
    } else {
      message("Weird stuff for specific treatment index. id = ", id)
    }
  }
  if (length(treatment_index) == 1) {
    treatment_table = tables[[treatment_index]]
    if (nrow(treatment_table) > 0) fwrite(treatment_table, treatment_info_path)
  }

  interesting_index = which(sapply(
    tables,
    function(x) any(grepl("^ *[A-Z]0*\\d+:", names(x))) && any(names(x) == "")
  ))
  interesting_tables = tables[interesting_index]
  if (length(interesting_tables) == 0) {
    meta[id == all_ids[i], let(
      interesting_tables = FALSE,
      bad_experiment = TRUE
    )]
    next
  }

  index_columns = sapply(interesting_tables, function(x) which(names(x) == ""))
  if (any(table(index_columns) != length(interesting_tables))) {
    meta[id == all_ids[i], let(
      interesting_tables = FALSE,
      bad_experiment = TRUE
    )]
    next
  }
  index_columns = unique(as.vector(index_columns))
  index_column_names = paste0("index_column", seq_along(index_columns))

  for (j in seq_along(interesting_tables)) {
    names(interesting_tables[[j]])[index_columns] = index_column_names
    interesting_tables[[j]] = as.data.table(interesting_tables[[j]])
    interesting_tables[[j]] = my_melt(interesting_tables[[j]], id.vars = index_column_names)
  }

  table = rbindlist(interesting_tables)

  fwrite(table, out_path)
}
pb$terminate()

fwrite(meta, meta_path)


meta = fread(meta_path)

if (is.null(meta$bad_classification)) meta$bad_classification = FALSE

# Loop through the data to look for classification info,
# and update meta with information about bad classifications
all_classifications = list()
all_ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(all_ids))
for (i in seq_along(all_ids)) {
  pb$tick()

  id = sprintf("%05d", all_ids[i])
  country = meta[id == all_ids[i], country]
  in_path = file.path(
    raw_data_dir,
    paste0("full", ifelse(country == "Sweden", "_swedish", "")),
    paste0(id, ".html")
  )

  contents = xml2::read_html(in_path)
  tables = rvest::html_table(contents)
  text = rvest::html_text2(contents)

  # Get classification table, if it exists
  revision_string_is_in_text = grepl("\n[^[:alnum:]]*[rR]evision[^[:alnum:]]*\n", text)
  classification_string = ifelse(country == "Sweden", "Klassificering", "Klassifikation")
  classification_table_exists = any(sapply(
    tables,
    function(x) any(names(x) == classification_string)
  ))
  if (revision_string_is_in_text != classification_table_exists) {
    stop("Something is weird with i = ", i, ", id = ", id)
  }
  if (classification_table_exists) {
    table_index = which(sapply(tables, function(x) any(names(x) == classification_string)))
    table = tables[[table_index]]
    classification = unique(table[[classification_string]])
    if (is.na(classification) || classification != "OK") {
      meta[id == all_ids[i], let(
        bad_classification = TRUE,
        bad_experiment = TRUE
      )]
    }
  }
}

fwrite(meta, meta_path)


# Loop through all experiments to try to extract sowing and harvesting dates

all_ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(all_ids))
for (i in seq_along(all_ids)) {
  pb$tick()

  id = sprintf("%05d", all_ids[i])
  country = meta[id == all_ids[i], country]
  info_path = file.path(tmp_data_dir, "info", paste0(id, ".csv"))
  common_treatment_path = file.path(tmp_data_dir, "common_treatment", paste0(id, ".csv"))
  treatment_info_path = file.path(tmp_data_dir, "treatment_info", paste0(id, ".csv"))

  if (!file.exists(common_treatment_path)) next
  data = fread(common_treatment_path)
  if (nrow(data) == 0) {
    unlink(common_treatment_path)
    next
  }
  if (all(is.na(data$Kategori))) next
  if (any(data$Kategori == "")) {
    index = which(data$Kategori == "")
    index = index[index > 1]
    data$Kategori[index] = data$Kategori[index - 1]
    fwrite(data, common_treatment_path)
  }

  date_name = ifelse(country == "Sweden", "Datum", "Dato")
  harvest_category = ifelse(country == "Sweden", "Skörd och bärgning", "Høst og bjærgning")
  only_trial_name = ifelse(country == "Sweden", "Endast försöket", "Kun forsøg")
  scope_name = ifelse(country == "Sweden", "Syfte", "Omfang")
  treatment_name = ifelse(country == "Sweden", "Medel/Beh.", "Middel")
  harvest_type_names = if (country == "Sweden") {
    c("Sköredatum", "Trösknings-datum")
  } else {
    c("Høst-dato", "Tærske-dato")
  }

  data = data[data[[date_name]] != "", ]
  if (any(data$Kategori == harvest_category)) {
    harvest_data = data[data$Kategori == harvest_category, ]
    harvest_date = as.Date(harvest_data[[date_name]], format = "%d-%m-%Y")
    if (length(harvest_date) == 0) next
    if (length(harvest_date) > 1) {
      scope = harvest_data[[scope_name]]
      if (any(scope == only_trial_name)) harvest_data = harvest_data[scope == only_trial_name, ]
      harvest_date = as.Date(harvest_data[[date_name]], format = "%d-%m-%Y")
    }
    if (length(harvest_date) > 1) {
      type = harvest_data[[treatment_name]]
      for (name in harvest_type_names) {
        if (any(type == name)) harvest_data = harvest_data[type == name, ]
      }
      harvest_date = as.Date(harvest_data[[date_name]], format = "%d-%m-%Y")
    }
    if (length(harvest_date) > 1) {
      #message("Multiple harvest dates: i = ", i, ", id = ", id)
      harvest_date = NA
    }
    info = fread(info_path)
    if (!is.null(info$harvest_date)) info$harvest_date = NULL
    info$harvest_date = harvest_date
    fwrite(info, info_path)
  }

  if (file.exists(treatment_info_path)) {
    treatment_data = fread(treatment_info_path)
    treatment_data = treatment_data[!treatment_data$Led %in% toupper(letters), ]
    treatment_data = treatment_data[treatment_data[[date_name]] != "", ]
    treatment_data = treatment_data[!is.na(treatment_data[[date_name]]), ]
    treatment_data = treatment_data[St. == min(St.) | is.na(St.) | St. == ""]
    if (nrow(treatment_data) > 0) {
      sowing_date = as.Date(unique(treatment_data[[date_name]]), format = "%d-%m-%Y")
      if (length(sowing_date) > 1) {
        sowing_date = NA
        #message("Multiple specific treatment sowing dates: i = ", i, ", id = ", id)
      }
      info = fread(info_path)
      if (!is.null(info$sowing_date)) info$sowing_date = NULL
      info$sowing_date = sowing_date
      fwrite(info, info_path)
    }
  }

  sowing_category = ifelse(country == "Sweden", "Utsäde och sådd", "Udsæd og såning")
  trial_search_name = ifelse(country == "Sweden", "forsøg", "försök")
  sowing_type_names = if (country == "Sweden") {
    c("Sådatum, huvudgröda", "Sådd Datum,Utsädesmängd", "Lægge-dato")
  } else {
    c("Så-dato, hovedafgrøde", "Såning", "Lægge-dato")
  }

  if (any(data$Kategori == sowing_category)) {
    sowing_data = data[data$Kategori == sowing_category, ]
    sowing_date = as.Date(sowing_data[[date_name]], format = "%d-%m-%Y")
    if (length(sowing_date) == 0) next
    if (length(sowing_date) > 1) {
      scope = sowing_data[[scope_name]]
      sowing_data = sowing_data[grepl(trial_search_name, scope), ]
      scope = sowing_data[[scope_name]]
      if (any(scope == only_trial_name)) sowing_data = sowing_data[scope == only_trial_name, ]
      sowing_date = as.Date(sowing_data[[date_name]], format = "%d-%m-%Y")
      if (length(sowing_date) == 0) sowing_date = NA
    }
    if (length(sowing_date) > 1) {
      type = sowing_data[[treatment_name]]
      for (name in sowing_type_names) {
        if (any(type == name)) sowing_data = sowing_data[type == name, ]
      }
      sowing_date = as.Date(sowing_data[[date_name]], format = "%d-%m-%Y")
    }
    sowing_date = unique(sowing_date)
    if (length(sowing_date) > 1) {
      #message("Multiple sowing dates: i = ", i, ", id = ", id)
      sowing_date = NA
    }
    info = fread(info_path)
    if (!is.null(info$sowing_date2)) info$sowing_date2 = NULL
    info$sowing_date2 = sowing_date
    fwrite(info, info_path)
  }
}
pb$terminate()

# ==============================================================================
# Search through all the extracted data and find out what the data units are
# ==============================================================================

meta = fread(meta_path)

all_units = list()
all_ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(all_ids))
for (i in seq_along(all_ids)) {
  pb$tick()
  id = sprintf("%05d", all_ids[i])
  data_path = file.path(tmp_data_dir, "data", paste0(id, ".csv"))
  raw_data = fread(data_path)
  units = unique(raw_data$info3)
  all_units[[i]] = data.table(
    unit = unique(units),
    id = all_ids[i],
    country = meta[id == all_ids[i], country],
    crop_category = meta[id == all_ids[i], crop_category]
  )
}
all_units = rbindlist(all_units)
pb$terminate()

# Find out how many different unit types we have per crop type
n_unique_colnames = all_units[, .(
  n_unique_colnames = length(unique(unit)),
  n_experiments = length(unique(id))
), by = "crop_category"]
n_unique_colnames

# Find out what the most common unit types are per crop type and country
crop_categories = unique(all_units$crop_category)
for (i in seq_along(crop_categories)) {
  crop = crop_categories[i]
  my_table = table(all_units[crop_category == crop & country == "Sweden", unit]) |>
    sort(decreasing = TRUE)
  table_names = names(my_table)
  my_table = paste(
    my_table,
    length(unique(all_units[crop_category == crop & country == "Sweden", id])),
    sep = "/"
  )
  names(my_table) = table_names
  table_length = c(60, 40, 40)[i]
  print(paste("Swedish", crop))
  print(head(my_table, table_length))
  my_table = table(all_units[crop_category == crop & country != "Sweden", unit]) |>
    sort(decreasing = TRUE)
  table_names = names(my_table)
  my_table = paste(
    my_table,
    length(unique(all_units[crop_category == crop & country != "Sweden", id])),
    sep = "/"
  )
  names(my_table) = table_names
  table_length = c(40, 40, 40)[i]
  print(crop)
  print(head(my_table, table_length))
}

# ==============================================================================
# Extract all the downloaded data into two big data.tables.
# One containing meta information and one containing all the data
# ==============================================================================

meta = fread(meta_path)

# Extract and format all of the data of interest from all the available csv files
units_of_interest = list(
  "spring barley" = data.table::rowwiseDT(
    danish=, swedish=, english=,
    "LEJESÆDkarakter 0-10", "Liggsäd bedömning 0-10", "Lodging (0-10 scale)",
    "MELDUG% dækning", "Mjöldagg% täckning", "Mildew (% coverage)",
    "MELDUG% planter m", "Mjöldagg% plantor med", "Mildew (% plants m)",
    "BYGBLADPLET% dækning", "Kornets bladfläcksjuka% täckning", "BYGBLADPLET% dækning",
    "PLANTEBESTANDkarakter 0-10", "Plantbestånd bedömning 0-10", "Plant coverage (0-10 scale)",
    "RÅPROTEIN% i tørstof", "Råprotein% av TS", "Protein (% in dm)",
    "UDBYTTEforholdstal", "Skörd Relativtal", "Relative yield",
    "BYGRUST% dækning", "Kornrost% täckning", "Rust (% coverage)",
    "BYGRUST% planter m", "Kornrost% plantor med", "Rust (% plants m)",
    "SKOLDPLET% dækning", "Sköldfläcksjuka% täckning", "Scald (% coverage)",
    "SORTERING% kerner > 2,5 mm", "Sortering% kärnor > 2,5 mm *)", "Seed grading %>2.5mm",
    "HL-VÆGTkg", "HL-viktkg", "Specific weight",
    "STIVELSE% af tørstof", "Stärkelse% av TS", "Starch (% of dm)",
    "STRÅNEDKNÆKNING%", "Stråbrytning%", "Straw breaking (%)",
    "STRÅLÆNGDEcm", "Strålängdcm", "Straw length (cm)",
    "HALMSTYRKE%", "Stråstyrka%", "Straw strength",
    "UDB. OG MERUDB.hkg kerne", "Skörd & merskörddt/ha kärna", "Surplus yield for fungicide treatment",
    "1000 KORNSVÆGTg", "TK-viktg", "TGW (Thousand grain weight)",
    "VAND% i kerne/frø", "Vatten% i kärna/frö", "Water content % in seed",
    "UKRUDT% dækning af jord", "Ogräs% marktäckning", "Weed coverage (%)",
    "UDBYTTEhkg tørstof", "Skörddt/ha TS", "Yield (hkg drymatter (dm))",
    "UDBYTTEhkg råprotein", "Skörddt/ha råprotein", "Yield (hkg protein)",
    "UDBYTTEkg N i kerne", "Skördkg/ha N i kärna", "Yield (kg N in seed)"
  ),
  "potato" = data.table::rowwiseDT(
    danish=, swedish=, english=,
    "UDB. OG MERUDB.hkg knolde", "Skörd & merskörddt/ha knölar", "yield_and_yield_increase",
    "UDBYTTEforholdstal", "Skörd Relativtal", "yield_relative_number",
    "TØRSTOF% af råvare", "Torrsubstans% av råvara", "dry_matter_in_percentage",
    "UDB. OG MERUDB.hkg tørstof", "Skörd & merskörddt/ha TS", "dry_matter_yield_and_yield_increase",
    "UDBYTTEhkg tørstof", "Skörddt/ha TS", "dry_matter_yield",
    "STIVELSE% af råvare", "", "starch_in_percentage",
    "UDB. OG MERUDB.hkg stivelse", "Skörd & merskörddt/ha stärkelse", "starch_yield_and_yield_increase",
    "UDBYTTEhkg stivelse", "Skörddt/ha stärkelse", "starch_yield",
    "PLANTEBESTAND1000 planter/ha", "Plantbestånd1000 plantor/ha", "plant_population_1000_per_ha",
    "KNOLDSTØRRELSE% knoldvægt mindre end 40 mm", "Knölstorlek (dublett)% knölvikt < 40 mm", "size_under40mm_in_percentage",
    "KNOLDSTØRRELSE% knoldvægt på 40-60 mm", "", "size_40mm_60mm_in_percentage",
    "", "Knölstorlek% knölvikt 40-50 mm", "size_40mm_50mm_in_percentage",
    "", "Knölstorlek% knölvikt 50-60 mm", "size_50mm_60mm_in_percentage",
    "KNOLDSTØRRELSE% knoldvægt > 60 mm", "Knölstorlek% knölvikt > 60 mm", "size_over60mm_in_percentage"
  ),
  "red clover" = data.table::rowwiseDT(
    danish=, swedish=, english=,
    "UDBYTTEhkg grønt", "Skörddt/ha grönmassa", "Yield (hkg freshmatter (fm))",
    "UDBYTTEhkg tørstof", "Skörddt/ha TS", "Yield (hkg drymatter (dm))",
    "UDBYTTEhkg råprotein", "Skörddt/ha råprotein", "Yield (hkg protein)",
    "UDBYTTE, IALThkg grønt", "Skörd, totaltdt/ha grönmassa", "Total yield (hkg freshmatter (fm))",
    "UDBYTTE, IALThkg tørstof", "Skörd, totaltdt TS/ha", "Total yield (hkg drymatter (dm))",
    "UDBYTTE, IALThkg råprotein", "Skörd, totaltdt/ha råprotein", "Total yield (hkg protein)"
  )
)

all_data = all_info = all_common_treatments = list()
all_ids = meta[bad_experiment == FALSE, id]
pb = get_progress_bar(length(all_ids))
for (i in seq_along(all_ids)) {
  pb$tick()

  # Load the data of interest
  id = sprintf("%05d", all_ids[i])
  country = meta[id == all_ids[i], country]
  data_path = file.path(tmp_data_dir, "data", paste0(id, ".csv"))
  info_path = file.path(tmp_data_dir, "info", paste0(id, ".csv"))
  raw_data = fread(data_path)
  info = fread(info_path)

  common_treatment_path = file.path(tmp_data_dir, "common_treatment", paste0(id, ".csv"))
  if (file.exists(common_treatment_path)) {
    all_common_treatments[[i]] = fread(common_treatment_path)[, let(id = id)]
  }

  crop_category = meta[id == all_ids[i], crop_category]
  language = ifelse(country == "Sweden", "swedish", "danish")
  all_units = units_of_interest[[crop_category]][[language]]

  # info3 is the (badly chosen) column name for the unif of the data.
  # Check if the data file contains any data points with any units of interest.
  if (!any(raw_data$info3 %in% all_units)) next

  all_info[[i]] = data.table(
    id = id,
    experiment_title = info$experiment_title[1],
    crop_type = info$crop_type[1],
    crop_category = crop_category,
    experiment_design = info$design[1],
    address = info$location_host[1],
    #placement_group = info$placement_group[1],
    #location = info$location[1],
    country = info$plan_email_country[1],
    trial_number = info$trial_number[1],
    lfe = info$lfe[1],
    jb_soil_type = info$jb_soil_type[1],
    longitude = info$longitude[1],
    latitude = info$latitude[1],
    sowing_date = info$sowing_date[1],
    sowing_date2 = info$sowing_date2[1],
    harvest_date = info$harvest_date[1],
    surely_contains_variety_data = info$surely_contains_variety_data[1]
  )

  data = raw_data[info3 %in% all_units]
  data = unique(data) # Sometimes there are duplicate rows...

  # Translate from danish/swedish into english
  data[, let(info3 = as.character(factor(
    info3,
    levels = units_of_interest[[crop_category]][[language]],
    labels = units_of_interest[[crop_category]]$english
  )))]

  # Sometimes, there are duplicate values. I don't know if this is caused by typos
  # or by something else, but we have to deal with it in some way
  data = tryCatch(
    dcast(data, ... ~ info3, value.var = "value"),
    warning = function(e) {
      message("\ndcast warning for i = ", i, ", id = ", id, ":\n", e$message)
      out = dcast(
        data,
        ... ~ info3,
        value.var = "value",
        fun.aggregate = function(x) mean(x, na.rm = TRUE)
      )
      for (j in seq_len(ncol(out))) out[[j]][is.nan(out[[j]])] = NA
      out
    }
  )

  data[, let(st = as.integer(sub(".*st\\. *(\\d*)$", "\\1", tolower(info2))))]

  setnames(data, c("info1", "info2"), c("info", "date"))

  date_regex = ifelse(country == "Sweden", "\\d{4}-\\d{2}-\\d{2}", "\\d{2}-\\d{2}-\\d{4}")
  good_dates = grepl(date_regex, data$date)
  data[!good_dates, let(date = NA)]
  if (country == "Sweden") {
    data[good_dates, let(
      date = as.Date(sub(".*(\\d{4})-(\\d{2})-(\\d{2}).*", "\\1-\\2-\\3", date))
    )]
  } else {
    data[good_dates, let(
      date = as.Date(sub(".*(\\d{2})-(\\d{2})-(\\d{4}).*", "\\3-\\2-\\1", date))
    )]
  }
  all_years = data.table::year(data[!is.na(date), date])
  most_common_year = names(sort(table(all_years), decreasing = TRUE))[1]
  all_info[[i]]$year = as.integer(most_common_year)

  info_id_columns = grep("_id", names(info), value = TRUE)
  info_id_columns = info_id_columns[info_id_columns != "experiment_id"]
  if (length(info_id_columns) > 3 || length(info_id_columns) < 1) {
    message("Wrong number of treatments for id ", id)
    next
  }
  treatments = sub("_id", "", info_id_columns)
  data_id_columns = grep("index_column\\d", names(data), value = TRUE)
  if (length(data_id_columns) > length(treatments)) {
    message("Wrong number of treatments for id ", id)
    next
  }
  info_id_keys = lapply(treatments, function(x) sort(unique(info[[paste0(x, "_id")]])))
  data_id_keys = lapply(data_id_columns, function(x) sort(unique(data[[x]])))
  data_to_info_index = sapply(
    X = data_id_keys,
    FUN = function(x) which(sapply(info_id_keys, function(y) all(y %in% x) || all(x %in% y)))
  )
  if (is.list(data_to_info_index)) {
    message("treatments are not matching between data and info for id ", id)
    next
  }
  # Remove possibly unused treatments
  treatments = treatments[sort(data_to_info_index)]
  info_id_keys = info_id_keys[sort(data_to_info_index)]
  # For each element of info_id_keys, find out what the corresponding
  # actual name for that treatment factor is
  info_id_vals = list()
  for (j in seq_along(info_id_keys)) {
    info_id_vals[[j]] = rep(NA, length(info_id_keys[[j]]))
    for (k in seq_along(info_id_keys[[j]])) {
      info_id_vals[[j]][k] = unique(
        info[[paste0(treatments[j], "_value")]][info[[paste0(treatments[j], "_id")]] == info_id_keys[[j]][k]]
      )
    }
  }
  for (j in seq_along(data_to_info_index)) {
    my_index = data_to_info_index[j]
    data[[data_id_columns[j]]] = sapply(
      X = data[[data_id_columns[j]]],
      FUN = function(x) {
        if (any(info_id_keys[[my_index]] == x)) {
          info_id_vals[[my_index]][info_id_keys[[my_index]] == x]
        } else {
          NA
        }
      }
    )
    names(data)[which(names(data) == data_id_columns[j])] =
      paste0("treatment", data_to_info_index[j])
  }

  all_info[[i]][, let(treatment1 = treatments[1])]
  if (length(treatments) > 1) all_info[[i]][, let(treatment2 = treatments[2])]
  if (length(treatments) > 2) all_info[[i]][, let(treatment3 = treatments[3])]
  if (length(treatments) > 3) stop("too many treatments???")

  data[, let(id = id)]
  all_data[[i]] = data
}
pb$terminate()
all_data = rbindlist(all_data, fill = TRUE)
all_info = rbindlist(all_info, fill = TRUE)
all_common_treatments = rbindlist(all_common_treatments, fill = TRUE)
all_info = all_info[id %in% unique(all_data$id)]
all_common_treatments = all_common_treatments[id %in% unique(all_data$id)]

all_info[, .(
  N = .N,
  surely_contains_variety_data = sum(surely_contains_variety_data)
), by = "crop_type"]

all_info[, .(
  N = .N,
  surely_contains_variety_data = sum(surely_contains_variety_data)
), by = c("crop_category", "country")]

all_info[, .(
  N = .N,
  surely_contains_variety_data = sum(surely_contains_variety_data)
), by = c("country")]


table(all_info$surely_contains_variety_data)

all_data[, length(unique(date)), by = "id"][, summary(V1)]

# Extract all variety data from the experiments that surely contain variety data.
# Use this data to learn how we should clean the variety data in general, and to learn
# which experiments in the maybe category that probably contain variety data
format_treatment_data = function(x) {
  # # Remove (4/2n) information
  # x = gsub(" *\\([42]n\\)", "", x)
  # x = gsub(",*[42]n", "", x)
  # # Remove information about 6-row/2-row
  # x = gsub(" +[62]-rad$", "", x)
  # x = gsub("[62]-rad +", "", x)
  # x = sub(" +[62]r$", "", x)
  # x = sub("[62]r +", "", x)
  # Remove mass information from the variety data
  x = gsub("\\d*\\.?\\d+ +[kK][gG],*", "", x)
  # Remove all dates from the variety data
  x = gsub("\\d{2}-\\d{2}-\\d{4},* *", "", x)
  x = gsub("\\d{4}-\\d{2}-\\d{2},* *", "", x)
  # Remove the St. XX stuff from the variety data
  x = sub("[sS][tT]\\. *\\d{2},* *", "", x)
  x = sub(", *[sS][tT]\\.", "", x)
  # Remove the spiredygt stuff
  x = sub(",* *\\d+ Spiredygt. *[[:alpha:]]+/m2 *,*", "", x)
  # Remove weird - signs
  x = gsub(",* * -+", "", x)
  # Remove leading numbers, followed by a .
  x = sub("^\\d+\\.", "", x)
  x = gsub("\\d+\\.([a-zA-Z])", "\\1", x)
  # Remove the crop type from the variety type
  x = gsub(",* *[[:alpha:]]*byg *", "", x)
  x = gsub(",* *[rR]ødkløver *", "", x)
  # Remove information about the seed producer
  x = sub(",* *Scandinavian Seed AB *", "", x)
  x = sub(",* *Lantmännen Lantbruk *", "", x)
  x = sub(",* *Nordic Seed *", "", x)
  x = sub(",* *Syngenta *", "", x)
  x = sub(",* *Region *", "", x)
  x = sub(",* *KWS Scandinavia A/S *", "", x)
  x = sub(",* *Ympact *", "", x)
  x = sub(",* *Secobra Recherches SAS *", "", x)
  x = sub(",* *Limagrain A/S *", "", x)
  x = sub(",* *Saaten-Union GmbH *", "", x)
  x = sub(",* *Boreal *", "", x)
  x = sub(",* *Graminor AS *", "", x)
  x = sub(",* *Blålusern *", "", x)
  # Remove the Hybrid string. I don't know if this is a bad idea or not
  x = sub(",* *Hybrid *", "", x)
  # Change Sortblandning, DLG Barly Mix and NOS Sortsblandning to Blanding
  x = gsub("NOS Sortsblanding", "Blanding", x)
  x = gsub("[sS]ortblandning", "Blanding", x)
  x = gsub("DLG Barley Mix", "Blanding", x)
  # Remove leading numbers after the blanding string
  x = gsub("[bB]landing *\\d+", "Blanding", x)
  # Remove leading eko tags after the blanding string
  x = gsub("[bB]landing [Ee]ko", "Blanding", x)
  # Remove all but the first occurrence of [bB]landing
  x = gsub("(.)[bB]landing *,* *", "\\1", x)
  x = sub("^[bB]landing,", "Blanding:", x)
  # Remove information about crop type from some of the swedish experiments
  x = sub(", Vårkorn", "", x)
  x = sub(", tidigt", "", x)
  # Remove double variety information from some of the swedish experiments
  x = sub("^([^&]+) & \\1", "\\1", x)
  # Remove oat or wheat varietys
  x = sub(".*,* *Vårvete.*", "", x)
  x = sub(".*,* *Vårhvede.*", "", x)
  x = sub(".*,* *Vinterrug.*", "", x)
  x = sub(".*,* *Vinterhvede.*", "", x)
  x = sub(".*,* *Havre.*", "", x)
  # Change LP variety numbers from using commas to using dots
  x = sub("LP * (\\d+),(\\d+),(\\d+)", "LP \\1.\\2.\\3", x)
  # Change Blanding into Mixture
  x = sub("[bB]landing", "Mixture", x)
  # Remove the Ved anlæg string and the Ved string
  x = sub(",* *[vV]ed anlæg", "", x)
  x = sub(",* *[vV]ed", "", x)
  # Remove the (, mk) string
  x = sub(" *\\(, mk\\)", "", x)
  # Remove the Forår string
  x = sub(",* *[fF]orår", "", x)
  # Remove the Dyrkningssted string
  x = sub(",* *[fF]orår", "", x)
  # Remove the logaritmesprøjtning string
  x = gsub(",* *[lL]ogaritmesprøjtning", "", x)
  # Remove the variety X string
  x = gsub(",* *Sort [ATP]", "", x)
  # Remove other unwanted strings
  x = gsub(",* *P-bejdset", "", x)
  x = gsub(",* *Nu-Trax P", "", x)
  x = gsub(",* *Power Pack", "", x)
  # Remove unnamed varieties
  x = sub("Sort \\d+", "", x)
  # Remove double commas
  x = gsub(",,+", ",", x)
  # Remove leading stuff
  x = sub("^ +", "", x)
  x = sub("^, *", "", x)
  # Remove trailing numbers, unless they are connected to a specific variety type
  x = gsub("^([a-zA-Z]+) (\\d+)$", "\\1&&&\\2", x)
  x = sub(" +\\d+$", "", x)
  x = sub("^([a-zA-Z]+)&&&(\\d+)$", "\\1 \\2", x)
  # Remove commas in front of & signs
  x = gsub(" *, *&", " &", x)
  # Remove trailing stuff
  x = sub(" +$", "", x)
  x = sub(" *,+$", "", x)
  x = sub(" *&$", "", x)
  x = sub(" *\\([a-zA-Z]?$", "", x)
  # Remove double spaces
  x = gsub("  +", " ", x)
  # Remove variety names with percentages in them
  x = gsub(".*\\d+ *%.*", "", x)
  # Remove variety names with liters in them
  x = gsub(".*\\d+ *l .*", "", x)
  # Turn empty strings into NAs
  x[nchar(x) == 0] = NA_character_
  x
}

variety_data = list()
for (i in seq_len(nrow(all_info))) {
  if (!all_info$surely_contains_variety_data[i]) next
  index = which(all_info[i, .(treatment1, treatment2, treatment3)] %in% c("Sorter", "Sort"))
  if (length(index) != 1) {
    message("Wrong number of treatment indices for i = ", i, ", id = ", all_info$id[i])
    next
  }
  my_id = all_info$id[i]
  colname = paste0("treatment", index)
  variety_data[[length(variety_data) + 1]] = data.table(
    id = my_id,
    treatment_nr = index,
    variety = unique(all_data[id == my_id][[colname]])
  )
}
variety_data = rbindlist(variety_data)

variety_data[, let(variety = format_treatment_data(variety))]
all_varieties = unique(variety_data$variety)

# ---
# Get all possible variety data from the maybe_varieties
while (TRUE) {
  maybe_varieties = parallel::mclapply(
    X = all_info[!id %in% unique(variety_data$id), id],
    mc.cores = n_cores,
    FUN = function(my_id) {
      for (j in 1:3) {
        maybe_are_varieties = FALSE
        treatments = unique(all_data[id == my_id][[paste0("treatment", j)]])
        treatments = format_treatment_data(treatments)
        if (any(grepl("^Mixture: ", treatments))) maybe_are_varieties = TRUE
        n_treats = length(treatments)
        n_varieties = sum(
          sapply(
            all_varieties,
            function(x) sum(grepl(x, treatments, fixed = TRUE))
          ),
          na.rm = TRUE
        )
        if (n_varieties > 2) maybe_are_varieties = TRUE
        if (!maybe_are_varieties) next
        output = data.table(id = my_id, variety = treatments, treatment_nr = j)
        return(output)
      }
      NULL
    }
  )
  maybe_varieties = rbindlist(maybe_varieties)

  print(nrow(maybe_varieties))
  if (nrow(maybe_varieties) == 0) break

  variety_data = rbind(variety_data, maybe_varieties)
  all_varieties = unique(variety_data$variety)
}
# ---

# Remove all data where the variety contains a comma
variety_data[, let(variety = sub(".*,.*", "", variety))]

local({
  tt = table(c(variety_data$variety, maybe_varieties$variety))
  order = order(tt, decreasing = TRUE)
  names = names(tt)
  n_ids = length(unique(c(variety_data$id, maybe_varieties$id)))
  tt = paste(tt, n_ids, sep = "/")
  names(tt) = names
  message("\n")
  print(tt[head(order, 140)], max = 5000)
  message("\n")
})

all_info = all_info[id %in% variety_data$id]
all_data = all_data[id %in% variety_data$id]

all_info[, .(N = .N), by = c("crop_category", "country")]
all_info[, .(N = .N), by = c("country")]



# There are so few experiments with 3 treatments that we just remove them
mean(is.na(all_data$treatment3))
triple_treat_ids = unique(all_data[!is.na(treatment3), id])
triple_treat_ids
all_info[id %in% triple_treat_ids, .(
  id, experiment_title, year, treatment1, treatment2, treatment3
)]
all_info = all_info[!id %in% triple_treat_ids]
all_data = all_data[!id %in% triple_treat_ids]
all_data$treatment3 = NULL
all_info$treatment3 = NULL
variety_data = variety_data[!id %in% triple_treat_ids]
# ---

# Remove the coordinates from a few experiments with clearly wrong coordinates
all_info[
 id %in% c(31332:31339, 2375, 32981, 3319),
 let(longitude = NA, latitude = NA)
]

# Format all_data and all_info to make it easier to know
# what the variety types is for each experiment
all_varieties = lapply(
  X = unique(all_data$id),
  FUN = function(my_id) {
    treatment_nr = unique(variety_data[id == my_id, treatment_nr])
    all_data[id == my_id][[paste0("treatment", treatment_nr)]]
  }
)
all_varieties = unlist(all_varieties)
all_varieties = format_treatment_data(all_varieties)
all_data$variety = all_varieties

all_treatments = lapply(
  X = unique(all_data$id),
  FUN = function(my_id) {
    treatment_nr = unique(variety_data[id == my_id, treatment_nr])
    treatment_nr = ifelse(treatment_nr == 1, 2, 1)
    all_data[id == my_id][[paste0("treatment", treatment_nr)]]
  }
)
all_treatments = unlist(all_treatments)
all_data$treatment = all_treatments
all_data$treatment1 = all_data$treatment2 = NULL

all_treatment_descriptions = lapply(
  X = unique(all_data$id),
  FUN = function(my_id) {
    treatment_nr = unique(variety_data[id == my_id, treatment_nr])
    treatment_nr = ifelse(treatment_nr == 1, 2, 1)
    all_info[id == my_id][[paste0("treatment", treatment_nr)]]
  }
)
all_treatment_descriptions = unlist(all_treatment_descriptions)
all_treatment_descriptions = sub("unknown_.*", "unknown", all_treatment_descriptions)
all_info$treatment = all_treatment_descriptions
all_info$treatment1 = all_info$treatment2 = NULL
# ---

# Add information about 2-row/6-row and ploidy level into the data
grep("[^[:alnum:]]?[24]n[^[:alnum:]]?", unique(all_varieties), value = TRUE)
grep("[^[:alnum:]]?[26]r[^[:alnum:]]?", unique(all_varieties), value = TRUE)

all_data$ploidy_level = all_data$n_row = NA_integer_
all_data[grepl("[^[:alnum:]]?2n[^[:alnum:]]?", variety), let(ploidy_level = 2)]
all_data[grepl("[^[:alnum:]]?4n[^[:alnum:]]?", variety), let(ploidy_level = 4)]
all_data[grepl("[^[:alnum:]]?2r[^[:alnum:]]?", variety), let(n_row = 2)]
all_data[grepl("[^[:alnum:]]?6r[^[:alnum:]]?", variety), let(n_row = 6)]

# Then, remove information about 2-row/6-row and ploidy level from the variety names
all_data[, let(variety = sub("[^[:alnum:]]?[26]r[^[:alnum:]]?", "", variety))]
all_data[, let(variety = sub("[^[:alnum:]]?[24]n[^[:alnum:]]?", "", variety))]

# Remove a bad variety that has weird ploidy levels
all_data = all_data[variety != "Rödklöver eur"]

# Ensure that all rows of all_data with the same variety type also have the same
# values for n_row and ploidy_level
all_varieties = unique(all_data$variety)
all_varieties = all_varieties[!is.na(all_varieties)]
for (s in all_varieties) {
  unique_n_row = all_data[variety == s, unique(n_row)]
  if (length(unique_n_row) != 1) {
    unique_n_row = unique_n_row[!is.na(unique_n_row)]
    if (length(unique_n_row) != 1) stop("Something is wrong with the n_row for variety ", s)
    all_data[variety == s, let(n_row = unique_n_row)]
  }
  unique_ploidy = all_data[variety == s, unique(ploidy_level)]
  if (length(unique_ploidy) != 1) {
    unique_ploidy = unique_ploidy[!is.na(unique_ploidy)]
    if (length(unique_ploidy) != 1) stop("Something is wrong with the ploidy level for variety ", s)
    all_data[variety == s, let(ploidy_level = unique_ploidy)]
  }
}

# ---
# Remove some bad data

# This must be an error!
all_info[year == 1960, let(year = NA)]
# We believe st should be a number between 1 and 99. We therefore believe
# the number above or below this to be typos or NAs
all_data[st > 99 | st < 1, let(st = NA)]

# Remove experiments where the variety data is obviously not a variety type
bad_ids = c(
  all_data[grepl("\\d g ", tolower(variety)), unique(id)],
  all_data[grepl("ingen", tolower(variety)), unique(id)],
  all_data[grepl("x-pert|zenit|dominik|flämings", tolower(variety)), unique(id)],
  all_data[grepl("rajgræs|rajsvingel", tolower(variety)), unique(id)],
  all_data[grepl("tiptor|pluton|cerone|amistar|dominik", tolower(variety)), unique(id)]
)
bad_ids = unique(bad_ids)

all_info = all_info[!id %in% bad_ids]
all_data = all_data[!id %in% bad_ids]

# ---
# Remove all parenthesis from the variety names
parenthesis_varieties = grep("\\(", unique(all_data$variety), value = TRUE)
# Start be removing some of the seemingly meaningless three-letter stuff, which
# sometimes are not inside a parenthesis also
all_data[, let(variety = sub(" *\\(?[sS][sS][dD]\\)?", "", variety))]
all_data[, let(variety = sub(" *\\(?[lL][mM][lL]\\)?", "", variety))]
all_data[, let(variety = sub(" *\\([lL][mM]\\)?", "", variety))]
all_data[, let(variety = sub(" *\\(?[nN][sS][dD]\\)?", "", variety))]
all_data[, let(variety = sub(" *\\([sS][wW]\\)", "", variety))]
grep("\\(?lm\\)?", tolower(unique(all_data$variety)), value = TRUE)

# Now, just remove all remaining parenthesis. This might be a bad idea,
# but I choose to do it at this moment, and then I can change my mind later
all_data[, let(variety = sub(" *\\(.*\\)", "", variety))]

# ---

# Clean up the common treatment data
all_common_treatments = all_common_treatments[id %in% unique(all_info$id)]
data.table::setnames(all_common_treatments, "St.", "st")
all_common_treatments[st == "", let(st = NA)]

# Add information about the number of unique varieties per experiment
n_varieties = all_data[, .(n_varieties = length(unique(variety))), by = "id"]
all_info = merge(all_info, n_varieties, by = "id", all.x = TRUE)

# Remove experiments with only one or two varieties
bad_ids = all_info[n_varieties < 3, id]
all_info = all_info[!id %in% bad_ids]
all_data = all_data[!id %in% bad_ids]
all_common_treatments = all_common_treatments[!id %in% bad_ids]


all_info[, .(N = .N), by = c("crop_category", "country")]
all_info[, .(N = .N), by = c("country")]

# ---
# Save all_info into three separate files, one for each crop category
crop_categories = unique(meta$crop_category)
for (i in seq_along(crop_categories)) {
  my_info = all_info[crop_category == crop_categories[i]]
  my_data = all_data[id %in% my_info$id]
  my_common_treatments = all_common_treatments[id %in% my_info$id]
  column_names = c("id", "date", "variety", "treatment", "info", "st")
  column_names = c(column_names, units_of_interest[[crop_categories[i]]]$english)
  if (crop_categories[i] == "spring barley") column_names = c(column_names, "n_row")
  if (crop_categories[i] == "red clover") column_names = c(column_names, "ploidy_level")
  column_names = column_names[column_names %in% names(my_data)]
  my_data = my_data[, ..column_names]
  info_path = file.path(final_dir, paste0(sub(" ", "-", crop_categories[i]), "_info.csv"))
  fwrite(my_info, info_path)
  data_path = file.path(final_dir, paste0(sub(" ", "-", crop_categories[i]), "_data.csv"))
  fwrite(my_data, data_path)
  common_treatment_path = file.path(
    final_dir,
    paste0(sub(" ", "-", crop_categories[i]), "_common-treatments.csv")
  )
  fwrite(my_common_treatments, common_treatment_path)
}

# ==============================================================================
# Extract ids of interest from the three downloaded excel files
# ==============================================================================

if (!file.exists(excel_meta_path)) {
  excel_dir = file.path(data_dir, "excel-files")
  excel_filepaths = list.files(excel_dir, full.names = TRUE)

  excel_meta = list()
  for (i in seq_along(excel_filepaths)) {
    crop_category = basename(excel_filepaths[i])
    crop_category = sub("\\.xlsx", "", crop_category)
    crop_category = sub("-", " ", crop_category)
    excel_meta[[i]] = suppressWarnings(readxl::read_excel(excel_filepaths[i]))
    excel_meta[[i]] = excel_meta[[i]][, c(1, 2, 3, 4, 6, 8)]
    names(excel_meta[[i]]) = c("plan_nr", "run_nr", "title", "lfe", "year", "url")
    excel_meta[[i]] = as.data.table(excel_meta[[i]])
    excel_meta[[i]][, let(crop_category = crop_category)]
  }
  excel_meta = rbindlist(excel_meta)
  excel_meta[, let(
    id = as.integer(sub(".*KardexID=(\\d+)&.*", "\\1", url)),
    guid = sub(".*GUID=(.+)$", "\\1", url),
    url = NULL
  )]

  fwrite(excel_meta, excel_meta_path)
}

# ==============================================================================
# Add info from the excel files to the saved info data
# ==============================================================================

excel_meta = fread(excel_meta_path)

crop_categories = unique(sapply(strsplit(list.files(final_dir), "_"), `[[`, 1))
crop_categories = sub("-", " ", crop_categories)

for (i in seq_along(crop_categories)) {
  info_path = file.path(final_dir, paste0(sub(" ", "-", crop_categories[i]), "_info.csv"))
  info = fread(info_path)
  info = merge(
    info,
    excel_meta[crop_category == crop_categories[i], .(id, plan_nr, run_nr, my_year = year)],
    by = "id",
    all.x = TRUE
  )
  info[year != my_year, let(year = my_year)]
  info[, let(my_year = NULL)]
  fwrite(info, info_path)
}
