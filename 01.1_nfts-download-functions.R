# This script contains extra functions used in the script for downloading
# field trial data from the NFTS web page

library(progress)
library(stringr)
library(rvest)
library(data.table)

get_progress_bar = function(n) {
  progress_bar$new(
    format = ":percent [:bar] time elapsed: :elapsedfull, eta: :eta",
    width = 70, clear = FALSE,
    total = n
  )
}

get_compressed_info = function(contents, language = c("danish", "swedish")) {
  language = match.arg(language)

  text = rvest::html_text2(contents)
  if (is.null(text) || nchar(text) < 20) return(NULL)

  # Remove all the \r characters from the text
  text = gsub("\r", "", text)

  # Identify the crop type of the experiment
  crop_type = if (language == "danish") {
    sub(".*Afgrøde: *([^\t\n]+).*", "\\1", text)
  } else {
    sub(".*Gröda: *([^\t\n]+).*", "\\1", text)
  }
  if (identical(crop_type, text)) crop_type = NA
  crop_type = sub(" *$", "", crop_type)

  # Ensure that the experiment does not contain irrelevant data
  # -------------------------------------------------------------
  # Check if any treatment titles contain the string 'Sorter'
  surely_contains_variety_data = if (language == "danish") {
    grepl("F\\d +\t+Sorte*r*\t", text)
  } else {
    grepl("F\\d +\t+Sorte*r*\t", text)
  }

  # Identify the location/host of the experiment (I am not fully sure what this thing describes...)
  location = if (language == "danish") {
    sub(".*Forsøgsvært: *([^\t\n]+).*", "\\1", text)
  } else {
    sub(".*Försöksvärd: *([^\t\n]+).*", "\\1", text)
  }
  if (identical(location, text)) location = NA
  location = sub(" *$", "", location)
  location = sub("^\r* *", "", location)
  location = sub(" *\r*$", "", location)
  location = sub(",$", "", location)
  location = sub("^, *", "", location)
  location = gsub("\"", "", location)

  # Identify the lfe (landsforsøgs something?) of the experiment
  lfe = sub(".*Lfe: *([^\t\n]+).*", "\\1", text)
  if (identical(lfe, text)) lfe = NA
  lfe = sub(" *$", "", lfe)
  lfe = sub("^\r* *", "", lfe)
  lfe = sub(" *\r*$", "", lfe)
  lfe = sub(",$", "", lfe)
  lfe = sub("\\d *- *", "", lfe)

  # Identify the title of the experiment
  experiment_title = if (language == "danish") {
    sub(".*Forsøgstitel: *([^\t\n]+).*", "\\1", text)
  } else {
    sub(".*Försök titel: *([^\t\n]+).*", "\\1", text)
  }
  if (identical(experiment_title, text)) experiment_title = NA
  experiment_title = sub(" *$", "", experiment_title)

  # Extract the NFTS trial number
  trial_number = {
    if (language == "danish") {
      sub(".*[^[:alnum:]-]([[:alnum:]-]+)[\t\n\r ]*Placeringsgruppering:.*", "\\1", text)
    } else {
      sub(".*[^[:alnum:]-]([[:alnum:]-]+)[\t\n\r ]*Nationellt jordbruksområde:.*", "\\1", text)
    }
  }
  # If this failed, try something else
  if (identical(trial_number, text)) {
    trial_number = {
      if (language == "danish") {
        sub(".*[^[:alnum:]-]([[:alnum:]-]+)[\t\n\r ]*Forsøgstitel:.*", "\\1", text)
      } else {
        sub(".*[^[:alnum:]-]([[:alnum:]-]+)[\t\n\r ]*Försök titel:.*", "\\1", text)
      }
    }
  }
  if (identical(trial_number, text)) trial_number = NA

  # Identify where the experiment takes place
  placement_group = if (language == "danish") {
    sub(".*Placeringsgruppering: *([^\t\n]+).*", "\\1", text)
  } else {
    sub(".*Nationellt *jordbruksområde: *([^\t\n]+).*", "\\1", text)
  }
  if (identical(placement_group, text)) placement_group = NA
  placement_group = sub(" *$", "", placement_group)

  # Get contact email for the person or entity responsible for planning the experiment
  plan_email = if (language == "danish") {
    sub(".*Planansvarlig[\r ]*\n[^\n]*\n([[:alnum:]\\.-]+\\@[[:alnum:]-]+\\.[[:alpha:]\\.-]+).*", "\\1", text)
  } else {
    sub(".*Försöksledare[\r ]*\n[^\n]*\n([[:alnum:]\\.-]+\\@[[:alnum:]-]+\\.[[:alpha:]\\.-]+).*", "\\1", text)
  }
  if (!identical(plan_email, text)) {
    plan_email_ending = tail(strsplit(plan_email, "\\.")[[1]], 1)
    plan_email_country = data.table::fcase(
      plan_email_ending == "dk", "Denmark",
      plan_email_ending == "se", "Sweden",
      plan_email_ending == "no", "Norway"
    )
  } else {
    plan_email_country = NA
  }

  # Locate all contact emails for the experiment
  email_pattern = "[[:alnum:]-]+\\@[[:alnum:]-]+\\.[[:alpha:]-\\.]+"
  email_addresses = stringr::str_extract_all(text, email_pattern)[[1]]
  email_address_endings = sapply(strsplit(email_addresses, split = "\\."), tail, 1)

  # Find out how many different data categories there are in the webpage.
  # Category nr. 1 is marked F1, nr. 2 is marked F2 and so on
  main_table_text = if (language == "danish") {
    sub(".*Forsøgsbehandlinger:[\t\n]+(F1.+?)[\t\n]*Grundbehandlinger:.*", "\\1", text)
  } else {
    sub(".*Försöksbehandlingar:[\t\n]+(F1.+?)[\t\n]*Grundbehandlingar:.*", "\\1", text)
  }
  n_categories = length(strsplit(main_table_text, "F\\d +")[[1]]) - 1
  if (n_categories == 0) return(NULL)

  categories = list()
  n_unknown_categories = 0
  for (i in seq_len(n_categories)) {
    # Extract the table containint all of the variety types
    category_name = sub(
      paste0(".*F", i, " *\t+([[:alpha:] ]*)[^\n]*?\n.*$"),
      "\\1",
      main_table_text
    )
    if (category_name == "") {
      n_unknown_categories = n_unknown_categories + 1
      category_name = paste0("unknown_category_", n_unknown_categories)
    }
    table = sub(
      paste0(
        ".*F", i, " *\t+[[:alpha:] ]*[^\n]*\n(.+?)[\t\n]*",
        ifelse(i == n_categories, "$", paste0("F", i + 1)),
        ".*"),
      "\\1",
      main_table_text
    )
    if (identical(table, main_table_text)) stop("Something went wrong when extracting the varieties table")
    # Add markers in the table for whenever we get to a new data type within the category.
    # This makes it easier to extract each data type into a separate element of a vector using strsplit
    if (i == 1) {
      table = gsub("\t*\n(\\d+\t)", "newvarietytype:\\1", table)
      table = sub("\t*\n(\\d+)$", "newvarietytype:\\1", table)
    } else if (i == 2) {
      table = gsub("\t*\n([A-Z](\t|$))", "newvarietytype:\\1", table)
    } else if (i == 3) {
      table = gsub("\t*\n([IVX]+(\t|$))", "newvarietytype:\\1", table)
    } else {
      stop("I don't know what to do with three different categories for one id!")
    }
    # Extract each data type into a separate element of a vector using strsplit
    res = strsplit(table, "newvarietytype:")[[1]]
    category_ids = sub("^([^\t]+)\t.*", "\\1", res)
    # Format the text: Change newline to &, remove some \t-signs and turn others into commas
    res = sub("^[^\t]+(\t*)", "\\1", res)
    res = gsub("\t*\n", " & ", res)
    res = gsub(" +\t+", " ", res)
    res = gsub("\t+(.)", ", \\1", res)
    res = gsub("\t+", "", res)
    res = sub(",$", "", res)
    res = sub("^, *", "", res)
    res = sub(",+", ",", res)
    res = sub("[^[:alnum:]\\)\\(]+$", "", res)
    #res = sub("^[^,]+, *(.*)$", "\\1", res)
    res = data.frame(category_ids, res)
    names(res) = paste0(category_name, c("_id", "_value"))
    categories[[i]] = res
  }

  if (length(categories) == 1) {
    final_result = categories[[1]]
  } else {
    final_result = merge(categories[[1]], categories[[2]], all = TRUE)
    for (i in seq_len(length(categories) - 2)) {
      final_result = merge(final_result, categories[[i + 2]], all = TRUE)
    }
  }
  final_result = as.data.table(final_result)

  # Put all of the additional extracted information into the final result
  final_result[, let(
    crop_type = crop_type,
    surely_contains_variety_data = surely_contains_variety_data,
    any_unknown_treatments = (n_unknown_categories > 0),
    experiment_title = experiment_title,
    lfe = lfe,
    trial_number = trial_number,
    location_host = location,
    placement_group = placement_group,
    email_endings = list(email_address_endings),
    plan_email_country = plan_email_country
  )]

  final_result
}

get_crop_type = function(id) {
  stop("Never use this function!")
  url = paste0(url_base_compressed, id)

  text = tryCatch(
    #rvest::html_text2(rvest::read_html(url, encoding = "UTF-8")),
    rvest::html_text(rvest::read_html(url, encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(text) || nchar(text) < 20) return(NA)

  # Remove all the \r characters from the text
  text = gsub("\r", "", text)

  # Identify the crop type of the experiment
  crop_type = sub(".*Afgrøde: *([^\t\n]+).*", "\\1", text)
  if (identical(crop_type, text)) crop_type = NA

  crop_type
}
