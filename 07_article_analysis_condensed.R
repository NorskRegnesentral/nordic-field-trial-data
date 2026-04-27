library(ggplot2)
library(data.table)
library(ggsci)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(pdftools)
library(patchwork)

rm(list = ls())

mytheme = function(){
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.key=element_blank(),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 13)
        )
}


datadir = "./"
figdir = "./"

data_b = data.table::fread(paste0(datadir, "/01_barley/01_BarleyData.csv"))
data_c = data.table::fread(paste0(datadir, "/02_redclover/02_RedCloverData.csv"))
data_p = data.table::fread(paste0(datadir, "/03_potato/03_PotatoData.csv"))

f = function(data) {
        r1 = t(data[, .(n = length(unique(trial))), by = .(country)][, 2])
        r2 = t(data[, .(n = paste0(min(year), "-", max(year))), by = .(country)][, 2])
        data[, `:=`(lon = round(longitude, 1), lat = round(latitude, 1))]
        data[, site := .GRP, by = .(lon, lat)]
        # data[, site := .GRP, by = .(region)]
        r3 = t(data[, .(n = length(unique(site))), by = .(country)][, 2])
        data[, env := .GRP, by = .(lon, lat, year)]
        r3b = t(data[, .(n = length(unique(env))), by = .(country)][, 2])
        r4 = t(data[, .(n = length(unique(variety))), by = .(country)][, 2])

        tmp = data[, .(n_geno = length(unique(variety))), by = .(env, country)]
        r5 = t(tmp[, .(n = mean(n_geno)), by = .(country)][, 2])
        r50 = mean(tmp$n_geno)
        r6 = t(tmp[, .(n = min(n_geno)), by = .(country)][, 2])
        r7 = t(tmp[, .(n = max(n_geno)), by = .(country)][, 2])

        cat(paste0(paste(rep("-", 20), collapse = ""), "\n",
                "Trials : ", paste(cbind(sum(r1), r1), collapse = " & "), "\n",
                "Year : ", paste(cbind("", r2), collapse = " & "), "\n",
                "Sites : ", paste(cbind(sum(r3), r3), collapse = " & "), "\n",
                "Environments : ", paste(cbind(sum(r3b), r3b), collapse = " & "), "\n",
                "Genotypes : ", paste(cbind(sum(r4), r4), collapse = " & "), "\n",
                "Mean : ", paste(round(cbind(r50, r5), 1), collapse = " & "), "\n",
                "Min : ", paste(cbind(min(r6), r6), collapse = " & "), "\n",
                "Max : ", paste(cbind(max(r7), r7), collapse = " & "), "\n",
                paste(rep("-", 20), collapse = "")
        ))
}

# key number per crop
f(data_b)
f(data_c)
f(data_p)

# yield per crop for 2014-2020
yrs = 2014:2024
tmp_b = data_b[year %in% yrs, .SD, .SDcols = c("year", "country", "dry_matter_yield")]
tmp_b[, cultivar := "spring barley"]
tmp_b[, yield := dry_matter_yield]
tmp_c = data_c[year %in% yrs & cut == 1, .SD, .SDcols = c("year", "country", "dry_matter_yield")]
tmp_c[, cultivar := "red clover"]
tmp_c[, yield := dry_matter_yield]
tmp_p = data_p[year %in% yrs, .SD, .SDcols = c("year", "country", "yield")]
tmp_p[, cultivar := "potato"]
tmp = rbind(tmp_b, tmp_c, tmp_p, fill = TRUE)
tmp[, cultivar := factor(
  cultivar,
  levels = c("spring barley", "red clover", "potato"),
  labels = c("Spring barley", "Red clover", "Potato")
)]

plot = ggplot(tmp, aes(x = year, y = yield, group = interaction(year, country))) +
  mytheme() +
  theme(legend.position = "top") +
  geom_boxplot(aes(fill = country), size = 0.25, outlier.size = .7) +
  facet_wrap(~cultivar, ncol = 1, scales = "free") +
  labs(x = "Year", y = "Yield (hkg/ha)", fill = NULL) +
  scale_x_continuous(breaks = yrs) +
  scale_fill_viridis_d(option = "C", begin = .25, end = 1)

pdf(paste0(figdir, "yield-year-country-overview.pdf"), height = 6, width = 10)
print(plot)
dev.off()

# Convert the pdf to a png to reduce file size
pdftools::pdf_convert(
  pdf = paste0(figdir, "/yield-year-country-overview.pdf"),
  filenames = paste0(figdir, "/yield-year-country-overview.png"),
  format = "png",
  dpi = 150
)


# map of trial sites
data = rbind(
  copy(data_b)[, cultivar := "Spring barley"],
  copy(data_c)[, cultivar := "Red clover"],
  copy(data_p)[, cultivar := "Potato"],
  fill = TRUE
) |>
  _[, .(trial, cultivar, country, longitude, latitude)] |>
  unique()
data[, cultivar := factor(cultivar, levels = c("Spring barley", "Red clover", "Potato"))]

map_data = rnaturalearthdata::map_units50
nordic_countries = c("Norway", "Finland", "Denmark", "Sweden", "Iceland")
map_data_nordics = map_data |>
  dplyr::filter(name %in% nordic_countries)

## The map (maps + ggplot2 )
plot = ggplot() +
  ## First layer: worldwide map
  geom_sf(
    data = map_data,
    color = "gray",
    fill = "gray",
    alpha = .3,
    linewidth = .1
  ) +
  geom_hex(
    data = data,
    aes(x = longitude, y = latitude),
    bins = 26
  ) +
  facet_wrap(~cultivar, ncol = 2) +
  ## Second layer: Country map
  geom_sf(
    data = map_data_nordics,
    color = "black",
    fill = NA,
    linewidth = .1
  ) +
  #geom_tile(data = ll1, aes(x = r_lon, y = r_lat, fill = log(n))) +
  coord_sf(
    xlim = c(-23, 31),
    ylim = c(54, 71)
  ) +
  scale_fill_viridis_c(
    breaks = c(1, 5, 15, 50, 150),
    trans = "log",
    begin = .15,
    end = .9
  ) +
  mytheme() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.75, .22),
    panel.background = element_rect(fill = "white")
  ) +
  labs(x = NULL, y = NULL, fill = "# trials")

pdf(
  paste0(figdir, "/trial_sites-country-trials.pdf"),
  height = 6,
  width = 8
)
print(plot)
dev.off()

# Convert the pdf to a png to reduce file size
pdftools::pdf_convert(
  pdf = paste0(figdir, "/trial_sites-country-trials.pdf"),
  filenames = paste0(figdir, "/trial_sites-country-trials.png"),
  format = "png",
  dpi = 150
)

# ==============================================================================
# Histogram of the number of trials and years per variety
# ==============================================================================

tmp = rbind(
  data_b[, .(trial, year, country, variety)][, let(tag = "Spring barley")],
  data_p[, .(trial, year, country, variety)][, let(tag = "Potato")],
  data_c[, .(trial, year, country, variety)][, let(tag = "Red clover")]
)

tmp[, .N, by = c("variety", "tag")][order(N)]

plot1 = tmp[, .N, by = c("variety", "tag")] |>
  _[, let(N = cut(N, breaks = c(1, 5, 10, 25, 50, 100, 200, 400, Inf), include.lowest = TRUE))] |>
  ggplot() +
  geom_bar(aes(x = N)) +
  scale_x_discrete(
    labels = c(
      "1-5", "6-10", "11-25", "26-50", "51-100", "101-200", "201-400", ">400"
    )
  ) +
  labs(x = "Number of field trials", y = "Number of varieties") +
  facet_wrap(~tag, scales = "free_y", ncol = 1) +
  theme_light() +
  theme(
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    text = element_text(size = 15)
  )

plot2 = tmp[, .(N = length(unique(year))), by = c("variety", "tag")] |>
  _[, let(N = cut(N, breaks = c(1, 1.1, 2, 3, 5, 7, 10, 15, 20, Inf), include.lowest = TRUE))] |>
  ggplot() +
  geom_bar(aes(x = N)) +
  scale_x_discrete(
    labels = c(
      "1", "2", "3", "4-5", "6-7", "8-10", "11-15", "16-20", ">20"
    )
  ) +
  labs(x = "Number of years", y = "Number of varieties") +
  facet_wrap(~tag, scales = "free_y", ncol = 1) +
  theme_light() +
  theme(
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    text = element_text(size = 15)
  )

pdf(
  paste0(figdir, "n_trials.pdf"),
  width = 8,
  height = 8
)
print(plot1)
print(plot2)
dev.off()

# ==============================================================================
# Connectivity plots
# ==============================================================================

tmp = rbind(
  data_b[, .(trial, year, country, variety)][, let(tag = "Spring barley")],
  data_p[, .(trial, year, country, variety)][, let(tag = "Potato")],
  data_c[, .(trial, year, country, variety)][, let(tag = "Red clover")]
)
tmp[, let(n = .N), by = c("variety", "tag")]

country_order = c("Finland", "Sweden", "Norway", "Denmark", "Iceland")
tmp[, let(country = factor(country, levels = country_order))]

all_varieties = tmp[, .(variety, tag)] |> unique()
multicountry_varieties = tmp |>
  _[, .(is_in_multiple = length(unique(country)) > 1), by = c("variety", "tag")] |>
  _[is_in_multiple == TRUE] |>
  _[, let(is_in_multiple = NULL)]

one_country_varieties = tmp |>
  _[, .(is_in_multiple = length(unique(country)) > 1), by = c("variety", "tag")] |>
  _[is_in_multiple == FALSE] |>
  _[, let(is_in_multiple = NULL)]
one_country_varieties = merge(
  one_country_varieties,
  unique(tmp[, .(tag, variety, country)]),
  by = c("tag", "variety")
)
one_country_varieties[, let(country = factor(country, levels = country_order))]

tmp_multi = tmp |>
  _[multicountry_varieties[, .(tag, variety)], on = c("tag", "variety"), nomatch = 0] |>
  _[order(-n)]

# Helper: number of shared trials between all pairs of varieties.
# C[i,j] = number of trials in which both variety i and variety j appear.
# Diagonal is set to NA (self-pairing is uninformative).
compute_cooccurrence = function(dt, var_order) {
  dt_u = unique(dt[variety %in% var_order, .(variety, trial)])
  M = dcast(dt_u, variety ~ trial, fun.aggregate = length, fill = 0L)
  vars = M[[1]]
  M = as.matrix(M[, -1, with = FALSE])
  rownames(M) = vars
  missing_vars = var_order[!var_order %in% rownames(M)]
  if (length(missing_vars) > 0) {
    tmp = matrix(0, nrow = length(missing_vars), ncol = ncol(M))
    colnames(tmp) = colnames(M)
    rownames(tmp) = missing_vars
    M = rbind(M, tmp)
  }
  M = M[var_order, , drop = FALSE]
  C = tcrossprod(M)
  n = nrow(C)
  vn = rownames(C)
  out = data.table(
    var1 = factor(rep(vn, times = n), levels = rev(vn)),
    var2 = factor(rep(vn, each = n), levels = vn),
    n_trials = as.vector(C)
  )
  out[n_trials == 0, let(n_trials = NA)]
  out
}

crops = c("Spring barley", "Potato", "Red clover")

n_trial_threshold = 30
plots = list()
for (crop in crops) {
  var_order = tmp_multi[tag == crop, unique(variety)]
  n_multi = length(var_order)
  var_order = unique(c(
    var_order,
    tmp[tag == crop & n > n_trial_threshold][order(country), unique(variety)]
  ))
  cooc = compute_cooccurrence(tmp[tag == crop], var_order)
  #
  get_one_country = function(vars, crop) {
    unique_vars = unique(vars)
    out = rep(NA, length(vars))
    for (var in unique_vars) {
      tmp = one_country_varieties[tag == crop][variety == var]
      if (nrow(tmp) == 1) {
        out[vars == var] = paste(tmp$country, "only")
      } else {
        out[vars == var] = "Multiple countries"
      }
    }
    out = factor(
      out,
      levels = c("Multiple countries", paste(country_order, "only"))
    )
    out
  }
  cooc$country1 = get_one_country(cooc$var1, crop = crop)
  cooc$country2 = get_one_country(cooc$var2, crop = crop)
  cooc$country_tiles = cooc$country1
  cooc[country1 != country2, let(country_tiles = NA)]
  cooc = cooc[order(var2)]
  last_cooc_varieties = sapply(
    X = unique(cooc[!is.na(country_tiles)]$country_tiles),
    FUN = function(x) {
      index = max(which(cooc$country_tiles == x))
      cooc[index, var2]
    }
  )
  line_data = data.frame(value = c(0, as.integer(last_cooc_varieties)))
  label_data = data.table(
    name = c("Multiple countries", paste(country_order, "only"))
  )
  label_data = label_data[name %in% cooc$country1]
  label_data$axis_index = (head(line_data$value, -1) + tail(line_data$value, -1)) / 2 + .5
  label_data$axis_val = unique(cooc$var2)[label_data$axis_index]
  plots[[crop]] = ggplot() +
    geom_tile(
      data = cooc,
      mapping = aes(x = var2, y = var1, fill = n_trials),
      na.rm = TRUE
    ) +
    scale_fill_viridis_c(
      "Shared\ntrials",
      option = "C", begin = 0.1, end = 0.9, na.value = NA,
      trans = "log",
      breaks = 2^seq(0, 20, by = 2),
      limits = c(1, 480)
    ) +
    geom_vline(data = line_data, aes(xintercept = value + .5), linewidth = .1) +
    geom_hline(data = line_data, aes(yintercept = max(value) - value + .5), linewidth = .1) +
    labs(x = NULL, y = NULL, title = crop) +
    scale_x_discrete(
      breaks = label_data$axis_val,
      labels = label_data$name
    ) +
    scale_y_discrete(
      breaks = label_data$axis_val,
      labels = label_data$name
    ) +
    coord_equal() +
    theme_minimal(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
      panel.grid = element_blank(),
      text = element_text(size = 10),
      plot.title = element_text(size = 10, face = "bold")
    )
}
plot = patchwork::wrap_plots(plots, nrow = 2, guides = "collect")

pdf(paste0(figdir, "connectivity_global.pdf"), width = 9, height = 8)
print(plot)
dev.off()
