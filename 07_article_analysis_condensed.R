library(ggplot2)
library(data.table)
library(ggsci)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(pdftools)

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

datadir = "/nr/project/stat/NordForsk_FoodBalSec/Data/DataverseFolder/"
figdir = "/nr/project/stat/NordForsk_FoodBalSec/results/figures/"

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

# Restrict to the 50 most-tested varieties per crop (by number of unique trials)
top_varieties = tmp[, .(n_trials = uniqueN(trial)), by = .(tag, variety)] |>
  _[order(tag, -n_trials)] |>
  _[, head(.SD, 50), by = tag]

tmp_top = tmp[top_varieties[, .(tag, variety)], on = c("tag", "variety"), nomatch = 0]

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

make_cooccurrence_plot = function(cooc, title) {
  ggplot(cooc, aes(x = var2, y = var1, fill = n_trials)) +
    geom_tile(na.rm = TRUE) +
    scale_fill_viridis_c(
      "Shared\ntrials",
      option = "C", begin = 0.1, end = 0.9, na.value = NA,
      trans = "log",
      breaks = 2^seq(0, 20, by = 2)
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold")
    )
}

crops = c("Spring barley", "Potato", "Red clover")

# ---- Plot 1: global top-50 per crop (50 × 50, one page per crop) ----
plots_global = lapply(crops, \(crop) {
  var_order = top_varieties[tag == crop][order(-n_trials), variety]
  cooc = compute_cooccurrence(tmp_top[tag == crop], var_order)
  make_cooccurrence_plot(cooc, crop)
})

pdf(paste0(figdir, "connectivity_global.pdf"), width = 10, height = 9)
invisible(lapply(plots_global, print))
dev.off()

# ---- Plot 2: top-50 per country per crop (one page per crop × country) ----
# Varieties and co-occurrences are computed within each country's trials only.
plots = list()
for (crop in crops) {
  #var_order = top_varieties[tag == crop][order(-n_trials), variety]
  countries = tmp[tag == crop, sort(unique(country))]
  for (ctry in countries) {
    dt = tmp[tag == crop & country == ctry]
    if (nrow(dt) == 0) next
    var_order = dt[, .N, by = "variety"][order(-N)][1:25, variety]
    var_order = var_order[!is.na(var_order)]
    cooc = compute_cooccurrence(dt, var_order)
    plots[[length(plots) + 1]] = make_cooccurrence_plot(cooc, paste0(crop, " \u2013 ", ctry))
  }
}

pdf(paste0(figdir, "connectivity_by_country.pdf"), width = 10, height = 9)
invisible(lapply(plots, print))
dev.off()

# ==============================================================================
# Connectivity gain from combining countries
# For the global top-50 varieties per crop, compute two metrics for each
# country individually and for all countries combined:
#   (1) % of variety pairs sharing ≥1 trial  (breadth of connectivity)
#   (2) mean shared trials among connected pairs  (depth of connectivity)
# ==============================================================================
conn_gain = lapply(crops, \(crop) {
  var_order = top_varieties[tag == crop][order(-n_trials), variety]
  groups = c(tmp[tag == crop, sort(unique(country))], "All countries")

  lapply(groups, \(grp) {
    dt = if (grp == "All countries") tmp[tag == crop] else
           tmp[tag == crop & country == grp]
    cooc = compute_cooccurrence(dt, var_order)

    # Lower triangle only (unique pairs, diagonal already NA)
    pairs = cooc[as.integer(var1) > as.integer(var2)]
    data.table(
      crop = crop,
      group = grp,
      pct_connected = pairs[, mean(n_trials > 0, na.rm = TRUE) * 100],
      mean_shared = pairs[n_trials > 0, mean(n_trials)]
    )
  }) |> rbindlist()
}) |> rbindlist()

# Factor: individual countries alphabetically, "All countries" last
country_levels = c(sort(unique(conn_gain[group != "All countries", group])), "All countries")
conn_gain[, group := factor(group, levels = country_levels)]
conn_gain[, combined := group == "All countries"]
conn_gain[, crop := factor(crop, levels = c("Spring barley", "Potato", "Red clover"))]

# Plot 1: % of pairs connected
p_pct = ggplot(conn_gain, aes(x = group, y = pct_connected, fill = combined)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", pct_connected)),
            vjust = -0.4, size = 2.8) +
  scale_fill_manual(values = c("FALSE" = "#5B90C0", "TRUE" = "#E6842A"),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 108), expand = c(0, 0)) +
  facet_wrap(~crop) +
  labs(x = NULL, y = "Variety pairs sharing \u22651 trial (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    text = element_text(size = 12)
  )

# Plot 2: mean shared trials among connected pairs
p_depth = ggplot(conn_gain, aes(x = group, y = mean_shared, fill = combined)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", mean_shared)),
            vjust = -0.4, size = 2.8) +
  scale_fill_manual(values = c("FALSE" = "#5B90C0", "TRUE" = "#E6842A"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~crop) +
  labs(x = NULL, y = "Mean shared trials (connected pairs)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    text = element_text(size = 12)
  )

pdf(paste0(figdir, "connectivity_gain.pdf"), width = 10, height = 5)
print(p_pct)
print(p_depth)
dev.off()
