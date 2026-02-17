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
  mytheme() + theme(legend.position = "top") +
  geom_boxplot(aes(fill = country), size = 0.25, outlier.size = .7) +
  facet_wrap(~cultivar, ncol = 1, scales = "free") +
  labs(x = "Year", y = "Yield (hkg/ha)", fill = NULL) +
  scale_x_continuous(breaks = yrs) +
  scale_fill_npg()

pdf(paste0(figdir, "yield-year-country-overview.pdf"), height = 6, width = 10)
print(plot)
dev.off()


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
