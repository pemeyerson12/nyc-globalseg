library(tidyverse)
library(tidycensus)
library(sf)
library(mapview)
library(tibble)
library(tigris)
library(stringr)
library(scales)
library(segregation)

occ_order <- read.csv("vars00.csv") %>% dplyr::select(category, group) %>%
  filter(category != "z") %>% unique()

x00 <- read.csv("fua00.csv") %>% dplyr::select(!X)
x11 <- read.csv("fua11.csv") %>% dplyr::select(!X)
x21 <- read.csv("fua21.csv") %>% dplyr::select(!X)

# 1. changes in structure (top/middle/bottom)
tmb00 <- x00 %>%
  count(group, wt = jobs) %>%
  mutate(pct = n / sum(n), yr = "2000")

tmb11 <- x11 %>%
  count(group, wt = jobs) %>%
  mutate(pct = n / sum(n), yr = "2011")

tmb21 <- x21 %>%
  count(group, wt = jobs) %>%
  mutate(pct = n / sum(n), yr = "2021")

tmb <- bind_rows(bind_rows(tmb00, tmb11), tmb21)

tmb_plot <- ggplot(tmb, aes(x = pct, y = yr, fill = group)) +
  geom_col(position = position_stack()) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            color = "white") +
  scale_y_discrete(limits = rev) +
  labs(x = "Share of jobs", y = NULL, fill = NULL) +
  theme_minimal()

# 1. changes in structure (by occ)
occ00 <- occ_order %>% inner_join(x00 %>%
  count(category, wt = jobs) %>%
  mutate(pct = n / sum(n), yr = "2000"))

occ11 <- occ_order %>% inner_join(x11 %>%
  count(category, wt = jobs) %>%
  mutate(pct = n / sum(n), yr = "2011"))

occ21 <- occ_order %>% inner_join(x21 %>%
  count(category, wt = jobs) %>%
  mutate(pct = n / sum(n), yr = "2021"))

occ <- bind_rows(bind_rows(occ00, occ11), occ21) %>%
  mutate(category = factor(category, levels = unique(occ_order$category)),
    group = factor(group))

group_cols <- setNames(grey.colors(3, start = 0.8, end = 0.2),
                       c("Bottom", "Middle", "Top"))

cat_cols <- occ_order %>%
  distinct(category, group) %>%
  { setNames(group_cols[as.character(.$group)], .$category) }

occ_plot <- ggplot(occ, aes(x = pct, y = yr, fill = category)) +
  geom_col(position = position_stack(reverse = TRUE),
           color = "white") +
  geom_text(aes(label = 100 * round(pct, 2)),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            color = "white", size = 3) +
  scale_fill_manual(values = cat_cols,
                    breaks = levels(occ$category)) +
  scale_y_discrete(limits = rev) +
  labs(x = "Share of jobs", y = NULL, fill = NULL) +
  theme_minimal()

# 2. dissimilarity index
dissim_pair <- function(x, g1, g2) {
  dissimilarity(
    data   = x %>% filter(group %in% c(g1, g2)),
    group  = "group",
    unit   = "GEOID",
    weight = "jobs"
  )$est
}

years <- list("2000" = x00, "2011" = x11, "2021" = x21)

pairs <- list(
  "Top vs. Bottom"    = c("Top", "Bottom"),
  "Top vs. Middle"    = c("Top", "Middle"),
  "Middle vs. Bottom" = c("Middle", "Bottom")
)

dissim <- map_dfr(names(years), function(yr) {
  map_dfr(names(pairs), function(p) {
    tibble(comparison = p,
           yr = yr,
           D = dissim_pair(years[[yr]], pairs[[p]][1], pairs[[p]][2]))
  })
}) %>%
  pivot_wider(names_from = yr, values_from = D)

# 3. location quotient
fuaNY <- c("Bronx", "Kings", "Nassau", "New York", "Queens", "Richmond",
           "Rockland", "Suffolk", "Westchester")
fuaNJ <- c("Bergen", "Essex", "Hudson", "Middlesex", "Monmouth", "Ocean",
           "Passaic", "Union")

options(tigris_use_cache = TRUE)

states_sf   <- states(cb = TRUE, year = 2020) %>% filter(STUSPS %in% c("NY", "NJ", "CT", "PA"))
counties_sf <- bind_rows(
  counties(state = "NY", cb = TRUE, year = 2020) %>% filter(NAME %in% fuaNY),
  counties(state = "NJ", cb = TRUE, year = 2020) %>% filter(NAME %in% fuaNJ)
)

water_sf <- bind_rows(
  lapply(fuaNY, \(cty) area_water("NY", cty, year = 2020)),
  lapply(fuaNJ, \(cty) area_water("NJ", cty, year = 2020))
) %>%
  filter(AWATER > 1e6)

tracts_ny <- tracts(state = "NY", county = fuaNY, year = 2010, cb = TRUE)
tracts_nj <- tracts(state = "NJ", county = fuaNJ, year = 2010, cb = TRUE)

nyc_fua <- bind_rows(tracts_ny, tracts_nj) %>%
  mutate(GEOID = as.double(str_remove(GEO_ID, "^.*US"))) %>%
  dplyr::select(GEOID)

lq_breaks <- c(0, 0.5, 0.75, 1.25, 1.5, Inf)
lq_labels <- c("0 - 0.5", "0.5 - 0.75", "0.75 - 1.25", "1.25 - 1.5", "1.5+")

make_btm <- function(x, shapes = nyc_fua) {
  lq_data <- x %>%
    group_by(GEOID) %>%
    summarize(
      bottom = sum(jobs[group == "Bottom"]),
      total  = sum(jobs)
    ) %>%
    mutate(
      lq     = (bottom / total) / (sum(bottom) / sum(total)),
      lq_bin = cut(lq, breaks = lq_breaks, labels = lq_labels,
                   include.lowest = TRUE)
    )
  
  shapes %>% left_join(lq_data, by = "GEOID")
}

btm00 <- make_btm(x00)
btm11 <- make_btm(x11)
btm21 <- make_btm(x21)

make_top <- function(x, shapes = nyc_fua) {
  lq_data <- x %>%
    group_by(GEOID) %>%
    summarize(
      top = sum(jobs[group == "Top"]),
      total  = sum(jobs)
    ) %>%
    mutate(
      lq     = (top / total) / (sum(top) / sum(total)),
      lq_bin = cut(lq, breaks = lq_breaks, labels = lq_labels,
                   include.lowest = TRUE)
    )
  
  shapes %>% left_join(lq_data, by = "GEOID")
}

top00 <- make_top(x00)
top11 <- make_top(x11)
top21 <- make_top(x21)

lq_colors <- setNames(
  c("#8c510a", "#dfc27d", "#ffffff", "#80cdc1", "#01665e"),
  lq_labels
)

plot_lq <- function(d, legend_title, cty_shp, xlim = NULL, ylim = NULL, pad = 0.01) {
  bb <- sf::st_bbox(cty_shp)
  if (is.null(xlim)) xlim <- c(bb[["xmin"]] - pad, bb[["xmax"]] + pad)
  if (is.null(ylim)) ylim <- c(bb[["ymin"]] - pad, bb[["ymax"]] + pad)
  
  ggplot() +
    geom_sf(data = d, aes(fill = lq_bin), color = NA) +
    geom_sf(data = water_sf, fill = "#cfe3f1", color = NA) +
    geom_sf(data = cty_shp, fill = NA, color = "grey40", linewidth = 0.15) +
    scale_fill_manual(values = lq_colors, na.value = "grey90",
                      drop = FALSE, name = legend_title) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = "#cfe3f1", color = NA))
}

btm00_map <- plot_lq(btm00, "Bottom LQ 2000", counties_sf)
btm11_map <- plot_lq(btm11, "Bottom LQ 2011", counties_sf)
btm21_map <- plot_lq(btm21, "Bottom LQ 2021", counties_sf)

top00_map <- plot_lq(top00, "Top LQ 2000", counties_sf)
top11_map <- plot_lq(top11, "Top LQ 2011", counties_sf)
top21_map <- plot_lq(top21, "Top LQ 2021", counties_sf)

# NYC maps
countyNYC <- c("Bronx", "Kings", "Queens", "New York", "Richmond")
countyNYC_sf <- counties_sf %>%
  filter(STATEFP == "36", NAME %in% countyNYC)

regex_nyc <- str_c(countyNYC_sf$GEOID, collapse = "|")
btm00_NYC <- btm00 %>% filter(str_detect(as.character(GEOID), regex_nyc))
btm11_NYC <- btm11 %>% filter(str_detect(as.character(GEOID), regex_nyc))
btm21_NYC <- btm21 %>% filter(str_detect(as.character(GEOID), regex_nyc))

btm00_mapNYC <- plot_lq(btm00_NYC, "NYC Bottom LQ 2000", countyNYC_sf)
btm11_mapNYC <- plot_lq(btm11_NYC, "NYC Bottom LQ 2011", countyNYC_sf)
btm21_mapNYC <- plot_lq(btm21_NYC, "NYC Bottom LQ 2021", countyNYC_sf)

top00_NYC <- top00 %>% filter(str_detect(as.character(GEOID), regex_nyc))
top11_NYC <- top11 %>% filter(str_detect(as.character(GEOID), regex_nyc))
top21_NYC <- top21 %>% filter(str_detect(as.character(GEOID), regex_nyc))

top00_mapNYC <- plot_lq(top00_NYC, "NYC Top LQ 2000", countyNYC_sf)
top11_mapNYC <- plot_lq(top11_NYC, "NYC Top LQ 2011", countyNYC_sf)
top21_mapNYC <- plot_lq(top21_NYC, "NYC Top LQ 2021", countyNYC_sf)