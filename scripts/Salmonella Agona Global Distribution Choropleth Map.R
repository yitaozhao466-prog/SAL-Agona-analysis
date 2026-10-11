library(tidyverse)
library(sf)
library(rnaturalearth)

sf_use_s2(FALSE)

out_dir <- file.path(getwd(), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cut_lon <- -30
low_col <- "#e8f4f8"; high_col <- "#08519c"; grey_fill <- "#e0e0e0"
border_col <- "white"
dash_col <- "grey20"

# ---- 1. Strain Count Data ----
strain_data <- tribble(
  ~country, ~strains,
  "USA", 3300, "UK", 2689, "China", 1065, "France", 501, "Mexico", 210,
  "Australia", 178, "Denmark", 155, "Germany", 153, "Canada", 152, "Chile", 136,
  "Japan", 66, "Pakistan", 61, "South Africa", 46, "Portugal", 43, "Brazil", 28,
  "Spain", 25, "India", 23, "United Arab Emirates", 17, "Czechia", 16,
  "Sri Lanka", 16, "Jordan", 15, "Northern Ireland", 12, "Thailand", 12,
  "Burkina Faso", 11, "Tanzania", 10, "Vietnam", 9, "Kenya", 7, "Netherlands", 7,
  "Austria", 6, "Italy", 6, "Turkey", 6, "Ghana", 5, "Lebanon", 5, "Norway", 4,
  "Singapore", 4, "South Korea", 4, "Yemen", 4, "Belgium", 3, "Uruguay", 3,
  "Egypt", 2, "Indonesia", 2, "Luxembourg", 2, "Myanmar", 2, "Peru", 2,
  "Philippines", 2, "Poland", 2, "Sweden", 2, "Switzerland", 2, "Armenia", 1,
  "Bahamas", 1, "Bangladesh", 1, "Cambodia", 1, "Cayman Islands", 1,
  "Colombia", 1, "Dominican Republic", 1, "Ecuador", 1, "Ethiopia", 1,
  "Greece", 1, "Haiti", 1, "New Zealand", 1, "Paraguay", 1, "Romania", 1,
  "Russia", 1, "Slovakia", 1, "Syria", 1
) %>%
  mutate(country = if_else(country == "Northern Ireland", "UK", country)) %>%
  group_by(country) %>% summarise(strains = sum(strains), .groups = "drop") %>%
  mutate(map_country = recode(country,
                              "USA"                = "United States",
                              "UK"                 = "United Kingdom",
                              "South Korea"        = "Republic of Korea",
                              "Dominican Republic" = "Dominican Rep.",
                              "Cayman Islands"     = "Cayman Is."))

# ---- 2. World Basemap ----
world_raw <- tryCatch(
  ne_download(scale = 10, type = "admin_0_countries_chn",
              category = "cultural", returnclass = "sf"),
  error = function(e) NULL)
if (is.null(world_raw)) stop("Natural Earth download failed. Please manually download and use st_read()")

names(world_raw) <- tolower(names(world_raw))
world_others <- world_raw %>%
  select(map_country = name) %>%
  filter(map_country != "China") %>%
  group_by(map_country) %>% summarise(.groups = "drop") %>%
  st_transform(4326) %>% st_make_valid()

# ---- 3. DataV China Provincial Boundaries + South China Sea Dashed Line ----
# Note: This URL returns 35 features (34 provinces + 1 unnamed dashed line)
datav_url <- "https://geo.datav.aliyun.com/areas_v3/bound/geojson?code=100000_full"
cn_raw <- tryCatch(st_read(datav_url, quiet = TRUE),
                   error = function(e) stop("DataV download failed. Please manually save GeoJSON and read with st_read()"))
cn_raw <- st_transform(cn_raw, 4326)

# Unnamed feature = dashed line (stored as polygon), must be preserved separately
is_dash <- is.na(cn_raw$name) | cn_raw$name == ""
cn_dash <- cn_raw[is_dash, ] %>% st_make_valid()
cn_poly <- cn_raw[!is_dash, ] %>% st_make_valid()

if (nrow(cn_dash) == 0) stop("Dashed line feature not found. Please check data source.")

# Diagnostic: segment count and bounding box
dash_segs <- st_cast(st_geometry(cn_dash), "POLYGON")
dash_bb <- do.call(rbind, lapply(dash_segs, st_bbox))
cat("Dashed line segment count: ", length(dash_segs), "\n")
print(round(dash_bb[order(dash_bb[, "xmin"]), ], 2))
if (length(dash_segs) != 10) {
  warning("Dashed line segment count is ", length(dash_segs),
          ", not 10. If Taiwan segment is missing, use official vector data for supplementation.")
}

china_geom <- st_union(cn_poly)
china <- st_sf(map_country = "China", geometry = china_geom)
print(st_bbox(china))

world_others <- suppressWarnings(st_difference(world_others, china_geom)) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry))

world <- bind_rows(world_others, china)

unmatched <- setdiff(strain_data$map_country, world$map_country)
if (length(unmatched) > 0) message("Unmatched countries: ", paste(unmatched, collapse = ", "))

world_data <- world %>%
  left_join(strain_data %>% select(map_country, strains), by = "map_country") %>%
  mutate(has_data = !is.na(strains))

# ---- 4. Reproject to Pacific-Centered View ----
bx <- function(x1, x2) st_as_sfc(st_bbox(c(xmin = x1, xmax = x2, ymin = -90, ymax = 90),
                                         crs = st_crs(4326)))
part_e <- suppressWarnings(st_intersection(world_data, bx(cut_lon, 180)))
part_w <- suppressWarnings(st_intersection(world_data, bx(-180, cut_lon)))
st_geometry(part_w) <- st_geometry(part_w) + c(360, 0)
st_crs(part_w) <- st_crs(4326)

world_pac <- bind_rows(part_e, part_w) %>%
  group_by(map_country, strains, has_data) %>% summarise(.groups = "drop") %>%
  st_make_valid()

# Dashed line located near 120°E, east of cut_lon = -30, no translation needed

# ---- 5. Visualization ----
p <- ggplot() +
  geom_sf(data = filter(world_pac, !has_data), fill = grey_fill,
          color = border_col, linewidth = 0.15) +
  geom_sf(data = filter(world_pac, has_data), aes(fill = strains),
          color = border_col, linewidth = 0.15) +
  # Dashed line: fixed color, top layer, not included in color scale
  geom_sf(data = cn_dash, fill = dash_col, color = dash_col, linewidth = 0.3) +
  scale_fill_gradient(name = "Strains", low = low_col, high = high_col,
                      trans = "log10", limits = c(1, 3300),
                      breaks = c(1, 10, 100, 1000, 3300),
                      labels = scales::comma) +
  coord_sf(xlim = c(cut_lon, cut_lon + 360), ylim = c(-90, 85), expand = FALSE) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 10)))

ggsave(file.path(out_dir, "agona_global_distribution.pdf"), p, width = 300, height = 175,
       units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "agona_global_distribution.png"), p, width = 300, height = 175,
       units = "mm", dpi = 300, bg = "white")
p