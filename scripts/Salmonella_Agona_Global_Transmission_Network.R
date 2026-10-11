# 1. Load required R packages
library(ggplot2)
library(dplyr)
library(maps)

# 2. Read standard data
raw_data <- "
Export_Country Import_Country
UK France
UK Denmark
India USA
UK India
India Australia
UK France
UK India
UK France
USA India
UK USA
UK France
France India
UK France
UK India
UK Australia
UK France
UK Norway
UK USA
UK USA
UK Australia
UK India
UK France
UK France
UK Germany
UK France
USA Germany
UK USA
UK Germany
UK Germany
UK Germany
UK Portugal
UK France
France Denmark
UK France
France UK
France UK
France UK
France Lebanon
France UK
UK Romania
France UK
UK France
UK Australia
UK USA
UK France
UK Canada
UK China
UK France
UK USA
UK China
UK USA
UK France
UK USA
UK France
UK France
UK France
UK Italy
UK USA
India UK
UK USA
UK Canada
UK USA
UK Canada
UK China
UK Sweden
UK USA
UK Canada
UK China
UK USA
UK China
UK France
UK USA
UK Pakistan
UK Canada
UK Pakistan
UK Canada
UK Australia
UK USA
UK Portugal
UK USA
UK USA
UK Canada
UK France
UK USA
UK Canada
UK USA
UK Pakistan
UK Australia
UK USA
UK USA
UK USA
UK USA
UK USA
UK Pakistan
UK China
UK USA
UK USA
India UK
UK India
UK Germany
USA Czechia
UK USA
UK France
UK Australia
UK Australia
UK Thailand
UK Australia
UK France
UK Canada
UK Australia
UK USA
UK USA
UK France
UK France
UK Thailand
UK France
Thailand France
UK Thailand
Thailand USA
UK Thailand
UK USA
UK France
UK France
UK USA
UK USA
UK USA
UK USA
UK USA
UK USA
UK Canada
Canada USA
UK Australia
UK USA
UK Cambodia
UK Canada
Canada USA
UK Australia
UK USA
UK France
UK Canada
UK USA
Australia Canada
Australia USA
UK Australia
UK France
UK USA
UK Germany
UK France
UK USA
UK Australia
UK USA
UK China
UK France
UK France
UK France
UK Australia
UK China
UK Australia
UK Australia
Australia China
UK Australia
UK France
Australia UK
UK Australia
UK Thailand
UK USA
China UK
China Canada
China USA
UK Australia
China UK
Australia UK
China Australia
UK Canada
UK France
UK USA
UK France
UK USA
China UK
UK Canada
France UK
France Australia
France USA
UK France
UK Germany
UK France
UK Australia
UK France
UK Australia
UK USA
UK Australia
UK Russia
UK China
China UK
China USA
China UK
China USA
China UK
UK South_Korea
UK USA
USA UK
Australia UK
Australia Vietnam
UK Australia
China South_Korea
China USA
China UK
China UK
USA China
China USA
China UK
UK France
China UK
China UK
USA Vietnam
UK Australia
UK Vietnam
UK USA
USA UK
China USA
China UK
China UK
China Australia
China USA
China Denmark
China Australia
China UK
UK China
"

df <- read.table(text = raw_data, header = TRUE, stringsAsFactors = FALSE)
edges <- df %>% count(Export_Country, Import_Country, name = "weight")

# 3. Calculate Top 10 countries (same as chord diagram)
all_countries <- c(df$Export_Country, df$Import_Country)
country_counts <- sort(table(all_countries), decreasing = TRUE)
top10_countries <- names(country_counts)[1:10]

# 4. Complete country geographic coordinates dictionary
geo_dict <- data.frame(
  country = c("UK", "France", "Denmark", "India", "USA", "Australia", "Norway", 
              "Germany", "Portugal", "Lebanon", "Romania", "Canada", "China", 
              "Italy", "Sweden", "Pakistan", "Czechia", "Thailand", "Cambodia", 
              "Russia", "South_Korea", "Vietnam"),
  lon = c(-3.4359, 2.2137, 9.5018, 78.9629, -95.7129, 133.7751, 8.4689, 
          10.4515, -8.2245, 35.8623, 24.9668, -106.3468, 104.1954, 
          12.5674, 18.6435, 69.3451, 15.4730, 100.9925, 104.9910, 
          105.3188, 127.7669, 108.2772),
  lat = c(55.3781, 46.2276, 56.2639, 20.5937, 37.0902, -25.2744, 60.4720, 
          51.1657, 39.3999, 33.8547, 45.9432, 56.1304, 35.8617, 
          41.8719, 60.1282, 30.3753, 49.8175, 15.8700, 12.5657, 
          61.5240, 35.9078, 14.0583)
)

# 5. Scientific color palette matching Top 10 countries
# (Order: UK, China, remaining Top10)
top_ordered <- c("UK", "China", setdiff(top10_countries, c("UK", "China")))
sci_palette <- c("#E64B35", "#4DBBD5", "#00A087", "#F39B7F", "#3C5488", 
                 "#8491B4", "#91D1C2", "#7E6148", "#B09C85", "#9B59B6")
color_mapping <- setNames(sci_palette, top_ordered)
color_mapping["Other"] <- "#C5C9C7" # Gray for other countries

# 6. Inject color and hierarchical logic into edges
edges_geo <- edges %>%
  left_join(geo_dict, by = c("Export_Country" = "country")) %>% 
  rename(lon_out = lon, lat_out = lat) %>%
  left_join(geo_dict, by = c("Import_Country" = "country")) %>% 
  rename(lon_in = lon, lat_in = lat) %>%
  filter(!is.na(lon_out) & !is.na(lon_in)) %>%
  # Determine if export country is in Top 10 for edge coloring
  mutate(Source_Group = ifelse(Export_Country %in% top10_countries, Export_Country, "Other")) %>%
  # CRITICAL: Set Z-axis order for plotting (Plot_Order)
  # Layer order: Other (1) → Top10 (2) → China (3) → UK (4)
  mutate(Plot_Order = case_when(
    Source_Group == "Other" ~ 1,
    Source_Group == "China" ~ 3,
    Source_Group == "UK" ~ 4,
    TRUE ~ 2
  )) %>%
  arrange(Plot_Order, weight) # Thicker lines on top

# 7. Inject color and size into nodes
nodes <- data.frame(country = c(edges$Export_Country, edges$Import_Country)) %>%
  count(country, name = "degree") %>%
  left_join(geo_dict, by = "country") %>% 
  filter(!is.na(lon)) %>%
  mutate(Node_Group = ifelse(country %in% top10_countries, country, "Other"))

# 8. Begin plotting publication-quality global transmission network
world_map <- map_data("world")

ggplot() +
  # Layer 1: Clean world basemap (light gray land, white borders)
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "#E8EAEB", color = "#FFFFFF", linewidth = 0.3) +
  
  # Layer 2: Transmission arrows (mapped to Top 10 country colors)
  geom_curve(data = edges_geo, 
             aes(x = lon_out, y = lat_out, xend = lon_in, yend = lat_in, 
                 linewidth = weight, color = Source_Group),
             curvature = 0.25, 
             alpha = 0.65, # 65% transparency to show network complexity
             arrow = arrow(length = unit(0.015, "npc"), type = "closed")) +
  
  # Layer 3: Country node points
  geom_point(data = nodes, 
             aes(x = lon, y = lat, size = degree, fill = Node_Group), 
             color = "white", shape = 21, stroke = 0.5, alpha = 0.9) +
  
  # Layer 4: Highlight UK with glowing halo to emphasize source status
  geom_point(data = nodes %>% filter(country == "UK"), 
             aes(x = lon, y = lat, size = degree + 5), 
             color = "#E64B35", fill = NA, shape = 1, stroke = 1.5) +
  
  # Color and legend configuration (unified with chord diagram)
  scale_color_manual(values = color_mapping, name = "Source Country (Top 10)") +
  scale_fill_manual(values = color_mapping, guide = "none") +
  scale_linewidth_continuous(range = c(0.2, 2.5), guide = "none") +
  scale_size_continuous(range = c(2, 9), guide = "none") + 
  
  # Theme refinement (Nature/Science style: clean, frameless)
  coord_fixed(1.3) + 
  theme_void() + 
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, 
                                  margin = margin(b = 15)),
        plot.margin = margin(10, 10, 10, 10)) +
  labs(title = "Global Spatial Dissemination Network of Salmonella Agona Clade B")