##============================================================
## Geographic Transmission Network Analysis
## Multi-sheet regional core hub identification and mapping
## Global dissemination patterns across transmission routes
##============================================================
# install.packages(c("readxl", "ggplot2", "dplyr", "maps", "patchwork", "ggnewscale"))

library(readxl)
library(ggplot2)
library(dplyr)
library(maps)
library(patchwork)
library(ggnewscale)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Load Data and Define Geographic Reference
##============================================================
input_file <- "data/geographic_transmission_network.xlsx"

cat("========== DATA LOADING ==========\n")

# Verify file exists
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# Load all sheet names
sheet_names <- excel_sheets(input_file)
cat("Sheets detected:", paste(sheet_names, collapse = ", "), "\n\n")

##============================================================
## 2. Geographic Coordinates Dictionary
##============================================================
# Manually curated coordinates for accurate global positioning
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
          61.5240, 35.9078, 14.0583),
  stringsAsFactors = FALSE
)

cat("Geographic coordinates defined for", nrow(geo_dict), "countries\n\n")

##============================================================
## 3. Load World Map Basemap
##============================================================
world_map <- map_data("world") %>%
  filter(region != "Antarctica")

cat("World map loaded:", n_distinct(world_map$region), "regions\n\n")

##============================================================
## 4. Visualization Function
##============================================================
# Create individual transmission network map for each region/sheet
create_transmission_map <- function(df, sheet_title) {
  
  cat("Processing sheet:", sheet_title, "...\n")
  
  # Standardize column names (first two columns: source, destination)
  if (ncol(df) >= 2) {
    colnames(df)[1:2] <- c("source_country", "destination_country")
  } else {
    warning("Sheet has fewer than 2 columns, skipping: ", sheet_title)
    return(NULL)
  }
  
  # Count transmission frequency for each source country
  source_stats <- df %>%
    group_by(source_country) %>%
    summarise(
      frequency = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(frequency))
  
  if (nrow(source_stats) == 0) {
    cat("  → No data in sheet, skipping\n")
    return(ggplot() + theme_void() + labs(title = sheet_title))
  }
  
  cat("  → Core hub (source country):", source_stats$source_country[1], 
      "with", source_stats$frequency[1], "transmission events\n")
  
  # Identify core hub (highest frequency source)
  core_hub <- source_stats$source_country[1]
  
  # Standardize country names to match geo_dict
  source_stats <- source_stats %>%
    mutate(
      region_name = case_when(
        source_country == "UK"  ~ "UK",
        source_country == "USA" ~ "USA",
        TRUE ~ source_country
      )
    )
  
  # Join map data with source frequency for coloring
  map_colored <- world_map %>%
    left_join(source_stats, by = c("region" = "region_name"))
  
  ##========== Prepare transmission edges ==========
  
  # Secondary hub edges (all sources except core hub) - thin black lines
  edges_secondary <- df %>%
    filter(source_country != core_hub) %>%
    group_by(source_country, destination_country) %>%
    summarise(transmission_count = n(), .groups = "drop") %>%
    # Join source coordinates
    left_join(geo_dict, by = c("source_country" = "country")) %>%
    rename(lon_source = lon, lat_source = lat) %>%
    # Join destination coordinates
    left_join(geo_dict, by = c("destination_country" = "country")) %>%
    rename(lon_dest = lon, lat_dest = lat) %>%
    # Filter to only those with valid coordinates
    filter(!is.na(lon_source) & !is.na(lon_dest))
  
  # Core hub edges (primary source hub) - thick red lines
  edges_core <- df %>%
    filter(source_country == core_hub) %>%
    group_by(source_country, destination_country) %>%
    summarise(transmission_count = n(), .groups = "drop") %>%
    # Join source coordinates
    left_join(geo_dict, by = c("source_country" = "country")) %>%
    rename(lon_source = lon, lat_source = lat) %>%
    # Join destination coordinates
    left_join(geo_dict, by = c("destination_country" = "country")) %>%
    rename(lon_dest = lon, lat_dest = lat) %>%
    # Filter to only those with valid coordinates
    filter(!is.na(lon_source) & !is.na(lon_dest))
  
  cat("  → Secondary hub edges:", nrow(edges_secondary), "\n")
  cat("  → Core hub edges:", nrow(edges_core), "\n")
  
  ##========== Build map visualization ==========
  
  p <- ggplot(data = map_colored) +
    
    # Basemap: countries without transmission data (light grey)
    geom_polygon(
      data = . %>% filter(is.na(frequency)),
      aes(x = long, y = lat, group = group),
      fill = "#E5E7E9",
      color = "white",
      linewidth = 0.1
    ) +
    
    # New color scale for secondary hubs
    ggnewscale::new_scale_fill() +
    
    # Secondary hubs: blue gradient (color-coded by source frequency)
    geom_polygon(
      data = . %>% filter(!is.na(frequency), region != core_hub),
      aes(x = long, y = lat, group = group, fill = frequency),
      color = "white",
      linewidth = 0.1
    ) +
    scale_fill_gradient(
      name = "Secondary Hubs\n(Frequency)",
      low = "#A8D8F0",
      high = "#1E3A8A"
    ) +
    
    # New color scale for core hub
    ggnewscale::new_scale_fill() +
    
    # Core hub: prominent red highlight
    geom_polygon(
      data = . %>% filter(region == core_hub),
      aes(x = long, y = lat, group = group, fill = region),
      color = "white",
      linewidth = 0.1
    ) +
    scale_fill_manual(
      name = "Core Hub",
      values = c("#E64B35")
    ) +
    
    # Transmission arrows from secondary hubs: thin black lines
    geom_curve(
      data = edges_secondary,
      aes(x = lon_source, y = lat_source,
          xend = lon_dest, yend = lat_dest),
      color = "black",
      linewidth = 0.3,
      curvature = 0.2,
      alpha = 0.6,
      arrow = arrow(length = unit(0.01, "npc"), type = "closed")
    ) +
    
    # Transmission arrows from core hub: thick red lines (layered on top)
    geom_curve(
      data = edges_core,
      aes(x = lon_source, y = lat_source,
          xend = lon_dest, yend = lat_dest,
          linewidth = transmission_count),
      color = "#E64B35",
      curvature = 0.2,
      alpha = 0.7,
      arrow = arrow(length = unit(0.015, "npc"), type = "closed")
    ) +
    
    scale_linewidth_continuous(range = c(0.4, 1.8), guide = "none") +
    
    # Map projection
    coord_fixed(1.3) +
    
    # Theme
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.margin = margin(5, 5, 5, 5),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "cm")
    ) +
    labs(title = sheet_title)
  
  cat("  ✓ Map generated\n\n")
  return(p)
  
}

##============================================================
## 5. Process All Sheets and Generate Maps
##============================================================
plot_list <- list()

for (sheet_name in sheet_names) {
  
  tryCatch({
    # Load data from sheet
    current_df <- read_excel(input_file, sheet = sheet_name, col_names = TRUE)
    
    # Generate map
    p <- create_transmission_map(current_df, sheet_title = sheet_name)
    
    if (!is.null(p)) {
      plot_list[[sheet_name]] <- p
    }
    
  }, error = function(e) {
    cat("Error processing sheet", sheet_name, ":", e$message, "\n")
  })
  
}

cat("Successfully generated", length(plot_list), "maps\n\n")

##============================================================
## 6. Combine All Maps into Grid Layout
##============================================================
if (length(plot_list) == 0) {
  stop("No maps generated. Check input data.")
}

# Calculate layout dimensions
n_plots <- length(plot_list)
n_cols <- 2
n_rows <- ceiling(n_plots / n_cols)

cat("Layout: ", n_rows, "rows ×", n_cols, "columns\n\n")

# Combine plots
combined_plot <- wrap_plots(plot_list, nrow = n_rows, ncol = n_cols, 
                            guides = "collect") &
  theme(legend.position = "right")

# Add overall title
final_plot <- combined_plot + plot_annotation(
  title = "Global Transmission Networks: Core Hubs and Dissemination Routes",
  subtitle = "Multi-region analysis of Salmonella Agona geographic dispersal",
  theme = theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey40")
  )
)

print(final_plot)

##============================================================
## 7. Save Visualization Outputs
##============================================================
cat("========== SAVING OUTPUTS ==========\n\n")

# Determine output dimensions based on grid size
output_width <- 14
output_height <- 5 * n_rows  # ~5 inches per row

# Save high-resolution PNG
ggsave(
  "output/geographic_transmission_network_grid.png",
  plot = final_plot,
  width = output_width,
  height = output_height,
  dpi = 300,
  bg = "white"
)

cat("✓ PNG saved: geographic_transmission_network_grid.png\n")
cat("  Dimensions:", output_width, "×", output_height, "inches, 300 DPI\n\n")

# Save vector PDF
ggsave(
  "output/geographic_transmission_network_grid.pdf",
  plot = final_plot,
  width = output_width,
  height = output_height,
  device = cairo_pdf,
  bg = "white"
)

cat("✓ PDF saved: geographic_transmission_network_grid.pdf\n")
cat("  Dimensions:", output_width, "×", output_height, "inches\n\n")

##============================================================
## 8. Export Summary Statistics
##============================================================
# Compile hub information for all sheets
hub_summary_list <- list()

for (sheet_name in sheet_names) {
  tryCatch({
    current_df <- read_excel(input_file, sheet = sheet_name, col_names = TRUE)
    colnames(current_df)[1:2] <- c("source_country", "destination_country")
    
    # Calculate statistics
    hub_stats <- current_df %>%
      group_by(source_country) %>%
      summarise(
        transmission_events = n(),
        unique_destinations = n_distinct(destination_country),
        .groups = "drop"
      ) %>%
      arrange(desc(transmission_events)) %>%
      mutate(
        region = sheet_name,
        rank = row_number(),
        .before = source_country
      )
    
    hub_summary_list[[sheet_name]] <- hub_stats
    
  }, error = function(e) {
    cat("Error summarizing sheet", sheet_name, "\n")
  })
}

# Combine all hub summaries
hub_summary <- bind_rows(hub_summary_list)

write.csv(
  hub_summary,
  "output/transmission_hub_summary.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("✓ Hub summary table: transmission_hub_summary.csv\n\n")

##============================================================
## 9. Completion Summary
##============================================================
cat("========== ANALYSIS COMPLETE ==========\n\n")
cat("✓ Visualizations:\n")
cat("  - geographic_transmission_network_grid.png (300 DPI)\n")
cat("  - geographic_transmission_network_grid.pdf (vector)\n\n")
cat("✓ Summary tables:\n")
cat("  - transmission_hub_summary.csv (core hubs by region)\n\n")
cat("Grid layout:", n_rows, "rows ×", n_cols, "columns =", n_plots, "maps\n\n")
cat("Map visualization legend:\n")
cat("  Red countries = Core transmission hub (highest frequency source)\n")
cat("  Blue gradient = Secondary hubs (frequency coded)\n")
cat("  Grey = No transmission data\n")
cat("  Red arrows = Core hub dissemination routes\n")
cat("  Black arrows = Secondary hub dissemination routes\n\n")