##============================================================
## Temporal and Host Source Distribution Analysis
## Time periods vs. host sources for Salmonella Agona isolates
## Publication-grade visualization with professional styling
##============================================================
# install.packages(c("readxl", "dplyr", "ggplot2", "patchwork", "forcats"))

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(forcats)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Load Data
##============================================================
df <- read_excel("data/time_host_metadata.xlsx") %>%
  rename(ID = 1, Host = 2, Year = 3) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Year), !is.na(Host))

cat("========== DATA SUMMARY ==========\n")
cat("Total isolates loaded:", nrow(df), "\n")
cat("Year range:", min(df$Year), "–", max(df$Year), "\n")
cat("Unique host sources:", n_distinct(df$Host), "\n")
cat("Host types:", paste(unique(df$Host), collapse = ", "), "\n\n")

##============================================================
## 2. Define Time Periods (5-year intervals)
##============================================================
df <- df %>%
  mutate(
    YearGroup = case_when(
      Year < 2000                 ~ "1952-1999",
      Year >= 2000 & Year <= 2005 ~ "2000-2005",
      Year >= 2006 & Year <= 2010 ~ "2006-2010",
      Year >= 2011 & Year <= 2015 ~ "2011-2015",
      Year >= 2016 & Year <= 2020 ~ "2016-2020",
      Year >= 2021 & Year <= 2025 ~ "2021-2025",
      TRUE                        ~ "2026+"
    ),
    YearGroup = factor(YearGroup, levels = c(
      "1952-1999", "2000-2005", "2006-2010", 
      "2011-2015", "2016-2020", "2021-2025", "2026+"
    ))
  ) %>%
  # Remove empty time groups
  filter(!is.na(YearGroup))

cat("Data after period assignment:", nrow(df), "rows\n")
cat("Time periods represented:", paste(unique(df$YearGroup), collapse = ", "), "\n\n")

##============================================================
## 3. Host Source Ordering (unknown forced to last)
##============================================================
host_order <- df %>%
  filter(Host != "unknown") %>%
  count(Host, sort = TRUE) %>%
  pull(Host) %>%
  c(., "unknown")  # Append "unknown" at end

df <- df %>%
  mutate(Host = factor(Host, levels = host_order))

cat("Host ordering (by frequency):", paste(host_order, collapse = " > "), "\n\n")

##============================================================
## 4. Professional Color Palette (Nature/Lancet style)
##============================================================
# Colors chosen for print quality and color-blind accessibility
host_colors <- c(
  "Human"        = "#BC3C29", # Classic deep red
  "Poultry"      = "#E18727", # Warm orange
  "Swine"        = "#20854E", # Forest green
  "Food"         = "#0072B5", # Deep blue
  "Cattle"       = "#7876B1", # Soft purple-grey
  "Environment"  = "#6F99AD", # Blue-grey
  "other animal" = "#FFB970", # Light champagne
  "unknown"      = "#E8E8E8"  # Minimal light grey
)

# Auto-fill for additional host types if present
extra_hosts <- setdiff(levels(df$Host), names(host_colors))
if (length(extra_hosts) > 0) {
  extra_cols <- colorRampPalette(
    c("#8C510A", "#DFC27D", "#80CDC1", "#01665E")
  )(length(extra_hosts))
  names(extra_cols) <- extra_hosts
  host_colors <- c(host_colors, extra_cols)
  cat("Additional hosts detected and colored:", paste(extra_hosts, collapse = ", "), "\n\n")
}

##============================================================
## 5. Aggregate Data for Plotting
##============================================================
# Time period × host combination counts
df_time <- df %>% count(YearGroup, Host)

# Total isolates per time period (for labels)
df_top <- df_time %>%
  group_by(YearGroup) %>%
  summarise(total = sum(n), .groups = "drop")

# Host source frequency (all years combined)
df_host <- df %>%
  count(Host) %>%
  mutate(Host = fct_reorder(Host, n))

cat("Summary by time period:\n")
print(df_top)
cat("\nSummary by host source:\n")
print(df_host)
cat("\n")

##============================================================
## 6. Professional Publication Theme
##============================================================
# Classic black-and-white theme suitable for high-impact journals
theme_publication <- theme_classic(base_size = 12) +
  theme(
    # Axes
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    
    # Panels
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Text
    text = element_text(color = "black")
  )

##============================================================
## 7. Panel A: Temporal Stacked Bar Chart
##============================================================
pA <- ggplot(df_time, aes(x = YearGroup, y = n, fill = Host)) +
  # Stacked bar with thin black border (publication quality)
  geom_col(width = 0.65, color = "black", linewidth = 0.25) +
  
  # Total count labels above bars
  geom_text(
    data = df_top,
    aes(x = YearGroup, y = total, label = total, fill = NULL),
    vjust = -0.6, 
    size = 3.2, 
    color = "black", 
    fontface = "bold"
  ) +
  
  # Color and scale
  scale_fill_manual(values = host_colors, name = "Host Source") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  
  # Labels
  labs(x = "Collection Period", y = "Number of Isolates") +
  
  # Theme
  theme_publication +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    plot.margin = margin(10, 10, 5, 10)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

##============================================================
## 8. Panel B: Host Source Horizontal Bar Chart
##============================================================
pB <- ggplot(df_host, aes(x = n, y = Host, fill = Host)) +
  # Horizontal bars with black border
  geom_col(width = 0.65, color = "black", linewidth = 0.25) +
  
  # Count labels at bar end
  geom_text(
    aes(label = n), 
    hjust = -0.3, 
    size = 3.2, 
    color = "black"
  ) +
  
  # Color scale
  scale_fill_manual(values = host_colors, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  # Labels
  labs(x = "Total Isolates", y = NULL) +
  
  # Theme
  theme_publication +
  theme(
    axis.text.y = element_text(face = "italic"),  # Host names in italics (optional)
    plot.margin = margin(10, 15, 5, 5)
  )

##============================================================
## 9. Combine Panels and Create Final Visualization
##============================================================
final_plot <- pA + pB +
  plot_layout(widths = c(2.2, 1)) +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  ) &
  theme(plot.tag = element_text(size = 16, face = "bold"))

print(final_plot)

##============================================================
## 10. Save Outputs
##============================================================
# High-resolution PNG (600 DPI for print quality)
ggsave("output/temporal_host_distribution.png", 
       plot = final_plot, 
       width = 10.5, height = 5.2, 
       dpi = 600, 
       bg = "white")

# Vector PDF (preferred for publication submission)
ggsave("output/temporal_host_distribution.pdf", 
       plot = final_plot, 
       width = 10.5, height = 5.2, 
       device = cairo_pdf,
       bg = "white")

##============================================================
## 11. Export Data for Supplementary Materials
##============================================================
# Time period summary
write.csv(df_top, 
          "output/temporal_distribution_summary.csv", 
          row.names = FALSE)

# Time period × host cross-tabulation
time_host_cross <- df_time %>%
  pivot_wider(names_from = Host, values_from = n, values_fill = 0)

write.csv(time_host_cross, 
          "output/temporal_host_crosstab.csv", 
          row.names = FALSE)

# Host source summary
host_summary <- df_host %>%
  rename(count = n) %>%
  arrange(desc(count)) %>%
  mutate(percentage = round(count / sum(count) * 100, 2))

write.csv(host_summary, 
          "output/host_source_summary.csv", 
          row.names = FALSE)

# Detailed raw data
write.csv(df %>% select(ID, Host, Year, YearGroup), 
          "output/temporal_host_raw_data.csv", 
          row.names = FALSE)

##============================================================
## 12. Completion Summary
##============================================================
cat("========== ANALYSIS COMPLETE ==========\n")
cat("✓ Publication-quality visualization created!\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  Figures:\n")
cat("    - temporal_host_distribution.png (600 DPI, print-ready)\n")
cat("    - temporal_host_distribution.pdf (vector, publication-ready)\n\n")
cat("  Data exports:\n")
cat("    - temporal_distribution_summary.csv\n")
cat("    - temporal_host_crosstab.csv\n")
cat("    - host_source_summary.csv\n")
cat("    - temporal_host_raw_data.csv\n\n")