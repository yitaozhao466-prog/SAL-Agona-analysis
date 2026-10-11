##============================================================
## Serovar, Country, and Source Niche Distribution Analysis
## Ternary-like visualization using patchwork layout
## Publication-grade figure combining bubble plot with marginal bar charts
##============================================================
# install.packages(c("readxl", "tidyverse", "patchwork", "ggsci"))

library(readxl)
library(tidyverse)
library(patchwork)
library(ggsci)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Load Data
##============================================================
file_path <- "data/ternary_serovar_country_source.xlsx"

raw_data <- read_excel(file_path)

cat("========== DATA SUMMARY ==========\n")
cat("Raw data dimensions:", nrow(raw_data), "rows ×", ncol(raw_data), "columns\n")
cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
cat("First few rows:\n")
print(head(raw_data, n = 3))
cat("\n")

##============================================================
## 2. Data Standardization
##============================================================
# Standardize column names
colnames(raw_data) <- c("Serovar", "Source_Niche", "Country")

# Normalize missing values and "unknown" variants
clean_data <- raw_data %>%
  mutate(
    Serovar = case_when(
      is.na(Serovar) ~ "Unknown",
      tolower(Serovar) %in% c("unknown", "na", "null", "") ~ "Unknown",
      TRUE ~ as.character(Serovar)
    ),
    Country = case_when(
      is.na(Country) ~ "Unknown",
      tolower(Country) %in% c("unknown", "na", "null", "") ~ "Unknown",
      TRUE ~ as.character(Country)
    ),
    Source_Niche = case_when(
      is.na(Source_Niche) ~ "Unknown",
      tolower(Source_Niche) %in% c("unknown", "na", "null", "") ~ "Unknown",
      TRUE ~ as.character(Source_Niche)
    )
  )

cat("Data after cleaning:", nrow(clean_data), "rows\n")
cat("Unique serovars:", n_distinct(clean_data$Serovar), "\n")
cat("Unique countries:", n_distinct(clean_data$Country), "\n")
cat("Unique source niches:", n_distinct(clean_data$Source_Niche), "\n\n")

##============================================================
## 3. Select Top 20 Serovars (others → "Other")
##============================================================
top_serovars <- clean_data %>%
  filter(Serovar != "Unknown") %>%
  count(Serovar, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(Serovar)

cat("Top 20 serovars:\n")
print(data.frame(Rank = 1:length(top_serovars), Serovar = top_serovars))
cat("\n")

clean_data <- clean_data %>%
  mutate(Serovar = if_else(Serovar %in% c(top_serovars, "Unknown"), Serovar, "Other"))

# Set serovar factor levels: Top 20 (high to low) → "Other" → "Unknown"
serovar_levels <- top_serovars
if ("Other" %in% clean_data$Serovar) serovar_levels <- c(serovar_levels, "Other")
if ("Unknown" %in% clean_data$Serovar) serovar_levels <- c(serovar_levels, "Unknown")
clean_data$Serovar <- factor(clean_data$Serovar, levels = serovar_levels)

##============================================================
## 4. Select Top 20 Countries (others → "Other")
##============================================================
top_countries <- clean_data %>%
  filter(Country != "Unknown") %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(Country)

cat("Top 20 countries:\n")
print(data.frame(Rank = 1:length(top_countries), Country = top_countries))
cat("\n")

clean_data <- clean_data %>%
  mutate(Country = if_else(Country %in% c(top_countries, "Unknown"), Country, "Other"))

# Set country factor levels: Unknown (bottom) → Other → Top 20 (high frequency at top)
country_levels <- rev(top_countries)  # Reverse for visualization
if ("Other" %in% clean_data$Country) country_levels <- c("Other", country_levels)
if ("Unknown" %in% clean_data$Country) country_levels <- c("Unknown", country_levels)
clean_data$Country <- factor(clean_data$Country, levels = country_levels)

##============================================================
## 5. Professional Color Palette (Journal-grade)
##============================================================
# Inspired by Kleborate and high-impact journal standards
unique_niches <- unique(clean_data$Source_Niche)

journal_colors <- c(
  "#1c786a", # Deep teal/green
  "#594391", # Deep purple
  "#e15759", # Dusty rose/pink
  "#76b7b2", # Icy blue
  "#4e79a7", # Calm blue
  "#edc948", # Warm sand yellow
  "#b07aa1", # Lilac purple
  "#ff9da7", # Cherry blossom pink
  "#9c755f"  # Muted brown
)

# Dynamic color binding
my_palette <- setNames(
  journal_colors[1:min(length(unique_niches), length(journal_colors))],
  unique_niches[1:min(length(unique_niches), length(journal_colors))]
)

# Ensure Unknown and Other map to neutral grey
if ("Unknown" %in% names(my_palette)) my_palette["Unknown"] <- "#d3d3d3"
if ("Other" %in% names(my_palette)) my_palette["Other"] <- "#a9a9a9"

cat("Source niche color mapping:\n")
print(my_palette)
cat("\n")

##============================================================
## 6. Build Central Main Plot (Bubble/Jitter Plot)
##============================================================
p_main <- ggplot(clean_data, aes(x = Serovar, y = Country, color = Source_Niche)) +
  # Jittered points for individual genomes
  geom_jitter(width = 0.25, height = 0.25, size = 1.2, alpha = 0.8, shape = 16) +
  
  # Color scale
  scale_color_manual(values = my_palette, name = "Source Niche") +
  
  # Theme
  theme_bw(base_family = "sans") +
  theme(
    # Axes
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                               size = 9, face = "bold", color = "black"),
    axis.text.y = element_text(size = 9, face = "bold", color = "black"),
    axis.title = element_blank(),
    
    # Grid
    panel.grid.major = element_line(color = "grey93", linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    
    # Layout
    plot.margin = margin(t = 0, r = 0, b = 5, l = 5),
    legend.position = "none"  # Collected via patchwork
  )

##============================================================
## 7. Build Top Marginal Bar Chart (Serovar Counts)
##============================================================
p_top <- ggplot(clean_data, aes(x = Serovar, fill = Source_Niche)) +
  # Stacked bar chart
  geom_bar(position = "stack", width = 0.8, color = NA) +
  
  # Color scale
  scale_fill_manual(values = my_palette) +
  
  # Labels
  labs(y = "Number of Genomes") +
  
  # Theme
  theme_classic(base_family = "sans") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_text(size = 9, face = "bold", color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.margin = margin(t = 10, r = 0, b = 0, l = 5),
    legend.position = "none"
  )

##============================================================
## 8. Build Right Marginal Bar Chart (Country Counts)
##============================================================
p_right <- ggplot(clean_data, aes(y = Country, fill = Source_Niche)) +
  # Stacked bar chart
  geom_bar(position = "stack", width = 0.8, color = NA) +
  
  # Color scale
  scale_fill_manual(values = my_palette) +
  
  # Labels
  labs(x = "Number of Genomes") +
  
  # Theme
  theme_classic(base_family = "sans") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_text(size = 9, face = "bold", color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    plot.margin = margin(t = 0, r = 10, b = 5, l = 0),
    legend.position = "none"
  )

##============================================================
## 9. Combine All Panels Using patchwork
##============================================================
# Layout: 
# [Top bar chart] [Spacer]
# [Main plot]     [Right bar chart]
combined_plot <- p_top + plot_spacer() + p_main + p_right +
  plot_layout(
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),    # Main plot : Right chart ratio = 4:1
    heights = c(1, 4),   # Top chart : Main plot ratio = 1:4
    guides = "collect"   # Collect legends
  ) &
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold", color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.key = element_rect(fill = "white", color = NA)
  )

print(combined_plot)

##============================================================
## 10. Save High-Resolution Outputs
##============================================================
# Vector PDF (preferred for publication - lossless scaling)
ggsave(
  filename = "output/serovar_country_source_ternary.pdf",
  plot = combined_plot,
  width = 11,
  height = 10,
  device = cairo_pdf,
  bg = "white"
)

# High-resolution PNG (600 DPI for print quality)
ggsave(
  filename = "output/serovar_country_source_ternary.png",
  plot = combined_plot,
  width = 11,
  height = 10,
  dpi = 600,
  bg = "white"
)

##============================================================
## 11. Export Summary Statistics
##============================================================
# Serovar summary
serovar_summary <- clean_data %>%
  count(Serovar, name = "count") %>%
  arrange(desc(count)) %>%
  mutate(percentage = round(count / sum(count) * 100, 2))

cat("========== SEROVAR SUMMARY ==========\n")
print(serovar_summary)
cat("\n")

write.csv(serovar_summary, "output/serovar_summary.csv", row.names = FALSE)

# Country summary
country_summary <- clean_data %>%
  count(Country, name = "count") %>%
  arrange(desc(count)) %>%
  mutate(percentage = round(count / sum(count) * 100, 2))

cat("========== COUNTRY SUMMARY ==========\n")
print(country_summary)
cat("\n")

write.csv(country_summary, "output/country_summary.csv", row.names = FALSE)

# Source niche summary
source_summary <- clean_data %>%
  count(Source_Niche, name = "count") %>%
  arrange(desc(count)) %>%
  mutate(percentage = round(count / sum(count) * 100, 2))

cat("========== SOURCE NICHE SUMMARY ==========\n")
print(source_summary)
cat("\n")

write.csv(source_summary, "output/source_niche_summary.csv", row.names = FALSE)

# Three-way cross-tabulation
threeway_summary <- clean_data %>%
  count(Serovar, Country, Source_Niche, name = "count") %>%
  arrange(desc(count))

write.csv(threeway_summary, "output/serovar_country_source_crosstab.csv", row.names = FALSE)

# Raw data with standardized format
write.csv(clean_data, "output/cleaned_ternary_data.csv", row.names = FALSE)

##============================================================
## 12. Completion Summary
##============================================================
cat("========== ANALYSIS COMPLETE ==========\n")
cat("✓ Publication-grade visualization created!\n\n")
cat("Output files saved to 'output/' directory:\n\n")
cat("Figures:\n")
cat("  - serovar_country_source_ternary.pdf (vector, recommended for journals)\n")
cat("  - serovar_country_source_ternary.png (600 DPI, print-ready)\n\n")
cat("Summary statistics:\n")
cat("  - serovar_summary.csv (top serovars)\n")
cat("  - country_summary.csv (top countries)\n")
cat("  - source_niche_summary.csv (source distribution)\n")
cat("  - serovar_country_source_crosstab.csv (three-way table)\n")
cat("  - cleaned_ternary_data.csv (full cleaned dataset)\n\n")
cat("Figure dimensions: 11\" × 10\" (suitable for most journals)\n\n")