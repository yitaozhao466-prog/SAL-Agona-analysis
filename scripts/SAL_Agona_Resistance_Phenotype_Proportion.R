##============================================================
## Resistance Phenotype Proportion Analysis by Country
## Stacked percentage bar chart for Salmonella Agona isolates
##============================================================
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Load Data
##============================================================
data <- read_excel("data/resistance_phenotype_country.xlsx")

cat("========== DATA SUMMARY ==========\n")
cat("Dimensions:", nrow(data), "rows ×", ncol(data), "columns\n")
cat("Columns:", paste(names(data), collapse = ", "), "\n")
cat("First few rows:\n")
print(head(data, n = 3))
cat("\n")

##============================================================
## 2. Reshape Data: Wide to Long Format
##============================================================
data_long <- data %>%
  pivot_longer(
    cols = -number,
    names_to = "country",
    values_to = "count"
  ) %>%
  mutate(
    number_fac = factor(number, levels = 0:9),
    count = as.numeric(count),
    count = replace_na(count, 0)
  )

cat("Transformed data dimensions:", nrow(data_long), "rows\n")
cat("Countries included:", paste(unique(data_long$country), collapse = ", "), "\n\n")

##============================================================
## 3. Soft Color Palette: 0=grey, 1-9=muted colors
##============================================================
# Resistance phenotype color scheme:
# 0 = susceptible (grey)
# 1-9 = progressive resistance levels (low to high saturation)
custom_colors <- c(
  "0" = "#D0D0D0",  # Grey (susceptible)
  "1" = "#C5D9DE",  # Light blue-grey
  "2" = "#A9C4C0",  # Muted teal
  "3" = "#D5DDB8",  # Muted sage green
  "4" = "#EBD9A0",  # Muted sand
  "5" = "#E8BE8A",  # Muted apricot
  "6" = "#DB9F7E",  # Muted terracotta
  "7" = "#C97B6B",  # Muted rust red
  "8" = "#B05A5A",  # Darker mauve red
  "9" = "#8A4151"   # Wine red (highly resistant)
)

##============================================================
## 4. Stacked Percentage Bar Chart
##============================================================
p <- ggplot(data_long, aes(x = country, fill = number_fac)) +
  # Stacked percentage bar (normalize to 100% within each country)
  geom_bar(aes(weight = count), 
           position = "fill", 
           width = 0.7,
           color = "grey35", 
           linewidth = 0.2) +
  
  # Color scale
  scale_fill_manual(
    values = custom_colors, 
    name = "Resistance\nPhenotypes",
    labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ) +
  
  # Y-axis as percentage
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # X-axis scaling
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  
  # Labels
  labs(
    title = "Resistance Phenotype Distribution by Country",
    subtitle = "Salmonella Agona Isolates",
    y = "Proportion (%)",
    x = "Country"
  ) +
  
  # Theme
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "grey40"),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 11),
    legend.position = "right",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

print(p)

##============================================================
## 5. Save Outputs
##============================================================
ggsave("output/resistance_phenotype_proportion_by_country.png", p,
       width = 12, height = 8, dpi = 300, bg = "white")
ggsave("output/resistance_phenotype_proportion_by_country.pdf", p,
       width = 12, height = 8, units = "in", device = cairo_pdf)

# Export plot data
write.csv(data_long %>% arrange(country, number),
          "output/resistance_phenotype_data.csv", row.names = FALSE)

# Summary statistics
summary_stats <- data_long %>%
  group_by(country) %>%
  summarise(
    total_isolates = sum(count, na.rm = TRUE),
    phenotype_0 = sum(count[number_fac == "0"], na.rm = TRUE),
    highly_resistant = sum(count[number_fac %in% c("6", "7", "8", "9")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_susceptible = round(phenotype_0 / total_isolates * 100, 2),
    pct_highly_resistant = round(highly_resistant / total_isolates * 100, 2)
  )

cat("========== PHENOTYPE SUMMARY BY COUNTRY ==========\n")
print(summary_stats)
cat("\n")

write.csv(summary_stats, "output/resistance_phenotype_summary.csv", row.names = FALSE)

cat("========== ANALYSIS COMPLETE ==========\n")
cat("✓ Visualization saved: resistance_phenotype_proportion_by_country.png/pdf\n")
cat("✓ Data exported: resistance_phenotype_data.csv\n")
cat("✓ Summary statistics: resistance_phenotype_summary.csv\n\n")