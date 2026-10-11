##============================================================
## Clade A-G Temporal Distribution: 100% Stacked Bar Chart
## Annual proportion analysis of Salmonella Agona clades
##============================================================
# install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "scales"))

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Load Data
##============================================================
file_path <- "data/clade_yearly_statistics.xlsx"

df <- read_excel(file_path)

cat("========== DATA SUMMARY ==========\n")
cat("Raw data dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n")
cat("First few rows:\n")
print(head(df, n = 5))
cat("\n")

##============================================================
## 2. Data Preparation
##============================================================
df2 <- df %>%
  filter(clade %in% LETTERS[1:7]) %>%   # Filter clades A-G
  mutate(
    Year = as.integer(Year),
    clade = factor(clade, levels = c("A", "B", "C", "D", "E", "F", "G"))
  )

cat("Filtered data (A-G clades):", nrow(df2), "rows\n")
cat("Year range:", min(df2$Year), "–", max(df2$Year), "\n")
cat("Clades included:", paste(levels(df2$clade), collapse = ", "), "\n\n")

##============================================================
## 3. Count and Complete Missing Combinations
##============================================================
# Group by year and clade
count_df <- df2 %>%
  group_by(Year, clade) %>%
  summarise(n = n(), .groups = "drop")

# Fill missing year-clade combinations with 0
# This ensures all clades appear in all years (even if count = 0)
count_df <- count_df %>%
  complete(
    Year = sort(unique(Year)),
    clade = factor(c("A", "B", "C", "D", "E", "F", "G"), 
                   levels = c("A", "B", "C", "D", "E", "F", "G")),
    fill = list(n = 0)
  )

cat("Complete dataset after filling missing combinations:", nrow(count_df), "rows\n\n")

##============================================================
## 4. Clade Color Scheme (A-G)
##============================================================
clade_colors <- c(
  "A" = "#4E79A7",  # Blue
  "B" = "#F28E2B",  # Orange
  "C" = "#E15759",  # Red
  "D" = "#76B7B2",  # Teal
  "E" = "#59A14F",  # Green
  "F" = "#A57C5B",  # Brown
  "G" = "#B07AA1"   # Purple
)

##============================================================
## 5. Create 100% Stacked Bar Chart
##============================================================
p <- ggplot(count_df, aes(x = factor(Year), y = n, fill = clade)) +
  # 100% stacked bar: bars touch each other (width = 1, no expansion)
  geom_col(
    position = "fill",
    width = 1,           # KEY: bars touch without gaps
    color = NA,          # KEY: remove white separator borders
    linewidth = 0,
    alpha = 1
  ) +
  
  # Remove x-axis expansion
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  
  # Color scale
  scale_fill_manual(
    values = clade_colors, 
    drop = FALSE,
    name = "Clade"
  ) +
  
  # Y-axis as percentage
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # Labels
  labs(
    title = "Clade Distribution by Year",
    subtitle = "Salmonella Agona (Clades A-G)",
    x = "Year",
    y = "Proportion (%)"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15)
  )

print(p)

##============================================================
## 6. Alternative: Standard Stacked Bar Chart (for reference)
##============================================================
# If you prefer bars with small gaps and visible separators:
p_alt <- ggplot(count_df, aes(x = factor(Year), y = n, fill = clade)) +
  geom_col(
    position = "fill",
    width = 0.9,         # Small gaps between bars
    color = "white",     # White separators
    linewidth = 0.3
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(
    values = clade_colors, 
    drop = FALSE,
    name = "Clade"
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Clade Distribution by Year (Alternative)",
    subtitle = "Salmonella Agona (Clades A-G)",
    x = "Year",
    y = "Proportion (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15)
  )

##============================================================
## 7. Save Outputs
##============================================================
# Main visualization (100% stacked, no gaps)
ggsave("output/clade_temporal_distribution.png", p,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave("output/clade_temporal_distribution.pdf", p,
       width = 12, height = 7, units = "in", device = cairo_pdf)

# Alternative version (with gaps)
ggsave("output/clade_temporal_distribution_alt.png", p_alt,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave("output/clade_temporal_distribution_alt.pdf", p_alt,
       width = 12, height = 7, units = "in", device = cairo_pdf)

##============================================================
## 8. Export Data for Analysis
##============================================================
# Export raw counts
write.csv(count_df, "output/clade_yearly_counts.csv", row.names = FALSE)

# Calculate proportions for export
proportion_df <- count_df %>%
  group_by(Year) %>%
  mutate(
    total = sum(n),
    proportion = n / total,
    percentage = proportion * 100
  ) %>%
  ungroup() %>%
  select(Year, clade, count = n, total, proportion, percentage)

write.csv(proportion_df, "output/clade_yearly_proportions.csv", row.names = FALSE)

# Summary statistics
summary_stats <- proportion_df %>%
  group_by(clade) %>%
  summarise(
    mean_percentage = round(mean(percentage), 2),
    min_percentage = round(min(percentage), 2),
    max_percentage = round(max(percentage), 2),
    years_observed = sum(count > 0),
    total_isolates = sum(count),
    .groups = "drop"
  )

cat("========== CLADE SUMMARY STATISTICS ==========\n")
print(summary_stats)
cat("\n")

write.csv(summary_stats, "output/clade_summary_statistics.csv", row.names = FALSE)

##============================================================
## 9. Year-by-year Breakdown
##============================================================
yearly_summary <- proportion_df %>%
  group_by(Year) %>%
  summarise(
    total_isolates = first(total),
    clade_A = paste0(
      round(percentage[clade == "A"], 1), "%"
    ),
    clade_B = paste0(
      round(percentage[clade == "B"], 1), "%"
    ),
    clade_C = paste0(
      round(percentage[clade == "C"], 1), "%"
    ),
    clade_D = paste0(
      round(percentage[clade == "D"], 1), "%"
    ),
    clade_E = paste0(
      round(percentage[clade == "E"], 1), "%"
    ),
    clade_F = paste0(
      round(percentage[clade == "F"], 1), "%"
    ),
    clade_G = paste0(
      round(percentage[clade == "G"], 1), "%"
    ),
    .groups = "drop"
  )

cat("========== YEARLY CLADE DISTRIBUTION ==========\n")
print(yearly_summary)
cat("\n")

write.csv(yearly_summary, "output/clade_yearly_breakdown.csv", row.names = FALSE)

##============================================================
## 10. Completion Summary
##============================================================
cat("========== ANALYSIS COMPLETE ==========\n")
cat("✓ Main visualization: clade_temporal_distribution.png/pdf\n")
cat("✓ Alternative version: clade_temporal_distribution_alt.png/pdf\n")
cat("✓ Data exports:\n")
cat("   - clade_yearly_counts.csv (raw counts)\n")
cat("   - clade_yearly_proportions.csv (percentages)\n")
cat("   - clade_summary_statistics.csv (by clade)\n")
cat("   - clade_yearly_breakdown.csv (by year)\n\n")