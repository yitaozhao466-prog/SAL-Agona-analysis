##====================================
## Invasiveness Index Analysis by Clade and Country
##====================================
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(grid)
library(rstatix)
library(ggpubr)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##====================================
## 1. Color Configuration
##====================================
color_list <- c(
  "A" = "#FFA500", "B" = "#FFFF00", "C" = "#00FFFF", "D" = "#FFC0CB",
  "E" = "#9932CC", "F" = "#2E8B57", "G" = "#1E3A8A"
)

country_colors <- c(
  "USA" = "#E41A1C", "UK" = "#377EB8", "China" = "#4DAF4A", "France" = "#FF7F00",
  "Australia" = "#984EA3", "Chile" = "#F781BF", "Canada" = "#999999", "Denmark" = "#FFD700",
  "Germany" = "#00CED1", "Japan" = "#8DD3C7", "Mexico" = "#BEBADA", "Pakistan" = "#FFFFB3",
  "other country" = "#A65628", "unknown" = "#BC80BD"
)

##====================================
## 2. Load and Prepare Data
##====================================
data <- read.csv("data/SAL_Agona_Invasiveness_Index_Analysis.xlsx", stringsAsFactors = FALSE)
data <- na.omit(data)
data$clade <- factor(data$clade, levels = names(color_list))
data$Country <- factor(data$Country, levels = names(country_colors))

# Calculate global mean for reference line
mean_val <- mean(data$Invasiveness.index, na.rm = TRUE)

##====================================
## 3. Statistical Test: Dunn's Post-Hoc Test
##====================================
# Perform Dunn's test for pairwise comparisons
dunn <- data %>%
  dunn_test(Invasiveness.index ~ clade, p.adjust.method = "holm")

# Format statistical results with custom significance labels
dunn_label <- dunn %>%
  select(group1, group2, p = p.adj) %>%
  mutate(
    p.adj.signif = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ "ns"
    ),
    # Auto-position y-coordinates for significance brackets (avoid overlaps)
    y.position = seq(0.14, 0.16, length.out = n())
  )

# Save statistical results
write.csv(dunn_label, "output/invasiveness_index_statistics.csv", row.names = FALSE)

# Display results
cat("\n========== DUNN'S TEST RESULTS ==========\n")
print(dunn_label)

##====================================
## 4. Create Boxplot with Jittered Points
##====================================
boxplot <- ggplot(data, aes(x = clade, y = Invasiveness.index, fill = clade)) +
  # Boxplot layer (outline only, no fill)
  geom_boxplot(width = 0.55, alpha = 0, outlier.shape = NA, show.legend = FALSE) +
  
  # Jittered points colored by country
  geom_jitter(
    aes(color = Country),
    position = position_jitter(0.2), 
    size = 0.8, 
    alpha = 0.8, 
    shape = 16, 
    show.legend = TRUE
  ) +
  
  # Reference line for global mean
  geom_hline(yintercept = mean_val, linetype = "dashed", color = "red", linewidth = 1) +
  annotate(
    "text", 
    x = 6.5, 
    y = mean_val - 0.003, 
    label = paste0("Mean = ", round(mean_val, 4)),
    color = "red", 
    hjust = 0, 
    vjust = 0, 
    size = 4
  ) +
  
  # Color scales
  scale_color_manual(name = "Country", values = country_colors, na.value = "grey60") +
  scale_fill_manual(name = "Clade", values = color_list) +
  
  # Axis and labels
  scale_y_continuous(limits = c(0.08, 0.16), breaks = seq(0.08, 0.16, 0.02)) +
  labs(
    title = "Invasiveness Index Distribution by Clade and Country",
    x = "Clade",
    y = "Invasiveness Index"
  ) +
  
  # Theme customization
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.position = "right"
  ) +
  guides(
    fill = "none",
    color = guide_legend(override.aes = list(shape = 16, size = 4))
  )

##====================================
## 5. Add Significance Brackets
##====================================
final_plot <- boxplot +
  stat_pvalue_manual(
    dunn_label,
    label = "p.adj.signif",        # Use custom significance symbols
    tip.length = 0.01,
    hide.ns = TRUE                  # Hide non-significant comparisons
  )

print(final_plot)

# Save the plot
ggsave("output/invasiveness_index_boxplot.pdf", final_plot, width = 12, height = 8, dpi = 300)
ggsave("output/invasiveness_index_boxplot.png", final_plot, width = 12, height = 8, dpi = 300)

##====================================
## 6. Export Summary Statistics
##====================================
# Calculate summary statistics by clade
summary_stats <- data %>%
  group_by(clade) %>%
  summarise(
    n = n(),
    mean = mean(Invasiveness.index, na.rm = TRUE),
    sd = sd(Invasiveness.index, na.rm = TRUE),
    median = median(Invasiveness.index, na.rm = TRUE),
    min = min(Invasiveness.index, na.rm = TRUE),
    max = max(Invasiveness.index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(clade)

cat("\n========== SUMMARY STATISTICS BY CLADE ==========\n")
print(summary_stats)

# Save summary statistics
write.csv(summary_stats, "output/invasiveness_index_summary.csv", row.names = FALSE)

cat("\n✓ Analysis complete! Results saved to 'output/' directory.\n")