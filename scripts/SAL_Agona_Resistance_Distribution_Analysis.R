##============================================================
## Antibiotic Resistance Distribution by Country & Time Period
## Comparison of Pre-2013 vs Post-2013 Salmonella Agona Isolates
##============================================================
rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Pre-2013 Data
##============================================================
pre_2013 <- data.frame(
  Country = c("Australia", "Canada", "Chile", "China", "France", "Germany", 
              "Mexico", "UK", "USA", "other country"),
  n0 = c(1, 15, 2, 23, 62, 61, 34, 487, 105, 214),
  n1 = c(0, 2, 0, 1, 3, 0, 20, 13, 14, 6),
  n2 = c(0, 1, 0, 0, 1, 0, 0, 9, 15, 2),
  n3 = c(0, 5, 0, 0, 1, 2, 0, 19, 51, 18),
  n4 = c(0, 0, 0, 0, 3, 0, 0, 24, 20, 2),
  n5 = c(0, 0, 0, 6, 0, 0, 1, 1, 12, 17),
  n6 = c(1, 0, 0, 0, 0, 0, 0, 2, 36, 3),
  n7 = c(1, 0, 0, 0, 0, 0, 0, 0, 3, 1),
  n8 = c(0, 0, 0, 0, 1, 0, 0, 1, 1, 1),
  n9 = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  YearGroup = "Pre-2013",
  stringsAsFactors = FALSE
)

##============================================================
## 2. Post-2013 Data
##============================================================
post_2013 <- data.frame(
  Country = c("Australia", "Canada", "Chile", "China", "France", "Germany", 
              "Mexico", "UK", "USA", "other country"),
  n0 = c(133, 88, 102, 222, 356, 83, 111, 1143, 2035, 280),
  n1 = c(1, 7, 0, 27, 8, 0, 2, 27, 173, 29),
  n2 = c(0, 3, 0, 70, 6, 0, 0, 31, 175, 17),
  n3 = c(13, 4, 0, 35, 20, 2, 16, 232, 251, 47),
  n4 = c(4, 3, 6, 15, 3, 0, 8, 60, 55, 23),
  n5 = c(1, 9, 11, 60, 6, 2, 11, 89, 122, 11),
  n6 = c(15, 9, 15, 238, 20, 1, 2, 434, 88, 114),
  n7 = c(3, 2, 0, 211, 2, 0, 5, 43, 50, 31),
  n8 = c(2, 0, 0, 43, 1, 1, 0, 13, 18, 21),
  n9 = c(3, 0, 0, 27, 0, 1, 0, 0, 3, 2),
  YearGroup = "Post-2013",
  stringsAsFactors = FALSE
)

##============================================================
## 3. Data Preparation & Reshaping
##============================================================
combined <- rbind(pre_2013, post_2013) %>%
  pivot_longer(
    cols = starts_with("n"),
    names_to = "ResistanceNumber",
    values_to = "Count"
  ) %>%
  mutate(
    Count = as.numeric(Count),
    Count = replace_na(Count, 0),
    ResistanceNumber = as.integer(gsub("n", "", ResistanceNumber))
  ) %>%
  arrange(Country, YearGroup, ResistanceNumber)

combined$YearGroup <- factor(combined$YearGroup, levels = c("Pre-2013", "Post-2013"))

country_year_combos <- combined %>%
  distinct(Country, YearGroup) %>%
  arrange(Country, YearGroup)

# Generate x-axis positions (Pre-2013 and Post-2013 paired for each country)
n_countries <- length(unique(country_year_combos$Country))
x_positions <- numeric(nrow(country_year_combos))
current_pos <- 1

for (i in seq_len(n_countries)) {
  idx_pre <- (i - 1) * 2 + 1
  idx_post <- i * 2
  x_positions[idx_pre] <- current_pos
  x_positions[idx_post] <- current_pos + 1
  current_pos <- current_pos + 3
}

country_year_combos$x_pos <- x_positions

plot_data <- combined %>%
  left_join(country_year_combos, by = c("Country", "YearGroup")) %>%
  filter(!is.na(x_pos))

cat("========== DATA SUMMARY ==========\n")
cat("Total rows:", nrow(plot_data), "\n")
cat("Missing values present:", any(is.na(plot_data)), "\n")
cat("X-position range:", range(plot_data$x_pos), "\n\n")

##============================================================
## 4. Minimum Display Height (for visual clarity of small counts)
##============================================================
group_totals <- plot_data %>%
  group_by(Country, YearGroup, x_pos) %>%
  summarise(group_total = sum(Count, na.rm = TRUE), .groups = "drop")

global_max <- max(group_totals$group_total)
min_height <- global_max * 0.01

plot_data <- plot_data %>%
  left_join(group_totals, by = c("Country", "YearGroup", "x_pos")) %>%
  mutate(
    Count_display = ifelse(group_total > 0 & group_total < min_height,
                           Count * (min_height / group_total),
                           Count),
    Count_original = Count
  )

##============================================================
## 5. Significance Testing (High-level resistance: 6,7,8,9 vs. Low: 0-5)
##============================================================
high_number_ratio <- plot_data %>%
  mutate(HighNumber = ifelse(ResistanceNumber %in% c(6, 7, 8, 9), Count_original, 0)) %>%
  group_by(Country, YearGroup, x_pos) %>%
  summarise(
    High = sum(HighNumber, na.rm = TRUE),
    Total = sum(Count_original, na.rm = TRUE),
    Low = Total - High,
    Total_display = sum(Count_display, na.rm = TRUE),
    .groups = "drop"
  )

signif_data <- high_number_ratio %>%
  select(Country, YearGroup, High, Low, x_pos, Total_display) %>%
  pivot_wider(
    names_from = YearGroup,
    values_from = c(High, Low, x_pos, Total_display),
    names_sep = "_"
  )

# Function to convert p-value to significance stars
get_star <- function(p) {
  if (is.na(p)) return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

# Perform Fisher's exact test for each country
signif_labels <- signif_data %>%
  rowwise() %>%
  mutate(
    p = tryCatch({
      if (any(c(`High_Pre-2013`, `High_Post-2013`, 
                `Low_Pre-2013`, `Low_Post-2013`) < 0)) {
        NA
      } else {
        fisher.test(matrix(c(`High_Pre-2013`, `High_Post-2013`,
                             `Low_Pre-2013`, `Low_Post-2013`), nrow = 2))$p.value
      }
    }, error = function(e) NA),
    star = get_star(p),
    x_mid = mean(c(`x_pos_Pre-2013`, `x_pos_Post-2013`), na.rm = TRUE),
    pre_total = `Total_display_Pre-2013`,
    post_total = `Total_display_Post-2013`,
    y_top = max(pre_total, post_total, na.rm = TRUE) * 1.02
  ) %>%
  ungroup()

cat("========== STATISTICAL TEST RESULTS ==========\n")
print(signif_labels %>% select(Country, p, star))
cat("\n")

##============================================================
## 6. Soft Color Palette: 0=grey, 1-9=muted colors
##============================================================
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
## 7. Plotting
##============================================================
plot_data$ResistanceNumber <- factor(plot_data$ResistanceNumber, levels = 9:0)

max_y <- max(plot_data %>%
               group_by(Country, YearGroup, x_pos) %>%
               summarise(total = sum(Count_display, na.rm = TRUE), .groups = "drop") %>%
               pull(total)) * 1.1

p <- ggplot(plot_data, aes(x = x_pos, y = Count_display, fill = ResistanceNumber)) +
  # Stacked bar chart
  geom_bar(stat = "identity", position = "stack", width = 0.8,
           color = "grey35", linewidth = 0.2) +
  
  # Significance brackets: vertical lines
  geom_segment(
    data = signif_labels,
    aes(x = `x_pos_Pre-2013`, xend = `x_pos_Pre-2013`,
        y = pre_total, yend = y_top),
    inherit.aes = FALSE, color = "grey25", linewidth = 0.5
  ) +
  geom_segment(
    data = signif_labels,
    aes(x = `x_pos_Post-2013`, xend = `x_pos_Post-2013`,
        y = post_total, yend = y_top),
    inherit.aes = FALSE, color = "grey25", linewidth = 0.5
  ) +
  # Horizontal connecting line
  geom_segment(
    data = signif_labels,
    aes(x = `x_pos_Pre-2013`, xend = `x_pos_Post-2013`,
        y = y_top, yend = y_top),
    inherit.aes = FALSE, color = "grey25", linewidth = 0.5
  ) +
  # Significance stars
  geom_text(
    data = signif_labels,
    aes(x = x_mid, y = y_top * 1.05, label = star),
    inherit.aes = FALSE, size = 4, color = "grey25", fontface = "bold"
  ) +
  
  # Color scale
  scale_fill_manual(
    values = custom_colors,
    name = "Resistance\nNumber",
    labels = 9:0
  ) +
  
  # X-axis: country names
  scale_x_continuous(
    breaks = x_positions[seq(1, length(x_positions), by = 2)] + 0.5,
    labels = unique(country_year_combos$Country),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # Y-axis
  scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
  
  # Labels
  labs(
    title = "Antibiotic Resistance Profile: Pre-2013 vs Post-2013",
    subtitle = "Salmonella Agona Distribution by Country",
    y = "Number of Isolates",
    x = "Country"
  ) +
  
  # Theme
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey30"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 13, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

# Add descriptive text annotation
desc_text <- paste(
  "Each country shown with paired bars: Pre-2013 (left) vs Post-2013 (right).",
  "Significance based on Fisher's exact test comparing high-level resistance (6-9) vs low-level (0-5).",
  "* p < 0.05 | ** p < 0.01 | *** p < 0.001 | ns = not significant",
  sep = "\n"
)

p <- p + annotate(
  "text",
  x = min(plot_data$x_pos) - 1.5,
  y = max_y * 0.92,
  label = desc_text,
  hjust = 0, vjust = 1,
  size = 3, fontface = "italic", color = "grey40"
)

print(p)

##============================================================
## 8. Save Outputs
##============================================================
ggsave("output/resistance_distribution_by_country.png", p,
       width = 14, height = 8, dpi = 300, bg = "white")
ggsave("output/resistance_distribution_by_country.pdf", p,
       width = 14, height = 8, units = "in", device = cairo_pdf)

# Export statistical results
write.csv(signif_labels %>% select(Country, p, star),
          "output/resistance_significance_test_results.csv", row.names = FALSE)

# Export plot data
write.csv(plot_data %>% select(Country, YearGroup, ResistanceNumber, Count_original),
          "output/resistance_distribution_data.csv", row.names = FALSE)

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("✓ Visualization saved: resistance_distribution_by_country.png/pdf\n")
cat("✓ Statistics exported: resistance_significance_test_results.csv\n")
cat("✓ Data exported: resistance_distribution_data.csv\n\n")