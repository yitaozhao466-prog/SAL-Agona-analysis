##============================================================
## Clade B Antimicrobial Resistance Comparative Analysis
## Nine-drug profiles with Fisher's exact test
## Grouped bar chart and radar chart visualizations
##============================================================
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "ggsignif", 
#                    "patchwork", "writexl", "fmsb"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsignif)
library(patchwork)
library(writexl)
library(fmsb)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Configuration & Parameters
##============================================================
input_file <- "data/clade_amr_profile.xlsx"

# Define nine antimicrobial drugs for analysis
drugs <- c("AMG", "PEN", "ESBL", "PHE", "TMP", "MAC", "QUI", "SUL", "TET")

# Clade-specific color palette
clade_colors <- c(
  "B-1" = "#d9ead3",  # Light sage green
  "B-2" = "#d0e0e3",  # Light blue-grey
  "B-3" = "#f4cccc"   # Light rose
)

cat("========== CONFIGURATION ==========\n")
cat("Input file: ", input_file, "\n")
cat("Drugs analyzed: ", paste(drugs, collapse = ", "), "\n")
cat("Clades: B-1, B-2, B-3\n\n")

##============================================================
## 2. Load and Prepare Data
##============================================================
df <- read_excel(input_file)

cat("Raw data loaded: ", nrow(df), " rows × ", ncol(df), " columns\n\n")

# Transform to long format and code resistance status
df_long <- df %>%
  select(clade, all_of(drugs)) %>%
  pivot_longer(
    cols = all_of(drugs), 
    names_to = "Drug", 
    values_to = "Status"
  ) %>%
  mutate(
    # NA or "-" coded as 0 (no resistance), otherwise 1 (resistance detected)
    Resistance = ifelse(is.na(Status) | Status == "-", 0, 1)
  ) %>%
  mutate(
    clade = factor(clade, levels = c("B-1", "B-2", "B-3")),
    Drug = factor(Drug, levels = drugs)
  )

cat("Data transformed to long format: ", nrow(df_long), " rows\n")
cat("Clades represented: ", paste(unique(df_long$clade), collapse = ", "), "\n\n")

##============================================================
## 3. Fisher's Exact Test Function
##============================================================
# Calculate pairwise Fisher's exact test p-values
get_p_value <- function(d_sub, g1, g2) {
  sub_tab <- d_sub %>% filter(clade %in% c(g1, g2))
  
  # Construct 2×2 contingency table
  tab <- table(
    factor(sub_tab$clade, levels = c(g1, g2)),
    factor(sub_tab$Resistance, levels = c(0, 1))
  )
  
  p <- 1  # Default: not significant
  tryCatch({
    if (sum(rowSums(tab) > 0) > 1) {
      p <- fisher.test(tab)$p.value
    }
  }, error = function(e) {})
  
  return(p)
}

# Convert p-value to significance stars
to_star <- function(p) {
  if (is.na(p)) return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("ns")
}

##============================================================
## 4. Generate Nine Individual Plots (3×3 Grid)
##============================================================
plot_list <- list()

for (d in drugs) {
  # Extract single-drug data
  sub_df <- df_long %>% filter(Drug == d)
  
  # Calculate resistance percentages for each clade
  sum_df <- sub_df %>%
    group_by(clade) %>%
    summarise(
      Total = n(),
      Resistant = sum(Resistance, na.rm = TRUE),
      Percent = if_else(Total > 0, Resistant / Total * 100, 0),
      .groups = "drop"
    ) %>%
    # Fill missing clades with zeros
    complete(clade = c("B-1", "B-2", "B-3"), 
             fill = list(Total = 0, Resistant = 0, Percent = 0))
  
  # Calculate pairwise p-values
  p12 <- get_p_value(sub_df, "B-1", "B-2")
  p23 <- get_p_value(sub_df, "B-2", "B-3")
  p13 <- get_p_value(sub_df, "B-1", "B-3")
  
  comp_list <- list(c("B-1", "B-2"), c("B-2", "B-3"), c("B-1", "B-3"))
  anno_list <- c(to_star(p12), to_star(p23), to_star(p13))
  
  # Dynamic y-axis positioning for significance brackets
  max_val <- max(sum_df$Percent, na.rm = TRUE)
  if (max_val < 5) max_val <- 5
  
  y_pos <- c(max_val + 10, max_val + 10, max_val + 25)
  upper_lim <- max_val + 35
  
  # Filter to show only significant comparisons
  sig_idx <- which(anno_list != "ns")
  
  # Base bar plot
  p <- ggplot(sum_df, aes(x = clade, y = Percent, fill = clade)) +
    geom_col(color = "black", width = 0.6) +
    scale_fill_manual(values = clade_colors) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0)), 
      limits = c(0, max(100, upper_lim))
    ) +
    labs(
      title = d,
      x = "",
      y = "Resistance Rate (%)"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 11, color = "black"),
      axis.title = element_text(size = 12, face = "bold")
    )
  
  # Add significance brackets if comparisons are significant
  if (length(sig_idx) > 0) {
    p <- p + geom_signif(
      comparisons = comp_list[sig_idx],
      annotations = anno_list[sig_idx],
      y_position = y_pos[sig_idx],
      tip_length = 0.02,
      vjust = 0.5,
      textsize = 5,
      color = "black"
    )
  }
  
  plot_list[[d]] <- p
}

cat("Generated ", length(plot_list), " individual drug plots\n\n")

##============================================================
## 5. Combine Nine Plots into 3×3 Grid
##============================================================
final_9plot <- wrap_plots(plot_list, ncol = 3)

print(final_9plot)

##============================================================
## 6. Generate Summary Statistics Table
##============================================================
p_summary_list <- list()

for (d in drugs) {
  sub_df <- df_long %>% filter(Drug == d)
  
  sum_df <- sub_df %>%
    group_by(clade) %>%
    summarise(
      Total = n(),
      Resistant = sum(Resistance, na.rm = TRUE),
      Percent = if_else(Total > 0, Resistant / Total * 100, 0),
      .groups = "drop"
    ) %>%
    complete(clade = c("B-1", "B-2", "B-3"),
             fill = list(Total = 0, Resistant = 0, Percent = 0))
  
  # Extract statistics for each clade
  pct_B1 <- sum_df %>% filter(clade == "B-1") %>% pull(Percent)
  pct_B2 <- sum_df %>% filter(clade == "B-2") %>% pull(Percent)
  pct_B3 <- sum_df %>% filter(clade == "B-3") %>% pull(Percent)
  
  n_B1 <- sum_df %>% filter(clade == "B-1") %>% pull(Total)
  n_B2 <- sum_df %>% filter(clade == "B-2") %>% pull(Total)
  n_B3 <- sum_df %>% filter(clade == "B-3") %>% pull(Total)
  
  # Calculate pairwise p-values
  p12 <- get_p_value(sub_df, "B-1", "B-2")
  p23 <- get_p_value(sub_df, "B-2", "B-3")
  p13 <- get_p_value(sub_df, "B-1", "B-3")
  
  # Create summary row
  p_summary_list[[d]] <- data.frame(
    Drug = d,
    B1_n = n_B1,
    B1_pct = round(pct_B1, 2),
    B2_n = n_B2,
    B2_pct = round(pct_B2, 2),
    B3_n = n_B3,
    B3_pct = round(pct_B3, 2),
    P_B1_vs_B2 = signif(p12, 4),
    Sig_B1_vs_B2 = to_star(p12),
    P_B2_vs_B3 = signif(p23, 4),
    Sig_B2_vs_B3 = to_star(p23),
    P_B1_vs_B3 = signif(p13, 4),
    Sig_B1_vs_B3 = to_star(p13),
    stringsAsFactors = FALSE
  )
}

# Combine all drugs
p_summary_table <- bind_rows(p_summary_list) %>%
  mutate(Drug = factor(Drug, levels = drugs)) %>%
  arrange(Drug)

cat("========== FISHER'S EXACT TEST RESULTS ==========\n")
print(p_summary_table)
cat("\n")

##============================================================
## 7. Generate Grouped Bar Chart (All drugs × All clades)
##============================================================
# Aggregate data for grouped bar plot
sum_df_all <- df_long %>%
  group_by(Drug, clade) %>%
  summarise(
    Total = n(),
    Resistant = sum(Resistance, na.rm = TRUE),
    Percent = if_else(Total > 0, Resistant / Total * 100, 0),
    .groups = "drop"
  ) %>%
  complete(Drug, clade, 
           fill = list(Total = 0, Resistant = 0, Percent = 0))

# Grouped bar plot
p_grouped <- ggplot(sum_df_all, aes(x = Drug, y = Percent, fill = clade)) +
  geom_col(
    position = position_dodge(width = 0.8),
    color = "black",
    width = 0.7
  ) +
  scale_fill_manual(values = clade_colors, name = "Clade") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, 100)
  ) +
  labs(
    title = "Antimicrobial Resistance Profile by Clade",
    x = "Antibiotic Drug",
    y = "Resistance Rate (%)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(p_grouped)

##============================================================
## 8. Generate Radar Chart (Spider Plot)
##============================================================
# Transform data to wide format for radar chart
radar_data <- sum_df_all %>%
  select(clade, Drug, Percent) %>%
  pivot_wider(names_from = Drug, values_from = Percent) %>%
  as.data.frame()

# Set clade as row names
rownames(radar_data) <- radar_data$clade
radar_data <- radar_data[, -1]

# Add significance stars to drug names (based on p-values)
colnames(radar_data) <- c(
  "AMG***", "ESBL***", "MAC***", "PEN*",
  "PHE***", "QUI***", "SUL***", "TET**", "TMP***"
)

# fmsb requirement: first row = max, second row = min
max_min <- data.frame(
  matrix(rep(c(100, 0), each = ncol(radar_data)), nrow = 2, byrow = TRUE)
)
colnames(max_min) <- colnames(radar_data)
rownames(max_min) <- c("Max", "Min")

# Combine max/min with actual data
radar_df <- rbind(max_min, radar_data)

# Define colors with transparency
colors_border <- c(
  rgb(0.58, 0.77, 0.49, 1),   # B-1 border (green)
  rgb(0.46, 0.65, 0.69, 1),   # B-2 border (blue)
  rgb(0.88, 0.40, 0.40, 1)    # B-3 border (red)
)

colors_fill <- c(
  rgb(0.58, 0.77, 0.49, 0.3), # B-1 fill (semi-transparent green)
  rgb(0.46, 0.65, 0.69, 0.3), # B-2 fill (semi-transparent blue)
  rgb(0.88, 0.40, 0.40, 0.3)  # B-3 fill (semi-transparent red)
)

# Set plot margins to prevent truncation
par(mar = c(1, 1, 2, 1))

# Generate radar chart
radarchart(
  radar_df,
  axistype = 1,
  pcol = colors_border,
  pfcol = colors_fill,
  plwd = 2,
  cglcol = "grey50",
  cglty = 3,
  cglwd = 1,
  axislabcol = "black",
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  vlcex = 1.1,
  title = "MDR Profile of Salmonella Agona Clades B-1, B-2, B-3"
)

# Add legend with sample sizes
n_B1 <- sum_df_all %>% filter(clade == "B-1") %>% pull(Total) %>% first()
n_B2 <- sum_df_all %>% filter(clade == "B-2") %>% pull(Total) %>% first()
n_B3 <- sum_df_all %>% filter(clade == "B-3") %>% pull(Total) %>% first()

legend(
  x = "topright",
  legend = c(
    paste("B-1 (n=", n_B1, ")", sep = ""),
    paste("B-2 (n=", n_B2, ")", sep = ""),
    paste("B-3 (n=", n_B3, ")", sep = "")
  ),
  bty = "n",
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 1.2,
  pt.cex = 3
)

##============================================================
## 9. Save All Outputs
##============================================================
cat("========== SAVING OUTPUTS ==========\n\n")

# Save 9-plot grid
ggsave(
  "output/clade_amr_nine_drug_grid.png",
  plot = final_9plot,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)
ggsave(
  "output/clade_amr_nine_drug_grid.pdf",
  plot = final_9plot,
  width = 12,
  height = 10,
  bg = "white"
)

# Save grouped bar chart
ggsave(
  "output/clade_amr_grouped_bar_chart.png",
  plot = p_grouped,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
ggsave(
  "output/clade_amr_grouped_bar_chart.pdf",
  plot = p_grouped,
  width = 10,
  height = 6,
  bg = "white"
)

# Save radar chart (manually using device function)
png("output/clade_amr_radar_chart.png", width = 800, height = 800, res = 300)
par(mar = c(1, 1, 2, 1))
radarchart(
  radar_df,
  axistype = 1,
  pcol = colors_border,
  pfcol = colors_fill,
  plwd = 2,
  cglcol = "grey50",
  cglty = 3,
  cglwd = 1,
  axislabcol = "black",
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  vlcex = 1.1,
  title = "MDR Profile of Salmonella Agona Clades B-1, B-2, B-3"
)
legend(
  x = "topright",
  legend = c(
    paste("B-1 (n=", n_B1, ")", sep = ""),
    paste("B-2 (n=", n_B2, ")", sep = ""),
    paste("B-3 (n=", n_B3, ")", sep = "")
  ),
  bty = "n",
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 1.2,
  pt.cex = 3
)
dev.off()

pdf("output/clade_amr_radar_chart.pdf", width = 10, height = 10)
par(mar = c(1, 1, 2, 1))
radarchart(
  radar_df,
  axistype = 1,
  pcol = colors_border,
  pfcol = colors_fill,
  plwd = 2,
  cglcol = "grey50",
  cglty = 3,
  cglwd = 1,
  axislabcol = "black",
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  vlcex = 1.1,
  title = "MDR Profile of Salmonella Agona Clades B-1, B-2, B-3"
)
legend(
  x = "topright",
  legend = c(
    paste("B-1 (n=", n_B1, ")", sep = ""),
    paste("B-2 (n=", n_B2, ")", sep = ""),
    paste("B-3 (n=", n_B3, ")", sep = "")
  ),
  bty = "n",
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 1.2,
  pt.cex = 3
)
dev.off()

# Save statistical summary tables
write_xlsx(p_summary_table, "output/clade_amr_fisher_test_results.xlsx")
write.csv(
  p_summary_table,
  "output/clade_amr_fisher_test_results.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Save resistance rate data
write.csv(
  sum_df_all,
  "output/clade_amr_resistance_rates.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Save raw long-format data
write.csv(
  df_long,
  "output/clade_amr_raw_data.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

##============================================================
## 10. Completion Summary
##============================================================
cat("✓ Nine-drug plot grid saved: clade_amr_nine_drug_grid.png/pdf\n")
cat("✓ Grouped bar chart saved: clade_amr_grouped_bar_chart.png/pdf\n")
cat("✓ Radar chart saved: clade_amr_radar_chart.png/pdf\n")
cat("✓ Statistical results saved: clade_amr_fisher_test_results.xlsx/csv\n")
cat("✓ Resistance rates saved: clade_amr_resistance_rates.csv\n")
cat("✓ Raw data saved: clade_amr_raw_data.csv\n\n")
cat("All output files saved to 'output/' directory\n\n")