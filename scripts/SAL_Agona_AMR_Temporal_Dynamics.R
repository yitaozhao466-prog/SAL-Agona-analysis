##============================================================
## AMR Temporal Dynamics Analysis
## Pre-2013 vs 2013+ Comparison with Clade B Contribution
## Dumbbell plot and delta decomposition visualization
##============================================================
# install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "rstatix", "patchwork"))

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rstatix)
library(patchwork)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Load Data
##============================================================
input_file <- "data/amr_phenotype_clade_year.xlsx"

df <- read_excel(input_file) %>%
  filter(!is.na(Year), !is.na(clade), !is.na(耐药基因)) %>%
  mutate(
    period = factor(
      ifelse(Year <= 2012, "Pre-2013", "2013+"),
      levels = c("Pre-2013", "2013+")
    )
  )

cat("========== DATA SUMMARY ==========\n")
cat("Total records loaded:", nrow(df), "\n")
cat("Year range:", min(df$Year), "–", max(df$Year), "\n")
cat("Clades represented:", paste(unique(df$clade), collapse = ", "), "\n")
cat("Mean AMR genes (Pre-2013):", 
    round(mean(df$耐药基因[df$period == "Pre-2013"], na.rm = TRUE), 3), "\n")
cat("Mean AMR genes (2013+):", 
    round(mean(df$耐药基因[df$period == "2013+"], na.rm = TRUE), 3), "\n\n")

##============================================================
## 2. Prepare Two Datasets
##============================================================
# Dataset 1: All clades
# Dataset 2: All clades excluding B
df_plot <- bind_rows(
  df %>% mutate(dataset = "All clades"),
  df %>% filter(clade != "B") %>% mutate(dataset = "All clades excl. B")
) %>%
  mutate(
    dataset = factor(dataset,
                     levels = c("All clades excl. B", "All clades"))
  )

cat("Samples in each dataset:\n")
cat("  All clades:", nrow(df), "\n")
cat("  All clades excl. B:", nrow(df %>% filter(clade != "B")), "\n\n")

##============================================================
## 3. Bootstrap 95% Confidence Intervals
##============================================================
set.seed(123)  # For reproducibility

boot_ci <- function(x, B = 2000) {
  m <- replicate(B, mean(sample(x, replace = TRUE)))
  c(lo = quantile(m, 0.025), hi = quantile(m, 0.975))
}

sum_df <- df_plot %>%
  group_by(dataset, period) %>%
  summarise(
    n = n(),
    mean = mean(耐药基因, na.rm = TRUE),
    sd = sd(耐药基因, na.rm = TRUE),
    lo = boot_ci(耐药基因)[1],
    hi = boot_ci(耐药基因)[2],
    .groups = "drop"
  )

cat("========== SUMMARY STATISTICS (Mean ± 95% Bootstrap CI) ==========\n")
print(sum_df %>%
        mutate(across(c(mean, sd, lo, hi), ~ round(., 3))) %>%
        as.data.frame())
cat("\n")

##============================================================
## 4. Calculate Delta and Statistical Tests
##============================================================
# Delta = change from Pre-2013 to 2013+
delta_df <- sum_df %>%
  select(dataset, period, mean) %>%
  pivot_wider(names_from = period, values_from = mean) %>%
  mutate(
    delta = `2013+` - `Pre-2013`,
    pct_change = delta / `Pre-2013` * 100
  )

# Wilcoxon rank-sum test (non-parametric alternative to t-test)
p_df <- df_plot %>%
  group_by(dataset) %>%
  wilcox_test(耐药基因 ~ period) %>%
  select(dataset, p, p.signif)

# Calculate Clade B contribution to total increase
delta_all <- delta_df$delta[delta_df$dataset == "All clades"]
delta_noB <- delta_df$delta[delta_df$dataset == "All clades excl. B"]
contrib_B <- delta_all - delta_noB
pct_B <- contrib_B / delta_all * 100

cat("========== TEMPORAL CHANGE ANALYSIS ==========\n")
cat("Delta (All clades):", round(delta_all, 3), "\n")
cat("Delta (excl. B):", round(delta_noB, 3), "\n")
cat("Clade B contribution:", round(contrib_B, 3), 
    sprintf("(%.1f%% of total increase)\n\n", pct_B))

cat("========== STATISTICAL TEST RESULTS (Wilcoxon) ==========\n")
print(p_df %>%
        mutate(p = signif(p, 4)) %>%
        as.data.frame())
cat("\n")

##============================================================
## 5. Prepare Labels and Annotations
##============================================================
lab_df <- delta_df %>%
  left_join(p_df, by = "dataset") %>%
  mutate(
    # Format delta and p-value for display
    label = sprintf("Δ = %.2f\np = %.2e", delta, p),
    # Position label to right of higher value
    x_lab = pmax(`Pre-2013`, `2013+`) + 0.35
  ) %>%
  select(dataset, label, x_lab)

##============================================================
## 6. Define Soft Color Palette
##============================================================
# Professional low-saturation colors
col_pre <- "#6B8CAE"    # Muted blue-grey (Pre-2013)
col_post <- "#D98F6C"   # Soft rust-orange (2013+)
col_B <- "#C0735A"      # Clay/terracotta (Clade B emphasis)
col_grey <- "#B4BBC4"   # Light grey-blue (Other clades, connector lines)
col_txt <- "#2F3640"    # Dark grey (text)

cols_period <- c("Pre-2013" = col_pre, "2013+" = col_post)

##============================================================
## 7. Create Dumbbell Plot (Panel A)
##============================================================
p1 <- ggplot() +
  # Connector lines (grey) linking Pre-2013 to 2013+ for each dataset
  geom_segment(
    data = delta_df,
    aes(y = dataset, yend = dataset,
        x = `Pre-2013`, xend = `2013+`),
    color = col_grey,
    linewidth = 2.2,
    lineend = "round"
  ) +
  
  # Error bars (95% bootstrap CI) for each point
  geom_errorbarh(
    data = sum_df,
    aes(y = dataset, xmin = lo, xmax = hi, color = period),
    height = 0.12,
    linewidth = 0.6
  ) +
  
  # Points for mean values
  geom_point(
    data = sum_df,
    aes(x = mean, y = dataset, color = period),
    size = 5.5
  ) +
  
  # Annotation with delta and p-value
  geom_text(
    data = lab_df,
    aes(x = x_lab, y = dataset, label = label),
    hjust = 0,
    fontface = "bold",
    size = 3.6,
    lineheight = 0.95,
    color = col_txt
  ) +
  
  # Color and scale
  scale_color_manual(values = cols_period, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) +
  
  # Labels
  labs(
    x = "Mean AMR Gene Count (95% Bootstrap CI)",
    y = NULL,
    title = "Temporal Change in AMR Burden"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", color = col_txt, size = 14),
    axis.text.y = element_text(color = col_txt, face = "bold", size = 11),
    axis.text.x = element_text(color = col_txt, size = 10)
  )

##============================================================
## 8. Create Delta Decomposition Plot (Panel B)
##============================================================
# Break down total increase into Clade B vs Other contributions
decomp <- tibble(
  part = factor(c("Clade B", "Other clades"),
                levels = c("Other clades", "Clade B")),
  start = c(delta_noB, 0),
  end = c(delta_all, delta_noB)
)

p2 <- ggplot(decomp) +
  # Stacked horizontal line segment
  geom_segment(
    aes(x = start, xend = end, y = 1, yend = 1, color = part),
    linewidth = 9,
    lineend = "butt"
  ) +
  
  # Label for "Other clades" contribution
  annotate(
    "text",
    x = delta_noB / 2,
    y = 1.35,
    label = sprintf("Other clades\n%.2f (%.1f%%)",
                    delta_noB, 100 - pct_B),
    size = 3.5,
    color = "grey40",
    fontface = "plain"
  ) +
  
  # Label for "Clade B" contribution
  annotate(
    "text",
    x = delta_noB + contrib_B / 2,
    y = 1.35,
    label = sprintf("Clade B\n%.2f (%.1f%%)",
                    contrib_B, pct_B),
    size = 3.5,
    fontface = "bold",
    color = col_B
  ) +
  
  # Color scale
  scale_color_manual(
    values = c("Clade B" = col_B, "Other clades" = col_grey)
  ) +
  scale_y_continuous(limits = c(0.7, 1.7)) +
  scale_x_continuous(limits = c(0, delta_all * 1.02)) +
  
  # Labels
  labs(
    x = "Increase in Mean AMR Gene Count (Δ)",
    y = NULL,
    title = sprintf("Clade B accounts for %.0f%% of the increase", pct_B),
    caption = "Grey: increase from other clades. Orange: Clade B contribution. Segments are additive."
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = col_txt, size = 10),
    plot.title = element_text(face = "bold", color = col_B, size = 14),
    plot.caption = element_text(color = "grey45", size = 8, hjust = 0)
  )

##============================================================
## 9. Combine Panels
##============================================================
final_plot <- p1 / p2 + 
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

print(final_plot)

##============================================================
## 10. Export Summary Tables
##============================================================
# Summary statistics table
summary_table <- sum_df %>%
  select(dataset, period, n, mean, sd, lo, hi) %>%
  mutate(
    across(c(mean, sd, lo, hi), ~ round(., 3))
  ) %>%
  rename(
    Dataset = dataset,
    Period = period,
    `Sample Size` = n,
    `Mean AMR Genes` = mean,
    `Std Dev` = sd,
    `CI Lower` = lo,
    `CI Upper` = hi
  )

write.csv(
  summary_table,
  "output/amr_temporal_summary.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Delta and statistical test results
delta_table <- delta_df %>%
  left_join(p_df, by = "dataset") %>%
  select(dataset, `Pre-2013`, `2013+`, delta, pct_change, p, p.signif) %>%
  mutate(
    across(c(`Pre-2013`, `2013+`, delta, pct_change, p), ~ round(., 4))
  ) %>%
  rename(
    Dataset = dataset,
    `Pre-2013 Mean` = `Pre-2013`,
    `2013+ Mean` = `2013+`,
    `Delta (Δ)` = delta,
    `% Change` = pct_change,
    `P-value` = p,
    Significance = p.signif
  )

write.csv(
  delta_table,
  "output/amr_temporal_delta_results.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Clade B contribution analysis
clade_contrib <- tibble(
  Metric = c("Total Increase (All clades)",
             "Increase (excl. B)",
             "Clade B Contribution",
             "Clade B % of Total"),
  Value = c(round(delta_all, 3),
            round(delta_noB, 3),
            round(contrib_B, 3),
            sprintf("%.1f%%", pct_B))
)

write.csv(
  clade_contrib,
  "output/amr_clade_b_contribution.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

##============================================================
## 11. Save Visualizations
##============================================================
# High-resolution PNG
ggsave(
  "output/amr_temporal_dumbbell_plot.png",
  plot = final_plot,
  width = 7.5,
  height = 5.5,
  dpi = 600,
  bg = "white"
)

# Vector PDF (preferred for publication)
ggsave(
  "output/amr_temporal_dumbbell_plot.pdf",
  plot = final_plot,
  width = 7.5,
  height = 5.5,
  device = cairo_pdf,
  bg = "white"
)

##============================================================
## 12. Completion Summary
##============================================================
cat("========== ANALYSIS COMPLETE ==========\n\n")
cat("✓ Visualization saved:\n")
cat("  - amr_temporal_dumbbell_plot.png (600 DPI)\n")
cat("  - amr_temporal_dumbbell_plot.pdf (vector)\n\n")
cat("✓ Summary tables exported:\n")
cat("  - amr_temporal_summary.csv (mean ± CI by period)\n")
cat("  - amr_temporal_delta_results.csv (change analysis)\n")
cat("  - amr_clade_b_contribution.csv (Clade B impact)\n\n")
cat("========== KEY FINDINGS ==========\n")
cat(sprintf("Total increase in AMR burden (All clades): Δ = %.3f\n", delta_all))
cat(sprintf("Increase without Clade B: Δ = %.3f\n", delta_noB))
cat(sprintf("Clade B contribution: %.3f (%.1f%% of total increase)\n",
            contrib_B, pct_B))
cat(sprintf("Statistical significance (Wilcoxon, all clades): p = %.2e\n\n",
            p_df$p[p_df$dataset == "All clades"]))