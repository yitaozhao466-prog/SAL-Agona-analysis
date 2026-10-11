##============================================================
## Global & China Antibiotic Resistance Trends with Inset
## Analysis of 9 drug classes from before 2000 to 2025
##============================================================
library(ggplot2)
library(cowplot)
library(grid)

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

##============================================================
## 1. Main Dataset: 9 Drug Classes (before 2000 + 2001-2025, 26 values total)
##============================================================
main_list <- list(
  Penicillin = c(3.85, 8.00, 12.50, 5.38, 10.53, 5.30, 3.53, 4.94,
                 2.38, 11.94, 3.85, 3.33, 6.72, 16.78, 20.07, 10.75,
                 10.10, 16.27, 13.82, 12.10, 9.87, 19.03, 14.71, 11.90,
                 16.08, 11.43),
  Macrolide = c(0.00, 0.00, 1.56, 1.08, 1.75, 0.00, 1.18, 0.00,
                0.60, 0.00, 0.00, 0.00, 0.00, 0.00, 1.86, 1.25,
                1.98, 2.96, 2.29, 0.95, 1.29, 4.18, 6.10, 5.68,
                13.00, 2.86),
  ESBL = c(0.00, 8.00, 12.50, 10.75, 0.00, 3.97, 14.12, 7.41,
           1.79, 3.73, 3.85, 10.00, 5.88, 6.71, 4.83, 7.00,
           3.56, 6.48, 11.86, 7.18, 6.80, 12.30, 19.25, 23.36,
           23.83, 14.29),
  Trimethoprim = c(4.80, 50.00, 15.63, 8.60, 3.51, 4.64, 15.29, 3.70,
                   1.79, 5.22, 3.08, 5.56, 5.88, 22.82, 20.07, 16.00,
                   13.66, 16.61, 24.16, 17.30, 13.59, 21.58, 24.41, 24.89,
                   22.23, 28.57),
  Tetracycline = c(11.54, 42.00, 25.00, 25.81, 29.82, 9.27, 30.59, 17.28,
                   10.12, 23.88, 13.85, 14.44, 16.81, 28.19, 41.26, 30.00,
                   24.95, 30.72, 38.08, 31.95, 25.40, 34.34, 41.16, 43.23,
                   47.55, 40.00),
  Phenicols = c(1.92, 12.00, 12.50, 12.90, 5.26, 3.97, 15.29, 6.17,
                3.57, 11.19, 4.62, 5.56, 3.36, 9.40, 12.27, 14.25,
                12.08, 14.11, 21.55, 13.04, 9.87, 22.04, 24.10, 22.60,
                28.96, 25.71),
  Quinolone = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.18, 0.00,
                0.60, 0.00, 3.08, 2.22, 4.20, 15.44, 16.73, 17.00,
                14.46, 20.25, 28.40, 18.43, 12.14, 20.42, 29.89, 29.80,
                35.35, 37.14),
  Aminoglycosides = c(14.42, 56.00, 21.88, 21.51, 21.05, 11.92, 30.59, 18.52,
                      8.93, 23.13, 13.08, 12.22, 36.97, 22.15, 36.80, 23.00,
                      17.43, 26.05, 34.60, 26.18, 20.55, 32.71, 35.99, 38.43,
                      43.22, 31.43),
  Sulfonamide = c(13.46, 56.00, 21.88, 23.66, 22.81, 8.61, 31.76, 18.52,
                  8.93, 25.37, 9.23, 10.00, 22.69, 27.52, 39.41, 21.50,
                  19.41, 25.37, 27.75, 26.09, 21.20, 29.47, 29.58, 31.22,
                  35.12, 14.29)
)

##============================================================
## 2. Inset Dataset: China (2012-2024, 13 values)
##============================================================
inset_list <- list(
  Penicillin = c(15.61, 72.00, 69.09, 20.75, 19.61, 48.96,
                 50.94, 60.87, 64.58, 63.51, 51.19, 28.85, 34.74),
  Macrolide = c(0.00, 0.00, 0.00, 1.89, 1.96, 4.17,
                5.66, 7.25, 0.00, 20.27, 22.62, 22.44, 41.78),
  ESBL = c(5.56, 0.00, 0.00, 1.89, 0.00, 2.08,
           1.89, 14.49, 4.17, 35.14, 34.52, 45.51, 48.83),
  Trimethoprim = c(2.78, 72.00, 69.09, 22.64, 21.57, 22.92,
                   45.28, 59.42, 62.50, 66.22, 47.62, 33.33, 32.39),
  Tetracycline = c(15.61, 72.00, 69.09, 50.94, 37.25, 52.08,
                   54.72, 81.16, 68.75, 68.92, 63.10, 60.90, 77.46),
  Phenicols = c(15.61, 32.00, 34.55, 52.83, 49.02, 32.29,
                43.40, 50.72, 12.50, 62.16, 57.14, 55.77, 61.50),
  Quinolone = c(0.00, 72.00, 69.09, 71.70, 64.71, 61.46,
                69.81, 76.81, 68.75, 74.32, 66.67, 58.33, 77.93),
  Aminoglycosides = c(15.61, 68.00, 69.09, 24.53, 21.57, 50.00,
                      54.72, 71.01, 66.67, 67.57, 69.05, 60.90, 78.87),
  Sulfonamide = c(18.39, 72.00, 69.09, 18.87, 19.61, 50.00,
                  50.94, 68.12, 66.67, 68.92, 59.52, 55.77, 69.95)
)

# Validation
stopifnot(length(main_list) == 9, length(inset_list) == 9)
stopifnot(all(sapply(main_list,  length) == 26))
stopifnot(all(sapply(inset_list, length) == 13))

##============================================================
## 3. Aggregate: Calculate mean resistance rate per year
##============================================================
main_mat  <- do.call(cbind, main_list)
inset_mat <- do.call(cbind, inset_list)

data <- data.frame(year = c("before2000", 2001:2025), 
                   value = rowMeans(main_mat))
data$year_num <- ifelse(data$year == "before2000", 1999, as.numeric(data$year))

data_before_2013 <- data[data$year_num <= 2013, ]
data_after_2013  <- data[data$year_num >= 2013, ]

data_inset <- data.frame(time = c(2012, 2013:2024), 
                         mean_count = rowMeans(inset_mat))

##============================================================
## 4. Linear Regression Analysis
##============================================================
model_before <- lm(value ~ year_num, data = data_before_2013)
model_after  <- lm(value ~ year_num, data = data_after_2013)

y_max_main  <- 50   # Main plot y-axis limit (adjust here for all plots)
y_cap_main  <- 50   # Main plot prediction line cap
y_cap_inset <- 80   # Inset prediction line cap

# Generate prediction lines
pred_before <- data.frame(year_num = seq(min(data_before_2013$year_num),
                                         max(data_before_2013$year_num), length.out = 100))
pred_before$value <- pmax(0, pmin(y_cap_main, predict(model_before, newdata = pred_before)))

pred_after <- data.frame(year_num = seq(min(data_after_2013$year_num),
                                        max(data_after_2013$year_num), length.out = 100))
pred_after$value <- pmax(0, pmin(y_cap_main, predict(model_after, newdata = pred_after)))

# Equation and R² formatting functions
make_eq <- function(m) paste0("y = ", sprintf("%.2f", coef(m)[2]), "x ", sprintf("%+.0f", coef(m)[1]))
make_r2 <- function(m) paste0("R² = ", sprintf("%.3f", summary(m)$r.squared))

equation_before <- make_eq(model_before)
r_squared_before <- make_r2(model_before)
equation_after  <- make_eq(model_after)
r_squared_after  <- make_r2(model_after)

# Inset regression
model_inset <- lm(mean_count ~ time, data = data_inset)
pred_inset <- data.frame(time = seq(min(data_inset$time), max(data_inset$time), length.out = 100))
pred_inset$mean_count <- pmax(0, pmin(y_cap_inset, predict(model_inset, newdata = pred_inset)))
equation_inset  <- make_eq(model_inset)
r_squared_inset <- make_r2(model_inset)

##============================================================
## 5. Color Scheme
##============================================================
color_main       <- "#2E86AB"        # Main data line
color_reg_before <- "#A23B72"        # Before 2013 trend
color_reg_after  <- "#F18F01"        # After 2013 trend
color_inset      <- "#C73E1D"        # China data

##============================================================
## 6. Main Plot (y-axis: 0-50)
##============================================================
main_plot <- ggplot() +
  # Regression lines
  geom_line(data = pred_before, aes(x = year_num, y = value),
            color = color_reg_before, linetype = "dashed", linewidth = 0.8) +
  geom_line(data = pred_after, aes(x = year_num, y = value),
            color = color_reg_after, linetype = "dashed", linewidth = 0.8) +
  
  # Data line and points
  geom_line(data = data, aes(x = year_num, y = value), 
            color = color_main, linewidth = 1) +
  geom_point(data = data, aes(x = year_num, y = value), 
             color = color_main, size = 2.5, shape = 16) +
  
  # Vertical line at transition point (2013)
  geom_segment(aes(x = 2013, xend = 2013, y = 0, yend = y_max_main),
               linetype = "dotted", color = "grey50", linewidth = 0.6) +
  
  # Annotations
  annotate("text", x = 1999, y = 48, label = "Global",
           size = 5.5, hjust = 0, fontface = "bold", color = "grey20") +
  annotate("text", x = 2005, y = 48, label = "Overall (9 drug classes)",
           size = 6, hjust = 0, fontface = "bold", color = "black") +
  annotate("text", x = 2000, y = 43, label = equation_before,
           size = 3.5, hjust = 0, color = color_reg_before, fontface = "italic") +
  annotate("text", x = 2000, y = 39.5, label = r_squared_before,
           size = 3.5, hjust = 0, color = color_reg_before) +
  annotate("text", x = 2017, y = 10, label = equation_after,
           size = 3.5, hjust = 0, color = color_reg_after, fontface = "italic") +
  annotate("text", x = 2017, y = 6, label = r_squared_after,
           size = 3.5, hjust = 0, color = color_reg_after) +
  annotate("text", x = 2013, y = 1.5, label = "2013",
           size = 3, hjust = 0.5, color = "grey40", fontface = "italic") +
  
  # Axes and limits
  labs(x = "Year", y = "Resistance Rate (%)") +
  scale_x_continuous(
    breaks = c(1999, 2005, 2010, 2015, 2020, 2025),
    labels = c("before\n2000", "2005", "2010", "2015", "2020", "2025"),
    limits = c(1999, 2025),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, y_max_main), 
    breaks = seq(0, y_max_main, 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # Theme
  theme_bw() +
  theme(
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

##============================================================
## 7. Inset Plot (China: x-axis with no margin, 2012 at left edge)
##============================================================
inset_plot <- ggplot() +
  # Regression line
  geom_line(data = pred_inset, aes(x = time, y = mean_count),
            color = "black", linetype = "dashed", linewidth = 0.7) +
  
  # Data line and points
  geom_line(data = data_inset, aes(x = time, y = mean_count),
            color = color_inset, linewidth = 0.9) +
  geom_point(data = data_inset, aes(x = time, y = mean_count),
             color = color_inset, size = 2, shape = 16) +
  
  # Annotations
  annotate("text", x = 2014, y = 72, label = equation_inset,
           size = 2.6, hjust = 0, fontface = "italic") +
  annotate("text", x = 2014, y = 66, label = r_squared_inset,
           size = 2.6, hjust = 0) +
  annotate("text", x = 2012.3, y = 77, label = "China",
           size = 3, hjust = 0, fontface = "bold", color = color_inset) +
  
  # Axes and limits
  scale_x_continuous(
    breaks = c(2012, 2016, 2020, 2024),
    labels = c("before\n2013", "2016", "2020", "2024"),
    limits = c(2012, 2024),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 80), 
    breaks = seq(0, 80, 20), 
    expand = c(0, 0)
  ) +
  
  # Theme
  theme_bw() +
  theme(
    text = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 8, 5, 5)
  )

##============================================================
## 8. Precise Alignment: Inset left edge = Main plot x=2013
##============================================================
W <- 10  # Canvas width (inches)
H <- 7   # Canvas height (inches)

# Function to extract panel dimensions
fixed_dims <- function(p) {
  g  <- ggplotGrob(p)
  pn <- g$layout[g$layout$name == "panel", ][1, ]
  cv <- function(u) grid::convertUnit(sum(u), "in", valueOnly = TRUE)
  list(
    l = cv(g$widths[seq_len(pn$l - 1)]),
    r = cv(g$widths[-seq_len(pn$r)]),
    t = cv(g$heights[seq_len(pn$t - 1)]),
    b = cv(g$heights[-seq_len(pn$b)])
  )
}

fm <- fixed_dims(main_plot)
fi <- fixed_dims(inset_plot)

pw_main <- W - fm$l - fm$r
ph_main <- H - fm$t - fm$b

# Data coordinates → Canvas inches (including expansion 0.02)
xlo <- 1999 - 0.02 * 26
xhi <- 2025 + 0.02 * 26
ylo <- 0 - 0.02 * y_max_main
yhi <- y_max_main + 0.02 * y_max_main

x_in <- function(x) fm$l + (x - xlo) / (xhi - xlo) * pw_main
y_in <- function(y) fm$b + (y - ylo) / (yhi - ylo) * ph_main

# ---- Adjustable parameters ----
x_left_data  <- 2013   # Inset left edge aligns with main plot x (= before 2013)
x_right_data <- 2025   # Inset right edge aligns with main plot x
y_bot_data   <- 28     # Inset panel bottom (main plot y coordinate)
y_top_data   <- 48     # Inset panel top (main plot y coordinate)

pw_inset <- x_in(x_right_data) - x_in(x_left_data)
ph_inset <- y_in(y_top_data)   - y_in(y_bot_data)

inset_x <- (x_in(x_left_data) - fi$l) / W
inset_y <- (y_in(y_bot_data)  - fi$b) / H
inset_w <- (fi$l + pw_inset + fi$r) / W
inset_h <- (fi$b + ph_inset + fi$t) / H

##============================================================
## 9. Combine plots
##============================================================
final_plot <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset_plot, x = inset_x, y = inset_y, width = inset_w, height = inset_h)

print(final_plot)

##============================================================
## 10. Save outputs
##============================================================
ggsave("output/AMR_Temporal_Trends.png", final_plot,
       width = W, height = H, units = "in", dpi = 300, bg = "white")
ggsave("output/AMR_Temporal_Trends.pdf", final_plot,
       width = W, height = H, units = "in", device = cairo_pdf)
ggsave("output/AMR_Temporal_Trends.tiff", final_plot,
       width = W, height = H, units = "in", dpi = 300, compression = "lzw")

# Export data for reference
write.csv(data,       "output/AMR_Global_Trends_Data.csv",  row.names = FALSE)
write.csv(data_inset, "output/AMR_China_Trends_Data.csv", row.names = FALSE)

cat("\n✓ Analysis complete! All outputs saved to 'output/' directory.\n")
cat("  - Main plot: AMR_Temporal_Trends.png/pdf/tiff\n")
cat("  - Data exports: AMR_Global_Trends_Data.csv, AMR_China_Trends_Data.csv\n\n")