##====================================
## Approach 1: Corrected Radar Chart (Polygon) - Absolute Zero Origin
##====================================
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# 1. Load data (adjust sheet name as needed)
df <- read_excel("data/AMR_Gene_Radar_Polygon.xlsx", sheet = 1)
# Alternative: read_excel("data/date1.xlsx", sheet = 1)

# --- Data Cleaning ---
df_num <- df %>%
  mutate(across(.cols = -clade, .fns = ~ as.numeric(str_remove(as.character(.x), "%"))))

df_long <- df_num %>%
  pivot_longer(cols = -clade, names_to = "gene", values_to = "freq")

# --- Statistical Calculation ---
df_long2 <- df_long %>% 
  mutate(group = ifelse(clade == "B", "B", "Other"))

stat_gene <- df_long2 %>%
  group_by(gene, group) %>%
  summarise(mean_freq = mean(freq, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = mean_freq) %>%
  mutate(B = ifelse(is.na(B), 0, B), Other = ifelse(is.na(Other), 0, Other))

eps <- 1e-6
stat_gene <- stat_gene %>%
  mutate(
    log2FC = log2((B + eps) / (Other + eps)),
    abs_log2FC = abs(log2FC)
  )

# --- Select Top 30 ---
topN <- 30
top30 <- stat_gene %>%
  arrange(desc(abs_log2FC)) %>%
  slice(1:topN) %>%
  arrange(desc(log2FC))

top30$gene <- factor(top30$gene, levels = top30$gene)

# --- Drug Class Classification ---
top30 <- top30 %>%
  mutate(drug_class = case_when(
    str_detect(gene, "^tet") ~ "TET",
    str_detect(gene, "^sul") ~ "SUL",
    str_detect(gene, "^dfr") ~ "TMP",
    str_detect(gene, "^qnr|^oqx|^qep") ~ "QNL",
    str_detect(gene, "^cat|^cml|^flo") ~ "PHE",
    str_detect(gene, "^fos") ~ "FOS",
    str_detect(gene, "^erm|^mph|^lnu|^ere|^mef|^msr") ~ "MAC",
    str_detect(gene, "^aac|^aad|^aph|^ant|^arm|^rmt") ~ "AMG",
    gene %in% c("blaTEM-1", "blaTEM", "blaCARB-2") ~ "PEN",
    str_detect(gene, "^bla") ~ "ESBL",
    TRUE ~ "Other"
  ))

# --- Prepare Plotting Data ---
col_purple_B <- "#9B59B6" 
col_blue_Other <- "#3498DB"
gene_label_colors <- ifelse(top30$log2FC > 0, col_purple_B, col_blue_Other)

# 1. Outer ring: points and text data
ring1_2 <- top30 %>%
  mutate(direction = ifelse(log2FC >= 0, "Up_in_B", "Down_in_B"),
         log2FC_label = sprintf("%.2f", log2FC))

ring1_2$id <- as.numeric(ring1_2$gene)
number_of_bar <- nrow(ring1_2)
ring1_2$angle <- 90 - 360 * (ring1_2$id - 0.5) / number_of_bar
ring1_2$hjust <- ifelse(ring1_2$angle < -90, 1, 0)
ring1_2$angle <- ifelse(ring1_2$angle < -90, ring1_2$angle + 180, ring1_2$angle)

# 2. Inner ring: frequency data
ring3 <- top30 %>%
  select(gene, B, Other) %>%
  pivot_longer(cols = c(B, Other), names_to = "group", values_to = "value")

##====================================
## CORE MODIFICATION: Precise Mapping Algorithm
##====================================
r_petal_start <- 30  # Corresponds to 0% frequency
r_petal_end   <- 60  # Corresponds to max frequency

# [CRITICAL]: Force minimum = 0, maximum = current data maximum
val_min <- 0 
val_max <- max(ring3$value, na.rm = TRUE)

# Calculate mapped height
ring3_plot <- ring3 %>%
  mutate(
    # Linear interpolation: (value / max) * track_width + start_radius
    value_scaled = r_petal_start + (value / val_max) * (r_petal_end - r_petal_start)
  )

##====================================
## PLOTTING
##====================================
# Color definition
line_col_1 <- "#CCCCCC" 
line_col_2 <- "#FFD700" 
line_col_5 <- "#FFA500" 
drug_colors <- c("AMG" = "#E74C3C", "PEN" = "#8E44AD", "ESBL" = "#2980B9", "PHE" = "#16A085", 
                 "TMP" = "#F39C12", "FOS" = "#D35400", "MAC" = "#C0392B", "QNL" = "#27AE60", 
                 "SUL" = "#7F8C8D", "TET" = "#2C3E50", "Other" = "grey50")

p1 <- ggplot() +
  # 1. Reference lines
  geom_hline(yintercept = r_petal_start, color = line_col_1, linewidth = 0.6, linetype = "dashed") +  # 0% line
  geom_hline(yintercept = r_petal_end, color = line_col_2, linewidth = 0.6, linetype = "solid") +     # Max% line
  geom_hline(yintercept = 115, color = line_col_5, linewidth = 0.8) + 
  
  # 2. [CORE] Frequency petals (Polygon)
  geom_polygon(
    data = ring3_plot,
    aes(x = gene, y = value_scaled, fill = group, group = group),
    alpha = 0.4, linewidth = 0.5, color = NA  # No border to avoid confusion, focus on area
  ) +
  
  # 3. Outer ring: Log2FC points
  geom_point(data = ring1_2, aes(x = gene, y = 75, size = abs_log2FC, color = direction), alpha = 0.9) +
  
  # 4. Text labels
  geom_text(data = ring1_2, aes(x = gene, y = 88, label = log2FC_label, color = direction), 
            size = 2.8, fontface = "bold") +
  
  # 5. Drug class annotations
  geom_text(data = ring1_2, aes(x = gene, y = 107.5, label = drug_class, angle = angle, color = drug_class), 
            size = 2.6, fontface = "bold") +
  
  # Coordinate settings
  coord_polar() +
  scale_y_continuous(limits = c(0, 115), breaks = NULL) +
  scale_fill_manual(values = c(B = col_purple_B, Other = col_blue_Other), name = "Mean Frequency") +
  scale_color_manual(values = c(Up_in_B = col_purple_B, Down_in_B = col_blue_Other, drug_colors), 
                     breaks = c("Up_in_B", "Down_in_B"), name = "Group / Direction") +
  scale_size_continuous(range = c(2, 7), name = "|log2FC|") +
  
  theme_minimal() +
  theme(
    axis.title = element_blank(), 
    axis.text.y = element_blank(), 
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 9, face = "bold.italic", color = gene_label_colors),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  ggtitle("AMR Gene Profile (Polygon)", 
          subtitle = paste0("Inner Circle = 0% | Outer Circle (Gold) = ", round(val_max, 1), "% Max Frequency"))

print(p1)

# Optional: save to output directory
if (!dir.exists("output")) dir.create("output")
ggsave("output/AMR_Gene_Radar_Polygon.pdf", p1, width = 10, height = 10, dpi = 300)
ggsave("output/AMR_Gene_Radar_Polygon.png", p1, width = 10, height = 10, dpi = 300)