# =========================
# 0) Preparation
# =========================
library(readxl)
library(dplyr)

file1 <- "D:/your_data_path/Resistance Concordance Analyzerdata_1.xlsx"
file2 <- "D:/your_data_path/Resistance Concordance Analyzerdata_2.xlsx"

# Read tables (defaults to first sheet)
t1 <- read_excel(file1)
t2 <- read_excel(file2)

# Standardize column names (trim whitespace)
names(t1) <- trimws(names(t1))
names(t2) <- trimws(names(t2))

id_col <- "Strain_ID"
stopifnot(id_col %in% names(t1), id_col %in% names(t2))

# Drug columns must be identical
drug_cols1 <- setdiff(names(t1), id_col)
drug_cols2 <- setdiff(names(t2), id_col)
stopifnot(all(drug_cols1 %in% drug_cols2), all(drug_cols2 %in% drug_cols1))

drug_cols <- drug_cols1

# =========================
# 1) S/R Conversion Rules
# =========================
to_SR <- function(x) {
  # Rules:
  # Empty/NA => S (Susceptible)
  # Non-empty (number/letter/other) => R (Resistant)
  if (is.na(x)) return("S")
  s <- trimws(as.character(x))
  if (length(s) == 0 || s == "" || tolower(s) == "na") return("S")
  if (tolower(s) == "s" || tolower(s) == "susceptible") return("S")
  return("R")
}

# =========================
# 2) Convert to S/R and align by Strain_ID
# =========================
t1_sr <- t1 %>%
  mutate(across(all_of(drug_cols), ~ vapply(.x, to_SR, FUN.VALUE = character(1))))

t2_sr <- t2 %>%
  mutate(across(all_of(drug_cols), ~ vapply(.x, to_SR, FUN.VALUE = character(1))))

df <- t1_sr %>%
  select(all_of(c(id_col, drug_cols))) %>%
  inner_join(
    t2_sr %>% select(all_of(c(id_col, drug_cols))),
    by = id_col,
    suffix = c("_data1", "_data2")
  )

# =========================
# 3) Match rate by drug
# =========================
match_by_drug <- lapply(drug_cols, function(drug) {
  a <- df[[paste0(drug, "_data1")]]
  b <- df[[paste0(drug, "_data2")]]
  
  valid <- !is.na(a) & !is.na(b)
  match_n <- sum(a[valid] == b[valid])
  
  tibble(
    Drug = drug,
    Comparisons = sum(valid),
    Matches = match_n,
    Match_Rate = ifelse(sum(valid) > 0, match_n / sum(valid), NA_real_)
  )
}) %>% bind_rows()

match_by_drug <- match_by_drug %>% arrange(desc(Match_Rate))

print(match_by_drug)

# =========================
# 4) Overall match rate (all drugs + all strains)
# =========================
overall_counts <- lapply(drug_cols, function(drug) {
  a <- df[[paste0(drug, "_data1")]]
  b <- df[[paste0(drug, "_data2")]]
  
  valid <- !is.na(a) & !is.na(b)
  tibble(
    Comparisons = sum(valid),
    Matches = sum(a[valid] == b[valid])
  )
}) %>% bind_rows()

overall_match <- overall_counts %>%
  summarise(
    Total_Comparisons = sum(Comparisons, na.rm = TRUE),
    Total_Matches = sum(Matches, na.rm = TRUE),
    Overall_Match_Rate = Total_Matches / Total_Comparisons
  )

print(overall_match)