# ===================== run_analysis_github.R =====================
# Note: The genus mapping table (Genus_real<-> Genus_mask) can be provided by the authors.
# Reviewers may contact the authors to obtain genus_mapping_PRIVATE1.csv.
# =================================================================

rm(list = ls())

library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(tidyr)

data <- read_excel("allgenus-R.xlsx", sheet = "genuspecentageasv")

variables <- c("log2SII", "LYM_count", "PLT", "NEU_count", "PISA", "BOP", "PESA")

color_vars <- sprintf("Genus_%03d", 1:160)  

map_file <- "genus_mapping_PRIVATE1.csv"
if (!file.exists(map_file)) {
  stop("Missing mapping file: genus_mapping_PRIVATE1.csv")
}

genus_map <- read.csv(map_file, stringsAsFactors = FALSE)

if (nrow(genus_map) != length(color_vars)) {
  stop("Mapping rows (", nrow(genus_map), ") != length(color_vars) (", length(color_vars), ").")
}

rename_map <- setNames(genus_map$Genus_mask, genus_map$Genus_real)

missing_in_data <- setdiff(genus_map$Genus_real, colnames(data))
if (length(missing_in_data) > 0) {
  stop("These genus columns are missing in data: ", paste(missing_in_data, collapse = ", "))
}

data <- data %>% rename(!!!rename_map)

Low_SII <- data[data$SII_2group == 1, ]
High_SII <- data[data$SII_2group == 2, ]
total_SII <- data

spearman_complete <- function(y, x) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  
  idx <- complete.cases(y, x)
  n <- sum(idx)
  
  if (n < 3) {
    return(c(rho = NA, p = NA, n = n))
  }
  
  test <- suppressWarnings(
    cor.test(y[idx], x[idx], method = "spearman", exact = FALSE)
  )
  
  c(rho = unname(test$estimate),
    p   = test$p.value,
    n   = n)
}

run_spearman_block <- function(df, group_name) {
  
  res_list <- list()
  
  for (var in variables) {
    for (genus in color_vars) {
      
      out <- spearman_complete(df[[var]], df[[genus]])
      
      res_list[[length(res_list) + 1]] <- data.frame(
        Group    = group_name,
        Variable = var,
        Genus    = genus,
        rho      = out["rho"],
        p_raw    = out["p"],
        n        = out["n"],
        stringsAsFactors = FALSE
      )
    }
  }
  
  res <- bind_rows(res_list)
  
  res <- res %>%
    group_by(Group, Variable) %>%
    mutate(p_FDR = p.adjust(p_raw, method = "BH")) %>%
    ungroup()
  
  return(res)
}

res_total <- run_spearman_block(total_SII, "Total")
res_low   <- run_spearman_block(Low_SII,   "Low_SII")
res_high  <- run_spearman_block(High_SII,  "High_SII")

res_all <- bind_rows(res_total, res_low, res_high)

res_long <- res_all %>%
  mutate(
    coef = rho,
    P    = p_raw,
    q    = p_FDR
  ) %>%
  select(Group, Variable, Genus, coef, P, q)

res_wide <- res_long %>%
  pivot_longer(
    cols = c(coef, P, q),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    col_name = paste(stat, Group, Variable, sep = "-")
  ) %>%
  select(Genus, col_name, value) %>%
  pivot_wider(
    names_from  = col_name,
    values_from = value
  )

write.csv(
  res_wide,
  file = "Spearman_summary_coef_P_q_all_groups.csv",
  row.names = FALSE
)

genus_candidate <- res_all %>%
  group_by(Genus) %>%
  summarise(
    max_abs_rho = max(abs(rho), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(max_abs_rho >= 0.2)

candidate_genera <- genus_candidate$Genus
length(candidate_genera)

data_perm <- data %>%
  select(
    all_of(c("SII_2group", variables, candidate_genera))
  )

set.seed(1234)

permutation_test_label_shuffle <- function(x, y, group_labels,
                                           n_perm = 10000,
                                           min_n = 5) {
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  idx <- complete.cases(x, y, group_labels)
  x <- x[idx]
  y <- y[idx]
  g <- group_labels[idx]
  
  g1_idx <- which(g == 1)
  g2_idx <- which(g == 2)
  
  if (length(g1_idx) < min_n || length(g2_idx) < min_n) {
    return(c(
      obs_diff = NA,
      p_perm   = NA,
      n_low    = length(g1_idx),
      n_high   = length(g2_idx)
    ))
  }
  
  coef1 <- suppressWarnings(
    cor(x[g1_idx], y[g1_idx], method = "spearman")
  )
  coef2 <- suppressWarnings(
    cor(x[g2_idx], y[g2_idx], method = "spearman")
  )
  
  obs_diff <- abs(coef1 - coef2)
  
  perm_diffs <- replicate(n_perm, {
    perm_g <- sample(g)
    p1 <- which(perm_g == 1)
    p2 <- which(perm_g == 2)
    
    if (length(p1) < min_n || length(p2) < min_n) {
      return(NA)
    }
    
    pc1 <- suppressWarnings(
      cor(x[p1], y[p1], method = "spearman")
    )
    pc2 <- suppressWarnings(
      cor(x[p2], y[p2], method = "spearman")
    )
    
    abs(pc1 - pc2)
  })
  
  perm_diffs <- perm_diffs[!is.na(perm_diffs)]
  
  if (length(perm_diffs) == 0) {
    p_perm <- NA
  } else {
    p_perm <- mean(perm_diffs >= obs_diff)
  }
  
  return(c(
    obs_diff = obs_diff,
    p_perm   = p_perm,
    n_low    = length(g1_idx),
    n_high   = length(g2_idx)
  ))
}

group_labels <- data$SII_2group

perm_results <- list()

for (genus in candidate_genera) {
  
  x <- data[[genus]]
  y <- data[["log2SII"]]
  
  out <- permutation_test_label_shuffle(
    x = x,
    y = y,
    group_labels = group_labels,
    n_perm = 10000,
    min_n = 5
  )
  
  perm_results[[genus]] <- data.frame(
    Genus     = genus,
    obs_diff = out["obs_diff"],
    perm_p   = out["p_perm"],
    n_low    = out["n_low"],
    n_high   = out["n_high"],
    stringsAsFactors = FALSE
  )
}

perm_df <- bind_rows(perm_results)
perm_df <- perm_df %>%
  mutate(perm_q = p.adjust(perm_p, method = "BH"))

write.csv(as.data.frame(perm_df), "permutation.csv", row.names = TRUE)
