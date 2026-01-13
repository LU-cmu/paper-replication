# =========================
# Note: The genus mapping table (Genus_real <-> Genus_mask) can be provided by the authors.
#       Reviewers may contact the authors to obtain genus_mapping_PRIVATE3.csv.
# =========================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(showtext)
library(writexl)

data <- read_excel(
  "allgenus-R.xlsx",
  sheet = "genuspercentageasv"
)

variables <- c("log2SII", "LYM_count", "PLT", "NEU_count")

map_file <- "genus_mapping_PRIVATE3.csv"
if (!file.exists(map_file)) {
  stop("Missing mapping file: genus_mapping_PRIVATE3.csv (provide locally; do not upload to GitHub).")
}
genus_map <- read.csv(map_file, stringsAsFactors = FALSE)


rename_map <- setNames(genus_map$Genus_mask, genus_map$Genus_real)

missing_in_data <- setdiff(genus_map$Genus_real, colnames(data))
if (length(missing_in_data) > 0) {
  stop("These genus columns are missing in data: ", paste(missing_in_data, collapse = ", "))
}

data <- data %>% rename(!!!rename_map)


color_vars  <- genus_map$Genus_mask
genus_order <- genus_map$Genus_mask


Low_SII   <- data[data$SII_2group == 1, ]
High_SII  <- data[data$SII_2group == 2, ]
total_SII <- data


spearman_complete <- function(y, x) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  idx <- complete.cases(y, x)
  n <- sum(idx)
  if (n < 3) return(c(rho = NA, p = NA, n = n))
  test <- suppressWarnings(cor.test(y[idx], x[idx], method = "spearman", exact = FALSE))
  c(rho = unname(test$estimate), p = test$p.value, n = n)
}

run_spearman_block <- function(df, group_name) {
  res_list <- list()
  for (var in variables) {
    for (genus in color_vars) {
      out <- spearman_complete(df[[var]], df[[genus]])
      res_list[[length(res_list) + 1]] <- data.frame(
        Group = group_name,
        Variable = var,
        Genus = genus,
        rho = out["rho"],
        p_raw = out["p"],
        n = out["n"],
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(res_list) %>%
    group_by(Group, Variable) %>%
    mutate(p_FDR = p.adjust(p_raw, method = "BH")) %>%
    ungroup()
}

res_all <- bind_rows(
  run_spearman_block(total_SII, "Total"),
  run_spearman_block(Low_SII,   "Low_SII"),
  run_spearman_block(High_SII,  "High_SII")
)

res_long <- res_all %>%
  transmute(
    Group, Variable, Genus,
    coef = rho,
    P = p_raw,
    q = p_FDR
  )


font_add(
  family = "Times New Roman",
  regular = "C:/Windows/Fonts/times.ttf",
  bold = "C:/Windows/Fonts/timesbd.ttf",
  italic = "C:/Windows/Fonts/timesi.ttf",
  bolditalic = "C:/Windows/Fonts/timesbi.ttf"
)
showtext_auto(TRUE)


plot_df <- res_long %>%
  mutate(
    Genus = as.character(Genus),   # 关键：先确保是 character
    P_plot = ifelse(is.na(P), NA, ifelse(P <= 1e-300, 1e-300, P)),
    logP = -log10(P_plot),
    Significance = case_when(
      P < 0.001 ~ "***",
      P < 0.01  ~ "**",
      P < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Var_Group = paste0(Variable, "\n", Group)
  )

genus_order_use <- intersect(genus_order, unique(plot_df$Genus))
plot_df$Genus <- factor(plot_df$Genus, levels = rev(genus_order_use))

plot_df$Var_Group <- factor(
  plot_df$Var_Group,
  levels = unlist(lapply(variables, function(v) {
    paste0(v, "\n", c("Total", "High_SII", "Low_SII"))
  }))
)

# Added p for permutation manual
write_xlsx(
  plot_df,
  path = "plot_df_Spearman_results2.xlsx"
)

plot_df <- read_excel("plot_df_Spearman_results3.xlsx")


var_group_levels <- as.vector(outer(
  variables,
  c("Total", "Low_SII", "High_SII"),
  paste, sep = "\n"
))
plot_df$Var_Group <- factor(plot_df$Var_Group, levels = var_group_levels)

plot_df$Genus <- factor(
  plot_df$Genus,
  levels = rev(genus_order)
)
genus_order_use <- intersect(genus_order, unique(plot_df$Genus))
plot_df$Genus <- factor(plot_df$Genus, levels = rev(genus_order_use))

plot_df$Var_Group <- factor(
  plot_df$Var_Group,
  levels = unlist(lapply(variables, function(v) {
    paste0(v, "\n", c("Total", "High_SII", "Low_SII"))
  }))
)

p <- ggplot() +
  
  geom_point(
    data = plot_df %>% filter(!is.na(coef)),
    aes(x = Var_Group, y = Genus, size = logP, color = coef)
  ) +
  
  geom_point(
    data = plot_df %>% filter(is.na(coef)),
    aes(x = Var_Group, y = Genus),
    shape = 21,
    size = 4,
    stroke = 0.8,
    color = "grey40",
    fill = "white"
  ) +
  
  geom_text(
    data = plot_df %>% filter(!is.na(coef)),
    aes(x = Var_Group, y = Genus, label = Significance),
    family = "Times New Roman",
    fontface = "bold",
    size = 5
  ) +
  
  scale_color_gradient2(
    low = "#3B4CC0", mid = "white", high = "#B40426",
    midpoint = 0, limits = c(-0.5, 0.5),
    name = "Spearman R"
  ) +
  
  scale_size_continuous(range = c(2, 12), name = expression(-log[10](P))) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 1,
                               family = "Times New Roman", color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold.italic",
                               family = "Times New Roman", color = "black"),
    text = element_text(family = "Times New Roman")
  ) +
  labs(x = NULL, y = "Genus")


ggsave(
  "Bubble_Spearman_Pvalue_SII_Microbiota.pdf",
  plot = p,
  width = 13,
  height = 10
)
