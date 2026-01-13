
rm(list = ls())

library(readxl)
library(dplyr)

# ===== 1) 读取数据（隐藏路径可自行改为相对路径） =====
dt <- read_excel("PISA-R.xlsx") %>% as.data.frame()

# ---- 2) 分组变量（保持你原逻辑）----
group_var <- if ("log2SII_3group2_2" %in% names(dt)) "log2SII_3group2_2" else "log2SII_3group2"
dt[[group_var]] <- as.factor(dt[[group_var]])

SII1 <- dt[dt[[group_var]] == 0, , drop = FALSE]
SII2 <- dt[dt[[group_var]] == 1, , drop = FALSE]
overall <- dt

cat("Low_SII (SII1) n =", nrow(SII1), "\n")
cat("High_SII (SII2) n =", nrow(SII2), "\n")

# ---- 3) 变量列表（保持不变）----
continuous_vars <- c("PI", "BMI", "age", "BOP", "PESA", "PISA", "PD5", "SII", "PD")

categorical_vars <- c("Alcohol_consumption",
                       "Interval_since_the_most_recent_supragingival_scaling",
                       "gender", "stage2")

# ---- 4) 格式化函数 ----
fmt_mean_sd <- function(x) {
  sprintf("%.2f ± %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

fmt_n_pct <- function(counts) {
  pct <- round(100 * counts / sum(counts), 1)
  paste0(counts, " (", pct, "%)")
}

# ---- 5) 连续变量：展示统一 mean±SD，但P值按正态性切换 ----
test_continuous_meanSD_switchP <- function(varname) {
  
  x_all <- overall[[varname]]
  x1 <- SII1[[varname]]
  x2 <- SII2[[varname]]
  
  x_all2 <- x_all[!is.na(x_all)]
  shapiro_p <- if (length(x_all2) >= 3) shapiro.test(x_all2)$p.value else NA
  
  overall_show <- fmt_mean_sd(x_all)
  low_show     <- fmt_mean_sd(x1)
  high_show    <- fmt_mean_sd(x2)
  
  if (!is.na(shapiro_p) && shapiro_p > 0.05) {
    test <- t.test(dt[[varname]] ~ dt[[group_var]])
    method <- "t-test"
  } else {
    test <- wilcox.test(dt[[varname]] ~ dt[[group_var]])
    method <- "Wilcoxon rank-sum"
  }
  
  data.frame(
    Variable = varname,
    Overall  = overall_show,
    Low_SII  = low_show,
    High_SII = high_show,
    P_value  = test$p.value,
    Test     = method,
    stringsAsFactors = FALSE
  )
}

# ---- 6) 分类变量：n (%) + Chi-square / Fisher ----
test_categorical_table1 <- function(varname) {
  
  tab <- table(dt[[varname]], dt[[group_var]])
  tab_overall <- table(dt[[varname]])
  
  # 兼容列名不完整的情况
  low_col  <- if ("0" %in% colnames(tab)) tab[, "0"] else rep(0, nrow(tab))
  high_col <- if ("1" %in% colnames(tab)) tab[, "1"] else rep(0, nrow(tab))
  
  overall_fmt <- paste0(tab_overall, " (", round(100 * tab_overall / sum(tab_overall), 1), "%)")
  low_fmt  <- fmt_n_pct(low_col)
  high_fmt <- fmt_n_pct(high_col)
  
  chi <- suppressWarnings(chisq.test(tab))
  if (any(chi$expected < 5)) {
    test <- fisher.test(tab)
    method <- "Fisher exact"
  } else {
    test <- chi
    method <- "Chi-square"
  }
  
  data.frame(
    Variable = paste0(varname, " - ", rownames(tab)),
    Overall  = overall_fmt,
    Low_SII  = low_fmt,
    High_SII = high_fmt,
    P_value  = test$p.value,
    Test     = method,
    stringsAsFactors = FALSE
  )
}

# ---- 7) 生成 Table1_meanSD 并导出 ----
table1_cont_meanSD <- do.call(rbind, lapply(continuous_vars, test_continuous_meanSD_switchP))
table1_cat         <- do.call(rbind, lapply(categorical_vars, test_categorical_table1))

Table1_meanSD <- rbind(table1_cont_meanSD, table1_cat)

Table1_meanSD$P_value <- ifelse(Table1_meanSD$P_value < 0.001, "<0.001",
                                sprintf("%.3f", Table1_meanSD$P_value))

write.csv(Table1_meanSD, "Table1_LowSII_vs_HighSII_meanSD.csv", row.names = FALSE)
print(Table1_meanSD)
