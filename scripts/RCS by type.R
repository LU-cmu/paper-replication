# 清空环境
rm(list = ls())

# 导入包
library(Hmisc)
library(rms)
library(ggplot2)
library(scales)
library(splines)
library(readxl)


PISA_R <- read_excel("PISA-R.xlsx")

dt <- PISA_R
dd<-datadist(dt)
options(datadist='dd')

dt$gender <-as.factor(dt$gender)
dt$log2PISA_group2 <-as.factor(dt$log2PISA_group2)

dt$Type_new <- as.integer(dt$Type_new)

dd <- datadist(dt)
options(datadist = "dd")

dt1 <- subset(dt, Type_new == 1)  # Type-Porphyromonas
dt2 <- subset(dt, Type_new == 2)  # Type-Capnocytophaga


fit3 <- ols(PISA ~ rcs(log2SII, 3)  + age + gender, data = dt1)
fit4 <- ols(PISA ~ rcs(log2SII, 4) + age + gender, data = dt1)
fit5 <- ols(PISA ~ rcs(log2SII, 5) + age + gender, data = dt1)

fit33 <- ols(PISA ~ rcs(log2SII, 3)  + age + gender, data = dt2)
fit44 <- ols(PISA ~ rcs(log2SII, 4) + age + gender, data = dt2)
fit55 <- ols(PISA ~ rcs(log2SII, 5) + age + gender, data = dt2)

AIC(fit3)
AIC(fit4)
AIC(fit5)

AIC(fit33)
AIC(fit44)
AIC(fit55)

fit1 <- ols(PISA ~ rcs(log2SII, 3)+ age + gender, data = dt1)
fit2 <- ols(PISA ~ rcs(log2SII, 4)+ age + gender, data = dt2)


an1<- anova(fit1)
an2<- anova(fit2)

x_grid <- seq(7.8, 10.2,length.out = 200)

pred1 <- as.data.frame(Predict(fit1, log2SII = x_grid, conf.int = 0.95))
pred2 <- as.data.frame(Predict(fit2, log2SII = x_grid, conf.int = 0.95))

pred1$Group <- "Type-Porphyromonas"
pred2$Group <- "Type-Capnocytophaga"

pred_df <- rbind(pred1, pred2)

p <- ggplot(pred_df, aes(x = log2SII, y = yhat, color = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Relationship between SII and PISA by microbial type
    (adjusted for age and gender)",
    x = expression(log[2]*SII),
    y = expression("Predicted PISA ("*mm^2*")"),
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(0.1, "cm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18,
                              margin = margin(b = 20)),
      legend.position = c(0.87, 0.95),
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.7, "cm"),
      legend.spacing.x = unit(0.4, "cm")
  ) +
  scale_color_manual(
    values = c(
      "Type-Porphyromonas" = "#2B7A78",
      "Type-Capnocytophaga" = "#D65A31"
    )
  )+
  scale_fill_manual(
    values = c(
      "Type-Porphyromonas" = "#9FD3C7",
      "Type-Capnocytophaga" = "#F4A58A"
    )
  )+

  scale_y_continuous(breaks = seq(0, 6000, by = 2000))

p <- p +
  
geom_vline(xintercept = c(9.14), linetype = "dashed", color = "red", linewidth = 0.5) +
  
  annotate("text", x = 9.15, y = 5500,
           label = "log[2]*SII == 9.14",
           parse = TRUE, size = 6, color = "black", hjust = -0.05)
print(p)

ggsave("PISA_Prediction_Plot-noadjusted-byType_new.pdf",
       plot = p, width = 9, height = 8, dpi = 300)
