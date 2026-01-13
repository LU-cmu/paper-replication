rm(list = ls())

library(Hmisc)
library(rms)
library(ggplot2)
library(scales)
library(ggrcs)
library(splines)
library(foreign)
library(readxl)
library(rcssci)

PISA_R <- read_excel("PISA-R.xlsx")

dt <- PISA_R
dd<-datadist(dt)
options(datadist='dd')

dt$gender <-as.factor(dt$gender)
dt$log2PISA_group2 <-as.factor(dt$log2PISA_group2)

mod1 = fit<-ols(PISA ~rcs(log2SII, 3)+age+gender, data=dt)
mod2 = fit<-ols(PISA ~rcs(log2SII, 4)+age+gender, data=dt)
mod3 = fit<-ols(PISA ~rcs(log2SII, 5)+age+gender, data=dt)

AIC(mod1)
AIC(mod2)
AIC(mod3)#aic Min

# RCS模型
fit <- ols(PISA ~ rcs(log2SII, 4) +age+gender, data=dt)
summary(fit)
an<-anova(fit)
pred <- Predict(fit, log2SII, conf.int=0.95)
pred_df <- as.data.frame(pred)

xmin <- min(pred_df$log2SII, na.rm = TRUE)
xmax <- max(pred_df$log2SII, na.rm = TRUE)

dt_clip <- dt %>% 
  filter(log2SII >= xmin & log2SII <= xmax)



p <- ggplot() +
  
  geom_point(data = dt_clip,
             aes(x = log2SII, y = PISA),
             alpha = 0.5, size = 2, color = "grey40") +
  
  geom_line(data = pred_df,
            aes(x = log2SII, y = yhat),
            color = "#FF7A5C", linewidth = 1.2) +
  
  geom_ribbon(data = pred_df,
              aes(x = log2SII, ymin = lower, ymax = upper),
              fill = "#FFDAB9", alpha = 0.4) +
  
  labs(
    title = "Relationship between SII and PISA (adjusted for age and gender)",
    x = expression(log[2]*SII),
    y = expression(PISA~"(" * mm^2 * ")")
  ) +
  
  theme_minimal(base_size = 22) +
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
    plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey40")
  ) +
  
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  
  geom_vline(xintercept = c(8.48, 9.19), linetype = "dashed", color = "red", linewidth = 0.5) +
  
  annotate("text", x = 9.4, y = 4000,
           label = "italic(P)~'(nonlinear)'*' = 0.005'",
           parse = TRUE, size = 6, color = "black", hjust = 0.15) +
  annotate("text", x = 8.48, y = 3600,
           label = "log[2]*SII == 8.48",
           parse = TRUE, size = 6, color = "black", hjust = -0.03) +
  annotate("text", x = 9.19, y = 3600,
           label = "log[2]*SII == 9.19",
           parse = TRUE, size = 6, color = "black", hjust = -0.05)


ggsave("PISA_Prediction_Plot-adjusted-with-scatter.pdf", plot = p, width = 9, height = 8, dpi = 300)
