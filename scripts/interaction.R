# 清空环境
rm(list = ls())

# 导入包
library(Hmisc)
library(rms)
library(ggplot2)
library(scales)
library(ggrcs)
library(ggplot2)
library(splines)
library(foreign)
library(readxl)
library(rcssci)


dt <- read_excel("allgenus-R.xlsx", sheet = "genuspercentageasv")
dt$gender <- as.factor(dt$gender)
dt$Type_new <- as.factor(dt$Type_new)
dt$SII_2group <- as.factor(dt$SII_2group)
dd <- datadist(dt)
options(datadist = "dd")

fit <- ols(PISA ~ rcs(log2SII) * Type_new  + age + gender, data = dt)
summary(fit)
anova(fit)
an<-anova(fit)

write.csv(as.data.frame(an), file = "anova_type-SII.csv", row.names = TRUE)


