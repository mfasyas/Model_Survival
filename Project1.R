library(survival)
library(survminer)
library(dplyr)


setwd("F:\\Coding\\R\\Model_Survival")
df <- read.csv("Data_Project_Modsur1.csv")

dfcurve <- survfit(Surv(time, censor)~1, data = df)

ggsurvplot(
  dfcurve,
  size = 1,
  palette = c("#E7B800", "#2E9FDF"),
  conf.int = FALSE,
  pval = FALSE,
  legend.labs = c("Non-Grouped"),
  ggtheme = theme_classic(),
  risk.table = TRUE,
  censor = FALSE
)

summary(dfcurve)

#==========================================================
dfcurve2 <- survfit(Surv(time, censor)~group, data = df)

ggsurvplot(
  dfcurve2,
  size = 1,
  palette = c("#E7B800", "#2E9FDF"),
  conf.int = FALSE,
  pval = FALSE,
  legend.labs = c("Plasebo", "Chemotherapy"),
  ggtheme = theme_classic(),
  risk.table = TRUE,
  censor = FALSE
)

summary(dfcurve2)

#==========================================================
dfcurve3 <- survfit(Surv(time, censor)~number, data = df)

ggsurvplot(
  dfcurve3,
  size = 1,
  palette = c("#E7B800", "#2E9FDF"),
  conf.int = FALSE,
  pval = FALSE,
  legend.labs = c("1 Tumor", "More than 1 Tumor"),
  ggtheme = theme_classic(),
  risk.table = TRUE,
  censor = FALSE
)

summary(dfcurve3)

#==========================================================
df2 <- df

df2$newgroup <- ifelse(df$number == 1 & df$group == 1, 1,
                        ifelse(df$number == 2 & df$group == 1, 2,
                               ifelse(df$number == 1 & df$group == 2, 3,
                                      ifelse(df$number == 2 & df$group == 2, 4, NA))))

View(df2)

dfcurve4 <- survfit(Surv(time, censor)~newgroup, data=df2)

ggsurvplot(
  dfcurve4,
  size = 1,
  palette = c("#E7B800", "#2E9FDF", "#323036", "#de3a3a"),
  conf.int = FALSE,
  pval = FALSE,
  legend.labs = c("Plasebo with 1 Tumor", "Plasebo with 2 or More Tumor",
                  "Chemotherapy with 1 Tumor", "Chemotherapy with 2 or more Tumor"),
  ggtheme = theme_classic(),
  risk.table = TRUE,
  censor = FALSE
)

summary(dfcurve4)











