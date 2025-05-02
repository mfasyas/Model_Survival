setwd("F:\\Coding\\R\\Model_Survival")
getwd()
library(survival)
library(survminer)
library(dplyr)

data <- read.csv("Data_Project_Modsur2.csv")
View(df)

df <- data[
    c(
        "tenure",
         "Churn",
         "Contract",
         "InternetService",
         "SeniorCitizen",
         "PaymentMethod"
    )
]

head(df)

# Diambil kolom tenure, Churn, Contract, InternetService, SeniorCitizen, dan PaymentMethod
# Ini berdasarkan informasi dari instruksi Project 2
# Berdasarkan data dan deskripsi yang diobservasi di sini adalah status churn (1 ketika tidak aktif, 0 ketika aktif/censored)

# Nomor 1
# Encoding label Contract

df$ContractEncode <- as.integer(factor(df$Contract))

# Month-to-month = 1, One year = 2, Two year = 3

df$Churn <- ifelse(df$Churn == "True", 1, 0)

dfcurve0 <- survfit(Surv(tenure, Churn)~ContractEncode, data = df)

summary(dfcurve0)

ggsurvplot(
  dfcurve0,
  size = 1,
  palette = c("#E7B800", "#2E9FDF","#2b1101"),
  conf.int = FALSE,
  pval = FALSE,
  legend.labs = c("Month", "One Year", "Two Year"),
  ggtheme = theme_classic(),
  risk.table = FALSE,
  censor = FALSE
)
