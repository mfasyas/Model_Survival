# Pastikan memuat library yang diperlukan
library(survival)
library(survminer)

# Baca data
telco <- read.csv("Data_Project_Modsur2_Cleaned.csv")

# Periksa kolom terkait untuk memastikan tidak ada yang hilang
table(telco$Contract)
table(telco$SeniorCitizen)

# Ubah SeniorCitizen menjadi faktor
telco$SeniorCitizen <- factor(telco$SeniorCitizen, labels = c("Not Senior", "Senior"))

# Buat objek Surv
surv_obj <- Surv(time = telco$tenure, event = telco$Churn)

# Fit model survival dengan stratifikasi berdasarkan SeniorCitizen
fit_strat <- survfit(surv_obj ~ Contract + strata(SeniorCitizen), data = telco)

# Plot Kaplan-Meier
ggsurvplot(fit_strat,
           data = telco,
           pval = TRUE,
           facet.by = "SeniorCitizen",  # Pisahkan berdasarkan SeniorCitizen
           legend.title = "Contract Type",
           xlab = "Tenure (Months)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Curves by Contract Type, Stratified by Senior Citizen Status",
           palette = "Set1",
           risk.table = TRUE)

# Uji Stratifikasi Log-Rank
logrank_strat <- survdiff(surv_obj ~ Contract + strata(SeniorCitizen), data = telco)
print(logrank_strat)

