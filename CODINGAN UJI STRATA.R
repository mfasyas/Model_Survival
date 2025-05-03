# -----------------------------------------
# 1. Load library
# -----------------------------------------
library(survival)

# -----------------------------------------
# 2. Load data
# -----------------------------------------
df <- read.csv("Data_Project_Modsur2_Cleaned.csv")

# -----------------------------------------
# 3. Pastikan format variabel sudah sesuai
# -----------------------------------------
df$Contract <- factor(df$Contract,
                      levels = c("Month-to-month", "One year", "Two year"))

df$SeniorCitizen <- factor(df$SeniorCitizen,
                           levels = c(0, 1),
                           labels = c("Non-Lansia", "Lansia"))

df$Churn <- as.numeric(df$Churn)  # 1 = churn, 0 = aktif (censored)

# -----------------------------------------
# 4. Buat objek survival
# -----------------------------------------
surv_obj <- Surv(time = df$tenure, event = df$Churn)

# -----------------------------------------
# 5. Uji Stratifikasi Log-Rank
# -----------------------------------------
logrank_strat <- survdiff(surv_obj ~ Contract + strata(SeniorCitizen), data = df)

# Tampilkan hasil uji
print(logrank_strat)

# Hitung p-value secara manual
pval <- 1 - pchisq(logrank_strat$chisq, df = as.numeric(logrank_strat$df))
cat("P-value:", pval, "\n")

# -----------------------------------------
# 6. Visualisasi Kurva Kaplan-Meier 
# -----------------------------------------
km_fit <- survfit(surv_obj ~ Contract, data = df)

plot(km_fit, col = c("blue", "orange", "green"),
     xlab = "Tenure (bulan)",
     ylab = "Probabilitas Bertahan",
     main = "Kaplan-Meier Berdasarkan Jenis Kontrak")
legend("bottomleft", legend = levels(df$Contract), col = c("blue", "orange", "green"), lty = 1)
