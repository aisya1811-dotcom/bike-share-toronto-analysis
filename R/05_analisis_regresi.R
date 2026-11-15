# =====================================================================
# SOAL 6 - REGRESSION ANALYSIS (35 poin)
# Data   : data_final_soal3.rds
# Y      : durasi perjalanan (menit)  = trip_duration_seconds / 60
# Aturan : perjalanan > 24 jam dikeluarkan (sama seperti Soal 5)
# [SARAN TAMBAHAN] = bukan ketentuan soal
# =====================================================================
rm(list = ls())
# setwd("~/Documents/Coolyeah/Semester 5/Business Intelligence/UTS/Data")  # aktifkan jika perlu
options(scipen = 999)
library(ggplot2)
dir.create("Hasil_Regresi", showWarnings = FALSE)

data_final <- readRDS("data_final_soal3.rds")
stopifnot(all(c("durasi_menit", "jam", "bulan", "tahun", "kategori_hari",
                "pulang_pergi", "user_type", "durasi_lebih_24_jam") %in% names(data_final)))

d0 <- subset(data_final, durasi_lebih_24_jam == FALSE)
dr <- data.frame(
  durasi      = d0$durasi_menit,
  jam         = as.numeric(d0$jam),
  bulan       = as.numeric(d0$bulan),
  tahun       = as.numeric(d0$tahun),
  akhir_pekan = as.numeric(d0$kategori_hari == "Akhir pekan"),
  casual      = as.numeric(d0$user_type == "Casual Member"),
  pulang_pergi = ifelse(is.na(d0$pulang_pergi), NA, as.numeric(d0$pulang_pergi == "Ya")))
rm(data_final, d0); invisible(gc())
n_awal <- nrow(dr)
dr <- dr[complete.cases(dr), ]            # buang baris yang nama stasiunnya kosong (pulang_pergi NA)
cat("Observasi setelah >24 jam dikeluarkan:", n_awal, "\n")
cat("Observasi dipakai di regresi (lengkap):", nrow(dr), "\n\n")
dr$log_durasi <- log(dr$durasi)

# ---- 6a. Model ---------------------------------------------------------
# durasi = b0 + b1*jam + b2*bulan + b3*tahun + b4*akhir_pekan + b5*casual + b6*pulang_pergi + e
cat("===== 6a-6c. MODEL REGRESI LINEAR BERGANDA =====\n")
model <- lm(durasi ~ jam + bulan + tahun + akhir_pekan + casual + pulang_pergi, data = dr, model = FALSE)
ringkas <- summary(model)
print(ringkas)

# ---- 6b. Persamaan -----------------------------------------------------
b <- coef(model)
cat("\n===== 6b. PERSAMAAN MODEL =====\n")
cat(sprintf("Durasi_hat = %.4f %+.4f*jam %+.4f*bulan %+.4f*tahun %+.4f*akhir_pekan %+.4f*casual %+.4f*pulang_pergi\n",
            b[1], b[2], b[3], b[4], b[5], b[6], b[7]))

# ---- 6c. Tabel estimasi parameter -------------------------------------
ci <- confint.default(model)               # selang kepercayaan 95% (pendekatan normal; n sangat besar)
tab_koef <- data.frame(
  Variabel   = names(b),
  Estimasi   = round(b, 4),
  Std_Error  = round(ringkas$coefficients[, 2], 4),
  t_hitung   = round(ringkas$coefficients[, 3], 2),
  p_value    = ifelse(ringkas$coefficients[, 4] < 2.2e-16, "< 2.2e-16",
                      format(signif(ringkas$coefficients[, 4], 3), scientific = FALSE)),
  CI95_bawah = round(ci[, 1], 4),
  CI95_atas  = round(ci[, 2], 4),
  Keputusan  = ifelse(ringkas$coefficients[, 4] < 0.05, "Tolak H0", "Gagal tolak H0"),
  row.names = NULL)
cat("\nTabel parameter (uji t parsial, H0: beta_j = 0, alpha = 5%):\n")
print(tab_koef, row.names = FALSE)

# ---- 6d. Uji F simultan ------------------------------------------------
fs <- ringkas$fstatistic
p_F <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
cat("\n===== 6d. UJI F SIMULTAN =====\n")
cat("H0: semua koefisien X = 0 ; H1: minimal satu koefisien != 0 ; alpha = 5%\n")
cat(sprintf("F hitung = %.2f ; db1 = %d ; db2 = %d ; p-value %s\n", fs[1], fs[2], fs[3],
            ifelse(p_F < 2.2e-16, "< 2.2e-16", format(signif(p_F, 3), scientific = FALSE))))
cat("Keputusan:", ifelse(p_F < 0.05, "Tolak H0", "Gagal tolak H0"), "\n")
cat(sprintf("R2 = %.4f ; R2 adjusted = %.4f ; Residual Std. Error = %.4f menit\n",
            ringkas$r.squared, ringkas$adj.r.squared, ringkas$sigma))

# ---- 6h. Asumsi regresi ------------------------------------------------
cat("\n===== 6h. PEMERIKSAAN ASUMSI =====\n")
res  <- resid(model)
fit  <- fitted(model)
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
kurt <- function(x) mean((x - mean(x))^4) / sd(x)^4
set.seed(123)
idx5k <- sample(length(res), 5000)
idxg  <- sample(length(res), 20000)         # sampel untuk grafik
idxbp <- sample(length(res), 100000)        # sampel untuk uji Breusch-Pagan

# 1) Normalitas residual
cat("\n(1) Normalitas residual\n")
cat(sprintf("Skewness residual = %.3f ; Kurtosis = %.3f (normal: 0 dan 3)\n", skew(res), kurt(res)))
print(shapiro.test(res[idx5k]))
png("Hasil_Regresi/qq_residual.png", 900, 800, res = 150)
qqnorm(res[idx5k], main = "Q-Q Plot Residual (sampel 5.000)"); qqline(res[idx5k], col = "red")
dev.off()

# 2) Homoskedastisitas: Breusch-Pagan manual pada sampel 100.000
cat("\n(2) Homoskedastisitas (Breusch-Pagan, sampel 100.000)\n")
aux <- lm(I(res[idxbp]^2) ~ jam + bulan + tahun + akhir_pekan + casual + pulang_pergi, data = dr[idxbp, ])
LM  <- length(idxbp) * summary(aux)$r.squared
p_bp <- pchisq(LM, df = 6, lower.tail = FALSE)
cat(sprintf("BP = %.2f ; db = 6 ; p-value %s\n", LM,
            ifelse(p_bp < 2.2e-16, "< 2.2e-16", format(signif(p_bp, 3), scientific = FALSE))))
g1 <- ggplot(data.frame(fit = fit[idxg], res = res[idxg]), aes(fit, res)) +
  geom_point(alpha = .25) + geom_hline(yintercept = 0, colour = "red") +
  labs(title = "Residual vs Fitted (sampel 20.000)", x = "Nilai prediksi (menit)", y = "Residual") +
  theme_minimal()
ggsave("Hasil_Regresi/residual_vs_fitted.png", g1, width = 8, height = 5, dpi = 200)

# 3) Multikolinearitas: VIF dari invers matriks korelasi X
cat("\n(3) Multikolinearitas (VIF; > 10 mengindikasikan masalah)\n")
X <- dr[, c("jam", "bulan", "tahun", "akhir_pekan", "casual", "pulang_pergi")]
vif <- diag(solve(cor(X)))
print(round(vif, 3))

# 4) Pencilan / residual baku besar
cat("\n(4) Residual baku |e/sigma| > 3\n")
rb <- res / ringkas$sigma
cat(sprintf("Jumlah = %d (%.2f%% dari %d observasi)\n", sum(abs(rb) > 3),
            mean(abs(rb) > 3) * 100, length(rb)))

# 5) Linearitas: lihat residual vs fitted dan rata-rata durasi per jam (Soal 5)
# 6) Independensi: data adalah perjalanan individual, bukan deret waktu berurutan;
#    uji Durbin-Watson tidak bermakna di sini -> dinyatakan sebagai keterbatasan.

# Catatan: model log(durasi) di bawah hanya pemeriksaan tambahan dan tidak dimasukkan ke laporan
# ---- [SARAN TAMBAHAN] model dengan log(durasi) -------------------------
cat("\n\n===== [SARAN TAMBAHAN] MODEL log(durasi) =====\n")
model_log <- lm(log_durasi ~ jam + bulan + tahun + akhir_pekan + casual + pulang_pergi, data = dr, model = FALSE)
rl <- summary(model_log)
print(rl)
bl <- coef(model_log)
tab_log <- data.frame(Variabel = names(bl), Estimasi = round(bl, 4),
                      Persen_perubahan_durasi = round((exp(bl) - 1) * 100, 2),
                      t_hitung = round(rl$coefficients[, 3], 2),
                      p_value = ifelse(rl$coefficients[, 4] < 2.2e-16, "< 2.2e-16",
                                       format(signif(rl$coefficients[, 4], 3), scientific = FALSE)),
                      row.names = NULL)
print(tab_log, row.names = FALSE)
fl <- rl$fstatistic
cat(sprintf("F = %.2f (db %d ; %d) ; R2 = %.4f ; R2 adj = %.4f\n", fl[1], fl[2], fl[3],
            rl$r.squared, rl$adj.r.squared))
rsl <- resid(model_log)
cat(sprintf("Skewness residual = %.3f ; Kurtosis = %.3f\n", skew(rsl), kurt(rsl)))
print(shapiro.test(rsl[idx5k]))
png("Hasil_Regresi/qq_residual_log.png", 900, 800, res = 150)
qqnorm(rsl[idx5k], main = "Q-Q Plot Residual model log(durasi)"); qqline(rsl[idx5k], col = "red")
dev.off()
aux2 <- lm(I(rsl[idxbp]^2) ~ jam + bulan + tahun + akhir_pekan + casual + pulang_pergi, data = dr[idxbp, ])
LM2 <- length(idxbp) * summary(aux2)$r.squared
cat(sprintf("Breusch-Pagan (log) = %.2f ; p-value %s\n", LM2,
            ifelse(pchisq(LM2, 6, lower.tail = FALSE) < 2.2e-16, "< 2.2e-16",
                   format(signif(pchisq(LM2, 6, lower.tail = FALSE), 3), scientific = FALSE))))
g2 <- ggplot(data.frame(fit = fitted(model_log)[idxg], res = rsl[idxg]), aes(fit, res)) +
  geom_point(alpha = .25) + geom_hline(yintercept = 0, colour = "red") +
  labs(title = "Residual vs Fitted model log(durasi) (sampel 20.000)", x = "Prediksi log(durasi)", y = "Residual") +
  theme_minimal()
ggsave("Hasil_Regresi/residual_vs_fitted_log.png", g2, width = 8, height = 5, dpi = 200)

# ---- Simpan ------------------------------------------------------------
write.csv(tab_koef, "Hasil_Regresi/tabel_koefisien.csv", row.names = FALSE)
write.csv(tab_log, "Hasil_Regresi/tabel_koefisien_log.csv", row.names = FALSE)
write.csv(data.frame(Variabel = names(vif), VIF = round(vif, 3)), "Hasil_Regresi/vif.csv", row.names = FALSE)
cat("\nSelesai. Hasil ada di folder Hasil_Regresi.\n")
