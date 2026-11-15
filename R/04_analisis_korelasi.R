# =====================================================================
# SOAL 5 - CORRELATION ANALYSIS (25 poin)
# Aturan : perjalanan > 24 jam DIKELUARKAN (sudah ditandai di Soal 2)
# =====================================================================
rm(list = ls())
# Sesuaikan lokasi folder data dengan komputer masing-masing, lalu aktifkan baris di bawah
# setwd("~/Documents/Coolyeah/Semester 5/Business Intelligence/UTS/Data")
options(scipen = 999)
library(ggplot2)
dir.create("Hasil_Korelasi", showWarnings = FALSE)

data_final <- readRDS("data_final_soal3.rds")

# ---- 5.0 Persiapan data -----------------------------------------------
perlu <- c("durasi_menit", "jam", "hari", "bulan", "tahun", "kategori_hari",
           "pulang_pergi", "user_type", "start_station_name",
           "durasi_lebih_24_jam")
stopifnot(all(perlu %in% names(data_final)))

d <- subset(data_final, durasi_lebih_24_jam == FALSE)
cat("Observasi sebelum dikeluarkan >24 jam:", nrow(data_final), "\n")
cat("Observasi dipakai untuk korelasi      :", nrow(d), "\n")
cat("Dikeluarkan                           :", nrow(data_final) - nrow(d), "\n\n")

# Variabel numerik / dummy (0-1) untuk korelasi
d$jam_num         <- as.numeric(d$jam)
d$bulan_num       <- as.numeric(d$bulan)                     # 1 = Jan ... 12 = Des
d$akhir_pekan     <- as.numeric(d$kategori_hari == "Akhir pekan")
d$casual          <- as.numeric(d$user_type == "Casual Member")
d$pulang_pergi_01 <- ifelse(is.na(d$pulang_pergi), NA, as.numeric(d$pulang_pergi == "Ya"))
d$tahun_num       <- as.numeric(d$tahun)
d$log_durasi      <- log(d$durasi_menit)                      # untuk [SARAN TAMBAHAN]

# ---- Fungsi uji korelasi ----------------------------------------------
# H0: rho = 0 ; H1: rho != 0 ; alpha = 5%
# Statistik uji: t = r*sqrt((n-2)/(1-r^2)), db = n-2
uji_korelasi <- function(x, y, metode = "pearson", alpha = 0.05) {
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]; n <- length(x)
  r <- cor(x, y, method = metode)
  t_hit <- r * sqrt((n - 2) / (1 - r^2))
  p <- 2 * pt(-abs(t_hit), df = n - 2)
  kekuatan <- cut(abs(r), c(-Inf, .1, .3, .5, .7, Inf),
                  labels = c("sangat lemah", "lemah", "sedang", "kuat", "sangat kuat"))
  data.frame(Metode = metode, N = n, Koefisien = round(r, 4),
             t_hitung = round(t_hit, 2),
             p_value = ifelse(p < 2.2e-16, "< 2.2e-16", format(signif(p, 3), scientific = FALSE)),
             Keputusan = ifelse(p < alpha, "Tolak H0", "Gagal tolak H0"),
             Arah = ifelse(r > 0, "positif", "negatif"),
             Kekuatan = as.character(kekuatan))
}
# Menjalankan Pearson DAN Spearman sekaligus
dua_metode <- function(x, y, label) {
  hasil <- rbind(uji_korelasi(x, y, "pearson"), uji_korelasi(x, y, "spearman"))
  cbind(Pasangan = label, hasil)
}

# =====================================================================
# 5e (dikerjakan lebih dulu): PEMERIKSAAN ASUMSI PEARSON
# =====================================================================
cat("===== 5e. PEMERIKSAAN ASUMSI PEARSON =====\n")
set.seed(123)
samp <- d[sample(nrow(d), 1000000), ]       # sampel hanya untuk grafik
skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }

asumsi <- data.frame(
  Variabel = c("durasi_menit", "log(durasi_menit)"),
  Skewness = round(c(skew(d$durasi_menit), skew(d$log_durasi)), 3),
  Mean     = round(c(mean(d$durasi_menit), mean(d$log_durasi)), 3),
  Median   = round(c(median(d$durasi_menit), median(d$log_durasi)), 3))
print(asumsi, row.names = FALSE)

# Shapiro-Wilk hanya bisa untuk n <= 5000 -> sampel 5000 (n besar membuat uji selalu menolak)
set.seed(123)
s5000 <- d$durasi_menit[sample(nrow(d), 5000)]
cat("\nShapiro-Wilk durasi_menit (sampel 5000):\n")
print(shapiro.test(s5000))
cat("Shapiro-Wilk log(durasi_menit) (sampel 5000):\n")
print(shapiro.test(log(s5000)))

# Q-Q plot dan histogram
png("Hasil_Korelasi/qq_durasi.png", 1600, 800, res = 200)
par(mfrow = c(1, 2))
qqnorm(s5000, main = "Q-Q Plot Durasi (menit)"); qqline(s5000, col = "red")
qqnorm(log(s5000), main = "Q-Q Plot log(Durasi)"); qqline(log(s5000), col = "red")
dev.off()

# Linearitas: rata-rata durasi per jam (garis) -> apakah hubungan jam-durasi linear?
rata_jam <- aggregate(durasi_menit ~ jam_num, data = d, FUN = mean)
g <- ggplot(rata_jam, aes(jam_num, durasi_menit)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Rata-rata durasi per jam mulai", x = "Jam", y = "Rata-rata durasi (menit)") +
  theme_minimal()
ggsave("Hasil_Korelasi/linearitas_jam_durasi.png", g, width = 9, height = 5, dpi = 200)
print(rata_jam)

cat("\nKesimpulan asumsi: lihat skewness, Q-Q plot dan Shapiro. Jika durasi sangat\n",
    "menjulur ke kanan, normalitas tidak terpenuhi -> Spearman dipakai sebagai\n",
    "pembanding utama (berbasis peringkat, tidak mensyaratkan normalitas).\n\n")

# =====================================================================
# 5a-5d : UJI KORELASI UNTUK TIAP PERTANYAAN
# H0: rho = 0  vs  H1: rho != 0 ; alpha = 5%
# =====================================================================
cat("===== PERTANYAAN 1: Jam vs durasi =====\n")
t1 <- dua_metode(d$jam_num, d$durasi_menit, "jam mulai - durasi")
print(t1, row.names = FALSE)

cat("\n===== PERTANYAAN 2: Hari kerja vs akhir pekan =====\n")
cat("(akhir_pekan: 0 = hari kerja, 1 = akhir pekan; Pearson di sini = korelasi point-biserial)\n")
t2 <- dua_metode(d$akhir_pekan, d$durasi_menit, "akhir pekan - durasi")
print(t2, row.names = FALSE)
ring2 <- aggregate(durasi_menit ~ kategori_hari, data = d,
                   FUN = function(x) c(N = length(x), Mean = mean(x), Median = median(x), SD = sd(x)))
print(ring2)

cat("\n===== PERTANYAAN 3: Jarak vs durasi =====\n")
cat("KETERBATASAN: dataset TIDAK memiliki variabel jarak / koordinat stasiun.\n",
    "Alternatif (proxy): pulang_pergi = 1 bila stasiun awal = stasiun akhir\n",
    "(perjalanan melingkar, jarak antarstasiun = 0). Ini BUKAN jarak sebenarnya.\n")
t3 <- dua_metode(d$pulang_pergi_01, d$durasi_menit, "pulang-pergi - durasi")
print(t3, row.names = FALSE)

cat("\n===== PERTANYAAN 4: Jam dengan aktivitas tinggi =====\n")
akt <- as.data.frame(table(jam = d$jam_num), responseName = "jumlah")
akt$jam <- as.numeric(as.character(akt$jam))
akt$persen <- round(akt$jumlah / sum(akt$jumlah) * 100, 2)
akt$peringkat <- rank(-akt$jumlah)
print(akt[order(-akt$jumlah), ], row.names = FALSE)
med_jam <- aggregate(durasi_menit ~ jam_num, data = d, FUN = median)
akt <- merge(akt, med_jam, by.x = "jam", by.y = "jam_num")
cat("\nKorelasi jumlah perjalanan per jam vs median durasi per jam (n = 24 jam):\n")
t4 <- dua_metode(akt$jumlah, akt$durasi_menit, "jumlah perjalanan - median durasi (per jam)")
print(t4, row.names = FALSE)

cat("\n===== PERTANYAAN 5: Faktor yang berhubungan dengan durasi =====\n")
fakt <- list(
  "jam mulai"            = d$jam_num,
  "bulan"                = d$bulan_num,
  "tahun"                = d$tahun_num,
  "akhir pekan (1=ya)"   = d$akhir_pekan,
  "Casual Member (1=ya)" = d$casual,
  "pulang-pergi (1=ya)"  = d$pulang_pergi_01)
t5 <- do.call(rbind, lapply(names(fakt), function(k) dua_metode(fakt[[k]], d$durasi_menit, k)))
print(t5, row.names = FALSE)

# Matriks korelasi Spearman antarvariabel
mat <- d[, c("durasi_menit", "jam_num", "bulan_num", "tahun_num", "akhir_pekan", "casual", "pulang_pergi_01")]
mat_s <- round(cor(mat, method = "spearman", use = "pairwise.complete.obs"), 3)
cat("\nMatriks korelasi Spearman:\n"); print(mat_s)
mat_p <- round(cor(mat, method = "pearson", use = "pairwise.complete.obs"), 3)
cat("\nMatriks korelasi Pearson:\n"); print(mat_p)

cat("\n===== PERTANYAAN 6: Pola penggunaan berdasarkan waktu dan karakteristik stasiun =====\n")
cat("KETERBATASAN: dataset tidak memuat atribut stasiun (kapasitas, lokasi/koordinat).\n",
    "Alternatif: karakteristik stasiun diwakili oleh POPULARITAS (jumlah perjalanan yang\n",
    "berangkat dari stasiun tsb). Analisis per stasiun (nama), stasiun dengan >= 100 perjalanan.\n")
nm <- trimws(gsub("\\s+", " ", d$start_station_name))
d$nama_stasiun <- nm
dd <- d[!is.na(d$nama_stasiun), ]
st <- data.frame(
  jumlah    = as.numeric(tapply(dd$durasi_menit, dd$nama_stasiun, length)),
  med_durasi= as.numeric(tapply(dd$durasi_menit, dd$nama_stasiun, median)),
  rata_durasi = as.numeric(tapply(dd$durasi_menit, dd$nama_stasiun, mean)),
  pct_akhir_pekan = as.numeric(tapply(dd$akhir_pekan, dd$nama_stasiun, mean)) * 100,
  pct_casual = as.numeric(tapply(dd$casual, dd$nama_stasiun, mean)) * 100,
  rata_jam  = as.numeric(tapply(dd$jam_num, dd$nama_stasiun, mean)),
  row.names = sort(unique(dd$nama_stasiun)))
st <- st[st$jumlah >= 100, ]
cat("Jumlah stasiun dianalisis:", nrow(st), "\n")
t6 <- rbind(
  dua_metode(st$jumlah, st$med_durasi, "popularitas stasiun - median durasi"),
  dua_metode(st$jumlah, st$pct_akhir_pekan, "popularitas stasiun - % perjalanan akhir pekan"),
  dua_metode(st$jumlah, st$pct_casual, "popularitas stasiun - % Casual Member"),
  dua_metode(st$pct_casual, st$med_durasi, "% Casual Member - median durasi (stasiun)"))
print(t6, row.names = FALSE)

# Pola jam x kategori hari (counts) untuk melengkapi 'pola waktu'
tab_jam_hari <- table(d$jam_num, d$kategori_hari)
cat("\nJumlah perjalanan per jam x kategori hari:\n"); print(tab_jam_hari)

cat("\n===== PERTANYAAN 7: Annual vs Casual setelah mempertimbangkan waktu dan karakteristik =====\n")
cat("Dipakai KORELASI PARSIAL antara casual (1 = Casual) dan durasi, dengan mengontrol:\n",
    "jam mulai, akhir pekan, bulan, tahun, pulang-pergi.\n",
    "Cara: dihitung dari invers matriks korelasi (setara dengan korelasi residual).\n")
# Korelasi parsial lewat matriks korelasi: r_xy.z = -P12 / sqrt(P11*P22), P = invers matriks korelasi
parsial <- function(y, x, kontrol, metode = "pearson") {
  dat <- data.frame(y = y, x = x, kontrol)
  dat <- dat[complete.cases(dat), ]
  P <- solve(cor(dat, method = metode))
  r <- -P[1, 2] / sqrt(P[1, 1] * P[2, 2])
  n <- nrow(dat); k <- ncol(dat) - 2
  t_hit <- r * sqrt((n - 2 - k) / (1 - r^2))
  p <- 2 * pt(-abs(t_hit), df = n - 2 - k)
  data.frame(Metode = paste(metode, "(parsial)"), N = n, Koefisien = round(r, 4),
             t_hitung = round(t_hit, 2),
             p_value = ifelse(p < 2.2e-16, "< 2.2e-16", format(signif(p, 3), scientific = FALSE)),
             Keputusan = ifelse(p < 0.05, "Tolak H0", "Gagal tolak H0"))
}
kontrol <- data.frame(jam = d$jam_num, akhir_pekan = d$akhir_pekan, bulan = d$bulan_num,
                      tahun = d$tahun_num, pulang_pergi = d$pulang_pergi_01)
kol7 <- c("Pasangan", "Metode", "N", "Koefisien", "t_hitung", "p_value", "Keputusan")
t7_kasar <- dua_metode(d$casual, d$durasi_menit, "Casual - durasi (tanpa kontrol)")
print(t7_kasar, row.names = FALSE)
t7_p  <- cbind(Pasangan = "Casual - durasi (dengan kontrol)", parsial(d$durasi_menit, d$casual, kontrol, "pearson"))
t7_sp <- cbind(Pasangan = "Casual - durasi (dengan kontrol)", parsial(d$durasi_menit, d$casual, kontrol, "spearman"))
print(rbind(t7_p, t7_sp), row.names = FALSE)
cat("\n[SARAN TAMBAHAN] Korelasi parsial pada log(durasi) (lebih mendekati normal):\n")
t7_log <- cbind(Pasangan = "Casual - log(durasi) (dengan kontrol)", parsial(d$log_durasi, d$casual, kontrol, "pearson"))
print(t7_log, row.names = FALSE)

# ---- Visual pendukung --------------------------------------------------
g1 <- ggplot(samp, aes(x = factor(jam), y = durasi_menit)) +
  geom_boxplot(outlier.alpha = .05) + scale_y_log10() +
  labs(title = "Durasi per jam mulai (skala log)", x = "Jam", y = "Durasi (menit, log)") +
  theme_minimal()
ggsave("Hasil_Korelasi/boxplot_durasi_per_jam.png", g1, width = 10, height = 5, dpi = 200)

g2 <- ggplot(samp, aes(x = kategori_hari, y = durasi_menit)) +
  geom_boxplot(outlier.alpha = .05) + scale_y_log10() +
  labs(title = "Durasi: hari kerja vs akhir pekan (skala log)", x = "", y = "Durasi (menit, log)") +
  theme_minimal()
ggsave("Hasil_Korelasi/boxplot_durasi_kategori_hari.png", g2, width = 6, height = 5, dpi = 200)

g3 <- ggplot(akt, aes(jam, jumlah)) + geom_col() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Jumlah perjalanan per jam", x = "Jam", y = "Jumlah") + theme_minimal()
ggsave("Hasil_Korelasi/jumlah_per_jam.png", g3, width = 9, height = 5, dpi = 200)

# ---- Simpan tabel ------------------------------------------------------
tabel_semua <- rbind(t1, t2, t3, t4, t5, t6)
write.csv(tabel_semua, "Hasil_Korelasi/tabel_uji_korelasi.csv", row.names = FALSE)
write.csv(rbind(t7_kasar[, kol7], t7_p[, kol7], t7_sp[, kol7], t7_log[, kol7]),
          "Hasil_Korelasi/tabel_korelasi_parsial.csv", row.names = FALSE)
write.csv(as.data.frame(mat_s), "Hasil_Korelasi/matriks_spearman.csv")
write.csv(as.data.frame(mat_p), "Hasil_Korelasi/matriks_pearson.csv")
cat("\nSelesai. Semua tabel dan grafik ada di folder Hasil_Korelasi.\n")