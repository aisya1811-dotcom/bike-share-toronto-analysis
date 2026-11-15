# ============================================================
# UTS BUSINESS INTELLIGENCE
# SOAL 4. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================

library(ggplot2)
options(scipen = 999)      # menghindari tampilan angka ilmiah (1e+05)

# Sesuaikan lokasi folder data dengan komputer masing-masing, lalu aktifkan baris di bawah
# setwd("~/Documents/Coolyeah/Semester 5/Business Intelligence/UTS/Data")
data_final <- readRDS("data_final_soal3.rds")

# Folder untuk menyimpan grafik dan tabel
dir.create("Hasil_EDA", showWarnings = FALSE)

# Sampel acak 1 juta perjalanan HANYA untuk menggambar grafik
# (8,7 juta titik membuat grafik sangat lambat). Seluruh statistik
# dan tabel di bawah dihitung dari data penuh.
set.seed(123)
data_plot <- data_final[sample(nrow(data_final), 1000000),
                        c("durasi_menit", "user_type")]

# ------------------------------------------------------------
# 4a. STATISTIK DESKRIPTIF
# ------------------------------------------------------------
ringkas <- function(x) {
  c(N      = length(x),
    Mean   = mean(x),
    Median = median(x),
    SD     = sd(x),
    Min    = min(x),
    Q1     = unname(quantile(x, 0.25)),
    Q3     = unname(quantile(x, 0.75)),
    Max    = max(x))
}

statistik_deskriptif <- rbind(
  trip_duration_seconds = ringkas(data_final$trip_duration_seconds),
  durasi_menit          = ringkas(data_final$durasi_menit)
)

cat("\nSTATISTIK DESKRIPTIF DURASI PERJALANAN:\n")
print(round(statistik_deskriptif, 2))
write.csv(round(statistik_deskriptif, 2), "Hasil_EDA/statistik_deskriptif.csv")

# ------------------------------------------------------------
# 4b. HISTOGRAM DAN DENSITY PLOT DURASI
# ------------------------------------------------------------
# Durasi sangat menjulur ke kanan, sehingga histogram dibuat dua cara:
# (1) dibatasi 0-60 menit agar bentuk sebaran terlihat, dan
# (2) seluruh durasi dengan sumbu logaritmik.
# Tidak ada data yang dibuang; hanya tampilan grafik yang dibatasi.

cat("\nPersentase perjalanan berdurasi <= 60 menit:",
    round(mean(data_final$durasi_menit <= 60) * 100, 2), "%\n")

p_hist <- ggplot(subset(data_plot, durasi_menit <= 60), aes(x = durasi_menit)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "steelblue", colour = "white") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Histogram Durasi Perjalanan (0-60 Menit)",
       subtitle = "Sampel acak 1.000.000 perjalanan",
       x = "Durasi (menit)", y = "Frekuensi") +
  theme_minimal()
print(p_hist)
ggsave("Hasil_EDA/histogram_durasi.png", p_hist, width = 8, height = 5, dpi = 150)

p_density <- ggplot(subset(data_plot, durasi_menit <= 60), aes(x = durasi_menit)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Density Plot Durasi Perjalanan (0-60 Menit)",
       subtitle = "Sampel acak 1.000.000 perjalanan",
       x = "Durasi (menit)", y = "Kepadatan") +
  theme_minimal()
print(p_density)
ggsave("Hasil_EDA/density_durasi.png", p_density, width = 8, height = 5, dpi = 150)

p_hist_log <- ggplot(data_plot, aes(x = durasi_menit)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "white") +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Histogram Durasi Perjalanan (Skala Logaritmik)",
       subtitle = "Seluruh durasi, sampel acak 1.000.000 perjalanan",
       x = "Durasi (menit, skala log)", y = "Frekuensi") +
  theme_minimal()
print(p_hist_log)
ggsave("Hasil_EDA/histogram_durasi_log.png", p_hist_log, width = 8, height = 5, dpi = 150)

# ------------------------------------------------------------
# 4c. BOXPLOT DAN PENCILAN (OUTLIER)
# ------------------------------------------------------------
# Batas pencilan dihitung dari seluruh data dengan aturan 1,5 x IQR.
q1  <- unname(quantile(data_final$durasi_menit, 0.25))
q3  <- unname(quantile(data_final$durasi_menit, 0.75))
iqr <- q3 - q1
batas_bawah <- q1 - 1.5 * iqr
batas_atas  <- q3 + 1.5 * iqr

tabel_pencilan <- data.frame(
  Keterangan = c("Q1 (menit)", "Q3 (menit)", "IQR (menit)",
                 "Batas bawah (menit)", "Batas atas (menit)",
                 "Pencilan di atas batas atas", "Pencilan di bawah batas bawah",
                 "Persentase pencilan atas (%)"),
  Nilai = c(q1, q3, iqr, batas_bawah, batas_atas,
            sum(data_final$durasi_menit > batas_atas),
            sum(data_final$durasi_menit < batas_bawah),
            mean(data_final$durasi_menit > batas_atas) * 100)
)
cat("\nBATAS PENCILAN DURASI (ATURAN 1,5 x IQR):\n")
print(tabel_pencilan, digits = 6)
write.csv(tabel_pencilan, "Hasil_EDA/tabel_pencilan.csv", row.names = FALSE)

p_box <- ggplot(data_plot, aes(y = durasi_menit)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.2, outlier.size = 0.5) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Boxplot Durasi Perjalanan (Skala Logaritmik)",
       subtitle = "Sampel acak 1.000.000 perjalanan",
       y = "Durasi (menit, skala log)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
print(p_box)
ggsave("Hasil_EDA/boxplot_durasi.png", p_box, width = 6, height = 6, dpi = 150)

# ------------------------------------------------------------
# 4d. PERBANDINGAN DURASI BERDASARKAN USER TYPE
# ------------------------------------------------------------
deskriptif_user <- do.call(
  rbind,
  lapply(split(data_final$durasi_menit, data_final$user_type), ringkas)
)
cat("\nSTATISTIK DURASI (MENIT) BERDASARKAN USER TYPE:\n")
print(round(deskriptif_user, 2))
write.csv(round(deskriptif_user, 2), "Hasil_EDA/deskriptif_user_type.csv")

p_box_user_log <- ggplot(data_plot, aes(x = user_type, y = durasi_menit, fill = user_type)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 0.5, show.legend = FALSE) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Boxplot Durasi Perjalanan Berdasarkan User Type (Skala Logaritmik)",
       x = "User type", y = "Durasi (menit, skala log)") +
  theme_minimal()
print(p_box_user_log)
ggsave("Hasil_EDA/boxplot_user_type_log.png", p_box_user_log, width = 7, height = 5, dpi = 150)

# Tampilan diperbesar pada durasi 0-60 menit (data tidak dibuang)
p_box_user_zoom <- ggplot(data_plot, aes(x = user_type, y = durasi_menit, fill = user_type)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 0.5, show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(title = "Boxplot Durasi Berdasarkan User Type (Diperbesar 0-60 Menit)",
       x = "User type", y = "Durasi (menit)") +
  theme_minimal()
print(p_box_user_zoom)
ggsave("Hasil_EDA/boxplot_user_type_zoom.png", p_box_user_zoom, width = 7, height = 5, dpi = 150)

# ------------------------------------------------------------
# 4e. POLA JUMLAH PERJALANAN: JAM, HARI, BULAN
# ------------------------------------------------------------
per_jam   <- as.data.frame(table(Jam   = data_final$jam),   responseName = "Jumlah")
per_hari  <- as.data.frame(table(Hari  = data_final$hari),  responseName = "Jumlah")
per_bulan <- as.data.frame(table(Bulan = data_final$bulan), responseName = "Jumlah")

cat("\nJUMLAH PERJALANAN PER JAM:\n");   print(per_jam)
cat("\nJUMLAH PERJALANAN PER HARI:\n");  print(per_hari)
cat("\nJUMLAH PERJALANAN PER BULAN:\n"); print(per_bulan)

write.csv(per_jam,   "Hasil_EDA/per_jam.csv",   row.names = FALSE)
write.csv(per_hari,  "Hasil_EDA/per_hari.csv",  row.names = FALSE)
write.csv(per_bulan, "Hasil_EDA/per_bulan.csv", row.names = FALSE)

buat_batang <- function(df, x, judul, sumbu_x, nama_file) {
  p <- ggplot(df, aes(x = .data[[x]], y = Jumlah)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = judul, x = sumbu_x, y = "Jumlah perjalanan") +
    theme_minimal()
  print(p)
  ggsave(paste0("Hasil_EDA/", nama_file), p, width = 8, height = 5, dpi = 150)
}

buat_batang(per_jam,   "Jam",   "Jumlah Perjalanan Berdasarkan Jam Keberangkatan",   "Jam",   "pola_jam.png")
buat_batang(per_hari,  "Hari",  "Jumlah Perjalanan Berdasarkan Hari Keberangkatan",  "Hari",  "pola_hari.png")
buat_batang(per_bulan, "Bulan", "Jumlah Perjalanan Berdasarkan Bulan Keberangkatan", "Bulan", "pola_bulan.png")

# Tambahan (di luar ketentuan soal): proporsi perjalanan per jam
# pada hari kerja dibandingkan akhir pekan.
pola_jam_kategori <- as.data.frame(
  prop.table(table(Jam = data_final$jam, Kategori = data_final$kategori_hari), margin = 2),
  responseName = "Proporsi"
)
pola_jam_kategori$Jam <- as.integer(as.character(pola_jam_kategori$Jam))

p_jam_kategori <- ggplot(pola_jam_kategori, aes(x = Jam, y = Proporsi, colour = Kategori)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Proporsi Perjalanan per Jam: Hari Kerja vs Akhir Pekan",
       x = "Jam keberangkatan", y = "Proporsi perjalanan", colour = "Kategori hari") +
  theme_minimal()
print(p_jam_kategori)
ggsave("Hasil_EDA/pola_jam_kategori_hari.png", p_jam_kategori, width = 9, height = 5, dpi = 150)

# ------------------------------------------------------------
# 4f. STASIUN KEBERANGKATAN DAN TUJUAN TERTINGGI
# ------------------------------------------------------------
# Analisis memakai NAMA stasiun (setelah spasi dirapikan) karena ID stasiun
# tidak tersedia pada 2017-Q3 dan 2017-Q4. Baris tanpa nama tidak dihitung.
bersihkan_nama <- function(x) trimws(gsub("\\s+", " ", x))

top_awal   <- sort(table(bersihkan_nama(data_final$start_station_name)), decreasing = TRUE)
top_tujuan <- sort(table(bersihkan_nama(data_final$end_station_name)),   decreasing = TRUE)

top10_awal <- data.frame(Stasiun = names(top_awal)[1:10],   Jumlah = as.vector(top_awal)[1:10])
top10_tujuan <- data.frame(Stasiun = names(top_tujuan)[1:10], Jumlah = as.vector(top_tujuan)[1:10])

cat("\n10 STASIUN KEBERANGKATAN TERBANYAK:\n"); print(top10_awal)
cat("\n10 STASIUN TUJUAN TERBANYAK:\n");        print(top10_tujuan)
write.csv(top10_awal,   "Hasil_EDA/top10_stasiun_awal.csv",   row.names = FALSE)
write.csv(top10_tujuan, "Hasil_EDA/top10_stasiun_tujuan.csv", row.names = FALSE)

buat_top <- function(df, judul, nama_file) {
  p <- ggplot(df, aes(x = reorder(Stasiun, Jumlah), y = Jumlah)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = judul, x = NULL, y = "Jumlah perjalanan") +
    theme_minimal()
  print(p)
  ggsave(paste0("Hasil_EDA/", nama_file), p, width = 9, height = 5, dpi = 150)
}
buat_top(top10_awal,   "10 Stasiun Keberangkatan Terbanyak", "top10_stasiun_awal.png")
buat_top(top10_tujuan, "10 Stasiun Tujuan Terbanyak",        "top10_stasiun_tujuan.png")

cat("\nSOAL 4 SELESAI. Hasil tersimpan di folder Hasil_EDA.\n")