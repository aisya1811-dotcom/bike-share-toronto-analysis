# ============================================================
# UTS BUSINESS INTELLIGENCE
# SOAL 3. DATA TYPE AND DATA PREPARATION IN R
# ============================================================

# Sesuaikan lokasi folder data dengan komputer masing-masing, lalu aktifkan baris di bawah
# setwd("~/Documents/Coolyeah/Semester 5/Business Intelligence/UTS/Data")

# Membaca data hasil Soal 1-2
data_bersih <- readRDS("data_bersih_soal2.rds")

# ------------------------------------------------------------
# 3a. PEMERIKSAAN TIPE DATA SEBELUM KONVERSI
# ------------------------------------------------------------
cat("\nSTRUKTUR DATA SEBELUM KONVERSI:\n")
str(data_bersih)

cat("\nRINGKASAN DATA SEBELUM KONVERSI:\n")
print(summary(data_bersih))

# ------------------------------------------------------------
# 3b. KONVERSI TIPE DATA
# ------------------------------------------------------------
data_final <- data_bersih
rm(data_bersih)          # menghemat memori
invisible(gc())

# Variabel identitas -> character
kolom_identitas <- c(
  "trip_id", "subscription_id", "bike_id",
  "start_station_id", "end_station_id"
)
for (kolom in kolom_identitas) {
  data_final[[kolom]] <- as.character(data_final[[kolom]])
}

# Variabel numerik -> numeric
data_final$trip_duration_seconds <- as.numeric(data_final$trip_duration_seconds)

# Variabel waktu -> POSIXct (memakai hasil konversi dari Soal 2)
data_final$start_time <- data_final$start_time_datetime
data_final$end_time   <- data_final$end_time_datetime
data_final$start_time_datetime <- NULL
data_final$end_time_datetime   <- NULL

# user_type -> factor. Label 2017 ("Member", "Casual") diseragamkan
# dengan label 2018-2020 ("Annual Member", "Casual Member").
cat("\nLABEL user_type PER TAHUN (SEBELUM DISERAGAMKAN):\n")
print(table(substr(data_final$source_file, 1, 4), data_final$user_type))

data_final$user_type <- trimws(data_final$user_type)
data_final$user_type[data_final$user_type == "Member"] <- "Annual Member"
data_final$user_type[data_final$user_type == "Casual"] <- "Casual Member"
data_final$user_type <- factor(
  data_final$user_type,
  levels = c("Annual Member", "Casual Member")
)
stopifnot(!anyNA(data_final$user_type))   # berhenti jika ada label tak dikenal

# source_file -> factor
data_final$source_file <- factor(data_final$source_file)

# ------------------------------------------------------------
# 3c. FEATURE ENGINEERING
# ------------------------------------------------------------
nama_hari  <- c("Minggu", "Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu")
nama_bulan <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun",
                "Jul", "Agu", "Sep", "Okt", "Nov", "Des")

# Memecah waktu keberangkatan menjadi komponen (tahun, bulan, jam, hari)
waktu_lt <- as.POSIXlt(data_final$start_time, tz = "UTC")

# Durasi dalam menit
data_final$durasi_menit <- data_final$trip_duration_seconds / 60

# Tahun, bulan, jam, dan hari keberangkatan
data_final$tahun <- as.integer(waktu_lt$year + 1900)
data_final$bulan <- factor(nama_bulan[waktu_lt$mon + 1], levels = nama_bulan)
data_final$jam   <- as.integer(waktu_lt$hour)
data_final$hari  <- factor(
  nama_hari[waktu_lt$wday + 1],
  levels = c("Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu", "Minggu")
)

# Hari kerja (Senin-Jumat) dan akhir pekan (Sabtu-Minggu)
data_final$kategori_hari <- factor(
  ifelse(waktu_lt$wday %in% c(0, 6), "Akhir pekan", "Hari kerja"),
  levels = c("Hari kerja", "Akhir pekan")
)

rm(waktu_lt)
invisible(gc())

# Perjalanan pulang-pergi: stasiun awal sama dengan stasiun tujuan
nama_mulai   <- trimws(gsub("\\s+", " ", data_final$start_station_name))
nama_selesai <- trimws(gsub("\\s+", " ", data_final$end_station_name))
data_final$pulang_pergi <- factor(
  ifelse(nama_mulai == nama_selesai, "Ya", "Tidak"),
  levels = c("Tidak", "Ya")
)
rm(nama_mulai, nama_selesai)
invisible(gc())

# ------------------------------------------------------------
# PEMERIKSAAN HASIL
# ------------------------------------------------------------
cat("\nSTRUKTUR DATA SETELAH KONVERSI DAN FEATURE ENGINEERING:\n")
str(data_final)

tipe_data <- data.frame(
  Variabel  = names(data_final),
  Tipe_Data = sapply(data_final, function(x) class(x)[1]),
  row.names = NULL
)
cat("\nTIPE DATA SETIAP VARIABEL:\n")
print(tipe_data)

cat("\nRINGKASAN VARIABEL BARU:\n")
print(summary(data_final[, c(
  "user_type", "durasi_menit", "tahun", "bulan",
  "jam", "hari", "kategori_hari", "pulang_pergi"
)]))

cat("\nJUMLAH PER USER TYPE:\n");       print(table(data_final$user_type))
cat("\nJUMLAH PER TAHUN:\n");           print(table(data_final$tahun))
cat("\nJUMLAH PER KATEGORI HARI:\n");   print(table(data_final$kategori_hari))
cat("\nJUMLAH PER HARI:\n");            print(table(data_final$hari))
cat("\nJUMLAH PER BULAN:\n");           print(table(data_final$bulan))
cat("\nJUMLAH PERJALANAN PULANG-PERGI:\n")
print(table(data_final$pulang_pergi, useNA = "ifany"))

# Menyimpan data untuk Soal 4 dan seterusnya
saveRDS(data_final, file = "data_final_soal3.rds")

cat("\nSOAL 3 SELESAI. Jumlah observasi:", nrow(data_final),
    " Jumlah variabel:", ncol(data_final), "\n")