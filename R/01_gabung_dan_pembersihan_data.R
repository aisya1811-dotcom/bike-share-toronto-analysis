# ============================================================
# UTS Business Intelligence
# Analisis Toronto Bike Share Ridership 2017-2020
# Soal 1 dan Soal 2


# ============================================================
# Persiapan
# ============================================================

# Menentukan folder tempat file data disimpan.
# Sesuaikan lokasi folder data dengan komputer masing-masing, lalu aktifkan baris di bawah
# setwd("~/Documents/Coolyeah/Semester 5/Business Intelligence/UTS/Data")

# Mengambil seluruh file CSV dari folder dan subfolder.
file_list <- list.files(
  path = getwd(),
  # Hanya file data asli (2017-Q1.csv ... 2020-12.csv); file hasil analisis ikut terbaca jika disimpan di folder yang sama
  pattern = "^20(17|18|19|20)-(Q[1-4]|[0-9]{2})\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# Memeriksa jumlah file yang ditemukan.
cat("Jumlah file CSV ditemukan:", length(file_list), "\n")

# Melihat nama file yang akan dibaca.
print(file_list)

if (length(file_list) != 24) {
  stop(
    paste0(
      "Ditemukan ", length(file_list),
      " file CSV. Seharusnya terdapat 24 file. ",
      "Periksa folder kerja dan kelengkapan data."
    )
  )
}

# Menentukan nama variabel yang digunakan dalam analisis.
kolom_standar <- c(
  "trip_id",
  "subscription_id",
  "trip_duration_seconds",
  "start_station_id",
  "start_time",
  "start_station_name",
  "end_station_id",
  "end_time",
  "end_station_name",
  "bike_id",
  "user_type",
  "source_file"
)


# ============================================================
# Membaca dan menggabungkan data
# ============================================================

# Fungsi untuk membaca file dan menyamakan nama kolom.
siapkan_data <- function(nama_file) {

  data <- read.csv(
    nama_file,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE,
    na.strings = c("", "NULL", "NULLNULL"),
    strip.white = TRUE
  )

  # Menyesuaikan penulisan nama kolom.
  nama_kolom <- names(data)
  nama_kolom <- trimws(nama_kolom)
  nama_kolom <- gsub("\\s+", " ", nama_kolom)
  nama_kolom <- tolower(nama_kolom)
  nama_kolom <- gsub("[^a-z0-9]+", "_", nama_kolom)
  nama_kolom <- gsub("^_|_$", "", nama_kolom)
  names(data) <- nama_kolom

  # Menyesuaikan beberapa variasi nama kolom dari file sumber.
  daftar_alias <- list(
    trip_id               = c("trip_id"),
    subscription_id       = c("subscription_id"),
    trip_duration_seconds = c("trip_duration_seconds", "trip_duration"),
    start_station_id      = c("start_station_id", "from_station_id"),
    start_time            = c("start_time", "trip_start_time"),
    start_station_name    = c("start_station_name", "from_station_name"),
    end_station_id        = c("end_station_id", "to_station_id"),
    end_time              = c("end_time", "trip_stop_time"),
    end_station_name      = c("end_station_name", "to_station_name"),
    bike_id               = c("bike_id"),
    user_type             = c("user_type")
  )

  # Menyusun data dengan nama kolom yang sama.
  data_standar <- as.data.frame(
    setNames(
      replicate(length(kolom_standar) - 1,
                rep(NA_character_, nrow(data)),
                simplify = FALSE),
      kolom_standar[-length(kolom_standar)]
    ),
    stringsAsFactors = FALSE
  )

  for (kolom_tujuan in names(daftar_alias)) {
    kandidat <- daftar_alias[[kolom_tujuan]]
    kolom_ditemukan <- kandidat[kandidat %in% names(data)]

    if (length(kolom_ditemukan) > 0) {
      data_standar[[kolom_tujuan]] <-
        data[[kolom_ditemukan[1]]]
    }
  }

  # Menyimpan nama file asal setiap observasi.
  data_standar$source_file <- basename(nama_file)

  # Mengatur urutan kolom.
  data_standar <- data_standar[, kolom_standar]

  return(data_standar)
}

# Membaca setiap file CSV.
data_per_file <- lapply(file_list, siapkan_data)

# Menggabungkan data dari seluruh periode.
data_gabungan_awal <- do.call(rbind, data_per_file)
rownames(data_gabungan_awal) <- NULL

# Memeriksa kelengkapan kolom utama pada setiap file.
kritis <- c("trip_id", "trip_duration_seconds", "start_time", "user_type")
cek_kritis <- sapply(
  split(data_gabungan_awal[, kritis], data_gabungan_awal$source_file),
  function(x) colSums(is.na(x))
)
print(cek_kritis)
stopifnot(all(cek_kritis == 0))

cat("\nDimensi data gabungan:\n")
print(dim(data_gabungan_awal))

cat("\nNama variabel:\n")
print(names(data_gabungan_awal))

cat("\nJumlah observasi per file:\n")
print(table(data_gabungan_awal$source_file))


# ============================================================
# Soal 1. Data Understanding
# ============================================================

# ------------------------------------------------------------
# 1a, 1b, dan 1c. Identifikasi variabel, tipe, dan satuan
# ------------------------------------------------------------

data_dictionary <- data.frame(
  Nama_Variabel = c(
    "trip_id",
    "subscription_id",
    "trip_duration_seconds",
    "start_station_id",
    "start_time",
    "start_station_name",
    "end_station_id",
    "end_time",
    "end_station_name",
    "bike_id",
    "user_type",
    "source_file"
  ),

  Deskripsi = c(
    "Identitas unik perjalanan sepeda",
    "Identitas pelanggan atau langganan",
    "Lama perjalanan sepeda",
    "Identitas stasiun awal",
    "Waktu dimulainya perjalanan",
    "Nama stasiun awal",
    "Identitas stasiun tujuan",
    "Waktu berakhirnya perjalanan",
    "Nama stasiun tujuan",
    "Identitas sepeda yang digunakan",
    "Jenis pengguna layanan sepeda",
    "Nama file sumber data (variabel tambahan, dibuat peneliti)"
  ),

  Tipe_Data = c(
    "Identity",
    "Identity",
    "Numeric",
    "Identity",
    "Datetime",
    "Categorical",
    "Identity",
    "Datetime",
    "Categorical",
    "Identity",
    "Categorical",
    "Categorical"
  ),

  Satuan_Pengukuran = c(
    "Tidak ada",
    "Tidak ada",
    "Detik",
    "Tidak ada",
    "Tanggal dan waktu",
    "Tidak ada",
    "Tidak ada",
    "Tanggal dan waktu",
    "Tidak ada",
    "Tidak ada",
    "Tidak ada",
    "Tidak ada"
  ),

  stringsAsFactors = FALSE
)

cat("\nDATA DICTIONARY:\n")
print(data_dictionary)
View(data_dictionary)


# ------------------------------------------------------------
# 1d. Variabel dependen dan independen
# ------------------------------------------------------------

variabel_analisis <- data.frame(
  Peran = c(
    "Dependen (Y)",
    "Independen (X)",
    "Independen (X)",
    "Independen (X)",
    "Tidak dipakai sebagai X"
  ),
  Nama_Variabel = c(
    "trip_duration_seconds",
    "user_type",
    "start_time",
    "start_station_name dan end_station_name",
    "trip_id, subscription_id, bike_id, start_station_id, end_station_id"
  ),
  Keterangan = c(
    "Durasi perjalanan sepeda dalam detik",
    "Jenis pengguna (Annual Member atau Casual Member); pola durasinya dapat berbeda",
    "Diturunkan menjadi jam, hari, bulan, dan kategori hari kerja/akhir pekan pada Soal 3",
    "Bahan membentuk indikator perjalanan pulang-pergi (stasiun awal sama dengan stasiun tujuan) pada Soal 3",
    "Variabel identitas dengan banyak kategori; tidak bermakna sebagai penjelas durasi, dan ID stasiun tidak tersedia pada 2017-Q3 dan 2017-Q4"
  ),
  stringsAsFactors = FALSE
)

# Catatan: dataset tidak menyediakan informasi jarak perjalanan.

# ------------------------------------------------------------
# 1e. Data dictionary
# ------------------------------------------------------------
# Data dictionary sudah disusun pada bagian 1a-1c
# dan disimpan dalam objek data_dictionary.


# ============================================================
# Soal 2. Data Cleaning
# ============================================================

# ------------------------------------------------------------
# 2a. Identifikasi missing values
# ------------------------------------------------------------

missing_sebelum <- colSums(
  is.na(data_gabungan_awal)
)

ringkasan_missing_sebelum <- data.frame(
  Nama_Variabel = names(missing_sebelum),
  Jumlah_Missing = as.numeric(missing_sebelum)
)

cat("\nMISSING VALUES SEBELUM CLEANING:\n")
print(ringkasan_missing_sebelum)
View(ringkasan_missing_sebelum)


# ------------------------------------------------------------
# 2b. Pemeriksaan duplikasi Trip Id
# ------------------------------------------------------------

jumlah_duplikat_trip_id <- sum(
  duplicated(data_gabungan_awal$trip_id)
)

cat("\nJUMLAH DUPLIKASI TRIP ID:\n")
print(jumlah_duplikat_trip_id)

# Memeriksa duplikasi Trip Id untuk masing-masing file.
duplikat_per_file <- do.call(
  rbind,
  lapply(
    split(data_gabungan_awal, data_gabungan_awal$source_file),
    function(x) {
      data.frame(
        source_file = unique(x$source_file),
        Jumlah_Duplikat = sum(duplicated(x$trip_id))
      )
    }
  )
)

rownames(duplikat_per_file) <- NULL

cat("\nDUPLIKASI TRIP ID PER FILE:\n")
print(duplikat_per_file)


# ------------------------------------------------------------
# 2c. Pemeriksaan durasi yang tidak wajar
# ------------------------------------------------------------

# Mengubah kolom durasi ke bentuk numerik.
durasi_awal <- suppressWarnings(
  as.numeric(data_gabungan_awal$trip_duration_seconds)
)

jumlah_durasi_missing <- sum(is.na(durasi_awal))
jumlah_durasi_nol <- sum(durasi_awal == 0, na.rm = TRUE)
jumlah_durasi_negatif <- sum(durasi_awal < 0, na.rm = TRUE)
jumlah_durasi_lebih_24_jam <- sum(
  durasi_awal > 86400,
  na.rm = TRUE
)

cat("\nPEMERIKSAAN DURASI:\n")
cat("Durasi missing:", jumlah_durasi_missing, "\n")
cat("Durasi nol:", jumlah_durasi_nol, "\n")
cat("Durasi negatif:", jumlah_durasi_negatif, "\n")
cat("Durasi lebih dari 24 jam:", jumlah_durasi_lebih_24_jam, "\n")

cat("\nRINGKASAN DURASI:\n")
print(summary(durasi_awal))

# Jumlah durasi kurang dari 60 detik.
cat(
  "Durasi kurang dari 60 detik:",
  sum(durasi_awal < 60, na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------
# Pembersihan awal data
# ------------------------------------------------------------

data_bersih_awal <- data_gabungan_awal

data_bersih_awal$trip_duration_seconds <- durasi_awal

# Menandai perjalanan dengan durasi lebih dari 24 jam.
data_bersih_awal$durasi_lebih_24_jam <-
  data_bersih_awal$trip_duration_seconds > 86400

# Mengeluarkan observasi dengan durasi kosong atau tidak positif.
# Pada data ini tidak ditemukan durasi yang kosong atau negatif.
data_bersih <- subset(
  data_bersih_awal,
  !is.na(trip_duration_seconds) &
    trip_duration_seconds > 0
)

rownames(data_bersih) <- NULL

cat("\nDIMENSI DATA SETELAH CLEANING AWAL:\n")
print(dim(data_bersih))


# ------------------------------------------------------------
# 2d. Pemeriksaan kesesuaian durasi dan waktu
# ------------------------------------------------------------

# Membaca waktu sesuai format yang digunakan pada masing-masing file.
# UTC digunakan sebagai zona netral saat membaca waktu yang tercatat,
# bukan untuk menyatakan bahwa waktu pada data merupakan waktu UTC.

parse_waktu_final <- function(x, source_file) {

  hasil <- as.POSIXct(
    rep(NA_real_, length(x)),
    origin = "1970-01-01",
    tz = "UTC"
  )

  # File 2017-Q1 dan 2017-Q2 menggunakan format hari/bulan/tahun.
  indeks_dmy <- source_file %in% c(
    "2017-Q1.csv",
    "2017-Q2.csv"
  )

  hasil[indeks_dmy] <- as.POSIXct(
    x[indeks_dmy],
    format = "%d/%m/%Y %H:%M",
    tz = "UTC"
  )

  # File 2017-Q4 menggunakan bulan/hari/tahun dua digit
  # dengan informasi detik.
  indeks_q4 <- source_file == "2017-Q4.csv"

  hasil[indeks_q4] <- as.POSIXct(
    x[indeks_q4],
    format = "%m/%d/%y %H:%M:%S",
    tz = "UTC"
  )

  # File lainnya menggunakan bulan/hari/tahun.
  indeks_mdy <- !indeks_dmy & !indeks_q4

  format_lain <- c(
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M"
  )

  for (fmt in format_lain) {
    indeks <- indeks_mdy & is.na(hasil) & !is.na(x)

    hasil[indeks] <- as.POSIXct(
      x[indeks],
      format = fmt,
      tz = "UTC"
    )
  }

  return(hasil)
}

# Mengubah waktu mulai dan selesai ke format datetime.
data_bersih$start_time_datetime <- parse_waktu_final(
  data_bersih$start_time,
  data_bersih$source_file
)

data_bersih$end_time_datetime <- parse_waktu_final(
  data_bersih$end_time,
  data_bersih$source_file
)

cat("\nJUMLAH DATETIME YANG GAGAL DIKONVERSI:\n")
cat(
  "Start time:",
  sum(is.na(data_bersih$start_time_datetime)),
  "\n"
)
cat(
  "End time:",
  sum(is.na(data_bersih$end_time_datetime)),
  "\n"
)

# Menghitung lama waktu berdasarkan waktu mulai dan selesai.
data_bersih$selisih_waktu_detik <- as.numeric(
  difftime(
    data_bersih$end_time_datetime,
    data_bersih$start_time_datetime,
    units = "secs"
  )
)

# Membandingkan hasil perhitungan waktu dengan durasi pada data.
data_bersih$selisih_durasi <-
  data_bersih$selisih_waktu_detik -
  data_bersih$trip_duration_seconds

data_bersih$selisih_absolut <- abs(
  data_bersih$selisih_durasi
)

cat("\nRINGKASAN SELISIH ABSOLUT:\n")
print(summary(data_bersih$selisih_absolut))

jumlah_selisih_lebih_60 <- sum(
  data_bersih$selisih_absolut > 60,
  na.rm = TRUE
)

jumlah_waktu_negatif <- sum(
  data_bersih$selisih_waktu_detik < 0,
  na.rm = TRUE
)

cat(
  "Jumlah perbedaan lebih dari 60 detik:",
  jumlah_selisih_lebih_60,
  "\n"
)

cat(
  "Jumlah waktu selesai lebih awal daripada waktu mulai:",
  jumlah_waktu_negatif,
  "\n"
)

# Memisahkan observasi dengan perbedaan lebih dari 60 detik.
data_tidak_konsisten <- subset(
  data_bersih,
  selisih_absolut > 60
)

# Memeriksa apakah selisih tersebut mendekati satu jam.
data_tidak_konsisten$jarak_dari_1_jam <- abs(
  abs(data_tidak_konsisten$selisih_durasi) - 3600
)

data_tidak_konsisten$dugaan_DST <- ifelse(
  data_tidak_konsisten$jarak_dari_1_jam <= 60,
  "Mendekati 1 jam",
  "Di luar pola 1 jam"
)

cat("\nPOLA SELISIH YANG DIDUGA BERKAITAN DENGAN DST:\n")
print(table(data_tidak_konsisten$dugaan_DST))

# Tanggal perubahan daylight saving time (DST) di Toronto.
tgl_jam_maju   <- as.Date(c("2017-03-12", "2018-03-11", "2019-03-10", "2020-03-08"))
tgl_jam_mundur <- as.Date(c("2017-11-05", "2018-11-04", "2019-11-03", "2020-11-01"))

tgl_mulai   <- as.Date(data_tidak_konsisten$start_time_datetime)
tgl_selesai <- as.Date(data_tidak_konsisten$end_time_datetime)

# Memeriksa apakah perjalanan melewati tanggal perubahan jam.
ada_tanggal <- function(tgl_transisi) {
  sapply(seq_along(tgl_mulai), function(i) {
    any(tgl_transisi >= tgl_mulai[i] & tgl_transisi <= tgl_selesai[i])
  })
}

data_tidak_konsisten$melewati_jam_maju   <- ada_tanggal(tgl_jam_maju)
data_tidak_konsisten$melewati_jam_mundur <- ada_tanggal(tgl_jam_mundur)

cat("\nKAITAN DENGAN TANGGAL PERPINDAHAN JAM (DST):\n")
print(table(
  Jam_maju   = data_tidak_konsisten$melewati_jam_maju,
  Jam_mundur = data_tidak_konsisten$melewati_jam_mundur
))

cat("\nARAH SELISIH MENURUT JENIS PERPINDAHAN JAM:\n")
print(table(
  Jenis = ifelse(data_tidak_konsisten$melewati_jam_maju,
                 "Jam maju (Maret)", "Jam mundur (November)"),
  Arah  = ifelse(data_tidak_konsisten$selisih_durasi > 0, "Positif", "Negatif")
))

# Menghitung ketidaksesuaian untuk setiap file.
if (nrow(data_tidak_konsisten) > 0) {
  jumlah_tidak_konsisten <- aggregate(
    selisih_absolut ~ source_file,
    data = data_tidak_konsisten,
    FUN = length
  )

  names(jumlah_tidak_konsisten)[2] <-
    "Jumlah_Ketidaksesuaian"

  cat("\nKETIDAKSESUAIAN BERDASARKAN FILE:\n")
  print(jumlah_tidak_konsisten)
}

# Melihat beberapa contoh observasi yang memiliki selisih lebih dari 60 detik.
cat("\nCONTOH DATA DENGAN SELISIH LEBIH DARI 60 DETIK:\n")

if (nrow(data_tidak_konsisten) > 0) {
  print(
    head(
      data_tidak_konsisten[, c(
        "source_file",
        "start_time",
        "end_time",
        "trip_duration_seconds",
        "selisih_waktu_detik",
        "selisih_durasi",
        "selisih_absolut"
      )],
      20
    )
  )
}

# Memisahkan observasi ketika waktu selesai tercatat lebih awal.
data_waktu_negatif <- subset(
  data_bersih,
  selisih_waktu_detik < 0
)

cat("\nCONTOH DATA DENGAN WAKTU NEGATIF:\n")

if (nrow(data_waktu_negatif) > 0) {
  print(
    head(
      data_waktu_negatif[, c(
        "source_file",
        "start_time",
        "end_time",
        "trip_duration_seconds",
        "selisih_waktu_detik",
        "selisih_durasi"
      )],
      10
    )
  )
}

# ------------------------------------------------------------
# 2e. Keputusan terhadap data bermasalah
# ------------------------------------------------------------

# Membuat format angka dengan pemisah ribuan titik.
fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)

# Menghitung jumlah setiap permasalahan.
baris_2017_2018 <- sum(
  substr(data_gabungan_awal$source_file, 1, 4) %in% c("2017", "2018")
)
baris_2017_q34 <- sum(
  data_gabungan_awal$source_file %in% c("2017-Q3.csv", "2017-Q4.csv")
)

missing_subscription <- sum(is.na(data_gabungan_awal$subscription_id))
missing_bike         <- sum(is.na(data_gabungan_awal$bike_id))
missing_end_time     <- sum(is.na(data_gabungan_awal$end_time))

missing_id_stasiun <- sum(is.na(data_gabungan_awal$start_station_id)) +
  sum(is.na(data_gabungan_awal$end_station_id))
missing_nama_stasiun <- sum(is.na(data_gabungan_awal$start_station_name)) +
  sum(is.na(data_gabungan_awal$end_station_name))

# Missing struktural terjadi ketika kolom tidak tersedia pada file tertentu.
id_struktural   <- 2 * baris_2017_q34
id_sporadis     <- missing_id_stasiun - id_struktural
bike_struktural <- baris_2017_2018
bike_sporadis   <- missing_bike - bike_struktural

# Menghitung perjalanan dengan durasi antara 1 sampai 59 detik.
jumlah_durasi_1_59 <- sum(data_bersih$trip_duration_seconds < 60)

persen_24_jam <- format(
  round(jumlah_durasi_lebih_24_jam / nrow(data_bersih) * 100, 3),
  decimal.mark = ","
)
maks_hari <- format(
  round(max(data_bersih$trip_duration_seconds) / 86400, 1),
  decimal.mark = ","
)

jumlah_jam_maju   <- sum(data_tidak_konsisten$melewati_jam_maju)
jumlah_jam_mundur <- sum(data_tidak_konsisten$melewati_jam_mundur)

# Menyimpan penanda untuk digunakan pada analisis berikutnya.
data_bersih$waktu_tidak_konsisten <- data_bersih$selisih_absolut > 60

keputusan_pembersihan <- data.frame(
  Permasalahan = c(
    "Missing subscription_id",
    "Missing bike_id",
    "Missing ID stasiun",
    "Missing nama stasiun",
    "Missing end_time",
    "Duplikasi Trip Id",
    "Durasi nol",
    "Durasi negatif",
    "Durasi 1-59 detik",
    "Durasi lebih dari 24 jam",
    "Ketidaksesuaian waktu lebih dari 60 detik"
  ),
  
  Jumlah = c(
    missing_subscription,
    missing_bike,
    missing_id_stasiun,
    missing_nama_stasiun,
    missing_end_time,
    jumlah_duplikat_trip_id,
    jumlah_durasi_nol,
    jumlah_durasi_negatif,
    jumlah_durasi_1_59,
    jumlah_durasi_lebih_24_jam,
    jumlah_selisih_lebih_60
  ),
  
  Tindakan = c(
    "Dipertahankan",
    "Dipertahankan",
    "Dipertahankan",
    "Dipertahankan",
    "Dihapus (bersama durasi nol)",
    "Tidak ada tindakan",
    "Dihapus",
    "Tidak ada tindakan",
    "Dihapus",
    "Dipertahankan dan ditandai",
    "Dipertahankan dan ditandai"
  ),
  
  Alasan = c(
    "Kolom tidak tersedia pada seluruh file 2017-2018 (struktural) dan tidak dipakai sebagai variabel analisis",
    paste0("Struktural pada 2017-2018 (", fmt(bike_struktural),
           " baris) dan sporadis pada ", fmt(bike_sporadis),
           " baris; tidak dipakai sebagai variabel analisis"),
    paste0(fmt(id_struktural),
           " struktural (file 2017-Q3 dan 2017-Q4 tidak memiliki kolom ID stasiun) dan ",
           fmt(id_sporadis), " sporadis pada 2019-2020"),
    "Sporadis; durasi dan waktu perjalanan tetap valid sehingga observasi masih dapat digunakan",
    "Observasi tersebut juga berdurasi nol sehingga ikut dihapus",
    "Tidak ditemukan duplikasi Trip Id",
    "Perjalanan tanpa lama waktu bukan perjalanan yang sebenarnya terjadi",
    "Tidak ditemukan durasi negatif",
    paste0("Durasi sependek ini diduga false start atau kegagalan docking. ",
           "Durasi 1-59 detik tidak ditemukan pada 2017-Q1, 2017-Q2, dan seluruh 2018, ",
           "sehingga penghapusan menyeragamkan definisi perjalanan antarperiode"),
    paste0("Jumlahnya sangat kecil (", persen_24_jam,
           "% data) dan maksimum mencapai ", maks_hari,
           " hari sehingga diduga sepeda tidak dikembalikan atau terjadi kesalahan pencatatan. ",
           "Ditandai agar dapat dikeluarkan pada analisis korelasi dan regresi"),
    paste0("Seluruh observasi berada pada rentang tanggal perpindahan jam (DST) Toronto: ",
           jumlah_jam_maju, " melewati jam maju (selisih positif) dan ",
           jumlah_jam_mundur, " melewati jam mundur (selisih negatif), ",
           "sehingga bukan kesalahan pencatatan")
  ),
  
  stringsAsFactors = FALSE
)

cat("\nKEPUTUSAN PEMBERSIHAN DATA:\n")
print(keputusan_pembersihan)
View(keputusan_pembersihan)

# Mengeluarkan perjalanan dengan durasi 1 sampai 59 detik.
data_bersih <- subset(data_bersih, trip_duration_seconds >= 60)
rownames(data_bersih) <- NULL

cat("\nDimensi data akhir setelah seluruh keputusan cleaning:\n")
print(dim(data_bersih))


# ------------------------------------------------------------
# 2f. Ringkasan kondisi data sebelum dan sesudah cleaning
# ------------------------------------------------------------

# Membandingkan missing values pada 12 variabel.
missing_sebelum <- colSums(is.na(data_gabungan_awal[, kolom_standar]))
missing_sesudah <- colSums(is.na(data_bersih[, kolom_standar]))

ringkasan_missing <- data.frame(
  Nama_Variabel    = kolom_standar,
  Sebelum_Cleaning = as.numeric(missing_sebelum),
  Sesudah_Cleaning = as.numeric(missing_sesudah)
)

cat("\nRINGKASAN MISSING VALUES SEBELUM DAN SESUDAH CLEANING:\n")
print(ringkasan_missing)
View(ringkasan_missing)

# Membandingkan kondisi durasi sebelum dan sesudah cleaning.
durasi_sebelum <- as.numeric(data_gabungan_awal$trip_duration_seconds)
durasi_sesudah <- data_bersih$trip_duration_seconds

ringkasan_cleaning <- data.frame(
  Keterangan = c(
    "Jumlah observasi",
    "Jumlah variabel (11 asli + 1 tambahan)",
    "Total missing values",
    "Trip Id duplikat",
    "Durasi nol",
    "Durasi negatif",
    "Durasi 1-59 detik",
    "Durasi lebih dari 24 jam (ditandai)",
    "Jumlah observasi dihapus"
  ),
  
  Sebelum_Cleaning = c(
    nrow(data_gabungan_awal),
    length(kolom_standar),
    sum(missing_sebelum),
    sum(duplicated(data_gabungan_awal$trip_id)),
    sum(durasi_sebelum == 0, na.rm = TRUE),
    sum(durasi_sebelum < 0, na.rm = TRUE),
    sum(durasi_sebelum > 0 & durasi_sebelum < 60, na.rm = TRUE),
    sum(durasi_sebelum > 86400, na.rm = TRUE),
    0
  ),
  
  Sesudah_Cleaning = c(
    nrow(data_bersih),
    length(kolom_standar),
    sum(missing_sesudah),
    sum(duplicated(data_bersih$trip_id)),
    sum(durasi_sesudah == 0, na.rm = TRUE),
    sum(durasi_sesudah < 0, na.rm = TRUE),
    sum(durasi_sesudah > 0 & durasi_sesudah < 60, na.rm = TRUE),
    sum(durasi_sesudah > 86400, na.rm = TRUE),
    nrow(data_gabungan_awal) - nrow(data_bersih)
  )
)

cat("\nRINGKASAN KONDISI DATA SEBELUM DAN SESUDAH CLEANING:\n")
print(ringkasan_cleaning)
View(ringkasan_cleaning)

# Membandingkan jumlah observasi pada setiap file.
per_file <- data.frame(
  source_file = names(table(data_gabungan_awal$source_file)),
  Sebelum     = as.vector(table(data_gabungan_awal$source_file)),
  Sesudah     = as.vector(table(data_bersih$source_file))
)
per_file$Dihapus <- per_file$Sebelum - per_file$Sesudah

cat("\nJUMLAH OBSERVASI PER FILE SEBELUM DAN SESUDAH CLEANING:\n")
print(per_file)


# Menyimpan data hasil pembersihan untuk digunakan pada tahap berikutnya
saveRDS(data_bersih, file = "data_bersih_soal2.rds")

# ============================================================
# Akhir Soal 1 dan Soal 2
# ============================================================

cat("\n============================================\n")
cat("PROSES SOAL 1 DAN SOAL 2 SELESAI DIJALANKAN\n")
cat("============================================\n")
cat("Data awal:", nrow(data_gabungan_awal), "observasi\n")
cat("Data setelah cleaning:", nrow(data_bersih), "observasi\n")
cat("Observasi dihapus:", nrow(data_gabungan_awal) - nrow(data_bersih), "\n")


