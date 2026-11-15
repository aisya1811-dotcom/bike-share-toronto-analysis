# Analisis Toronto Bike Share Ridership 2017–2020

Analisis data perjalanan Toronto Bike Share periode 2017–2020 (24 file CSV,
sekitar 8,7 juta perjalanan) menggunakan R. Proyek ini mencakup pemahaman data,
pembersihan data, persiapan data, analisis eksploratif (EDA), analisis korelasi,
dan regresi linear berganda dengan durasi perjalanan sebagai variabel dependen.

## Data
Data tidak disertakan di repositori ini karena ukurannya besar.
Data diunduh dari Kaggle:
[BikeShare Toronto Ridership Data 2017-2020](https://www.kaggle.com/datasets/jasonzxho/bikeshare-toronto-ridership-data-20172020)
(unggahan @jasonzxho), berlisensi *Open Government Licence – Toronto*.
Letakkan 24 file CSV (2017-Q1 sampai 2020-12) pada satu folder.

## Cara menjalankan
1. Atur *working directory* ke folder data (aktifkan baris `setwd(...)` di tiap skrip
   dan sesuaikan lokasinya).
2. Jalankan skrip berurutan:

| Skrip | Isi |
|---|---|
| `R/01_gabung_dan_pembersihan_data.R` | Membaca dan menggabungkan data, pemeriksaan *missing value*, duplikasi, durasi tidak wajar, konsistensi waktu, serta pembersihan |
| `R/02_tipe_data_dan_persiapan.R` | Konversi tipe data dan pembentukan variabel baru |
| `R/03_analisis_eksplorasi.R` | Statistik deskriptif, histogram, *boxplot*, pola jam, hari, bulan, dan stasiun |
| `R/04_analisis_korelasi.R` | Uji korelasi Pearson dan Spearman, korelasi parsial, pemeriksaan asumsi |
| `R/05_analisis_regresi.R` | Regresi linear berganda, uji F dan t, pemeriksaan asumsi |

Skrip saling bergantung: 01 menghasilkan `data_bersih_soal2.rds`, 02 menghasilkan
`data_final_soal3.rds`, yang kemudian dipakai skrip 03–05.
Package yang digunakan: `ggplot2`. Diuji dengan R 4.6.1.

## Temuan utama
- Setelah pembersihan tersisa 8.713.667 perjalanan dari 8.766.149 (52.482 dihapus,
  yaitu durasi nol dan durasi 1–59 detik).
- Median durasi perjalanan 12,05 menit. *Casual Member* (median 19,68 menit) berdurasi
  lebih lama daripada *Annual Member* (median 10,27 menit).
- Jumlah perjalanan memuncak pada pukul 17, dengan puncak lebih kecil pada pukul 8,
  dan memuncak pada Agustus (musim panas).
- Korelasi Spearman antara jenis pengguna (*Casual Member*) dan durasi sebesar 0,40,
  yang terkuat di antara variabel yang tersedia.
- Model regresi linear berganda hanya menjelaskan sekitar 9% variasi durasi (R² = 0,09).

## Catatan dan keterbatasan
- Dataset tidak memuat variabel jarak, sehingga indikator perjalanan pulang-pergi
  (`pulang_pergi`) hanya digunakan sebagai pendekatan.
- Perjalanan berdurasi 1–59 detik dihapus untuk menyeragamkan definisi perjalanan
  antarperiode; dampaknya terhadap hasil tidak diuji terpisah.
- Asumsi normalitas dan homoskedastisitas residual pada model regresi tidak terpenuhi.
- Hasil menunjukkan hubungan, bukan sebab-akibat.

## Tentang proyek
Dikerjakan sebagai tugas Ujian Tengah Semester mata kuliah Business Intelligence, Statistika.
