# LAND OF PROPERTY
Dashboard interaktif berbasis R Shiny untuk eksplorasi, analisis, dan estimasi harga properti di kawasan Jabodetabek

## Tentang Proyek
Land of Property adalah aplikasi web dashboard yang dibangun menggunakan R Shiny untuk membantu pengguna menjelajahi data properti (rumah, townhouse, villa, apartemen) di wilayah Jabodetabek. Aplikasi ini menggabungkan visualisasi data interaktif, peta sebaran properti, analisis statistik deskriptif, serta model regresi linear berganda untuk mengestimasi harga wajar sebuah properti.

## Fitur Utama
1. Cari Properti
2. Kalkulator Nilai
3. Eksplorasi Statistik
4. Tambah Properti

## Software yang Digunakan
Proyek dibangun di R dengan packages yang dipakai sebagai berikut
shiny            : kerangka kerja aplikasi web interaktif
bslib            : tema Bootstrap 5 untuk UI
leaflet          : peta interaktif
plotly           : grafik interaktif
DT               : tabel data interaktif
shinycssloaders  : indikator loading
ggplot2          : visualisasi data statis (dirender ulang via Plotly)
dplyr            : manipulasi data
scales           : format angka & skala

## Instalasi Packages
```
install.packages(c(
  "shiny", "bslib", "leaflet", "plotly", "DT",
  "shinycssloaders", "ggplot2", "dplyr", "scales"
))
```
## Cara Penggunaan
1. Clone Repository di R
   ```
   git clone https://github.com/bntgmaulana86-bot/Land-of-Property-Final.git
   cd Land-of-Property-Final
   ```
   (Jalankan kode diatas pada terminal R satu per satu)
3. Buka Prop3.R di RStudio.
4. Pastikan file jabodetabek_house_price.csv berada pada direktori kerja yang sama (working directory).
5. Jalankan aplikasi dengan menekan tombol Run App

## Kontributor
Kelompok 3 Proyek Komputasi Statistika:
1. Musthafa Kamal                  1314624013 @kamal141025
2. Pradhika Lazuardie Setaiawan    1314624039 @acumalala
3. Anastasya Tabita Andini         1314624040 @tabitanadini
4. Sultan Arya Ilham               1314624048 @sailhamm
5. Bintang Maulana Hafizh          1314624049 @bntgmaulana86-bot
6. Larasati Purnamasari            1314624066 @larasatisari



