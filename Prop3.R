# ==============================================================================
# 1. LOAD LIBRARIES
# ==============================================================================
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(shinycssloaders) 
library(ggplot2)
library(dplyr)
library(scales) 

# ==============================================================================
# 2. PERSIAPAN DATA & PREPROCESSING
# ==============================================================================

# 1. Membaca data dengan fill = TRUE untuk mengatasi error sisaan koma Excel
df_house <- read.csv("jabodetabek_house_price.csv", stringsAsFactors = FALSE, fill = TRUE)

# 2. Membersihkan "kolom hantu" (kolom kosong yang terbaca sebagai X, X.1, dst dari Excel)
df_house <- df_house[, !grepl("^X", names(df_house))]

# --- PERBAIKAN TOTAL DATA GANDA & KONSISTENSI KAPITALISASI TEKS ---
if ("city" %in% colnames(df_house)) {
  df_house$city <- trimws(gsub("\\s+", " ", df_house$city)) 
  df_house$city <- tools::toTitleCase(tolower(df_house$city)) 
}
if ("district" %in% colnames(df_house)) {
  df_house$district <- trimws(gsub("\\s+", " ", df_house$district))
  df_house$district <- tools::toTitleCase(tolower(df_house$district))
}
if ("certificate" %in% colnames(df_house)) {
  df_house$certificate <- trimws(gsub("\\s+", " ", df_house$certificate))
  df_house$certificate <- toupper(df_house$certificate) 
}

# 3. Hapus kolom garage lama (jika ada sisaan), lalu ubah nama carports menjadi garage
if ("garages" %in% colnames(df_house)) df_house$garages <- NULL
if ("garage" %in% colnames(df_house)) df_house$garage <- NULL
if ("carports" %in% colnames(df_house)) {
  colnames(df_house)[colnames(df_house) == "carports"] <- "garage"
}

# 4. Hapus price_in_billion_rp lama, lalu hitung ulang dari price_in_rp
if ("price_in_billion_rp" %in% colnames(df_house)) df_house$price_in_billion_rp <- NULL
if ("price_in_rp" %in% colnames(df_house)) {
  df_house$price_in_billion_rp <- df_house$price_in_rp / 1000000000 
}

df_house$electricity <- as.character(df_house$electricity)

# --- TAHAP PREPROCESSING PEMBERSIHAN DATA ---
df_house <- df_house[!is.na(df_house$lat) & !is.na(df_house$long), ]

# Membatasi bounding box koordinat hanya di area Jabodetabek
df_house <- df_house[df_house$lat >= -6.80 & df_house$lat <= -5.90 & 
                       df_house$long >= 106.30 & df_house$long <= 107.30, ]

df_house <- df_house[!is.na(df_house$bedrooms) & df_house$bedrooms <= 10, ]

list_kota <- c("Semua Kota", sort(unique(df_house$city[!is.na(df_house$city)])))
list_sertifikat <- c("Semua Sertifikat", unique(df_house$certificate[!is.na(df_house$certificate)]))

pilih_min_harga <- min(df_house$price_in_billion_rp, na.rm = TRUE)
if (is.infinite(pilih_min_harga) || is.na(pilih_min_harga)) pilih_min_harga <- 0
pilih_max_harga <- max(df_house$price_in_billion_rp, na.rm = TRUE)
if (is.infinite(pilih_max_harga) || is.na(pilih_max_harga)) pilih_max_harga <- 50

min_luas_t <- floor(min(df_house$land_size_m2, na.rm = TRUE))
min_luas_b <- floor(min(df_house$building_size_m2, na.rm = TRUE))

