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

# ==============================================================================
# 3. USER INTERFACE (UI) 
# ==============================================================================
tema_properti <- bs_theme(
  version = 5,
  bg = "#F4F6F9",                
  fg = "#2C3E50",                
  primary = "#F1C40F",          
  secondary = "#34495E",        
  base_font = font_google("Inter")
)

ui <- page_navbar(
  title = tags$span(icon("building"), " Land Of Property", style = "font-weight: 800; color: #2C3E50; letter-spacing: 0.5px;"),
  theme = tema_properti,
  id = "nav_utama",
  fillable = TRUE,
)
  # --- FLEXBOX & GRID CSS KHUSUS UNTUK CHECKBOX ANTI-BERANTAKAN ---
  tags$head(
    tags$style(HTML("
      #facilities {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
        gap: 12px;
        padding: 4px;
      }
      #facilities .form-check {
        display: flex !important;
        align-items: center !important;
        margin: 0 !important;
        padding-left: 0 !important;
      }
      #facilities .form-check-input {
        margin-left: 0 !important;
        margin-right: 8px !important;
        float: none !important;
        flex-shrink: 0; 
      }
      #facilities .form-check-label {
        white-space: nowrap !important; 
      }
    "))
  )
  
  # ----------------------------------------------------------------------------
  # TAB 1: CARI PROPERTI 
  # ----------------------------------------------------------------------------
  nav_panel(
    "Cari Properti", icon = icon("search"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        bg = "#2C3E50", 
        fg = "white",
        tags$h5(icon("sliders-h"), " Filter Pencarian", class = "mb-4 text-warning fw-bold"),
        
        selectInput("filter_kota", "📍 Lokasi / Wilayah:", choices = list_kota, selected = "Semua Kota"),
        selectInput("filter_sertifikat", "📜 Jenis Sertifikat:", choices = list_sertifikat, selected = "Semua Sertifikat"),
        
        hr(style = "border-color: #7f8c8d; margin-top: 20px; margin-bottom: 20px;"),
        
        sliderInput("filter_harga", "💰 Harga Jual (Miliar Rp):", min = 0, max = 50, value = c(pilih_min_harga, 15), step = 0.5),
        sliderInput("filter_kamar", "🛏️ Kamar Tidur:", min = 1, max = 10, value = c(1, 10), step = 1),
        sliderInput("filter_luas_tanah", "📏 Luas Tanah (m²):", min = min_luas_t, max = 1000, value = c(10, 1000), step = 10),
        sliderInput("filter_luas_bangunan", "🏢 Luas Bangunan (m²):", min = min_luas_b, max = 1000, value = c(10, 1000), step = 10),
        
        br(),
        downloadButton("download_data", "Export Data (CSV)", class = "btn-warning w-100 fw-bold shadow-sm")
      ),
      
      div(
        layout_columns(
          fill = FALSE,
          value_box(title = "Total Properti", value = textOutput("box_total_listing"), showcase = icon("home"), theme = "primary"),
          value_box(title = "Rata-rata Harga", value = textOutput("box_avg_harga"), showcase = icon("tags"), theme = "success"),
          value_box(title = "Rata-rata Luas Tanah", value = textOutput("box_avg_m2"), showcase = icon("ruler-combined"), theme = "info")
        ),
        
        card(
          class = "shadow-sm border-0 mt-3",
          card_header("📍 Peta Sebaran Properti (Citra Satelit & Street Map)", class = "bg-white fw-bold"),
          withSpinner(leafletOutput("peta_properti", height = "500px"), type = 4, color = "#F1C40F")
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "shadow-sm border-0 mt-3",
            card_header("📊 Distribusi Harga Properti Per Kota", class = "bg-white fw-bold"),
            withSpinner(plotlyOutput("plot_harga_kota", height = "450px"), type = 4, color = "#34495E")
          ),
          card(
            class = "shadow-sm border-0 mt-3",
            card_header("📈 Tren Harga vs Luas Tanah", class = "bg-white fw-bold"),
            withSpinner(plotlyOutput("plot_scatter_harga", height = "450px"), type = 4, color = "#34495E")
          )
        ),
        
        card(
          class = "shadow-sm border-0 mt-3 mb-4",
          card_header("📋 Database Properti Interaktif", class = "bg-white fw-bold"),
          withSpinner(DTOutput("tabel_rumah"), type = 4, color = "#F1C40F")
        )
      )
    )
  )
  
  # ----------------------------------------------------------------------------
  # TAB 2: KALKULATOR NILAI & STATISTIK REGRESI 
  # ----------------------------------------------------------------------------