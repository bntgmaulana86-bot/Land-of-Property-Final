# ==============================================================================
# 1. LIBRARY YANG DIGUNAKAN
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
# 2. PERSIAPAN DATA, PREPROCESSING DATA, DAN MENGHAPUS DATA YANG KOSONG
# ==============================================================================

# 1. Membaca data dengan fill = TRUE untuk mengatasi error sisaan koma Excel
df_house <- read.csv("jabodetabek_house_price.csv", stringsAsFactors = FALSE, fill = TRUE)

# 2. Membersihkan "kolom hantu" (kolom kosong yang terbaca sebagai X, X.1, dst dari Excel)
df_house <- df_house[, !grepl("^X", names(df_house))]
df_house <- df_house[!is.na(df_house$price_in_billion_rp) & df_house$price_in_billion_rp <= 50, ]

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
  df_house$certificate[grepl("SHM", df_house$certificate)] <- "SHM"
  df_house$certificate[grepl("HGB", df_house$certificate)] <- "HGB"
  df_house$certificate[grepl("HP", df_house$certificate)] <- "HP"
  df_house$certificate[grepl("LAINNYA", df_house$certificate)] <- "Lainnya"
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
df_house$building_age[is.na(df_house$building_age)] <- median(df_house$building_age, na.rm = TRUE)
df_house$bedrooms[is.na(df_house$bedrooms)] <- median(df_house$bedrooms, na.rm = TRUE)
df_house$bathrooms[is.na(df_house$bathrooms)] <- median(df_house$bathrooms, na.rm = TRUE)
df_house$floors[is.na(df_house$floors)] <- median(df_house$floors, na.rm = TRUE)

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
# 3. SYNTAX BAGIAN USER INTERFACE (UI) 
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
  
  # --- FLEXBOX & GRID CSS KHUSUS UNTUK CHECKBOX AGAR TIDAK BERANTAKAN ---
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
  ),
  
  # ----------------------------------------------------------------------------
  # TAB AWAL/TAB 1 : CARI PROPERTI 
  # ----------------------------------------------------------------------------
  # Fitur Filter Properti
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
  # Fitur Tiga Kotak di atas (TOTAL PROPERTI, RATA-RATA HARGA, DAN RATA-RATA LUAS TANAH    
      div(
        layout_columns(
          fill = FALSE,
          value_box(title = "Total Properti", value = textOutput("box_total_listing"), showcase = icon("home"), theme = "primary"),
          value_box(title = "Rata-rata Harga", value = textOutput("box_avg_harga"), showcase = icon("tags"), theme = "success"),
          value_box(title = "Rata-rata Luas Tanah", value = textOutput("box_avg_m2"), showcase = icon("ruler-combined"), theme = "info")
        ),
   # FITUR PETA     
        card(
          class = "shadow-sm border-0 mt-3",
          card_header("📍 Peta Sebaran Properti (Citra Satelit & Street Map)", class = "bg-white fw-bold"),
          withSpinner(leafletOutput("peta_properti", height = "500px"), type = 4, color = "#F1C40F")
        ),
   # FITUR PLOT    
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
   # FITUR TABEL DATABASE PROPERTI     
        card(
          class = "shadow-sm border-0 mt-3 mb-4",
          card_header("📋 Database Properti Interaktif", class = "bg-white fw-bold"),
          withSpinner(DTOutput("tabel_rumah"), type = 4, color = "#F1C40F")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2 : KALKULATOR NILAI & STATISTIK REGRESI 
  # ----------------------------------------------------------------------------
  # FITUR PREDIKSI HARGA WAJAR
  nav_panel(
    "Kalkulator Nilai", icon = icon("calculator"),
    div(class = "container-fluid mt-3",
        tags$h3("Estimasi Nilai Properti & Model Kalkulasi", class = "fw-bold mb-4", style = "color: #2C3E50;"),
        
        layout_columns(
          col_widths = c(4, 8),
          
          card(
            class = "border-0 shadow-sm",
            card_header("Spesifikasi Aset Untuk Prediksi", class = "bg-light fw-bold"),
            card_body(
              numericInput("pred_luas", "Luas Tanah (m²):", value = 150, min = 10),
              numericInput("pred_luas_bangunan", "Luas Bangunan (m²):", value = 120, min = 10),
              selectInput("pred_kota", "Wilayah:", choices = NULL),
              numericInput("pred_bedrooms", "Kamar Tidur:", value = 3, min = 1),
              numericInput("pred_bathrooms", "Kamar Mandi:", value = 2, min = 1),
              numericInput("pred_floors", "Jumlah Lantai:", value = 1, min = 1),
              numericInput("pred_year_built", "Tahun Dibangun:", value = 2020),
              br(),
              actionButton("btn_prediksi", "Hitung Nilai Wajar", 
                           class = "btn-lg w-100 fw-bold shadow", 
                           style = "background-color: #F1C40F; color: #2C3E50; border: none;", 
                           icon = icon("chart-line"))
            )
          ),
     # FITUR HASIL ESTIMASI DAN RATA-RATA MARGIN KESALAHAN     
          div(
            card(
              class = "shadow-sm border-0",
              card_header("Hasil Prediksi Harga Pasaran", class = "fw-bold", style = "background-color: #F39C12; color: white;"),
              card_body(
                layout_columns(
                  col_widths = c(6, 6),
                  div(
                    tags$h6("Estimasi Harga Wajar:"),
                    tags$h2(textOutput("hasil_prediksi"), class = "fw-bold mt-2", style = "color: #2C3E50;")
                  ),
                  div(
                    tags$h6("Rata-rata Margin Kesalahan (Toleransi Meleset):", class="text-secondary"),
                    tags$h4(textOutput("metrik_error"), class="text-danger fw-bold mt-2", style="margin-bottom: 0;"),
                    tags$small("*(Harga asli di lapangan bisa lebih tinggi atau lebih rendah sekitar angka di atas dari estimasi)*", class="text-muted")
                  )
                )
              )
            ),
    # FITUR INTERPRETASI, MODEL REGRESI, DAN PLOT RESIDUAL        
            accordion(
              open = TRUE, class = "mt-3 shadow-sm",
              accordion_panel(
                "Cara Membaca Prediksi Harga Ini", icon = icon("info-circle"),
                uiOutput("interpretasi_teks")
              ),
              accordion_panel(
                "Detail Persamaan Model (Multilinear Regression)", icon = icon("superscript"),
                uiOutput("persamaan_teks")
              ),
              accordion_panel(
                "Diagnostik Sebaran Prediksi (Fitted vs Residuals)", icon = icon("chart-bar"),
                tags$p("Grafik ini melihat apakah 'tebakan' harga dari model kita meleset jauh atau tidak. Semakin titik-titik menempel di garis merah, semakin akurat modelnya.", class="text-muted"),
                withSpinner(plotlyOutput("plot_residual", height = "300px"), type = 4, color = "#F1C40F")
              )
            )
          )
        )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: STATISTIK EKSPLORATORI 
  # ----------------------------------------------------------------------------
  # FITUR BOXPLOT DAN PENJELASAN
  nav_panel(
    "Eksplorasi Statistik", icon = icon("chart-pie"),
    div(class = "container-fluid mt-3 mb-5",
        tags$h3("Analisis Outlier (Pencilan Harga)", class = "fw-bold mb-4", style = "color: #2C3E50;"),
     
        layout_columns(
          col_widths = c(12),
          card(
            class = "shadow-sm border-0",
            card_header("Distribusi Harga & Deteksi Outlier (Boxplot)", class = "bg-white fw-bold"),
            card_body(
              tags$div(
                class = "alert alert-info",
                style = "background-color: #E8F8F5; border-color: #A3E4D7; color: #117864;",
                tags$b("💡 Cara Membaca Grafik:"), tags$br(),
                "Garis tebal di dalam kotak adalah harga tengah (median) di kota tersebut. Sedangkan ", 
                tags$b("titik-titik merah di luar kotak"), " merupakan properti pencilan dengan harga yang melampaui standar harga pasar wajar di wilayah tersebut. Rincian angka dapat dilihat pada tabel di bawah."
              ),
              withSpinner(plotlyOutput("plot_boxplot", height = "450px"), type = 4, color = "#F1C40F")
            )
          )
        ),
  # FITUR TABEL RANGKUMAN ANGKA PENTING SETIAP DAERAH   
        card(
          class = "shadow-sm border-0 mt-3",
          card_header("Detail Angka Boxplot (Statistika Deskriptif Per Wilayah)", class = "bg-white fw-bold"),
          card_body(
            withSpinner(DTOutput("tabel_deskriptif"), type = 4, color = "#2C3E50")
          )
        )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 4: TAMBAH PROPERTI BAGI USER SEBAGAI PENJUAL
  # ----------------------------------------------------------------------------
  # FITUR INPUT DATA PROPERTI
  nav_panel(
    "Tambah Properti", icon = icon("plus-circle"),
    div(class = "container mt-4 mb-5", style = "max-width: 900px;",
        card(
          class = "shadow-lg border-0",
          card_header("Tambah Properti Baru", class = "bg-white fw-bold text-center fs-4 py-3"),
          card_body(
            class = "p-4",
            layout_columns(
              col_widths = c(6, 6),
              textInput("title", "Nama Properti"),
              selectInput("property_type", "Jenis Properti", choices = c("Rumah", "Townhouse", "Villa", "Apartemen")),
              numericInput("price_in_rp", "Harga Jual (Rp)", value = 1500000000, min = 0, step = 50000000),
              selectInput("certificate", "Jenis Sertifikat", choices = c("SHM", "HGB", "HP", "Lainnya")),
              selectInput("city", "Kota / Wilayah", choices = list_kota[-1]),
              textInput("district", "Kecamatan")
            ),
            
            textAreaInput("address", "Alamat Lengkap (Jl, RT/RW, Patokan)", rows = 2, width = "100%"),
            
            textInput("url", "Link / URL Website Sumber Properti (Misal: Rumah123 / Lamudi):", 
                      placeholder = "https://www.rumah123.com/properti/...", width = "100%"),
   # FITUR PINPOINT PROPERTI         
            hr(class = "my-4 border-2"),
            tags$h5(icon("map-marker-alt"), " Pin Lokasi Properti", class = "fw-bold mb-3 text-secondary"),
            tags$p("Klik area pada peta di bawah ini untuk menentukan titik koordinat Latitude dan Longitude secara otomatis.", class="text-muted"),
            
            leafletOutput("map_input", height = "300px"),
            br(),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("lat", "📍 Latitude Koordinat", value = -6.200000, step = 0.000001),
              numericInput("long", "📍 Longitude Koordinat", value = 106.800000, step = 0.000001)
            ),
    # FITUR SPESIFIKASI FISIK DAN FASILITAS PENDUKUNG PROPERTI        
            hr(class = "my-4 border-2"),
            tags$h5(icon("tools"), " Spesifikasi Fisik & Fasilitas", class = "fw-bold mb-3 text-secondary"),
            
            layout_columns(
              col_widths = 3, 
              numericInput("land_size_m2", "L. Tanah (m²)", value = 120),
              numericInput("building_size_m2", "L. Bangunan (m²)", value = 90),
              numericInput("bedrooms", "Kamar Tidur", value = 3, min = 1),
              numericInput("bathrooms", "Kamar Mandi", value = 2, min = 1),
              
              numericInput("maid_bedrooms", "K. Tidur ART", value = 0, min = 0),
              numericInput("maid_bathrooms", "K. Mandi ART", value = 0, min = 0),
              numericInput("floors", "Jumlah Lantai", value = 1),
              numericInput("garage", "Kapasitas Garasi", value = 1),
              
              selectizeInput("electricity", "Daya Listrik (VA)", 
                             choices = c("450", "900", "1300", "2200", "3500", "4400", "5500", "6600"), 
                             selected = "2200", 
                             options = list(create = TRUE, placeholder = "Ketik/Pilih Daya")),
              numericInput("year_built", "Tahun Dibangun", value = as.numeric(format(Sys.Date(), "%Y"))),
              selectInput("property_condition", "Kondisi Properti", choices = c("Baru", "Bagus", "Renovasi", "Butuh Renovasi")),
              selectInput("building_orientation", "Orientasi Bangunan", choices = c("Utara", "Timur Laut", "Timur", "Tenggara", "Selatan", "Barat Daya", "Barat", "Barat Laut", "Tidak Diketahui")),
              
              selectInput("furnishing", "Kondisi Perabotan:", choices = c("Full Furnished", "Semi Furnished", "Unfurnished"), selected = "Unfurnished")
            ),
            
            div(class = "mt-3 p-3 rounded shadow-sm border", style = "background-color: #F8F9FA;",
                tags$label(icon("list-check"), " Fasilitas Pendukung:", class = "fw-bold mb-2"),
                
                checkboxGroupInput("facilities", label = NULL, 
                                   choices = c("AC", "Kolam Renang", "Taman", "CCTV", 
                                               "Keamanan 24 Jam", "Gym", "Internet", 
                                               "Water Heater", "Balkon", "Smart Home",
                                               "Ruang Cuci", "Dapur Bersih", "Dapur Kotor", 
                                               "Gudang", "Bebas Banjir", "Panel Surya"), 
                                   inline = FALSE),
                
                hr(style = "border-top: 1px dashed #ccc; margin: 15px 0;"),
                
                textInput("facilities_custom", 
                          label = "Tambahkan Fasilitas Lainnya (Jika tidak ada di atas):", 
                          placeholder = "Contoh: Jacuzzi, Lift, Helipad (pisahkan dengan koma jika lebih dari satu)",
                          width = "100%")
            ),
   # FITUR SIMPAN PROPERTI BARU          
            br(),
            actionButton("simpan", "Simpan Properti", 
                         class = "btn-lg w-100 fw-bold shadow py-3", 
                         style = "background-color: #2C3E50; color: white;", icon = icon("save"))
          )
        )
    )
  )
)

# ==============================================================================
# 4. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(df = df_house)
  
  observeEvent(input$simpan, {
    if (input$city == "" || input$title == "") {
      showNotification("Gagal: Lengkapi Kota dan Nama Properti!", type = "error")
      return()
    }
    
    fasilitas_terpilih <- input$facilities
    fasilitas_custom <- trimws(input$facilities_custom)
    semua_fasilitas <- c(fasilitas_terpilih)
    
    if (!is.null(fasilitas_custom) && fasilitas_custom != "") {
      semua_fasilitas <- c(semua_fasilitas, fasilitas_custom)
    }
    
    fasilitas_final <- paste(semua_fasilitas, collapse = ", ")
    if (fasilitas_final == "") fasilitas_final <- NA 
    
    tryCatch({
      data_baru <- data.frame(
        url = trimws(input$url),  
        price_in_rp = input$price_in_rp,
        title = input$title,
        address = input$address,
        district = input$district,
        city = input$city,
        lat = input$lat,
        long = input$long,
        facilities = fasilitas_final,  
        property_type = input$property_type,
        ads_id = paste0("ADS", format(Sys.time(), "%Y%m%d%H%M%S")),
        bedrooms = input$bedrooms,
        bathrooms = input$bathrooms,
        land_size_m2 = input$land_size_m2,
        building_size_m2 = input$building_size_m2,
        garage = input$garage, 
        certificate = input$certificate,
        electricity = input$electricity, 
        maid_bedrooms = input$maid_bedrooms,       
        maid_bathrooms = input$maid_bathrooms,     
        floors = input$floors,
        building_age = as.numeric(format(Sys.Date(), "%Y")) - input$year_built,
        year_built = input$year_built,
        property_condition = input$property_condition,       
        building_orientation = input$building_orientation,   
        furnishing = input$furnishing, 
        price_in_billion_rp = input$price_in_rp / 1000000000,
        stringsAsFactors = FALSE
      )
      
      rv$df <- bind_rows(rv$df, data_baru)
      
      write.csv(rv$df, "jabodetabek_house_price.csv", row.names = FALSE)
      
      showNotification("Properti baru berhasil disimpan dan Data Dashboard telah diperbarui!", type = "message")
      
      updateTextInput(session, "url", value = "")
      updateTextInput(session, "title", value = "")
      updateTextInput(session, "address", value = "")
      updateTextInput(session, "district", value = "")
      updateTextInput(session, "facilities_custom", value = "")
      updateCheckboxGroupInput(session, "facilities", selected = character(0))
      updateSelectInput(session, "furnishing", selected = "Unfurnished")
      
    }, error = function(e) {
      if (grepl("cannot open the connection", e$message)) {
        showNotification("Gagal menyimpan: Pastikan file 'jabodetabek_house_price.csv' tidak sedang dibuka di Excel!", type = "error", duration = 7)
      } else {
        showNotification(paste("Terjadi kesalahan saat menyimpan:", e$message), type = "error")
      }
    })
  })
  
  output$map_input <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 106.8229, lat = -6.2088, zoom = 10) %>% 
      addMarkers(lng = 106.800000, lat = -6.200000, layerId = "pin_lokasi")
  })
  
  observeEvent(input$map_input_click, {
    click <- input$map_input_click
    updateNumericInput(session, "lat", value = round(click$lat, 6))
    updateNumericInput(session, "long", value = round(click$lng, 6))
    leafletProxy("map_input") %>% clearMarkers() %>% addMarkers(lng = click$lng, lat = click$lat, layerId = "pin_lokasi")
  })
  
  observeEvent(c(input$lat, input$long), {
    req(input$lat, input$long)
    leafletProxy("map_input") %>% clearMarkers() %>% addMarkers(lng = input$long, lat = input$lat, layerId = "pin_lokasi")
  })
  
  data_terfilter <- reactive({
    df <- rv$df
    if (input$filter_kota != "Semua Kota") df <- df[df$city == input$filter_kota, ]
    df <- df[!is.na(df$price_in_billion_rp) & df$price_in_billion_rp >= input$filter_harga[1] & df$price_in_billion_rp <= input$filter_harga[2], ]
    df <- df[!is.na(df$bedrooms) & df$bedrooms >= input$filter_kamar[1] & df$bedrooms <= input$filter_kamar[2], ]
    if (input$filter_sertifikat != "Semua Sertifikat") df <- df[!is.na(df$certificate) & df$certificate == input$filter_sertifikat, ]
    df <- df[!is.na(df$land_size_m2) & df$land_size_m2 >= input$filter_luas_tanah[1] & df$land_size_m2 <= input$filter_luas_tanah[2], ]
    df <- df[!is.na(df$building_size_m2) & df$building_size_m2 >= input$filter_luas_bangunan[1] & df$building_size_m2 <= input$filter_luas_bangunan[2], ]
    return(df)
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("Data_Properti_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(data_terfilter(), file, row.names = FALSE) }
  )
  
  output$box_total_listing <- renderText({ format(nrow(data_terfilter()), big.mark = ".", decimal.mark = ",") })
  output$box_avg_harga <- renderText({
    rata_harga <- mean(data_terfilter()$price_in_billion_rp, na.rm = TRUE)
    if(is.nan(rata_harga)) rata_harga <- 0
    paste0("Rp ", format(round(rata_harga, 2), big.mark = ".", decimal.mark = ","), " M")
  })
  output$box_avg_m2 <- renderText({
    rata_luas <- mean(data_terfilter()$land_size_m2, na.rm = TRUE) 
    if(is.nan(rata_luas)) rata_luas <- 0
    paste0(format(round(rata_luas, 0), big.mark = ".", decimal.mark = ","), " m²")
  })
  
  # --- PERBAIKAN UTAMA: MENGGUNAKAN PROVIDER SATELIT RESMI YANG VALID ---
  output$peta_properti <- renderLeaflet({
    req(data_terfilter())
    leaflet(data_terfilter()) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satelit") %>% 
      addCircleMarkers(
        lng = ~long, lat = ~lat, color = "#e74c3c", fillColor = "#e74c3c", radius = 5, fillOpacity = 0.8, weight = 1,
        popup = ~paste0(
          "<div style='font-family: Arial, sans-serif; min-width: 180px;'>",
          "<h5><b>", title, "</b></h5>",
          "<b>Wilayah:</b> ", city, "<br>",
          "<b>Harga Jual:</b> Rp ", round(price_in_billion_rp, 2), " M<br>",
          "<b>Spesifikasi:</b> LT ", land_size_m2, " m² | LB ", building_size_m2, " m²<br>",
          ifelse(!is.na(url) & url != "", 
                 paste0("<br><a href='", url, "' target='_blank' class='btn btn-warning btn-sm text-dark fw-bold w-100' style='padding: 4px; font-size: 11px; text-decoration: none; border-radius: 4px; display: inline-block; text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>🌐 Kunjungi Halaman Website</a>"), 
                 "<br><i class='text-muted' style='font-size:11px;'>Tidak ada link tautan tersedia</i>"),
          "</div>"
        ),
        group = "Titik Properti"
      ) %>%
      addLayersControl(baseGroups = c("Street Map", "Satelit"), overlayGroups = c("Titik Properti"), options = layersControlOptions(collapsed = FALSE))
  })
  
  output$plot_harga_kota <- renderPlotly({
    req(nrow(data_terfilter()) > 0)
    df_bar <- data_terfilter()
    df_agg <- aggregate(price_in_billion_rp ~ city, data = df_bar, FUN = max)
    counts <- table(df_bar$city)
    df_agg$city_label <- paste0(df_agg$city, "<br>(", counts[df_agg$city], ")")
    df_bar$city_label <- paste0(df_bar$city, "<br>(", counts[df_bar$city], ")")
    
    plot_ly() %>%
      add_bars(data = df_agg, x = ~city_label, y = ~price_in_billion_rp, name = 'Maksimum', marker = list(color = '#5DADE2', opacity = 0.9), text = ~paste('Rp', round(price_in_billion_rp, 2), 'M'), hoverinfo = "text") %>%
      add_markers(data = df_bar, x = ~city_label, y = ~price_in_billion_rp, name = 'Sebaran', marker = list(color = '#E67E22', size = 16, symbol = 'line-ew', line = list(width = 2)), text = ~paste("Rp", round(price_in_billion_rp, 2), "M"), hoverinfo = "text") %>%
      layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = "Harga (Miliar Rp)"), margin = list(b = 60), showlegend = TRUE, plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$plot_scatter_harga <- renderPlotly({
    req(nrow(data_terfilter()) > 0)
    g <- ggplot(data_terfilter(), aes(x = land_size_m2, y = price_in_billion_rp, color = city, text = paste("Kota:", city, "<br>Rp", round(price_in_billion_rp, 2), "M<br>Luas:", land_size_m2, "m²"))) +
      geom_point(alpha = 0.7, size = 2.5) + geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "#2C3E50", linewidth = 1.2, linetype = "dashed") + 
      theme_minimal() + labs(x = "Luas Tanah (m²)", y = "Harga Jual (Miliar Rp)", color = "Wilayah")
    ggplotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })
  
  output$tabel_rumah <- renderDT({
    datatable(data_terfilter()[, c("city", "price_in_rp", "bedrooms", "building_size_m2", "land_size_m2", "certificate")], 
              options = list(pageLength = 10, scrollX = TRUE), colnames = c("Kota", "Harga (Rp)", "K. Tidur", "LB (m²)", "LT (m²)", "Sertifikat"), class = 'table-striped table-hover table-bordered') %>%
      formatCurrency("price_in_rp", currency = "Rp ", interval = 3, mark = ".", digits = 0)
  })
  
  # ==============================================================================
  # LOGIKA MODEL REGRESI (MULTILINEAR BERBASIS LUAS TANAH + LUAS BANGUNAN)
  # ==============================================================================
  observe({
    df <- rv$df
    updateSelectInput(session, "pred_kota", choices = sort(unique(df$city[!is.na(df$city)])))
  })
  
  model_regresi <- reactive({
    lm(price_in_billion_rp ~ land_size_m2 + building_size_m2 + city + 
         bedrooms + bathrooms + floors + building_age, data = rv$df)
  })
  
  angka_prediksi <- eventReactive(input$btn_prediksi, {
    mod <- model_regresi()
    data_baru <- data.frame(
      land_size_m2 = input$pred_luas, 
      building_size_m2 = input$pred_luas_bangunan, 
      city = input$pred_kota,
      bedrooms = input$pred_bedrooms,
      bathrooms = input$pred_bathrooms,
      floors = input$pred_floors,
      building_age = as.numeric(format(Sys.Date(), "%Y")) - input$pred_year_built
    )
    pred <- predict(mod, newdata = data_baru)
    if(pred < 0) return(0) else return(pred)
  }, ignoreNULL = FALSE)
  
  output$hasil_prediksi <- renderText({ paste("Rp", format(round(angka_prediksi(), 2), big.mark = ".", decimal.mark = ","), "M") })
  
  output$metrik_error <- renderText({
    mod <- model_regresi()
    mae_miliar <- mean(abs(residuals(mod)), na.rm = TRUE)
    mae_juta <- mae_miliar * 1000
    paste0("± Rp ", format(round(mae_juta, 0), big.mark = ".", decimal.mark = ","), " Juta")
  })
  
 output$persamaan_teks <- renderUI({
    mod <- model_regresi()
    coefs <- coef(mod)
    
    # Membuat teks dinamis berdasarkan semua koefisien yang ada di model
    teks_model <- paste(
      round(coefs[1], 2), # Intercept
      paste(
        sapply(2:length(coefs), function(i) {
          paste0(ifelse(coefs[i] >= 0, "+ ", "- "), abs(round(coefs[i], 3)), "(", names(coefs)[i], ")")
        }), 
        collapse = " "
      )
    )
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 15px; border-left: 5px solid #F39C12; font-family: monospace;'>",
      "<b>Y = ", teks_model, "</b> </div>"
    ))
  })
  
  output$interpretasi_teks <- renderUI({
    mod <- model_regresi()
    koef_luas_t <- round(coef(mod)["land_size_m2"] * 1000, 1)
    koef_luas_b <- round(coef(mod)["building_size_m2"] * 1000, 1) 
    if(is.na(koef_luas_t)) koef_luas_t <- 0
    if(is.na(koef_luas_b)) koef_luas_b <- 0
    
    semua_kota <- unique(rv$df$city[!is.na(rv$df$city)])

    df_uji <- data.frame(
      land_size_m2 = median(rv$df$land_size_m2, na.rm=TRUE), 
      building_size_m2 = median(rv$df$building_size_m2, na.rm=TRUE), 
      city = semua_kota,
      bedrooms = median(rv$df$bedrooms, na.rm=TRUE),
      bathrooms = median(rv$df$bathrooms, na.rm=TRUE),
      floors = median(rv$df$floors, na.rm=TRUE),
      building_age = median(rv$df$building_age, na.rm=TRUE)
    )
    
    df_uji$pred <- predict(mod, newdata = df_uji)
    kota_termahal <- df_uji$city[which.max(df_uji$pred)]
    
    HTML(paste(
      "<div style='font-size: 15px; padding: 10px;'>",
      "<p style='margin-bottom: 15px;'><b>1. Logika Pengaruh Fisik (Luas Tanah & Bangunan):</b><br>",
      "• Setiap penambahan luas tanah <b>1 m²</b> akan berkontribusi menaikkan harga rumah sekitar <b>Rp ", format(koef_luas_t, big.mark = ".", decimal.mark = ","), " Juta</b>.<br>",
      "• Setiap penambahan luas bangunan <b>1 m²</b> akan berkontribusi menaikkan harga rumah sekitar <b>Rp ", format(koef_luas_b, big.mark = ".", decimal.mark = ","), " Juta</b>.</p>",
      "<p style='margin-bottom: 0;'><b>2. Faktor Wilayah:</b><br>",
      "Berdasarkan sebaran data saat ini, dengan asumsi ukuran fisik rumah yang sama, wilayah <b>", kota_termahal, "</b> memegang standar harga dasar pasaran tertinggi di antara area Jabodetabek lainnya.</p>",
      "</div>"
    ))
  })
  
  output$plot_residual <- renderPlotly({
    df_diag <- data.frame(Fitted = fitted(model_regresi()), Residuals = residuals(model_regresi()))
    p <- ggplot(df_diag, aes(x = Fitted, y = Residuals)) + geom_point(alpha = 0.5, color = "#5DADE2") +
      geom_hline(yintercept = 0, color = "#e74c3c", linetype = "dashed", linewidth = 1.2) + theme_minimal() +
      labs(x = "Nilai Prediksi Harga", y = "Jarak Meleset (Residuals)")
    ggplotly(p)
  })
  
  # ==============================================================================
  # LOGIKA TAB 3: STATISTIK EKSPLORATORI 
  # ==============================================================================
  output$plot_boxplot <- renderPlotly({
    df_box <- rv$df %>% filter(!is.na(city) & !is.na(price_in_billion_rp))
    kota_order <- df_box %>% group_by(city) %>% summarise(med = median(price_in_billion_rp, na.rm = TRUE)) %>% arrange(med) %>% pull(city)
    df_box$city <- factor(df_box$city, levels = kota_order)
    
    plot_ly(data = df_box, x = ~price_in_billion_rp, y = ~city, color = ~city, type = "box", colors = "viridis", hoverinfo = "none", 
            marker = list(color = '#e74c3c', size = 6, symbol = "circle", opacity = 0.8), line = list(width = 1.5)) %>%
      layout(xaxis = list(title = "Harga (Miliar Rp)", zeroline = FALSE), yaxis = list(title = "Wilayah", zeroline = FALSE), showlegend = FALSE, margin = list(l = 100))
  })
  
  output$tabel_deskriptif <- renderDT({
    df_stat <- rv$df %>%
      filter(!is.na(price_in_billion_rp) & !is.na(city)) %>%
      group_by(city) %>%
      summarise(Total = n(), Min = min(price_in_billion_rp), Q1 = quantile(price_in_billion_rp, 0.25), Median = median(price_in_billion_rp),
                Rata_rata = mean(price_in_billion_rp), Q3 = quantile(price_in_billion_rp, 0.75), Max = max(price_in_billion_rp)) %>%
      mutate(across(c(Min, Q1, Median, Rata_rata, Q3, Max), ~round(.x, 2))) %>%
      arrange(desc(Median)) %>% rename(Wilayah = city)
    
    datatable(df_stat, options = list(pageLength = 8, scrollX = TRUE, dom = 'Brtip'), class = 'table-bordered table-striped', rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
