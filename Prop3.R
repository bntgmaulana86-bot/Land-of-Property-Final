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
  ),
  
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
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2: KALKULATOR NILAI & STATISTIK REGRESI 
  # ----------------------------------------------------------------------------
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
              br(),
              actionButton("btn_prediksi", "Hitung Nilai Wajar", 
                           class = "btn-lg w-100 fw-bold shadow", 
                           style = "background-color: #F1C40F; color: #2C3E50; border: none;", 
                           icon = icon("chart-line"))
            )
          ),
          
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
  # TAB 4: TAMBAH PROPERTI BARU 
  # ----------------------------------------------------------------------------
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
      addProviderTiles(providers$Esri.WorldImagery, group = "Satelit") %>% # <-- Sudah diperbaiki di sini
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