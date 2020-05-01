library(shiny)
library(shinyMobile)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(leaflet)
library(DT)
library(curl)
library(scales)

trend <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features$attributes %>%
  mutate(Tanggal = as_date(as_datetime(Tanggal/1000, tz = "Asia/Jakarta")),
         Pembaruan_Terakhir = as_datetime(Pembaruan_Terakhir/1000, tz = "Asia/Jakarta")) %>%
  # select(-Hari_ke, -FID, -Penambahan_Kasus_Terkini) %>%
  filter(!is.na(Jumlah_Kasus_Kumulatif))

pembaruan <- format(trend$Pembaruan_Terakhir[1] - 7*60*60, "Pembaruan Data Terakhir<br/>%d %B %Y %H:%M WIB")

prov <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/COVID19_Indonesia_per_Provinsi/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features
prov <- bind_cols(prov$attributes, prov$geometry) %>%
  filter(Provinsi != "Indonesia") %>%
  select(-Kode_Provi, -FID) %>%
  mutate(CFR = round(Kasus_Meni/Kasus_Posi*100, 2),
         `Ratio Sembuh` = round(Kasus_Semb/Kasus_Posi*100, 2))

trendprov <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Harian_per_Provinsi_COVID19_Indonesia_Rev/FeatureServer/0/query?outFields=*&where=1%3D1&outFields=*&outSR=4326&f=json")
trendprov <- trendprov$features$attributes %>%
  mutate(Tanggal = as_date(as_datetime(Tanggal/1000, tz = "Asia/Jakarta"))) %>%
  select(Provinsi, Tanggal, Kasus_Terkonfirmasi_Akumulatif, Penambahan_Harian_Kasus_Terkonf, Kasus_Sembuh_Akumulatif, Penambahan_Harian_Kasus_Sembuh, Kasus_Meninggal_Akumulatif, Penambahan_Harian_Kasus_Meningg)

ui <- f7Page(
  tags$style('body{background-color:#ffffff;}'),
  title = "Covid19 Monitoring Mobile Apps",
  f7TabLayout(
    # navbar is mandatory
    navbar = f7Navbar(
      title = "Covid19 Monitoring Mobile Apps", 
      hairline = FALSE,
      shadow = TRUE, left_panel = F, right_panel = F
    ), 
    f7Tabs(animated = TRUE,
           f7Tab(
             tabName = "Beranda",
             icon = icon("home"), active = TRUE,
             img(src = "CV-Header-2.jpg", width = "100%"),
             f7Card(
                    h3(HTML(pembaruan)),
                    fluidRow(
                      img(src = "StarCoreLow.png", width = "59%"),
                      img(src = "Covid19.jpg", width = "40%")
                      ),
                    h5("Sumber data: https://bnpb-inacovid19.hub.arcgis.com/")
                    )
           ),
           f7Tab(
             tabName = "Hari Ini",
             icon = icon("newspaper"),
             # br(),
             uiOutput("stats"),
             uiOutput("cfr_gauge"),
             uiOutput("harian")
             ), 
           f7Tab(
             tabName = "Provinsi", icon = icon("map-marked-alt"),
             f7Card(
               title = "Jumlah Kasus Terkonfirmasi per Provinsi",
               leafletOutput("provmap", height = 200)
               ),
             f7Card(
               f7Select(inputId = "provnm", label = "Provinsi", choices = sort(prov$Provinsi))
               ),
             f7Card(title = "Statistik Kasus Provinsi",
               dataTableOutput("provstats"),
               dataTableOutput("provcfr")
             ),
             f7Card(title = "Tren Kasus Provinsi",
                    plotOutput("trenprov")
             )
             ), 
           f7Tab(
             tabName = "Tren Kasus", icon = icon("chart-line"),
             f7Card(
               title = "Tren Kasus Nasional",
               plotOutput("trennasional", height = 350)
               ),
             f7Card(
               title = "Provinsi dengan Kasus Positif Terbanyak",
               plotOutput("provbar")
               ),
             f7Card(
               title = "Tren Rasio Sembuh vs Meninggal",
               plotOutput("trenratio", height = 350)
               )
             )
           )
  )
)

server <- function(input, output, session){
  limtren <- c(min(trend$Tanggal), if_else(day(max(trend$Tanggal)) < 15, ymd(paste(year(max(trend$Tanggal)), month(max(trend$Tanggal)), 15, sep = "-")), max(trend$Tanggal) %m+% months(1) %>% rollback() + 1))
  
  tren <- reactive({
    trend %>%
      select(Tanggal, Jumlah_Kasus_Kumulatif, Jumlah_Pasien_Sembuh, Jumlah_Pasien_Meninggal, Jumlah_pasien_dalam_perawatan) %>% 
      pivot_longer(cols = c(Jumlah_Kasus_Kumulatif, Jumlah_Pasien_Sembuh, Jumlah_Pasien_Meninggal, Jumlah_pasien_dalam_perawatan), names_to = "Kasus") %>% 
      mutate(Kasus = str_to_title(gsub("Jumlah ", "", gsub("_", " ", Kasus)), locale = "id"),
             Kasus = factor(case_when(Kasus == "Kasus Kumulatif" ~ "Positif",
                                 Kasus == "Pasien Dalam Perawatan" ~ "Dirawat",
                                 Kasus == "Pasien Meninggal" ~ "Meninggal",
                                 Kasus == "Pasien Sembuh" ~ "Sembuh"), 
                            levels = c("Positif", "Dirawat", "Sembuh", "Meninggal"))
             ) 
  })
  
  output$trennasional <- renderPlot({
    
    tren() %>% 
      ggplot(aes(x = Tanggal, y = value, color = Kasus)) +
      geom_line(alpha = 0.7) + 
      geom_point(alpha = 0.7, size = 2) + 
      # geom_text(aes(x = lbl$Tanggal[lbl$Kasus == "Positif"], y = lbl$value[lbl$Kasus == "Positif"], 
      #               color = "red", label = lbl$value[lbl$Kasus == "Positif"])) + 
      scale_color_manual(values = c("red", "grey", "lightgreen", "darkred")) + 
      scale_x_date(date_labels = "%d\n%b\n%y", limits = limtren) + 
      scale_y_continuous(breaks = seq(0, max(tren()$value) + 1000, by = 1000), limits = c(0, max(tren()$value) + 1000)) + 
      labs(color = "Kasus", y = "Banyaknya Kasus") + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text.align = 0)
  })
  
  ratio <- reactive({
    trend %>%
      select(Tanggal, Persentase_Pasien_dalam_Perawatan, Persentase_Pasien_Sembuh, Persentase_Pasien_Meninggal) %>% 
      pivot_longer(cols = c(Persentase_Pasien_dalam_Perawatan, Persentase_Pasien_Sembuh, Persentase_Pasien_Meninggal), names_to = "Kasus") %>% 
      mutate(Kasus = str_to_title(gsub("Persentase ", "", gsub("_", " ", Kasus)), locale = "id"),
             Kasus = factor(case_when(Kasus == "Pasien Dalam Perawatan" ~ "Dirawat",
                                      Kasus == "Pasien Meninggal" ~ "Meninggal",
                                      Kasus == "Pasien Sembuh" ~ "Sembuh"), 
                            levels = c("Dirawat", "Sembuh", "Meninggal"))
      ) 
  })
  
  output$trenratio <- renderPlot({
    limtren <- c(min(ratio()$Tanggal), if_else(day(max(trend$Tanggal)) < 15, ymd(paste(year(max(trend$Tanggal)), month(max(trend$Tanggal)), 15, sep = "-")), max(trend$Tanggal) %m+% months(1) %>% rollback() + 1))
    
    ratio() %>% 
      # filter(Kasus != "Dirawat") %>% 
      ggplot(aes(x = Tanggal, y = value/100, color = Kasus)) +
      geom_line(alpha = 0.7) + 
      geom_point(alpha = 0.7, size = 2) + 
      # geom_text(aes(x = lbl$Tanggal[lbl$Kasus == "Positif"], y = lbl$value[lbl$Kasus == "Positif"], 
      #               color = "red", label = lbl$value[lbl$Kasus == "Positif"])) + 
      scale_color_manual(values = c("grey", "lightgreen", "darkred")) + 
      scale_x_date(date_labels = "%d\n%b\n%y", limits = limtren) + 
      scale_y_continuous(labels = percent_format(1), breaks = seq(0, 1, by = 0.10)) +
      labs(color = "Kasus", y = "Rasio Terhadap Kasus Terkonfirmasi") + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text.align = 0)
  })
  
  output$cfr_gauge <- renderUI({
    cfrval <- round(last(trend$Persentase_Pasien_Meninggal), 2)
    cfrcolr <- if_else(cfrval >= 10.0, "darkred", 
                    if_else(5.0 <= cfrval & cfrval < 10.0, "red",
                            if_else(3.0 <= cfrval & cfrval < 5.0, "orange", 
                                    if_else(1.0 <= cfrval & cfrval < 3.0, "green", "blue"))))
    
    mrval <- round(last(trend$Persentase_Pasien_Sembuh), 2)
    mrcolr <- if_else(mrval >= 20.0, "blue", 
                       if_else(10.0 <= mrval & mrval < 20.0, "green",
                               if_else(5.0 <= mrval & mrval < 10.0, "orange", 
                                       if_else(1.0 <= mrval & mrval < 5.0, "red", "darkred"))))
    
    f7Card(
      div(style="display: flex;flex-align: center;",
        column(6,
               h4("Sembuh"),
               f7Gauge(id = "mr", type = "semicircle", 
                value = mrval, valueText = paste0(formatC(mrval, big.mark = ".", decimal.mark = ","), "%"), 
                labelText = sprintf("%s (+%s)", 
                                    formatC(last(trend$Jumlah_Pasien_Sembuh), big.mark = ".", decimal.mark = ","), 
                                    formatC(last(trend$Jumlah_Kasus_Sembuh_per_Hari), big.mark = ".", decimal.mark = ",")), 
                valueTextColor = mrcolr, borderColor = mrcolr, valueFontSize = 20)
               ),
        column(6,
               h4("Meninggal"),
               f7Gauge(id = "cfr", type = "semicircle", 
                value = cfrval, valueText = paste0(formatC(cfrval, big.mark = ".", decimal.mark = ","), "%"), 
                labelText = sprintf("%s (+%s)", 
                                    formatC(last(trend$Jumlah_Pasien_Meninggal), big.mark = ".", decimal.mark = ","), 
                                    formatC(last(trend$Jumlah_Kasus_Meninggal_per_Hari), big.mark = ".", decimal.mark = ",")), 
                valueTextColor = cfrcolr, borderColor = cfrcolr, valueFontSize = 20)
               )
      )
    )
  })
  
  output$stats <- renderUI({
    konfirmasi <- last(trend$Jumlah_Kasus_Kumulatif)
    kasusbaru <- last(trend$Jumlah_Kasus_Baru_per_Hari)
    perawatan <- last(trend$Jumlah_pasien_dalam_perawatan)
    pctperawatan <- round(last(trend$Persentase_Pasien_dalam_Perawatan), 2)
    tagList(
      div(style="display: inline-block;width:49.5%",
             f7Card(
               h5("Terkonfirmasi", style = "color: #c28815;margin:0 auto;text-align: center;"),
               h3(formatC(konfirmasi, big.mark = ".", decimal.mark = ","), style = "color: #c28815;margin:0 auto;text-align: center;"),
               h5(if_else(kasusbaru > 0, 
                          paste0("+", formatC(kasusbaru, big.mark = ".", decimal.mark = ","), " kasus"), 
                          paste0(formatC(kasusbaru, big.mark = ".", decimal.mark = ","), " kasus")), 
                  style = "color: #c28815;margin:0 auto;text-align: center;"),
             )
      ),
      div(style="display: inline-block;width:49.5%",
             f7Card(
               h5("Dalam Perawatan", style = "color: #6a6a6a;margin:0 auto;text-align: center;"),
               h3(formatC(perawatan, big.mark = ".", decimal.mark = ","), style = "color: #6a6a6a;margin:0 auto;text-align: center;"),
               h5(paste0(formatC(pctperawatan, big.mark = ".", decimal.mark = ","), "%"), 
                  style = "color: #6a6a6a;margin:0 auto;text-align: center;"),
             )
      ),
    )
  })
  
  kasusharian <- reactive({
    trend %>%
      select(Tanggal, Jumlah_Kasus_Baru_per_Hari, Jumlah_Kasus_Dirawat_per_Hari, Jumlah_Kasus_Sembuh_per_Hari, Jumlah_Kasus_Meninggal_per_Hari) %>% 
      pivot_longer(cols = c(Jumlah_Kasus_Baru_per_Hari, Jumlah_Kasus_Dirawat_per_Hari, Jumlah_Kasus_Sembuh_per_Hari, Jumlah_Kasus_Meninggal_per_Hari), names_to = "Kasus") %>% 
      mutate(Kasus = str_to_title(gsub(" per Hari", "", gsub("Jumlah Kasus ", "", gsub("_", " ", Kasus))), locale = "id"))
  })
  
  output$plotharian <- renderPlot({
    
    kasusharian() %>% 
      ggplot(aes(x = Tanggal, y = value, color = Kasus)) +
      geom_line(alpha = 0.7) + 
      geom_point(alpha = 0.7, size = 2) + 
      scale_color_manual(values = c("red", "grey", "darkred", "lightgreen")) + 
      scale_x_date(date_labels = "%d\n%b\n%y", limits = limtren) + 
      labs(color = "Kasus", y = "Banyakya Kasus") + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text.align = 0)
  })
  
  output$harian <- renderUI({
    f7Card(title = "Jumlah Kasus Harian",
      plotOutput("plotharian")
    )
  })
  
  # Tab Provinsi
  output$provmap <- renderLeaflet({
    leaflet(prov) %>% addTiles() %>%
      addCircles(lng = ~x, lat = ~y, weight = 1,
                 radius = ~Kasus_Posi*50, color = "red",
                 popup = ~paste(Provinsi, formatC(Kasus_Posi, big.mark = ".", decimal.mark = ","), " Kasus")
      )
  })
  
  output$provstats <- DT::renderDataTable({
    prov %>% 
      filter(Provinsi == input$provnm) %>% 
      # select(Kasus_Posi, Kasus_Semb, Kasus_Meni) %>% 
      transmute(Positif = formatC(Kasus_Posi, big.mark = ".", decimal.mark = ","), 
                Sembuh = formatC(Kasus_Semb, big.mark = ".", decimal.mark = ","), 
                Meninggal = formatC(Kasus_Meni, big.mark = ".", decimal.mark = ",")
                ) %>% 
      datatable(options = list(dom = "t", 
                               autoWidth = FALSE,
                               columnDefs = list(list(className = 'dt-center', targets = 0),
                                                 list(className = 'dt-center', targets = 1),
                                                 list(className = 'dt-center', targets = 2)),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}")
                               ), 
                rownames = FALSE
                )
  })
  
  output$provcfr <- DT::renderDataTable({
    prov %>% 
      filter(Provinsi == input$provnm) %>% 
      # select(Kasus_Posi, Kasus_Semb, Kasus_Meni) %>% 
      transmute(`Case Fatality Rate` = paste0(formatC(CFR, big.mark = ".", decimal.mark = ","), "%"),
                `Ratio Sembuh` = paste0(formatC(`Ratio Sembuh`, big.mark = ".", decimal.mark = ","), "%")) %>% 
      datatable(options = list(dom = "t", 
                               columnDefs = list(list(className = 'dt-center', targets = 0:1)),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}")
      ), 
      rownames = FALSE
      )
  })
  
  output$trenprov <- renderPlot({
    tren <- trendprov %>% 
      filter(Provinsi == input$provnm) %>%
      select(Tanggal, Kasus_Terkonfirmasi_Akumulatif, Kasus_Sembuh_Akumulatif, Kasus_Meninggal_Akumulatif) %>% 
      pivot_longer(cols = c(Kasus_Terkonfirmasi_Akumulatif, Kasus_Sembuh_Akumulatif, Kasus_Meninggal_Akumulatif), names_to = "Kasus") %>% 
      mutate(Kasus = str_to_title(gsub(" Akumulatif", "", gsub("Kasus ", "", gsub("_", " ", Kasus))), locale = "id"),
             Kasus = factor(case_when(Kasus == "Terkonfirmasi" ~ "Positif",
                                      TRUE ~ as.character(Kasus)), 
                            levels = c("Positif", "Sembuh", "Meninggal")))
    
    
    tren %>%
      ggplot(aes(x = Tanggal, y = value, color = Kasus)) +
      geom_line(alpha = 0.7) + 
      geom_point(alpha = 0.7, size = 2) + 
      scale_color_manual(values = c("red", "lightgreen", "darkred")) + 
      scale_x_date(date_labels = "%d\n%b\n%y", limits = limtren) + 
      # scale_y_continuous(breaks = seq(0, max(tren$value) + 100, by = 100), limits = c(0, max(tren$value) + 100)) + 
      labs(color = "Kasus", y = "Banyaknya Kasus Kumulatif") + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text.align = 0)
      
  })
  
  output$provbar <- renderPlot({
    bar <- prov %>% 
      arrange(desc(Kasus_Posi)) %>% 
      head(10)
    
    bar %>% 
      ggplot(aes(x = reorder(Provinsi, Kasus_Posi), y = Kasus_Posi, fill = Provinsi)) + 
      geom_bar(stat = "identity", alpha = 0.7) + 
      coord_flip(ylim = c(0, max(bar$Kasus_Posi) + 500 + 10^(as.integer(log(max(bar$Kasus_Posi), 10))))) + 
      geom_text(aes(label = sprintf("%s (%s%%)", formatC(bar$Kasus_Posi, big.mark = ".", decimal.mark = ","), bar$CFR)), hjust = -.1, size = 3) + 
      labs(x = "Provinsi", y = "Jumlah Kasus Positif (CFR %)") + 
      theme_minimal() +
      theme(legend.position = "none")
  })
  
}
shinyApp(ui = ui, server = server)
