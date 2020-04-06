library(shiny)
library(shinymanager)
library(shinyMobile)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)

# credentials <- data.frame(
#   user = c("161710001", "shiny", "shinymanager"), # mandatory
#   password = c("1234", "werty", "12345"), # mandatory
#   nama = c("Aep Hidayatuloh", "nis01", "nis21"),
#   start = c("2019-04-15"), # optinal (all others)
#   expire = c(NA, NA, "2020-12-31"),
#   admin = c(FALSE, FALSE, TRUE),
#   comment = "Simple and secure authentification mechanism
#   for single ‘Shiny’ applications.",
#   stringsAsFactors = FALSE
# )
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "data/database.sqlite"
# )
set_labels(language = "en", "Please authenticate" = "", "Username:" = "NIS", "Password:" = "Password")

ui <- fluidPage(
  auth_ui(
    id = "auth",
    # add image on top ?
    tags_top = 
      tags$div(
        tags$img(
          src = "logo.png", width = 120
        ),
        tags$h5("SMK Kesehatan Dewantara", style = "align:center"),
        tags$h6("Sistem Manajemen Administrasi", style = "align:center")
      ),
    # add information on bottom ?
    # tags_bottom = tags$div(
    #   tags$p(
    #     "For any question, please  contact ",
    #     tags$a(
    #       href = "mailto:someone@example.com?Subject=Shiny%20aManager",
    #       target="_top", "administrator"
    #     )
    #   )
    # ),
    # change auth ui background ?
    # https://developer.mozilla.org/fr/docs/Web/CSS/background
    background  = "linear-gradient(rgba(0, 0, 255, 0.5),
                       rgba(255, 255, 0, 0.5)),
                       url('logo.png');repeat:false;", 
    choose_language = FALSE
  ),
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  theme = shinytheme("paper"),
  # tags$style('body{background-color:#ffffff;}'),
  title = "Nama Lembaga Pendidikan",
  # f7Page(
  fluidRow(
      # navbar is mandatory
      # navbar = f7Navbar(
      #   title = "Ujian Tengah Semester I 2019/2020",
      #   hairline = FALSE,
      #   shadow = TRUE
      # ),
    h4("Ujian Tengah Semester I 2019/2020", style = "text-alin:center;"),
    uiOutput("user"),
      # p("Text bla bla bla"),
      # f7Button("start", "Mulai"),
      # hidden(f7Button("submit", "Submit")),
    uiOutput("soals"),
    hidden(
      div(id="botbtn",
          div(style="display:inline-block;width:49%",
              actionButton(inputId = "prevs", label = "<< Sebelum", width = "100%")
              ),
          div(style="display:inline-block;width:49%",
              actionButton(inputId = "nexts", label = "Berikut >>", width = "100%")
            )
        )
      ),
      uiOutput("fabbtn")
    )
  # )
)
set_labels(language = "en", "Please authenticate" = "Login", "Username:" = "NIS", "Password:" = "Password")

# ui <- secure_app(ui, enable_admin = TRUE, theme = "paper", status = "primary")

server <- function(input, output, session){
  
  source("global2.R")
  
  # call the server part
  # check_credentials returns a function to authenticate users
  # auth <- secure_server(
  #   check_credentials = check_credentials(db = "data/database.sqlite")
  # )
  
  # authentication module
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(db = "data/database.sqlite")
  )
  
  observe({
    req(auth$user)
    output$fabbtn <- renderUI({
      fab_button(inputId = "fab", status = "primary", icon = icon("medrt"),
                 actionButton(inputId = "start", label = NULL, tooltip = "Mulai", icon = icon("envelope-open-text"))#,
                 # actionButton(inputId = "submit", label = NULL, tooltip = "Submit", icon = icon("send"))
                 )
    })
  })
  observeEvent(input$start, {
    hideElement("start")
    output$fabbtn <- renderUI({
      fab_button(inputId = "fab", status = "primary", icon = icon("medrt"),
                 # actionButton(inputId = "start", label = "Mulai", tooltip = "Mulai", icon = icon("envelope-open-text"))#,
                 actionButton(inputId = "submit", label = NULL, tooltip = "Submit", icon = icon("send"))
      )
    })
  })
  
  observe({
    req(auth$user)
    shinyalert(text = sprintf("<div>Ujian Tengah Semester I<br/>
                     SMK Dewantara</div>
                     <br/>
             
                     Nama:<br/>%s<br/>
                     NIS:<br/>%s<br/>", auth$user_info$nama, auth$user_info$user), html = TRUE)
  })
  
  output$user <- renderUI({
    tagList(
      h5(sprintf("NIS: %s", auth$user_info$user)),
      h5(sprintf("Nama: %s", auth$user_info$nama))
    )
  })
  
  output$user_info <- renderPrint({
    req(auth$user)
    auth$user_info
  })
  
  
  lapply(1:31, function(i) {
    # output[[paste0("no", i)]] <- renderUI({
    #   h5(paste("Soal no.", i))
    # })
    # jawaban
    output[[paste0("soal",i)]] <- renderUI({
      # h5(paste("Soal no.", i))
      soal(i, input)
      # paste("Jawaban:", jawab(input[[paste0('pg',i)]]))
    })
    
    # output[[paste0("result",i)]] <- renderText({
    #   paste("Jawaban:", jawab(input$pg1))
    # })
  })
  nosoal <- reactiveVal(0)
  observeEvent(input$start, {
    hideElement("start")
    showElement("submit")
    showElement("botbtn")
    nosoal(1)
    output$soals <- renderUI({
      uiOutput(paste0("soal",as.numeric(nosoal())))
    })
  })
  observeEvent(input$prevs, {
    nosoal(nosoal()-1)
    output$soals <- renderUI({
      uiOutput(paste0("soal",as.numeric(nosoal())))
    })
  })
  output$radio <- renderPrint({
    input[[paste0('pg',as.numeric(nosoal()))]]
  })
  observeEvent(input$save, {
    jwb <- input[[paste0('pg',as.numeric(nosoal()))]]
    if(!is.null(jwb)){
      # jawaban()[as.numeric(nosoal()),2] <- jwb
      shinyalert(title = "Disimpan", text = sprintf("Jawaban: %s", jwb))
    } else {
      shinyalert(text = "Silahkan pilih salah satu jawaban", type = "error")
    }
  })
  observeEvent(input$nexts, {
    nosoal(nosoal()+1)
    output$soals <- renderUI({
      uiOutput(paste0("soal",as.numeric(nosoal())))
    })
  })
  observeEvent(input$submit, {
    
    dijawab <- 29
    kosong <- 2
    shinyalert::shinyalert(title = "Anda yakin akan submit jawaban?", 
                           text = sprintf("Dijawab: %s | Kosong: %s<br/><br/><strong>PERHATIAN!</strong><br/>Jawaban Anda akan disimpan dan dinilai, kemudian ujian ini akan ditutup dan selesai. Tidak dapat diulang atau diperbaiki.", dijawab, kosong), 
                           html = TRUE,
                           closeOnClickOutside = FALSE, showCancelButton = TRUE, confirmButtonText = "Ya", cancelButtonText = "Batal", 
                           callbackR = function(x){
                             if(x == TRUE){
                               benar <- 27
                               salah <- 2
                               nilai <- 80
                               shinyalert(title = NULL,
                                          text = sprintf("<div style='text-align:center;'>Ujian Tengah Semester I<br/>
                                                         SMK Kesehatan Dewantara<br/>
                                                         Tahun Ajaran 2019/2020<br/>
                                                         <br/>
                                                         Nama:<br/>%s<br/>
                                                         NIS:<br/>%s<br/>
                                                         <br/>
                                                         <div style='font-weight:bold;'>Jawaban:<br/>
                                                         <span style='color:#5ad668'>Benar: %s</span> | 
                                                         <span style='color:#db6153'>Salah: %s</span> | 
                                                         <span style='color:#f2df63'>Kosong: %s</span><br/>
                                                         <div style='font-weight:bold;'>Nilai: %s</div>
                                                         </div>
                                                         </div>", auth$user_info$nama, 
                                                         auth$user_info$user, benar, salah, kosong, nilai), html = TRUE, closeOnEsc = FALSE, 
                                          closeOnClickOutside = FALSE, showConfirmButton = TRUE, showCancelButton = FALSE, 
                                          confirmButtonText = "Selesai & Keluar", cancelButtonText = "Batal", 
                                          callbackR = function(x){
                                            if(x == TRUE){
                                              # dbcon <- dbConnect(RSQLite::SQLite(), dataloc)
                                              # rc <- dbSendStatement(conn = dbcon, statement = sprintf("UPDATE TABLE credentials SET "))
                                              # dbDisconnect(dbcon)
                                              # print(jawaban())
                                              session$reload()
                                            }
                                          })
                             }
                           })
  })
  
}
shinyApp(ui = ui, server = server)
