library(shiny)
library(shinymanager)
library(shinyMobile)
library(shinyjs)
library(shinyalert)

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

ui <- f7Page(
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  tags$style('body{background-color:#ffffff;}'),
  title = "Nama Lembaga Pendidikan",
  f7SingleLayout(
    # navbar is mandatory
    navbar = f7Navbar(
      title = "Ujian Tengah Semester I 2019/2020", 
      hairline = FALSE,
      shadow = TRUE
    ),
    uiOutput("user"),
    # p("Text bla bla bla"),
    f7Button("start", "Mulai"),
    hidden(f7Button("submit", "Submit")),
    uiOutput("soals"),
    hidden(
      div(id="botbtn",
          f7Segment(
            f7Button("prevs", "<< Sebelum"),
            f7Button("nexts", "Berikut >>")
          )
      )
    )
  )
)
set_labels(language = "en", "Please authenticate" = "Login", "Username:" = "NIS", "Password:" = "Password")

ui <- secure_app(ui, enable_admin = TRUE, theme = "paper", status = "primary", 
                 tags_top = tags$div(style="text-align:center;",
                   h3(HTML("<strong>Sistem Manajemen Administrasi<br/>SMK Kesehatan Dewantara</strong>"))
                 ))

server <- function(input, output, session){
  
  source("global.R")
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(db = "data/database.sqlite")
  )
  
  observe({
    res_auth
    shinyalert(text = sprintf("<div>Ujian Tengah Semester I<br/>
                     SMK Dewantara</div>
                     <br/>
             
                     Nama:<br/>%s<br/>
                     NIS:<br/>%s<br/>", reactiveValuesToList(res_auth)$nama, reactiveValuesToList(res_auth)$user), html = TRUE)
  })
  
  output$user <- renderUI({
    tagList(
      fluidRow(
        h5(sprintf("NIS: %s", reactiveValuesToList(res_auth)$user)),
        h5(sprintf("Nama: %s", reactiveValuesToList(res_auth)$nama))
      )
    )
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
  jawaban <- eventReactive(input$save, {
    jwb <- substr(input[[paste0('pg',as.numeric(nosoal()))]], start = 1, 1)
    jawaban()[as.numeric(nosoal()),2] <- jwb
    print(jwb)
    jawaban()
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
                                                         </div>", reactiveValuesToList(res_auth)$nama, 
                                                         reactiveValuesToList(res_auth)$user, benar, salah, kosong, nilai), html = TRUE, closeOnEsc = FALSE, 
                                          closeOnClickOutside = FALSE, showConfirmButton = TRUE, showCancelButton = FALSE, 
                                          confirmButtonText = "Selesai", cancelButtonText = "Batal", 
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
