library(RSQLite)
# library(xlsx)
# library(dplyr)
# library(DT)

# app/shiny/
dataloc = "data/soal.sqlite"

getTable = function(tblnm, file.loc = "data/soal.sqlite")
{
  dbcon <- dbConnect(RSQLite::SQLite(), file.loc)
  tbl_dt <- dbReadTable(dbcon, tblnm)
  dbDisconnect(dbcon)
  return(tbl_dt)
}

data_soal <- getTable("bank_soal_mtk", dataloc)
data_soal <- data_soal %>% subset(gunakan == "T")
kunci_jawaban <- data_soal$kunci_jawaban[1:10]

jawab = function(input)
{
  input = ifelse(is.null(input), "", substr(x = input, start = 1, stop = 1))
  return(input)
}

soal = function(i, input)
{
list(
  wellPanel(
    h5(paste("Soal no.", i), style = "font-weight:bold;font-size:130%;"),
    p(withMathJax(HTML(data_soal$soal[i]))), 
  # hidden(f7Text("gunakan_gambar", label = NULL, value = data_soal$gunakan_gambar[i])),
  # hidden(f7Text("type_soal", label = NULL, value = data_soal$type_soal[i])),
  # plotOutput("pgimg"),
  # radioButtons(inputId = paste0("pg",i), label = NULL, 
  #              choiceNames = list(paste(HTML(data_soal$A[i])), 
  #                             paste(HTML(data_soal$B[i])), 
  #                             paste(HTML(data_soal$C[i])), 
  #                             paste(HTML(data_soal$D[i])),
  #                             paste(HTML(data_soal$E[i]))), 
  #              choiceValues = LETTERS[1:5],
  #              selected = NULL), #data_soal$jawaban_inline[i]),
  HTML(sprintf('
              <div id="%s" class="form-group shiny-input-radiogroup shiny-input-container">
              <label class="control-label shiny-label-null" for="%s"></label>
              <div class="shiny-options-group">
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="A"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="B"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="C"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="D"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="E"/>
                    <span>%s</span>
                  </label>
                </div>
              </div>
            </div>
              ', 
              paste0("pg",i), 
              paste0("pg",i),
              paste0("pg",i),
              paste(HTML(data_soal$A[i])), 
              paste0("pg",i),
              paste(HTML(data_soal$B[i])),
              paste0("pg",i),
              paste(HTML(data_soal$C[i])),
              paste0("pg",i),
              paste(HTML(data_soal$D[i])),
              paste0("pg",i),
              paste(HTML(data_soal$E[i]))
              )),
  # fab_button(inputId = "fab", status = "primary", icon = icon("medrt"),
  #            actionButton(inputId = "start", label = NULL, tooltip = "Mulai", icon = icon("envelope-open-text"))#,
  #            # actionButton(inputId = "submit", label = NULL, tooltip = "Submit", icon = icon("send"))
  # ),
  style="background:#fff;"
  )
)
}


