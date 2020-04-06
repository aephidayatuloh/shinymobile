library(RSQLite)
# library(xlsx)
# library(dplyr)
# library(DT)

# app/shiny/
dataloc = "data/soal.sqlite"

getTable = function(tblnm, file.loc = "data/soal.sqlite")
{
  dbcon = dbConnect(RSQLite::SQLite(), file.loc)
  tbl_dt = dbReadTable(dbcon, tblnm)
  dbDisconnect(dbcon)
  return(tbl_dt)
}

data_soal = getTable("bank_soal_mtk", dataloc)
data_soal = data_soal %>% subset(gunakan == "T")
kunci_jawaban = data_soal$kunci_jawaban
jawaban <- data.frame(no = 1:length(kunci_jawaban), jawaban = rep(NA, length(kunci_jawaban)))

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
  # img(src = paste("images", data_soal$nama_gambar[i], sep = "/")),
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
  # f7Text(inputId = paste0("essay",i), label = NULL, placeholder = "Jawaban..."),
  # p(paste("Jawaban:", jawab(input[[paste0('pg',i)]]))),
  # br()
  # verbatimTextOutput("radio"),
  actionBttn(inputId = "save", label = "Simpan", icon = icon("save"), style = "bordered", size = "xs", color = "primary", block = TRUE),
  style="background:#fff;"
  )
)
}


