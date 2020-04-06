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
  f7Radio(inputId = paste0("pg",i), label = NULL, 
               choices = list(paste("A.",HTML(data_soal$A[i])), 
                              paste("B.",HTML(data_soal$B[i])), 
                              paste("C.",HTML(data_soal$C[i])), 
                              paste("D.",HTML(data_soal$D[i])),
                              paste("E.",HTML(data_soal$E[i]))), 
               selected = NULL), #data_soal$jawaban_inline[i]),
  # f7Text(inputId = paste0("essay",i), label = NULL, placeholder = "Jawaban..."),
  # p(paste("Jawaban:", jawab(input[[paste0('pg',i)]]))),
  # br()
  f7Button("save", "Simpan"),
  style="background:#fff;"
  )
)
}


