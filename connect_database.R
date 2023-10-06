# library(DBI)
# library(bigrquery)
# library(dplyr)
# 
# # con <- dbConnect(
# #   bigrquery::bigquery(),
# #   project = "st-2023-kkt",
# #   dataset = "progres",
# #   billing = "st-2023-kkt"
# # )
# # 
# # dbListTables(con)
# # 
# # dbGetQuery(con, "INSERT INTO progres.kendala VALUES ('Tanimbar Selatan', 'Lermatang', 'RT001 RW 01', '2023-06-01', 'John Doe', '3')")
# # dbGetQuery(con, "INSERT INTO progres.progress VALUES ('Tanimbar Selatan', 'Lermatang', 'RT001 RW 01', '2023-06-01', 'John Doe', 5)")
# # kendala <- dbReadTable(con, "kendala")
# # progress <- dbReadTable(con, "progress")
# 
# insert_progres <- function(kecamatan, desa, sls, tanggal, ppl, progres) {
#   con <- dbConnect(
#     bigrquery::bigquery(),
#     project = "st-2023-kkt",
#     dataset = "progres",
#     billing = "st-2023-kkt"
#   )
#   dbGetQuery(con, "INSERT INTO progres.progress VALUES (kecamatan, desa, sls, tanggal, ppl, progres)")
#   dbDisconnect(con)
# }
# 
# insert_kendala <- function(kecamatan, desa, sls, tanggal, ppl, kendala) {
#   con <- dbConnect(
#     bigrquery::bigquery(),
#     project = "st-2023-kkt",
#     dataset = "progres",
#     billing = "st-2023-kkt"
#   )
#   dbGetQuery(con, "INSERT INTO progres.progress VALUES (kecamatan, desa, sls, tanggal, ppl, kendala)")
#   dbDisconnect(con)
# }
# 
# view_progres <- function() {
#   con <- dbConnect(
#     bigrquery::bigquery(),
#     project = "st-2023-kkt",
#     dataset = "progres",
#     billing = "st-2023-kkt"
#   )
#   df <- dbGetQuery(con, "SELECT * FROM progres.progress")
#   dbDisconnect(con)
#   return(df)
# }
# 
# view_kendala <- function() {
#   con <- dbConnect(
#     bigrquery::bigquery(),
#     project = "st-2023-kkt",
#     dataset = "progres",
#     billing = "st-2023-kkt"
#   )
#   df <- dbGetQuery(con, "SELECT * FROM progres.kendala")
#   dbDisconnect(con)
#   return(df)
# }
# 
# progres <- view_progres()
# kendala <- view_kendala()
