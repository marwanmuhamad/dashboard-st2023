library(plotly)
library(vistime)
# library(DT)
library(leaflet)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(openxlsx)
library(flexdashboard)
# library(readxl)
# Load datasets -----------------------------------------------------------
link1 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR-aLXMwN5WMAHjZwuqrwSJ0tG5ybL1e_p7B5KZER1Dj3Hjp-Y-OhrZ3yKkqf0LsDUr_jH92cNNt3XK/pub?output=csv"
link2 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSU5hOV8fg2u0_NJ3x-DTde6Bhw19MJZXKcspDAZH5gCM4o28AKoKZ9VdJvoAA30BSAY9K6bqy17BZ7/pub?gid=251123680&single=true&output=csv"
link3 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTZ_tORrfUf5Wwh13ZwbnWT7Qjz8ED1D2UAzz6LZcUj54RbpDzOzEEZpa5tBvWQKw/pub?output=csv"
wilayah_kerja <- read_csv(link1)
wilayah_kerja$KONSENTRASI <- ifelse(wilayah_kerja$KONSENTRASI == 1, "Konsentrasi", "Non-Konsentrasi")
wilayah_kerja$sumber <- NULL
wilayah_kerja$`Kondisi Master SLS/SubSLS/NonSLS` <- NULL
wilayah_kerja$idsubsls <- NULL

wilayah_kerja <- wilayah_kerja |> 
  mutate(target = ifelse(KONSENTRASI=="Konsentrasi", JKK, JKKTANI))

koseka <- read_csv(link2)
alokasi <- read_csv(link3)
alokasi <- alokasi |> 
  select(-c(idkec, iddesa, idsls, idsubsls))

kecamatan <- alokasi |> 
  select(nmkec) |> 
  distinct() |> 
  pull()

link_monev <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRHuaK3eTJF7dn-LB7-eic5O3-QnznvpGy0LxplvXR_a8jgRQg0zhgzHxj9bSoxr0W-Dh9Pbqk8tAzG/pub?output=csv"

# timer <- reactiveTimer(10000)
monev <- read.csv(link_monev)

monev <- monev |> 
  mutate(Desa = paste(Desa.040, Desa.041, Desa.042, Desa.043, Desa.050, Desa.051, Desa.052, Desa.053, Desa.054, Desa.055),
         PPL = paste(PPL.040, PPL.041, PPL.042, PPL.043, PPL.050, PPL.051, PPL.052, PPL.053, PPL.054, PPL.055)) |> 
  mutate(Desa = str_trim(Desa),
         PPL = str_trim(PPL),
         SLS = str_to_upper(SLS)) |> 
  select(Kecamatan, Desa, Tanggal.pencacahan, SLS, PPL, progres, kendala.lapangan)

monev <- monev |> 
  mutate(Desa = str_remove_all(Desa, pattern = "NA"),
         Desa = str_trim(Desa),
         PPL = str_remove_all(PPL, pattern = "NA"),
         PPL = str_trim(PPL))

bbox <- c(xmin = 130.697091, ymin = -8.345412, xmax = 132.008321,ymax = -6.653346)
koordinat_kecamatan <- data.frame(
  Kecamatan = c("Tanimbar Selatan", "Wertamrian", "Wermaktian", "Selaru", "Kormomolin",
                "Nirunmas", "Tanimbar Utara", "Fordata", "Wuarlabobar", "Molu Maru"),
  lng = c(131.301073, 131.353441, 131.032778, 131.115519, 131.579854, 131.652384, 131.715455, 131.945391, 131.440444, 131.576226),
  lat = c(-7.984151, -7.828576, -7.671275, -8.126157, -7.666433, -7.498377, -7.156625, -7.055233, -7.318695,-6.701389)
)

monev_peta <- monev |> 
  group_by(Kecamatan) |> 
  summarise(progres = sum(progres)) |> 
  mutate(Kecamatan = str_extract(Kecamatan, pattern = "[a-zA-Z\\s]+")) |> 
  mutate(Kecamatan = str_trim(Kecamatan))

koordinat_kecamatan <- left_join(koordinat_kecamatan, monev_peta, join_by("Kecamatan")) |> 
  mutate(progres_pencacahan = paste(Kecamatan, ":\n", progres)) |>
  mutate(progres_pencacahan = str_to_upper(progres_pencacahan))

link4 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSJU_Xb6FH4GmjIeT21UlGiTIL9rwIqVrcLvab2FJcZA3CyeEbLAjbsLriShd-Fq0GKmPfxB7UrvCHk/pub?output=csv"

timeline_kegiatan <- read.csv(link4)

data.present <- timeline_kegiatan %>% 
  drop_na(group, start, end, priority) %>% 
  mutate(color = case_when(priority == 1 ~ "lightgreen",
                           priority == 2 ~ "orange",
                           TRUE ~ "lightblue"),
         group = factor(group, levels = c("Rekrutmen", "Pelaksanaan", "Pengolahan")),
         event = factor(event, levels = c("Rekrutmen petugas", "Pelatihan petugas lapangan",
                                          "Pencacahan lengkap", "Monitoring dan evaluasi",
                                          "Rekrutmen petugas pengolahan", "Pelatihan petugas pengolahan",
                                          "Pelaksanaan pengolahan"))) %>% 
  group_by(end) %>% 
  arrange(end)
# str(data.present)

data.present <- data.present |> 
  mutate(start = dmy(start),
         end = dmy(end))
# data.present

p <- vistime(data.present,
             optimize_y = FALSE)

pb <- plotly_build(p)

to_do_list_plot <- add_segments(p, 
                                x = Sys.Date(), 
                                xend = Sys.Date(), 
                                y = pb$x$layout$yaxis$range[1], 
                                yend = pb$x$layout$yaxis$range[2], 
                                color = I("red"), text = "Hari ini") |> 
  layout(showlegend = FALSE)


# -------------------------------------------------------------------------

target_kkt <- sum(wilayah_kerja$JKKTANI)
target_kecamatan <- wilayah_kerja |> 
  group_by(nmkec) |> 
  summarise(jumlah_target = sum(JKKTANI)) |> 
  mutate(nmkec = case_when(
    nmkec == "WER MAKTIAN" ~ "WERMAKTIAN",
    nmkec == "WER TAMRIAN" ~ "WERTAMRIAN",
    nmkec == "WUAR LABOBAR" ~ "WUARLABOBAR",
    TRUE ~ nmkec
  )) |> 
  mutate(nmkec = str_to_title(nmkec))

target_kecamatan <- left_join(target_kecamatan, monev_peta, join_by("nmkec" == "Kecamatan")) |> 
  mutate(progres = ifelse(is.na(progres), 0, progres),
         persentase = round((progres/jumlah_target)*100, 2)
         )

persentase_kec_plotly <- target_kecamatan |> 
  mutate(nmkec = reorder(nmkec, desc(persentase))) |> 
  plot_ly(x = ~nmkec, y = ~persentase, alpha = 0.7) |> 
  add_bars(hoverinfo = "text", 
           hovertext = ~paste("Kecamatan :",nmkec, "<br>Persentase :", persentase, "%"), 
           name = "Capaian",
           hoverlabel = list(align = "left", bgcolor = "lightblue", bordercolor = "transparent",
                             font = list(color = "#515151"))) |> 
  layout(
    xaxis = list(title = "Kecamatan", color = "#515151"),
    yaxis = list(title = "Persentase (%)", color = "#515151"),
    title = list(text = "Persentase KK Tani Berhasil Dicacah", color = "#515151")
  )

capaian_kec_plotly <- target_kecamatan |> 
  mutate(nmkec = reorder(nmkec, desc(progres))) |> 
  plot_ly(x = ~nmkec, y = ~progres, alpha = 0.7) |> 
  add_bars(hoverinfo = "text", hovertext = ~paste("Kecamatan :", nmkec, "<br>Progres :", progres), 
           name = "Capaian",
           hoverlabel = list(align = "left", bgcolor = "lightblue", bordercolor = "transparent",
                             font = list(color = "#515151"))
           ) |> 
  layout(
    xaxis = list(title = "Kecamatan", color = "#515151"),
    yaxis = list(title = "Progres Jumlah RTUP", color = "#515151"),
    title = list(text = "Jumlah KK Tani Berhasil Dicacah", color = "#515151")
  )

# ID SOBAT ----------------------------------------------------------------

link_id <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSyT9S5TSmrg76vSzp9JYeVvyDMx0jfISh3rK8v9nod5lOXiS6YwPMJ_bftBRCDg0-1MTMl4DMyFsxO/pub?output=csv"

idsobat <- read.csv(link_id)
idsobat <- idsobat |> 
  select(NIK, Nama, Email, SOBAT.ID) |> 
  mutate(NIK = as.character(NIK),
         SOBAT.ID = as.character(SOBAT.ID),
         Nama = str_to_title(Nama))

kecamatan <- monev |> 
  select(Kecamatan) |> 
  distinct() |> 
  pull()


target_kecamatan2 <- target_kecamatan
names(target_kecamatan2) <- c("Kecamatan", "Jumlah Target", "Progres Pencacahan", "Persentase (%)")


link_alokasi <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTbb--DkX7wII0J47DEWtzGzpqWJHi9hTpPM8NmTZqJCHY7xpUl6-H4UB6Ds3VdDeLBcSEX9efVcmZL/pub?output=xlsx"
wb <- loadWorkbook(link_alokasi)

alokasi_df <- sheets(wb) |> map_df(~read.xlsx(link_alokasi, .))
alokasi_df <- alokasi_df |> 
  mutate(PPL = str_to_title(PPL)) |> 
  group_by(PPL) |> 
  summarise(JKK_Target = sum(JKK_Target2)) |> 
  filter(!is.na(PPL)) |> 
  mutate(target_harian = ceiling(JKK_Target/54))

target_harian_kecamatan <- target_kecamatan |> 
  mutate(target_harian = ceiling(jumlah_target/50)) |> # sebelumnya dibagi 54
  select(nmkec, target_harian)

today = with_tz(Sys.Date(), tzone = "Asia/Jayapura")
capaian_kecamatan_kemarin <- monev |> 
  mutate(Tanggal.pencacahan = mdy(Tanggal.pencacahan)) |> 
  filter(Tanggal.pencacahan == today - 1) |> 
  group_by(Kecamatan) |> 
  summarise(progres = sum(progres), .groups = "drop") |> 
  mutate(Kecamatan = str_extract(Kecamatan, pattern = "[a-zA-Z\\s]+"),
         Kecamatan = str_trim(Kecamatan))

capaian_kecamatan_kemarin2 <- left_join(target_harian_kecamatan, capaian_kecamatan_kemarin,  join_by("nmkec"=="Kecamatan")) |> 
  mutate(progres = ifelse(is.na(progres), 0, progres)) |> 
  mutate(pesan = ifelse(progres>target_harian, paste("Target", format(with_tz(Sys.Date()-1, tzone = "Asia/Jayapura"), "%d-%m-%y"), "tercapai"), 
                        paste("Target", format(with_tz(Sys.Date()-1, tzone = "Asia/Jayapura"), "%d-%m-%y"), "tidak tercapai")))
