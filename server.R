
server <- function(input, output, session) {
  monev_summary <- reactive(monev_peta)
  progres_kec <- reactive(target_kecamatan)
  wilker <- reactive(wilayah_kerja)
  alokasi_petugas <- reactive(alokasi)
  
# Home --------------------------------------------------------------------

  output$prog_kkt <- renderValueBox({
    persentase_kkt <- monev_summary() |>
      select(progres) |>
      pull() |> sum()
    valueBox(value = paste0(round((persentase_kkt/target_kkt)*100, 2), "%"),
             subtitle = "Kabupaten Kepulauan Tanimbar", icon = icon("line-chart"), color = "orange")
  })
  # output$prog_kkt <- renderGauge({
  #   persentase_kkt <- monev_summary() |> 
  #     select(progres) |> 
  #     pull() |> sum()
  #   gauge(value = round((persentase_kkt/target_kkt)*100, 2), 
  #         min = 0, max = 100, label = "Kabupaten Kepulauan Tanimbar", 
  #         sectors = gaugeSectors(success = c(0.75, 1),warning = c(0.5, 0.75), danger = c(0,0.5)))
  #   # valueBox(value = paste0(round((persentase_kkt/target_kkt)*100, 2), "%"), 
  #   #          subtitle = "Kabupaten Kepulauan Tanimbar", icon = icon("line-chart"), color = "orange")
  # })
  output$prog_tinggi <- renderValueBox({
    progres_max <- progres_kec() |>
      filter(persentase == max(persentase))
    valueBox(value = paste0(round(progres_max$persentase[1], 2), "%"),
             subtitle = paste("Progress Pencacahan Tertinggi:", progres_max$nmkec[1]),
             icon = icon("line-chart"), color = "olive")
  })
  output$prog_rendah <- renderValueBox({
    progres_min <- progres_kec() |>
      filter(persentase == min(persentase))
    valueBox(value = paste0(round(progres_min$persentase[1], 2), "%"),
             subtitle = paste("Progress Pencacahan Terendah:", progres_min$nmkec[1]),
             icon = icon("line-chart"), color = "red")
  })

  output$progress_map <- renderLeaflet({
    koordinat <- reactive(koordinat_kecamatan)
    koordinat() |> 
      leaflet() |> 
      setView(zoom = 9, lng = 131.3527, lat = -7.499379) |>
      # setView(zoom = 9, lng = 131.305791, lat = -7.982650) |> 
      # addMarkers(lng = 131.305791, lat = -7.98265, popup = "BPS Kabupaten Kepulauan Tanimbar") |> 
      addTiles() |> 
      addCircleMarkers(lng = ~lng, lat = ~lat, label = ~as.character(progres_pencacahan),
                       radius = 8,
                       stroke = FALSE,
                       fillOpacity = 0.6,
                       clusterOptions = markerClusterOptions(),
                       popup = ~progres_pencacahan,
      )
  })
  
  output$plot_timeline <- renderPlotly({
    to_do_list_plot
  })
  
  # Alokasi Petugas ---------------------------------------------------------
  
  # wilker2 <- reactive(wilayah_kerja)
  output$wilker <- renderDataTable({
    
    wilker() |> select(idkec, iddesa, idsls, everything())
  }, width = "95%", filter = "top", rownames = FALSE, options = list(pageLength=10),
  colnames = c("Kode Kecamatan", "Kode Desa", "Kode SLS", "Kecamatan", "Desa", "Nama SLS", 
               "SLS/Non-SLS", "JKK", "JKK Tani", "Konsentrasi", "Target Pencacahan")
  )
  
  # alokasi petugas
  output$tabel_alokasi <- renderDataTable({
    alokasi_petugas()
  }, width = "95%", filter = "top", rownames = FALSE)
  
  
  output$koseka_st <- renderDataTable({
    ksk <- reactive(koseka)
    ksk()
  }, width = "95%")
  
  sobat <- reactive(idsobat)
  
  output$sobat_id <- renderDataTable({
    sobat()},  width = "95%", filter = "top", rownames = FALSE)
  
  messages <- reactive(capaian_kecamatan_kemarin2)
  # Tambahkan warning di sini:
  output$msg <- renderMenu({
    # messages <- data.frame(
    #   from = c("Tansel", "Wertamrian", "Wermaktian", "Selaru",
    #            "Kormomolin", "Tanut", "Fordata", "Wuarlabobar", "Nirunmas", "Molu Maru"),
    #   message = rep("hello", 10),
    #   time = rep(Sys.time(), 10)
    # ) # gantikan dengan tabel hasil input
    message <- messages() |> mutate(time = rep(format(with_tz(Sys.Date(), tzone = "Asia/Jayapura"), "%d-%m-%Y"), 10)) |> 
      select(nmkec, pesan, time)
    msgs <- apply(message, 1, function(row) {
      messageItem(from = row[["nmkec"]], message = row[["pesan"]], time = row[["time"]])
    })
    dropdownMenu(type = "messages", .list = msgs, badgeStatus = "warning", headerText = "Pesan Baru Hari Ini")
  })
  

# progres lapangan --------------------------------------------------------

monitoring_progres <- reactive(monev)

output$tabel_progres <- renderDataTable({
  monitoring_progres() |> 
    select(-kendala.lapangan)
}, width = "95%", filter = "top", rownames = FALSE)

output$tabel_kendala <- renderDataTable({
  monitoring_progres() |> 
    select(-c(PPL, progres))
}, width = "95%", filter = "top", rownames = FALSE)

target_kecamatan3 <- reactive(target_kecamatan2)

output$target_capaian_kecamatan <- renderDataTable({
  target_kecamatan3()
  # names(target_kecamatan2()) <-  c("Kecamatan", "Jumlah Target", "Progres pencacahan", "Persentase")
}, width = "95%", filter = "top", rownames = TRUE)


target_individu <- reactive(alokasi_df)
output$target_cacah <- renderDataTable({target_individu()
  }, width = "95%", filter = "top", rownames = TRUE)

# Grafik ------------------------------------------------------------------
# capaian_per_kecamatan <- reactive(target_kecamatan)

output$plot_kecamatan <- renderPlotly({
  # Blok tambahan untuk filter bulan Juni/Juli
  # if (input$pilih_bulan == "Semua") {
  #   df <- capaian_per_kecamatan()
  # } else if (input$pilih_bulan == "Juni") {
  #   df <- monitoring_progres() |> 
  #     filter(str_starts(Tanggal.pencacahan, "6"))
  # } else {
  #   df <- monitoring_progres() |> 
  #     filter(str_starts(Tanggal.pencacahan, "7"))
  # }
  
  if (input$pilih_grafik_kecamatan == "Persentase") {
    persentase_kec_plotly
  } else {
    capaian_kec_plotly
  }
})

output$plot_desa <- renderPlotly({
  # Blok tambahan untuk filter bulan Juni/Juli
  if (input$pilih_bulan == "Semua") {
    df <- monitoring_progres()
  } else if (input$pilih_bulan == "Juni") {
    df <- monitoring_progres() |> 
      filter(str_starts(Tanggal.pencacahan, "6"))
  } else {
    df <- monitoring_progres() |> 
      filter(str_starts(Tanggal.pencacahan, "7"))
  }
    
    # di sini awalnya
  df |> 
    filter(Kecamatan == input$kecamatan_terpilih) |> 
    group_by(Desa) |> 
    summarise(progres = sum(progres), .groups = "drop") |> 
    mutate(Desa = reorder(Desa, desc(progres))) |> 
    plot_ly(x = ~Desa, y = ~progres, hoverinfo = "text", alpha = 0.7) |> 
    add_bars(hovertext = ~paste("Desa :", str_trim(str_replace(Desa, "\\[\\d+\\]", "")), "<br>Progres :", progres),
             hoverlabel = list(align = "left", bgcolor = "lightblue", bordercolor = "transparent",
                               font = list(color = "#515151"))
             ) |> 
    layout(
      xaxis = list(title = "Desa", color = "#515151"),
      yaxis = list(title = "Progres Jumlah RTUP", color = "#515151"),
      title = list(text = paste("Jumlah KK Tani Berhasil Dicacah di Kecamatan", 
                                str_extract(input$kecamatan_terpilih, patter = "[a-zA-Z\\s]+") |> str_trim()), 
                   color = "#515151")
    )
  
})

output$plot_petugas <- renderPlotly({
  # Blok tambahan untuk filter bulan Juni/Juli
  if (input$pilih_bulan == "Semua") {
    df <- monitoring_progres()
  } else if (input$pilih_bulan == "Juni") {
    df <- monitoring_progres() |> 
      filter(str_starts(Tanggal.pencacahan, "6"))
  } else {
    df <- monitoring_progres() |> 
      filter(str_starts(Tanggal.pencacahan, "7"))
  }
  df |> 
    filter(Kecamatan == input$kecamatan_terpilih2) |> 
    group_by(PPL) |> 
    summarise(progres = sum(progres), .groups = "drop") |> 
    mutate(PPL = reorder(PPL, desc(progres))) |> 
    plot_ly(x = ~PPL, y = ~progres, hoverinfo = "text", alpha = 0.7) |> 
    add_bars(hovertext = ~paste("PPL :", PPL, "<br>Progres :", progres), name = "Capaian",
             hoverlabel = list(align = "left", bgcolor = "lightblue", bordercolor = "transparent",
                               font = list(color = "#515151"))
             ) |> #name = "Capaian", 
    add_lines(y = 120,
              line = list(color = "red"), 
              showlegend = FALSE, name = "Target",
              hovertext = "Target : 120") |>
    # add_text(y = 121, x = monitoring_progres()$PPL[1], text = "Target Juni", showlegend = FALSE) |> 
    layout(
      xaxis = list(title = "Petugas Pencacah Lapangan (PPL)", zeroline = TRUE, color = "#515151"),
      yaxis = list(title = "Progres Jumlah RTUP", zeroline = TRUE, visible = TRUE, range = list(0, 125), 
                   color = "#515151"),
      title = list(text = paste("Jumlah KK Tani Berhasil Dicacah di Kecamatan", 
                                str_extract(input$kecamatan_terpilih2, 
                                            patter = "[a-zA-Z\\s]+") |> str_trim()), 
                   color = "#515151")
      
    )
})
observeEvent(input$kecamatan_terpilih3, 
             updateSelectInput(inputId = "pilih_petugas",
                               choices = monitoring_progres() |> 
                                 filter(Kecamatan == input$kecamatan_terpilih3) |> 
                                 select(PPL) |> 
                                 distinct() |> 
                                 pull()
                               )
             )
output$plot_history <- renderPlotly({
  # Blok tambahan untuk filter bulan Juni/Juli
  if (input$pilih_bulan == "Semua") {
    df <- monitoring_progres()
  } else if (input$pilih_bulan == "Juni") {
    df <- monitoring_progres() |> 
      filter(str_starts(Tanggal.pencacahan, "6"))
  } else {
    df <- monitoring_progres() |> 
      filter(str_starts(Tanggal.pencacahan, "7"))
  }
  req(input$pilih_petugas)
  df |> 
    filter(Kecamatan == input$kecamatan_terpilih3) |> 
    filter(PPL %in% input$pilih_petugas) |> 
    select(Tanggal.pencacahan, PPL, progres) |> 
    # group_by(Tanggal.pencacahan, PPL) |> 
    # summarise(progres = sum(progres)) |> 
    mutate(Tanggal.pencacahan = mdy(Tanggal.pencacahan)) |> 
    plot_ly(x = ~Tanggal.pencacahan, y = ~progres, color = ~PPL) |> 
    add_lines() |> 
    layout(
      xaxis = list(title = "Tanggal Pencacahan", color = "#515151"),
      yaxis = list(title = "Progres Pencacahan", color = "#515151"),
      title = list(text = paste("Progres Pencacahan KK Tani oleh PPL di Kecamatan", 
                                str_extract(input$kecamatan_terpilih3, patter = "[a-zA-Z\\s]+") |> str_trim() 
                                ), color = "#515151")
    )
    
})

}





