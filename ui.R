library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyBS)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green", 
                    footer = dashboardFooter(left = span("Saumlaki, April 2023 | Didesain oleh Marwan dengan", icon("mug-hot"))), 
                    scrollToTop = TRUE,
    dashboardHeader(title = "KKT-ST2023", 
                    dropdownMenuOutput("msg")
                    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home", lib = "glyphicon"), badgeLabel = "glimpse", badgeColor = "orange"),
            menuItem("Alokasi Petugas", tabName = "petugas",  icon = icon("user", lib = "glyphicon")),
            # menuItem("Entry Progress", tabName = "progress", icon = icon("edit", lib = "glyphicon")),
            menuItem("Progress Pencacahan", icon = icon("th", lib = "glyphicon"),#badgeLabel = "Penting", badgeColor = "olive",
                menuSubItem("Grafik", tabName = "grafik", icon = icon("stats", lib = "glyphicon")),
                menuSubItem("Tabel", tabName = "tabel", icon = icon("th-list", lib = "glyphicon"))
            )#,
            # menuItem("Tentang", tabName = "tentang", icon = icon("info-sign", lib = "glyphicon"))
            )
        ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # tags$link(rel="stylesheet", href="styles.css", type="text/css")
        ),
        tabItems(tabItem(tabName = "home", #h3("Selamat Datang"),
                         #tags$hr(),
                         box(width = 12, title = "Highlight",collapsible = TRUE,
                             # fluidRow(h5("Progress Pencacahan Menurut Kecamatan")),
                             fluidRow(
                                 valueBoxOutput("prog_kkt", width = 4),
                                 valueBoxOutput("prog_tinggi", width = 4),
                                 valueBoxOutput("prog_rendah", width = 4)
                             )
                         ),
                         box(width = 12, title = "Peta Wilayah",collapsible = TRUE,
                             leafletOutput("progress_map", height = 650)
                         ),
                         box(width = 12, title = "Timeline Kegiatan", collapsible = TRUE,
                             plotlyOutput("plot_timeline")
                             )
                         
                         ),
                 tabItem(tabName = "grafik",
                         box(width = 12, collapsible = TRUE,
                             title = "Grafik Progres Pencacahan KK Tani",
                             selectInput("pilih_bulan", label = "Pilih Bulan Kegiatan", 
                                         choices = c("Semua", "Juni", "Juli")),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Kecamatan",
                                                  fluidRow(),
                                                  fluidRow(column(width = 4,
                                                                  selectInput("pilih_grafik_kecamatan", "Pilih Jenis Grafik", 
                                                                              choices = c("Persentase", "Jumlah"), 
                                                                              selected = "Persentase")
                                                                  ) 
                                                           ),
                                                  fluidRow(column(width=12,
                                                                  plotlyOutput("plot_kecamatan")
                                                                  )
                                                           )
                                                  ),
                                         tabPanel("Desa",
                                                  fluidRow(),
                                                  fluidRow(column(width = 4,
                                                                  selectInput("kecamatan_terpilih", "Pilih kecamatan untuk ditampilkan",
                                                                              choices = kecamatan,
                                                                              selected = "[040] Tanimbar Selatan"
                                                                              )
                                                                  )),
                                                  fluidRow(column(width=12,
                                                                  plotlyOutput("plot_desa")
                                                                  ))
                                                  ),
                                         tabPanel("Petugas",
                                                  fluidRow(),
                                                  fluidRow(column(width = 4,
                                                                  selectInput("kecamatan_terpilih2", "Pilih kecamatan untuk ditampilkan",
                                                                              choices = kecamatan,
                                                                              selected = "[040] Tanimbar Selatan"
                                                                  )
                                                  )),
                                                  fluidRow(column(width=12,
                                                                  plotlyOutput("plot_petugas")
                                                  ))
                                                  ),
                                         tabPanel("History",
                                                  fluidRow(),
                                                  fluidRow(column(width = 4,
                                                                  selectInput("kecamatan_terpilih3", "Pilih kecamatan untuk ditampilkan",
                                                                              choices = kecamatan,
                                                                              selected = "[040] Tanimbar Selatan")
                                                                  ),
                                                           column(width = 4, selectInput("pilih_petugas", "Pilih PPL",
                                                                                         choices = c(""), multiple = TRUE)
                                                                  )
                                                           ),
                                                  fluidRow(column(width = 12,
                                                                  plotlyOutput("plot_history")
                                                                  ))
                                                  )
                             )
                             )
                         ),
                 tabItem(tabName = "tabel",
                         box(width = 12, collapsible = TRUE, 
                             title = "Tabel Progres Pencacahan dan Kendala Lapangan",
                             tabsetPanel(type = "tabs",
                                 tabPanel("Progres Pencacahan",
                                          tags$br(),
                                          dataTableOutput("tabel_progres")
                                          ),
                                 tabPanel("Kendala Lapangan",
                                          tags$br(),
                                          dataTableOutput("tabel_kendala")
                                          ),
                                 tabPanel("Target dan Capaian Kecamatan",
                                          tags$br(),
                                          dataTableOutput("target_capaian_kecamatan")
                                          ),
                                 tabPanel("Target Pencacahan",
                                          tags$br(),
                                          dataTableOutput("target_cacah")
                                          )
                             ))
                         ),
                 tabItem(tabName = "petugas", 
                         box(collapsible = TRUE, width = 12, 
                             title = "Alokasi Petugas dan Wilayah Kerja ST2023",
                             tabsetPanel(type = "tabs",
                                         tabPanel("Wilayah Konsentrasi/Non Konsentrasi",
                                                  tags$br(),
                                                  dataTableOutput("wilker")),
                                         tabPanel("Alokasi Petugas",
                                                  tags$br(),
                                                  dataTableOutput("tabel_alokasi")
                                                  ),
                                         tabPanel("Koseka",
                                                  tags$br(),
                                                  dataTableOutput("koseka_st")
                                                  ),
                                         tabPanel("ID Petugas", dataTableOutput("sobat_id"))
                                         )
                             
                             )
                         )#,
                 # tabItem(tabName = "tentang",
                 #         box(width = 12, collapsible = TRUE, title = "Tentang Aplikasi")
                 #         )
                 )
    )
)

