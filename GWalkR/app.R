library(shiny)
library(GWalkR)
library(bslib)

ui <- page_sidebar(
  title = "GWalkR",
    sidebar = sidebar(
      fileInput(
        "file",
        "CSVファイルを選択",
        accept = ".csv"
      ),
    checkboxInput("header", "ヘッダーあり", TRUE),
    selectInput(
      "sep",
      "区切り文字",
      choices = c(
        "カンマ" = ",",
        "タブ" = "\t",
        "セミコロン" = ";"
      ),
      selected = ","
    )
  ),
  gwalkrOutput("gwalkr_ui")
)

server <- function(input, output, session) {
  
  # データ読み込み
  data_reactive <- reactive({
    req(input$file)
    
    read_delim(
      input$file$datapath,
      delim = input$sep,
      col_names = input$header,
      show_col_types = FALSE
    )
  })
  
  # GWalkR 描画
  output$gwalkr_ui <- renderGwalkr(
    gwalkr(
      data = data_reactive()
    )
  )
}

shinyApp(ui, server)