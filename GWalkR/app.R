library(shiny)
library(GWalkR)
library(bslib)

ui <- page_sidebar( #sidebarのフォーマットを利用
  title = "GWalkR",
    sidebar = sidebar( #サイドバー内のオブジェクト
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
    ),
    fileInput(
        "config",
        "configファイルを選択",
        accept = ".json"
    )
  ),
  gwalkrOutput("gwalkr_ui")
)

server <- function(input, output, session) {
  
  # データ読み込み
  data_reactive <- reactive({
    req(input$file)
    
    readr::read_delim(
      input$file$datapath,
      delim = input$sep,
      col_names = input$header,
      show_col_types = FALSE
    )
  })
  
  # GWalkR 描画
  output$gwalkr_ui <- renderGwalkr(
    gwalkr(
      data = data_reactive(),
      visConfigFile = input$config
    )
  )
}

shinyApp(ui, server)