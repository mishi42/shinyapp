library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(visdat)
library(GWalkR)
library(DT)
library(tidyr)
library(DataExplorer)

# UI
ui <- page_navbar(
  id = "main_tabs",
  title = "CSV Data Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly") |>
    bs_add_rules("
      .main-container { padding: 20px; }
      .card { margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .plot-container { height: 600px; overflow-y: auto; }
    "),

  # Keyboard Shortcut Script
  tags$script("
    $(document).on('keydown', function(e) {
      if (['INPUT', 'TEXTAREA', 'SELECT'].includes(e.target.tagName)) return;
      if (e.key === '1') Shiny.setInputValue('goto_tab', 'upload');
      if (e.key === '2') Shiny.setInputValue('goto_tab', 'explore');
      if (e.key === '3') Shiny.setInputValue('goto_tab', 'gwalkr');
    });
  "),
  nav_panel("Data Upload",
    value = "upload",
    layout_sidebar(
      sidebar = sidebar(
        card(
          card_header("Import Settings"),
          fileInput("file1", "Choose CSV File",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          checkboxInput("header", "Header", TRUE),
          radioButtons("sep", "Separator",
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ","
          ),
          radioButtons("quote", "Quote",
            choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
            selected = '"'
          )
        )
      ),
      card(
        card_header("Data Preview"),
        DTOutput("contents")
      )
    )
  ),
  nav_panel("Data Exploration (DataExplorer)",
    value = "explore",
    layout_columns(
      col_widths = c(12),
      navset_card_underline(
        title = "Automated Data Profiling",
        nav_panel(
          "Intro",
          card_body(
            class = "plot-container",
            plotOutput("intro_plot", height = "500px")
          )
        ),
        nav_panel(
          "Numerical (Histograms)",
          card_body(
            class = "plot-container",
            plotOutput("dist_plot", height = "800px")
          )
        ),
        nav_panel(
          "Categorical (Bar Charts)",
          card_body(
            class = "plot-container",
            plotOutput("bar_plot", height = "800px")
          )
        ),
        nav_panel(
          "Missing Data",
          card_body(
            class = "plot-container",
            plotOutput("missing_plot", height = "500px")
          )
        ),
        nav_panel(
          "Summary",
          card_body(
            verbatimTextOutput("summary")
          )
        )
      )
    )
  ),
  nav_panel("Interactive (GWalkR)",
    value = "gwalkr",
    card(
      card_header("GWalkR Visualization"),
      card_body(
        gwalkrOutput("gwalkr", height = "800px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Handle Keyboard Shortcuts
  observeEvent(input$goto_tab, {
    nav_select("main_tabs", input$goto_tab)
  })

  # Reactive dataframe
  df <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
  })

  # Data Table Preview
  output$contents <- renderDT({
    datatable(df(),
      options = list(
        pageLength = 10, scrollX = TRUE,
        dom = "tp", # table and pagination only
        style = "bootstrap5"
      ),
      class = "cell-border stripe"
    )
  })

  # DataExplorer Intro Plot
  output$intro_plot <- renderPlot({
    req(df())
    plot_intro(df())
  })

  # DataExplorer Distribution Plots (Numerical)
  output$dist_plot <- renderPlot({
    req(df())
    # Identify numeric columns
    num_cols <- df() %>%
      select(where(is.numeric)) %>%
      names()
    if (length(num_cols) == 0) {
      return(NULL)
    }
    plot_histogram(df(), ggtheme = theme_minimal(base_size = 14))
  })

  # DataExplorer Bar Plots (Categorical)
  output$bar_plot <- renderPlot({
    req(df())
    # Identify categorical columns
    cat_cols <- df() %>%
      select(where(~ is.character(.) || is.factor(.))) %>%
      names()
    if (length(cat_cols) == 0) {
      return(NULL)
    }
    plot_bar(df(), ggtheme = theme_minimal(base_size = 14))
  })

  # Missing Data Visualization
  output$missing_plot <- renderPlot({
    req(df())
    plot_missing(df(), ggtheme = theme_minimal(base_size = 14))
  })

  # Statistical Summary
  output$summary <- renderPrint({
    req(df())
    summary(df())
  })

  # GWalkR Interactive Plot
  output$gwalkr <- renderGwalkr({
    req(df())
    gwalkr(df())
  })
}

# Run App
shinyApp(ui, server)
