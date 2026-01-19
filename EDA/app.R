library(shiny)
options(shiny.maxRequestSize = 1000 * 1024^2) # 1GB
library(bslib)
library(ggplot2)
library(dplyr)
library(visdat)
library(GWalkR)
library(DT)
library(tidyr)
library(DataExplorer)
library(httr)
library(jsonlite)
library(skimr)

# UI
ui <- page_navbar(
  id = "main_tabs",
  title = "CSV Data Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly") |>
    bs_add_rules("
      .main-container { padding: 20px; }
      .card { margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .plot-container { height: 600px; overflow-y: auto; }
      .chat-box { height: 400px; overflow-y: auto; border: 1px solid #dee2e6; padding: 10px; background: #f8f9fa; border-radius: 5px; margin-bottom: 10px; }
      .chat-message { margin-bottom: 10px; padding: 8px; border-radius: 5px; }
      .user-message { background: #e7f3ff; align-self: flex-end; }
      .bot-message { background: #ffffff; border: 1px solid #eee; }
      .skimr-container { overflow-x: auto; max-height: 800px; }
      .skimr-container pre { white-space: pre !important; width: max-content; min-width: 100%; }
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
            layout_sidebar(
              sidebar = sidebar(
                title = "Ollama Chat Assistant",
                div(
                  id = "chat_history",
                  class = "chat-box",
                  uiOutput("chat_ui")
                ),
                textAreaInput("user_msg", "Ask Ollama about the data:", rows = 3, placeholder = "e.g., What are the main trends in this data?"),
                actionButton("send_msg", "Send", class = "btn-primary w-100 mb-2"),
                actionButton("analyze_btn", "Analyze Trends", class = "btn-info w-100"),
                hr(),
                helpText("Ensure Ollama is running locally (http://127.0.0.1:11434)")
              ),
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
                      div(class = "skimr-container", verbatimTextOutput("summary"))
                    )
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
    skim(df())
  })
  
  # Ollama Chat Logic
  chat_data <- reactiveVal(list(
    list(role = "bot", content = "Hello! I can help you analyze your data. Upload a file and click 'Analyze Trends' or ask me a question.")
  ))
  
  output$chat_ui <- renderUI({
    msgs <- chat_data()
    tagList(
      lapply(msgs, function(m) {
        div(
          class = paste("chat-message", ifelse(m$role == "user", "user-message", "bot-message")),
          tags$strong(ifelse(m$role == "user", "You: ", "Ollama: ")),
          m$content
        )
      })
    )
  })
  
  # Scroll chat to bottom
  observeEvent(chat_data(), {
    insertUI(
      selector = "#chat_history",
      where = "beforeEnd",
      ui = tags$script("var objDiv = document.getElementById('chat_history'); objDiv.scrollTop = objDiv.scrollHeight;"),
      immediate = TRUE
    )
  })
  
  call_ollama <- function(prompt, history) {
    url <- "http://127.0.0.1:11434/api/generate"
    body <- list(
      #model = "llama3.2",
      model = "gemma3:1b",
      prompt = prompt,
      stream = FALSE
    )
    tryCatch(
      {
        res <- POST(url, body = body, encode = "json", timeout(300))
        if (status_code(res) == 200) {
          content(res)$response
        } else {
          paste("Error: Ollama returned status", status_code(res))
        }
      },
      error = function(e) {
        paste("Error: Could not connect to Ollama. Is it running on http://127.0.0.1:11434?", e$message)
      }
    )
  }
  
  observeEvent(input$send_msg, {
    req(input$user_msg)
    user_text <- input$user_msg
    updateTextAreaInput(session, "user_msg", value = "")
    
    new_history <- chat_data()
    new_history[[length(new_history) + 1]] <- list(role = "user", content = user_text)
    chat_data(new_history)
    
    withProgress(message = "Ollama is thinking...", value = 0, {
      response <- call_ollama(user_text, new_history)
      new_history[[length(new_history) + 1]] <- list(role = "bot", content = response)
      chat_data(new_history)
    })
  })
  
  observeEvent(input$analyze_btn, {
    req(df())
    d <- df()
    intro <- capture.output(print(introduce(d)))
    summary_text <- capture.output(summary(d))
    
    prompt <- paste(
      "Analyze the following data summary and tell me the key trends, potential issues (like missing values), and interesting patterns you see.",
      "\n\nData Introduction:\n", paste(intro, collapse = "\n"),
      "\n\nData Summary:\n", paste(summary_text, collapse = "\n"),
      "\n\nPlease provide a concise explanation in Japanese if possible, otherwise English is fine."
    )
    
    new_history <- chat_data()
    new_history[[length(new_history) + 1]] <- list(role = "user", content = "Can you analyze the trends in this data?")
    chat_data(new_history)
    
    withProgress(message = "Ollama is analyzing...", value = 0, {
      response <- call_ollama(prompt, new_history)
      new_history[[length(new_history) + 1]] <- list(role = "bot", content = response)
      chat_data(new_history)
    })
  })
  
  # GWalkR Interactive Plot
  output$gwalkr <- renderGwalkr({
    req(df())
    gwalkr(df())
  })
}

# Run App
shinyApp(ui, server)
