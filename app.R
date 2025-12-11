library(shiny)
library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(DT)
library(readxl)
library(sf)
library(scales)
library(bslib)
library(whomapper)

# Load helper functions when running the app from the source tree.
helper_files <- file.path(
  "R",
  c(
    "read_data.R",
    "read_shape.R",
    "get_risks.R",
    "vis_risk_table.R",
    "vis_scores.R"
  )
)
invisible(lapply(helper_files[file.exists(helper_files)], source))

ui <- page_sidebar(
  title = "WHO Seasonal Risk Assessment Tool",
  theme = bs_theme(bootswatch = "litera"),
  fillable = TRUE,
  sidebar = sidebar(
    width = 360,
    position = "left",
    open = "open",
    collapsible = TRUE,
    tabsetPanel(
      tabPanel(
        title = "Upload/Download",
        fileInput("upload_data", "Upload Risk Scores", accept = ".xlsx"),
        downloadButton("download_data", "Download Scores"),
        downloadButton("download_weightings", "Download Weightings")
      ),
      tabPanel(
        title = "Select Pillar Weights",
        uiOutput("pillar_weights")
      ),
      tabPanel(
        title = "Select Indicator Weights",
        uiOutput("indicator_weights")
      )
    )
  ),

  tags$style(HTML(
    "
  .map-grid {
    display: grid;
    grid-template-columns: 20% 20% 20% 40%; /* 3 x 20% = 60%, last = 40% */
    gap: 20px;
    align-items: start;
  }
  .map-cell {
    padding: 5px;
  }
"
  )),

  fluidRow(
    column(
      width = 12,
      h3("Risk Scores"),

      tabsetPanel(
        id = "score_tabs",

        tabPanel(
          "Overall Risk Score",
          br(),
          dataTableOutput("table_overall")
        ),

        tabPanel(
          "Exposure",
          br(),
          dataTableOutput("table_exposure")
        ),

        tabPanel(
          "Vulnerability",
          br(),
          dataTableOutput("table_vulnerability")
        ),

        tabPanel(
          "LOCC",
          br(),
          dataTableOutput("table_lcc")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      uiOutput("maps")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$upload_data)
    tryCatch(
      read_data(input$upload_data$datapath),
      error = function(e) {
        showNotification(
          paste("Error reading uploaded file:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
    )
  })

  shape <- reactive({
    req(input$upload_data)

    # TOD: make generic
    whomapper::pull_sfs(
      adm_level = 1,
      iso3 = "UKR",
      query_server = TRUE
    ) %>%
      rename(Adm1 = adm1_viz_name)
  })

  values <- reactiveValues(risks = NULL)

  observe({
    # Do nothing until a valid dataset exists
    req(!is.null(data()))

    # Minimal hard stop if required pieces are missing or empty
    if (
      is.null(data()$scores) ||
        ncol(data()$scores) == 0 ||
        is.null(data()$groupings) ||
        length(data()$groupings) == 0
    ) {
      showNotification(
        "Uploaded file doesn’t match the expected template (missing scores/groupings).",
        type = "error",
        duration = 10
      )
      values$risks <- NULL
      return()
    }

    values$groupings <- map(
      data()$groupings,
      ~ imap_dbl(.x, function(x, y) {
        if (!is.null(input[[y]])) input[[y]] else x
      })
    ) %>%
      map(~ .x / sum(.x))
    groupings <- values$groupings

    values$weightings <- map_dbl(
      names(data()$groupings),
      ~ if (!is.null(input[[.x]])) input[[.x]] else 1 / length(data()$groupings)
    ) %>%
      (\(x) x / sum(x))() %>%
      setNames(names(data()$groupings))
    weightings <- values$weightings

    values$risks <- tryCatch(
      get_risks(
        groupings = groupings,
        scores = data()$scores,
        weightings = weightings
      ),
      error = function(e) {
        showNotification(
          paste("Couldn’t compute risks:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      }
    )

    # === PER-TAB TABLE OUTPUTS =========================================

    # Default overall table
    output$table_overall <- DT::renderDT(
      vis_risk_table(values$risks, values$weightings),
      rownames = FALSE,
      options = list(
        order = FALSE,
        pageLength = 15,
        searching = FALSE
      )
    )

    # Exposure indicator table
    output$table_exposure <- DT::renderDT(
      {
        df <- make_indicator_table(
          scores = data()$scores,
          risks = values$risks,
          groupings = values$groupings, # the normalized groupings
          pillar_name = "Exposure"
        )
        vis_risk_table(df, values$groupings[["Exposure"]])
      },
      rownames = FALSE
    )

    # Vulnerability indicator table
    output$table_vulnerability <- DT::renderDT(
      {
        df <- make_indicator_table(
          scores = data()$scores,
          risks = values$risks,
          groupings = values$groupings,
          pillar_name = "Vulnerability"
        )
        vis_risk_table(df, values$groupings[["Vulnerability"]])
      },
      rownames = FALSE
    )

    # LOCC indicator table
    output$table_lcc <- DT::renderDT(
      {
        df <- make_indicator_table(
          scores = data()$scores,
          risks = values$risks,
          groupings = values$groupings,
          pillar_name = "LOCC" # or "Lack of Coping Capacity" depending on your naming
        )
        vis_risk_table(df, values$groupings[["LOCC"]])
      },
      rownames = FALSE
    )

    values$weightings_table <- map_dfr(
      seq_along(weightings),
      \(x) {
        tibble(
          pillar = names(groupings)[x],
          metric = names(groupings[[x]]),
          pillar_weight = weightings[x],
          metric_weight = groupings[[x]],
          total_weight = pillar_weight * metric_weight
        )
      }
    )

    output$tables <- DT::renderDT(
      vis_overall_table(values$risks, values$weightings),
      options = list(
        order = if (!is.null(values$risks)) {
          list(match("Total", names(values$risks)) - 1, "desc")
        } else {
          list(0, "desc")
        },
        pageLength = 15,
        searching = FALSE
      )
    )
  })

  observe({
    req(!is.null(data()))
    req(!is.null(values$groupings))
    output$pillar_weights <- renderUI({
      validate(need(!is.null(data()), "Please upload a valid file."))
      map(names(data()$groupings), function(i) {
        sliderInput(inputId = i, label = i, min = 0, max = 10, value = 5)
      })
    })

    output$indicator_weights <- renderUI(
      do.call(
        tabsetPanel,
        map(
          names(data()$groupings),
          function(group) {
            tabPanel(
              title = group,
              map(names(data()$groupings[[group]]), function(j) {
                sliderInput(
                  inputId = j,
                  label = j,
                  min = 0,
                  max = 10,
                  value = 5
                )
              })
            )
          }
        )
      )
    )

    # ---- YOUR MAP OUTPUT SECTION ----
    output$maps <- renderUI({
      validate(need(!is.null(data()), "No valid data available."))
      req(values$risks, shape())
      nms <- c(names(data()$groupings), "Total") # 4 names expected

      # Build 4 cells
      cells <- lapply(nms, function(name) {
        div(
          class = "map-cell",
          renderPlot(vis_scores(values$risks, shape(), name, title = name))
        )
      })

      # Wrap in CSS grid
      div(class = "map-grid", cells)
    })
  }) # <-- CLOSE observe() PROPERLY

  # ------------------------ DOWNLOAD HANDLERS ------------------------

  output$download_data <- downloadHandler(
    filename = "risk_scores.csv",
    content = \(file) {
      write.csv(values$risks, file, row.names = FALSE, na = "NA")
    }
  )

  output$download_weightings <- downloadHandler(
    filename = "weightings.csv",
    content = \(file) {
      write.csv(values$weightings_table, file, row.names = FALSE, na = "NA")
    }
  )
} # <-- CLOSE server FUNCTION PROPERLY

shinyApp(ui = ui, server = server)
