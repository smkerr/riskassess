library(shiny)
library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(DT)
library(openxlsx)
library(readxl)
library(sf)
library(scales)
library(bslib)
library(whomapper)

# Load helper functions
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

# --- UI ---------------------------------------------------------------
ui <- page_sidebar(
  title = "WHO Seasonal Risk Assessment Tool",
  theme = bs_theme(bootswatch = "litera"),
  fillable = TRUE,
  ## --- Sidebar ---
  sidebar = sidebar(
    width = 360,
    position = "left",
    open = "open",
    collapsible = TRUE,
    tabsetPanel(
      ### --- Upload/Download ---
      tabPanel(
        title = "Upload/Download",
        h5("Upload Risk Scores File"),
        p(
          class = "text-muted small mb-3",
          style = "font-family: inherit;",
          "Upload the completed ",
          strong("WHO Seasonal Risk Assessment Tool workbook"),
          " containing risk scores by indicator. ",
          "This file will be used to update calculations across the app."
        ),
        fileInput(
          "upload_data",
          label = NULL,
          buttonLabel = "Upload Workbook",
          accept = c(".xlsx", ".xls")
        ),
        h5("Download Updated File"),
        p(
          class = "text-muted small mb-3",
          style = "font-family: inherit;",
          "Download the ",
          strong("WHO Seasonal Risk Assessment Tool workbook"),
          " with updated weights and recalculated risk scores applied.",
        ),
        downloadButton("download_updated_file", "Download Workbook")
      ),
      ### --- Pillar Weights ---
      tabPanel(
        title = "Select Pillar Weights",
        uiOutput("pillar_weights")
      ),
      ### --- Indicator Weights ---
      tabPanel(
        title = "Select Indicator Weights",
        uiOutput("indicator_weights")
      )
    )
  ),

  # TODO: move elsewhere
  tags$style(HTML(
    "
    .map-grid {
      display: grid;
      grid-template-columns: 20% 20% 20% 40%;
      gap: 20px;
      align-items: start;
    }
    .map-cell {
      padding: 5px;
    }
    "
  )),

  ## --- Tables ---
  fluidRow(
    column(
      width = 12,
      h3("Risk Scores"),
      tabsetPanel(
        id = "score_tabs",

        tabPanel("Overall Risk Score", br(), dataTableOutput("table_overall")),
        tabPanel("Exposure", br(), dataTableOutput("table_exposure")),
        tabPanel("Vulnerability", br(), dataTableOutput("table_vulnerability")),
        tabPanel("LOCC", br(), dataTableOutput("table_lcc"))
      )
    )
  ),

  ## --- Maps ---
  fluidRow(
    column(
      width = 12,
      uiOutput("maps")
    )
  )
)

# --- Server -----------------------------------------------------------
server <- function(input, output) {
  # Read uploaded data
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

  # Load shapefile for mapping
  shape <- reactive({
    req(input$upload_data)
    whomapper::pull_sfs(
      adm_level = 1,
      iso3 = "UKR",
      query_server = TRUE
    ) %>%
      rename(Adm1 = adm1_viz_name)
  })

  values <- reactiveValues(risks = NULL)

  # Stores the editable weight tables loaded from Excel
  weights <- reactiveValues(
    pillar = NULL,
    indicator = NULL
  )

  # Compute pillar weights from inputs or defaults
  pillar_weightings <- reactive({
    req(weights$pillar)

    vals <- vapply(
      seq_len(nrow(weights$pillar)),
      function(i) {
        p <- weights$pillar$Pillar[i]
        input_val <- input[[paste0("pillar_", p)]]
        if (is.null(input_val)) {
          weights$pillar$`Pillar Weight`[i]
        } else {
          input_val / 100
        }
      },
      numeric(1)
    )

    vals <- vals / sum(vals, na.rm = TRUE)
    setNames(vals, weights$pillar$Pillar)
  })

  # Compute indicator weights within each pillar
  indicator_groupings <- reactive({
    req(weights$indicator)

    split(weights$indicator, weights$indicator$Pillar) |>
      map(function(df) {
        vals <- vapply(
          seq_len(nrow(df)),
          function(i) {
            id <- paste0("indicator_", df$Pillar[i], "_", i)
            input_val <- input[[id]]
            if (is.null(input_val)) {
              df$`Indicator Weight`[i]
            } else {
              input_val / 100
            }
          },
          numeric(1)
        )
        vals <- vals / sum(vals, na.rm = TRUE)
        setNames(vals, df$Indicator)
      })
  })

  # Update risks and weighting table when new data or inputs change
  observe({
    req(data(), weights$pillar, weights$indicator)

    values$groupings <- indicator_groupings()
    values$weightings <- pillar_weightings()

    values$weightings_table <- map_dfr(
      seq_along(values$weightings),
      \(x) {
        tibble(
          pillar = names(values$groupings)[x],
          metric = names(values$groupings[[x]]),
          pillar_weight = values$weightings[x],
          metric_weight = values$groupings[[x]],
          total_weight = values$weightings[x] * values$groupings[[x]]
        )
      }
    )

    values$risks <- get_risks(
      groupings = values$groupings,
      scores = data()$scores,
      weightings = values$weightings
    )
  })

  # Load weight tables from uploaded workbook
  observeEvent(input$upload_data, {
    req(input$upload_data$datapath)

    weights$pillar <- readxl::read_excel(
      input$upload_data$datapath,
      sheet = "Pillar Weights",
      range = "A1:B4"
    ) |>
      as.data.frame()

    weights$indicator <- readxl::read_excel(
      input$upload_data$datapath,
      sheet = "Indicator Weights",
      range = "A9:F24"
    ) |>
      as.data.frame()
  })

  # Main observer for table rendering
  observe({
    req(!is.null(data()))

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

    output$table_overall <- DT::renderDT(
      vis_risk_table(values$risks, values$weightings),
      rownames = FALSE,
      options = list(order = FALSE, pageLength = 10, searching = FALSE)
    )

    output$table_exposure <- DT::renderDT({
      df <- make_indicator_table(
        scores = data()$scores,
        risks = values$risks,
        groupings = values$groupings,
        pillar_name = "Exposure"
      )
      vis_risk_table(df, values$groupings[["Exposure"]])
    })

    output$table_vulnerability <- DT::renderDT({
      df <- make_indicator_table(
        scores = data()$scores,
        risks = values$risks,
        groupings = values$groupings,
        pillar_name = "Vulnerability"
      )
      vis_risk_table(df, values$groupings[["Vulnerability"]])
    })

    output$table_lcc <- DT::renderDT({
      df <- make_indicator_table(
        scores = data()$scores,
        risks = values$risks,
        groupings = values$groupings,
        pillar_name = "LOCC"
      )
      vis_risk_table(df, values$groupings[["LOCC"]])
    })

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

  # UI for editing pillar and indicator weights
  observe({
    req(!is.null(data()))

    output$pillar_weights <- renderUI({
      req(weights$pillar)
      tagList(
        lapply(seq_len(nrow(weights$pillar)), function(i) {
          pillar_name <- weights$pillar$Pillar[i]
          pillar_weight <- weights$pillar$`Pillar Weight`[i]
          numericInput(
            inputId = paste0("pillar_", pillar_name),
            label = paste0(pillar_name, " (%)"),
            value = round(pillar_weight * 100, 2),
            min = 0,
            max = 100,
            step = 5
          )
        })
      )
    })

    output$indicator_weights <- renderUI({
      req(weights$indicator)
      do.call(
        tabsetPanel,
        lapply(unique(weights$indicator$Pillar), function(pillar) {
          df <- weights$indicator |> filter(Pillar == pillar)
          tabPanel(
            title = pillar,
            tagList(
              lapply(seq_len(nrow(df)), function(i) {
                numericInput(
                  inputId = paste0("indicator_", pillar, "_", i),
                  label = paste0(df$Indicator[i], " (%)"),
                  value = round(df$`Indicator Weight`[i] * 100, 2),
                  min = 0,
                  max = 100,
                  step = 5
                )
              })
            )
          )
        })
      )
    })

    # Render maps
    output$maps <- renderUI({
      validate(need(!is.null(data()), "No valid data available."))
      req(values$risks, shape())

      map_order <- c("Exposure", "Vulnerability", "LOCC")

      nms <- c(
        intersect(map_order, names(values$groupings)),
        "Total"
      )
      cells <- lapply(nms, function(name) {
        div(
          class = "map-cell",
          renderPlot(
            vis_scores(
              values$risks,
              shape(),
              name,
              title = name
            )
          )
        )
      })

      div(class = "map-grid", cells)
    })
  })

  # File downloads
  output$download_data <- downloadHandler(
    filename = "risk_scores.csv",
    content = \(file) {
      write.csv(values$risks, file, row.names = FALSE, na = "NA")
    }
  )

  output$download_updated_file <- downloadHandler(
    filename = function() {
      paste0("WHO Seasonal Risk Assessment Tool_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(input$upload_data$datapath, weights$pillar, weights$indicator)

      wb <- openxlsx::loadWorkbook(input$upload_data$datapath)

      if ("Pillar Weights" %in% names(wb)) {
        openxlsx::writeData(
          wb,
          sheet = "Pillar Weights",
          x = weights$pillar,
          startRow = 1,
          startCol = 1,
          colNames = TRUE,
          withFilter = TRUE
        )
      }

      if ("Indicator Weights" %in% names(wb)) {
        openxlsx::writeData(
          wb,
          sheet = "Indicator Weights",
          x = weights$indicator,
          startRow = 1,
          startCol = 1,
          colNames = TRUE,
          withFilter = TRUE
        )
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
