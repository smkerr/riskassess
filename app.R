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
  # title = "WHO Seasonal Risk Assessment Tool for Acute Emergencies",
  title = div(
    class = "app-title d-flex align-items-center gap-2",
    tags$img(
      src = "who-logo.png",
      height = "36px",
      alt = "World Health Organization"
    ),
    span(
      "WHO Seasonal Risk Assessment Tool for Acute Emergencies",
      style = "font-weight: 700;"
    )
  ),
  theme = bs_theme(
    bootswatch = "yeti", # spacelab

    # Source: https://srhdteuwpubsa.z6.web.core.windows.net/gho/data/design-language/design-system/typography/
    base_font = font_google("Noto Sans"),
    # Core text colors
    fg = "#000000", # body text
    bg = "#FFFFFF",

    # Headings
    heading_color = "#009CDE", # WHO blue

    # Muted / secondary text
    secondary = "#595959", # gray
    # Links & interactive text
    link_color = "#009CDE", # WHO primary blue

    # Status colors
    danger = "#9a3709",
    warning = "#754d06",
    success = "#1d6339",
    info = "#245993",

    weights = c(400, 600, 700)
  ),
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
        br(),
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
          " with updated weights and recalculated risk scores applied."
        ),
        # downloadButton("download_updated_file", "Download Workbook")
        uiOutput("download_button")
      ),
      ### --- Pillar Weights ---
      tabPanel(
        title = "Select Pillar Weights",
        uiOutput("pillar_weights"),
        uiOutput("pillar_validation_msg")
      ),
      ### --- Indicator Weights ---
      tabPanel(
        title = "Select Indicator Weights",
        uiOutput("indicator_weights"),
        uiOutput("indicator_validation_msg")
      )
    )
  ),

  # TODO: move elsewhere
  tags$style(HTML(
    "
  .bslib-page-title h1 {
    color: white !important;
    font-weight: 600;
  }
    .map-grid {
      display: grid;
      grid-template-columns: 1fr 1fr 1fr 2fr;
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
        tabPanel("LOCC", br(), dataTableOutput("table_locc"))
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
  empty_state_msg <- function(text) {
    div(
      style = "
      display: flex;
      align-items: center;
      justify-content: center;
      height: 300px;
      text-align: center;
      color: #6c757d;
      font-size: 1.1rem;
    ",
      div(
        strong("No data uploaded yet"),
        br(),
        text
      )
    )
  }

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

  map_sf <- reactive({
    req(values$risks, shape())

    shape() %>%
      left_join(values$risks, by = "Adm1")
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

    # ---- VALIDATION ----
    pillar_check <- validate_pillar_weights(weights$pillar)
    indicator_check <- validate_indicator_weights(weights$indicator)

    if (!pillar_check$valid || any(!indicator_check$valid)) {
      msg <- c()

      if (!pillar_check$valid) {
        msg <- c(
          msg,
          sprintf(
            "Pillar weights sum to %.1f%% (off by %+0.1f%%).",
            pillar_check$total * 100,
            pillar_check$off_by * 100
          )
        )
      }

      bad_indicators <- indicator_check |> filter(!valid)

      if (nrow(bad_indicators) > 0) {
        msg <- c(
          msg,
          paste0(
            "Indicator weights do not sum to 100% for\n",
            paste(
              sprintf(
                "%s: %.1f%% (off by %+0.1f%%)",
                bad_indicators$Pillar,
                bad_indicators$total * 100,
                bad_indicators$off_by * 100
              ),
              collapse = "\n"
            )
          )
        )
      }

      showNotification(
        paste(msg, collapse = "\n"),
        type = "error",
        duration = NULL
      )

      # HARD STOP
      values$risks <- NULL
      return()
    }
  })

  output$table_overall <- DT::renderDT({
    validate(
      need(
        input$upload_data,
        "Please upload a completed WHO Seasonal Risk Assessment workbook to view risk scores using the Upload/Download panel on the left."
      )
    )

    validate(
      need(
        weights_valid(),
        "Tables are disabled until all weights sum to 100%."
      )
    )

    vis_risk_table(values$risks, values$weightings)
  })

  output$table_exposure <- DT::renderDT({
    validate(
      need(
        input$upload_data,
        "Please upload a completed WHO Seasonal Risk Assessment workbook to view risk scores."
      )
    )

    validate(
      need(
        weights_valid(),
        "Tables are disabled until all weights sum to 100%."
      )
    )

    df <- make_indicator_table(
      scores = data()$scores,
      risks = values$risks,
      groupings = values$groupings,
      pillar_name = "Exposure"
    )
    vis_risk_table(df, values$groupings[["Exposure"]])
  })

  output$table_vulnerability <- DT::renderDT({
    validate(
      need(
        input$upload_data,
        "Please upload a completed WHO Seasonal Risk Assessment workbook to view risk scores."
      )
    )

    validate(
      need(
        weights_valid(),
        "Tables are disabled until all weights sum to 100%."
      )
    )

    df <- make_indicator_table(
      scores = data()$scores,
      risks = values$risks,
      groupings = values$groupings,
      pillar_name = "Vulnerability"
    )
    vis_risk_table(df, values$groupings[["Vulnerability"]])
  })

  output$table_locc <- DT::renderDT({
    validate(
      need(
        input$upload_data,
        "Please upload a completed WHO Seasonal Risk Assessment workbook to view risk scores."
      )
    )

    validate(
      need(
        weights_valid(),
        "Tables are disabled until all weights sum to 100%."
      )
    )

    df <- make_indicator_table(
      scores = data()$scores,
      risks = values$risks,
      groupings = values$groupings,
      pillar_name = "LOCC"
    )
    vis_risk_table(df, values$groupings[["LOCC"]])
  })

  # Main observer for table rendering
  observe({
    # req(!is.null(data()))
    # req(weights_valid())

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

    output$tables <- DT::renderDT(
      {
        validate(
          need(
            weights_valid(),
            "Tables are disabled until all weights sum to 100%."
          )
        )

        vis_overall_table(values$risks, values$weightings)
      },
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

      tabs <- lapply(unique(weights$indicator$Pillar), function(pillar) {
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

      do.call(
        tabsetPanel,
        c(list(id = "indicator_tab"), tabs)
      )
    })

    # Render maps
    output$maps <- renderUI({
      req(weights_valid())

      if (!weights_valid()) {
        div(
          class = "text-danger",
          "Maps are disabled until all weights sum to 100%."
        )
      } else {
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
                map_sf = map_sf(),
                value = name,
                title = name
              )
            )
          )
        })

        div(class = "map-grid", cells)
      }
    })
  })

  # Update weights
  pillar_weights_updated <- reactive({
    req(weights$pillar)

    df <- weights$pillar

    df$`Pillar Weight` <- vapply(
      seq_len(nrow(df)),
      function(i) {
        p <- df$Pillar[i]
        input_val <- input[[paste0("pillar_", p)]]
        if (is.null(input_val)) df$`Pillar Weight`[i] else input_val / 100
      },
      numeric(1)
    )

    df
  })

  indicator_weights_updated <- reactive({
    req(weights$indicator)

    df <- weights$indicator

    # row index within each pillar (matches how UI creates inputs)
    row_in_pillar <- ave(seq_len(nrow(df)), df$Pillar, FUN = seq_along)

    df$`Indicator Weight` <- vapply(
      seq_len(nrow(df)),
      function(i) {
        id <- paste0("indicator_", df$Pillar[i], "_", row_in_pillar[i])
        input_val <- input[[id]]
        if (is.null(input_val)) df$`Indicator Weight`[i] else input_val / 100
      },
      numeric(1)
    )

    df
  })

  pillar_validation <- reactive({
    req(pillar_weights_updated())
    validate_pillar_weights(pillar_weights_updated())
  })

  output$pillar_validation_msg <- renderUI({
    v <- pillar_validation()

    if (v$valid) {
      div(class = "text-success small", "✓ Pillar weights sum to 100%")
    } else {
      div(
        class = "text-danger small",
        sprintf(
          "Pillar weights sum to %.1f%% (off by %+0.1f%%)",
          v$total * 100,
          v$off_by * 100
        )
      )
    }
  })

  indicator_validation <- reactive({
    req(indicator_weights_updated())
    validate_indicator_weights(indicator_weights_updated())
  })

  output$indicator_validation_msg <- renderUI({
    req(input$indicator_tab)

    v <- indicator_validation()

    row <- v |> filter(Pillar == input$indicator_tab)

    if (nrow(row) == 0) {
      return(NULL)
    }

    if (row$valid) {
      div(
        class = "text-success small",
        sprintf("✓ %s indicators sum to 100%%", row$Pillar)
      )
    } else {
      div(
        class = "text-danger small",
        sprintf(
          "%s indicators sum to %.1f%% (off by %+0.1f%%)",
          row$Pillar,
          row$total * 100,
          row$off_by * 100
        )
      )
    }
  })

  weights_valid <- reactive({
    pv <- pillar_validation()
    iv <- indicator_validation()

    pv$valid && all(iv$valid)
  })

  output$download_button <- renderUI({
    if (is.null(input$upload_data)) {
      return(NULL)
    }

    if (!weights_valid()) {
      div(
        class = "text-warning small",
        "Weights must sum to 100% before downloading the updated workbook."
      )
    } else {
      downloadButton(
        "download_updated_file",
        "Download Workbook",
        class = "btn-primary"
      )
    }
  })

  # File downloads
  output$download_updated_file <- downloadHandler(
    filename = function() {
      paste0("WHO_Seasonal_Risk_Assessment_Tool_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(
        input$upload_data$datapath,
        pillar_weights_updated(),
        indicator_weights_updated(),
        weights_valid()
      )

      wb <- openxlsx::loadWorkbook(input$upload_data$datapath)

      if ("Pillar Weights" %in% names(wb)) {
        openxlsx::writeData(
          wb,
          sheet = "Pillar Weights",
          x = pillar_weights_updated(),
          startRow = 2,
          startCol = 1,
          colNames = FALSE #,
          # withFilter = TRUE
        )
      }

      if ("Indicator Weights" %in% names(wb)) {
        openxlsx::writeData(
          wb,
          sheet = "Indicator Weights",
          x = indicator_weights_updated()$`Indicator Weight`,
          startRow = 10,
          startCol = 5,
          colNames = FALSE #,
          # withFilter = TRUE
        )
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
