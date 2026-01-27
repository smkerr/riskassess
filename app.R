# --- Packages ---------------------------------------------------------
library(shiny)
library(shinymanager)
library(bslib)
library(dplyr)
library(purrr)
library(magrittr)
library(scales)
library(ggplot2)
library(DT)
library(sf)
library(whomapper)
library(readxl)
library(openxlsx)
library(zip)
library(countrycode)

options(
  bslib.inspect = FALSE,
  shiny.autoreload = FALSE,
  sass.cache = TRUE,
  shiny.devmode = FALSE
)


# --- Helper functions -------------------------------------------------
helper_files <- file.path(
  "R",
  c(
    "get_risks.R",
    "make_indicator_table.R",
    "read_data.R",
    "read_shape.R",
    "validate_indicator_weights.R",
    "validate_pillar_weights.R",
    "vis_risk_table.R",
    "vis_scores.R"
  )
)

invisible(lapply(helper_files[file.exists(helper_files)], source))


# --- Authentication configuration -------------------------------------
credentials <- data.frame(
  user = "user",
  # TODO: revert password
  user = "",
  password = "",
  permissions = "admin",
  name = "WHO User",
  stringsAsFactors = FALSE
)


# --- UI ---------------------------------------------------------------
## --- Download/Upload Data --------------------------------------------
app_header_actions_ui <- function() {
  div(
    class = "d-flex align-items-center gap-2 ms-auto",

    actionButton(
      "open_upload_modal",
      label = "Upload workbook",
      icon = icon("upload"),
      class = "btn btn-primary"
    ),

    uiOutput("header_download_button")
  )
}

## --- Title -----------------------------------------------------------
app_title_ui <- function() {
  div(
    class = "d-flex align-items-center w-100 gap-3",

    div(
      class = "d-flex align-items-center gap-2",
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

    app_header_actions_ui()
  )
}

## --- Theme -----------------------------------------------------------
app_theme <- function() {
  bs_theme(
    bootswatch = "yeti",

    # Source: https://srhdteuwpubsa.z6.web.core.windows.net/gho/data/design-language/design-system/typography/
    base_font = font_google("Noto Sans"),

    # Core text colors
    fg = "#000000", # body text
    bg = "#FFFFFF",

    # Headings
    heading_color = "#009CDE", # WHO blue

    # Secondary text
    secondary = "#595959", # gray

    # Links
    link_color = "#009CDE", # WHO primary blue

    # Status colors
    danger = "#9a3709",
    warning = "#754d06",
    success = "#1d6339",
    info = "#245993",

    weights = c(400, 600, 700)
  )
}

## --- Sidebar UI ------------------------------------------------------
sidebar_ui <- function() {
  bslib::sidebar(
    width = 360,
    position = "left",
    open = "open",
    collapsible = TRUE,
    tabsetPanel(
      ### --- Instructions ---------------------------------------------
      tabPanel(
        title = "Instructions",
        tags$div(
          class = "p-2",
          tags$h5("How to use this app"),
          tags$p(
            "This application displays results from the WHO Seasonal Risk Assessment Excel workbook. ",
            tags$span(
              class = "sidebar-strong",
              "All indicators, weights, and scores must be defined in Excel before upload."
            ),
            " The app does not calculate or modify results."
          ),

          tags$h5("Quick start"),
          tags$ol(
            tags$li(
              tags$span(
                class = "sidebar-strong",
                "Complete the Excel workbook"
              ),
              tags$ul(
                tags$li(
                  "Select indicators and assign them to Exposure, Vulnerability, or Coping Capacity."
                ),
                tags$li("Define pillar weights and indicator weights."),
                tags$li("Enter indicator scores for each geographic unit."),
                tags$li(
                  "Confirm that composite scores are calculated in the workbook."
                )
              )
            ),
            tags$li(
              tags$span(
                class = "sidebar-strong",
                "Upload the completed workbook"
              ),
              " using the Upload workbook button above."
            ),
            tags$li(
              tags$span(class = "sidebar-strong", "View results"),
              " in the Risk Scores tabs."
            )
          ),

          tags$h5("Notes"),
          tags$ul(
            tags$li(
              "Results are relative and intended for comparison within the same assessment."
            ),
            tags$li(
              "To test alternative assumptions, update weights or indicator selections in Excel and re-upload the file."
            )
          ),

          tags$hr(),
          tags$p(tags$em(
            "Methodological documentation link will be provided here."
          ))
        )
      ),
      ### --- Pillar Weights -------------------------------------------
      tabPanel(
        title = "Select Pillar Weights",
        uiOutput("pillar_weights"),
        uiOutput("pillar_validation_msg")
      ),
      ### --- Indicator Weights ----------------------------------------
      tabPanel(
        title = "Select Indicator Weights",
        uiOutput("indicator_weights"),
        uiOutput("indicator_validation_msg")
      )
    )
  )
}


## --- Main UI ---------------------------------------------------------
main_ui <- function() {
  tagList(
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
      /* Fix blurry text in bslib sidebar */
      .bslib-sidebar,
      .bslib-sidebar * {
        transform: none !important;
        backface-visibility: hidden;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }

      /* Force clean font rendering */
      .bslib-sidebar {
        font-family: system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
      }
      "
    )),
    ### --- Summary Tables ---------------------------------------------
    fluidRow(
      column(
        width = 12,
        h3("Risk Scores"),
        tabsetPanel(
          id = "score_tabs",

          tabPanel(
            title = "Composite Risk Scores",
            br(),
            uiOutput("table_overall")
          ),
          tabPanel(
            "Exposure",
            br(),
            uiOutput("table_exposure")
          ),
          tabPanel(
            "Vulnerability",
            br(),
            uiOutput("table_vulnerability")
          ),
          tabPanel(
            "Coping Capacity",
            br(),
            uiOutput("table_coping_capacity")
          )
        )
      )
    ),

    ## --- Maps --------------------------------------------------------
    fluidRow(
      column(
        width = 12,
        uiOutput("maps"),
        uiOutput("map_download_buttons")
      )
    )
  )
}


## --- Custom Login JS -------------------------------------------------
auth_js <- function() {
  tags$head(
    tags$script(HTML(
      "
      // Intercept Enter and delay submit
      $(document).on('keydown', '#auth-user_pwd, #auth-user_id', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();

          // hide any previous error immediately
          $('.shinymanager-authentication .alert').hide();

          $(this).blur();
          setTimeout(function() {
            $('#auth-go_auth').click();
          }, 150);

          return false;
        }
      });

      // As soon as authentication UI starts disappearing, hide error
      const authObserver = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (!$('.shinymanager-authentication').is(':visible')) {
            $('.shinymanager-authentication .alert').hide();
          }
        });
      });

      $(document).ready(function() {
        const target = document.body;
        authObserver.observe(target, { childList: true, subtree: true });
      });
      "
    ))
  )
}


## --- CSS -------------------------------------------------------------
global_css <- function() {
  tagList(
    tags$style(HTML(
      "
      @import url('https://fonts.googleapis.com/css2?family=Noto+Sans:wght@400;600;700&display=swap');

      body,
      .shinymanager-container,
      .shinymanager-authentication {
        font-family: 'Noto Sans', sans-serif !important;
      }

      /* smooth visual transition after submit */
      .shinymanager-authentication {
        transition: opacity 0.4s ease;
      }

      .shinymanager-authentication.loading {
        opacity: 0.6;
        pointer-events: none;
      }

      /* Header buttons */
      .bslib-page-title .btn {
        font-weight: 600;
        padding: 6px 14px;
      }

      /* Keep buttons compact in header */
      .bslib-page-title .btn i {
        margin-right: 6px;
      }

      /* Helper text spacing */
      .bslib-page-title .text-muted,
      .bslib-page-title .text-warning {
        margin-top: 2px;
      }

      /* Disabled primary button – visually distinct but readable */
      .bslib-page-title .btn.btn-primary.btn-disabled {
          background-color: #6c757d !important;
          border-color: #6c757d !important;

          color: #ffffff !important;
          cursor: not-allowed;
          pointer-events: none;
        }

      .bslib-page-title .btn.btn-primary.btn-disabled:hover {
          background-color: #6c757d !important;
          border-color: #6c757d !important;
        }
      "
    ))
  )
}


## --- Render UI -------------------------------------------------------
ui <- shinymanager::secure_app(
  page_sidebar(
    title = app_title_ui(),
    theme = app_theme(),
    fillable = TRUE,
    sidebar = sidebar_ui(),
    main_ui()
  ),
  head_auth = auth_js(),
  tags_top = global_css(),
  language = "en"
)


# --- Server -----------------------------------------------------------
server <- function(input, output, session) {
  ## --- Authentication ------------------------------------------------
  shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials)
  )

  ## --- Data ingestion ------------------------------------------------
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

  country_from_workbook <- reactive({
    req(input$upload_data)

    country <- readxl::read_excel(
      input$upload_data$datapath,
      sheet = "1. Describe Your Emergency",
      range = "D6",
      col_names = FALSE
    )[[1, 1]]

    validate(
      need(
        is.character(country) && nzchar(country),
        "Country name in '1. Describe Your Acute Emergency'!D6 is missing."
      )
    )

    trimws(country)
  })

  iso3_from_country <- reactive({
    req(country_from_workbook())

    iso3 <- countrycode(
      sourcevar = country_from_workbook(),
      origin = "country.name",
      destination = "iso3c"
    )

    validate(
      need(
        !is.na(iso3),
        paste("Could not map country name to ISO3:", country_from_workbook())
      )
    )

    iso3
  })

  shape <- reactive({
    req(input$upload_data)
    whomapper::pull_sfs(
      adm_level = 1,
      iso3 = iso3_from_country(), # Aligns with Country / Territory value entered in 1. Describe Your Emergency
      query_server = TRUE
    ) %>%
      rename(`Subnational Level` = adm1_viz_name)
  })

  observeEvent(input$upload_data, {
    weights$pillar <- readxl::read_excel(
      input$upload_data$datapath,
      sheet = "4. Define Weights",
      range = "B7:C10" # Align with Step 4A. Define Pillar Weights table
    ) |>
      as.data.frame()

    raw_indicator_weights_tbl <- readxl::read_excel(
      input$upload_data$datapath,
      sheet = "4. Define Weights",
      range = cellranger::cell_cols("F:I")
    )

    weights$indicator <- raw_indicator_weights_tbl |> # Align with Step 4B. Define Indicator Weights table
      # use first row as column names
      rlang::set_names(unlist(raw_indicator_weights_tbl[1, ])) |>
      # drop header row
      dplyr::slice(-1) |>
      # drop empty rows
      dplyr::filter(!is.na(Indicator), Indicator != "") |>
      # validate numeric values
      dplyr::mutate(
        `Indicator Weight` = as.numeric(`Indicator Weight`),
        `Overall Weight` = as.numeric(`Overall Weight`)
      ) |>
      as.data.frame()

    # Initial validation
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

      values$risks <- NULL
      return()
    }
  })

  ## --- Reactive state ------------------------------------------------
  values <- reactiveValues(
    risks = NULL,
    groupings = NULL,
    weightings = NULL,
    weightings_table = NULL
  )

  ### --- Weight defaults ----------------------------------------------
  weights <- reactiveValues(
    pillar = NULL,
    indicator = NULL
  )

  ### --- Weight updates -----------------------------------------------
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

  ### --- Weight validation --------------------------------------------
  pillar_validation <- reactive({
    req(pillar_weights_updated())
    validate_pillar_weights(pillar_weights_updated())
  })

  indicator_validation <- reactive({
    req(indicator_weights_updated())
    validate_indicator_weights(indicator_weights_updated())
  })

  weights_valid <- reactive({
    pv <- pillar_validation()
    iv <- indicator_validation()

    pv$valid && all(iv$valid)
  })

  ## --- Weight computation --------------------------------------------
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

  ## --- Risk computation ----------------------------------------------
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
      left_join(values$risks, by = "Subnational Level")
  })

  ## --- Tables --------------------------------------------------------
  no_workbook_message <- function() {
    tagList(
      tags$h4("No workbook uploaded"),
      tags$p(
        "To view risk scores, upload a completed WHO Seasonal Risk Assessment Excel workbook ",
        "using the Upload workbook button in the upper righthand corner."
      ),
      tags$p(
        "The workbook must include defined indicators, weights, and scores. "
      )
    )
  }

  weights_invalid_message <- function() {
    tags$p("Tables are disabled until all weights sum to 100%.")
  }

  output$table_overall <- renderUI({
    if (is.null(input$upload_data)) {
      return(no_workbook_message())
    }
    if (!weights_valid()) {
      return(weights_invalid_message())
    }
    DT::dataTableOutput("table_overall_dt")
  })

  output$table_overall_dt <- DT::renderDT({
    req(values$risks)

    df <- values$risks %>%
      dplyr::select(
        `Subnational Level`,
        Exposure,
        Vulnerability,
        `Coping Capacity`,
        `Composite Risk Score`
      )

    vis_risk_table(df, values$weightings)
  })

  output$table_exposure <- renderUI({
    if (is.null(input$upload_data)) {
      return(no_workbook_message())
    }
    if (!weights_valid()) {
      return(weights_invalid_message())
    }
    DT::dataTableOutput("table_exposure_dt")
  })

  output$table_exposure_dt <- DT::renderDT({
    req(values$risks, values$groupings, data())

    df <- make_indicator_table(
      scores = data()$scores,
      risks = values$risks,
      groupings = values$groupings,
      pillar_name = "Exposure"
    )
    vis_risk_table(df, values$groupings[["Exposure"]])
  })

  output$table_vulnerability <- renderUI({
    if (is.null(input$upload_data)) {
      return(no_workbook_message())
    }
    if (!weights_valid()) {
      return(weights_invalid_message())
    }
    DT::dataTableOutput("table_vulnerability_dt")
  })

  output$table_vulnerability_dt <- DT::renderDT({
    req(values$risks, values$groupings, data())

    df <- make_indicator_table(
      scores = data()$scores,
      risks = values$risks,
      groupings = values$groupings,
      pillar_name = "Vulnerability"
    )
    vis_risk_table(df, values$groupings[["Vulnerability"]])
  })

  output$table_coping_capacity <- renderUI({
    if (is.null(input$upload_data)) {
      return(no_workbook_message())
    }
    if (!weights_valid()) {
      return(weights_invalid_message())
    }
    DT::dataTableOutput("table_coping_capacity_dt")
  })

  output$table_coping_capacity_dt <- DT::renderDT({
    req(values$risks, values$groupings, data())

    df <- make_indicator_table(
      scores = data()$scores,
      risks = values$risks,
      groupings = values$groupings,
      pillar_name = "Coping Capacity"
    )
    vis_risk_table(df, values$groupings[["Coping Capacity"]])
  })

  ## --- Maps ----------------------------------------------------------
  observe({
    req(weights_valid(), values$risks, shape())

    map_order <- c("Exposure", "Vulnerability", "Coping Capacity")
    nms <- c(
      intersect(map_order, names(values$groupings)),
      "Composite Risk Score"
    )

    lapply(nms, function(name) {
      local({
        nm <- name
        output[[paste0("map_", gsub(" ", "_", nm))]] <- renderPlot({
          vis_scores(
            map_sf = map_sf(),
            value = nm,
            title = nm
          )
        })
      })
    })
  })

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

      map_order <- c("Exposure", "Vulnerability", "Coping Capacity")

      nms <- c(
        intersect(map_order, names(values$groupings)),
        "Composite Risk Score"
      )
      cells <- lapply(nms, function(name) {
        div(
          class = "map-cell",
          plotOutput(outputId = paste0("map_", gsub(" ", "_", name)))
        )
      })

      div(class = "map-grid", !!!cells)
    }
  })

  output$map_download_buttons <- renderUI({
    req(weights_valid(), values$risks, shape())

    div(
      class = "d-flex justify-content-end mt-3 mb-4",
      downloadButton(
        "download_maps_png",
        "Download maps",
        class = "btn-primary"
      )
    )
  })

  output$download_maps_png <- downloadHandler(
    filename = function() {
      paste0("WHO_Seasonal_Risk_Assessment_Maps_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmpdir <- tempdir()

      map_names <- c(
        intersect(
          c("Exposure", "Vulnerability", "Coping Capacity"),
          names(values$groupings)
        ),
        "Composite Risk Score"
      )

      png_files <- character(0)

      for (nm in map_names) {
        p <- vis_scores(
          map_sf = map_sf(),
          value = nm,
          title = nm
        )

        outfile <- file.path(
          tmpdir,
          paste0("WHO_Seasonal_Risk_Assessment_Maps_", nm, ".png")
        )

        ggsave(
          filename = outfile,
          plot = p,
          width = 8,
          height = 6,
          dpi = 300,
          bg = "white"
        )

        png_files <- c(png_files, outfile)
      }

      zip::zipr(file, png_files)
    }
  )

  ## --- Weight Tables UI ----------------------------------------------
  output$pillar_weights <- renderUI({
    if (is.null(input$upload_data)) {
      return(
        tags$div(
          class = "p-2",
          tags$h5("No workbook uploaded"),
          tags$p(
            "Pillar weights are defined in the Seasonal Risk Assessment Excel workbook."
          ),
          tags$p(
            "Upload a completed workbook to view and review pillar weights."
          )
        )
      )
    }

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
    if (is.null(input$upload_data)) {
      return(
        tags$div(
          class = "p-2",
          tags$h5("No workbook uploaded"),
          tags$p(
            "Indicator weights are defined in the Seasonal Risk Assessment Excel workbook."
          ),
          tags$p(
            "Upload a completed workbook to view and review indicator weights."
          )
        )
      )
    }

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

    do.call(tabsetPanel, c(list(id = "indicator_tab"), tabs))
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

  ## --- Workbook upload/download --------------------------------------
  observeEvent(input$open_upload_modal, {
    showModal(
      modalDialog(
        title = "Upload WHO Seasonal Risk Assessment workbook",

        fileInput(
          "upload_data",
          label = NULL,
          buttonLabel = "Choose Excel file",
          accept = c(".xlsx"),
          width = "100%"
        ),

        footer = modalButton("Close"),
        easyClose = TRUE
      )
    )
  })

  output$header_download_button <- renderUI({
    btn_class <- "btn btn-primary"
    btn_icon <- icon("download")
    warning_text <- NULL

    if (is.null(input$upload_data)) {
      btn_class <- "btn btn-secondary"
      btn_icon <- icon("lock")
    } else if (!weights_valid()) {
      btn_class <- "btn btn-secondary"
      btn_icon <- icon("lock")
    }

    div(
      class = "d-flex flex-column align-items-start",
      downloadButton(
        "download_updated_file",
        label = "Download workbook",
        icon = btn_icon,
        class = btn_class
      )
    )
  })

  # File downloads
  output$download_updated_file <- downloadHandler(
    filename = function() {
      tryCatch(
        {
          paste0("WHO_Seasonal_Risk_Assessment_Tool_", Sys.Date(), ".xlsx")
        },
        error = function(e) {
          showNotification(
            paste("Error in filename function:", e$message),
            type = "error",
            duration = NULL
          )
          "download.xlsx"
        }
      )
    },
    content = function(file) {
      tryCatch(
        {
          req(input$upload_data)

          pillar_data_check <- pillar_weights_updated()

          indicator_data_check <- indicator_weights_updated()

          weights_check <- weights_valid()

          validate(
            need(
              !is.null(input$upload_data$datapath),
              "Upload data path is missing"
            )
          )

          # Strategy: Copy the original file and modify only specific cells
          # This preserves all formatting, formulas, and conditional formatting
          upload_path <- input$upload_data$datapath

          # Create a temporary copy of the uploaded file
          temp_copy <- tempfile(fileext = ".xlsx")
          file.copy(upload_path, temp_copy, overwrite = TRUE)

          # Try openxlsx2 first (better at handling complex files)
          if (requireNamespace("openxlsx2", quietly = TRUE)) {
            wb <- openxlsx2::wb_load(temp_copy)
            wb_type <- "openxlsx2"
          } else {
            # Try openxlsx loadWorkbook with the copy
            wb <- tryCatch(
              {
                loaded_wb <- openxlsx::loadWorkbook(temp_copy)
                loaded_wb
              },
              error = function(e) {
                # Fallback: recreate workbook
                sheet_names <- readxl::excel_sheets(temp_copy)
                new_wb <- openxlsx::createWorkbook()
                for (sheet_name in sheet_names) {
                  tryCatch(
                    {
                      sheet_data <- readxl::read_excel(
                        temp_copy,
                        sheet = sheet_name,
                        col_names = FALSE,
                        .name_repair = "minimal"
                      )
                      openxlsx::addWorksheet(new_wb, sheet_name)
                      openxlsx::writeData(
                        new_wb,
                        sheet = sheet_name,
                        x = sheet_data,
                        colNames = FALSE
                      )
                    },
                    error = function(e2) {
                      message(
                        "Warning: Could not process sheet '",
                        sheet_name,
                        "': ",
                        e2$message
                      )
                    }
                  )
                }
                new_wb
              }
            )
            wb_type <- "openxlsx"
          }

          # Write pillar weights to "4. Define Weights" sheet
          sheet_list <- if (wb_type == "openxlsx2") {
            wb$sheet_names
          } else {
            names(wb)
          }

          if ("4. Define Weights" %in% sheet_list) {
            pillar_data <- pillar_weights_updated()

            # Row 7 = headers, Rows 8-10 = data
            if (wb_type == "openxlsx2") {
              # openxlsx2 syntax
              wb <- openxlsx2::wb_add_data(
                wb,
                sheet = "4. Define Weights",
                x = pillar_data,
                start_row = 8,
                start_col = 2,
                col_names = FALSE
              )
            } else {
              # openxlsx syntax
              openxlsx::writeData(
                wb,
                sheet = "4. Define Weights",
                x = pillar_data,
                startRow = 8,
                startCol = 2,
                colNames = FALSE
              )
            }

            # Write indicator weights to the same sheet
            indicator_data <- indicator_weights_updated()

            if (
              !is.null(indicator_data) &&
                nrow(indicator_data) > 0 &&
                "Indicator Weight" %in% names(indicator_data)
            ) {
              ind_weights <- indicator_data[["Indicator Weight"]]

              # Write just the Indicator Weight column values
              # Row 7 = column headers, Rows 8+ = data
              if (wb_type == "openxlsx2") {
                wb <- openxlsx2::wb_add_data(
                  wb,
                  sheet = "4. Define Weights",
                  x = as.data.frame(ind_weights),
                  start_row = 8,
                  start_col = 8,
                  col_names = FALSE
                )
              } else {
                openxlsx::writeData(
                  wb,
                  sheet = "4. Define Weights",
                  x = ind_weights,
                  startRow = 8,
                  startCol = 8,
                  colNames = FALSE
                )
              }
            }
          }

          if (wb_type == "openxlsx2") {
            openxlsx2::wb_save(wb, file, overwrite = TRUE)
          } else {
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          }
        },
        error = function(e) {
          showNotification(
            paste(
              "Download error:",
              e$message,
              "\nCall stack:",
              paste(deparse(sys.calls()), collapse = "\n")
            ),
            type = "error",
            duration = NULL
          )
          message("ERROR: ", e$message)
          message(
            "ERROR traceback: ",
            paste(deparse(sys.calls()), collapse = "\n")
          )
          stop(e)
        }
      )
    }
  )
}

shinyApp(ui = ui, server = server)
