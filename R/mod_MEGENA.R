mod_megena_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 12,
      shinyWidgets::radioGroupButtons(
        inputId = ns("Step"),
        label = HTML('<i class="fa fa-bookmark" aria-hidden="true"></i> <font size="4" color="black"><b>STEP</b></font>'),
        choices = c("Step1：Calculate correlation", "Step2：Do MEGENA", "Step3：MEGENA module summary"),
        selected = "Step1：Calculate correlation",
        individual = TRUE,
        status = "primary",
        size = "sm",
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: white"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("Sub_Step"),
        type = "hidden",
        tabPanelBody(
          "Step Step1：Calculate correlation",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Calculate correlation</b></font>'),
            br(),
            br(),
            fileInput(
              inputId = ns("MEGENA_expr"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Upload your expression data</b></font>'),
                         bsplus::bs_button(
                           label = "?", 
                           button_type = "primary", 
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "Note：The row name is the name of the gene, the column name is the name of the sample, and the value is FPKM/RPKM/TPM.")),
              accept = ".txt",
              buttonLabel = "Browse...",
              placeholder = "No file selected",
              capture = NULL
            ),
            numericInput(
              inputId = ns("MEGENA1_perm"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Number of permutation tests</b></font>'),
                         bsplus::bs_button(
                           label = "?", 
                           button_type = "primary", 
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "The larger this number, the more precise your simulation of randomness will be, and the more reliable your assessment of statistical significance will become. However, a greater number of permutations also means that more computational resources and time will be required.")),
              min = 1,
              value = 100
            ),
            numericInput(
              inputId = ns("MEGENA1_fdr_cut"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>FDR threshold</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "FDR threshold to output final results of significant correlations.")),
              min = 0,
              max = 1,
              value = 0.05
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("MEGENA1_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("MEGENA1_reset"),
              label = "Reset",
              styleclass = "warning"
            ),
            shinysky::actionButton(
              inputId = ns("MEGENA1_exam"),
              label = "Load example",
              styleclass = "info"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("MEGENA1_tab_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("MEGENA1_tab")),
                type = "html",
                loader = "dnaspin"
              )
            )
          )
        ),
        tabPanelBody(
          "Step Step2：Do MEGENA",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Do MEGENA</b></font>'),
            br(),
            br(),
            numericInput(
              inputId = ns("MEGENA2_do_modp"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Cluster significance p-value threshold</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "A threshold used to determine the statistical significance of a cluster (or module) within a network")),
              min = 0,
              max = 1,
              value = 0.05
            ),
            numericInput(
              inputId = ns("MEGENA2_do_hubp"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Hub significance p-value threshold</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "A threshold used to determine whether the connectivity of a node (i.e., the number of connections it has with other nodes) is statistically significantly higher than the expected connectivity in a random planar network. This threshold helps us to identify hubs in the network, which are nodes with unusually high connectivity, and to distinguish them from nodes that may have higher connectivity merely by chance in a random network.")),
              min = 0,
              max = 1,
              value = 0.05
            ),
            numericInput(
              inputId = ns("MEGENA2_do_min"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Minimum cluster size</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "The minimum number of nodes required for an effective cluster.")),
              min = 1,
              value = 10
            ),
            numericInput(
              inputId = ns("MEGENA2_do_max"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Maximum cluster size</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "The maximum number of nodes required for an effective cluster.")),
              min = 1,
              value = 100
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("MEGENA2_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("MEGENA2_reset"),
              label = "Reset",
              styleclass = "warning"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("MEGENA2_tab_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("MEGENA2_tab")),
                type = "html",
                loader = "dnaspin"
              )
            )
          )
        ),
        tabPanelBody(
          "Step Step3：MEGENA module summary",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Module summary</b></font>'),
            br(),
            br(),
            numericInput(
              inputId = ns("MEGENA3_sum_modp"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Module compactness significance p-value threshold</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "The p-value for identifying modules with significant compactness.")),
              min = 0,
              max = 1,
              value = 0.05
            ),
            numericInput(
              inputId = ns("MEGENA3_sum_hubp"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Node degree significance p-value threshold</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "The p-value for identifying nodes with significantly high degree..")),
              min = 0,
              max = 1,
              value = 0.05
            ),
            numericInput(
              inputId = ns("MEGENA3_sum_min"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Minimum module size</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "Minimum module size allowed to finalize in the summary output.")),
              min = 1,
              value = 10
            ),
            numericInput(
              inputId = ns("MEGENA3_sum_max"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Maximum module size</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "maximum module size allowed to finalize in the summary output.")),
              min = 1,
              value = 100
            ),
            selectInput(
              inputId = ns("MEGENA3_module"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Select the module to view</b></font>')),
              choices = c("c1_1", "c1_2", "c1_3", "c1_4", "c1_5", "c1_6", "c1_7", "c1_8", "c1_9", "c1_10"),
              selected = "c1_5"
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
            numericInput(
              inputId = ns("MEGENA3_fig_height"), 
              label = "Plot height", 
              value = 800
            ),
            numericInput(
              inputId = ns("MEGENA3_fig_width"), 
              label = "Plot width", 
              value = 1000
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("MEGENA3_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("MEGENA3_reset"),
              label = "Reset",
              styleclass = "warning"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("MEGENA3_tab_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("MEGENA3_tab")),
                type = "html",
                loader = "dnaspin"
              )
            ),
            uiOutput(outputId = ns("MEGENA3_plot_layout")),
            column(
              width = 12,
              uiOutput(ns("MEGENA3_ui"))
            )
          )
        )
      )
    )
  )
}
mod_megena_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  ncore <- detectCores(all.tests = FALSE, logical = TRUE)
  observeEvent(input$Step, {
    updateTabsetPanel(
      session = session,
      inputId = "Sub_Step",
      selected = paste0("Step ", input$Step)
    )
  })
  observeEvent(input$MEGENA1_submit, {
    output$MEGENA1_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MEGENA1_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MEGENA1_tab_format"),
              label = "",
              width = "100%",
              status = "custom-class",
              choices = c("csv", "tsv", "txt", "xlsx"),
              individual = FALSE,
              selected = "csv",
              direction = "vertical",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: black"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: gray"))
            ),
            circle = FALSE, 
            status = "danger",
            icon = icon("gear"), 
            width = "10px",
            tooltip = tooltipOptions(title = "Select the table format")
          )
        ),
        column(
          width = 4,
          downloadButton(
            outputId = ns("MEGENA1_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  MEGENA_expr_data <- eventReactive(input$MEGENA1_submit, {
    if (input$MEGENA1_exam > 0 & is.null(input$MEGENA_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$MEGENA_expr)
        data <- read.delim(
          file = input$MEGENA_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    data <- expr_data()[rowSums(expr_data()) != 0,]
    return(data)
  })
  MEGENA_cor_data <- eventReactive(input$MEGENA1_submit, {
    data <- calculate.correlation(
      datExpr = MEGENA_expr_data(),
      doPerm = input$MEGENA1_perm,
      doPar = TRUE,
      num.cores = ncore,
      method = "pearson",
      FDR.cutoff = input$MEGENA1_fdr_cut,
      n.increment = input$MEGENA1_perm,
      is.signed = FALSE,
      output.permFDR = FALSE,
      output.corTable = FALSE,
      saveto = NULL
    )
    return(data)
  })
  MEGENA_PFN <- eventReactive(input$MEGENA1_submit, {
    data <- calculate.PFN(
      edgelist = MEGENA_cor_data()[,1:3],
      max.skipEdges = NULL,
      maxENum = NULL,
      doPar = TRUE,
      num.cores = ncore,
      keep.track = FALSE
    )
    return(data)
  })
  output$MEGENA1_tab <- renderDT({
    datatable(MEGENA_PFN(),
              rownames = 1,
              filter = "bottom",
              extensions = "FixedColumns",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$MEGENA1_tab_download <- downloadHandler(
    filename = function() {
      paste0("MEGENA_result1.", input$MEGENA1_tab_format)
    },
    content = function(file) {
      if (input$MEGENA1_tab_format == "txt") {
        write.table(MEGENA_PFN(), file, row.names = F, col.names = T, quote = F)
      } else if (input$MEGENA1_tab_format == "csv") {
        readr::write_csv(MEGENA_PFN(), file, col_names = T)
      } else if (input$MEGENA1_tab_format == "tsv") {
        readr::write_tsv(MEGENA_PFN(), file, col_names = T)
      } else if (input$MEGENA1_tab_format == "xlsx") {
        writexl::write_xlsx(MEGENA_PFN(), file, col_names = T)
      }
    }
  )
  observeEvent(input$MEGENA1_reset, {
    updateNumericInput(
      session = session,
      inputId = "MEGENA1_perm",
      value = 100
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA1_fdr_cut",
      value = 0.05
    )
  })
  observeEvent(input$MEGENA2_submit, {
    output$MEGENA2_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MEGENA2_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MEGENA2_tab_format"),
              label = "",
              width = "100%",
              status = "custom-class",
              choices = c("csv", "tsv", "txt", "xlsx"),
              individual = FALSE,
              selected = "csv",
              direction = "vertical",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: black"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: gray"))
            ),
            circle = FALSE, 
            status = "danger",
            icon = icon("gear"), 
            width = "10px",
            tooltip = tooltipOptions(title = "Select the table format")
          )
        ),
        column(
          width = 4,
          downloadButton(
            outputId = ns("MEGENA2_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  MEGENA_PFN_DF <- eventReactive(input$MEGENA2_submit, {
    data <- graph.data.frame(
      MEGENA_PFN(),
      directed = FALSE
    )
    return(data)
  })
  MEGENA_result <- eventReactive(input$MEGENA2_submit, {
    data <- do.MEGENA(
      g = MEGENA_PFN_DF(),
      do.hubAnalysis = TRUE,
      mod.pval = input$MEGENA2_do_modp,
      hub.pval = input$MEGENA2_do_hubp,
      remove.unsig = TRUE,
      min.size = input$MEGENA2_do_min,
      max.size = input$MEGENA2_do_max,
      doPar = TRUE,
      num.cores = ncore,
      n.perm = input$MEGENA1_perm,
      singleton.size = 3,
      save.output = FALSE
    )
    return(data)
  })
  MEGENA_result_tab <- eventReactive(input$MEGENA2_submit, {
    data <- module_convert_to_table(
      MEGENA.output = MEGENA_result(),
      mod.pval = input$MEGENA2_do_modp,
      hub.pval = input$MEGENA2_do_hubp,
      min.size = input$MEGENA2_do_min,
      max.size = input$MEGENA2_do_max
    )
    return(data)
  })
  output$MEGENA2_tab <- renderDT({
    datatable(MEGENA_result_tab(),
              rownames = FALSE,
              filter = "bottom",
              extensions = "FixedColumns",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$MEGENA2_tab_download <- downloadHandler(
    filename = function() {
      paste0("MEGENA_result2.", input$MEGENA2_tab_format)
    },
    content = function(file) {
      if (input$MEGENA2_tab_format == "txt") {
        write.table(MEGENA_result_tab(), file, row.names = F, col.names = T, quote = F)
      } else if (input$MEGENA2_tab_format == "csv") {
        readr::write_csv(MEGENA_result_tab(), file, col_names = T)
      } else if (input$MEGENA2_tab_format == "tsv") {
        readr::write_tsv(MEGENA_result_tab(), file, col_names = T)
      } else if (input$MEGENA2_tab_format == "xlsx") {
        writexl::write_xlsx(MEGENA_result_tab(), file, col_names = T)
      }
    }
  )
  observeEvent(input$MEGENA2_reset, {
    updateNumericInput(
      session = session,
      inputId = "MEGENA2_do_modp",
      value = 0.05
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA2_do_hubp",
      value = 0.05
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA2_do_min",
      value = 10
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA2_do_max",
      value = 100
    )
  })
  observeEvent(input$MEGENA3_submit, {
    output$MEGENA3_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MEGENA3_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MEGENA3_tab_format"),
              label = "",
              width = "100%",
              status = "custom-class",
              choices = c("csv", "tsv", "txt", "xlsx"),
              individual = FALSE,
              selected = "csv",
              direction = "vertical",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: black"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: gray"))
            ),
            circle = FALSE, 
            status = "danger",
            icon = icon("gear"), 
            width = "10px",
            tooltip = tooltipOptions(title = "Select the table format")
          )
        ),
        column(
          width = 4,
          downloadButton(
            outputId = ns("MEGENA3_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  observeEvent(input$MEGENA3_submit, {
    output$MEGENA3_plot_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MEGENA3_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MEGENA3_fig_format"),
              label = "",
              width = "100%",
              status = "custom-class",
              choices = c("png", "pptx", "pdf", "jpeg", "tiff", "bmp", "svg"),
              individual = FALSE,
              selected = "png",
              direction = "vertical",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: black"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: gray"))
            ),
            circle = FALSE, 
            status = "danger",
            icon = icon("gear"), 
            width = "10px",
            tooltip = tooltipOptions(title = "Select the image format")
          )
        ),
        column(
          width = 4,
          downloadButton(
            outputId = ns("MEGENA3_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$MEGENA3_ui <- renderUI({
    if (is.na(input$MEGENA3_fig_height) | is.na(input$MEGENA3_fig_width) | input$MEGENA3_fig_height * input$MEGENA3_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("MEGENA3_plot"),
          height = paste0(input$MEGENA3_fig_height, "px"),
          width = paste0(input$MEGENA3_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  MEGENA_sum <- eventReactive(input$MEGENA3_submit, {
    data <-  MEGENA.ModuleSummary(
      MEGENA.output = MEGENA_result(),
      mod.pvalue = input$MEGENA3_sum_modp,
      hub.pvalue = input$MEGENA3_sum_hubp,
      min.size = input$MEGENA3_sum_min,
      max.size = input$MEGENA3_sum_max,
      annot.table = NULL,
      symbol.col = NULL,
      id.col = NULL,
      output.sig = TRUE
    )
    return(data)
  })
  output$MEGENA3_tab <- renderDT({
    datatable(MEGENA_sum()$module.table,
              rownames = FALSE,
              filter = "bottom",
              extensions = "FixedColumns",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$MEGENA3_tab_download <- downloadHandler(
    filename = function() {
      paste0("MEGENA_result3.", input$MEGENA3_tab_format)
    },
    content = function(file) {
      if (input$MEGENA3_tab_format == "txt") {
        write.table(MEGENA_sum()$module.table, file, row.names = F, col.names = T, quote = F)
      } else if (input$MEGENA3_tab_format == "csv") {
        readr::write_csv(MEGENA_sum()$module.table, file, col_names = T)
      } else if (input$MEGENA3_tab_format == "tsv") {
        readr::write_tsv(MEGENA_sum()$module.table, file, col_names = T)
      } else if (input$MEGENA3_tab_format == "xlsx") {
        writexl::write_xlsx(MEGENA_sum()$module.table, file, col_names = T)
      }
    }
  )
  observeEvent(input$MEGENA3_submit, {
    updateSelectInput(
      session = session,
      inputId = "MEGENA3_module",
      choices = MEGENA_sum()$module.table$module.id,
      selected = MEGENA_sum()$module.table$module.id[1:1]
    )
  })
  output$MEGENA3_plot <- renderPlot({
    plot_module(
      output.summary = MEGENA_sum(),
      PFN = MEGENA_PFN_DF(),
      subset.module = input$MEGENA3_module,
      layout = "kamada.kawai", 
      label.hubs.only = FALSE, 
      color.code = "grey", 
      output.plot = FALSE, 
      col.names = c("magenta","green","cyan"), 
      label.scaleFactor = 1, 
      hubLabel.col = "black", 
      hubLabel.sizeProp = 0.5,
      show.legend = TRUE
    )
  })
  output$MEGENA3_fig_download <- downloadHandler(
    filename = function() {
      paste0("MEGENA3.", input$MEGENA3_fig_format)
    },
    content = function(file) {
      if (input$MEGENA3_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$MEGENA3_fig_width, 
          height = 10 * input$MEGENA3_fig_height, 
          res = 300)
      } else if (input$MEGENA3_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$MEGENA3_fig_width/20, 
          height = input$MEGENA3_fig_height/20, 
          onefile = F)
      } else if (input$MEGENA3_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$MEGENA3_fig_width, 
          height = 10 * input$MEGENA3_fig_height, 
          res = 300)
      } else if (input$MEGENA3_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$MEGENA3_fig_width, 
          height = 10 * input$MEGENA3_fig_height, 
          res = 300)
      } else if (input$MEGENA3_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$MEGENA3_fig_width, 
          height = 10 * input$MEGENA3_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$MEGENA3_fig_width/20, 
          height = input$MEGENA3_fig_height/20)
      }
      p <- plot_module(
        output.summary = MEGENA_sum(),
        PFN = MEGENA_PFN_DF(),
        subset.module = input$MEGENA3_module,
        layout = "kamada.kawai", 
        label.hubs.only = FALSE, 
        color.code = "grey", 
        output.plot = FALSE, 
        col.names = c("magenta","green","cyan"), 
        label.scaleFactor = 1, 
        hubLabel.col = "black", 
        hubLabel.sizeProp = 0.5,
        show.legend = TRUE
      )
      print(p)
      dev.off()
      if (input$MEGENA3_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          p <- plot_module(
            output.summary = MEGENA_sum(),
            PFN = MEGENA_PFN_DF(),
            subset.module = input$MEGENA3_module,
            layout = "kamada.kawai", 
            label.hubs.only = FALSE, 
            color.code = "grey", 
            output.plot = FALSE, 
            col.names = c("magenta","green","cyan"), 
            label.scaleFactor = 1, 
            hubLabel.col = "black", 
            hubLabel.sizeProp = 0.5,
            show.legend = TRUE
          )
          print(p)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$MEGENA3_reset, {
    updateNumericInput(
      session = session,
      inputId = "MEGENA3_sum_modp",
      value = 0.05
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA3_sum_hubp",
      value = 0.05
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA3_sum_min",
      value = 10
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA3_sum_max",
      value = 100
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA3_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "MEGENA3_fig_width",
      value = 1000
    )
  })
}