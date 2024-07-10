mod_wgcna_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 12,
      shinyWidgets::radioGroupButtons(
        inputId = ns("Step"),
        label =HTML('<i class="fa fa-bookmark" aria-hidden="true"></i> <font size="4" color="black"><b>STEP</b></font>'),
        choices = c("Step1：Soft threshold filtering", "Step2：Do WGCNA", "Step3：Module-trait correlation", "Step4：Module membership & Gene significance", "Step5：Module network"),
        selected = "Step1：Soft threshold filtering",
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
        #############################WGCNA1################################
        tabPanelBody(
          "Step Step1：Soft threshold filtering",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Soft threshold filtering</b></font>'),
            br(),
            br(),
            fileInput(
              inputId = ns("WGCNA_expr"),
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
            shinyWidgets::materialSwitch(
              inputId = ns("Sample_Gene_filter"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Screening samples and genes</b></font>'),
                         bsplus::bs_button(
                           label = "?",
                           button_type = "primary",
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "If you have a strong computer, it is not recommended to clean the data.")),
              value = TRUE,
              status = "danger"
            ),
            uiOutput(
              outputId = ns("Var_layout")
            ),
            uiOutput(
              outputId = ns("Var_set_layout")
            ),
            numericInput(
              inputId = ns("SFT"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Scale Free Topology Model Fit, signed R^2</b></font>')),
              min = 0,
              max = 1,
              value = 0.85
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
            numericInput(
              inputId = ns("WGCNA1_fig_height"), 
              label = "Plot height", 
              value = 800
            ),
            numericInput(
              inputId = ns("WGCNA1_fig_width"), 
              label = "Plot width", 
              value = 1000
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("WGCNA1_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("WGCNA1_reset"),
              label = "Reset",
              styleclass = "warning"
            ),
            shinysky::actionButton(
              inputId = ns("WGCNA1_exam"),
              label = "Load example",
              styleclass = "info"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("WGCNA1_tab_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("WGCNA1_tab")),
                type = "html",
                loader = "dnaspin"
              )
            ),
            uiOutput(outputId = ns("WGCNA1_plot_layout")),
            column(
              width = 12,
              uiOutput(ns("WGCNA1_ui"))
            )
          )
        ),
        #############################WGCNA2################################
        tabPanelBody(
          "Step Step2：Do WGCNA",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Do WGCNA</b></font>'),
            br(),
            br(),
            numericInput(
              inputId = ns("Power_threshold"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Soft-thresholding power for network construction</b></font>'),
                         bsplus::bs_button(
                           label = "?", 
                           button_type = "primary", 
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "Note：Select the power for R^2.")),
              min = 1,
              max = 20,
              value = 6
            ),
            numericInput(
              inputId = ns("minModuleSize"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Minimum module size for module detection</b></font>')),
              min = 1,
              value = 30
            ),
            numericInput(
              inputId = ns("maxBlockSize"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Maximum block size for module detection</b></font>')),
              min = 1,
              value = 5000
            ),
            numericInput(
              inputId = ns("mergeCutHeight"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Dendrogram cut height for module merging</b></font>')),
              min = 0,
              max = 1,
              value = 0.25
            ),
            numericInput(
              inputId = ns("deepSplit"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Sensitivity to module splitting</b></font>'),
                         bsplus::bs_button(
                           label = "?", 
                           button_type = "primary", 
                           button_size = "extra-small") %>%
                           bsplus::bs_embed_tooltip(title = "Note：0 least and 4 most sensitive.")),
              min = 0,
              max = 4,
              value = 2
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
            numericInput(
              inputId = ns("WGCNA2_fig_height"), 
              label = "Plot height", 
              value = 800
            ),
            numericInput(
              inputId = ns("WGCNA2_fig_width"), 
              label = "Plot width", 
              value = 1000
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("WGCNA2_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("WGCNA2_reset"),
              label = "Reset",
              styleclass = "warning"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("Dendroandcolors_layout")),
            column(
              width = 12,
              uiOutput(ns("Dendroandcolors_ui"))
            ),
            uiOutput(outputId = ns("WGCNA2_gene2module_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("WGCNA2_gene2module")),
                type = "html",
                loader = "dnaspin"
              )
            ),
            uiOutput(outputId = ns("WGCNA2_kME_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("WGCNA2_kME")),
                type = "html",
                loader = "dnaspin"
              )
            )
          )
        ),
        #############################WGCNA3################################
        tabPanelBody(
          "Step Step3：Module-trait correlation",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Module-trait</b></font>'),
            br(),
            br(),
            shinyWidgets::materialSwitch(
              inputId = ns("Sample_info_upload"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Upload trait information</b></font>')),
              value = FALSE,
              status = "danger"
            ),
            uiOutput(
              outputId = ns("Sample_info_layout")
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot setting</b></font>')),
            numericInput(
              inputId = ns("X_size"),
              label = "X-axis font size",
              value = 1
            ),
            numericInput(
              inputId = ns("Y_size"),
              label = "Y-axis font size",
              value = 1
            ),
            numericInput(
              inputId = ns("Content_size"),
              label = "Content font size",
              value = 1
            ),
            numericInput(
              inputId = ns("Legend_size"),
              label = "Legend font size",
              value = 1
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
            numericInput(
              inputId = ns("WGCNA3_fig_height"), 
              label = "Plot height", 
              value = 800
            ),
            numericInput(
              inputId = ns("WGCNA3_fig_width"), 
              label = "Plot width", 
              value = 1000
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("WGCNA3_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("WGCNA3_reset"),
              label = "Reset",
              styleclass = "warning"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("labeledHeatmap_layout")),
            column(
              width = 12,
              uiOutput(ns("labeledHeatmap_ui"))
            )
          )
        ),
        #############################WGCNA4################################
        tabPanelBody(
          "Step Step4：Module membership & Gene significance",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>MM & GS</b></font>'),
            br(),
            br(),
            selectInput(
              inputId = ns("Module"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Select a module</b></font>')),
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              inputId = ns("Sample"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Select a sample</b></font>')),
              choices = NULL,
              selected = NULL
            ),
            shinyWidgets::materialSwitch(
              inputId = ns("Hub"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Marker hub genes</b></font>')),
              value = TRUE,
              status = "danger"
            ),
            uiOutput(
              outputId = ns("MM_threshold_layout")
            ),
            uiOutput(
              outputId = ns("GS_threshold_layout")
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
            numericInput(
              inputId = ns("WGCNA4_fig_height"), 
              label = "Plot height", 
              value = 800
            ),
            numericInput(
              inputId = ns("WGCNA4_fig_width"), 
              label = "Plot width", 
              value = 1000
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("WGCNA4_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("WGCNA4_reset"),
              label = "Reset",
              styleclass = "warning"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("MM_GS_tab_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("MM_GS_tab")),
                type = "html",
                loader = "dnaspin"
              )
            ),
            uiOutput(outputId = ns("MM_GS_plot_layout")),
            column(
              width = 12,
              uiOutput(ns("MM_GS_plot_ui"))
            )
          )
        ),
        #############################WGCNA5################################
        tabPanelBody(
          "Step Step5：Module network",
          sidebarPanel(
            HTML('<font size="6" color="red"><b>Module network</b></font>'),
            br(),
            br(),
            selectInput(
              inputId = ns("Module_net"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Select a module</b></font>')),
              choices = NULL,
              selected = NULL
            ),
            numericInput(
              inputId = ns("WGCNA5_threshold"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Weight threshold</b></font>')),
              value = 0.2
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Node settings</b></font>')),
            shinyWidgets::colorPickr(
              inputId = ns("WGCNA5_node_border"),
              label = "Node border",
              position = "right-start",
              selected = "#9AC9DB"
            ),
            shinyWidgets::colorPickr(
              inputId = ns("WGCNA5_node_backgroud"),
              label = "Node backgroud",
              position = "right-start",
              selected = "#2878B5"
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Edge settings</b></font>')),
            shinyWidgets::colorPickr(
              inputId = ns("WGCNA5_edge"),
              label = "Edge",
              position = "right-start",
              selected = "#E7EFFA"
            ),
            selectInput(
              inputId = ns("WGCNA5_layout"),
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot layout</b></font>')),
              choices = c("layout_as_star",
                          "layout_as_tree",
                          "layout_in_circle",
                          "layout_nicely",
                          "layout_on_grid",
                          "layout_on_sphere",
                          "layout_randomly",
                          "layout_with_dh",
                          "layout_with_drl",
                          "layout_with_gem",
                          "layout_with_graphopt",
                          "layout_with_mds",
                          "layout_with_sugiyama"),
              selected = "layout_nicely"
            ),
            h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
            numericInput(
              inputId = ns("WGCNA5_fig_height"), 
              label = "Plot height", 
              value = 800
            ),
            numericInput(
              inputId = ns("WGCNA5_fig_width"), 
              label = "Plot width", 
              value = 1000
            ),
            br(),
            br(),
            shinysky::actionButton(
              inputId = ns("WGCNA5_submit"),
              label = "Submit",
              styleclass = "danger",
              size = "small"
            ),
            shinysky::actionButton(
              inputId = ns("WGCNA5_reset"),
              label = "Reset",
              styleclass = "warning"
            )
          ),
          mainPanel(
            uiOutput(outputId = ns("WGCNA5_tab_layout")),
            column(
              width = 12,
              shinycustomloader::withLoader(
                dataTableOutput(ns("WGCNA5_tab")),
                type="html", 
                loader= "dnaspin")
            ),
            column(
              width = 12,
              uiOutput(ns("WGCNA5_ui"))
            )
          )
        )
      )
    )
  )
}
mod_wgcna_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  ncore <- detectCores(all.tests = FALSE, logical = TRUE)
  enableWGCNAThreads(nThreads = ncore)
  ########################WGCNA1########################
  observeEvent(input$Step, {
    updateTabsetPanel(
      session = session,
      inputId = "Sub_Step",
      selected = paste0("Step ", input$Step)
    )
  })
  output$Var_layout <- renderUI({
    if (input$Sample_Gene_filter == "TRUE") {
      shinyWidgets::materialSwitch(
        inputId = ns("Var_set"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Removal of low-variability genes</b></font>'),
                   bsplus::bs_button(
                     label = "?",
                     button_type = "primary",
                     button_size = "extra-small") %>%
                     bsplus::bs_embed_tooltip(title = "The higher the number, the more genes are removed.")),
        value = TRUE,
        status = "danger"
      )
    } else {
      NULL
    }
  })
  observeEvent(input$Var_set, {
    output$Var_set_layout <- renderUI({
      if (input$Sample_Gene_filter == "TRUE" & input$Var_set == "TRUE") {
        numericInput(
          inputId = ns("Var_num"),
          label = "Variability threshold",
          value = 0.1,
          min = 0,
          max = 1
        )
      } else {
        NULL
      }
    })
  })
  observeEvent(input$WGCNA1_submit, {
    output$WGCNA1_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("WGCNA1_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("WGCNA1_tab_format"),
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
            outputId = ns("WGCNA1_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  observeEvent(input$WGCNA1_submit, {
    output$WGCNA1_plot_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("WGCNA1_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("WGCNA1_fig_format"),
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
            outputId = ns("WGCNA1_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$WGCNA1_ui <- renderUI({
    if (is.na(input$WGCNA1_fig_height) | is.na(input$WGCNA1_fig_width) | input$WGCNA1_fig_height * input$WGCNA1_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("WGCNA1_plot"),
          height = paste0(input$WGCNA1_fig_height, "px"),
          width = paste0(input$WGCNA1_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  WGCNA_expr_ori <- eventReactive(input$WGCNA1_submit, {
    if (input$WGCNA1_exam > 0 & is.null(input$WGCNA_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$WGCNA_expr)
        data <- read.delim(
          file = input$WGCNA_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    return(expr_data())
  })
  WGCNA_expr_filter <- eventReactive(input$WGCNA1_submit, {
    if (input$Sample_Gene_filter == "TRUE" & input$Var_set == "FALSE") {
      expr_data <- goodSamplesGenes(
        t(WGCNA_expr_ori()),
        verbose = 3
      )
      expr_good <- t(WGCNA_expr_ori())[expr_data$goodSamples, expr_data$goodGenes]
      data <- t(expr_good)
    } else if (input$Sample_Gene_filter == "TRUE" & input$Var_set == "TRUE") {
      expr_data <- goodSamplesGenes(
        t(WGCNA_expr_ori()),
        verbose = 3
      )
      expr_good <- t(WGCNA_expr_ori())[expr_data$goodSamples, expr_data$goodGenes]
      var_exp <- varFilter(
        as.matrix(t(expr_good)),
        var.func = IQR,
        var.cutoff = input$Var_num,
        filterByQuantile = T
      )
      data <- var_exp
    } else if (input$Sample_Gene_filter == "FALSE") {
      data <- WGCNA_expr_ori()
    }
    return(data)
  })
  output$WGCNA1_tab <- renderDT({
    datatable(
      WGCNA_expr_filter(),
      rownames = TRUE,
      extensions = "FixedColumns",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$WGCNA1_tab_download <- downloadHandler(
    filename = function() {
      paste0("Filter_expr_data.", input$WGCNA1_tab_format)
    },
    content = function(file) {
      expr_data <- data.frame(" " = rownames(WGCNA_expr_filter()),
                              WGCNA_expr_filter(), 
                              row.names = NULL, 
                              check.names = F)
      if (input$WGCNA1_tab_format == "txt") {
        write.table(expr_data, file, row.names = F, col.names = T, quote = F)
      } else if (input$WGCNA1_tab_format == "csv") {
        readr::write_csv(expr_data, file, col_names = T)
      } else if (input$WGCNA1_tab_format == "tsv") {
        readr::write_tsv(expr_data, file, col_names = T)
      } else if (input$WGCNA1_tab_format == "xlsx") {
        writexl::write_xlsx(expr_data, file, col_names = T)
      }
    }
  )
  WGCNA1_sft_data <- eventReactive(input$WGCNA1_submit, {
    data <- pickSoftThreshold(
      t(WGCNA_expr_filter()),
      powerVector = 1:20,
      networkType = "unsigned",
      verbose = 5)
    return(data)
  })
  output$WGCNA1_plot <- renderPlot({
    y_line <- as.numeric(input$SFT)
    p1 <- ggplot(data = WGCNA1_sft_data()$fitIndices, aes(x = Power, y = SFT.R.sq)) +
      geom_point(color = "red") +
      geom_text_repel(aes(label = Power)) +
      geom_hline(aes(yintercept = y_line),
                 color = "red") +
      labs(title = "Scale independence",
           x = "Soft Threshold (power)",
           y = "Scale Free Topology Model Fit,signed R^2") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5)) +
      annotate("text",
               x = 0,
               y = y_line,
               label = y_line,
               vjust = -1,
               color = "red")
    p2 <- ggplot(data = WGCNA1_sft_data()$fitIndices, aes(x = Power, y = mean.k.)) +
      geom_point(color = "red") +
      geom_text_repel(aes(label = Power)) +
      labs(title = "Mean connectivity",
           x = "Soft Threshold (power)",
           y = "Mean Connectivity") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5))
    plot_grid(p1, p2)
  })
  output$WGCNA1_fig_download <- downloadHandler(
    filename = function() {
      paste0("SFT_threshold.", input$WGCNA1_fig_format)
    },
    content = function(file) {
      if (input$WGCNA1_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$WGCNA1_fig_width, 
          height = 10 * input$WGCNA1_fig_height, 
          res = 300)
      } else if (input$WGCNA1_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$WGCNA1_fig_width/20, 
          height = input$WGCNA1_fig_height/20, 
          onefile = F)
      } else if (input$WGCNA1_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$WGCNA1_fig_width, 
          height = 10 * input$WGCNA1_fig_height, 
          res = 300)
      } else if (input$WGCNA1_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$WGCNA1_fig_width, 
          height = 10 * input$WGCNA1_fig_height, 
          res = 300)
      } else if (input$WGCNA1_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$WGCNA1_fig_width, 
          height = 10 * input$WGCNA1_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$WGCNA1_fig_width/20, 
          height = input$WGCNA1_fig_height/20)
      }
      y_line <- as.numeric(input$SFT)
      p1 <- ggplot(data = WGCNA1_sft_data()$fitIndices, aes(x = Power, y = SFT.R.sq)) +
        geom_point(color = "red") +
        geom_text_repel(aes(label = Power)) +
        geom_hline(aes(yintercept = y_line),
                   color = "red") +
        labs(title = "Scale independence",
             x = "Soft Threshold (power)",
             y = "Scale Free Topology Model Fit,signed R^2") +
        theme_few() +
        theme(plot.title = element_text(hjust = 0.5)) +
        annotate("text",
                 x = 0,
                 y = y_line,
                 label = y_line,
                 vjust = -1,
                 color = "red")
      p2 <- ggplot(data = WGCNA1_sft_data()$fitIndices, aes(x = Power, y = mean.k.)) +
        geom_point(color = "red") +
        geom_text_repel(aes(label = Power)) +
        labs(title = "Mean connectivity",
             x = "Soft Threshold (power)",
             y = "Mean Connectivity") +
        theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
      p <- plot_grid(p1, p2)
      plot(p)
      dev.off()
      if (input$WGCNA1_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          y_line <- as.numeric(input$SFT)
          p1 <- ggplot(data = WGCNA1_sft_data()$fitIndices, aes(x = Power, y = SFT.R.sq)) +
            geom_point(color = "red") +
            geom_text_repel(aes(label = Power)) +
            geom_hline(aes(yintercept = y_line),
                       color = "red") +
            labs(title = "Scale independence",
                 x = "Soft Threshold (power)",
                 y = "Scale Free Topology Model Fit,signed R^2") +
            theme_few() +
            theme(plot.title = element_text(hjust = 0.5)) +
            annotate("text",
                     x = 0,
                     y = y_line,
                     label = y_line,
                     vjust = -1,
                     color = "red")
          p2 <- ggplot(data = WGCNA1_sft_data()$fitIndices, aes(x = Power, y = mean.k.)) +
            geom_point(color = "red") +
            geom_text_repel(aes(label = Power)) +
            labs(title = "Mean connectivity",
                 x = "Soft Threshold (power)",
                 y = "Mean Connectivity") +
            theme_few() +
            theme(plot.title = element_text(hjust = 0.5))
          p <- plot_grid(p1, p2)
          plot(p)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$WGCNA1_reset, {
    updateMaterialSwitch(
      session = session,
      inputId = "Sample_Gene_filter",
      value = FALSE
    )
    updateNumericInput(
      session = session,
      inputId = "SFT",
      value = 0.85
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA1_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA1_fig_width",
      value = 1000
    )
  })
  ########################WGCNA2########################
  observeEvent(input$WGCNA2_submit, {
    output$Dendroandcolors_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("Dendroandcolors_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("Dendroandcolors_fig_format"),
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
            outputId = ns("Dendroandcolors_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$Dendroandcolors_ui <- renderUI({
    if (is.na(input$WGCNA2_fig_height) | is.na(input$WGCNA2_fig_width) | input$WGCNA2_fig_height * input$WGCNA2_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("Dendroandcolors_plot"),
          height = paste0(input$WGCNA2_fig_height, "px"),
          width = paste0(input$WGCNA2_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  observeEvent(input$WGCNA2_submit, {
    output$WGCNA2_gene2module_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("WGCNA2_gene2module_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("WGCNA2_gene2module_format"),
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
            outputId = ns("WGCNA2_gene2module_download"),
            label = "Download",
            icon = icon("download")
          )
        ),
        column(
          width = 12,
          h4("Gene→Module")
        )
      )
    )
  })
  observeEvent(input$WGCNA2_submit, {
    output$WGCNA2_kME_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("WGCNA2_kME_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("WGCNA2_kME_format"),
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
            outputId = ns("WGCNA2_kME_download"),
            label = "Download",
            icon = icon("download")
          )
        ),
        column(
          width = 12,
          h4("Gene→kME")
        )
      )
    )
  })
  WGCNA_net <- eventReactive(input$WGCNA2_submit, {
    data <- blockwiseModules(
      t(WGCNA_expr_filter()),
      power = input$Power_threshold,
      corType = "pearson",
      TOMType = "unsigned",
      networkType = "unsigned",
      minModuleSize = input$minModuleSize,
      maxBlockSize = input$maxBlockSize,
      mergeCutHeight = input$mergeCutHeight,
      numericLabels = FALSE,
      deepSplit = input$deepSplit,
      pamRespectsDendro = FALSE,
      saveTOMs = FALSE
    )
    return(data)
  })
  output$Dendroandcolors_plot <- renderPlot({
    plotDendroAndColors(
      dendro = WGCNA_net()$dendrograms[[1]], 
      colors = labels2colors(WGCNA_net()$colors)[WGCNA_net()$blockGenes[[1]]],
      groupLabels = "Module colors",
      dendroLabels = FALSE, 
      hang = 0.03,
      addGuide = TRUE, 
      guideHang = 0.05)
  })
  output$Dendroandcolors_fig_download <- downloadHandler(
    filename = function() {
      paste0("DendroAndColors.", input$Dendroandcolors_fig_format)
    },
    content = function(file) {
      if (input$Dendroandcolors_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$WGCNA2_fig_width, 
          height = 10 * input$WGCNA2_fig_height, 
          res = 300)
      } else if (input$Dendroandcolors_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$WGCNA2_fig_width/20, 
          height = input$WGCNA2_fig_height/20, 
          onefile = F)
      } else if (input$Dendroandcolors_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$WGCNA2_fig_width, 
          height = 10 * input$WGCNA2_fig_height, 
          res = 300)
      } else if (input$Dendroandcolors_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$WGCNA2_fig_width, 
          height = 10 * input$WGCNA2_fig_height, 
          res = 300)
      } else if (input$Dendroandcolors_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$WGCNA2_fig_width, 
          height = 10 * input$WGCNA2_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$WGCNA2_fig_width/20, 
          height = input$WGCNA2_fig_height/20)
      }
      plotDendroAndColors(
        dendro = WGCNA_net()$dendrograms[[1]], 
        colors = labels2colors(WGCNA_net()$colors)[WGCNA_net()$blockGenes[[1]]],
        groupLabels = "Module colors",
        dendroLabels = FALSE, 
        hang = 0.03,
        addGuide = TRUE, 
        guideHang = 0.05)
      dev.off()
      if (input$Dendroandcolors_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          plotDendroAndColors(
            dendro = WGCNA_net()$dendrograms[[1]], 
            colors = labels2colors(WGCNA_net()$colors)[WGCNA_net()$blockGenes[[1]]],
            groupLabels = "Module colors",
            dendroLabels = FALSE, 
            hang = 0.03,
            addGuide = TRUE, 
            guideHang = 0.05)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  WGCNA_result <- eventReactive(input$WGCNA2_submit, {
    data <- data.frame(
      Gene = rownames(data.frame(WGCNA_net()$colors)),
      module = data.frame(WGCNA_net()$colors)[,1]
    )
    return(data)
  })
  WGCNA_kME <- eventReactive(input$WGCNA2_submit, {
    data <- signedKME(
      t(WGCNA_expr_filter()),
      orderMEs(moduleEigengenes(t(WGCNA_expr_filter()),labels2colors(WGCNA_net()$colors))$eigengenes)
    )
    return(data)
  })
  output$WGCNA2_gene2module <- renderDT({
    datatable(
      WGCNA_result(),
      rownames = 1,
      filter = "bottom",
      extensions = "FixedColumns",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$WGCNA2_kME <- renderDT({
    datatable(
      WGCNA_kME(),
      rownames = TRUE,
      filter = "bottom",
      extensions = "FixedColumns",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$WGCNA2_gene2module_download <- downloadHandler(
    filename = function() {
      paste0("Gene_Module.", input$WGCNA2_gene2module_format)
    },
    content = function(file) {
      WGCNA_result <- WGCNA_result()
      if (input$WGCNA2_gene2module_format == "txt") {
        write.table(WGCNA_result, file, row.names = F, col.names = T, quote = F)
      } else if (input$WGCNA2_gene2module_format == "csv") {
        readr::write_csv(WGCNA_result, file, col_names = T)
      } else if (input$WGCNA2_gene2module_format == "tsv") {
        readr::write_tsv(WGCNA_result, file, col_names = T)
      } else if (input$WGCNA2_gene2module_format == "xlsx") {
        writexl::write_xlsx(WGCNA_result, file, col_names = T)
      }
    }
  )
  output$WGCNA2_kME_download <- downloadHandler(
    filename = function() {
      paste0("Gene_kME.", input$WGCNA2_kME_format)
    },
    content = function(file) {
      WGCNA_kME <- data.frame(" " = rownames(WGCNA_kME()),
                              WGCNA_kME(),
                              row.names = NULL,
                              check.names = F)
      if (input$WGCNA2_kME_format == "txt") {
        write.table(WGCNA_kME, file, row.names = F, col.names = T, quote = F)
      } else if (input$WGCNA2_kME_format == "csv") {
        readr::write_csv(WGCNA_kME, file, col_names = T)
      } else if (input$WGCNA2_kME_format == "tsv") {
        readr::write_tsv(WGCNA_kME, file, col_names = T)
      } else if (input$WGCNA2_kME_format == "xlsx") {
        writexl::write_xlsx(WGCNA_kME, file, col_names = T)
      }
    }
  )
  observeEvent(input$WGCNA2_reset, {
    updateNumericInput(
      session = session,
      inputId = "minModuleSize",
      value = 30
    )
    updateNumericInput(
      session = session,
      inputId = "maxBlockSize",
      value = 5000
    )
    updateNumericInput(
      session = session,
      inputId = "mergeCutHeight",
      value = 0.25
    )
    updateNumericInput(
      session = session,
      inputId = "deepSplit",
      value = 2
    )
  })
  ########################WGCNA3########################
  observeEvent(input$WGCNA3_submit, {
    output$labeledHeatmap_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("WGCNA3_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("WGCNA3_fig_format"),
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
            outputId = ns("WGCNA3_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$labeledHeatmap_ui <- renderUI({
    if (is.na(input$WGCNA3_fig_height) | is.na(input$WGCNA3_fig_width) | input$WGCNA3_fig_height * input$WGCNA3_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("WGCNA3_plot"),
          height = paste0(input$WGCNA3_fig_height, "px"),
          width = paste0(input$WGCNA3_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  output$Sample_info_layout <- renderUI({
    if (input$Sample_info_upload == "TRUE") {
      fileInput(
        inputId = ns("Sample_info_file"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Upload your trait data</b></font>'),
                   bsplus::bs_button(
                     label = "?", 
                     button_type = "primary", 
                     button_size = "extra-small") %>%
                     bsplus::bs_embed_tooltip(title = "Note：The row name is the name of the sample, the column name is the name of the trait, and the values are continuous data.")),
        accept = ".txt",
        buttonLabel = "Browse...",
        placeholder = "No file selected",
        capture = NULL
      )
    }
  })
  Sample_info <- eventReactive(input$WGCNA3_submit, {
    if (input$Sample_info_upload == "TRUE") {
      sample_info <- reactive({
        req(input$Sample_info_file)
        data <- read.delim(
          file = input$Sample_info_file$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    } else {
      sample_info <- reactive({
        data <- diag(ncol(WGCNA_expr_filter()))
        colnames(data) <- colnames(WGCNA_expr_filter())
        sample_info <- data.frame(
          data,
          row.names = colnames(WGCNA_expr_filter())
        )
        return(sample_info)
      })
    }
    return(sample_info())
  })
  nGenes <- eventReactive(input$WGCNA3_submit, {
    data <- nrow(WGCNA_expr_filter())
    return(data)
  })
  nSamples <- eventReactive(input$WGCNA3_submit, {
    data <- ncol(WGCNA_expr_filter())
    return(data)
  })
  moduleTraitCor <- eventReactive(input$WGCNA3_submit, {
    data <- cor(
      WGCNA_net()$MEs,
      Sample_info(),
      use = "p",
      method = "spearman")
    return(data)
  })
  moduleTraitPvalue <- eventReactive(input$WGCNA3_submit, {
    data <- corPvalueStudent(
      moduleTraitCor(),
      nSamples()
    )
    return(data)
  })
  textMatrix <- eventReactive(input$WGCNA3_submit, {
    data <- paste0(
      signif(moduleTraitCor(), 2),
      "\n(",
      signif(moduleTraitPvalue(), 1), 
      ")",
      sep = ""
    )
    dim(data) <- dim(moduleTraitCor())
    return(data)
  })
  output$WGCNA3_plot <- renderPlot({
    req(input$WGCNA3_submit)
    WGCNA::labeledHeatmap.multiPage(
      Matrix = moduleTraitCor(),
      xLabels = names(Sample_info()),
      yLabels = names(orderMEs(WGCNA_net()$MEs)),
      ySymbols = names(orderMEs(WGCNA_net()$MEs)),
      colorLabels = FALSE,
      colors = blueWhiteRed(50),
      textMatrix = textMatrix(),
      setStdMargins = FALSE,
      zlim = c(-1, 1),
      cex.lab.x = input$X_size,
      cex.lab.y = input$Y_size,
      cex.text = input$Content_size,
      cex.lab = input$Legend_size,
      main = paste0("Module-trait relationships"),
      maxRowsPerPage = 10000000000000,
      maxColsPerPage = 10000000000000,
      addPageNumberToMain = TRUE
    )
  })
  output$WGCNA3_fig_download <- downloadHandler(
    filename = function() {
      paste0("labeledHeatmap.", input$WGCNA3_fig_format)
    },
    content = function(file) {
      if (input$WGCNA3_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$WGCNA3_fig_width, 
          height = 10 * input$WGCNA3_fig_height, 
          res = 300)
      } else if (input$WGCNA3_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$WGCNA3_fig_width/20, 
          height = input$WGCNA3_fig_height/20, 
          onefile = F)
      } else if (input$WGCNA3_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$WGCNA3_fig_width, 
          height = 10 * input$WGCNA3_fig_height, 
          res = 300)
      } else if (input$WGCNA3_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$WGCNA3_fig_width, 
          height = 10 * input$WGCNA3_fig_height, 
          res = 300)
      } else if (input$WGCNA3_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$WGCNA3_fig_width, 
          height = 10 * input$WGCNA3_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$WGCNA3_fig_width/20, 
          height = input$WGCNA3_fig_height/20)
      }
      WGCNA::labeledHeatmap.multiPage(
        Matrix = moduleTraitCor(),
        xLabels = names(Sample_info()),
        yLabels = names(orderMEs(WGCNA_net()$MEs)),
        ySymbols = names(orderMEs(WGCNA_net()$MEs)),
        colorLabels = FALSE,
        colors = blueWhiteRed(50),
        textMatrix = textMatrix(),
        setStdMargins = FALSE,
        zlim = c(-1, 1),
        cex.lab.x = input$X_size,
        cex.lab.y = input$Y_size,
        cex.text = input$Content_size,
        cex.lab = input$Legend_size,
        main = paste0("Module-trait relationships"),
        maxRowsPerPage = 10000000000000,
        maxColsPerPage = 10000000000000,
        addPageNumberToMain = TRUE
      )
      dev.off()
      if (input$WGCNA3_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          WGCNA::labeledHeatmap.multiPage(
            Matrix = moduleTraitCor(),
            xLabels = names(Sample_info()),
            yLabels = names(orderMEs(WGCNA_net()$MEs)),
            ySymbols = names(orderMEs(WGCNA_net()$MEs)),
            colorLabels = FALSE,
            colors = blueWhiteRed(50),
            textMatrix = textMatrix(),
            setStdMargins = FALSE,
            zlim = c(-1, 1),
            cex.lab.x = input$X_size,
            cex.lab.y = input$Y_size,
            cex.text = input$Content_size,
            cex.lab = input$Legend_size,
            main = paste0("Module-trait relationships"),
            maxRowsPerPage = 10000000000000,
            maxColsPerPage = 10000000000000,
            addPageNumberToMain = TRUE
          )
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$WGCNA3_reset, {
    updateNumericInput(
      session = session,
      inputId = "X_size",
      value = 1
    )
    updateNumericInput(
      session = session,
      inputId = "Y_size",
      value = 1
    )
    updateNumericInput(
      session = session,
      inputId = "Content_size",
      value = 1
    )
    updateNumericInput(
      session = session,
      inputId = "Legend_size",
      value = 1
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA3_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA3_fig_width",
      value = 1000
    )
  })
  ########################WGCNA4########################
  observeEvent(input$WGCNA3_submit, {
    updateSelectInput(
      session = session,
      inputId = "Module",
      choices = modNames()
    )
    updateSelectInput(
      session = session,
      inputId = "Sample",
      choices = colnames(Sample_info())
    )
  })
  output$MM_threshold_layout <- renderUI({
    if (input$Hub == "TRUE") {
      numericInput(
        inputId = ns("MM_threshold"),
        label = "Module membership threshold(absolute value)",
        value = 0.8
      )
    } else {
      NULL
    }
  })
  output$GS_threshold_layout <- renderUI({
    if (input$Hub == "TRUE") {
      numericInput(
        inputId = ns("GS_threshold"),
        label = "Gene significance threshold(absolute value)",
        value = 0.2
      )
    } else {
      NULL
    }
  })
  observeEvent(input$WGCNA4_submit, {
    output$MM_GS_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MM_GS_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MM_GS_tab_format"),
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
            outputId = ns("MM_GS_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  observeEvent(input$WGCNA4_submit, {
    output$MM_GS_plot_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MM_GS_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MM_GS_fig_format"),
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
            outputId = ns("MM_GS_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$MM_GS_plot_ui <- renderUI({
    if (is.na(input$WGCNA4_fig_height) | is.na(input$WGCNA4_fig_width) | input$WGCNA4_fig_height * input$WGCNA4_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("MM_GS_plot"),
          height = paste0(input$WGCNA4_fig_height, "px"),
          width = paste0(input$WGCNA4_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  modNames <- eventReactive(input$WGCNA3_submit, {
    data <- substring(names(orderMEs(moduleEigengenes(t(WGCNA_expr_filter()),
                                                      labels2colors(WGCNA_net()$colors))$eigengenes)), 
                      3)
    return(data)
  })
  my_sample <- eventReactive(input$WGCNA4_submit, {
    data <- dplyr::select(Sample_info(), input$Sample) %>%
      data.frame(row.names = NULL)
    return(data)
  })
  moduleGenes <- eventReactive(input$WGCNA4_submit, {
    module <- input$Module
    data <- labels2colors(WGCNA_net()$colors)==module
    return(data)
  })
  my_column <- eventReactive(input$WGCNA4_submit, {
    module <- input$Module
    data <- match(module, modNames())
  })
  my_module <- eventReactive(input$WGCNA4_submit, {
    data <- as.data.frame(dimnames(data.frame(t(WGCNA_expr_filter())))[[2]][moduleGenes()])
    names(data) <- "Gene"
    return(data)
  })
  MM <- eventReactive(input$WGCNA4_submit, {
    data <- as.data.frame(cor(t(WGCNA_expr_filter()),
                              orderMEs(moduleEigengenes(t(WGCNA_expr_filter()),
                                                        labels2colors(WGCNA_net()$colors))$eigengenes), 
                              use = "p"))
    names(data) <- paste("MM", modNames(), sep="")
    MM <- abs(data[moduleGenes(), my_column()])
    return(MM)
  })
  GS <- eventReactive(input$WGCNA4_submit, {
    data <- as.data.frame(cor(t(WGCNA_expr_filter()), 
                              my_sample(), 
                              use = "p"))
    names(data) <- paste("GS.", names(my_sample()), sep="")
    GS <- abs(data[moduleGenes(), 1])
    return(GS)
  })
  MM_GS <- eventReactive(input$WGCNA4_submit, {
    data <- as.data.frame(cbind(MM(), GS()))
    colnames(data) <- c("MM", "GS")
    rownames(data) <- my_module()$Gene
    if (input$Hub == "TRUE") {
      data <- dplyr::mutate(
        data,
        Hub = if_else(abs(MM) > input$MM_threshold & abs(GS) > input$GS_threshold, "TRUE", "FALSE"))
    } else {
      data <- data
    }
    return(data)
  })
  MM_GS_cor <- eventReactive(input$WGCNA4_submit, {
    data <- round(cor(MM_GS()$MM, MM_GS()$GS), 2)
    return(data)
  })
  MM_GS_p <- eventReactive(input$WGCNA4_submit, {
    p <- cor.test(MM_GS()$MM, MM_GS()$GS)
    data <- round(p$p.value, 5)
    return(data)
  })
  x_threshold <- eventReactive(input$WGCNA4_submit, {
    data <- input$MM_threshold
    return(data)
  })
  y_threshold <- eventReactive(input$WGCNA4_submit, {
    data <- input$GS_threshold
    return(data)
  })
  module_plot <- eventReactive(input$WGCNA4_submit, {
    data <- input$Module
    return(data)
  })
  sample_plot <- eventReactive(input$WGCNA4_submit, {
    data <- input$Sample
    return(data)
  })
  output$MM_GS_tab <- renderDT({
    datatable(
      MM_GS(),
      rownames = TRUE,
      extensions = "FixedColumns",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$MM_GS_tab_download <- downloadHandler(
    filename = function() {
      paste0(input$Module, "&", input$Sample, "_MM_vs_GS.", input$MM_GS_tab_format)
    },
    content = function(file) {
      MM_GS <- data.frame(" " = rownames(MM_GS()),
                          MM_GS(),
                          row.names = NULL,
                          check.names = F)
      if (input$MM_GS_tab_format == "txt") {
        write.table(MM_GS, file, row.names = F, col.names = T, quote = F)
      } else if (input$MM_GS_tab_format == "csv") {
        readr::write_csv(MM_GS, file, col_names = T)
      } else if (input$MM_GS_tab_format == "tsv") {
        readr::write_tsv(MM_GS, file, col_names = T)
      } else if (input$MM_GS_tab_format == "xlsx") {
        writexl::write_xlsx(MM_GS, file, col_names = T)
      }
    }
  )
  output$MM_GS_plot <- renderPlot({
    if (input$Hub == "TRUE") {
      ggplot(
        data = MM_GS(),
        aes(
          x = MM,
          y = GS,
          color = Hub)) +
        geom_point(
          size = 4,
          alpha = 0.9) +
        scale_colour_manual(values=c("grey60", module_plot())) +
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+  
        labs(x= paste("Module membership in ", module_plot(), "module"), 
             y= paste("Gene significance for ", sample_plot()),
             title = paste("Module membership vs. gene significance\n","cor=", MM_GS_cor(), "p=", MM_GS_p())) +
        theme(axis.title.x = element_text(size=14), 
              axis.title.y = element_text(size=14),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(colour = "black"),
              axis.text.y = element_text(colour = "black"),
              plot.title = element_text(hjust = 0.5,
                                        size = 16,
                                        face = "bold"),
              plot.margin = unit(rep(2,4),'lines')) +
        theme(legend.position = 'none') +
        geom_hline(aes(yintercept = y_threshold()),
                   colour="#5B9BD5",
                   lwd=1,
                   linetype=5)+
        geom_vline(aes(xintercept = x_threshold()),
                   colour="#5B9BD5",
                   lwd=1,
                   linetype=5) +
        geom_smooth(method = "lm",
                    formula = y~x, 
                    color = "#756bb1", 
                    fill = "#cbc9e2")
    } else {
      ggplot(
        data = MM_GS(),
        aes(
          x = MM,
          y = GS,
          color = module_plot())) +
        geom_point(
          size = 4,
          alpha = 0.9) +
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+  
        labs(x= paste("Module membership in ", module_plot(), "module"), 
             y= paste("Gene significance for ", sample_plot()),
             title = paste("Module membership vs. gene significance\n","cor=", MM_GS_cor(), "p=", MM_GS_p())) +
        theme(axis.title.x = element_text(size=14), 
              axis.title.y = element_text(size=14),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(colour = "black"),
              axis.text.y = element_text(colour = "black"),
              plot.title = element_text(hjust = 0.5,
                                        size = 16,
                                        face = "bold"),
              plot.margin = unit(rep(2,4),'lines')) +
        theme(legend.position = 'none') +
        geom_smooth(method = "lm",
                    formula = y~x, 
                    color = "#756bb1", 
                    fill = "#cbc9e2")
    }
  })
  output$MM_GS_fig_download <- downloadHandler(
    filename = function() {
      paste0(input$Module, "&", input$Sample, "_MM_vs_GS.", input$MM_GS_fig_format)
    },
    content = function(file) {
      if (input$MM_GS_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$WGCNA4_fig_width, 
          height = 10 * input$WGCNA4_fig_height, 
          res = 300)
      } else if (input$MM_GS_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$WGCNA4_fig_width/20, 
          height = input$WGCNA4_fig_height/20, 
          onefile = F)
      } else if (input$MM_GS_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$WGCNA4_fig_width, 
          height = 10 * input$WGCNA4_fig_height, 
          res = 300)
      } else if (input$MM_GS_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$WGCNA4_fig_width, 
          height = 10 * input$WGCNA4_fig_height, 
          res = 300)
      } else if (input$MM_GS_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$WGCNA4_fig_width, 
          height = 10 * input$WGCNA4_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$WGCNA4_fig_width/20, 
          height = input$WGCNA4_fig_height/20)
      }
      if (input$Hub == "TRUE") {
        p <- ggplot(
          data = MM_GS(),
          aes(
            x = MM,
            y = GS,
            color = Hub)) +
          geom_point(
            size = 4,
            alpha = 0.9) +
          scale_colour_manual(values=c("grey60", module_plot())) +
          theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())+  
          labs(x= paste("Module membership in ", module_plot(), "module"), 
               y= paste("Gene significance for ", sample_plot()),
               title = paste("Module membership vs. gene significance\n","cor=", MM_GS_cor(), "p=", MM_GS_p())) +
          theme(axis.title.x = element_text(size=14), 
                axis.title.y = element_text(size=14),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(colour = "black"),
                axis.text.y = element_text(colour = "black"),
                plot.title = element_text(hjust = 0.5,
                                          size = 16,
                                          face = "bold"),
                plot.margin = unit(rep(2,4),'lines')) +
          theme(legend.position = 'none') +
          geom_hline(aes(yintercept = y_threshold()),
                     colour="#5B9BD5",
                     lwd=1,
                     linetype=5)+
          geom_vline(aes(xintercept = x_threshold()),
                     colour="#5B9BD5",
                     lwd=1,
                     linetype=5) +
          geom_smooth(method = "lm",
                      formula = y~x, 
                      color = "#756bb1", 
                      fill = "#cbc9e2")
      } else {
        p <- ggplot(
          data = MM_GS(),
          aes(
            x = MM,
            y = GS,
            color = module_plot())) +
          geom_point(
            size = 4,
            alpha = 0.9) +
          theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())+  
          labs(x= paste("Module membership in ", module_plot(), "module"), 
               y= paste("Gene significance for ", sample_plot()),
               title = paste("Module membership vs. gene significance\n","cor=", MM_GS_cor(), "p=", MM_GS_p())) +
          theme(axis.title.x = element_text(size=14), 
                axis.title.y = element_text(size=14),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(colour = "black"),
                axis.text.y = element_text(colour = "black"),
                plot.title = element_text(hjust = 0.5,
                                          size = 16,
                                          face = "bold"),
                plot.margin = unit(rep(2,4),'lines')) +
          theme(legend.position = 'none') +
          geom_smooth(method = "lm",
                      formula = y~x, 
                      color = "#756bb1", 
                      fill = "#cbc9e2")
      }
      plot(p)
      dev.off()
      if (input$MM_GS_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          if (input$Hub == "TRUE") {
            p <- ggplot(
              data = MM_GS(),
              aes(
                x = MM,
                y = GS,
                color = Hub)) +
              geom_point(
                size = 4,
                alpha = 0.9) +
              scale_colour_manual(values=c("grey60", module_plot())) +
              theme_bw() + 
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())+  
              labs(x= paste("Module membership in ", module_plot(), "module"), 
                   y= paste("Gene significance for ", sample_plot()),
                   title = paste("Module membership vs. gene significance\n","cor=", MM_GS_cor(), "p=", MM_GS_p())) +
              theme(axis.title.x = element_text(size=14), 
                    axis.title.y = element_text(size=14),
                    axis.text = element_text(size = 12),
                    axis.text.x = element_text(colour = "black"),
                    axis.text.y = element_text(colour = "black"),
                    plot.title = element_text(hjust = 0.5,
                                              size = 16,
                                              face = "bold"),
                    plot.margin = unit(rep(2,4),'lines')) +
              theme(legend.position = 'none') +
              geom_hline(aes(yintercept = y_threshold()),
                         colour="#5B9BD5",
                         lwd=1,
                         linetype=5)+
              geom_vline(aes(xintercept = x_threshold()),
                         colour="#5B9BD5",
                         lwd=1,
                         linetype=5) +
              geom_smooth(method = "lm",
                          formula = y~x, 
                          color = "#756bb1", 
                          fill = "#cbc9e2")
          } else {
            p <- ggplot(
              data = MM_GS(),
              aes(
                x = MM,
                y = GS,
                color = module_plot())) +
              geom_point(
                size = 4,
                alpha = 0.9) +
              theme_bw() + 
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())+  
              labs(x= paste("Module membership in ", module_plot(), "module"), 
                   y= paste("Gene significance for ", sample_plot()),
                   title = paste("Module membership vs. gene significance\n","cor=", MM_GS_cor(), "p=", MM_GS_p())) +
              theme(axis.title.x = element_text(size=14), 
                    axis.title.y = element_text(size=14),
                    axis.text = element_text(size = 12),
                    axis.text.x = element_text(colour = "black"),
                    axis.text.y = element_text(colour = "black"),
                    plot.title = element_text(hjust = 0.5,
                                              size = 16,
                                              face = "bold"),
                    plot.margin = unit(rep(2,4),'lines')) +
              theme(legend.position = 'none') +
              geom_smooth(method = "lm",
                          formula = y~x, 
                          color = "#756bb1", 
                          fill = "#cbc9e2")
          }
          plot(p)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$WGCNA4_reset, {
    updateMaterialSwitch(
      session = session,
      inputId = "Hub",
      value = TRUE
    )
    updateNumericInput(
      session = session,
      inputId = "MM_threshold",
      value = 0.8
    )
    updateNumericInput(
      session = session,
      inputId = "GS_threshold",
      value = 0.2
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA4_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA4_fig_width",
      value = 1000
    )
  })
  ########################WGCNA5########################
  observeEvent(input$WGCNA3_submit, {
    updateSelectInput(
      session = session,
      inputId = "Module_net",
      choices = modNames()
    )
  })
  observeEvent(input$WGCNA5_submit, {
    output$WGCNA5_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("WGCNA5_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("WGCNA5_tab_format"),
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
            outputId = ns("WGCNA5_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$WGCNA5_ui <- renderUI({
    if (is.na(input$WGCNA5_fig_height) | is.na(input$WGCNA5_fig_width) | input$WGCNA5_fig_height * input$WGCNA5_fig_width == 0) {
      NULL
    } else {
      visNetworkOutput(
        outputId = ns("WGCNA5_plot"),
        height = paste0(input$WGCNA5_fig_height, "px"),
        width = paste0(input$WGCNA5_fig_width, "px"))
    }
  })
  module_net_data <- eventReactive(input$WGCNA5_submit, {
    wgcna_result <-  data.frame(
      Gene = rownames(data.frame(WGCNA_net()$colors)),
      module = data.frame(WGCNA_net()$colors)[,1]
    )
    wgcna_result_module <- filter(wgcna_result, module %in% input$Module_net)
    expr_module <- t(WGCNA_expr_filter())[, wgcna_result_module$Gene]
    TOM_module <- TOMsimilarityFromExpr(
      expr_module,
      power = input$Power_threshold,
      networkType = "unsigned",
      TOMType = "unsigned"
    )
    dimnames(TOM_module) <- list(colnames(expr_module), colnames(expr_module))
    cyt_module <- exportNetworkToCytoscape(
      TOM_module,
      weighted = TRUE,
      threshold = 0
    )
    data <- (cyt_module$edgeData)[1:3]
    return(data)
  })
  output$WGCNA5_tab <- renderDT({
    datatable(
      module_net_data(),
      rownames = FALSE,
      filter = "bottom",
      extensions = "FixedColumns",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$WGCNA5_tab_download <- downloadHandler(
    filename = function() {
      paste0(input$Module_net, "_net.", input$WGCNA5_tab_format)
    },
    content = function(file) {
      if (input$WGCNA5_tab_format == "txt") {
        write.table(module_net_data(), file, row.names = F, col.names = T, quote = F)
      } else if (input$WGCNA5_tab_format == "csv") {
        readr::write_csv(module_net_data(), file, col_names = T)
      } else if (input$WGCNA5_tab_format == "tsv") {
        readr::write_tsv(module_net_data(), file, col_names = T)
      } else if (input$WGCNA5_tab_format == "xlsx") {
        writexl::write_xlsx(module_net_data(), file, col_names = T)
      }
    }
  )
  output$WGCNA5_plot <- renderVisNetwork({
    module_net_tab <- dplyr::filter(module_net_data(), weight > input$WGCNA5_threshold)
    from <- data.frame(id = module_net_tab$fromNode)
    to <- data.frame(id = module_net_tab$toNode)
    nodes <- rbind(from, to) %>%
      unique()
    nodes <- data.frame(
      id = nodes,
      color = list(border = input$WGCNA5_node_border,
                   background = input$WGCNA5_node_backgroud,
                   highlight = "#FF8884"),
      shape = "dot"
    )
    edges <- data.frame(
      from = module_net_tab$fromNode, 
      to = module_net_tab$toNode,
      value = module_net_tab$weight,
      color = list(color = input$WGCNA5_edge,
                   highlight = "#FF8884")
    )
    visNetwork(
      nodes = nodes,
      edges = edges,
    ) %>% 
      visInteraction(
        navigationButtons = T,
        dragNodes = T,
        dragView = T) %>%
      visOptions(
        manipulation = TRUE,
        highlightNearest = T,
        nodesIdSelection = T) %>%
      visIgraphLayout(
        layout = input$WGCNA5_layout) %>%
      visExport(
        type = c("jpeg"), 
        name = "export-network", 
        float = "right", 
        label = "Save network", 
        background = "white", 
        style= "")
  })
  observeEvent(input$WGCNA5_reset, {
    updateNumericInput(
      session = session,
      inputId = "WGCNA5_threshold",
      value = 0.2
    )
    updateColorPickr(
      session = session,
      inputId = "WGCNA5_node_border",
      value = "#9AC9DB"
    )
    updateColorPickr(
      session = session,
      inputId = "WGCNA5_node_backgroud",
      value = "#2878B5"
    )
    updateColorPickr(
      session = session,
      inputId = "WGCNA5_edge",
      value = "#E7EFFA"
    )
    updateSelectInput(
      session = session,
      inputId = "WGCNA5_layout",
      selected = "layout_nicely"
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA5_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "WGCNA5_fig_width",
      value = 1000
    )
  })
}