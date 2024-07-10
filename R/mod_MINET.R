mod_minet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      HTML('<font size="6" color="red"><b>MINET</b></font>'),
      br(),
      br(),
      fileInput(
        inputId = ns("MINET_expr"),
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
      selectInput(
        inputId = ns("MINET_cor_method"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Correlation calculation method</b></font>')),
        choices = c("kendall", "pearson", "spearman"),
        selected = "pearson"
      ),
      selectInput(
        inputId = ns("MINET_net_method"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Network construction method</b></font>'),
                   bsplus::bs_button(
                     label = "?", 
                     button_type = "primary", 
                     button_size = "extra-small") %>%
                     bsplus::bs_embed_tooltip(title = "Note：aracne:Algorithm for the Reconstruction of Accurate Cellular NEtworks;clr:Context Likelihood or Relatedness Network;mrnet:Maximum Relevance Minimum Redundancy;mrnetb:Maximum Relevance Minimum Redundancy Backward.")
                     ),
        choices = c("aracne", "clr", "mrnet", "mrnetb"),
        selected = "mrnet"
      ),
      numericInput(
        inputId = ns("MINET_threshold"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Weight threshold</b></font>')),
        value = 0.5
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Node settings</b></font>')),
      shinyWidgets::colorPickr(
        inputId = ns("MINET_node_border"),
        label = "Node border",
        position = "right-start",
        selected = "#9AC9DB"
      ),
      shinyWidgets::colorPickr(
        inputId = ns("MINET_node_backgroud"),
        label = "Node backgroud",
        position = "right-start",
        selected = "#2878B5"
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Edge settings</b></font>')),
      shinyWidgets::colorPickr(
        inputId = ns("MINET_edge"),
        label = "Edge",
        position = "right-start",
        selected = "#E7EFFA"
      ),
      selectInput(
        inputId = ns("MINET_layout"),
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
        inputId = ns("MINET_fig_height"), 
        label = "Plot height", 
        value = 800
      ),
      numericInput(
        inputId = ns("MINET_fig_width"), 
        label = "Plot width", 
        value = 1000
      ),
      br(),
      br(),
      shinysky::actionButton(
        inputId = ns("MINET_submit"),
        label = "Submit",
        styleclass = "danger",
        size = "small"
      ),
      shinysky::actionButton(
        inputId = ns("MINET_reset"),
        label = "Reset",
        styleclass = "warning"
      ),
      shinysky::actionButton(
        inputId = ns("MINET_exam"),
        label = "Load example",
        styleclass = "info"
      )
    ),
    mainPanel(
      uiOutput(outputId = ns("MINET_tab_layout")),
      column(
        width = 12,
        shinycustomloader::withLoader(
          dataTableOutput(ns("MINET_tab")),
          type="html", 
          loader= "dnaspin")
      ),
      column(
        width = 12,
          uiOutput(ns("MINET_ui"))
      )
    )
  )
}
mod_minet_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  observeEvent(input$MINET_submit, {
    output$MINET_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MINET_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MINET_tab_format"),
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
            outputId = ns("MINET_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$MINET_ui <- renderUI({
    if (is.na(input$MINET_fig_height) | is.na(input$MINET_fig_width) | input$MINET_fig_height * input$MINET_fig_width == 0) {
      NULL
    } else {
        visNetworkOutput(
          outputId = ns("MINET_plot"),
          height = paste0(input$MINET_fig_height, "px"),
          width = paste0(input$MINET_fig_width, "px"))
    }
  })
  MINET_expr_data <- eventReactive(input$MINET_submit, {
    if (input$MINET_exam > 0 & is.null(input$MINET_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$MINET_expr)
        data <- read.delim(
          file = input$MINET_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    return(expr_data())
  })
  MINET_tab_data <- eventReactive(input$MINET_submit, {
    expr_t <- t(MINET_expr_data())
    rownames(expr_t) <- NULL
    expr_mim <- build.mim(
      dataset = expr_t,
      estimator = input$MINET_cor_method,
      disc = "none",
      nbins = "none"
    )
    if (input$MINET_net_method == "aracne") {
      net_data <- aracne(
        mim = expr_mim,
        eps = 0
      )
    } else if(input$MINET_net_method == "clr") {
      net_data <- clr(
        mim = expr_mim,
        skipDiagonal = 1
      )
    } else if (input$MINET_net_method == "mrnet") {
      net_data <- mrnet(
        mim = expr_mim
      )
    } else if (input$MINET_net_method == "mrnetb") {
      net_data <- mrnetb(
        mim = expr_mim
      )
    }
    net_data[upper.tri(net_data)] = NA
    net_result <- reshape2::melt(net_data, na.rm = T)
    net_result <- dplyr::filter(net_result, value > 0)
    colnames(net_result) <- c("from", "to", "weight")
    return(net_result)
  })
  output$MINET_tab <- renderDT({
    datatable(MINET_tab_data(),
              rownames = FALSE,
              filter = "bottom",
              extensions = "FixedColumns",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  output$MINET_tab_download <- downloadHandler(
    filename = function() {
      paste0("Co-Expression.", input$MINET_tab_format)
    },
    content = function(file) {
      if (input$MINET_tab_format == "txt") {
        write.table(MINET_tab_data(), file, row.names = F, col.names = T, quote = F)
      } else if (input$MINET_tab_format == "csv") {
        readr::write_csv(MINET_tab_data(), file, col_names = T)
      } else if (input$MINET_tab_format == "tsv") {
        readr::write_tsv(MINET_tab_data(), file, col_names = T)
      } else if (input$MINET_tab_format == "xlsx") {
        writexl::write_xlsx(MINET_tab_data(), file, col_names = T)
      }
    }
  )
  output$MINET_plot <- renderVisNetwork({
    MINET_net_tab <- dplyr::filter(MINET_tab_data(), weight > input$MINET_threshold)
    from <- data.frame(id = MINET_net_tab$from)
    to <- data.frame(id = MINET_net_tab$to)
    nodes <- rbind(from, to) %>%
      unique()
    nodes <- data.frame(
      id = nodes,
      color = list(border = input$MINET_node_border,
                   background = input$MINET_node_backgroud,
                   highlight = "#FF8884"),
      shape = "dot"
    )
    edges <- data.frame(
      from = MINET_net_tab$from, 
      to = MINET_net_tab$to,
      value = MINET_net_tab$weight,
      color = list(color = input$MINET_edge,
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
        layout = input$MINET_layout) %>%
      visExport(
        type = c("jpeg"), 
        name = "export-network", 
        float = "right", 
        label = "Save network", 
        background = "white", 
        style= "")
  })
  observeEvent(input$MINET_reset, {
    updateSelectInput(
      session = session,
      inputId = "MINET_cor_method",
      selected = "pearson"
    )
    updateSelectInput(
     session = session,
     inputId = "MINET_net_method",
     selected = "mrnet"
    )
    updateNumericInput(
      session = session,
      inputId = "MINET_threshold",
      value = 0.5
    )
    updateColorPickr(
      session = session,
      inputId = "MINET_node_border",
      value  = "#9AC9DB"
    )
    updateColorPickr(
      session = session,
      inputId = "MINET_node_backgroud",
      value = "#2878B5"
    )
    updateColorPickr(
      session = session,
      inputId = "MINET_edge",
      value = "#E7EFFA"
    )
    updateSelectInput(
      session = session,
      inputId = "MINET_layout",
      selected = "layout_nicely"
    )
    updateNumericInput(
      session = session,
      inputId = "MINET_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "MINET_fig_width",
      value = 1000
    )
  })
}