mod_genie3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      HTML('<font size="6" color="red"><b>GENIE3</b></font>'),
      br(),
      br(),
      fileInput(
        inputId = ns("GENIE3_expr"),
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
        inputId = ns("GENIE3_regulators"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Add regulators</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      uiOutput(
        outputId = ns("GENIE3_regulators_ui")
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("GENIE3_targets"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Add targets</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      uiOutput(
        outputId = ns("GENIE3_targets_ui")
      ),
      selectInput(
        inputId = ns("GENIE3_tree"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Tree-based method used</b></font>'),
                   bsplus::bs_button(
                     label = "?", 
                     button_type = "primary", 
                     button_size = "extra-small") %>%
                     bsplus::bs_embed_tooltip(title = "RF：Random Forests;ET：Extra-Trees")),
        choices = c("RF", "ET"),
        selected = "RF"
      ),
      numericInput(
        inputId = ns("GENIE3_threshold"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Weight threshold</b></font>')),
        value = 0.2
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Node settings</b></font>')),
      shinyWidgets::colorPickr(
        inputId = ns("GENIE3_node_border"),
        label = "Node border",
        position = "right-start",
        selected = "#9AC9DB"
      ),
      shinyWidgets::colorPickr(
        inputId = ns("GENIE3_node_backgroud"),
        label = "Node backgroud",
        position = "right-start",
        selected = "#2878B5"
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Edge settings</b></font>')),
      shinyWidgets::colorPickr(
        inputId = ns("GENIE3_edge"),
        label = "Edge",
        position = "right-start",
        selected = "#E7EFFA"
      ),
      selectInput(
        inputId = ns("GENIE3_layout"),
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
        inputId = ns("GENIE3_fig_height"), 
        label = "Plot height", 
        value = 800
      ),
      numericInput(
        inputId = ns("GENIE3_fig_width"), 
        label = "Plot width", 
        value = 1000
      ),
      br(),
      br(),
      shinysky::actionButton(
        inputId = ns("GENIE3_submit"),
        label = "Submit",
        styleclass = "danger",
        size = "small"
      ),
      shinysky::actionButton(
        inputId = ns("GENIE3_reset"),
        label = "Reset",
        styleclass = "warning"
      ),
      shinysky::actionButton(
        inputId = ns("GENIE3_exam"),
        label = "Load example",
        styleclass = "info"
      )
    ),
    mainPanel(
      uiOutput(outputId = ns("GENIE3_tab_layout")),
      column(
        width = 12,
        shinycustomloader::withLoader(
          dataTableOutput(ns("GENIE3_tab")),
          type="html", 
          loader= "dnaspin")
      ),
      column(
        width = 12,
        uiOutput(ns("GENIE3_ui"))
      )
    )
  )
}
mod_genie3_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  observeEvent(input$GENIE3_submit, {
    output$GENIE3_tab_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("GENIE3_tab_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("GENIE3_tab_format"),
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
            outputId = ns("GENIE3_tab_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$GENIE3_ui <- renderUI({
    if (is.na(input$GENIE3_fig_height) | is.na(input$GENIE3_fig_width) | input$GENIE3_fig_height * input$GENIE3_fig_width == 0) {
      NULL
    } else {
      visNetworkOutput(
        outputId = ns("GENIE3_plot"),
        height = paste0(input$GENIE3_fig_height, "px"),
        width = paste0(input$GENIE3_fig_width, "px"))
    }
  })
  output$GENIE3_regulators_ui <- renderUI({
    if (input$GENIE3_regulators == TRUE) {
      textAreaInput(
        inputId = ns("GENIE3_regulators_list"),
        label = "Gene list",
        resize = "vertical", 
        height='200px', 
        width = '200%',
        placeholder = "One Gene ID in one row(At least two genes!).")
    } else {
      NULL
    }
  })
  output$GENIE3_targets_ui <- renderUI({
    if (input$GENIE3_targets == TRUE) {
      textAreaInput(
        inputId = ns("GENIE3_targets_list"),
        label = "Gene list",
        resize = "vertical", 
        height='200px', 
        width = '200%',
        placeholder = "One Gene ID in one row(At least two genes!).")
    } else {
      NULL
    }
  })
  GENIE3_expr_data <- eventReactive(input$GENIE3_submit, {
    if (input$GENIE3_exam > 0 & is.null(input$GENIE3_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$GENIE3_expr)
        data <- read.delim(
          file = input$GENIE3_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    return(expr_data())
  })
  GENIE3_regulators_gene <- eventReactive(input$GENIE3_submit, {
    data <- strsplit(input$GENIE3_regulators_list, "\n")[[1]]
    data1 <-  data[data != ""]
    return(data1)
  })
  GENIE3_targets_gene <- eventReactive(input$GENIE3_submit, {
    data <- strsplit(input$GENIE3_targets_list, "\n")[[1]]
    data1 <-  data[data != ""]
    return(data1)
  })
  GENIE3_tab_data <- eventReactive(input$GENIE3_submit, {
    ncore <- detectCores(all.tests = FALSE, logical = TRUE)
    if (input$GENIE3_regulators == FALSE & input$GENIE3_targets == FALSE) {
      data <- GENIE3(
        as.matrix(GENIE3_expr_data()),
        nCores = ncore,
        nTrees = 1000,
        regulators = NULL,
        targets = NULL,
        treeMethod = input$GENIE3_tree,
        returnMatrix = TRUE,
        verbose = FALSE
      ) %>%
        getLinkList()
    } else if (input$GENIE3_regulators == TRUE & input$GENIE3_targets == TRUE) {
      regulators <- GENIE3_regulators_gene()
      targets <- GENIE3_targets_gene()
      data <- GENIE3(
        as.matrix(GENIE3_expr_data()),
        nCores = ncore,
        nTrees = 1000,
        regulators = regulators,
        targets = targets,
        treeMethod = input$GENIE3_tree,
        returnMatrix = TRUE,
        verbose = FALSE
      ) %>%
        getLinkList()
    } else if (input$GENIE3_regulators == TRUE & input$GENIE3_targets == FALSE) {
      regulators <- GENIE3_regulators_gene()
      data <- GENIE3(
        as.matrix(GENIE3_expr_data()),
        nCores = ncore,
        nTrees = 1000,
        regulators = regulators,
        targets = NULL,
        treeMethod = input$GENIE3_tree,
        returnMatrix = TRUE,
        verbose = FALSE
      ) %>%
        getLinkList()
    } else if (input$GENIE3_regulators == FALSE & input$GENIE3_targets == TRUE) {
      targets <- GENIE3_targets_gene()
      data <- GENIE3(
        as.matrix(GENIE3_expr_data()),
        nCores = ncore,
        nTrees = 1000,
        regulators = NULL,
        targets = targets,
        treeMethod = input$GENIE3_tree,
        returnMatrix = TRUE,
        verbose = FALSE
      ) %>%
        getLinkList()
    }
    return(data)
  })
  observeEvent(input$GENIE3_exam, {
    updateMaterialSwitch(
      session = session,
      inputId = "GENIE3_regulators",
      value = TRUE
    )
    updateTextAreaInput(
      session = session,
      inputId = "GENIE3_regulators_list",
      value = "HORVU.MOREX.r3.1HG0000030\nHORVU.MOREX.r3.1HG0000040"
    )
    updateMaterialSwitch(
      session = session,
      inputId = "GENIE3_targets",
      value = TRUE
    )
    updateTextAreaInput(
      session = session,
      inputId = "GENIE3_targets_list",
      value = "HORVU.MOREX.r3.1HG0000050\nHORVU.MOREX.r3.1HG0000060"
    )
  })
  observeEvent(input$GENIE3_reset, {
    updateMaterialSwitch(
      session = session,
      inputId = "GENIE3_regulators",
      value = FALSE
    )
    updateMaterialSwitch(
      session = session,
      inputId = "GENIE3_targets",
      value = FALSE
    )
    updateSelectInput(
      session = session,
      inputId = "GENIE3_tree",
      selected = "RF"
    )
    updateNumericInput(
      inputId = "GENIE3_threshold",
      session = session,
      value = 0.2
    )
    updateColorPickr(
      session = session,
      inputId = "GENIE3_node_border",
      value = "#9AC9DB"
    )
    updateColorPickr(
      session = session,
      inputId = "GENIE3_node_backgroud",
      value = "#2878B5"
    )
    updateColorPickr(
      session = session,
      inputId = "GENIE3_edge",
      value = "#E7EFFA"
    )
    updateSelectInput(
      session = session,
      inputId = "GENIE3_layout",
      selected = "layout_nicely"
    )
    updateNumericInput(
      session = session,
      inputId = "GENIE3_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "GENIE3_fig_width",
      value = 1000
    )
  })
  output$GENIE3_tab <- renderDT(
    datatable(GENIE3_tab_data(),
              rownames = FALSE,
              filter = "bottom",
              extensions = "FixedColumns",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                columnDefs = list(list(className = "dt-left", targets = "_all"))))
  )
  output$GENIE3_tab_download <- downloadHandler(
    filename = function() {
      paste0("Co-Expression.", input$GENIE3_tab_format)
    },
    content = function(file) {
      if (input$GENIE3_tab_format == "txt") {
        write.table(GENIE3_tab_data(), file, row.names = F, col.names = T, quote = F)
      } else if (input$GENIE3_tab_format == "csv") {
        readr::write_csv(GENIE3_tab_data(), file, col_names = T)
      } else if (input$GENIE3_tab_format == "tsv") {
        readr::write_tsv(GENIE3_tab_data(), file, col_names = T)
      } else if (input$GENIE3_tab_format == "xlsx") {
        writexl::write_xlsx(GENIE3_tab_data(), file, col_names = T)
      }
    }
  )
  output$GENIE3_plot <- renderVisNetwork({
    GENIE3_net_tab <- dplyr::filter(GENIE3_tab_data(), weight > input$GENIE3_threshold)
    from <- data.frame(id = GENIE3_net_tab$regulatoryGene)
    to <- data.frame(id = GENIE3_net_tab$targetGene)
    nodes <- rbind(from, to) %>%
      unique()
    nodes <- data.frame(
      id = nodes,
      color = list(border = input$GENIE3_node_border,
                   background = input$GENIE3_node_backgroud,
                   highlight = "#FF8884"),
      shape = "dot"
    )
    edges <- data.frame(
      from = GENIE3_net_tab$regulatoryGene, 
      to = GENIE3_net_tab$targetGene,
      value = GENIE3_net_tab$weight,
      color = list(color = input$GENIE3_edge,
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
        layout = input$GENIE3_layout) %>%
      visExport(
        type = c("jpeg"), 
        name = "export-network", 
        float = "right", 
        label = "Save network", 
        background = "white", 
        style= "")
  })
}