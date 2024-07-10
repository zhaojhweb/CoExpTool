mod_mds_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      HTML('<font size="6" color="red"><b>MDS</b></font>'),
      br(),
      br(),
      fileInput(
        inputId = ns("MDS_expr"),
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
      fileInput(
        inputId = ns("MDS_group"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Upload your sample information</b></font>'),
                   bsplus::bs_button(
                     label = "?", 
                     button_type = "primary", 
                     button_size = "extra-small") %>%
                     bsplus::bs_embed_tooltip(title = "Note：The first column is the sample name, and the second column is the grouping information")),
        accept = ".txt",
        buttonLabel = "Browse...",
        placeholder = "No file selected",
        capture = NULL
      ),
      selectInput(
        inputId = ns("MDS_dist"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Distance matrix computation</b></font>')),
        choices = c("binary", "canberra", "euclidean", "maximum", "manhattan", "minkowski"),
        selected = "euclidean"
      ),
      selectInput(
        inputId = ns("MDS_method"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>MDS method</b></font>')),
        choices = c("PCoA", "Kruskal", "Sammon"),
        selected = "Kruskal"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("MDS_label"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Displays the sample name</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("MDS_ellipse"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Add a confidence ellipse</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      uiOutput(
        outputId = ns("MDS_ellipse_type_ui")
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
      numericInput(
        inputId = ns("MDS_fig_height"), 
        label = "Plot height", 
        value = 800
      ),
      numericInput(
        inputId = ns("MDS_fig_width"), 
        label = "Plot width", 
        value = 1000
      ),
      br(),
      br(),
      shinysky::actionButton(
        inputId = ns("MDS_submit"),
        label = "Submit",
        styleclass = "danger",
        size = "small"
      ),
      shinysky::actionButton(
        inputId = ns("MDS_reset"),
        label = "Reset",
        styleclass = "warning"
      ),
      shinysky::actionButton(
        inputId = ns("MDS_exam"),
        label = "Load example",
        styleclass = "info"
      )
    ),
    mainPanel(
      uiOutput(outputId = ns("MDS_layout")),
      column(
        width = 12,
        shinycustomloader::withLoader(
          uiOutput(ns("MDS_ui")), 
          type="html", 
          loader= "dnaspin")
      )
    )
  )
}
mod_mds_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  output$MDS_ui <- renderUI({
    if (is.na(input$MDS_fig_height) | is.na(input$MDS_fig_width) | input$MDS_fig_height * input$MDS_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("MDS_plot"),
          height = paste0(input$MDS_fig_height, "px"),
          width = paste0(input$MDS_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  observeEvent(input$MDS_submit, {
    output$MDS_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("MDS_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("MDS_fig_format"),
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
            outputId = ns("MDS_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$MDS_ellipse_type_ui <- renderUI({
    if (input$MDS_ellipse == "TRUE") {
      fluidPage(
        selectInput(
          inputId = ns("MDS_ellipse_type"),
          label = h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="3" color="black"><b>Ellipse type</b></font>')),
          choices = c("confidence", "convex", "euclid", "norm", "t"),
          selected = "confidence"
        ) 
      )
    } else {
      NULL
    }
  })
  MDS_expr_ori <- eventReactive(input$MDS_submit, {
    if (input$MDS_exam > 0 & is.null(input$MDS_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$MDS_expr)
        data <- read.delim(
          file = input$MDS_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    return(expr_data())
  })
  MDS_group_info <- eventReactive(input$MDS_submit, {
    if (input$MDS_exam > 0 & is.null(input$MDS_expr)) {
      group <- reactive({
        data <- Exam_sample_info
        return(data)
      })
    } else {
      group <- reactive({
        req(input$MDS_group)
        data <- read.delim(
          file = input$MDS_group$datapath,
          row.names = 1
        )
        return(data)
      })
    }
    return(group())
  })
  MDS_data <- eventReactive(input$MDS_submit, {
    expr_data_t <- t(MDS_expr_ori())
    expr_data_t_dist <- dist(
      x = expr_data_t,
      method = input$MDS_dist
    )
    if (input$MDS_method == "Kruskal") {
      expr_data_t_dist_mds <- isoMDS(expr_data_t_dist)
    } else if(input$MDS_method == "Sammon"){
      expr_data_t_dist_mds <- sammon(expr_data_t_dist)
    } else {
      expr_data_t_dist_mds <- cmdscale(expr_data_t_dist)
    }
    if (input$MDS_method == "Kruskal" | input$MDS_method == "Sammon") {
      mds_data <- expr_data_t_dist_mds %>%
        .$points %>%
        as_tibble()
    } else {
      mds_data <- expr_data_t_dist_mds %>%
        as_tibble()
    }
    colnames(mds_data) <- c("Dimension 1", "Dimension 2")
    rownames(mds_data) <- rownames(expr_data_t)
    return(mds_data)
  })
  mds <- eventReactive(input$MDS_submit, {
    mds_data <- as.matrix(MDS_data())
    mds_group <- merge(mds_data, MDS_group_info(), by = 0)
    mds <- data.frame(mds_group, row.names = 1, check.names = F)
    return(mds)
  })
  sample_info <- eventReactive(input$MDS_submit, {
    sample_info <- colnames(MDS_group_info())
    return(sample_info)
  })
  output$MDS_plot <- renderPlot({
    if (input$MDS_label == TRUE) {
      ggscatter(
        data = mds(), 
        x = "Dimension 1", 
        y = "Dimension 2", 
        label = rownames(mds()),
        size = 1,
        repel = TRUE,
        color = sample_info(),
        palette = "ucscgb",
        ellipse = input$MDS_ellipse,
        ellipse.type = input$MDS_ellipse_type
      )
    } else {
      ggscatter(
        data = mds(), 
        x = "Dimension 1", 
        y = "Dimension 2", 
        size = 1,
        repel = TRUE,
        color = sample_info(),
        palette = "ucscgb",
        ellipse = input$MDS_ellipse,
        ellipse.type = input$MDS_ellipse_type
      )
    }
  })
  output$MDS_fig_download <- downloadHandler(
    filename = function() {
      paste0("MDS_fig.", input$MDS_fig_format)
    },
    content = function(file) {
      if (input$MDS_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$MDS_fig_width, 
          height = 10 * input$MDS_fig_height, 
          res = 300)
      } else if (input$MDS_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$MDS_fig_width/20, 
          height = input$MDS_fig_height/20, 
          onefile = F)
      } else if (input$MDS_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$MDS_fig_width, 
          height = 10 * input$MDS_fig_height, 
          res = 300)
      } else if (input$MDS_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$MDS_fig_width, 
          height = 10 * input$MDS_fig_height, 
          res = 300)
      } else if (input$MDS_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$MDS_fig_width, 
          height = 10 * input$MDS_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$MDS_fig_width/20, 
          height = input$MDS_fig_height/20)
      }
      if (input$MDS_label == TRUE) {
        p <- ggscatter(
          data = mds(), 
          x = "Dimension 1", 
          y = "Dimension 2", 
          label = rownames(mds()),
          size = 1,
          repel = TRUE,
          color = sample_info(),
          palette = "ucscgb",
          ellipse = input$MDS_ellipse,
          ellipse.type = input$MDS_ellipse_type
        )
      } else {
        p <- ggscatter(
          data = mds(), 
          x = "Dimension 1", 
          y = "Dimension 2", 
          size = 1,
          repel = TRUE,
          color = sample_info(),
          palette = "ucscgb",
          ellipse = input$MDS_ellipse,
          ellipse.type = input$MDS_ellipse_type
        )
      }
      plot(p)
      dev.off()
      if (input$MDS_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          if (input$MDS_label == TRUE) {
            p <- ggscatter(
              data = mds(), 
              x = "Dimension 1", 
              y = "Dimension 2", 
              label = rownames(mds()),
              size = 1,
              repel = TRUE,
              color = sample_info(),
              palette = "ucscgb",
              ellipse = input$MDS_ellipse,
              ellipse.type = input$MDS_ellipse_type
            )
          } else {
            p <- ggscatter(
              data = mds(), 
              x = "Dimension 1", 
              y = "Dimension 2", 
              size = 1,
              repel = TRUE,
              color = sample_info(),
              palette = "ucscgb",
              ellipse = input$MDS_ellipse,
              ellipse.type = input$MDS_ellipse_type
            )
          }
          plot(p)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$MDS_reset, {
    updateSelectInput(
      session = session,
      inputId = "MDS_dist",
      selected = "euclidean"
    )
    updateSelectInput(
      session = session,
      inputId = "MDS_method",
      selected = "Kruskal"
    )
    updateNumericInput(
      session = session,
      inputId = "MDS_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "MDS_fig_width",
      value = 1000
    )
    updateMaterialSwitch(
      session = session,
      inputId = "MDS_label",
      value = TRUE
    )
    updateMaterialSwitch(
      session = session,
      inputId = "MDS_ellipse",
      value = TRUE
    )
    updateSelectInput(
      session = session,
      inputId = "MDS_ellipse_type",
      selected = "confidence"
    )
  })
}