mod_tsne_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      HTML('<font size="6" color="red"><b>TSNE</b></font>'),
      br(),
      br(),
      fileInput(
        inputId = ns("TSNE_expr"),
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
        inputId = ns("TSNE_group"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Upload your sample information</b></font>'),
                   bsplus::bs_button(
                     label = "?", 
                     button_type = "primary", 
                     button_size = "extra-small") %>%
                     bsplus::bs_embed_tooltip(title = "Note：The first column is the sample name, and the second column is the grouping information.")),
        accept = ".txt",
        buttonLabel = "Browse...",
        placeholder = "No file selected",
        capture = NULL
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("TSNE_pca"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Whether an initial PCA step should be performed</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      sliderInput(
        inputId = ns("TSNE_perp"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Perplexity</b></font>')),
        min = 1,
        max = 10,
        value = 2
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("TSNE_label"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Displays the sample name</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("TSNE_ellipse"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Add a confidence ellipse</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      uiOutput(
        outputId = ns("TSNE_ellipse_type_ui")
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
      numericInput(
        inputId = ns("TSNE_fig_height"), 
        label = "Plot height", 
        value = 800
      ),
      numericInput(
        inputId = ns("TSNE_fig_width"), 
        label = "Plot width", 
        value = 1000
      ),
      br(),
      br(),
      shinysky::actionButton(
        inputId = ns("TSNE_submit"),
        label = "Submit",
        styleclass = "danger",
        size = "small"
      ),
      shinysky::actionButton(
        inputId = ns("TSNE_reset"),
        label = "Reset",
        styleclass = "warning"
      ),
      shinysky::actionButton(
        inputId = ns("TSNE_exam"),
        label = "Load example",
        styleclass = "info"
      )
    ),
    mainPanel(
      uiOutput(outputId = ns("TSNE_layout")),
      column(
        width = 12,
        shinycustomloader::withLoader(
          uiOutput(ns("TSNE_ui")), 
          type="html", 
          loader= "dnaspin")
      )
    )
  )
}
mod_tsne_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  output$TSNE_ui <- renderUI({
    if (is.na(input$TSNE_fig_height) | is.na(input$TSNE_fig_width) | input$TSNE_fig_height * input$TSNE_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("TSNE_plot"),
          height = paste0(input$TSNE_fig_height, "px"),
          width = paste0(input$TSNE_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  observeEvent(input$TSNE_submit, {
    output$TSNE_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("TSNE_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("TSNE_fig_format"),
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
            outputId = ns("TSNE_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$TSNE_ellipse_type_ui <- renderUI({
    if (input$TSNE_ellipse == "TRUE") {
      fluidPage(
        selectInput(
          inputId = ns("TSNE_ellipse_type"),
          label = h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="3" color="black"><b>Ellipse type</b></font>')),
          choices = c("confidence", "convex", "euclid", "norm", "t"),
          selected = "confidence"
        ) 
      )
    } else {
      NULL
    }
  })
  TSNE_expr_ori <- reactive({
    if (input$TSNE_exam > 0 & is.null(input$TSNE_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$TSNE_expr)
        data <- read.delim(
          file = input$TSNE_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    return(expr_data())
  })
  TSNE_group_info <- eventReactive(input$TSNE_submit, {
    if (input$TSNE_exam > 0 & is.null(input$TSNE_group)) {
      group <- reactive({
        data <- Exam_sample_info
        return(data)
      })
    } else {
      group <- reactive({
        req(input$TSNE_group)
        data <- read.delim(
          file = input$TSNE_group$datapath,
          row.names = 1
        )
        return(data)
      })
    }
    return(group())
  })
  observeEvent(input$TSNE_expr, {
    updateSliderInput(
      session = session,
      inputId = "TSNE_perp",
      min = 1,
      max = trunc((ncol(TSNE_expr_ori())+1)/3-1),
      value = trunc(((ncol(TSNE_expr_ori())+1)/3-1)/2)
    )
  })
  observeEvent(input$TSNE_exam, {
    updateSliderInput(
      session = session,
      inputId = "TSNE_perp",
      min = 1,
      max = 31,
      value = 15
    )
  })
  TSNE_data <- eventReactive(input$TSNE_submit, {
    expr_data_t <- t(TSNE_expr_ori())
    tsne_data <- Rtsne(
      X = expr_data_t,
      dims = 2,
      pca = input$TSNE_pca,
      perplexity = input$TSNE_perp,
      num_threads = 0,
    )
    tsne_data_y <- tsne_data$Y
    colnames(tsne_data_y) <- c("Dimension 1", "Dimension 2")
    rownames(tsne_data_y) <- rownames(expr_data_t)
    tsne_data <- merge(tsne_data_y, TSNE_group_info(), by = 0)
    tsne_data <- data.frame(tsne_data, row.names = 1, check.names = F)
    return(tsne_data)
  })
  sample_info <- eventReactive(input$TSNE_submit, {
    sample_info <- colnames(TSNE_group_info())
    return(sample_info)
  })
  output$TSNE_plot <- renderPlot({
    if (input$TSNE_label == TRUE) {
      ggscatter(
        data = TSNE_data(), 
        x = "Dimension 1", 
        y = "Dimension 2", 
        label = rownames(TSNE_data()),
        size = 1,
        repel = TRUE,
        color = sample_info(),
        palette = "ucscgb",
        ellipse = input$TSNE_ellipse,
        ellipse.type = input$TSNE_ellipse_type
      )
    } else {
      ggscatter(
        data = TSNE_data(), 
        x = "Dimension 1", 
        y = "Dimension 2", 
        size = 1,
        repel = TRUE,
        color = sample_info(),
        palette = "ucscgb",
        ellipse = input$TSNE_ellipse,
        ellipse.type = input$TSNE_ellipse_type
      )
    }
  })
  output$TSNE_fig_download <- downloadHandler(
    filename = function() {
      paste0("TSNE_fig.", input$TSNE_fig_format)
    },
    content = function(file) {
      if (input$TSNE_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$TSNE_fig_width, 
          height = 10 * input$TSNE_fig_height, 
          res = 300)
      } else if (input$TSNE_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$TSNE_fig_width/20, 
          height = input$TSNE_fig_height/20, 
          onefile = F)
      } else if (input$TSNE_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$TSNE_fig_width, 
          height = 10 * input$TSNE_fig_height, 
          res = 300)
      } else if (input$TSNE_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$TSNE_fig_width, 
          height = 10 * input$TSNE_fig_height, 
          res = 300)
      } else if (input$TSNE_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$TSNE_fig_width, 
          height = 10 * input$TSNE_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$TSNE_fig_width/20, 
          height = input$TSNE_fig_height/20)
      }
      if (input$TSNE_label == TRUE) {
        p <- ggscatter(
          data = TSNE_data(), 
          x = "Dimension 1", 
          y = "Dimension 2", 
          label = rownames(TSNE_data()),
          size = 1,
          repel = TRUE,
          color = sample_info(),
          palette = "ucscgb",
          ellipse = input$TSNE_ellipse,
          ellipse.type = input$TSNE_ellipse_type
        )
      } else {
        p <- ggscatter(
          data = TSNE_data(), 
          x = "Dimension 1", 
          y = "Dimension 2", 
          size = 1,
          repel = TRUE,
          color = sample_info(),
          palette = "ucscgb",
          ellipse = input$TSNE_ellipse,
          ellipse.type = input$TSNE_ellipse_type
        )
      }
      plot(p)
      dev.off()
      if (input$TSNE_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          if (input$TSNE_label == TRUE) {
            p <- ggscatter(
              data = TSNE_data(), 
              x = "Dimension 1", 
              y = "Dimension 2", 
              label = rownames(TSNE_data()),
              size = 1,
              repel = TRUE,
              color = sample_info(),
              palette = "ucscgb",
              ellipse = input$TSNE_ellipse,
              ellipse.type = input$TSNE_ellipse_type
            )
          } else {
            p <- ggscatter(
              data = TSNE_data(), 
              x = "Dimension 1", 
              y = "Dimension 2", 
              size = 1,
              repel = TRUE,
              color = sample_info(),
              palette = "ucscgb",
              ellipse = input$TSNE_ellipse,
              ellipse.type = input$TSNE_ellipse_type
            )
          }
          plot(p)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$TSNE_reset, {
    updateMaterialSwitch(
      session = session,
      inputId = "TSNE_pca",
      value = TRUE
    )
    updateNumericInput(
      session = session,
      inputId = "TSNE_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "TSNE_fig_width",
      value = 1000
    )
    updateMaterialSwitch(
      session = session,
      inputId = "TSNE_label",
      value = TRUE
    )
    updateMaterialSwitch(
      session = session,
      inputId = "TSNE_ellipse",
      value = TRUE
    )
    updateSelectInput(
      session = session,
      inputId = "TSNE_ellipse_type",
      selected = "confidence"
    )
  })
}

