mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      HTML('<font size="6" color="red"><b>PCA</b></font>'),
      br(),
      br(),
      fileInput(
        inputId = ns("PCA_expr"),
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
        inputId = ns("PCA_group"),
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
      selectInput(
        inputId = ns("PCA_x"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>X-axis</b></font>')),
        choices = c("PC1", "PC2"),
        selected = "PC1"
      ),
      selectInput(
        inputId = ns("PCA_y"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Y-axis</b></font>')),
        choices = c("PC1", "PC2"),
        selected = "PC2"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("PCA_label"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Displays the sample name</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("PCA_ellipse"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Add a confidence ellipse</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      uiOutput(
        outputId = ns("PCA_ellipse_type_ui")
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
      numericInput(
        inputId = ns("PCA_fig_height"), 
        label = "Plot height", 
        value = 800
      ),
      numericInput(
        inputId = ns("PCA_fig_width"), 
        label = "Plot width", 
        value = 1000
      ),
      br(),
      br(),
      shinysky::actionButton(
        inputId = ns("PCA_submit"),
        label = "Submit",
        styleclass = "danger",
        size = "small"
      ),
      shinysky::actionButton(
        inputId = ns("PCA_reset"),
        label = "Reset",
        styleclass = "warning"
      ),
      shinysky::actionButton(
        inputId = ns("PCA_exam"),
        label = "Load example",
        styleclass = "info"
      )
    ),
    mainPanel(
      uiOutput(outputId = ns("PCA_layout")),
      column(
        width = 12,
        shinycustomloader::withLoader(
          uiOutput(ns("PCA_ui")), 
          type="html", 
          loader= "dnaspin")
      )
    )
  )
}
mod_pca_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  output$PCA_ui <- renderUI({
    if (is.na(input$PCA_fig_height) | is.na(input$PCA_fig_width) | input$PCA_fig_height * input$PCA_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("PCA_plot"),
          height = paste0(input$PCA_fig_height, "px"),
          width = paste0(input$PCA_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  observeEvent(input$PCA_submit, {
    output$PCA_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("PCA_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("PCA_fig_format"),
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
            outputId = ns("PCA_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  output$PCA_ellipse_type_ui <- renderUI({
    if (input$PCA_ellipse == "TRUE") {
      fluidPage(
        selectInput(
          inputId = ns("PCA_ellipse_type"),
          label = h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="3" color="black"><b>Ellipse type</b></font>')),
          choices = c("confidence", "convex", "euclid", "norm", "t"),
          selected = "confidence"
        ) 
      )
    } else {
      NULL
    }
  })
  PCA_expr_ori <- reactive({
    if (input$PCA_exam > 0 & is.null(input$PCA_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$PCA_expr)
        data <- read.delim(
          file = input$PCA_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    return(expr_data())
  })
  PCA_group_info <- eventReactive(input$PCA_submit, {
    if (input$PCA_exam > 0 & is.null(input$PCA_expr)) {
      group <- reactive({
        data <- Exam_sample_info
        return(data)
      })
    } else {
      group <- reactive({
        req(input$PCA_group)
        data <- read.delim(
          file = input$PCA_group$datapath,
          row.names = 1
        )
        return(data)
      })
    }
    return(group())
  })
  PCA_data <- eventReactive(input$PCA_submit, {
    group_sort <- PCA_group_info()[match(colnames(PCA_expr_ori()), rownames(PCA_group_info())),]
    group <- data.frame(
      row.names = colnames(PCA_expr_ori()),
      group_sort
    )
    colnames(group) <- colnames(PCA_group_info())
    pca_re <- PCAtools::pca(
      mat = PCA_expr_ori(),
      metadata = group
    )
    data <- merge(pca_re$rotated, group, by = 0)
    data1 <- data.frame(
      data,
      row.names = 1
    )
    return(data1)
  })
  observeEvent(input$PCA_submit, {
    updateSelectInput(
      session = session,
      inputId = "PCA_x",
      choices = c(paste0("PC",1:ncol(PCA_expr_ori()))),
      selected = "PC1"
    )
    updateSelectInput(
      session = session,
      inputId = "PCA_y",
      choices = c(paste0("PC",1:ncol(PCA_expr_ori()))),
      selected = "PC2"
    )
  })
  sample_info <- eventReactive(input$PCA_submit, {
    sample_info <- colnames(PCA_group_info())
    return(sample_info)
  })
  output$PCA_plot <- renderPlot({
    if (input$PCA_label == TRUE) {
      ggscatter(
        data = PCA_data(), 
        x = input$PCA_x, 
        y = input$PCA_y, 
        label = rownames(PCA_data()),
        size = 1,
        repel = TRUE,
        color = sample_info(),
        palette = "ucscgb",
        ellipse = input$PCA_ellipse,
        ellipse.type = input$PCA_ellipse_type
      )
    } else {
      ggscatter(
        data = PCA_data(), 
        x = input$PCA_x, 
        y = input$PCA_y, 
        size = 1,
        repel = TRUE,
        color = sample_info(),
        palette = "ucscgb",
        ellipse = input$PCA_ellipse,
        ellipse.type = input$PCA_ellipse_type
      )
    }
  })
  output$PCA_fig_download <- downloadHandler(
    filename = function() {
      paste0("PCA_fig.", input$PCA_fig_format)
    },
    content = function(file) {
      if (input$PCA_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$PCA_fig_width, 
          height = 10 * input$PCA_fig_height, 
          res = 300)
      } else if (input$PCA_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$PCA_fig_width/20, 
          height = input$PCA_fig_height/20, 
          onefile = F)
      } else if (input$PCA_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$PCA_fig_width, 
          height = 10 * input$PCA_fig_height, 
          res = 300)
      } else if (input$PCA_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$PCA_fig_width, 
          height = 10 * input$PCA_fig_height, 
          res = 300)
      } else if (input$PCA_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$PCA_fig_width, 
          height = 10 * input$PCA_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$PCA_fig_width/20, 
          height = input$PCA_fig_height/20)
      }
      if (input$PCA_label == TRUE) {
        p <- ggscatter(
          data = PCA_data(), 
          x = input$PCA_x, 
          y = input$PCA_y, 
          label = rownames(PCA_data()),
          size = 1,
          repel = TRUE,
          color = sample_info(),
          palette = "ucscgb",
          ellipse = input$PCA_ellipse,
          ellipse.type = input$PCA_ellipse_type
        )
      } else {
        p <- ggscatter(
          data = PCA_data(), 
          x = input$PCA_x, 
          y = input$PCA_y, 
          size = 1,
          repel = TRUE,
          color = sample_info(),
          palette = "ucscgb",
          ellipse = input$PCA_ellipse,
          ellipse.type = input$PCA_ellipse_type
        )
      }
      plot(p)
      dev.off()
      if (input$PCA_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          if (input$PCA_label == TRUE) {
            p <- ggscatter(
              data = PCA_data(), 
              x = input$PCA_x, 
              y = input$PCA_y, 
              label = rownames(PCA_data()),
              size = 1,
              repel = TRUE,
              color = sample_info(),
              palette = "ucscgb",
              ellipse = input$PCA_ellipse,
              ellipse.type = input$PCA_ellipse_type
            )
          } else {
            p <- ggscatter(
              data = PCA_data(), 
              x = input$PCA_x, 
              y = input$PCA_y, 
              size = 1,
              repel = TRUE,
              color = sample_info(),
              palette = "ucscgb",
              ellipse = input$PCA_ellipse,
              ellipse.type = input$PCA_ellipse_type
            )
          }
          plot(p)
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$PCA_reset, {
    updateSelectInput(
      session = session,
      inputId = "PCA_x",
      selected = "PC1"
    )
    updateSelectInput(
      session = session,
      inputId = "PCA_y",
      selected = "PC2"
    )
    updateNumericInput(
      session = session,
      inputId = "PCA_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "PCA_fig_width",
      value = 1000
    )
    updateMaterialSwitch(
      session = session,
      inputId = "PCA_label",
      value = TRUE
    )
    updateMaterialSwitch(
      session = session,
      inputId = "PCA_ellipse",
      value = TRUE
    )
    updateSelectInput(
      session = session,
      inputId = "PCA_ellipse_type",
      selected = "confidence"
    )
  })
}




















