mod_hcluster_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      HTML('<font size="6" color="red"><b>Hcluster</b></font>'),
      br(),
      br(),
      fileInput(
        inputId = ns("Hcluster_expr"),
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
        inputId = ns("Hcluster_dist"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Distance matrix computation</b></font>')),
        choices = c("binary", "canberra", "euclidean", "maximum", "manhattan", "minkowski"),
        selected = "euclidean"
      ),
      selectInput(
        inputId = ns("Hcluster_hclust"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Hierarchical clustering</b></font>')),
        choices = c("average", "centroid", "complete", "median", "single", "ward"),
        selected = "complete"
      ),
      shinyWidgets::awesomeRadio(
        inputId = ns("Hcluster_type"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot type</b></font>')),
        choices = c("rectangle", "triangle"),
        selected = "triangle",
        status = "danger",
        inline = TRUE,
        checkbox = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("Hcluster_horiz"),
        label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot horizontal</b></font>')),
        value = TRUE,
        status = "danger"
      ),
      h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="black"><b>Plot size</b></font>')),
      numericInput(
        inputId = ns("Hcluster_fig_height"), 
        label = "Plot height", 
        value = 800
      ),
      numericInput(
        inputId = ns("Hcluster_fig_width"), 
        label = "Plot width", 
        value = 1000
      ),
      br(),
      br(),
      shinysky::actionButton(
        inputId = ns("Hcluster_submit"),
        label = "Submit",
        styleclass = "danger",
        size = "small"
      ),
      shinysky::actionButton(
        inputId = ns("Hcluster_reset"),
        label = "Reset",
        styleclass = "warning"
      ),
      shinysky::actionButton(
        inputId = ns("Hcluster_exam"),
        label = "Load example",
        styleclass = "info"
      )
    ),
    mainPanel(
      uiOutput(outputId = ns("Hcluster_layout")),
      column(
        width = 12,
        shinycustomloader::withLoader(
          uiOutput(ns("Hcluster_ui")), 
          type="html", 
          loader= "dnaspin")
      )
    )
  )
}
mod_hcluster_server <- function(input, output, session) {
  ns <- session$ns
  set.seed(123)
  output$Hcluster_ui <- renderUI({
    if (is.na(input$Hcluster_fig_height) | is.na(input$Hcluster_fig_width) | input$Hcluster_fig_height * input$Hcluster_fig_width == 0) {
      NULL
    } else {
      shinycustomloader::withLoader(
        plotOutput(
          outputId = ns("Hcluster_plot"),
          height = paste0(input$Hcluster_fig_height, "px"),
          width = paste0(input$Hcluster_fig_width, "px")),
        type="html", 
        loader= "dnaspin")
    }
  })
  observeEvent(input$Hcluster_submit, {
    output$Hcluster_layout <- renderUI(
      fluidPage(
        column(
          width = 4,
          shinyWidgets::dropdownButton(
            inputId = ns("Hcluster_fig_format"),
            label = "Formats",
            shinyWidgets::radioGroupButtons(
              inputId = ns("Hcluster_fig_format"),
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
            outputId = ns("Hcluster_fig_download"),
            label = "Download",
            icon = icon("download")
          )
        )
      )
    )
  })
  Hcluster_data <- eventReactive(input$Hcluster_submit, {
    if (input$Hcluster_exam > 0 & is.null(input$Hcluster_expr)) {
      expr_data <- reactive({
        data <- Exam_expr
        return(data)
      })
    } else {
      expr_data <- reactive({
        req(input$Hcluster_expr)
        data <- read.delim(
          file = input$Hcluster_expr$datapath,
          row.names = 1,
          check.names = F
        )
        return(data)
      })
    }
    expr_data_t <- t(expr_data())
    expr_data_t_dist<- dist(
      x = expr_data_t,
      method = input$Hcluster_dist
    )
    expr_data_t_dist_hclust <- hclust(
      d = expr_data_t_dist,
      method = input$Hcluster_hclust
    )
    expr_data_t_dist_hclust_dend <- as.dendrogram(
      object = expr_data_t_dist_hclust
    )
    data <- expr_data_t_dist_hclust_dend
    return(data)
  })
  output$Hcluster_plot <- renderPlot({
    base::plot(
      x = Hcluster_data(),
      type = input$Hcluster_type,
      main = "Cluster Dendrogram",
      horiz = input$Hcluster_horiz,
      nodePar = list(
        pch = 18:20, 
        cex = 1.2:1.5, 
        col = 2:3
      ))
  })
  output$Hcluster_fig_download <- downloadHandler(
    filename = function() {
      paste0("Hcluster_fig.", input$Hcluster_fig_format)
    },
    content = function(file) {
      if (input$Hcluster_fig_format == "png") {
        png(
          filename = file, 
          width = 10 * input$Hcluster_fig_width, 
          height = 10 * input$Hcluster_fig_height, 
          res = 300)
      } else if (input$Hcluster_fig_format == "pdf") {
        pdf(
          file = file, 
          width = input$Hcluster_fig_width/20, 
          height = input$Hcluster_fig_height/20, 
          onefile = F)
      } else if (input$Hcluster_fig_format == "jpeg") {
        jpeg(
          filename = file, 
          width = 10 * input$Hcluster_fig_width, 
          height = 10 * input$Hcluster_fig_height, 
          res = 300)
      } else if (input$Hcluster_fig_format == "tiff") {
        tiff(
          filename = file, 
          width = 10 * input$Hcluster_fig_width, 
          height = 10 * input$Hcluster_fig_height, 
          res = 300)
      } else if (input$Hcluster_fig_format == "bmp") {
        bmp(
          filename = file, 
          width = 10 * input$Hcluster_fig_width, 
          height = 10 * input$Hcluster_fig_height, 
          res = 300)
      } else {
        svg(
          filename = file,
          width = input$Hcluster_fig_width/20, 
          height = input$Hcluster_fig_height/20)
      }
      base::plot(
        x = Hcluster_data(),
        type = input$Hcluster_type,
        main = "Cluster Dendrogram",
        horiz = input$Hcluster_horiz,
        nodePar = list(
          pch = 18:20, 
          cex = 1.2:1.5, 
          col = 2:3
        ))
      dev.off()
      if (input$Hcluster_fig_format == "pptx") {
        doc <- read_pptx()
        doc <- add_slide(doc)
        anyplot <- rvg::dml(code = {
          base:: plot(
            x = Hcluster_data(),
            type = input$Hcluster_type,
            main = "Cluster Dendrogram",
            horiz = input$Hcluster_horiz,
            nodePar = list(
              pch = 18:20, 
              cex = 1.2:1.5, 
              col = 2:3
            ))
        })
        doc <- ph_with(doc,anyplot,location = ph_location_fullsize())
        print(doc, target = file)
      }
    }
  )
  observeEvent(input$Hcluster_reset, {
    updateSelectInput(
      session = session,
      inputId = "Hcluster_dist",
      selected = "euclidean"
    )
    updateSelectInput(
      session = session,
      inputId = "Hcluster_hclust",
      selected = "complete"
    )
    updateAwesomeRadio(
      session = session,
      inputId = "Hcluster_type",
      selected = "triangle"
    )
    updateMaterialSwitch(
      session = session,
      inputId = "Hcluster_horiz",
      value = TRUE
    )
    updateNumericInput(
      session = session,
      inputId = "Hcluster_fig_height",
      value = 800
    )
    updateNumericInput(
      session = session,
      inputId = "Hcluster_fig_width",
      value = 1000
    )
  })
}