mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        includeMarkdown("www/md/home1.Rmd")
      )
    ),
    br(),
    br(),
    sidebarPanel(
      width = 12,
      fluidRow(
        HTML('<i class="fa fa-bookmark" aria-hidden="true"></i> <font size="4" color="black"><b>Cluster</b></font>'),
        br(),
        br(),
        column(
          width = 2
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_Hcluster", 
            label = "Hcluster", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_MDS", 
            label = "MDS", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_PCA", 
            label = "PCA", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_TSNE", 
            label = "TSNE", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2
        )
      ),
      br(),
      br(),
      fluidRow(
        HTML('<i class="fa fa-bookmark" aria-hidden="true"></i> <font size="4" color="black"><b>NetWork</b></font>'),
        br(),
        br(),
        column(
          width = 2
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_GENIE3", 
            label = "GENIE3", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_MEGENA", 
            label = "MEGENA", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_MINET", 
            label = "MINET", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2,
          shinyWidgets::actionBttn(
            inputId = "Link_WGCNA", 
            label = "WGCNA", 
            block = TRUE, 
            size = "lg", 
            style="unite", 
            color="success",
            no_outline = T,
            icon = icon("book"))
        ),
        column(
          width = 2
        )
      )
    ),
    fluidRow(
      column(
        width = 1
      ),
      column(
        width = 5,
        includeMarkdown("www/md/home2.Rmd")
      ),
      column(
        width = 5,
        includeMarkdown("www/md/home3.Rmd")
      ),
      column(
        width = 1
      ),
    ),
    mainPanel(),
  )
}
mod_home_server <- function(input, output, session){
  ns <- session$ns
}
