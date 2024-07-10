mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 5,
      offset = 1,
      includeMarkdown("www/md/about1.Rmd")
    ),
    column(
      width = 5,
      offset = 1,
      includeMarkdown("www/md/about2.Rmd")
    )
  )
}

mod_about_server <- function(input, output, session) {
  ns <- session$ns
}