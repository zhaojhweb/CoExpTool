#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize=3000*1024^2)
  observeEvent(input$Link_Hcluster, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>Hcluster</strong>"))
  })
  observeEvent(input$Link_MDS, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>MDS</strong>"))
  })
  observeEvent(input$Link_PCA, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>PCA</strong>"))
  })
  observeEvent(input$Link_TSNE, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>TSNE</strong>"))
  })
  observeEvent(input$Link_GENIE3, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>GENIE3</strong>"))
  })
  observeEvent(input$Link_MEGENA, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>MEGENA</strong>"))
  })
  observeEvent(input$Link_MINET, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>MINET</strong>"))
  })
  observeEvent(input$Link_WGCNA, {
    updateNavbarPage(
      session = session,
      inputId = "Page",
      selected = HTML("<strong style='font-size:18px'>WGCNA</strong>"))
  })
  callModule(mod_home_server, "home")
  callModule(mod_about_server, "about")
  callModule(mod_hcluster_server, "hcluster")
  callModule(mod_mds_server, "mds")
  callModule(mod_tsne_server, "tsne")
  callModule(mod_pca_server, "pca")
  callModule(mod_genie3_server, "genie3")
  callModule(mod_minet_server, "minet")
  callModule(mod_megena_server, "megena")
  callModule(mod_wgcna_server, "wgcna")
}
