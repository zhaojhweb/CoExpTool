#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shinydisconnect::disconnectMessage(
    text = "Time out！",
    refresh = "Reload now",
    background = "brown",
    colour = "white",
    overlayColour = "gray",
    overlayOpacity = 0.75,
    top = 200,
    refreshColour = "yellow"
  )
  navbarPage(
    title =  HTML("<strong style='font-size:20px'>CoExpTool</strong>"),
    windowTitle = "Welcome to CoExpTool!",
    theme = shinythemes::shinytheme("flatly"),
    inverse = TRUE,
    collapsible = TRUE,
    id = "Page",
    tabPanel(
      title = HTML("<strong style='font-size:20px'>Home</strong>"),
      icon = icon("home"),
      mod_home_ui("home")
    ),
    navbarMenu(
      title = HTML("<strong style='font-size:20px'>Cluster</strong>"),
      icon = icon("chart-gantt"),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Hcluster</strong>"),
        mod_hcluster_ui("hcluster")
      ),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>MDS</strong>"),
        mod_mds_ui("mds")
      ),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>PCA</strong>"),
        mod_pca_ui("pca")
      ),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>TSNE</strong>"),
        mod_tsne_ui("tsne")
      )
    ),
    navbarMenu(
      title = HTML("<strong style='font-size:20px'>NetWork</strong>"),
      icon = icon("circle-nodes"),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>GENIE3</strong>"),
        mod_genie3_ui("genie3")
      ),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>MEGENA</strong>"),
        mod_megena_ui("megena")
      ),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>MINET</strong>"),
        mod_minet_ui("minet")
      ),
      tabPanel(
        title = HTML("<strong style='font-size:18px'>WGCNA</strong>"),
        mod_wgcna_ui("wgcna")
      )
    ),
    tabPanel(
      title = HTML("<strong style='font-size:20px'>About</strong>"),
      value = "my_about",
      icon = icon("circle-info"),
      mod_about_ui("about")
    )
  )
}

