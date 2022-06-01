#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic
    mod_main_server("main")
}

app_ui <- function(request) {
    shiny.quartz::Page(
        "SARS-CoV-2 Variant and Lineage Investigator",
        mod_main_ui("main")
    )
}

ggquartz::apply_theme()


#' @export
run_app <- function() {
    shiny::shinyApp(
        server = app_server,
        ui = app_ui,
    )
}