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
        "main",
        mod_main_ui("main")
    )
}


#' @export
run_app <- function() {
    shiny::shinyApp(
        server = app_server,
        ui = app_ui,
    )
}