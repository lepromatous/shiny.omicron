#' main UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_main_ui <- function(id) {
    ns <- NS(id)
    shiny.quartz::QCard(
        title = "main",
    )
}


#' main Server Funciton
#'
#' @noRd
mod_main_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
    })
}

