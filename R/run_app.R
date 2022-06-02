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
        "",
        shiny.quartz::VStack(
            mod_main_ui("main"),
            shiny.quartz::AcknowledgementCard(
                authors =
                 list(
                    list(name = "Timothy L. Wiemken",
                        src = "https://media-exp1.licdn.com/dms/image/C4E03AQE4JkLxHi1g7A/profile-displayphoto-shrink_400_400/0/1603549767246?e=1655337600&v=beta&t=mGGA7RxgQvdi0yIvbwiOJXiJ0uvM_YAm0-UuybyOp8g"),
                    list(name = "Jacob A. Clarke",
                        src = "https://www.jacobaclarke.com/headshot_new_cropped.jpg",
                        href = "https://www.jacobaclarke.com/"),
                    list(name = "Christopher Prener")
                )
            )
        )
    )
}



#' @export
run_app <- function() {
    ggquartz::apply_theme()

    shiny::shinyApp(
        server = app_server,
        ui = app_ui,
    )
}