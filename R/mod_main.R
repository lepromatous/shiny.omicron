#' main UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @import shiny.quartz
#' @import shiny.mui
#' @import ggplot2
mod_main_ui <- function (id) {
    ns <- NS(id)
    TabContent <- function(name, ...) {
        conditionalPanel(
            ns = ns,
            condition = paste0("input.tabs == '", name,"'"),
            ...
        )
    }
    QCard(
        VStack(
            Tabs.shinyInput(
                ns("tabs"),
                value = "country",
                Tab(value = "country", label = "By Country"),
                Tab(value = "lineage", label = "By Lineage")
            ),
            TabContent("country",
                HStack(
                    QSelect.shinyInput(
                        ns("country"),
                        label = "Country",
                        options = make_options(shiny.covariants::country_list),
                        value = "USA"
                    )
                ),
                plotly::plotlyOutput(ns("proportions_plot")),
            ),
            TabContent("lineage",
                HStack(
                    QSelect.shinyInput(
                        ns("lineage"),
                        label = "Lineage",
                        options = make_options(shiny.covariants::variant_list),
                        value = "BA.1"
                    )
                ),
                plotly::plotlyOutput(ns("lineage_plot"), height = "1000px")
            )
        )
    )
}


#' main Server Funciton
#'
#' @noRd
mod_main_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$proportions_plot <- plotly::renderPlotly({
            req(input$country)
            plot_proportions(input$country)
        })
        output$lineage_plot <- plotly::renderPlotly({
            req(input$lineage)
            plot_lineage(input$lineage)
        })
    })
}

#' Plot Proportions
#' 
#' Plots the proportions of each variant of all variants for a given country over time.
#' 
#' @param countriez The country to filter by
#' @return a plotly plot
#' @import dplyr
plot_proportions <- function(countriez="USA"){
    (shiny.covariants::omicron_proportions %>%
        subset(country == countriez & omicron == T) %>%
        select(-c(country, omicron)) %>%
        # pivot wider and replace NA with 0 then make long again
        tidyr::pivot_wider(names_from = pango) %>%
        mutate(dplyr:::across(where(is.numeric), ~ replace(.x, is.na(.x), 0))) %>%
        tidyr::pivot_longer(
            -c(week_ending),
        ) %>%
        ggplot(aes(week_ending, value, fill = name)) +
        geom_bar(position = "dodge", stat = "identity") +
        labs(
            x = "\nTwo Week Period Ending",
            y = "Percent of All Strains \n",
            fill = "Pango Lineage"
        ) +
        scale_fill_brewer(palette = "Set1") +
        scale_x_date(date_breaks = "1 month") +
        theme(
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 90)
        )) %>%
        plotly::ggplotly()
}

#' Plot Lineage
#' plots a heatmap by country and date for a specific variant
#' @param lineage the omicron lineage to filter by
#' @return a plotly plot
plot_lineage <- function(lineage = "BA.1") {
    (omicron_proportions %>%
        subset(pango == lineage) %>%
        ggplot(aes(week_ending, country, fill = value)) +
            geom_tile(position = "dodge", stat = "identity") +
            labs(
            x = "\nTwo Week Period Ending",
            y = "",
            fill = paste0("Percent ", lineage, "\nof All Variants \n")
            ) +
            scico::scale_fill_scico(palette = "bilbao") +
            scale_x_date(date_breaks = "1 month") +
            theme(
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 90),
            axis.text.y = element_text(size = 5)
            )) %>%
            plotly::ggplotly()
}
