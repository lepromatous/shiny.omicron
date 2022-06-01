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

    LineageSelector <- QSelect.shinyInput(
                        ns("lineage"),
                        label = "Lineage",
                        options = make_options(shiny.omicron::variant_list),
                        value = "BA.1"
                    )
    QCard(
        VStack(
            Tabs.shinyInput(
                ns("tabs"),
                value = "country",
                Tab(value = "country", label = "By Country"),
                Tab(value = "map", label = "Maps"),
                Tab(value = "heatmap", label = "Heatmaps")
            ),
            TabContent("country",
                HStack(
                    QSelect.shinyInput(
                        ns("country"),
                        label = "Country",
                        options = make_options(shiny.omicron::country_list),
                        value = "USA"
                    )
                ),
                plotly::plotlyOutput(ns("proportions_plot")),
            ),
            TabContent("lineage",
                HStack(
                    LineageSelector
                ),
                plotly::plotlyOutput(ns("heatmap"), height = "1000px")
            ),
            TabContent("map",
                VStack(
                    QSelect.shinyInput(
                        ns("map_type"),
                        label = "Map Type",
                        options = make_options(
                            "By Variant",
                            "By Dominant Variant"
                        ),
                        value = "By Dominant Variant"
                    ),
                    conditionalPanel(condition = "input.map_type == 'By Variant'", ns = ns,
                        LineageSelector,
                    ),
                    ListSlider.shinyInput(
                        ns("date"), 
                        value = "2021-04-19",
                        options = shiny.omicron::dates_list,
                        animate = T,
                        animationStepSize = 1,
                        animationInterval = 2000,
                        markInterval = length(shiny.omicron::dates_list) / 3,
                        valueLabelDisplay = "auto")
                        ),
                plotly::plotlyOutput(ns("variant_map"))
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

        observe({
            req(input$tabs == "map")
            if(input$map_type == "By Variant") {
                options <- shiny.omicron::omicron_proportions %>%
                    subset(pango == input$lineage) %>%
                    .$week_ending %>%
                    unique() %>%
                    sort()
            } else {
                options <- shiny.omicron::dates_list
            }
            shiny.react::updateReactInput("date",
            options = options,
            markInterval = as.integer(length(options) / 3),
            session = session)
        })

        output$proportions_plot <- plotly::renderPlotly({
            req(input$country)
            req(input$tabs == "country")
            plot_proportions(input$country)
        })
        output$heatmap <- plotly::renderPlotly({
            req(input$lineage)
            req(input$tabs == "heatmaps")
            plot_heatmap_by_country(input$lineage)
        })

        debounced_date <- reactive(input$date) %>% debounce(100)
        output$variant_map <- plotly::renderPlotly({
            req(input$tabs == "map")
            req(input$lineage)
            req(debounced_date())
            if(input$map_type == "By Variant") {
                map_variant(pango = input$lineage, weekz = debounced_date())
            } else {
                map_dominant_variant(weekz = debounced_date())
            }
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
    shiny.omicron::omicron_proportions %>%
        subset(country == countriez & omicron == T) -> filtered

    req(nrow(filtered) > 0)

    (filtered %>%
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
        # scale_fill_brewer(palette = "Set1") +
        scale_x_date(date_breaks = "1 month") +
        theme(
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 90)
        )) %>%
        plotly::ggplotly()
}

#' Heatmap Theme
#' this is the base theme to apply to all of the heatmaps
heatmap_theme <- \(lineage) list(
    geom_tile(position = "dodge", stat = "identity"),
    labs(
        x = "\nTwo Week Period Ending",
        y = "",
        fill = paste0("Percent ", lineage, "\nof All Variants \n")
    ),
    scico::scale_fill_scico(palette = "bilbao"),
    scale_x_date(date_breaks = "1 month"),
    theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 5)
    )
)

#' Plot Heatmap by Country
#' plots a heatmap by country and date for a specific variant
#' @param lineage the omicron lineage to filter by
#' @return a plotly plot
plot_heatmap_by_country <- function(lineage = "BA.1") {
    (omicron_proportions %>%
        subset(pango == lineage) %>%
        ggplot(aes(week_ending, country, fill = value)) +
            heatmap_theme(lineage)) %>%
            plotly::ggplotly()
}
#' Prepare Maps
#' Merge dataframe with world geometries on country column
#' @param .data data to merge with country map
#' @import sf
prep_maps <- function(.data) {
    shiny.omicron::basemap %>% merge(.data, all.x = T)
}

#' Theme Map
#' Removes the axis from a plotly plot
theme_map <- \() list(
            theme(
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom"
            ))

#' Map Variant
#' map based on a selected variant for a given week
#' @param pangoz variant lineage
#' @param weekz week ending
#' @param quantile whether or not to split data into quintiles
#' @return a plotly plot
#' @import ggplot2
#' @import RColorBrewer
map_variant <- function(pangoz, weekz, quintile = TRUE) {
    df <- shiny.omicron::omicron_proportions %>%
        subset(pango == pangoz) %>%
        subset(week_ending == weekz) %>%
        prep_maps()

    # prep breaks
    if (quintile == TRUE) {
        df$map_breaks <- cut(df$value, breaks = c(0, 20, 40, 60, 80, 100),
                            include.lowest = TRUE, dig.lab = 3)
        df$map_breaks <- fct_explicit_na(df$map_breaks)
        levels(df$map_breaks) <- c(
            "19.9 or less",
            "20-39.9",
            "40-59.9",
            "60-79.9",
            "80 or greater",
            "No Data Available")
    }

    # select palette based on pango
    if (pangoz == "BA.1") {pals <- "Reds"}
    else if (pangoz == "BA.2") {pals <- "Blues"}
    else if (pangoz == "BA.2.12.17") {pals <- "Greens"}
    else if (pangoz == "BA.4") {pals <- "Purples"}
    else if (pangoz == "BA.5") {pals <- "Oranges"}
    else {pals <- "Reds"}

    len <- length(unique(df$map_breaks))

    if (len <= 3) {
        pals <- brewer.pal(n = 3, name = pals)
        pals <- pals[1:len - 1]
        pals <- c(pals, "grey70")
    } else if (len == 4) {
        pals <- brewer.pal(n = 3, name = pals)
        pals <- c(pals, "grey70")
    } else if (len >= 5){
        pals <- brewer.pal(n = len - 1, name = pals)
        pals <- c(pals, "grey70")
    }

    # create map
    if (quintile == TRUE) {
        plot <- ggplot() +
                geom_sf(data = df, mapping = aes(fill = map_breaks)) +
                theme_map() +
                scale_fill_manual(values = pals, name = "% of Cases")
    } else {
        plot <- ggplot() +
            geom_sf(data = df, mapping = aes(fill = value)) +
            theme_map() +
            scale_fill_distiller(
                palette = pals,
                na.value = "grey70",
                name = "% of Cases",
                direction = 1
            )
    }
    return(plotly::ggplotly(plot))
}

#' Map Dominan Variant
#' map based on the most prevalent variant for a given week
#' @param weekz week ending
#' @return a plotly plot
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom dplyr group_by distinct
map_dominant_variant <- function(weekz) {
    # prepare data
    df <- shiny.omicron::omicron_proportions %>%
        group_by(country, week_ending) %>%
        subset(value == max(value)) %>%
        subset(week_ending == weekz) %>%
        group_by(country) %>%
        distinct(country, .keep_all = TRUE) %>%
        prep_maps()

    (ggplot() +
        geom_sf(data = df, mapping = aes(fill = pango)) +
        theme_map()) %>%
        plotly::ggplotly()
}