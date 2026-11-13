library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

#  DATA 
who        <- readRDS("who_data.rds")
indicators <- setdiff(names(who), "Countries")
countries  <- sort(who$Countries)

# percentile (0-100) 


n_pages       <- ceiling(nrow(who) / 10)

ui <- page_navbar(
  title = "WHO Data Explorer", fillable = FALSE, bg = "#0B3C5D", inverse = TRUE,
  theme = bs_theme(version = 5, bootswatch = "flatly", bg = "#F6FAFD", fg = "#1D2D3E",
                   primary = "#009ADE", secondary = "#0B3C5D"),
  
  # Tab 1: Country Profile 
  nav_panel("Country Profile",
            layout_sidebar(fillable = FALSE,
                           sidebar = sidebar(bg = "#E6F2FB",
                                             selectInput("tb1_country1", "Country", countries, selected = "India"),
                                             checkboxInput("compare", "Compare with another country"),
                                             conditionalPanel("input.compare",
                                                              selectInput("tb1_country2", "Comparison country", countries, selected = "Japan"))
                           ),
                           fluidRow(
                             column(6,
                                    h4("Selected country data"),
                                    tableOutput("tb1_country_table")
                                    ),
                             column(6,
                                    div(h4("Percentile profile"), plotOutput("radar", height = "500px")))
                             ),
                         card(
                           card_header("World data (all countries)"),
                           card_body(
                           selectInput("page", "Page", 1:n_pages),
                           div(class = "tb1_table-responsive", tableOutput("tb1_table"))
                           )
                           )
            ))
  
  
)

server <- function(input, output, session) {
  percentile <- function(x) (rank(x, ties.method = "min", na.last = "keep") - 1) / (sum(!is.na(x)) - 1) * 100
  pct <- who
  pct[indicators] <- lapply(who[indicators], percentile)
  sel <- reactive(unique(c(input$tb1_country1, if (input$compare) input$tb1_country2)))
  
  # Tab 1
  output$tb1_country_table <- renderTable({
    df <- data.frame(Indicator = indicators)
    for (cn in sel()) df[[cn]] <- unlist(who[who$Countries == cn, indicators], use.names = FALSE)
    df
  }, striped = TRUE, digits = 2)
  
  # Tab 1:radar char
  output$radar <- renderPlot({
    d <- do.call(rbind, lapply(sel(), function(cn)
      data.frame(Country    = cn,
                 Indicator  = factor(indicators, levels = indicators),
                 Percentile = unlist(pct[pct$Countries == cn, indicators], use.names = FALSE))))
    pal <- setNames(c("#009ADE", "#F26B3A")[seq_along(sel())], sel())
    
    ggplot(d, aes(Indicator, Percentile, group = Country, colour = Country, fill = Country)) +
      geom_polygon(alpha = 0.2, linewidth = 1) +
      geom_point(size = 2) +
      coord_polar() +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_discrete(labels = scales::label_wrap(14)) +
      scale_colour_manual(values = pal) +
      scale_fill_manual(values = pal) +
      labs(x = NULL, y = "Percentile (0-100)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", axis.text.x = element_text(size = 8))
  })
  
  #  Tab 1: world data tb1_table
  output$tb1_table <- renderTable({
    page <- as.numeric(input$page)
    rows <- ((page - 1) * 10 + 1):min(page * 10, nrow(who))
    who[rows, c("Countries", indicators)]
  }, striped = TRUE, digits = 2)
  
  # Tab 2: data for the scatter plot
  plot_data <- reactive({
    validate(need(input$var_x != input$var_y, "Please select two different indicators."))
    extra <- setdiff(c(input$var_color, input$var_size), "None")
    d <- who[, unique(c("Countries", input$var_x, input$var_y, extra))]
    d <- d[complete.cases(d), ]
    d$plot_x <- d[[input$var_x]]
    d$plot_y <- d[[input$var_y]]
    if (input$var_color != "None") d$plot_c <- d[[input$var_color]]
    if (input$var_size  != "None") d$plot_s <- d[[input$var_size]]
    d
  })
  
  # hide the helper columns (plot_x, plot_y, ...) in tables
  shown <- function(d) d[, !grepl("^plot_", names(d)), drop = FALSE]
  
 
}

shinyApp(ui, server)