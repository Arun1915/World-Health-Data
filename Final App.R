#App design
library(shiny)
library(bslib)

#Plotting
library(ggplot2)
library(plotly)

#Dataframe manipulation
library(dplyr)
library(tidyr)

#Distribution Analysis
library(psych)

#  DATA 
who_data<- readRDS("who_data.rds")

ui <- page_navbar(
  title = "WHO Data Explorer", fillable = FALSE, bg = "#0B3C5D", inverse = TRUE,
  theme = bs_theme(version = 5, bootswatch = "flatly", bg = "#F6FAFD", fg = "#1D2D3E",
                   primary = "#009ADE", secondary = "#0B3C5D"),
  
  # Tab 1: Country Profile 
  nav_panel("Country Profile",
            layout_sidebar(fillable = FALSE,
                           sidebar = sidebar(bg = "#E6F2FB",
                                             selectInput("tb1_country1", "Country", who_data$Countries, selected = "India"),
                                             checkboxInput("compare", "Compare with another country"),
                                             conditionalPanel("input.compare",
                                                              selectInput("tb1_country2", "Comparison country", who_data$Countries, selected = "Japan"))
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
                               selectInput("page", "Page", 1:ceiling(nrow(who_data) / 10)),
                               div(class = "tb1_table-responsive", tableOutput("tb1_table"))
                             )
                           )
            )
            ),
  
  #Tab 2
  nav_panel("Scatter Plot",{
    layout_sidebar(
      sidebar = sidebar(
        selectInput("t2_x_axis", "X-Axis", colnames(who_data)[2:length(colnames(who_data))]),
        selectInput("t2_y_axis", "Y-Axis", colnames(who_data)[2:length(colnames(who_data))]),
        sliderInput("t2_deg", "Degree of Polynomial", value = 1, min=1, max = 10),
        actionButton("t2_simulate", "Plot")
      ),
      plotlyOutput("t2_plot"),
      verbatimTextOutput("t2_textO")
    )
  }),
  
  #Tab 3
  nav_panel( "Column Distribution", {
    layout_sidebar(
      sidebar = sidebar(
        selectInput("t3_column", "Choose Column", colnames(who_data)[2:length(colnames(who_data))]),
        actionButton("t3_simulate","Plot"),
      ),
      fluidRow(
        column(6,
               plotOutput("t3_pdf")               
        ),
        column(6,
               plotOutput("t3_boxPlt")               
        )
      ),
      fluidRow(
        card(
          card_header("Summary Statistics"),
          card_body(
            tableOutput("t3_summ"),
            style = "overflow-x: auto; max-width: 100%;"
          )
        )
      )
    )
  }),
  
  nav_panel("Country Comparison",
            layout_sidebar(
              
              
              sidebar = sidebar(
                # Dropdown for Countries
                selectInput("t4_countries", "Select Countries:",
                            choices = who_data$Countries,
                            multiple = TRUE,
                            selected = head(who_data$Countries, 10)),
                
                # Dropdown for Columns
                selectInput("t4_columns", "Select Metrics:",
                            choices =  names(who_data)[2:ncol(who_data)],
                            multiple = TRUE,
                            selected =  names(who_data)[2:3])
              ),
              
              # Main content for layout_sidebar (replaces mainPanel)
              plotOutput("t4_bar_plot", height = "700px")
            )
  )
)

server <- function(input, output, session) {
  # Tab 1
  indicators <- setdiff(names(who_data), "Countries")
  percentile <- function(x) (rank(x, ties.method = "min", na.last = "keep") - 1) / (sum(!is.na(x)) - 1) * 100
  pct <- who_data
  pct[indicators] <- lapply(who_data[indicators], percentile)
  sel <- reactive(unique(c(input$tb1_country1, if (input$compare) input$tb1_country2)))
  
  output$tb1_country_table <- renderTable({
    df <- data.frame(Indicator = indicators)
    for (cn in sel()) df[[cn]] <- unlist(who_data[who_data$Countries == cn, indicators], use.names = FALSE)
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
    rows <- ((page - 1) * 10 + 1):min(page * 10, nrow(who_data))
    who_data[rows, c("Countries", indicators)]
  }, striped = TRUE, digits = 2)
  
  #Tab 2
  #Remove the rows which are NA only for the 2 columns selected
  #Return dataframe with countries, and the 2 columns
  t2_df = eventReactive(input$t2_simulate, {
    data_temp = who_data %>% filter(!is.na(.data[[input$t2_x_axis]]) & !is.na(.data[[input$t2_y_axis]]))
    dataframe = data_temp[, c("Countries", input$t2_x_axis, input$t2_y_axis)]
    colnames(dataframe)[2] = "X"
    colnames(dataframe)[3] = "Y"
    return(dataframe)
  })
  
  #To ensure plot is remade only on click of "Plot"
  t2_deg = eventReactive(input$t2_simulate, {
    return(input$t2_deg)
  })
  
  #Plot
  #aes(group=1) - to remove the grouping of points based on country.
  #scale_continuous functions to avoid overcrowding
  #need to convert all number columns to numeric to avoid tedious code
  output$t2_plot = renderPlotly({
    plot = ggplot(t2_df(), aes(X, Y, text = Countries))+
      geom_point() + scale_x_continuous(n.breaks = 5) + scale_y_continuous(n.breaks = 5) +
      geom_smooth(method="lm", aes(group = 1), formula = paste0("y ~ poly(x,",t2_deg(), ")"))
    
    ggplotly(plot, tooltip = c("text", "X", "Y"))
  })
  
  output$t2_textO = renderText({
    paste("[ The Correlation is: ",cor(t2_df()$X,t2_df()$Y),"]")
  })
  
  #Tab 3
  t3_df = eventReactive(input$t3_simulate, {
    data_temp = who_data %>% filter(!is.na(.data[[input$t3_column]]))
    df = data_temp[, c("Countries", input$t3_column)]
    colnames(df)[2] = "X"
    return(df)
  })
  output$t3_pdf = renderPlot({
    ggplot(t3_df(), aes(X, y = after_stat(density)))+ geom_histogram(color = "white", fill = "lightblue")+geom_density(color="red", linewidth = 3)
  })
  output$t3_boxPlt = renderPlot({
    ggplot(t3_df(), aes(y=X))+ geom_boxplot()
  })
  output$t3_summ = renderTable({
    describe(t3_df()[,2])
  })
  
  #Tab 4
  output$t4_bar_plot <- renderPlot({
    # Require at least one column and country to prevent errors 
    req(input$t4_columns, input$t4_countries)
    
    # Filter and prep the data 
    df <- who_data %>%
      filter(Countries %in% input$t4_countries) %>%
      select(Countries, all_of(input$t4_columns)) %>%
      # pivot_longer is required by ggplot to group multiple columns side-by-side
      pivot_longer(cols = -Countries, names_to = "Metric", values_to = "Value")
    
    # Create the plot
    ggplot(df, aes(x = Countries, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() + # Flips the chart so long country names are readable
      theme_minimal()
  })
}

shinyApp(ui, server)
