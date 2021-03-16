# SHINY APP DEMO
# standalone file
# ENSAE course on Shiny, March 15th 2021

# This is an example of Shiny web application
# analysing census population Data in the United States
# from: 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv'

# Specifically,
# in Onglet "General statistics", the user has access to some details on specified state
# in Onglet "Compare states development", the user may select several states and compare all major indicators
# in Onglet "Map", the user can take a look at for any specified year the indicator of their choosing of the overall United States

# load libraries (make sure all are installed before running all)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(DT)
library(tigris)
library(leaflet)
library(tidyverse)

# load .csv dataset
my_dataset = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv")
str(my_dataset)

# ...
# unique(sub('20.*$', '', colnames(test)))
variable_dict <- matrix(data = c(
    'RNETMIG', 'Net migration rate',
    'RDOMESTICMIG', 'Net domestic migration rate',
    'RINTERNATIONALMIG', 'Net internation migration rate',
    'RNATURALINC', 'Natural increase rate',
    'RDEATH', 'Death rate',
    'RBIRTH', 'Birth rate',
    'RESIDUAL', 'Residual',
    'NETMIG', 'Net migration',
    'DOMESTICMIG', 'Net domestic migration',
    'INTERNATIONALMIG', 'Net international migration',
    'NATURALINC', 'Natural increase',
    'DEATHS', 'Deaths', 
    'BIRTHS', 'Births',
    'NPOPCHG', 'Change in resident total population',
    'POPESTIMATE', 'Resident total population estimate',
    'ESTIMATESBASE2010', 'Resident total population estimates base',
    'CENSUS2010POP', 'Resident total Census 2010 population'),
    byrow = TRUE, ncol = 2)
colnames(variable_dict) <- c('name', 'label')
variable_dict = cbind(variable_dict, 'name_2019' = paste0(variable_dict[,1], "2019"))
VARS_OF_2010 <- c('ESTIMATESBASE2010', 'CENSUS2010POP')
variable_dict[variable_dict[,'name'] %in% VARS_OF_2010 == TRUE,'name_2019'] <- variable_dict[variable_dict[,'name'] %in% VARS_OF_2010 == TRUE,'name']

# names
NON_STATES_NAMES <- my_dataset$NAME[1:5]
STATES_NAMES <- my_dataset$NAME[-(1:5)]

# prepare map informations
UnitedStates <- states(cb=T)
UnitedStates <- merge(x = UnitedStates, y = my_dataset, by.x = 'NAME', by.y = 'NAME')
map_zero <- UnitedStates %>% 
    leaflet() %>% 
    addTiles() %>% 
    fitBounds(lng1 = -120, lat1 = 20, lng2 = -75, lat2 = 50)

# User interface ================

ui <- dashboardPage(
    header = dashboardHeader(title = 'My Dashboard'), 
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("General statistics", tabName = "tab_general_statistics", icon = icon("dashboard")),
            menuItem("States comparison", tabName = "tab_states_comparison", icon = icon("th")),
            menuItem("US Map", tabName = "tab_us_map", icon = icon("dashboard"))
        )
    ), 
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "tab_general_statistics", class = "active",
                    fluidRow(
                        box(width = 6, background = 'blue',
                            selectInput(inputId = "state_selected", label = "State Selected", choices = STATES_NAMES)
                        ),
                        valueBoxOutput(outputId = "state_selected_mini_info", width = 6)
                    ),
                    fluidRow(
                        box(width = 12,
                            DTOutput("general_statistics_by_state")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "tab_states_comparison", class = "active",
                    fluidRow(
                        useShinyalert(),
                        box(
                            selectizeInput(inputId = "states_to_compare", options = list(maxItems = 4), label = "States (max 4)", choices = STATES_NAMES),
                            selectizeInput(inputId = "indicators_to_compare", options = list(maxItems = 5), label = "Indicators (max 5)", choices = variable_dict[variable_dict[,'name'] %in% VARS_OF_2010 == FALSE,'name']),
                            helpText("Note: start typing in to get suggestions.\nTry to break the app by selecting no state or indicator, see what happens :)")
                        ),
                        box(
                            checkboxInput(inputId = "dont_use_facets_for_states", label = "Overlay the time series ?", value = FALSE)
                        ), 
                        plotOutput(outputId = "plot_comparison_indicator_between_states")
                    )
            ),
            
            tabItem(tabName = 'tab_us_map', class = "active",
                    selectInput(inputId = "indicator_us_map", label = "Indicator", choices = colnames(my_dataset)[-(1:5)]),
                    leafletOutput(outputId = "us_map")
            )
        )
    )
)

# Server ================

server <- function(input, output, session) {
    
    # tab "General statistics"
    output$general_statistics_by_state <- renderDT({
        # get state specific data + US + its region
        region_name <- as.character(my_dataset$NAME[my_dataset$REGION[my_dataset$NAME == input$state_selected]])
        o <- my_dataset %>% filter(NAME %in% c(input$state_selected, "United States", region_name))
        # transpose the extract
        o <- o %>% column_to_rownames(var = 'NAME')
        o <- data.frame(t(o[3:1,-(1:5)]))
        o <- o %>% rownames_to_column()
        # merge with variable dictionary
        o <- merge(x = data.frame(variable_dict, stringsAsFactors = F),
                   y = o, 
                   by.x = 'name_2019', by.y = 'rowname')
        # add 2019 label when appropriate
        o$label[o$name %in% VARS_OF_2010 == FALSE] <- paste0(o$label[o$name %in% VARS_OF_2010 == FALSE], " (19)")
        # round values
        rate_info <- grepl(pattern = "rate", o$label)
        o[rate_info,-(1:3)] <- round(o[rate_info,-(1:3)], 2)
        o[rate_info == FALSE,-(1:3)] <- round(o[rate_info == FALSE,-(1:3)])
        # return nice, sorted table
        o <- o[order(o$label), -c(1:2)]
        colnames(o)[1] <- 'Indicator'
        return(datatable(o, rownames = FALSE, options = list(pageLength = 15, dom = 't')))
    })
    output$state_selected_mini_info <- renderValueBox({
        valueBox(
            "Population in 2019",
            my_dataset$POPESTIMATE2019[my_dataset$NAME == input$state_selected],
            icon = icon("credit-card")
        )
    })
    
    check_no_country_or_indicator_selected <- reactive({
        length(input$states_to_compare) == 0 | length(input$indicators_to_compare) == 0
    })
    
    observe({
        req(length(input$states_to_compare) == 0 | length(input$indicators_to_compare) == 0)
        # Show a modal when the button is pressed
        l0 = (length(input$states_to_compare) == 0)
        l1 = (length(input$indicators_to_compare) == 0)
        msg = case_when(
            l0 & l1 ~ "Please select at least one state and one indicator !",
            l1 ~ "Please select (at least) one indicator",
            l0 ~ "Please select (at least) one state"
        )
        print(l0)
        print(l1)
        print(msg)
        shinyalert("Oops!", msg, type = "error")
    })
    
    # tab "States comparison"
    # input$state_to_compare_1
    # input$state_to_compare_2
    output$plot_comparison_indicator_between_states <- renderPlot({
        
        if(check_no_country_or_indicator_selected()){
            return(NULL)
        }
        
        # get state specific data 
        o <- my_dataset %>% filter(NAME %in% input$states_to_compare)
        # transpose the extract
        o <- o %>% column_to_rownames(var = 'NAME')
        o <- data.frame(t(o[,-(1:5)]))
        o <- o %>% rownames_to_column()
        # select on the indicator of interest
        p <- paste0(input$indicators_to_compare, collapse = '|')
        o <- o[grepl(pattern = p, x = o$rowname),]
        o$Year <- as.numeric(gsub(pattern = p, replacement = '', x = o$rowname))
        o$Indicator <- gsub(pattern = '20.*', replacement = '', x = o$rowname)
        o <- o %>% dplyr::select(-rowname) %>% pivot_longer(cols = -c(Year, Indicator))
        if(input$dont_use_facets_for_states == FALSE){
            gg = ggplot(data = o) +
                aes(x = Year, y = value, col = Indicator) +
                geom_point(cex = 3, col = 'black') +
                geom_line() +
                ylab("Value") +
                ggtitle("Indicator evolution over 2010-2019") +
                facet_wrap(.~name)
            print(gg)
        }else{
            gg = ggplot(data = o) +
                aes(x = Year, y = value, col = Indicator, pch = name) +
                geom_point(cex = 3, col = 'black') +
                geom_line() +
                ylab("Value") +
                ggtitle("Indicator evolution over 2010-2019") +
                labs(pch = "State")
            print(gg)
            
        }
    })
    
    output$us_map <- renderLeaflet({
        # indicator_us_map
        nom_variable <- input$indicator_us_map
        pal <- colorNumeric(palette = "Reds", domain = UnitedStates[[nom_variable]])
        cmd <- paste0("
map_zero %>% 
    addPolygons(color = 'white', 
                fillColor =~pal(", nom_variable, "), 
                popup=~paste0(NAME, ' - ", nom_variable, " : ', round(", nom_variable, ",2)))
")
        eval(parse(text = cmd))
    })    
    
}

# Run ================

shinyApp(ui, server)
