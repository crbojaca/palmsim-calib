library(shiny)
library(shinyjs)
library(shinyWidgets)
library(echarts4r)
library(reticulate)
library(tools)
library(DT)

# Load the model
os <- import('os')
pd <- import('pandas')
sys <- import('sys')

# From palmsim import PalmField
palsim <- import('palmsim')
PalmField <- palsim$PalmField

# Function to generate tables
table_gen <- function(df, name){
  datatable(
    df %>%
      select(date, YAP, starts_with(name)),
    options = list(
      pageLength = 10,
      dom = 'Brtip',
      scrollX = TRUE
    ),
    rownames = FALSE
  ) %>%
    formatRound(-1, digits = 2)
}

# Define UI for application
ui <- fluidPage(
  
  useShinyjs(),
  
  # Application title
  titlePanel("PALMSIM Simulator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Open box for input file
      fileInput("climate_file", "Choose climate file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      h5('Climate file variables:'),
      
      verbatimTextOutput('clim_vars', placeholder = TRUE),
      
      tags$hr(),
      
      dateInput('dates', 'Starting simulation date:',
                value = Sys.Date()),
      
      # Select the finishing date of the simulation
      dateInput('dates_end', 'Finishing simulation date:',
                value = Sys.Date()),
      
      tags$hr(),
      
      # Select the planting density
      sliderInput('density', 'Planting density (palms/ha):',
                  min = 100, max = 200, value = 143),
      
      tags$hr(),
      
      # Select the period frequency
      sliderInput('dt', 'Time step (days):',
                  min = 1, max = 30, value = 10),
      
      # Select latitude
      sliderInput('latitude', 'Latitude:',
                  min = -23.5, max = 23.5, step = 0.25, value = 0),
      
      tags$hr(),
      
      # Define soil depth
      sliderInput('soil_depth', 'Soil depth (m):',
                  min = 0.5, max = 2, value = 1),
      
      tags$hr(),
      
      # Choose soil texture
      selectInput('soil_texture', 'Soil texture:',
                  choices = sort(c('clay', 'sand', 'loamy sand', 'sandy loam', 'loam',
                                   'silty loam', 'sandy clay loam', 'clay loam',
                                   'silty clay loam', 'sandy clay', 'silty clay'))),
      
      tags$hr(),
      
      # Buttons to run and save simulation
      disabled(actionButton('run_sim', 'Run simulation')),
      
      fluidRow(br()),
      
      disabled(downloadButton('save_sim', 'Save simulation'))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Create a tab called weather
      tabsetPanel(
        tabPanel('General', 
                 fluidRow(br()),
                 echarts4rOutput('general_plot', height = '800px'),
                 DTOutput('general_table')
        ),
        tabPanel('Generative', 
                 fluidRow(br()),
                 echarts4rOutput('generative_plot_1'),
                 column(6, echarts4rOutput('generative_plot_2')),
                 column(6, echarts4rOutput('generative_plot_3')),
                 column(12, echarts4rOutput('generative_plot_4')),
                 DTOutput('generative_table')
        ),
        tabPanel('Assimilates', 
                 fluidRow(br()),
                 echarts4rOutput('assimilates_plot_1'),
                 echarts4rOutput('assimilates_plot_2'),
                 echarts4rOutput('assimilates_plot_3'),
                 DTOutput('assimilates_table')
        ),
        tabPanel('Fronds',
                 fluidRow(br()),
                 column(6, echarts4rOutput('fronds_plot_1')),
                 column(6, echarts4rOutput('fronds_plot_2')),
                 h1(' '),
                 column(6, echarts4rOutput('fronds_plot_3')),
                 column(6, echarts4rOutput('fronds_plot_4')),
                 DTOutput('fronds_table')
        ),
        tabPanel('Roots', 
                 fluidRow(br()),
                 column(6, echarts4rOutput('roots_plot_1')),
                 column(6, echarts4rOutput('roots_plot_2')),
                 DTOutput('roots_table')
        ),
        tabPanel('Trunk', 
                 fluidRow(br()),
                 column(6, echarts4rOutput('trunk_plot_1')),
                 column(6, echarts4rOutput('trunk_plot_2')),
                 column(6, echarts4rOutput('trunk_plot_3')),
                 column(6, echarts4rOutput('trunk_plot_4')),
                 DTOutput('trunk_table')
        ),
        tabPanel('Soil', 
                 fluidRow(br()),
                 echarts4rOutput('soil_plot_1'),
                 echarts4rOutput('soil_plot_2'),
                 DTOutput('soil_table')
        ),
        tabPanel('Weather', 
                 fluidRow(br()),
                 echarts4rOutput('weather_plot_1'),
                 echarts4rOutput('weather_plot_3'),
                 column(6, echarts4rOutput('weather_plot_2')),
                 column(6, echarts4rOutput('weather_plot_4')),
                 DTOutput('weather_table')
        )
      )
    )
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
)

# Define server logic
server <- function(input, output, session){
  
  # Return the requested dataset
  df_climate <- reactive({
    req(input$climate_file)
    file <- input$climate_file
    file$datapath <- gsub("\\\\", "/", file$datapath)
    py_run_string("import pandas as pd")
    py_run_string(paste("weather = pd.read_csv('", file$datapath, "', index_col = 'Date')", sep = ""))
    py_run_string("weather.index = pd.to_datetime(weather.index)")

    return(py$weather)
  })
  
  val_date <- reactive({
    req(input$dates)
    input$dates
  })
  
  end_date <- reactive({
    req(input$dates_end)
    input$dates_end
  })
  
  # Enable run_sim button when climate file is uploaded
  observe({
    if(!is.null(input$climate_file)){
      shinyjs::enable('run_sim')
    }
  })
  
  # Update radio buttons to select the column to display
  observe({
    output$clim_vars <- renderText(names(df_climate()) %>% paste(collapse = ', '))
  })
  
  # Update slider to select dates from climate file
  observe({
    updateDateInput(session, 'dates',
                    value = min(rownames(df_climate())))
  })
  
  observe({
    updateDateInput(session, 'dates_end',
                    value = max(rownames(df_climate())))
  })
  
  ##### Run the simulation #####
  runsim <- eventReactive(input$run_sim, {
    
    # Configure the simulation
    py_run_string("from palmsim import PalmField")
    py_run_string(paste0("pf = PalmField(year_of_planting = ", as.integer(year(val_date())), 
                    ", month_of_planting = ", as.integer(lubridate::month(val_date())), 
                    ", day_of_planting = ", as.integer(day(val_date())),
                    ", planting_density = ", as.integer(input$density),
                    ", dt = ", as.integer(input$dt), 
                    ", latitude = ", input$latitude,
                    ", soil_depth = ", input$soil_depth,
                    ", soil_texture_class = '", input$soil_texture, "')"))
    
    # Couple the weather data
    py_run_string("pf.weather.radiation_series = weather['solar (MJ/m2/day)']")
    py_run_string("pf.weather.rainfall_series = weather['precip (mm/day)']")
    py_run_string("pf.weather.temperature_series = weather['temperature (degC)']")
    py_run_string("pf.weather.humidity_series = weather['humidity (%)']")
    
    # Run the simulation
    py_run_string(paste0("df = pf.run(duration = ",
                               as.integer(as.numeric(difftime(ymd(end_date()), ymd(val_date()), units = 'days'))), ")"))
    
    df <- py$df
      
    df$date <- ymd(df$date)
    
    df$YAP <- df$YAP + (yday(df$date) / 365)
    
    return(df)
  })
  
  ##### Save the simulation #####
  observeEvent(input$run_sim, {
    shinyjs::enable('save_sim')
  })
  
  output$save_sim <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), '_sim_', 
             'clim_', file_path_sans_ext(input$climate_file), '_',
             'ini_', df_climate()$Date, '_',
             'lat_', input$latitude, '.csv')
    },
    content = function(file) {
      write_csv(runsim(), file)
    }
  )
  
  ##### PLOTS #####
  output$general_plot <- renderEcharts4r({
    runsim() %>%
      mutate(mass_total = round(mass_total, 1),
             mass_vegetative = round(mass_vegetative, 1),
             mass_generative = round(mass_generative, 1),
             `FFB_production (kg/ha/yr)` = round(`FFB_production (kg/ha/yr)`, 1)) %>%
      e_charts(YAP) %>%
      e_area(mass_total, x_index = 1, y_index = 1) %>%
      e_area(mass_vegetative, x_index = 1, y_index = 1) %>%
      e_area(mass_generative, x_index = 1, y_index = 1) %>%
      e_line(`FFB_production (kg/ha/yr)`) %>%
      e_x_axis(gridIndex = 1, name = 'Years after planting') %>%
      e_y_axis(gridIndex = 0, name = 'Mass (kg/ha)') %>%
      e_y_axis(gridIndex = 1, name = 'FFB (kg/ha/yr)') %>%
      e_grid(height = '35%') %>%
      e_grid(height = '35%', top = '50%') %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider')
  })
  
  ##### Generative #####
  output$generative_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`generative_CPO_production (kg_DM/ha/day)`) %>%
      e_line(`generative_EFB_production (kg_DM/ha/day)`) %>%
      e_line(`generative_PKO_production (kg_DM/ha/day)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('generative') %>%
      e_connect_group('generative')
  })
  
  output$generative_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`generative_bunch_count_daily (1/ha/day)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('generative') %>%
      e_connect_group('generative')
  })
  
  output$generative_plot_3 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`generative_bunch_weight (kg)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('generative') %>%
      e_connect_group('generative')
  })
  
  output$generative_plot_4 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`generative_female_fraction (1)`) %>%
      e_line(`generative_inflorescence_abortion_fraction (1)`) %>%
      e_line(`generative_bunch_failure_fraction (1)`) %>%
      e_line(`generative_stress_index (1)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('generative') %>%
      e_connect_group('generative')
  })
  
  ##### Assimilates #####
  output$assimilates_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`assimilates_assim_produced (kg_CH2O/ha/day)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('assimilates') %>%
      e_connect_group('assimilates')
  })
  
  output$assimilates_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`assimilates_assim_growth_fronds (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_growth_roots (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_growth_trunk (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_growth_generative (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_growth_total (kg_CH2O/ha/day)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('assimilates') %>%
      e_connect_group('assimilates')
  })
  
  output$assimilates_plot_3 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`assimilates_assim_maintenance_fronds (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_maintenance_roots (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_maintenance_trunk (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_maintenance_generative (kg_CH2O/ha/day)`) %>%
      e_line(`assimilates_assim_maintenance_total (kg_CH2O/ha/day)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('assimilates') %>%
      e_connect_group('assimilates')
  })
  
  ##### Fronds #####
  output$fronds_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`fronds_leaf_area_index (1)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('fronds') %>%
      e_connect_group('fronds')
  })
  
  output$fronds_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`fronds_leaf_area_per_palm (m2)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('fronds') %>%
      e_connect_group('fronds')
  })
  
  output$fronds_plot_3 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`fronds_count_per_palm (1/palm)`) %>%
      e_line(`fronds_fronds_goal_count_per_palm (1/palm)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('fronds') %>%
      e_connect_group('fronds')
  })
  
  output$fronds_plot_4 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`fronds_specific_leaf_area (cm2/g_DM)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('fronds') %>%
      e_connect_group('fronds')
  })
  
  ##### Roots #####
  output$roots_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`roots_mass (kg_DM/ha)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('roots') %>%
      e_connect_group('roots')
  })
  
  output$roots_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`roots_mass_per_palm (kg_DM/palm)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('roots') %>%
      e_connect_group('roots')
  })
  
  ##### Trunk #####
  output$trunk_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`trunk_mass (kg_DM/ha)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('trunk') %>%
      e_connect_group('trunk')
  })
  
  output$trunk_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`trunk_mass_per_palm`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('trunk') %>%
      e_connect_group('trunk')
  })
  
  output$trunk_plot_3 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`trunk_volume (m3)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('trunk') %>%
      e_connect_group('trunk')
  })
  
  output$trunk_plot_4 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`trunk_density (kg/m3)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('trunk') %>%
      e_connect_group('trunk')
  })
  
  ##### Soil #####
  
  output$soil_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`soil_available_water (mm)`) %>%
      e_line(`soil_water_deficit (mm)`) %>%
      e_line(`soil_water_holding_capacity (mm)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('soil') %>%
      e_connect_group('soil')
  })
  
  output$soil_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`soil_drainage (mm/day)`) %>%
      e_line(`soil_evaporation (mm/day)`) %>%
      e_line(`soil_transpiration (mm/day)`) %>%
      e_line(`soil_evapotranspiration (mm/day)`) %>%
      e_line(`soil_rainfall (mm/day)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('soil') %>%
      e_connect_group('soil')
  })
  
  ##### Weather #####
  
  output$weather_plot_1 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`weather_radiation_extraterrestrial_daily (MJ/m2/d)`) %>%
      e_line(`weather_radiation (MJ/m2/day)`) %>%
      e_line(`weather_PAR (MJ/m2/day)`) %>%
      e_line(`weather_canopy_net_radiation_capture (MJ/m2/d)`) %>%
      e_line(`weather_canopy_net_radiation_capture_reference (MJ/m2/d)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('weather') %>%
      e_connect_group('weather')
  })
  
  output$weather_plot_2 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`weather_relative_humidity (1)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('weather') %>%
      e_connect_group('weather')
  })
  
  output$weather_plot_3 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`weather_temperature (degC)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('weather') %>%
      e_connect_group('weather')
  })
  
  output$weather_plot_4 <- renderEcharts4r({
    runsim() %>%
      e_charts(YAP) %>%
      e_line(`weather_windspeed (m/s)`) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + ' : ' + params.value[1].toFixed(2);
        }")) %>%
      e_datazoom(x_index = c(0, 1), type = 'slider') %>%
      e_group('weather') %>%
      e_connect_group('weather')
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##### TABLES #####
  output$general_table <- renderDT({
    datatable(
      runsim() %>%
        select(date, YAP, mass_total, mass_vegetative, mass_generative) %>%
        mutate(mass_total = round(mass_total, 2),
               mass_vegetative = round(mass_vegetative, 2),
               mass_generative = round(mass_generative, 2)),
      options = list(
        pageLength = 10,
        dom = 'Brtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(2:5, digits = 2)
  })
  
  output$generative_table <- renderDT({
    table_gen(runsim(), 'generative')
  })
  
  output$assimilates_table <- renderDT({
    table_gen(runsim(), 'assimilates')
  })
  
  output$fronds_table <- renderDT({
    table_gen(runsim(), 'fronds')
  })
  
  output$roots_table <- renderDT({
    table_gen(runsim(), 'roots')
  })
  
  output$trunk_table <- renderDT({
    table_gen(runsim(), 'trunk')
  })
  
  output$soil_table <- renderDT({
    table_gen(runsim(), 'soil')
  })
  
  output$weather_table <- renderDT({
    table_gen(runsim(), 'weather')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


