library('tidyverse')
library('tmap')
library('sf')
library('shiny')
library('shinydashboard')
library('scales')
library('plotly')
library('d3treeR')
library('treemap')
library('shinycustomloader')
library('DT')

##############################################################################################################

# Import data
hdb <- read_csv('data/aspatial/Resale Flat Prices 2000-2020 (data.gov).csv')

hdb_towns <- st_read(dsn = 'data/geospatial', layer = 'HDB_Towns_Boundary')
hdb_towns_3414 <- hdb_towns %>%
    st_transform(3414)
mpsz <- st_read(dsn = "data/geospatial", layer = "MP14_SUBZONE_NO_SEA_PL")

private_prop <- read_csv('data/aspatial/private_residential_property_transactions.csv')
private_res_data <- read_csv("data/aspatial/private_residential_with_latlon.csv")


# Data preparation

## HDB map
hdb_map_data <- hdb %>%
    separate(col = month, into = c('year', 'month'), sep = '-') %>%
    group_by(year, town, flat_type) %>%
    summarise(Mean = mean(resale_price),
              Median = median(resale_price),
              Minimum = min(resale_price),
              Maximum = max(resale_price),
              `Average Floor Area` = round(mean(floor_area_sqm), 0),
              `Average Price Per sqm` = round(mean(resale_price/floor_area_sqm), 0)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'flat_type', values_from = c('Mean', 'Median', 'Minimum', 'Maximum',
                                                          'Average Floor Area', 'Average Price Per sqm'),
                names_sep = ' - ') %>%
    left_join(hdb_towns_3414, by = c('town' = 'Town')) %>%
    st_as_sf(sf_column_name = 'geometry')

type_list <- c('1 ROOM', '2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM', 'EXECUTIVE', 'MULTI-GENERATION')
hdb_resale_colnames <- colnames(hdb_map_data)

## HDB home
hdb_home_data <- hdb %>%
    separate(col = month, into = c('year', 'month'), sep = '-') %>%
    filter(year >= 2015) %>%
    group_by(town, flat_type) %>%
    summarise(price = round(mean(resale_price), 0),
              size = round(mean(floor_area_sqm), 0),
              pricesqm = round(mean(resale_price/floor_area_sqm), 0)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'flat_type', values_from = c('price', 'size', 'pricesqm'))

ave_town_row <- subset(hdb_home_data, town == 'ANG MO KIO')
ave_town_cols <-  colnames(ave_town_row)[complete.cases(t(ave_town_row))][-1]
town_housing_types <- substring(ave_town_cols[grepl('size', ave_town_cols)], 6)
town_names <- hdb_home_data$town

## HDB price chart
hdb_line_data <- hdb %>%
  mutate(year = as.Date(paste(month, '-01', sep=""), '%Y-%m-%d')) %>%
  select(flat_type, town, year, resale_price)

## HDB treemap
hdb_treemap_data <- hdb %>% 
    separate(col = month, into = c('year', 'month'), sep = '-') %>%
    group_by(`town`, `flat_type`, `year`) %>%
    summarise(`Median Resale Price` = median(`resale_price`, na.rm = TRUE))

## Private property home
private_prop <- private_prop %>%
    mutate(`location` = case_when(
        `Postal District` == 1 ~ "D1 - Raffles Place, Cecil, Marina, People's Park",
        `Postal District` == 2 ~ "D2 - Anson, Tanjong Pagar",
        `Postal District` == 3 ~ "D3 - Queenstown, Tiong Bahru",
        `Postal District` == 4 ~ "D4 - Telok Blangah, Harbourfront",
        `Postal District` == 5 ~ "D5 - Pasir Panjang, Hong Leong Garden, Clementi New Town",
        `Postal District` == 6 ~ "D6 - High Street, Beach Road (part)",
        `Postal District` == 7 ~ "D7 - Middle Road, Golden Mile",
        `Postal District` == 8 ~ "D8 - Little India",
        `Postal District` == 9 ~ "D9 - Orchard, Cairnhill, River Valley",
        `Postal District` == 10 ~ "D10 - Ardmore, Bukit Timah, Holland Road, Tanglin",
        `Postal District` == 11 ~ "D11 - Watten Estate, Novena, Thomson",
        `Postal District` == 12 ~ "D12 - Balestier, Toa Payoh, Serangoon",
        `Postal District` == 13 ~ "D13 - Macpherson, Braddell",
        `Postal District` == 14 ~ "D14 - Geylang, Eunos",
        `Postal District` == 15 ~ "D15 - Katong, Joo Chiat, Amber Road",
        `Postal District` == 16 ~ "D16 - Bedok, Upper East Coast, Eastwood, Kew Drive",
        `Postal District` == 17 ~ "D17 - Loyang, Changi",
        `Postal District` == 18 ~ "D18 - Tampines, Pasir Ris",
        `Postal District` == 19 ~ "D19 - Serangoon Garden, Hougang, Punggol",
        `Postal District` == 20 ~ "D20 - Bishan, Ang Mo Kio",
        `Postal District` == 21 ~ "D21 - Upper Bukit Timah, Clementi Park, Ulu Pandan",
        `Postal District` == 22 ~ "D22 - Jurong",
        `Postal District` == 23 ~ "D23 - Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang",
        `Postal District` == 24 ~ "D24 - Lim Chu Kang, Tengah",
        `Postal District` == 25 ~ "D25 - Kranji, Woodgrove",
        `Postal District` == 26 ~ "D26 - Upper Thomson, Springleaf",
        `Postal District` == 27 ~ "D27 - Yishun, Sembawang",
        `Postal District` == 28 ~ "D28 - Seletar"
    ))

priv_home_data <- private_prop %>%
    mutate(price_per_unit = `Price ($)` / `No. of Units`,
           area_per_unit = `Area (Sqft)` / `No. of Units`) %>%
    group_by(location, Type) %>%
    summarise(price = round(mean(price_per_unit), 0),
              area = round(mean(area_per_unit)/10.764, 0),
              pricesqm = round(price/area, 0)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'Type', values_from = c('price', 'area', 'pricesqm'))

ave_district_row <- subset(priv_home_data, location == 'D1 - Raffles Place, Cecil, Marina, People\'s Park')
ave_district_cols <- colnames(ave_district_row)[complete.cases(t(ave_district_row))][-1]
district_housing_types <- substring(ave_district_cols[grepl('area', ave_district_cols)], 6)
district_names <- priv_home_data$location

## Private property price chart
priv_line_data <- private_prop %>%
    mutate(date = as.Date(paste("01-", `Date of Sale`, sep=""), '%d-%b-%y')) %>%
    group_by(date, Type, location) %>%
    mutate(price_per_unit = `Price ($)` / `No. of Units`) %>%
    summarise(price = round(mean(price_per_unit), 0)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'Type', values_from = c('price'))

district_bydate_df <- subset(priv_line_data, location == 'D1 - Raffles Place, Cecil, Marina, People\'s Park')
district_housing_types2 <- colnames(district_bydate_df[(which(sapply(district_bydate_df, function(x) all(is.na(x))) == FALSE)[-1][-1])])
district_names2 <- unique(priv_line_data$location)

## Private property treemap
priv_treemap_data <- private_res_data %>% 
  separate(col = `Date of Sale`, into = c('month','year'), sep = '-')%>%
  group_by(`Postal District`, `Type`, `year`) %>%
  summarise(`Total Unit Sold` = sum(`No. of Units`, na.rm = TRUE), 
            `Total Area` = sum(`Area (Sqft)`, na.rm = TRUE),
            `Median Unit Price ($ psqm)` = round(median(`Unit Price ($psf)`*10.764, na.rm = TRUE), 0))

## Private property map
private_res_sf <- private_res_data %>%
  filter(!is.na(lat) & `Street Name` != "TAI KENG PLACE, Singapore")%>%
  mutate(lat = as.numeric(lat),long = as.numeric(long),
         `Area (Sqm)` = (`Area (Sqft)` / 10.764),
         `Unit Price ($psm)` = round(`Price ($)`/`Area (Sqm)`,0)) %>%
  st_as_sf(coords = c("long","lat"),crs = 4326) %>%
  st_transform(crs = st_crs(mpsz))
private_res_sf$subzone <- as.character(mpsz$SUBZONE_N[unlist(st_intersects(private_res_sf$geometry, mpsz))])

## Postal district table
postal_district_table =  data.frame("Postal District" = c("D1 - Raffles Place, Cecil, Marina, People's Park",
                                                          "D2 - Anson, Tanjong Pagar",
                                                          "D3 - Queenstown, Tiong Bahru",
                                                          "D4 - Telok Blangah, Harbourfront",
                                                          "D5 - Pasir Panjang, Hong Leong Garden, Clementi New Town",
                                                          "D6 - High Street, Beach Road (part)",
                                                          "D7 - Middle Road, Golden Mile",
                                                          "D8 - Little India",
                                                          "D9 - Orchard, Cairnhill, River Valley",
                                                          "D10 - Ardmore, Bukit Timah, Holland Road, Tanglin",
                                                          "D11 - Watten Estate, Novena, Thomson",
                                                          "D12 - Balestier, Toa Payoh, Serangoon",
                                                          "D13 - Macpherson, Braddell",
                                                          "D14 - Geylang, Eunos",
                                                          "D15 - Katong, Joo Chiat, Amber Road",
                                                          "D16 - Bedok, Upper East Coast, Eastwood, Kew Drive",
                                                          "D17 - Loyang, Changi",
                                                          "D18 - Tampines, Pasir Ris",
                                                          "D19 - Serangoon Garden, Hougang, Ponggol",
                                                          "D20 - Bishan, Ang Mo Kio",
                                                          "D21 - Upper Bukit Timah, Clementi Park, Ulu Pandan",
                                                          "D22 - Jurong",
                                                          "D23 - Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang",
                                                          "D24 - Lim Chu Kang, Tengah",
                                                          "D25 - Kranji, Woodgrove",
                                                          "D26 - Upper Thomson, Springleaf",
                                                          "D27 - Yishun, Sembawang",
                                                          "D28 - Seletar"))


##############################################################################################################

# Define UI
ui <- dashboardPage(
    
    skin = 'black',
    
    dashboardHeader(title = 'PropLocate'),
    
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("dashboard")),
            menuItem("HDB Flats", tabName = "hdb", icon = icon("building"),
                     menuItem('Price Trend',
                              tabName = 'hdb_price_tab',
                              icon = icon('line-chart')),
                     menuItem('Price Overview',
                              tabName = 'hdb_treemap_tab',
                              icon = icon('square')),
                     menuItem('Price Map by HDB Town',
                              tabName = 'hdb_map_tab',
                              icon = icon('map-marker')),
                     menuItem('Data Explorer',
                              tabName = 'hdb_data_tab',
                              icon = icon('table'))
                     ),
            menuItem("Private Properties", tabName = "private", icon = icon("home"),
                     menuItem('Price Trend',
                              tabName = 'priv_price_tab',
                              icon = icon('line-chart')),
                     menuItem('Price Overview',
                              tabName = 'priv_treemap_tab',
                              icon = icon('square')),
                     menuItem('Price Map by Subzone',
                              tabName = 'priv_map_tab',
                              icon = icon('map-marker')),
                     menuItem('Data Explorer',
                              tabName = 'priv_data_tab',
                              icon = icon('table'))
                     )
        )
    ),
    
    ## Body
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),

        tabItems(
            
            ######################## HOME PAGE ########################
            tabItem(tabName = 'home',
                    h2('The First Click to Your Dream Home'),
                    fluidRow(
                        box(title = 'HDB Flats',
                            status = 'primary',
                            selectInput('home_select_town',
                                        'Select Town',
                                        choices = town_names),
                            selectInput('home_select_housing',
                                        'Select Housing Type',
                                        choices = town_housing_types),
                            valueBoxOutput('ave_hdb_price',
                                           width = 12),
                            valueBoxOutput('ave_hdb_size',
                                           width = 6),
                            valueBoxOutput('ave_hdb_price_sqft',
                                           width = 6),
                            width = 6),
                        box(title = 'Private Properties',
                            status = 'primary',
                            selectInput('home_select_district',
                                        'Select Postal District',
                                        choices = district_names),
                            selectInput('home_select_privtype',
                                        'Select Housing Type',
                                        choices = district_housing_types),
                            valueBoxOutput('ave_priv_price',
                                           width = 12),
                            valueBoxOutput('ave_priv_size',
                                           width = 6),
                            valueBoxOutput('ave_priv_price_sqft',
                                           width = 6),
                            width = 6)),
                    tags$small('Data for this visualisation was procured from data.gov.sg and ura.gov.sg'),
                    tags$br(),
                    tags$small('Last updated: 7 November 2020')
                    ),
            
            
            ######################## HDB PRICE CHART ########################
            tabItem(tabName = 'hdb_price_tab',
                    h3('Price Trend Over Time'),
                    p('Time-series trend of average prices by housing type or town. Use the menu to explore!'),
                    fluidRow(
                        box(withLoader(plotlyOutput("hdb_price_line"), type = "html", loader="loader1"),
                            width = 9),
                        box(
                          radioButtons("groupByInput", label = "Select Filter",
                                       choices = list("Housing Type" = colnames(hdb_line_data)[1], "Town"=colnames(hdb_line_data)[2]) ,
                                       selected = colnames(hdb_line_data)[1]),
                          
                          conditionalPanel(
                            condition = "input.groupByInput == 'flat_type'",
                            checkboxGroupInput("flatTypeInput", "Housing Type",
                                               choices = unique(hdb_line_data$flat_type),
                                               selected = unique(hdb_line_data$flat_type))
                          ),
                          
                          conditionalPanel(
                            condition = "input.groupByInput == 'town'",
                            selectizeInput("townInput", "Town",
                                           choices = unique(hdb_line_data$town),  
                                           selected=hdb_line_data$town[1], multiple =TRUE)
                          ),
                          
                          sliderInput("yearInput", "Year", min=2000, max=2020, 
                                      value=c(2015, 2020), sep=""),
                          
                          width = 3
                        )
                    )  
            ),
            
            
            ######################## HDB TREEMAP ########################
            tabItem(tabName = 'hdb_treemap_tab',
                    h3('Median Resale Prices by Town and Housing Type'),
                    p('Larger and darker boxes reflect a higher median price. Click in to explore!'),
                    fluidRow(
                      box(withLoader(d3tree2Output("treemap2"), type = "html", loader="loader1"),
                          width = 10),
                      box(selectInput("variable3",
                                      "Year",
                                      choices = c("2000"=2000, "2001"=2002, "2003"=2003, "2004"=2004, "2005"=2005,
                                                  "2006"=2006, "2007"=2008, "2009"=2009, "2010"=2010, "2011"=2011,
                                                  "2012"=2012, "2013"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
                                                  "2018"=2018, "2019"=2019, "2020"=2020),
                                      selected = 2020),
                          width = 2))
            ),
            
            
            ######################## HDB MAP ########################
            tabItem(tabName = 'hdb_map_tab',
                    h3('Prices and Floor Area by HDB Town'),
                    p('Spatial distribution of prices or floor area by town. Click on the towns to view more information.'),
                    fluidRow(
                        box(withLoader(tmapOutput('hdb_map'), type = "html", loader="loader1"),
                            width = 9),
                        
                        box(selectInput("hdbmap_select_agg",
                                        "Colour Map By",
                                        choices = c('Average Price' = 1,
                                                    'Median Price' = 2,
                                                    'Minimum Price' = 3,
                                                    'Maximum Price' = 4,
                                                    'Average Floor Area' = 5,
                                                    'Average Price Per sqm' = 6),
                                        selected = 1),
                            
                            sliderInput('year',
                                        'Year',
                                        min = 2000,
                                        max = 2020,
                                        value = 2020,
                                        sep = ''),
                
                            radioButtons('house_type',
                                         'Housing Type',
                                         choices = list('1-Room' = 1,
                                                        '2-Room' = 2,
                                                        '3-Room' = 3,
                                                        '4-Room' = 4,
                                                        '5-Room' = 5,
                                                        'Executive' = 6,
                                                        'Multi-Generation' = 7),
                                         selected = 5),
                            width = 3
                        )
                    )
            ),
            
            ######################## HDB DATA TABLE ########################
            tabItem(tabName = 'hdb_data_tab',
                    h3('Data'),
                    p('Data table that was used for the visualisation of HDB properties.'),
                    fluidRow(
                      box(withLoader(DT::dataTableOutput('hdb_table'), type = "html", loader="loader1"),
                          width = 12))
                    ),
            
            
            ######################## PRIVATE PROPERTY PRICE TREND ########################
            tabItem(tabName = 'priv_price_tab',
                    h3('Price Trend Over Time'),
                    p('Time-series trend of average prices by postal district and housing type.'),
                    fluidRow(
                        box(withLoader(plotlyOutput('priv_price_line'), type = "html", loader="loader1"),
                            width = 9),
                        box(
                            selectInput('price_select_district',
                                        'Select District',
                                        choices = district_names2,
                                        selected = "D1 - Raffles Place, Cecil, Marina, People's Park"),
                            selectInput('price_select_privtype',
                                        'Select Housing Type',
                                        choices = district_housing_types2),
                            width = 3)
                    )
            ),
            
            
            ######################## PRIVATE PROPERTY TREEMAP ########################
            tabItem(tabName = 'priv_treemap_tab',
                    h3('Median Unit Price ($ psqm) by Postal District and Housing Type'),
                    p('Larger and darker boxes reflect a higher median price. Click in to explore!'),
                    fluidRow(
                      box(withLoader(d3tree2Output("treemap1"),type = "html", loader="loader1"),
                          width = 10),
                      box(selectInput("variable5",
                                      "Year",
                                      choices = c("2015"="15", "2016"="16", "2017"="17",
                                                  "2018"="18", "2019"="19", "2020"="20"),
                                      selected = "20"),
                          width = 2)
                    ),
                    p('Search for your preferred postal district number below.'),
                    fluidRow(
                      box(DT::dataTableOutput('postaltable'),
                          width = 12))
            ),
            
            
            ######################## PRIVATE PROPERTY MAP ########################
            tabItem(tabName = "priv_map_tab",
                    h3('Prices and Floor Area by Subzone'),
                    p('Spatial distribution of prices or floor area by subzone. Click on the subzones to view more information.'),
                    fluidRow(
                        box(withLoader(tmapOutput("pprice_map"),type = "html", loader="loader1"),
                            width = 9),
                        
                        box(
                            selectInput("filter", "Colour Map By", 
                                        c("Average Price"="Average Price",
                                          "Median Price" = "Median Price",
                                          "Minimum Price"="Minimum Price",
                                          "Maximum Price"="Maximum Price",
                                          "Average Floor Area (sqm)" = "Average Floor Area",
                                          "Average Price Per sqm" = "Average Price Per sqm")),
                            
                            radioButtons("p_type",
                                         "Housing Type",
                                         c("Apartment" = "Apartment",
                                           "Condominium" = "Condominium",
                                           "Executive Condominium" = "Executive Condominium",
                                           "Detached" = "Detached",
                                           "Semi-detached" =  "Semi-detached",
                                           "Strata Detached" = "Strata Detached",
                                           "Strata Semi-detached" = "Strata Semi-detached",
                                           "Strata Terrace" = "Strata Terrace",      
                                           "Terrace"  = "Terrace")),
                            width = 3)
                    )
            ),
            
            ######################## PRIVATE PROPERTY DATA TABLE ########################
            tabItem(tabName = 'priv_data_tab',
                    h3('Data'),
                    p('Data table that was used for the visualisation of private properties.'),
                    fluidRow(
                      box(withLoader(DT::dataTableOutput('priv_table'), type = "html", loader="loader1"),
                          width = 12))
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    ################################################# Home ################################################# 
    
    # hdb: update housing type inputs based on town selection
    ave_town_row <- reactive({
        subset(hdb_home_data, town == input$home_select_town)
    })
    town_housing_types <- reactive({
        ave_town_row <- subset(hdb_home_data, town == input$home_select_town)
        ave_town_cols <-  colnames(ave_town_row)[complete.cases(t(ave_town_row))][-1]
        
        substring(ave_town_cols[grepl('size', ave_town_cols)], 6)
    })
    observeEvent(
        input$home_select_town, {
            updateSelectInput(session, 'home_select_housing',
                              label = "Select Housing Type",
                              choices = town_housing_types()
            )
        }
    )
    
    ## hdb outputs
    output$ave_hdb_price <- renderValueBox({
        valueBox(
            paste0('$ ',
                   prettyNum(ave_town_row()[[paste0('price_', input$home_select_housing)]],
                         big.mark = ',',
                         trim = TRUE)),
            subtitle = 'Average Price',
            icon = icon('dollar'),
            color = 'light-blue'
        )
    })
    
    output$ave_hdb_size <- renderValueBox({
        valueBox(
            paste0(ave_town_row()[[paste0('size_', input$home_select_housing)]], ' m\u00B2'),
            subtitle = 'Average Floor Area',
            icon = icon('expand'),
            color = 'teal'
        )
    })
    
    output$ave_hdb_price_sqft <- renderValueBox({
        valueBox(
            paste0('$ ', 
                   prettyNum(ave_town_row()[[paste0('pricesqm_', input$home_select_housing)]],
                             big.mark = ',',
                             trim = TRUE)),
            subtitle = 'Average Price Per m\u00B2',
            icon = icon('dollar'),
            color = 'teal'
        )
    })
    
    
    # private property: update housing type inputs based on district selection
    ave_district_row <- reactive({
        subset(priv_home_data, location == input$home_select_district)
    })
    district_housing_types <- reactive({
        ave_district_row <- subset(priv_home_data, location == input$home_select_district)
        ave_district_cols <-  colnames(ave_district_row)[complete.cases(t(ave_district_row))][-1]
        
        substring(ave_district_cols[grepl('area', ave_district_cols)], 6)
    })
    observeEvent(
        input$home_select_district, {
            updateSelectInput(session, 'home_select_privtype',
                              label = "Select Housing Type",
                              choices = district_housing_types()
            )
        }
    )
    
    # private property outputs
    output$ave_priv_price <- renderValueBox({
        valueBox(
            paste0('$ ',
                   prettyNum(ave_district_row()[[paste0('price_', input$home_select_privtype)]],
                             big.mark = ',',
                             trim = TRUE)),
            subtitle = 'Average Price',
            icon = icon('dollar'),
            color = 'light-blue'
        )
    })
    
    output$ave_priv_size <- renderValueBox({
        valueBox(
            paste0(ave_district_row()[[paste0('area_', input$home_select_privtype)]], ' m\u00B2'),
            subtitle = 'Average Floor Area',
            icon = icon('expand'),
            color = 'teal'
        )
    })
    
    output$ave_priv_price_sqft <- renderValueBox({
        valueBox(
            paste0('$ ', prettyNum(ave_district_row()[[paste0('pricesqm_', input$home_select_privtype)]],
                                   big.mark = ',',
                                   trim = TRUE)),
            subtitle = 'Average Price Per m\u00B2',
            icon = icon('dollar'),
            color = 'teal'
        )
    })
    
    ################################################# HDB PRICE CHART #################################################
    d_flat <- reactive({
        hdb_line_data %>%
            group_by(!!!rlang::syms(input$groupByInput), year) %>%
            summarize(mean_price = round(mean(resale_price, na.rm = TRUE), 0)) %>%
            { if (input$groupByInput == 'flat_type') filter(.,flat_type %in% input$flatTypeInput) else filter(.,town %in% input$townInput)} %>%
            filter(between(year,
                           as.Date(paste0(input$yearInput[1], '-01-01')),
                           as.Date(paste0(input$yearInput[2], '-01-01'))))
    })
    output$hdb_price_line <- renderPlotly({
        groupBy <- input$groupByInput
        ggplotly(
            ggplot(d_flat(), aes(x = year, y = mean_price, group = get(groupBy),
                                        text = sprintf("%s<br>Average Price: $ %s", 
                                                       year,
                                                       prettyNum(mean_price, big.mark = ',', trim = TRUE)))) +
                geom_line(aes_string(color = groupBy)) + 
                scale_y_continuous(label = comma) +
                scale_color_brewer(palette="Dark2") +
                theme_minimal() +
                theme(axis.title.y = element_text(margin = margin(r = 50)),
                      legend.title = element_blank()),
            tooltip = 'text'
        ) %>%
            layout(xaxis = list(title = ''),
                   yaxis = list(title = ''))
    })
    
    ################################################# HDB TREEMAP #################################################
    t2<-reactive({
      hdb_treemap_year <- hdb_treemap_data %>%filter(year == input$variable3)
      treemap(hdb_treemap_year,
              index=c("town","flat_type"),
              vSize="Median Resale Price",
              vColor="Median Resale Price",
              type="value",
              algorithm = "squarified",
              palette="Blues",
              overlap.labels=0.5,
              fontsize.legend = 5,
              fun.aggregate = 'mean')
    })
    
    output$treemap2<-renderD3tree({
      d3tree3(t2(),rootname = paste("HDB Properties By Towns in", input$variable3))
    })

    ################################################# HDB MAP ################################################# 
    output$hdb_map <- renderTmap({
        
        selected_cols <- hdb_resale_colnames[grepl(type_list[[as.integer(input$house_type)]], hdb_resale_colnames)]
        
        tm_shape(hdb_map_data %>% filter(year == input$year)) +
            tm_fill(selected_cols[[as.integer(input$hdbmap_select_agg)]],
                    textNA = 'NA',
                    colorNA = 'gray85',
                    popup.vars = c('Mean Price' = selected_cols[[1]],
                                   'Median Price' = selected_cols[[2]],
                                   'Minimum Price' = selected_cols[[3]],
                                   'Maximum Price' = selected_cols[[4]],
                                   'Average Floor Area (sqm)' = selected_cols[[5]],
                                   'Average Price Per sqm' = selected_cols[[6]]),
                    id = 'town',
                    group = 'HDB Towns',
                    alpha = 0.8) +
            tm_borders(lwd = 0.5,
                       alpha = 0.5,
                       col = 'gray22') +
            tm_view(set.zoom.limits = c(11, 15))
    })
    
    
    ########################################### HDB DATA TABLE ###########################################
    output$hdb_table <- DT::renderDataTable({
      DT::datatable(hdb %>%
                      select(1:5, 7, 10),
                    options = list(pageLength = 5,
                                   scrollX = TRUE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#1b4e6b', 'color': '#fff'});",
                                     "}")
                                   ))
    })
    
    
    ########################################### PRIVATE PROPERTY PRICE CHART ###########################################
    district_housing_types2 <- reactive({
        district_bydate_df <- subset(priv_line_data, location == input$price_select_district)
        
        colnames(district_bydate_df[(which(sapply(district_bydate_df, function(x) all(is.na(x))) == FALSE)[-1][-1])])
    })
    observeEvent(
        input$price_select_district, {
            updateSelectInput(session, 'price_select_privtype',
                              label = "Select Housing Type",
                              choices = district_housing_types2()
            )
        }
    )
    
    output$priv_price_line <- renderPlotly({
        plot_ly(subset(priv_line_data, location == input$price_select_district),
                x = ~date,
                y = ~get(input$price_select_privtype),
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgb(57, 204, 204)'),
                connectgaps = TRUE) %>%
            layout(xaxis = list(title = ''),
                   yaxis = list(title = 'Average Price ($)'))
    })
    
    
    ########################################### PRIVATE PROPERTY TREEMAP ########################################### 
    t1 <- reactive({
      priv_treemap_data_year <- priv_treemap_data %>%filter(year == input$variable5)
      treemap(priv_treemap_data_year,
              index=c("Postal District","Type"),
              vSize="Median Unit Price ($ psqm)",
              vColor="Median Unit Price ($ psqm)",
              type="value",
              algorithm = "squarified",
              palette="Blues",
              overlap.labels = 0.5,
              fontsize.legend = 6,
              fun.aggregate = 'mean'
      )
    })
    
    output$treemap1<-renderD3tree({
      d3tree3(t1(), rootname = paste("Private Properties by Postal District in 20", input$variable5,sep=""))
    })
    
    ########################################### POSTAL DISTRICT TABLE ########################################### 
    output$postaltable <- DT::renderDataTable({
        DT::datatable(postal_district_table,
                      options = list(pageLength = 5,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#1b4e6b', 'color': '#fff'});",
                                       "}")
                                     ))
        })
    
    ########################################### PRIVATE PROPERTY MAP ########################################### 
    output$pprice_map <- renderTmap({
        private_res_clean <- private_res_sf %>%
          filter(Type %in% input$p_type)%>%
          group_by(subzone)%>%
          summarise(`Minimum Price` = round(min(`Price ($)`), 0),
                    `Maximum Price` = round(max(`Price ($)`), 0),
                    `Median Price` = round(median(`Price ($)`), 0),
                    `Average Price` = round(mean(`Price ($)`), 0),
                    `Average Floor Area` = round(mean(`Area (Sqm)`), 0),
                    `Average Price Per sqm` = round(mean(`Unit Price ($psm)`), 0)) %>%
          as.data.frame() %>%
          select(-geometry)
        map_data <- left_join(mpsz, private_res_clean, by= c("SUBZONE_N" = "subzone"))
        
        tm_shape(map_data)+
            tm_fill(input$filter,
                    id = "SUBZONE_N",
                    textNA = 'NA',
                    colorNA = 'gray85',
                    popup.vars = c("Average Price" = "Average Price",
                                 "Median Price" = "Median Price",
                                 "Minimum Price" = "Minimum Price",
                                 "Maximum Price" = "Maximum Price",
                                 "Average Floor Area (sqm)" = "Average Floor Area",
                                 "Average Price Per sqm" = "Average Price Per sqm"),
                    alpha = 0.8) +
            tm_borders(lwd = 0.5,
                       alpha = 0.5,
                       col = 'gray22') +
            tm_view(set.zoom.limits = c(11, 15))
    })
    
    ########################################### PRIVATE PROPERTY DATA TABLE ###########################################
    output$priv_table <- DT::renderDataTable({
      DT::datatable(private_prop %>%
                      select(2:5, 8:10, 12:13, 15),
                    options = list(pageLength = 3,
                                   scrollX = TRUE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#1b4e6b', 'color': '#fff'});",
                                     "}")
                                   ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
