library(shiny)
library(plotly)
library(hash)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
library(performance)

########Constant############
H = hash()
H[["Coverage"]] = "ppl_ful_vacc_per_100"
H[["Total vaccination"]] = "ttl_vacc"
H[["Total vaccination per 100"]] = "ttl_vacc_per_100"
H[["People vaccination"]] = "ppl_vacc"
H[["People vaccination per 100"]] = "ppl_vacc_per_100"
H[["Type of vaccine"]] = "vacc"
VARIABLE_CHOICE = c("Coverage", "Type of vaccine", "Total vaccination per 100", 
                    "People vaccination per 100", "Total vaccination", "People vaccination")
OFFSET = 2 #content start from column 4
CONTENT_WIDTH = 8 #content end at column 6
VACCINE = c("Oxford/AstraZeneca", "Pfizer/BioNTech", "Sinovac", "Sputnik V", "Sinopharm/Beijing","Johnson&Johnson","Moderna","Sinopharm/Wuhan","Covaxin","CanSino", "EpiVacCorona")

########Code####################

#Home tab
Home.fluidPage = fluidPage(
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, h2("User Guideline"))),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH,
                    tags$div(
                        tags$p(strong("Home Tab:"), "Show the user guide and the information of Group 4 Explorer members and the application.", style = "font-size: 22px"),
                        tags$p(strong("Data Tab:"), "Show the description of Covid-19 vaccination dataset that been retrieved from Kaggle.", style = "font-size: 22px"),
                        tags$p(strong("World Tab:"), "Show the states of vaccination across the world. In the side panel, user can change the variable and the time before to consider to understand the states from different perspective.", style = "font-size: 22px"),
                        tags$p(strong("Country Tab:"), "Show the states of vaccinination in an individual country. In the side panel, user can change the country and variable", style = "font-size: 22px"),
                    )
    )),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, h2("About"))),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, 
                    h3("Developers"),
                    tags$div(
                        tags$p("- Team Leader: Lee Chang Xiu (S17219894)", style = "font-size: 17px"),
                        tags$p("- Team Member: Siyi Zhou (S2031274)", style = "font-size: 17px"),
                        tags$p("- Team Member: Sharifah Shakila Sirajudin (S2017968)", style = "font-size: 17px"),
                        tags$p("- Team Member: Tan Wee Kiat (S2033309)", style = "font-size: 17px")
                    ),
                    h3("Purpose"),
                    tags$div(
                        tags$p("1. Give a general overview of different aspect of vaccination around the world.", style = "font-size: 17px"),
                        tags$p("2. Indicates the current progress of vaccination on each country and whether the current vaccination scheme needs further improvement.", style = "font-size: 17px")
                    ),
                    h3("Question"),
                    tags$div(
                        tags$p("1. What is the vaccination coverage rate of country around the world?", style = "font-size: 17px"),
                        tags$p("2. What is the vaccination performance of country and type of vaccine used around the world?", style = "font-size: 17px"),
                        tags$p("3. What is the current performance of an individual country and estimate time for 70% vaccination coverage?", style = "font-size: 17px")
                    ),
                    h3("Project Experiences"),
                    tags$div(
                        tags$p("1. We have successfully deployed shinyapps.io through data science process.", style = "font-size: 17px"),
                        tags$p("2. We are able to answer the project question from the dataset.", style = "font-size: 17px"),
                        tags$p("3. We have chosen vaccination data which can be update weekly and reproducibility.", style = "font-size: 17px"),
                        tags$p("4. We are able to create a map for visualize and forecast the data to aid understanding.", style = "font-size: 17px")
                    )
    ))
)

#Data Science Process tab
DSP.fluidPage = fluidPage(
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, h2("Data Science Pipeline"))),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, 
                    h3("Data Acquisition"),
                    tags$div(
                        tags$p("Covid World Vaccination Progress and Population by Country dataset from Kaggle", style = "font-size: 17px")
                    ),
                    h3("Data Pre-processing"),
                    tags$div(
                        tags$p("1. Impute the missing value in total vaccination by cumulative sum of daily vaccination.", style = "font-size: 17px"),
                        tags$p("2. Input of population by country for vaccination coverage.", style = "font-size: 17px")
                    ),
                    h3("Exploratory Data Analysis and Forecast"),
                    tags$div(
                        tags$p("1. Vaccination rate and coverage on world map and rank by country.", style = "font-size: 17px"),
                        tags$p("2. Distribution of vaccines around the world and ranking.", style = "font-size: 17px"),
                        tags$p("3. Country vaccination performance, forecast of 70% rate to achieve herd immunity and type of vaccine used.", style = "font-size: 17px")
                    )
    ))
)

#Data tab
Data.fluidPage = fluidPage(
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, h2("Dataset"))),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH,
                    tags$div(
                        tags$p("Covid-19 world vaccination progress dataset is collected from", a(href="https://www.kaggle.com/gpreda/covid-world-vaccination-progress", "Kaggle"), "website.", style = "font-size: 17px"),
                        tags$p("For the clean dataset used in this case please refer to", a(href="https://www.kaggle.com/gpreda/covid-world-vaccination-progress", "Github"), style = "font-size: 17px")
                    )
    )),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, h3("Dataset Description"))),
    fluidRow(column(offset = OFFSET, width = CONTENT_WIDTH, 
                    h4("This dataset description is focus on clean dataset used in this study and the time range of this dataset is 14 December 2020 - 6 May 2021 which include 190 country:"),
                    tags$div(
                        tags$p(strong("country:"),"this is the country for which the vaccination information is provided", style = "font-size: 17px"),
                        tags$p(strong("continent:"),"this is the continent of the particular country.", style = "font-size: 17px"),
                        tags$p(strong("date:"),"date for the data entry; for some of the dates we have only the daily vaccinations, for others, only the (cumulative) total.", style = "font-size: 17px"),
                        tags$p(strong("vacc:"),"stand for 'vaccines' used in the country (up to date)", style = "font-size: 17px"),
                        tags$p(strong("ttl_vacc:"),"stand for 'Total number of vaccination' absolute number of total immunizations in the country", style = "font-size: 17px"),
                        tags$p(strong("ppl_vacc:"),"stand for 'total number of people vaccinated' a person, depending on the immunization scheme, will receive one or typically 2 vaccines at a certain moment, the number of vaccination might be larger than the number of people", style = "font-size: 17px"),
                        tags$p(strong("ppl_ful_vacc:"),"stand for 'Total number of people fully vaccinated' this is the number of people that received 2 dose of vaccine.", style = "font-size: 17px"),
                        tags$p(strong("dai_vacc_raw:"),"stand for 'Daily vaccinations (raw)' the number of vaccination for that date/country.", style = "font-size: 17px"),
                        tags$p(strong("dai_vacc:"),"stand for 'daily vaccinations' for a certain data entry, the number of vaccination for that date/country.", style = "font-size: 17px"),
                        tags$p(strong("ttl_vacc_per_100:"),"stand for 'Total vaccinations per hundred' ratio (in percent) between vaccination number and total population up to the date in the country", style = "font-size: 17px"),
                        tags$p(strong("ppl_vacc_per_100:"),"stand for 'Total number of people vaccinated per hundred ' ratio (in percent) between population immunized and total population up to the date in the country", style = "font-size: 17px"),
                        tags$p(strong("ppl_full_vacc_per_100:"),"stand for 'Total number of people fully vaccinated per hundred' ratio (in percent) between population fully immunized and total population up to the date in the country ", style = "font-size: 17px"),
                        tags$p(strong("dai_vacc_per_million:"),"stand for 'Daily vaccinations per million' ratio (in ppm) between vaccination number and total population for the current date in the country", style = "font-size: 17px"),
                        tags$p(strong("csum:"),"stand for cummulative sum of daily vaccination", style = "font-size: 17px"),
                        tags$p(strong("population:"),"is the population of the country.", style = "font-size: 17px"),
                        tags$p(strong("codes:"),"is the country codes.", style = "font-size: 17px")
                    )
    ))
)

#World tab
World.sidebarLayout = fluidPage(column(offset = OFFSET, width = CONTENT_WIDTH,
                                       sidebarLayout(
                                           sidebarPanel(
                                               selectInput("variable1", "Variable", VARIABLE_CHOICE),
                                               conditionalPanel(condition = "input.variable1 == 'Type of vaccine'", selectInput("vaccine", "Vaccine (For Map)", VACCINE)),
                                               conditionalPanel(condition = "input.variable1 != 'Type of vaccine'", htmlOutput("timeBefore1"))
                                           ),
                                           mainPanel(fluidPage(
                                               fluidRow(
                                                   conditionalPanel(condition = "input.variable1 != 'Type of vaccine'", plotlyOutput("map1")),
                                                   conditionalPanel(condition = "input.variable1 == 'Type of vaccine'", plotlyOutput("map2"))
                                               ),
                                               fluidRow(
                                                   conditionalPanel(condition = "input.variable1 == 'Type of vaccine'",plotOutput("topvacc")),
                                                   conditionalPanel(condition = "input.variable1 != 'Type of vaccine'",fluidPage(
                                                       fluidRow(
                                                           column(width = 6, h3("Top 10", align = "left")),
                                                           column(width = 6, h3("Last 10", align = "left"))
                                                       ),
                                                       fluidRow(
                                                           column(width = 6, plotOutput("top10")),
                                                           column(width = 6, plotOutput("last10"))
                                                       )
                                                   ))
                                               )
                                           ))
                                       )
))

#country tab
Country.sidebarLayout = fluidPage(column(offset = OFFSET, width = CONTENT_WIDTH,
                                         
                                         sidebarLayout(
                                             sidebarPanel(
                                                 htmlOutput("country"),
                                                 selectInput("variable2", "Variable (For 1st Graph)",VARIABLE_CHOICE[!(VARIABLE_CHOICE %in% c("Type of vaccine", "Coverage"))])
                                             ),
                                             mainPanel(fluidPage(
                                                 fluidRow(
                                                     column(width = 12, plotlyOutput("linePlot2"))),
                                                 fluidRow(
                                                     column(width = 8, plotOutput("rate")),
                                                     column(width = 4, htmlOutput("edc"))
                                                 ),
                                                 fluidRow(
                                                     column(width = 4, htmlOutput("percentile")),
                                                     column(width = 8, plotOutput("freqdist"))
                                                 ),
                                                 fluidRow(
                                                     column(width = 12, h3("Vaccine Choice", align = "left")),
                                                     column(width = 12, htmlOutput("vaccchoice"))
                                                 )
                                             )
                                             )
                                         )
))

ui <- navbarPage("COVID-19 Vaccination",
                 tabPanel("Home", Home.fluidPage),
                 tabPanel("Data Science Process", DSP.fluidPage),
                 tabPanel("Data", Data.fluidPage),
                 tabPanel("World", World.sidebarLayout),
                 tabPanel("Country",Country.sidebarLayout))


server <- function(input, output) {
    df = read.csv("./data/df.clean.csv") 
    df[,"date"] = dmy(df[,"date"]) #Reformat date properly to be used
    df[,"ym"] = paste0(year(df[,"date"]), "-", month(df[,"date"], label = TRUE)) #limiting timeBefore choice in ranking section
    
    
    #####Data dependent UI#####
    output$timeBefore1 = renderUI({selectInput("timeBefore1", "Time Before",sort(unique(df[,"ym"]), decreasing = TRUE), width = "100%")})
    
    output$country = renderUI({selectInput("country",  "Country", 
                                           unique(subset(df, select = "country")), 
                                           width = "100%")})
    
    #####Output#####
    
    #world
    output$map1 = renderPlotly(get_map1(input$variable1, input$timeBefore1))
    output$map2 = renderPlotly(get_map2(input$vaccine))
    output$last10 = renderPlot(sort_function(input$variable1, input$timeBefore1, 'last'))
    output$top10 = renderPlot(sort_function(input$variable1, input$timeBefore1, 'top'))
    output$topvacc = renderPlot(vaccTypePlot())
    
    #country
    output$linePlot2 = renderPlotly(timelineggplot(input$country, input$variable2))
    output$rate = renderPlot(predict_day(input$country, 70, graph = TRUE))
    output$edc = renderUI({predict_day(input$country, 70, graph = FALSE)})
    output$freqdist = renderPlot(freq_dist_coverage(input$country, plot = TRUE))
    output$percentile = renderUI({
        tags$div(tags$p("Better than", style = "text-align: center; margin-top: 100px;"),
                 tags$h1(paste0(freq_dist_coverage(input$country, plot = FALSE), "%"), style = "text-align: center;"),
                 tags$p("of the country in the world", style = "text-align: center;"),
                 tags$h1(paste0(freq_dist_coverage(input$country, world = FALSE, plot = FALSE), "%"), style = "text-align: center;"),
                 tags$p("of the country In the continent", style = "text-align: center;"))
    })
    output$vaccchoice = renderUI({tags$p(vaccTypeText(input$country), style = "text-align: left;")})
}

getdf = function(variable, inputMonth){
    if(!is.null(inputMonth)){
        df = read.csv("./data/df.clean.csv")
        df[,"date"] = dmy(df[,"date"])
        df[,"month"] = month(df[,"date"], label = TRUE) #label true to display in alphabet instead of number
        df[,"year"] = year(df[,"date"])
        variable = H[[variable]]
        df = subset(df, year <= str_split(inputMonth, "-")[[1]][1] & month <= str_split(inputMonth, "-")[[1]][2]) #alphabet comparison (Feb > Jan) is possible due to lubridate
        if(variable == "ppl_ful_vacc_per_100"){
            country_list = unique(df[,"country"])
            for(i in 1:length(country_list)){
                if(all(is.na(df[df[,"country"] == country_list[i],"ppl_ful_vacc_per_100"]))){
                    df[df[,"country"] == country_list[i],"ppl_ful_vacc_per_100"] =  (df[df[,"country"] == country_list[i],"ttl_vacc"] / (2 * df[df[,"country"] == country_list[i],"population"])) * 100
                }
            }
        }
        x <- df %>% 
            group_by(codes) %>%
            select(codes, country, variable) %>% drop_na() %>% #Select the column needed then drop na to prevent dplyr issue warning in summarise_if
            summarise_all(max)
    }else{
        return(NULL) #Tell other function no data is returned
    }
}

get_map1 = function(variable, inputMonth){
    df = getdf(variable, inputMonth)
    if(!is.null(df)){
        plot_ly(df, type='choropleth', locations=df$codes, z=df[[H[[variable]]]], text=df$country ,colorscale="Viridis") %>% 
            colorbar(thickness = 10) %>% 
            layout(margin = list(r = 0,t = 0,b = 0,l = 0))
    }
}

get_map2 = function(vaccine){
    if(!is.null(vaccine)){
        df = read.csv("./data/df.clean.csv")
        df2 = subset(df, select = c("country", "codes" , "vacc"))
        df2 = unique(df2)
        df2$isUsed = NA
        for(i in 1:nrow(df2)){
            if(vaccine %in% str_split(df2[i,"vacc"], ", ")[[1]]){
                df2[i ,"isUsed"] = paste0("Use ", vaccine)
            }else{
                df2[i ,"isUsed"] = paste0("Not use ", vaccine )
            }
        }
        df = subset(df2, select = c("country", "codes", "isUsed"))
        df$isUsed = factor(df$isUsed)
        df$isUsedNumeric = as.numeric(df$isUsed) #Each factor is coded as a discrete value because plotly can only read numeric value
        
        #####generate color######
        nfactor = length(levels(df$isUsed))
        foo = c("#E41A1C", "#377EB8") #Color List
        names(foo) = levels(df$isUsed)
        CUTS = seq(0,1,length.out=nfactor+1)
        BREAKS = rep(CUTS,ifelse(CUTS %in% 0:1,1,2)) #Tell plotly from where to where for each color in color bar
        colorScale = data.frame(z=BREAKS,col=rep(foo[1:nfactor],each=2),stringsAsFactors=FALSE)
        
        #####plot######
        plot_ly(
            data = df,
            type = "choropleth",
            locations = df$codes,
            z = df$isUsedNumeric,
            text = df$country,
            colorscale=colorScale,
            colorbar=list(tickvals=1:nfactor, ticktext=names(foo))
        ) %>% colorbar(thickness = 10) %>%
            layout(margin = list(r = 0,t = 0,b = 0,l = 0))
    }
}

sort_function <- function(variable, inputMonth, type){
    #!is.null(variable) & !is.null(inputMonth) & !is.null(type) & variable  because The function will run once before the UI is rendered, which make the input null, trigger dplyr's warnings.
    #variable != "Type of vaccine" because Function involve in any conditional panel will run once, even if the panel may not end  up showing.
    if(!is.null(variable) & !is.null(inputMonth) & !is.null(type) & variable != "Type of vaccine"){ 
        df = read.csv("./data/df.clean.csv")
        df[,"date"] = dmy(df[,"date"])
        df[,"month"] = month(df[,"date"], label = TRUE) #label true to display in alphabet instead of letter
        df[,"year"] = year(df[,"date"])
        variable = H[[variable]]
        df2 = subset(df, year <= str_split(inputMonth, "-")[[1]][1] & month <= str_split(inputMonth, "-")[[1]][2]) #alphabet comparison (Feb > Jan) is possible due to lubridate
        x <- df2 %>% 
            group_by(country) %>%
            select(country, variable) %>% drop_na() %>% #Select the column needed then drop na to prevent dplyr issue warning in summarise_if
            summarise_if(is.numeric, max) 
        
        if(type == "top"){
            top10 <- head(arrange(x, desc(x[,variable])), 10)
            top10[["country"]] = str_wrap(top10[["country"]], 10)
            
            #Lollipop Chart
            ggplot(data = top10, aes(x = reorder(country, .data[[variable]]) , y = .data[[variable]])) +
                geom_point(size = 3, color="purple") + 
                geom_segment(aes(x = country,
                                 xend = country,
                                 y = 0,
                                 yend = .data[[variable]])) +
                coord_flip() + 
                theme(axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      title=element_blank())
            
            
        }else if(type == "last"){
            last10 <- head(arrange(x, x[,variable]), 10)
            last10[["country"]] = str_wrap(last10[["country"]], 10)
            
            ## Lollipop Chart
            ggplot(data = last10, aes(x = reorder(country, desc(.data[[variable]])) , y = .data[[variable]])) +
                geom_point(size = 3, color="purple") + 
                geom_segment(aes(x = country,
                                 xend = country,
                                 y = 0,
                                 yend = .data[[variable]])) +
                coord_flip() + 
                theme(axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      title=element_blank())  
        } 
    }
}

emptyplot = function(text = "Place here", border = TRUE){
    if(border == TRUE){
        ggplot() + 
            annotate("text", x = 4, y = 25, size=8, label = text) + 
            theme_void() + theme(panel.border = element_rect(colour = "black", fill=NA, size=5))
    }else{
        ggplot() + 
            annotate("text", x = 4, y = 25, size=8, label = text) + 
            theme_void() 
    }
    
}

timelineggplot = function(selectCountry, variable){
    if(!is.null(selectCountry) & !is.null(variable)){
        df = read.csv("./data/df.clean.csv")
        df[,"date"] = dmy(df[,"date"])
        variable = H[[variable]]
        df_country = subset(df, country == selectCountry, select = c("date", variable)) %>% drop_na()
        if(nrow(df_country) > 0){
            plot_ly(x = df_country$date, y = df_country[, variable], 
                    type="scatter", mode="markers", 
                    fill = "tozeroy", name = variable) %>% layout(xaxis = list(rangeslider = list(type = "date")))
        }else{
            #When there is no row, display a text message notifying user instead of display empty graph
            plot_ly(x = df_country$date, y = df_country[, variable], 
                    type="scatter", mode="markers", 
                    fill = "tozeroy", name = variable) %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                                                                  annotations = list(text = "Sorry, we don't have any data.", showarrow = FALSE))
        }
    }
}

vaccTypePlot = function(){
    vaccine_data_val = read.csv("./data/vaccine_data.val.csv") #Read from data instead of compute the value on the go because of processing large dataset will block UI thread 
    ggplot(data = vaccine_data_val, aes(x=reorder(Vaccine_name,-Vaccine_count1), y=Vaccine_count1))+
        geom_point(size = 3, color="purple") + 
        geom_segment(aes(x = Vaccine_name,
                         xend = Vaccine_name,
                         y = 0,
                         yend = Vaccine_count1)) + 
        labs(x = "Vaccines", y = "Percentage") +
        scale_y_continuous(labels = scales::percent) + 
        theme_light() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank(),
              title = element_blank()) +
        geom_text(mapping=aes(x=reorder(Vaccine_name,-Vaccine_count1), y=Vaccine_count1, label = paste(Vaccine_count1*100,"%")), vjust=-0.5)
}

vaccTypeText = function(country_name){
    df = read.csv("./data/df.clean.csv")
    df = subset(df, country == country_name)
    return(df[1,"vacc"])
}

predict_day <- function(country_name, perc, graph = TRUE){
    # detect the null value
    if(!is.null(country_name) & !is.null(perc)){
        
        # load the data
        df2 = read.csv("./data/df.clean.csv")
        
        # add a new column called vacc_rate
        df2$vacc_rate = df2$ppl_ful_vacc_per_100
        estimator = "Total number of people fully vaccinated per hundred"
        
        # select data by country
        df_country <- df2 %>% filter(df2$country == country_name) %>% select("date", "vacc_rate", "ttl_vacc", "population")
        
        #Change estimator of coverage when vacc_rate is all empty
        if(nrow(df_country[!is.na(df_country$vacc_rate),]) <= 1){
            df_country$vacc_rate = (df_country[,"ttl_vacc"]/ (2 * df_country[,"population"])) * 100
            estimator = "(Total number of vaccinations / (2 * population)) * 100"
        }
        df_country$day <- 1:nrow(df_country)
        
        #Clean data, remove any cleft because any cleft is not logical
        df_country = df_country[!is.na(df_country[,"vacc_rate"]),] # assume no missing data
        i = 1
        j = 2
        found = FALSE
        while(j <= length(df_country$vacc_rate)){
            if(df_country$vacc_rate[i] >  df_country$vacc_rate[j]){
                df_country$vacc_rate[j] = NA
                found = TRUE
                if(found == TRUE){
                    j  = j + 1
                }
            }else{
                found = FALSE
                j = j + 1
                i = j - 1
            }
        }
        
        df_country = df_country[!is.na(df_country[,"vacc_rate"]),]
        if(nrow(df_country) > 1){
            #build linear model based on last 30 points
            x <- tail(df_country, 3)$day
            y <- tail(df_country, 3)$vacc_rate
            
            #days -> rate
            linear = lm(y ~ x)
            
            #rate -> date
            linearinverted = c(-(linear$coefficients[1] / linear$coefficients[2]), 1 / linear$coefficients[2])
            
            #Forecast data until the day when coverage = 70
            day70 = linearinverted[1] + linearinverted[2] * 70 #rate --> day
            forecast = data.frame(vacc_rate = predict(linear, data.frame(x = tail(df_country, 1)$day:day70)), day = tail(df_country, 1)$day:day70)
            
            p = ggplot(data = data.frame(x = df_country$day, y = df_country$vacc_rate)) + 
                geom_line(mapping = aes(x = x, y = y, color = "Actual"), linetype = "dashed") +
                xlab("Day") +
                ylab(paste0("% Coverage - ", estimator))+
                theme_light() + ylim(0, 100) + xlim(0,300) +
                geom_line(data = forecast, mapping = aes(x = day, y = vacc_rate, color = "Forecast"), linetype = "dashed") +
                geom_hline(aes(yintercept = 70, color = "Target"), linetype = 3) +
                theme(legend.title = element_blank(), legend.position="bottom")
            
            #Forecast the coverage at the day when app is run
            last_day = tail(df_country, 1)
            today_day = as.numeric(last_day$day + (today() - dmy(last_day$date))) 
            today_rate = predict(linear,  data.frame(x = today_day))
            
            # to decide which one to return
            if(graph == TRUE){
                return(p)
            }else{
                if(nrow(df_country[df_country$vacc_rate > 70, ]) > 0 | today_rate > 70){
                    return(0)
                }else{
                    result = as.numeric(round(day70) - round(today_day)) #Use model to forecast the day to get 70% coverage, then the result - today_day to get how much day is needed to reach 70%
                    html_code =  tags$div(tags$h1(paste0(result, " Days"), style = "text-align: center; margin-top: 150px;"),
                                          tags$p("Needed for 70% coverage*", style = "text-align: center;"))
                    return(html_code)
                }
            }
        }else if(nrow(df_country) == 1){
            p = ggplot(data = data.frame(x = df_country$day, y = df_country$vacc_rate)) +
                geom_point(mapping = aes(x = x, y = y), color = "blue") +
                xlab("Day") +
                ylab(paste0("% Coverage - ", estimator))+
                theme_light() + ylim(0, 100) + xlim(0,300) +
                geom_hline(aes(yintercept = 70), linetype = "dashed") 
            if(graph == TRUE){
                return(p)
            }else{
                if(nrow(df_country[df_country$vacc_rate > 70, ]) > 0){
                    html_code =  tags$div(tags$h1(paste0(0, " Days"), style = "text-align: center; margin-top: 150px;"),
                                          tags$p("Needed for 70% coverage*", style = "text-align: center;"))
                    return(html_code)
                }else{
                    html_code =  tags$div(tags$h1(paste0("Too few data to calculate"), style = "text-align: center; margin-top: 150px;"))
                    return(html_code)
                }
            }
        }else{
            p = emptyplot("Too few data to display anything", border = FALSE)
            if(graph == TRUE){
                return(p)
            }else{
                html_code =  tags$div(tags$h1(paste0("Too few data to calculate"), style = "text-align: center; margin-top: 150px;"))
                return(html_code)
            }
        }
    }
}

freq_dist_coverage = function(country_name, world = TRUE, plot = TRUE){
    if(!is.null(country_name)){
        # load the data
        df2 = read.csv("./data/df.clean.csv")
        
        # add a new column called vacc_rate
        df2$vacc_rate = df2$ppl_ful_vacc_per_100
        estimator = "Total number of people fully vaccinated per hundred"
        
        # select data by country
        df_country <- df2 %>% filter(df2$country == country_name) %>% select("date", "vacc_rate", "ttl_vacc", "population")
        
        #Change estimator of coverage when vacc_rate is all empty
        if(nrow(df_country[!is.na(df_country$vacc_rate),]) <= 1){
            df_country$vacc_rate = (df_country[,"ttl_vacc"]/ (2 * df_country[,"population"])) * 100
            estimator = "(Total number of vaccinations / (2 * population)) * 100"
        }
        
        #If estimator is changed, recompute vacc_rate
        if(estimator == "(Total number of vaccinations / (2 * population)) * 100"){
            country_list = unique(df2[,"country"])
            for(i in 1:length(country_list)){
                df2[df2[,"country"] == country_list[i],"vacc_rate"] =  (df2[df2[,"country"] == country_list[i],"ttl_vacc"] / (2 * df2[df2[,"country"] == country_list[i],"population"])) * 100
            }
        }
        
        #Get maximum coverage of country
        vacc_rate_country  = max(df_country[, "vacc_rate"], na.rm = TRUE)
        
        #Compute percentile of the country in the world
        df_world = df2 %>% select(country, vacc_rate) %>% drop_na() %>% group_by(country)  %>% summarise(vacc_rate = max(vacc_rate, na.rm = TRUE)) 
        df_world = df_world[!is.infinite(df_world$vacc_rate),]
        world_ecdf = ecdf(df_world$vacc_rate)
        world_percentile = world_ecdf(vacc_rate_country)
        
        #Compute percentile of the country in the continent
        country_continent = subset(df2, country == country_name)[1,"continent"]
        df_continent = subset(df2, continent == country_continent)
        df_continent = df_continent %>% select(country, vacc_rate) %>% drop_na() %>% group_by(country) %>%  summarise(vacc_rate = max(vacc_rate, na.rm = TRUE))
        df_continent = df_continent[!is.infinite(df_continent$vacc_rate),]
        continent_ecdf = ecdf(df_continent$vacc_rate)
        continent_percentile = continent_ecdf(vacc_rate_country)
        
        if(plot == TRUE){
            ggplot() + 
                geom_density(data = df_world,  mapping = aes(x= vacc_rate, color = "World")) + 
                geom_density(data = df_continent, mapping = aes(x = vacc_rate, color = country_continent)) +
                geom_vline(aes(xintercept = vacc_rate_country), linetype = "dashed") +
                annotate("text", x = vacc_rate_country + 3, y = 0.03, label = country_name, angle = 90) + xlab(paste0("% Coverage - ", estimator)) + 
                ylab("Density") + 
                theme_light() +
                theme(legend.title = element_blank(), legend.position="bottom")
        }else{
            if(world){
                round( (world_percentile) * 100)
            }else{
                round( (continent_percentile) * 100)
            }
        }
    }
}

shinyApp(ui = ui, server = server)
