library(shiny) 
library(dplyr)
library(statebins)
library(ggplot2)
library(data.table)
library(rsconnect)
library(DT)

ui <- fluidPage(
  HTML('<script src="//use.typekit.net/tgy5tlj.js"></script>'),
  HTML('<script>try{Typekit.load();}catch(e){}</script>'),
  theme = "theme.css",
  title = "DSPG2020STW", 

   navbarPage( title = "Skilled Technical Workforce",

       tabPanel("About",
                h1("Skilled Techinical Workforce"),
                p("A job in the skilled technical workforce (STW) is one that is open to an individual without a bachelor’s degree who has a high 
                  level of knowledge in a technical domain such as computers, mathematics, healthcare, architecture, engineering, construction, or extraction. 
                  The United States needs a STW to foster innovation and remain competitive in the global economy, but findings by the National Academies’ 
                  in Building America’s Skilled Technical Workforce (2017) indicate the United States is not adequately developing and sustaining the STW 
                  needed to compete in the 21st century; they project that by 2022 the United States will have 3.4 million unfilled STW jobs."),
                p("Our project aims to explore the usability of the Burning Glass Technologies job ad data for describing the demand of the skilled technical workforce."),
                h5("Who We Are"),
                h4("University of Virginia, Biocomplexity Institute, Social and Decision Analytics Division"),
                p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia. 
                  SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research 
                  and quantitative methods to inform policy decision-making and evaluation. 
                  The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology, 
                  political science, policy, health IT, public health, program evaluation, and data science. 
                  The SDAD office is located near our nation's capital in Arlington, VA. You can learn more about us at",
                tags$a(href="https://biocomplexity.virginia.edu/social-decision-analytics.", "https://biocomplexity.virginia.edu/social-decision-analytics.")),

                h4("Data Science for the Public Good Program"),
                p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its seventh year, the program engages students from across the country 
                  to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. 
                  DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information 
                  generated within every community can be leveraged to improve quality of life and inform public policy."),
                h4("Team"),
                p("Vicki Lancaster, PI"),
                p("Sarah McDonald, Acting Fellow"),
                p("Ian MacLeod, Intern"),
                p("Gina Fendley, Intern")
                
                ), # end about tab
                
       tabPanel("Data Profiling", 
                
                fluidRow(column(12, align = "center", h3("Burning Glass Job Ads Data Profiling, 2010-2019"))), 
                fluidRow(
                  column(8,fluidRow(dataTableOutput("profile"))), 
                  column(1),
                  column(3, 
                    wellPanel(
                           selectInput("prof_select", label = "Select Year:", choices = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)),                            
                           strong(p("Metrics:")),
                            p(), 
                            p(tags$ul(
                              tags$li("Completeness: The percentage of non-missing observations"),
                              tags$li("Validity: The percentage of data elements whose attributes possess values within the range expected for a legitimate entry"),
                              tags$li("Uniqueness: The number of unique values that have been entered for a variable")
                            ))))),
       fluidRow(h4("Discussion"), 
                p(),
                p("Note: All information regarding variable descriptions was taken from the Burning Glass Data Dictionary"))),   
                    
                
               
       
       #end profiling tab------------------------------------------ 
       
       
       navbarMenu("BGT/JOLTS Benchmark", 
        tabPanel("Total/Regional Comparisons", 
                fluidRow(column(12, align = "center", h3("National and Regional Comparison of Job Estimates"))),
                fluidRow(column(2),
                         column(8, align = "center",
                                p("The", tags$span(' blue ', style = "background-color: #232D4B; color: white;border-radius: 25px; white-space: pre-wrap;"), 
                                  "dots show JOLTS job opening estimates, and the", tags$span(' orange ', style = "background-color: #E57200; color: white;border-radius: 25px; white-space: pre-wrap;"), 
                                  "dots show the BGT job-ads estimates."),
                                selectInput("select", "", choices = c("National", "Regional"), selected = "National"),
                                plotOutput("jobsByYearOrRegion")),
                         column(2)),
                
                fluidRow(p("Paragraph that describes how JOLTS estimates job openings and how BGT collects and records job-ads: 
                   job openings versus job-ads what are the biases in the comparisons and a paragraph that summarizes the statebins changes in the 
                   23 MOC over the years 2010-2019 between and within states (we can all brainstorm on this)"))
                ),
                
        
        
        
        tabPanel("State Comparisons", 
             fluidRow(width = 12, align = "center", column(12, h3("Percent Difference Between BGT and JOLTS") )), 
             fluidRow(width = 12, column(5), 
                      column(2, sliderInput("slide", label = NULL, min = 2010, max = 2019, value = 2014, sep = "")),
                      column(5)),
             fluidRow(column(1), 
                      column(10, plotOutput("statebins", width= "100%", height = "600px")),
                      column(1)),
             fluidRow(h4("Discussion"),
                      p("Paragraph discussing change in state level percent difference over time")),
             fluidRow(column(2,  
                             wellPanel(
                               selectInput("definition2", "SOC Definition", choices = c("SOC 11", "SOC 13", "SOC 15",
                                                                                       "SOC 17", "SOC 19", "SOC 21", "SOC 23", "SOC 25", "SOC 27", "SOC 29", "SOC 31", "SOC 33",
                                                                                       "SOC 35", "SOC 37", "SOC 39", "SOC 41", "SOC 43", "SOC 45", "SOC 47", "SOC 49", "SOC 51", "SOC 53", "SOC 55")),
                               textOutput("soc2")))), 
             fluidRow(dataTableOutput("summary")) 
        ) # end tabPanel
        
        
        ),#end navbar
                
       #end Jolts vs BGT tab-----------------
       
       navbarMenu("BGT Education", 
         tabPanel("State Comparisons",
                  fluidRow(width = 12, align = "center", column(12, h3("Percent of BGT Job Ads That Do Not Require a College Degree by State") )), 
                  fluidRow(width = 12, column(5), 
                           column(2, sliderInput("slide2", label = NULL, min = 2010, max = 2019, value = 2014, sep = "")),
                           column(5)),
                  fluidRow(column(1), 
                           column(10, plotOutput("stw", width= "100%", height = "600px")),
                           column(1)), 
                  fluidRow(column(2,  
                                  wellPanel(
                                    selectInput("definition", "SOC Definition", choices = c("SOC 11", "SOC 13", "SOC 15",
                                                                                            "SOC 17", "SOC 19", "SOC 21", "SOC 23", "SOC 25", "SOC 27", "SOC 29", "SOC 31", "SOC 33",
                                                                                            "SOC 35", "SOC 37", "SOC 39", "SOC 41", "SOC 43", "SOC 45", "SOC 47", "SOC 49", "SOC 51", "SOC 53", "SOC 55")),
                                    textOutput("soc")))), 
                  fluidRow(dataTableOutput("stwTable")) 
         ), # end tabPanel
       
          tabPanel("Occupation Comparisons")
          
          ),#end navbar
       
       #end STW vs Non-STW-------------
       tabPanel(
         "Data Sources",
         h3("Data Sources", align = "center", style = "margin-bottom: 50px"),
  
         fluidRow(
           column(1),
           column(3, h4("Burning Glass Technologies (BGT)")),
           column(7, wellPanel(p("Burning Glass Technologies is a market analytics firm based in Boston, 
                                 MA, that uses AI technologies to collect and host a massive repository of 
                                 workforce and employment data. Data are collected using a web-crawling technique
                                 that uses computer programs called spiders to browse approximately 50,000+ online 
                                 job boards, corporate websites, and other places where job ads are posted and extract 
                                 more than 70 variables per advertisement to create this repository of jobs data."))),
           column(1)
         ),
         hr(),
         fluidRow(column(1),
                  column(3, h4("Job Openings and Labor Turnover Survery (JOLTS)")),
                  column(7, wellPanel(p("Job Openings and Labor Turnover Survey (JOLTS)
                                        estimates are based on a national sample of approximately 16,000 establishments,
                                        reporting data on factors such as total employment, job openings, hires, quits, 
                                        layoffs, and other separations. While the current national sample size is designed 
                                        to support estimates for major industries at the national level and total nonfarm 
                                        estimates at the regional level, the Bureau of Labor Statistics (BLS) is 
                                        currently researching the possibility of leveraging the sample to produce 
                                        model-assisted estimates at the state total nonfarm level. We
                                        use both the national and state estimates to benchmark the BGT data. "))),
                  column(1)
         ),
         hr(),
         fluidRow(column(1),
                  column(3, h4("Occupational Employment Statistics (OES)")),
                  column(7, wellPanel(p("The Occupational Employment Statistics (OES) program produces 
                                        employment and wage estimates annually for nearly 800 occupations. 
                                        These estimates are available for the nation as a whole, for 
                                        individual states, and for metropolitan and nonmetropolitan areas; 
                                        national occupational estimates for specific industries are also 
                                        available. The OES data are used for comparisons with the BGT data."))),
                  column(1)
                )
       )
     )
   )  


server <- function(input, output) {
  
  
  data <- read.csv("prof.csv", col.names = c("Variable", "Completeness", "Validity", "Uniqueness", "Year"))
  data$Description <- c("Unique identifier generated by BGT",
                        "Date the job posting was spidered", 
                        "State where job is located", 
                        "Occupation code of the job assigned", 
                        "SOC code title", 
                        "Latitude", 
                        "Longitude",
                        "Minimum education required", 
                        "Maximum education required")
  #rendering profiling table
  output$profile <- renderDataTable({

    DT::datatable(data[data$Year == input$prof_select, c("Variable", "Description", "Completeness", "Validity", "Uniqueness")],
                  options = list(dom = 't'), rownames = FALSE)
    
  })  
  
  output$jobsByYearOrRegion <- renderPlot({
    if(input$select == "National"){
      total_wide <- read.csv("jobsByYear.csv")
      ggplot(total_wide, aes(x= year, xend = year, y = bgt, yend = jolts)) + 
        geom_segment(color = "grey60") + 
        geom_point(y = total_wide$bgt, color = "#E57200", size = 3)+
        geom_point(y = total_wide$jolts, color = "#232D4B", size = 3) +
        scale_x_continuous(breaks = 2010:2019, 
                           limits =c(2010,2019)) + 
        scale_y_continuous(breaks = seq(0, 90000000, 10000000),  
                           labels = scales::comma, 
                           limits = c(0, 90000000),
                           expand = c(0, 0))+
        theme_minimal() +
        labs(y = "Number of Job Openings/Ads", 
             x = "", 
             title = "",
             subtitle= "")
    } else {
      total_wide_region <- read.csv("total_wide_region.csv")
      ggplot(total_wide_region, aes(x = year, xend = year, y = bgt, yend = jolts)) + 
        geom_segment(color = "grey60") +
        geom_point(y = total_wide_region$bgt, color = "#E57200", size = 3)+
        geom_point(y = total_wide_region$jolts, color = "#232D4B", size = 3) +
        scale_y_continuous(labels = scales::comma, breaks = seq(0, 30000000, 5000000)) +  
        scale_x_continuous(breaks = c(2010:2019)) + 
        scale_color_manual(values=c("#E57200", "#232D4B")) +
        facet_wrap(~region) + 
        theme_minimal() +
        theme(plot.title = element_text(hjust = .5, size = 20), 
              plot.subtitle = element_text(size = 12),
              axis.title.x = element_blank(), 
              legend.position = "none", 
              strip.text.x = element_text(face = "bold",size = 12)) +
        labs(title = "",
             y = "Number of Job Openings/Ads",
             subtitle = "") 
      
    }
  })
  
  #Rendering statebins plot
  output$statebins <- renderPlot({
    
    #renderPlot height and width
    data <- read.csv("statebinsData.csv")
    
    viz_data <- data %>% filter(year == input$slide) 
    
    statebins(viz_data, state_col = "state", value_col = "per_diff", direction = 1, round = TRUE, 
              name = "Percent Difference", font_size = 5) + 
      theme_statebins() +
      scale_fill_gradient(low = "white", high = "#0E879C", na.value = "grey60", limits = c(0, 150)) +
      theme(plot.margin = margin(0,0,0,0),
            legend.position = c(.35, .9),
            legend.justification = c("right", "top"),
            legend.direction =  "horizontal") + 
      labs(fill = "Percent Difference")
  }) 
  
  
  #SOC definitions
  output$definitions <- renderTable({
    def <- read.csv("socDefinitions.csv")
    
    def$X <- NULL
    def
  })
    
  #Summary table 
  output$summary <- renderDataTable({
    
    data <- read.csv("occupation_groups.csv")
    
    #removes the x column by setting it to NULL
    data$X <- NULL
    
    names(data)[names(data) == "state"] <- "State"
    names(data)[names(data) == "year"] <- "Year"
    names(data)[names(data) == "per_diff"] <- "Percent Difference"
    names(data)[names(data) == "X11"] <- "SOC 11"
    names(data)[names(data) == "X13"] <- "SOC 13"
    names(data)[names(data) == "X15"] <- "SOC 15"
    names(data)[names(data) == "X17"] <- "SOC 17"
    names(data)[names(data) == "X19"] <- "SOC 19"
    names(data)[names(data) == "X21"] <- "SOC 21"
    names(data)[names(data) == "X23"] <- "SOC 23"
    names(data)[names(data) == "X25"] <- "SOC 25"
    names(data)[names(data) == "X27"] <- "SOC 27"
    names(data)[names(data) == "X27"] <- "SOC 27"
    names(data)[names(data) == "X29"] <- "SOC 29"
    names(data)[names(data) == "X31"] <- "SOC 31"
    names(data)[names(data) == "X33"] <- "SOC 33"
    names(data)[names(data) == "X35"] <- "SOC 35"
    names(data)[names(data) == "X37"] <- "SOC 37"
    names(data)[names(data) == "X39"] <- "SOC 39"
    names(data)[names(data) == "X41"] <- "SOC 41"
    names(data)[names(data) == "X43"] <- "SOC 43"
    names(data)[names(data) == "X45"] <- "SOC 45"
    names(data)[names(data) == "X47"] <- "SOC 47"
    names(data)[names(data) == "X49"] <- "SOC 49"
    names(data)[names(data) == "X51"] <- "SOC 51"
    names(data)[names(data) == "X53"] <- "SOC 53"
    names(data)[names(data) == "X55"] <- "SOC 55"
    names(data)[names(data) == "X.NA."] <- "SOC NA"
  
    viz_data <- data %>% filter(Year == input$slide) %>%
      mutate(`Percent Difference` = round(`Percent Difference`, 4))
    
    
    
    DT::datatable(viz_data,
                  options = list(dom = 't', pageLength = 51, scrollX = TRUE), rownames = FALSE)
      
    
  })
  
# Eduation output
  output$stw <- renderPlot({
    
    data <- read.csv("stw_edu.csv")
    
    statebins(data[data$year == input$slide2, ], state_col = "state", value_col = "nobach", 
             direction = 1, round = TRUE, name = "Percent of Job Ads", font_size = 5) + 
      theme_statebins() +
      scale_fill_gradient(low = "white",high = "#0E879C", na.value = "grey60", limits = c(0, 1)) +
      theme(plot.margin = margin(0,0,0,0),
            legend.position = c(.35, .9),
            legend.justification = c("right", "top"),
            legend.direction =  "horizontal") + 
      labs(fill = "Percent of Job Ads")
  })
  
  output$stwTable <- renderDataTable({
    table <- read.csv("stw_edu.csv")
    
    names(table)[names(table) == "state"] <- "State"
    names(table)[names(table) == "year"] <- "Year"
    names(table)[names(table) == "nobach"] <- "% No Bachelors"
    names(table)[names(table) == "X11"] <- "SOC 11"
    names(table)[names(table) == "X13"] <- "SOC 13"
    names(table)[names(table) == "X15"] <- "SOC 15"
    names(table)[names(table) == "X17"] <- "SOC 17"
    names(table)[names(table) == "X19"] <- "SOC 19"
    names(table)[names(table) == "X21"] <- "SOC 21"
    names(table)[names(table) == "X23"] <- "SOC 23"
    names(table)[names(table) == "X25"] <- "SOC 25"
    names(table)[names(table) == "X27"] <- "SOC 27"
    names(table)[names(table) == "X27"] <- "SOC 27"
    names(table)[names(table) == "X29"] <- "SOC 29"
    names(table)[names(table) == "X31"] <- "SOC 31"
    names(table)[names(table) == "X33"] <- "SOC 33"
    names(table)[names(table) == "X35"] <- "SOC 35"
    names(table)[names(table) == "X37"] <- "SOC 37"
    names(table)[names(table) == "X39"] <- "SOC 39"
    names(table)[names(table) == "X41"] <- "SOC 41"
    names(table)[names(table) == "X43"] <- "SOC 43"
    names(table)[names(table) == "X45"] <- "SOC 45"
    names(table)[names(table) == "X47"] <- "SOC 47"
    names(table)[names(table) == "X49"] <- "SOC 49"
    names(table)[names(table) == "X51"] <- "SOC 51"
    names(table)[names(table) == "X53"] <- "SOC 53"
    names(table)[names(table) == "X55"] <- "SOC 55"

    viz_data <- table%>%filter(Year == input$slide2) 
    
    DT::datatable(viz_data,
                  options = list(dom = 't', pageLength = 51, scrollX = TRUE), rownames = FALSE)
    
  })
  
  output$soc <- renderText({
    if(input$definition == "SOC 11"){
      print("Management Occupations")
    }else if(input$definition == "SOC 13"){
      print("Business and Financial Operations Occupations")
    }else if(input$definition == "SOC 15"){
      print("Computer and Mathematical Occupations")
    }else if(input$definition == "SOC 17"){
      print("Architecture and Engineering Occupations")
    }else if(input$definition == "SOC 19"){
      print("Life, Physical, and Social Science Occupations")
    }else if(input$definition == "SOC 21"){
      print("Community and Social Service Occupations")
    }else if(input$definition == "SOC 23"){
      print("Legal Occupations")
    }else if(input$definition == "SOC 25"){
      print("Educational Instruction and Library Occupations")
    }else if(input$definition == "SOC 27"){
      print("Arts, Design, Entertainment, Sports, and Media Occupations")
    }else if(input$definition == "SOC 29"){
      print("Healthcare Practitioners and Technical Occupations")
    }else if(input$definition == "SOC 31"){
      print("Healthcare Support Occupations")
    }else if(input$definition == "SOC 33"){
      print("Protective Service Occupations")
    }else if(input$definition == "SOC 35"){
      print("Food Preperation and Serving Related Occupations")
    }else if(input$definition == "SOC 37"){
      print("Building and Grounds Cleaning and Maintenance Occupations")
    }else if(input$definition == "SOC 39"){
      print("Personal Care and Service Occupations")
    }else if(input$definition == "SOC 41"){
      print("Sales and Related Occupations")
    }else if(input$definition == "SOC 43"){
      print("Office and Administrative Support Occupations")
    }else if(input$definition == "SOC 45"){
      print("Farming, Fishing, and Forestry Occupations")
    }else if(input$definition == "SOC 47"){
      print("Construction and Extraction Occupations")
    }else if(input$definition == "SOC 49"){
      print("Installation, Maintenance, and Repair Occupations")
    }else if(input$definition == "SOC 51"){
      print("Production Occupations")
    }else if(input$definition == "SOC 53"){
      print("Transportation and Material Moving Occupations")
    }else{
      print("Military Specific Occupations")
    }
  })
  
  
  
  output$soc2 <- renderText({
    if(input$definition2 == "SOC 11"){
      print("Management Occupations")
    }else if(input$definition2 == "SOC 13"){
      print("Business and Financial Operations Occupations")
    }else if(input$definition2 == "SOC 15"){
      print("Computer and Mathematical Occupations")
    }else if(input$definition22 == "SOC 17"){
      print("Architecture and Engineering Occupations")
    }else if(input$definition2 == "SOC 19"){
      print("Life, Physical, and Social Science Occupations")
    }else if(input$definition2 == "SOC 21"){
      print("Community and Social Service Occupations")
    }else if(input$definition2 == "SOC 23"){
      print("Legal Occupations")
    }else if(input$definition2 == "SOC 25"){
      print("Educational Instruction and Library Occupations")
    }else if(input$definition2 == "SOC 27"){
      print("Arts, Design, Entertainment, Sports, and Media Occupations")
    }else if(input$definition2 == "SOC 29"){
      print("Healthcare Practitioners and Technical Occupations")
    }else if(input$definition2 == "SOC 31"){
      print("Healthcare Support Occupations")
    }else if(input$definition2 == "SOC 33"){
      print("Protective Service Occupations")
    }else if(input$definition2 == "SOC 35"){
      print("Food Preperation and Serving Related Occupations")
    }else if(input$definition2 == "SOC 37"){
      print("Building and Grounds Cleaning and Maintenance Occupations")
    }else if(input$definition2 == "SOC 39"){
      print("Personal Care and Service Occupations")
    }else if(input$definition2 == "SOC 41"){
      print("Sales and Related Occupations")
    }else if(input$definition2 == "SOC 43"){
      print("Office and Administrative Support Occupations")
    }else if(input$definition2 == "SOC 45"){
      print("Farming, Fishing, and Forestry Occupations")
    }else if(input$definition2 == "SOC 47"){
      print("Construction and Extraction Occupations")
    }else if(input$definition2 == "SOC 49"){
      print("Installation, Maintenance, and Repair Occupations")
    }else if(input$definition2 == "SOC 51"){
      print("Production Occupations")
    }else if(input$definition2 == "SOC 53"){
      print("Transportation and Material Moving Occupations")
    }else{
      print("Military Specific Occupations")
    }
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server) 

