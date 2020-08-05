library(shiny) 
library(dplyr)
library(statebins)
library(ggplot2)
library(data.table)
library(rsconnect)
library(DT)
library(lubridate) 
library(tidyr) 



statesWithDc <- c(state.name, "District of Columbia")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



ui <- fluidPage( 
  HTML('<script src="//use.typekit.net/tgy5tlj.js"></script>'),
  HTML('<script>try{Typekit.load();}catch(e){}</script>'),
  theme = "theme.css",
  title = "DSPG2020STW",  

   navbarPage( title = "Skilled Technical Workforce",

       tabPanel("About",
                fluidRow(column(3, tags$a(tags$img(height = "100%", width = "70%", src = "biilogo.png", align = "left" ), href="https://biocomplexity.virginia.edu/")),
                         column(6, h1("Skilled Techinical Workforce")),
                         column(3, tags$a(tags$img(height = "45%", width = "50%", src = "nsf-ncses.png", align = "right"), href="https://www.nsf.gov/statistics/"))
                         ),
                h5("Project"),
                p("A job in the skilled technical workforce (STW) is one that is open to an individual without a bachelor’s degree who has a high level of knowledge in a technical domain. The United States needs a STW to foster innovation and remain competitive in the global economy, but findings by the National Academies’ 
                in Building America’s Skilled Technical Workforce (2017) indicate the United States is not adequately developing and sustaining the STW needed to compete in the 21st century; they project that by 2022 the United States will have 3.4 million unfilled STW jobs. Our understanding of this shortfall is in part due to 
                data deficits that prohibit our ability to describe and quantify the skill formation pathways that lead to employment in the STW. These data deficits hinder the ability of policy makers to develop workforce programs, the ability of educators to develop relevant training programs, and the ability of job seekers to make 
                informed decisions regarding the nondegree credentials that will lead to employment in the STW."), 
                p("The work presented here was done by students in the 2020 Data Science for the Public Good Program. The project explores the fitness-for-use of the", tags$a(href = "https://www.burning-glass.com/","Burning Glass Technologies"),"job ad data to help fill in some of the STW data gaps 
                by benchmarking these data against the Bureau of Labor Statistics",tags$a(href = "https://www.bls.gov/jlt/", "Job Opening and Turnover Survey (JOLTS)"), "national and experimental state job openings estimates and the", tags$a(href = "https://www.bls.gov/oes/home.htm", "Occupations Employment Statistics (OES)"),"state Standard Occupational Classification (SOC) estimates."),
                helpText(tags$em("National Academies of Sciences, Engineering, and Medicine (2017) Building America's Skilled Technical Workforce. Washington, DC: The National Academies Press.")),
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
                p("The ", tags$a(href = "https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program","Data Science for the Public Good (DSPG)")," Young Scholars program is a summer immersive program held at SDAD. Entering its seventh year, the program engages students from across the country 
                  to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. 
                  DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information 
                  generated within every community can be leveraged to improve quality of life and inform public policy."),
                h4("Team"),
                p("Vicki Lancaster, Principal Scientist"),
                p("Sarah McDonald, Acting Fellow"),
                p("Ian MacLeod, Intern"),
                p("Gina Fendley, Intern")
                
                ), # end about tab
                
       tabPanel("Data Profiling",  
                fluidRow(column(12, align = "center", h3("Burning Glass Job Ads Data Profiling, 2010-2019"))), 
                fluidRow(h4("Methodology"), 
                         p("The data profiling process includes three measures: completeness, validity, and uniqueness. Completeness is the percentage of observations for a variable 
                  that include a value. If the observation is missing, indicated with “NA”, then the observation is not complete. We measured completeness for each variable by
                  counting the number of non-missing values and dividing it by the total number of observations.
                  Validity is the percentage of values that are within a specified range for a variable. An observation of “NA” is valid. To measure validity, we defined the expected range for each variable:"),
                         fluidRow(dataTableOutput("validity_table")),
                         br(),
                         p("The number of observations that met the specified conditions were counted and added to the number of NAs. We divided this sum by the total number of observations to obtain the variable’s validity.
                  The last measure is uniqueness. Uniqueness is the number of valid, unique observations. Missing values are not included in uniqueness. 
                  If two or more observations are identical, together they add a value of 1 to the uniqueness measure. 
                  We obtained uniqueness by counting the number of distinct values for the variable.")
                ),
                
                fluidRow(
                  column(8,fluidRow(h4("Results"), dataTableOutput("profile"))), 
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
                br(),
                h4("Observations"),
                p("The greatest change in the completeness of the BGT job-ad variables that were profiled is the increase in completeness for the minimum and maximum number of years of education required. 
                        In 2010 the percentage completeness was 44.18 for minimum and 14.74 for maximum education required; in contrast, in 2019 the minimum was 99.94% and maximum 100.00%."),
                p("The number of job-ads have increased 186% from 2010 (11,687,110) to 2019 (33,452,673). Some of this can be attributed to the change in the number of online job boards visited each day by BGT (~32,000 in
                         2013 to ~50,000 in 2019) and some to the increase in the percentage of job-ads posted online which Carnevale et al. (2014) estimated at 60-70% in 2014 and BGT estimates at 85% in 2019."),
                p("BGT assigns a North American Industry Classification System (NAICS) code to a job ad based on the name of the employer. Since it is common practice for staffing companies not to 
                  disclose the name of the employer in online job ads this variable was missing in ~25% of the job ads regardless of the year. This prohibited any benchmarking to JOLTS using industry aggregations."),
                helpText(tags$em("Carnevale Anthony, Tamara Jayasundera, and Dmitri Repnikov. 2014. “Understanding Online Job Ads Data” Georgetown University Center on Education and the Workforce."))
       ),
       
       #end profiling tab------------------------------------------ 
       
       
       navbarMenu("BGT/JOLTS Benchmark", 
        tabPanel("National/Regional Comparisons", 
                fluidRow(column(12, align = "center", h3("Benchmarking BGT Job Ads to JOLTS Job Openings"))),
                fluidRow(
                  p("JOLTS data provides a snapshot of near term labor demand at a point in time, the last business day of the reference month. The number of job openings is estimated from a stratified survey of 16,400 U.S. public and private non-farm business establishments. 
                         While the JOLTS asks a nationally representative sample of employers about job openings, the data are aggregated by geography (national, regional), industry (21 NAIC sectors), business type (private, government), and time (month)."),
                  p("BGT job ad data are collected using a web crawling technique that uses computer programs called spiders to browse approximately 50,000 online job boards, corporate websites, and other places where job ads are posted and extracts more than 70 variables per advertisement to create the repository of jobs data. 
                  De-duplication of the job ad is performed once at the website level, to avoid counting the same posting that recurs across multiple days, and once at the aggregate level, to eliminate the same posting advertised on multiple sites. 
                  It is important to note BGT only measures new postings (a given posting appears only on the first month it is recorded) while JOLTS measures active postings (the same posting can appear in two or more consecutive months if time to fill is more than 30 days).")),
                br(),
                fluidRow(column(1),
                         column(5, align = "center",
                                p("The", tags$span(' blue ', style = "background-color: #232D4B; color: white;border-radius: 25px; white-space: pre-wrap;"), 
                                  "dots show JOLTS job opening estimates, and the", br(), tags$span(' orange ', style = "background-color: #E57200; color: white;border-radius: 25px; white-space: pre-wrap;"), 
                                  "dots show the BGT job-ads."),
                                selectInput("select", "", choices = c("National", "Regional"), selected = "National"),
                                plotOutput("jobsByYearOrRegion", width = 500, height = 600)),
                     column(1),
                         column(4,
                                
                                fluidRow(
                                  
                                p(align = "right",
                                  "Northeast", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[7], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                  HTML("&nbsp;"),
                                  "Midwest", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[4], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                  HTML("&nbsp;"),
                                  "South", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[6], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                  HTML("&nbsp;"),
                                  "West", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[2],"; color: white;border-radius: 5px; white-space: pre-wrap;", sep = ""))
                                ), 
                                plotOutput("regions", height = "300px")
                                
                          
                                )), column(1)),
                fluidRow(column(2),
                       column(8, align = "center", h4("BGT/JOLTS Percent Difference by Region and Year"),
                               fluidRow(dataTableOutput("region_per_diff"))),
                       column(2)), 
                br(),
                fluidRow(
                h4("Observations"),
                p("There is a slight decrease in the percent difference, [(BGTi – JOLTSi)/JOLTSi]*100, from 2010 to 2019 for the national and regional comparisons. The West has the largest decrease from a percent difference of -65.39 in 2010 to -52.88 in 2019."),
                p("The increasing trends in the volume of BGT job ads and JOLT job openings are similar, as indicated with percent differences over time between -53% to -74%."), br()
                )),
                
        
        tabPanel("State Comparisons",  
             fluidRow(width = 12, align = "center", column(12, h3("Percent Difference Between BGT and JOLTS") )), 
             fluidRow(p("Since the JOLTS sample of 16,000 establishments does not directly support the production of sample based state estimates, in 2019, 
                        the JOLTS program began publishing model-assisted estimates at the state total nonfarm level. The estimate is a composite of the current 
                        JOLTS sample, data from the Quarterly Census of Employment and Wages (QCEW), and data from the Current Employment Statistics (CES) program. 
                        The Composite Synthetic job openings are a function of the ratio of industry-regional job openings and hires. This ratio of published job 
                        openings to hires is applied to model hires estimates to derive model job opening estimates.")),
             fluidRow(width = 12, column(5), 
                      column(2, sliderInput("slide", label = NULL, min = 2010, max = 2019, value = 2014, sep = ""))),
              fluidRow(align = "center", 
                       column(1),
                       column(10, 
                              p(strong("Percent Difference: "), 
                                "(0, -20]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[6], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                HTML("&nbsp;"),
                                "(-20, -40]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[3], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                HTML("&nbsp;"),
                                "(-40, -60]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[1], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                HTML("&nbsp;"),
                                "(-60, -80]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[2], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                HTML("&nbsp;"),
                                "(-80, -100]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[7],"; color: white;border-radius: 5px; white-space: pre-wrap;", sep = ""))
                               )),
                       column(1)),
             fluidRow(column(1), 
                      column(10, plotOutput("statebins", width= "100%", height = "600px")),
                      column(1)),
             
             br(),
             fluidRow(column(3),
                      column(6, align = "center", h4("BGT/JOLTS Percent Difference by State Over Time")),
                      column(3)),
             fluidRow(column(1),
               column(2, selectInput("stateGina", "Select State", choices = (statesWithDc))),
                      column(6,  plotOutput("gina")),
                      column(3)),
             
             fluidRow(h4("Observations"),
                      p("Unlike comparisons to JOLTS at the national level where JOLTS is the ground truth and differences with BGT are a function of comparing job openings to job ads, how the monthly 
                        counts are calculated, and the changing landscape of online job ads, with state comparisons there is no ground truth. 
                        JOLTS state estimates are experimental and do not provide with standard errors. "),
                      
                      p("The state percent differences are extremely volatile. Alaska, District of Columbia, Maine, and South Dakota all have percent differences that go 
                        from negative to positive or close to zero. Maine goes from a -75 percent difference to a +10 percent difference.")
                    
                      ),
             
             fluidRow(column(2,  
                             wellPanel(
                               selectInput("definition2", "SOC Definition", choices = c("SOC 11", "SOC 13", "SOC 15",
                                                                                       "SOC 17", "SOC 19", "SOC 21", "SOC 23", "SOC 25", "SOC 27", "SOC 29", "SOC 31", "SOC 33",
                                                                                       "SOC 35", "SOC 37", "SOC 39", "SOC 41", "SOC 43", "SOC 45", "SOC 47", "SOC 49", "SOC 51", "SOC 53", "SOC 55")),
                               
                               textOutput("soc2"))),
                      column(10, p("District of Columbia has either the smallest percent difference or is one of the top three states with the smallest percent difference 
                                   for all ten years. The D.C. SOCs with the largest percentage are Management Occupations, Business Financial Operations Occupations, 
                                   and Computer and Mathematical occupations which for all ten years accounted for ~50% of the job ads."),
                             p("The states with the largest percent difference, ~-75%, had SOCs in the double digits for Healthcare Practitioners and Technical Occupations and Sales and related Occupations."))), 
             
             fluidRow(column(1),
                      column(10, h4("BGT/JOLTS Percent Difference and Percent of BGT Job Ads in each Major Occupation Group by State"), align = "center"),
                      column(1)),
             fluidRow(dataTableOutput("summary")) 
        ) # end tabPanel
        
        
        ),#end navbar
                
       #end Jolts vs BGT tab-----------------
       tabPanel("BGT/OES Benchmark"),
       
         tabPanel("BGT Education",
                  fluidRow(width = 12, align = "center", column(12, h3("Percent of BGT Jobs Ads Requiring a Two-Year Degree or Less") )), 
                  fluidRow(width = 12, p("For the map below, we calculated the percent of BGT Job Ads that have a minimum education value of 0, 12, or 14, 
                                         which means the job required no education, a high school education, or a two-year degree.")),
                  fluidRow(width = 12, column(5), 
                           column(2, sliderInput("slide2", label = NULL, min = 2010, max = 2019, value = 2014, sep = "")),
                           column(5)),
                  
                  fluidRow(align = "center", 
                           column(1),
                           column(10, 
                                  p(strong("Percent Difference: "), 
                                    "(40, 50]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[6], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                    HTML("&nbsp;"),
                                    "(50, 60]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[3], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                    HTML("&nbsp;"),
                                    "(60, 70]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[1], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                    HTML("&nbsp;"),
                                    "(70, 80]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[2], "; color: white;border-radius: 5px; white-space: pre-wrap;", sep = "")),
                                    HTML("&nbsp;"),
                                    "(80, 90]", tags$span(HTML("&emsp;&nbsp;"), style = paste("background-color: ", cbPalette[7],"; color: white;border-radius: 5px; white-space: pre-wrap;", sep = ""))
                                  )),
                           column(1)),
                  
                  
                  fluidRow(column(1), 
                           column(10, plotOutput("stw", width= "100%", height = "600px")),
                           column(1)), 
                  fluidRow(h4("Observations"),
                           p("We see the District of Columbia has the smallest percentage, which means the many BGT Job Ads required a bachelor’s degree or higher, 
                             or did not have an explicit minimum education value listed."), 
                           p("West Virginia and Mississippi have multiple instances where the percent of BGT Job Ads that require a two-year degree or less fall in the 80-90% range.")), 
                  fluidRow(column(2,  
                                  wellPanel(
                                    selectInput("definition", "SOC Definition", choices = c("SOC 11", "SOC 13", "SOC 15",
                                                                                            "SOC 17", "SOC 19", "SOC 21", "SOC 23", "SOC 25", "SOC 27", "SOC 29", "SOC 31", "SOC 33",
                                                                                            "SOC 35", "SOC 37", "SOC 39", "SOC 41", "SOC 43", "SOC 45", "SOC 47", "SOC 49", "SOC 51", "SOC 53", "SOC 55")),
                                    textOutput("soc"))),
                           column(10, 
                                  p("In the table below, we classified BGT Job Ads using the Rothwell’s definition, and provide a percent of STW Job Ads in each major occupation group alongside of the percent of job ads requiring a two-year degree or less."),
                                  p("We see that the District of Columbia, which has the lowest percentage of job ads requiring a two-year degree or less, has a relatively large percentage of STW jobs in the SOC 15 group, which is Computer and Mathematical Occupations."),
                                  p("The largest percentage of STW jobs are in SOC 29 and SOC 49, which are Healthcare Practitioners and Technical Occupations and Installation, Maintenance, and Repair Occupations."))), 
                  fluidRow(column(1),
                           column(10, h4("Percent of Job Ads Requiring a Two-Year Degree or Less and Percent of STW BGT Job Ads by Major Occupation Groups"), align = "center"),
                           column(1)),
                  
                           
                  fluidRow(dataTableOutput("stwTable")) 
         ), # end tabPanel
       
       
       
       #STW Crosswalk
       tabPanel( "STW Crosswalk", 
         fluidRow(width = 12, align = "center", column(12, h3("STW Crosswalk") )), 
         fluidRow(p("The construction of the crosswalk follows Rothwell’s (2015) definition of the STW. Data sources used to construct the crosswalk are:"),
                  p(tags$a(href = "https://www.onetcenter.org/crosswalks.html#soc", "2010 O*NET-SOC occupations to 2010 SOC occupations")), 
                  p(tags$a(href = "https://www.onetcenter.org/db_releases.html","O*NET 15.1 Database: Content Model Knowledge data associated with each O*NET-SOC occupation")), 
                  p(tags$a(href = "https://www.onetcenter.org/db_releases.html","O*NET 15.1 Database: Education percent frequency data associated with each O*NET-SOC occupation")),
                  p(tags$a(href = "https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html", "2010 Census Occupation Codes with Crosswalk")),
                  helpText("Jonathan Rothwell, September 1, 2015. Defining Skilled Technical Work. Prepared for National Academies Board on Science, Technology, and Economic Policy Project on the “Supply Chain for Middle-Skilled Jobs: Education, Training and Certification Pathways”. https://sites.nationalacademies.org/cs/groups/pgasite/documents/webpage/pga_167744.pdf")),
         fluidRow(dataTableOutput("stwXwalk")) 
         
         
       ),
       
       
       
       
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
  sector <- read.csv("sector_profiling.csv", col.names = c("Variable", "Completeness", "Validity", "Uniqueness", "Year"))
  data$Description <- c("Unique identifier generated by BGT",
                        "Date the job posting was spidered", 
                        "State where job is located", 
                        "Occupation code of the job assigned", 
                        "SOC code title", 
                        "Latitude", 
                        "Longitude",
                        "Minimum education required", 
                        "Maximum education required")
  sector$Description <- "2-Digit NAICS code "
  
  data <- rbind(data, sector)
  
  data$Completeness = as.numeric(data$Completeness) * 100
  data$Completeness = sprintf(data$Completeness, fmt = "%#.2f")
  
  data$Validity = as.numeric(data$Validity) * 100
  data$Validity = sprintf(data$Validity, fmt = "%#.2f")
  
  names(data)[names(data) == "Completeness"] <- "% Completeness"
  names(data)[names(data) == "Validity"] <- "% Validity"
  
  
  
  #rendering profiling table
  output$profile <- renderDataTable({

    DT::datatable(data[data$Year == input$prof_select, c("Variable", "Description", "% Completeness", "% Validity", "Uniqueness")],
                  options = list(dom = 't'), rownames = FALSE)
    
  })
  
  library(dplyr)
  total_wide <- read.csv("nation_region_year.csv")
  total_wide_table <-total_wide %>% select(year, per_change, region) %>% spread(key = region, value = per_change) %>% select("Year" = year, National, Northeast, Midwest, South, West)
  total_wide_table$National <- sprintf(total_wide_table$National,fmt = "%#.2f")
  total_wide_table$Northeast <- sprintf(total_wide_table$Northeast,fmt = "%#.2f")
  total_wide_table$Midwest <- sprintf(total_wide_table$Midwest,fmt = "%#.2f")
  total_wide_table$South <- sprintf(total_wide_table$South,fmt = "%#.2f")
  total_wide_table$West <- sprintf(total_wide_table$West,fmt = "%#.2f")
  
  output$region_per_diff <- renderDataTable({
    DT::datatable(total_wide_table, options = list(dom = 't'), rownames = FALSE)
  })
  
  
  output$regions <- renderPlot({
    library(statebins)
    states <- data.frame(state.name, state.region)
    states <- rbind(states, c("District of Columbia", "South"))
    levels(states$state.region)[levels(states$state.region)=="North Central"] <- "Midwest"
    
    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    
    statebins(state_data = states, state_col = "state.name", value_col = "state.region", ggplot2_scale_function = scale_fill_manual,
              font_size = 5, 
              round = TRUE,
              values = c("South" = cbPalette[6], "Midwest" = cbPalette[4],"Northeast" = cbPalette[7], 
                         "West" = cbPalette[2])) +
      theme_statebins()+
      theme(plot.margin = margin(0,0,0,0), 
            legend.position = "none") 
    
    
  }) 

  output$jobsByYearOrRegion <- renderPlot({
    if(input$select == "National"){
      ggplot(total_wide[total_wide$region == "National", ], aes(x= year, xend = year, y = bgt, yend = jolts)) + 
        geom_segment(color = "grey60") + 
        geom_point(y = total_wide[total_wide$region == "National", "bgt"], color = "#E57200", size = 3)+
        geom_point(y = total_wide[total_wide$region == "National", "jolts"], color = "#232D4B", size = 3) +
        scale_x_continuous(breaks = 2010:2019, 
                           limits =c(2010,2019), 
                           minor_breaks =  NULL) + 
        scale_y_continuous(breaks = seq(0, 100000000, 25000000),  
                           labels = scales::comma, 
                           limits = c(0, 100000000),
                           expand = c(0, 0))+
        theme_minimal() +
        labs(y = "Number of Job Ads and Job Openings", 
             x = "", 
             title = "",
             subtitle= "") +
        coord_flip() +
        theme(plot.margin = unit(c(5.5, 50, 5.5, 5.5), "pt"),
              plot.title = element_text(face = "bold", hjust = 0.5), 
              axis.text.x = element_text(size = 12), 
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14))
    } else {
      ggplot(total_wide[total_wide$region == "Northeast"|total_wide$region == "Midwest"|total_wide$region == "South"|total_wide$region == "West",], aes(x = year, xend = year, y = bgt, yend = jolts)) + 
        geom_segment(color = "grey60") +
        geom_point(y = total_wide[total_wide$region == "Northeast"|total_wide$region == "Midwest"|total_wide$region == "South"|total_wide$region == "West","bgt"], color = "#E57200", size = 3)+
        geom_point(y = total_wide[total_wide$region == "Northeast"|total_wide$region == "Midwest"|total_wide$region == "South"|total_wide$region == "West", "jolts"], color = "#232D4B", size = 3) +
        scale_y_continuous(labels = scales::comma, breaks = seq(0, 30000000, 10000000), limits = c(0, 35000000)) +  
        scale_x_continuous(breaks = c(2010:2019)) + 
        scale_color_manual(values=c("#E57200", "#232D4B")) +
        facet_wrap(~region, ncol = 1) + 
        theme_minimal() +
        theme(plot.title = element_text(hjust = .5, size = 20), 
              plot.subtitle = element_text(size = 12),
              axis.title.x = element_blank(), 
              axis.title.y= element_blank(), 
              legend.position = "none", 
              strip.text.x = element_text(face = "bold",size = 12), 
              axis.text.x = element_text(size = 12), 
              axis.text.y = element_text(size = 12)) +
        labs(title = "",
             y = "Number of Job Openings/Ads",
             subtitle = "") +
        coord_flip() 
      
    }
  })
  
  #Rendering statebins plot
  output$statebins <- renderPlot({
  
    
    data <- read.csv("state_year.csv")
    
    viz_data <- data %>% filter(year == input$slide) 
    
    
    
    mutate(viz_data, value =  ifelse(per_change <= 0 & per_change > -20, "[0, -20)",
                                    ifelse(per_change <= -20 & per_change > -40, "[-20, -40)", 
                                           ifelse(per_change <= -40 & per_change > -60, "[-40, -60)", 
                                                  ifelse(per_change <= -60 & per_change > -80, "[-60, -80)", 
                                                         ifelse(per_change <= -80 & per_change > -100, "[-80, -100)", NA)))))) %>%
      statebins(ggplot2_scale_function = scale_fill_manual,
                font_size = 5, 
                round = TRUE,
                values = c("[0, -20)" = cbPalette[6], "[-20, -40)" = cbPalette[3],"[-40, -60)" = cbPalette[1], 
                           "[-60, -80)" = cbPalette[2], "[-80, -100)"= cbPalette[7] )) +
      theme_statebins()+
      theme(plot.margin = margin(0,0,0,0), 
            legend.position = "none") 
    
    
  
  })
  
  
  
  
  #Rendering Gina's timechart
  output$gina <- renderPlot({
    gina <- read.csv("state_month.csv")
    gina$time <- as_date(parse_date_time(gina$date, "ym"))
    graphic_data <- gina[gina$State == input$stateGina, ]
    
    ggplot(graphic_data) +
     geom_line(aes(x=time, y=per_change),color="#E57200")  +
      theme_minimal() +
       scale_y_continuous(limits = c(-100, 20)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
         labs(title = "", x = "", y = "")
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
    names(data)[names(data) == "state"] <- "State"
    names(data)[names(data) == "year"] <- "Year"
    names(data)[names(data) == "per_change"] <- "Percent Change"
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
  
    viz_data <- data %>% filter(Year == input$slide) 
    viz_data[, -c(1:2)] <- lapply(viz_data[, -c(1:2)], sprintf, fmt = "%#.2f")
    
    
    DT::datatable(viz_data,
                  options = list(dom = 't', pageLength = 51, scrollX = TRUE), rownames = FALSE)
      
    
  })
  
# Eduation output
  output$stw <- renderPlot({
    
    data <- read.csv("stw_edu.csv")
    

    mutate(data[data$year == input$slide2, ], value = ifelse(nobach >= 40 & nobach < 50, "[40, 50)",
                                                     ifelse(nobach >= 50 & nobach < 60, "[50, 60)", 
                                                            ifelse(nobach >= 60 & nobach < 70, "[60, 70)", 
                                                                   ifelse(nobach >= 70 & nobach < 80, "[70, 80)", 
                                                                          ifelse(nobach >= 80 & nobach < 90, "[80, 90)", NA)))))) %>%
      statebins(ggplot2_scale_function = scale_fill_manual,
                round = TRUE, font_size = 5, 
                values = c("[40, 50)" = cbPalette[6], "[50, 60)" = cbPalette[3],"[60, 70)" = cbPalette[1], 
                           "[70, 80)" = cbPalette[2], "[80, 90)"= cbPalette[7] )) +
      theme_statebins()+
      theme(plot.margin = margin(0,0,0,0),
            legend.position = "none")  
  })
  
  output$stwTable <- renderDataTable({
    table <- read.csv("stw_edu.csv")
    
    names(table)[names(table) == "state"] <- "State"
    names(table)[names(table) == "year"] <- "Year"
    names(table)[names(table) == "nobach"] <- "% Two-Year Degree or Less"
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
    
    table[, -c(1:2)] <- lapply(table[, -c(1:2)], sprintf, fmt = "%#.2f")
    
    DT::datatable(table[table$Year == input$slide2, ],
                  options = list(dom = 't', pageLength = 51, scrollX = TRUE), rownames = FALSE)
    
  })
  
  
  
  output$stwXwalk <- renderDataTable({
    
    
    xwalk <- read.csv("rothwell_copy.csv", colClasses = c(X2010.Census.Code = "character"), na.strings = ".")
    
    
    names(xwalk)[names(xwalk) == "X2010.SOC.Code"] <- "2010 SOC Code"
    names(xwalk)[names(xwalk) == "O.NET.SOC.2010.Code"] <- "ONET-SOC 2010 Code"
    names(xwalk)[names(xwalk) == "O.NET.SOC.2010.Title"] <- "ONET-SOC 2010 Code Title"
    names(xwalk)[names(xwalk) == "X2010.SOC.Title"] <- "2010 SOC Title"
    names(xwalk)[names(xwalk) == "education_STW"] <- "Rothwell Education Requirement"
    names(xwalk)[names(xwalk) == "knowledge_STW"] <- "Rothwell Knowledge Requirement"
    names(xwalk)[names(xwalk) == "rothwell_STW"] <- "Rothwell Category"
    names(xwalk)[names(xwalk) == "Occupation.2010.Description"] <- "Occupation 2010 Description"
    names(xwalk)[names(xwalk) == "X2010.Census.Code"] <- "2010 Census Code"
    
    
    
    DT::datatable(xwalk,
                  options = list(dom = 'tp', pageLength = 50, scrollX = TRUE), rownames = FALSE)
    
  })  
  
  output$validity_table <- renderDataTable({
    
    data <- read.csv("validity_table.csv")
    data <- rbind(data, c("sector", "the observation is either 11, 21, 22, 23, 31-33, 42, 44-45, 48-49, 51, 52, 53, 54, 55, 56, 61, 62,  71, 72, 81, 92"))
    DT::datatable(data, options = list(dom= 't'), rownames = F)
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
    }else if(input$definition2 == "SOC 17"){
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

