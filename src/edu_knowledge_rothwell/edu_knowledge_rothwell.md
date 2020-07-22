Download the O\*NET Knowledge survey results and O\*NET SOC crosswalk.
Only level data are kept (Methodology Step 2). Each ONETSOC code is
matched to a SOC Code with the 2010 ONET SOC crosswalk (Step 1).

    library(readxl)
    library(dplyr)

    #download.file(url = "https://www.onetcenter.org/dl_files/database/db_24_3_excel/Knowledge.xlsx", destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/knowledge.xlsx")

    #download.file(url = "https://www.onetcenter.org/taxonomy/2010/soc/2010_to_SOC_Crosswalk.xls?fmt=xls", destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010_to_SOC_Crosswalk.xls")

    knowledge <- read_xlsx("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/knowledge.xlsx")
    xwalk_soc <- read_xls("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010_to_SOC_Crosswalk.xls", skip =3)

    knowledge <- knowledge[knowledge$`Scale Name` == "Level", ]


    knowledge <- merge(knowledge[, c("O*NET-SOC Code", "Element Name",  "Scale Name", "Data Value")], xwalk_soc[, c("O*NET-SOC 2010 Code", "2010 SOC Code")],
          by.x = "O*NET-SOC Code", by.y = "O*NET-SOC 2010 Code", all =  T)

Potentially STW competency categories are listed in the variable
*element\_stw*.

The mean knowledge score is computed for each SOC and competency
category (Step 2, Step 4). O\*NET Categories with at least a 4.5 and
competency categories defined in *element\_stw* will be further
considered for STW.

    element_stw <- c(
    "Biology",                    "Building and Construction",
    "Chemistry",                  "Computers and Electronics", 
     "Design",                     "Economics and Accounting",  
    "Engineering and Technology", "Food Production",           
     "Mathematics",                "Mechanical",                
    "Medicine and Dentistry",     "Physics",                   
    "Production and Processing",  "Telecommunications")

    knowledge_soc <- knowledge %>% 
      group_by(`2010 SOC Code`, `Element Name`) %>% # group by SOC, competency category
      summarize(`Mean Data Value` = mean(`Data Value`)) %>% #take mean for each SOC, competency category grouping
      mutate(`Category STW Indicator` = ifelse(`Element Name` %in% element_stw & `Mean Data Value`>= 4.5, 1, 
                                               ifelse(is.na(`Element Name`)==T|is.na(`Mean Data Value`)==TRUE, NA, 0))) #%>% # if STW competency category and mean value is greater than 4.5, that specific SOC, competency category combination is considered STW for knowledge

    ## `summarise()` regrouping output by '2010 SOC Code' (override with `.groups` argument)

    knowledge_soc <- knowledge_soc %>%
      select(!`Element Name` & !`Mean Data Value`) %>%
      group_by(`2010 SOC Code`) %>%
      summarize(knowledge_STW = ifelse(length(`2010 SOC Code`)==1 & is.na(`Category STW Indicator`)==T, NA, 
                                       ifelse(length(`2010 SOC Code`)>1 & sum(`Category STW Indicator`, na.rm = T) > 0,1, 0))) %>% summarize(knowledge_STW = unique(knowledge_STW))

    ## `summarise()` regrouping output by '2010 SOC Code' (override with `.groups` argument)

    ## `summarise()` ungrouping output (override with `.groups` argument)

    # to pass the knowledge STW definition, only one of the SOC, copetency category combinations must have a 1

    head(knowledge_soc,10)

    ## # A tibble: 10 x 2
    ##    `2010 SOC Code` knowledge_STW
    ##    <chr>                   <dbl>
    ##  1 11-1011                     0
    ##  2 11-1021                     0
    ##  3 11-1031                    NA
    ##  4 11-2011                     0
    ##  5 11-2021                     0
    ##  6 11-2022                     0
    ##  7 11-2031                     0
    ##  8 11-3011                     0
    ##  9 11-3021                     1
    ## 10 11-3031                     1

We then download the Education data and calculate the percentage of
respondents without a bachelor's degree (Step 3). We then take the mean
percentage of respondents with less than a bachelor’s degree for each
SOC code (Step 4).

    #download.file(url = "https://www.onetcenter.org/dl_files/database/db_24_3_excel/Education%2C%20Training%2C%20and%20Experience.xlsx", destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/education.xlsx")

    education <- read_xlsx("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/education.xlsx")
    education <- education[education$`Element Name` == "Required Level of Education", c("O*NET-SOC Code","Title",  "Category", "Data Value")]

    education <- merge(education, xwalk_soc[, c("O*NET-SOC 2010 Code", "2010 SOC Code")],
          by.x = "O*NET-SOC Code", by.y = "O*NET-SOC 2010 Code", all = T)

    education <- education  %>%
      mutate(hasbach = ifelse(Category <= 5, "nobach", ifelse(Category > 5, "bach", NA))) %>% # if category is greater than 5, the percentage cooresponds to bachelor's degree or higher.
      group_by(`O*NET-SOC Code`, `Title`,  `2010 SOC Code`, hasbach) %>%
      summarize(`Percent No Bachelors` = sum(`Data Value`)) %>%
      filter(hasbach == "nobach"|is.na(hasbach)==TRUE) %>%
      select(!hasbach)

    ## `summarise()` regrouping output by 'O*NET-SOC Code', 'Title', '2010 SOC Code' (override with `.groups` argument)

    # calculate the percentage of respondents for each O*NET without a bachelor's degree

    education_soc <- education %>%
      group_by(`2010 SOC Code`) %>%
      summarize(`Mean Percent No Bachelors` = mean(`Percent No Bachelors`, na.rm = T), 
                education_STW = ifelse(`Mean Percent No Bachelors`> 50, 1, ifelse(is.na(`Mean Percent No Bachelors`)==T, NA, 0))) # take the mean percentage of respondents with less than a bachelor’s degree for each SOC code

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(education_soc, 10)

    ## # A tibble: 10 x 3
    ##    `2010 SOC Code` `Mean Percent No Bachelors` education_STW
    ##    <chr>                                 <dbl>         <dbl>
    ##  1 11-1011                                7.06             0
    ##  2 11-1021                               52.0              1
    ##  3 11-1031                              NaN               NA
    ##  4 11-2011                               31.7              0
    ##  5 11-2021                                9.36             0
    ##  6 11-2022                               17.4              0
    ##  7 11-2031                               11.2              0
    ##  8 11-3011                               71.7              1
    ##  9 11-3021                               35.2              0
    ## 10 11-3031                               14.7              0

    education <- merge(education, education_soc[, c("2010 SOC Code", "education_STW" )], by = "2010 SOC Code")

Finally, we merge our knowledge and education findings together. If
there is a "1" in both the knowledge and education tables, then a SOC
code is categorized as STW.

    rothwell <- merge(education, knowledge_soc, by = "2010 SOC Code", all =  T)

    rothwell$rothwell_STW <- ifelse(rothwell$knowledge_STW == 1 & rothwell$education_STW ==1, 1, ifelse(is.na(rothwell$knowledge_STW) == T & is.na(rothwell$education_STW) ==T, NA, 0))

    head(rothwell, 10)

    ##    2010 SOC Code O*NET-SOC Code                                     Title
    ## 1        11-1011     11-1011.00                          Chief Executives
    ## 2        11-1011     11-1011.03             Chief Sustainability Officers
    ## 3        11-1021     11-1021.00           General and Operations Managers
    ## 4        11-1031     11-1031.00                                      <NA>
    ## 5        11-2011     11-2011.00       Advertising and Promotions Managers
    ## 6        11-2011     11-2011.01                                      <NA>
    ## 7        11-2021     11-2021.00                        Marketing Managers
    ## 8        11-2022     11-2022.00                            Sales Managers
    ## 9        11-2031     11-2031.00 Public Relations and Fundraising Managers
    ## 10       11-3011     11-3011.00          Administrative Services Managers
    ##    Percent No Bachelors education_STW knowledge_STW rothwell_STW
    ## 1                 10.28             0             0            0
    ## 2                  3.85             0             0            0
    ## 3                 51.97             1             0            0
    ## 4                    NA            NA            NA           NA
    ## 5                 31.69             0             0            0
    ## 6                    NA             0             0            0
    ## 7                  9.36             0             0            0
    ## 8                 17.39             0             0            0
    ## 9                 11.18             0             0            0
    ## 10                71.74             1             0            0

We fix the case where registered nurses fail to meet the knowledge and
education criteria (Step 5).

    rothwell[rothwell$`2010 SOC Code` == "29-1141", "rothwell_STW"] <- 1

    #write.csv(rothwell, "/sfs/qumulo/qhome/sm9dv/dspg20STW/src/edu_knowledge_rothwell/rothwell.csv")

Merge with OCC
