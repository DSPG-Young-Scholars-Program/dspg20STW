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
          by.x = "O*NET-SOC Code", by.y = "O*NET-SOC 2010 Code")

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
      mutate(`Category STW Indicator` = ifelse(`Element Name` %in% element_stw & `Mean Data Value`>= 4.5, 1, 0)) %>% # if STW competency category and mean value is greater than 4.5, that specific SOC, competency category combination is considered STW for knowledge
      group_by(`2010 SOC Code`) %>%
      summarize(knowledge_STW = ifelse(sum(`Category STW Indicator`) > 0, 1, 0)) # to pass the knowledge STW definition, only one of the SOC, copetency category combinations must have a 1

    ## `summarise()` regrouping output by '2010 SOC Code' (override with `.groups` argument)

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(knowledge_soc,10)

    ## # A tibble: 10 x 2
    ##    `2010 SOC Code` knowledge_STW
    ##    <chr>                   <dbl>
    ##  1 11-1011                     0
    ##  2 11-1021                     0
    ##  3 11-2011                     0
    ##  4 11-2021                     0
    ##  5 11-2022                     0
    ##  6 11-2031                     0
    ##  7 11-3011                     0
    ##  8 11-3021                     1
    ##  9 11-3031                     1
    ## 10 11-3051                     1

We then download the Education data and calculate the percentage of
respondents without a bachelor's degree (Step 3). We then take the mean
percentage of respondents with less than a bachelor’s degree for each
SOC code (Step 4).

    #download.file(url = "https://www.onetcenter.org/dl_files/database/db_24_3_excel/Education%2C%20Training%2C%20and%20Experience.xlsx", destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/education.xlsx")

    education <- read_xlsx("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/education.xlsx")
    education <- education[education$`Element Name` == "Required Level of Education", c("O*NET-SOC Code","Title",  "Category", "Data Value")]

    education <- merge(education, xwalk_soc[, c("O*NET-SOC 2010 Code", "2010 SOC Code")],
          by.x = "O*NET-SOC Code", by.y = "O*NET-SOC 2010 Code")

    education <- education  %>%
      mutate(hasbach = ifelse(Category > 5, "bach", "nobach")) %>% # if category is greater than 5, the percentage cooresponds to bachelor's degree or higher.
        filter(hasbach == "nobach") %>%
        select(-hasbach)%>%
      select(!Category) %>%
      group_by(`O*NET-SOC Code`, `Title`,  `2010 SOC Code`) %>%
      summarize(`Percent No Bachelors` = sum(`Data Value`)) # calculate the percentage of respondents for each O*NET without a bachelor's degree

    ## `summarise()` regrouping output by 'O*NET-SOC Code', 'Title' (override with `.groups` argument)

    education_soc <- education %>%
      group_by(`2010 SOC Code`) %>%
      summarize(`Mean Percent No Bachelors` = mean(`Percent No Bachelors`), 
                education_STW = ifelse(`Mean Percent No Bachelors`> 50, 1, 0)) # take the mean percentage of respondents with less than a bachelor’s degree for each SOC code

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(education_soc, 10)

    ## # A tibble: 10 x 3
    ##    `2010 SOC Code` `Mean Percent No Bachelors` education_STW
    ##    <chr>                                 <dbl>         <dbl>
    ##  1 11-1011                                7.06             0
    ##  2 11-1021                               52.0              1
    ##  3 11-2011                               31.7              0
    ##  4 11-2021                                9.36             0
    ##  5 11-2022                               17.4              0
    ##  6 11-2031                               11.2              0
    ##  7 11-3011                               71.7              1
    ##  8 11-3021                               35.2              0
    ##  9 11-3031                               14.7              0
    ## 10 11-3051                               51.5              1

    education <- merge(education, education_soc[, c("2010 SOC Code", "education_STW" )], by = "2010 SOC Code")

Finally, we merge our knowledge and education findings together. If
there is a "1" in both the knowledge and education tables, then a SOC
code is categorized as STW.

    rothwell <- merge(knowledge_soc, education, by = "2010 SOC Code")

    rothwell$rothwell_STW <- ifelse(rothwell$knowledge_STW == 1 & rothwell$education_STW ==1, 1, 0)

    head(rothwell, 10)

    ##    2010 SOC Code knowledge_STW O*NET-SOC Code
    ## 1        11-1011             0     11-1011.00
    ## 2        11-1011             0     11-1011.03
    ## 3        11-1021             0     11-1021.00
    ## 4        11-2011             0     11-2011.00
    ## 5        11-2021             0     11-2021.00
    ## 6        11-2022             0     11-2022.00
    ## 7        11-2031             0     11-2031.00
    ## 8        11-3011             0     11-3011.00
    ## 9        11-3021             1     11-3021.00
    ## 10       11-3031             1     11-3031.01
    ##                                        Title Percent No Bachelors education_STW
    ## 1                           Chief Executives                10.28             0
    ## 2              Chief Sustainability Officers                 3.85             0
    ## 3            General and Operations Managers                51.97             1
    ## 4        Advertising and Promotions Managers                31.69             0
    ## 5                         Marketing Managers                 9.36             0
    ## 6                             Sales Managers                17.39             0
    ## 7  Public Relations and Fundraising Managers                11.18             0
    ## 8           Administrative Services Managers                71.74             1
    ## 9  Computer and Information Systems Managers                35.15             0
    ## 10                Treasurers and Controllers                 0.00             0
    ##    rothwell_STW
    ## 1             0
    ## 2             0
    ## 3             0
    ## 4             0
    ## 5             0
    ## 6             0
    ## 7             0
    ## 8             0
    ## 9             0
    ## 10            0

We fix the case where registered nurses fail to meet the knowledge and
education criteria (Step 5).

    rothwell[rothwell$`2010 SOC Code` == "29-1141", "rothwell_STW"] <- 1
