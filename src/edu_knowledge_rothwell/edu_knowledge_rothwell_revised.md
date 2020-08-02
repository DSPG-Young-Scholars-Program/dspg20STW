The construction of the crosswalk follows Rothwell’s (2015) definition
of the STW.  
Data sources used to construct the crosswalk are: • 2010 O*NET-SOC
occupations to 2010 SOC occupations :
<https://www.onetcenter.org/taxonomy/2010/soc/2010_to_SOC_Crosswalk.xls?fmt=xls>
• O\*NET 15.1 Database : Content Model Knowledge data associated with
each O*NET-SOC occupation:
<https://www.onetcenter.org/dl_files/db_15_1.zip> • O\*NET 15.1
Database: Education percent frequency data associated with each
O\*NET-SOC occupation: <https://www.onetcenter.org/dl_files/db_15_1.zip>
• 2010 Census Occupation Codes with Crosswalk :
<https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2010-occ-codes-with-crosswalk-from-2002-2011.xls>

Following Rothwell’s paper, the following steps were taken to determine
which occupation codes to designate as skilled technical workforce
(STW). His analysis, as outlined in the paper, utilizes the O*NET
Knowledge survey, which asks workers to rate the level of knowledge
needed to perform their job across 33 distinct knowledge domains on a
scale from 1¬ to 7. The STW crosswalk was constructed by completing the
following steps: 1. The O*NET Version 15.1 (released February 2011)
Knowledge and Education survey data were downloaded.

Knowledge and Education/Training/Experience Tables Download

    #download.file(url = "https://www.onetcenter.org/dl_files/db_15_1.zip", destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/db_15_1.zip")

    #system(command = "unzip -c /sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/db_15_1.zip 'db_15_1/Knowledge.txt' > /sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/Knowledge.txt" )

    #system(command = "unzip -c /sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/db_15_1.zip 'db_15_1/Education, Training, and Experience.txt' > /sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/Education.txt" )

OCC and SOC Crosswalk Download

    #download.file(url = "https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2010-occ-codes-with-crosswalk-from-2002-2011.xls",
     #             destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010-occ-codes-with-crosswalk-from-2002-2011.xls")

    #download.file(url = "https://www.onetcenter.org/taxonomy/2010/soc/2010_to_SOC_Crosswalk.xls?fmt=xls", 
          #        destfile = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010_to_SOC_Crosswalk.xls")

    knowledge <- fread("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/Knowledge.txt")
    xwalk_soc <- read_xls("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010_to_SOC_Crosswalk.xls", skip =3)

    knowledge <- knowledge[knowledge$`Scale ID` == "LV", ]


    knowledge <- merge(knowledge[, c("O*NET-SOC Code", "Element Name", "Data Value")], xwalk_soc[, c("O*NET-SOC 2010 Code", "2010 SOC Code")],
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
    ##  1 11-1011                     1
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

    education <- fread("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/Education.txt")
    education <- education[education$`Element Name` == "Required Level of Education", c("O*NET-SOC Code", "Category", "Data Value")]

    education <- merge(education, xwalk_soc[, c("O*NET-SOC 2010 Code", "2010 SOC Code")],
          by.x = "O*NET-SOC Code", by.y = "O*NET-SOC 2010 Code", all = T)

    education <- education  %>%
      mutate(hasbach = ifelse(Category <= 5, "nobach", ifelse(Category > 5, "bach", NA))) %>% # if category is greater than 5, the percentage cooresponds to bachelor's degree or higher.
      group_by(`O*NET-SOC Code`,  `2010 SOC Code`, hasbach) %>%
      summarize(`Percent No Bachelors` = sum(`Data Value`)) %>%
      filter(hasbach == "nobach"|is.na(hasbach)==TRUE) %>%
      select(!hasbach)

    ## `summarise()` regrouping output by 'O*NET-SOC Code', '2010 SOC Code' (override with `.groups` argument)

    # calculate the percentage of respondents for each O*NET without a bachelor's degree

    education_soc <- education %>%
      group_by(`2010 SOC Code`) %>%
      summarize(`Mean Percent No Bachelors` = mean(`Percent No Bachelors`, na.rm = T), 
                education_STW = ifelse(`Mean Percent No Bachelors`> 50, 1, ifelse(is.na(`Mean Percent No Bachelors`)==T, NA, 0)))%>%
      select(!`Mean Percent No Bachelors`)# take the mean percentage of respondents with less than a bachelor’s degree for each SOC code

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(education_soc, 10)

    ## # A tibble: 10 x 2
    ##    `2010 SOC Code` education_STW
    ##    <chr>                   <dbl>
    ##  1 11-1011                     0
    ##  2 11-1021                     1
    ##  3 11-1031                    NA
    ##  4 11-2011                     0
    ##  5 11-2021                     0
    ##  6 11-2022                     0
    ##  7 11-2031                     0
    ##  8 11-3011                     1
    ##  9 11-3021                     0
    ## 10 11-3031                     0

Finally, we merge our knowledge and education findings together. If
there is a "1" in both the knowledge and education tables, then a SOC
code is categorized as STW.

    rothwell_soc <- merge(education_soc, knowledge_soc, by = "2010 SOC Code", all =  T)

    rothwell_soc$rothwell_STW <- ifelse(rothwell_soc$knowledge_STW == 1 & rothwell_soc$education_STW ==1, 1, ifelse(is.na(rothwell_soc$knowledge_STW) == T & is.na(rothwell_soc$education_STW) ==T, NA, 0))

    #We fix the case where registered nurses fail to meet the knowledge and education criteria (Step 5).
    rothwell_soc[rothwell_soc$`2010 SOC Code` == "29-1141", "rothwell_STW"] <- 1

    head(rothwell_soc, 10)

    ##    2010 SOC Code education_STW knowledge_STW rothwell_STW
    ## 1        11-1011             0             1            0
    ## 2        11-1021             1             0            0
    ## 3        11-1031            NA            NA           NA
    ## 4        11-2011             0             0            0
    ## 5        11-2021             0             0            0
    ## 6        11-2022             0             0            0
    ## 7        11-2031             0             0            0
    ## 8        11-3011             1             0            0
    ## 9        11-3021             0             1            0
    ## 10       11-3031             0             1            0

Merge with OCC

    rothwell <- merge(xwalk_soc, rothwell_soc, by = "2010 SOC Code")
    occ <- read_xls("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010-occ-codes-with-crosswalk-from-2002-2011.xls",
                    skip = 4)

    ## New names:
    ## * `` -> ...1

    occ <- occ[,-1]

    occ <- occ %>% filter(grepl("^\\d{4}$", x = `2010 Census Code`))

    # some of the SOC codes in the OCC data table have "Xs" where multiple SOC codes is relevant to one OCC codes.
    # ex) if 13-1111 and 13-1112 corresponded to occ code, 3783, the SOC codes would show as 13-111X

    occ_XX <- occ %>% filter(grepl(pattern = "^\\d{2}\\-\\d{2}XX$", x = `2010 SOC Code`)) %>%
      mutate(`2010 SOC Code X` = substr(`2010 SOC Code`, start = 1, stop = 5))

    occ_X <- occ %>% filter(grepl(pattern = "^\\d{2}\\-\\d{3}X$", x = `2010 SOC Code`)) %>%
      mutate(`2010 SOC Code X` = substr(`2010 SOC Code`, start = 1, stop = 6))

    occ_not_X <- occ %>% filter(grepl(pattern = "^\\d{2}\\-\\d{4}$", x = `2010 SOC Code`)) 

    rothwell <- rothwell %>%
      merge(occ, by= "2010 SOC Code", all.x = T)

      
    for(j in occ_X$`2010 SOC Code X`){ 
        
      for(i in rothwell[is.na(rothwell$`2010 Census Code`) == T, "2010 SOC Code"]){

        if( i %like% j){
          
          rothwell[rothwell$`2010 SOC Code` == i, "2010 Census Code"] <- occ_X[occ_X$`2010 SOC Code X` == j, "2010 Census Code"]
          rothwell[rothwell$`2010 SOC Code` == i, "Occupation 2010 Description"] <- occ_X[occ_X$`2010 SOC Code X` == j, "Occupation 2010 Description"]

          }
      
        }


      }

    for(j in occ_XX$`2010 SOC Code X`){ 
        
      for(i in rothwell[is.na(rothwell$`2010 Census Code`) == T, "2010 SOC Code"]){

        if( i %like% j){
          
          rothwell[rothwell$`2010 SOC Code` == i, "2010 Census Code"] <- occ_XX[occ_XX$`2010 SOC Code X` == j, "2010 Census Code"]
          rothwell[rothwell$`2010 SOC Code` == i, "Occupation 2010 Description"] <- occ_XX[occ_XX$`2010 SOC Code X` == j, "Occupation 2010 Description"]

          }
      
        }


      }

    #write.csv(rothwell, "/sfs/qumulo/qhome/sm9dv/dspg20STW/src/edu_knowledge_rothwell/rothwell.csv")
