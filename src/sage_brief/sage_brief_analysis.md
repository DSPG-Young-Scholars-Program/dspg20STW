    library(dplyr)
    library(ggplot2)
    library(readxl)
    library(tidyr)
    library(ggrepel)
      
    data <- read.csv("/sfs/qumulo/qhome/sm9dv/dspg20STW/src/sage_brief/stw_edu_region.csv")


    data$less_than_bach <- rowSums(data[, c("minedu_0", "minedu_12", "minedu_14")])


    soc_region <- data %>% 
      select(state.region, X2010.SOC.Code, less_than_bach, N) %>% 
      group_by(state.region, X2010.SOC.Code)%>%
      summarise(less_than_bach = sum(less_than_bach), 
                N = sum(N), 
                per = less_than_bach/N)

    xwalk <- read_xls("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/onet/2010_to_SOC_Crosswalk.xls", skip = 3)

    soc_region_chart <- merge(soc_region, xwalk[, c("2010 SOC Code", "2010 SOC Title")], by.x = "X2010.SOC.Code", by.y = "2010 SOC Code", all.x = T)

First, we will identify the SOC codes in which 100% of the jobs did not
require a bachelor's degree:

    # socs where 100% of the jobs did not require a bachelors degree or above
    soc_100 <- soc_region %>% filter(per == 1)

    # northeast
    unique(pull(soc_100[soc_100$state.region == "Northeast", "X2010.SOC.Code"]))

    ##  [1] "11-9131" "29-2055" "29-2092" "45-4022" "47-2011" "47-2021" "47-2022"
    ##  [8] "47-2081" "47-2121" "47-2151" "47-2152" "47-2171" "47-2181" "47-3012"
    ## [15] "47-3013" "47-3015" "47-5012" "47-5013" "49-2021" "49-2022" "49-2096"
    ## [22] "49-2097" "49-2098" "49-3031" "49-3041" "49-3052" "49-3053" "49-3091"
    ## [29] "49-3092" "49-9044" "49-9052" "49-9064" "49-9094" "49-9096" "51-2093"
    ## [36] "51-4023" "51-5111" "53-7031"

    unique(substr(unique(pull(soc_100[soc_100$state.region == "Northeast", "X2010.SOC.Code"])), 1, 2))

    ## [1] "11" "29" "45" "47" "49" "51" "53"

    # midwest
    unique(pull(soc_100[soc_100$state.region == "Midwest", "X2010.SOC.Code"]))

    ##  [1] "29-2041" "47-2011" "47-2021" "47-2022" "47-2031" "47-2081" "47-2111"
    ##  [8] "47-2151" "47-2152" "47-2171" "47-2181" "47-2221" "47-3011" "47-3012"
    ## [15] "47-3013" "47-3015" "47-5011" "47-5012" "49-2021" "49-2096" "49-2097"
    ## [22] "49-2098" "49-3042" "49-3051" "49-3053" "49-3091" "49-9012" "49-9044"
    ## [29] "49-9052" "49-9094" "49-9096" "51-2041" "51-2093" "51-4081" "51-4192"
    ## [36] "53-7031"

    unique(substr(unique(pull(soc_100[soc_100$state.region == "Midwest", "X2010.SOC.Code"])), 1, 2 ))

    ## [1] "29" "47" "49" "51" "53"

    # south
    unique(pull(soc_100[soc_100$state.region == "South", "X2010.SOC.Code"]))

    ##  [1] "45-4022" "47-2011" "47-2021" "47-2072" "47-2081" "47-2121" "47-2151"
    ##  [8] "47-2181" "47-2221" "47-3015" "47-5011" "47-5012" "49-2021" "49-2096"
    ## [15] "49-2097" "49-3043" "49-3053" "49-3091" "49-9044" "49-9052" "49-9096"
    ## [22] "51-5111" "51-7032" "53-7031"

    unique(substr(unique(pull(soc_100[soc_100$state.region == "South", "X2010.SOC.Code"])), 1, 2 ))

    ## [1] "45" "47" "49" "51" "53"

    # west
    unique(pull(soc_100[soc_100$state.region == "West", "X2010.SOC.Code"]))

    ##  [1] "47-2011" "47-2021" "47-2022" "47-2072" "47-2081" "47-2151" "47-2171"
    ##  [8] "47-2181" "47-2221" "47-3011" "47-3013" "47-3015" "47-5012" "49-2021"
    ## [15] "49-2096" "49-2097" "49-3053" "49-3091" "49-9012" "49-9044" "49-9064"
    ## [22] "49-9094" "49-9096" "51-2093" "51-4023" "51-4081" "51-7031"

    unique(substr(unique(pull(soc_100[soc_100$state.region == "West", "X2010.SOC.Code"])),1,2))

    ## [1] "47" "49" "51"

    # see how many socs whre 100 % of jobs did not require a bacheors degree or above by region
    soc_100 %>% select(state.region, X2010.SOC.Code) %>%group_by(state.region) %>% summarize(countSOCs = n()) 

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 4 x 2
    ##   state.region countSOCs
    ##   <chr>            <int>
    ## 1 Midwest             36
    ## 2 Northeast           38
    ## 3 South               24
    ## 4 West                27

    # major occ groups 47 and 49 have the most of these jobs
      soc_100 %>% ungroup() %>%select( X2010.SOC.Code, state.region) %>%
        mutate(maj_occ = substr(X2010.SOC.Code, start = 1, stop = 2)) %>% 
        group_by(state.region, maj_occ) %>%
        summarize(countMajOcc = n()) %>%
        spread(key = maj_occ, value = countMajOcc)

    ## `summarise()` regrouping output by 'state.region' (override with `.groups` argument)

    ## # A tibble: 4 x 8
    ## # Groups:   state.region [4]
    ##   state.region  `11`  `29`  `45`  `47`  `49`  `51`  `53`
    ##   <chr>        <int> <int> <int> <int> <int> <int> <int>
    ## 1 Midwest         NA     1    NA    17    13     4     1
    ## 2 Northeast        1     2     1    14    16     3     1
    ## 3 South           NA    NA     1    11     9     2     1
    ## 4 West            NA    NA    NA    13    10     4    NA

Boxplots

      is_outlier <- function(x) {
        return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
      }

      soc_region <- soc_region %>%
        group_by(state.region) %>%
        mutate(q1 = quantile(per, 0.25) - 1.5 * IQR(per)) %>%
        ungroup() 
      
      soc_region<-soc_region %>% mutate(outlier = ifelse(soc_region$per < q1, X2010.SOC.Code, NA))
      
      ggplot(soc_region, aes(x = per, y = state.region) )  + geom_boxplot()+
        geom_text_repel(aes(label = outlier), na.rm = TRUE) + 
        labs(title = "Share of STW Job Ads Requiring less than a\n Bachelor's Degree by Region and SOC", x= "", y = "")

![](sage_brief_analysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

      data$less_than_bach_per <- data$less_than_bach/data$N
    # onet level
     onet <- data %>% 
        group_by(state.region) %>%
        mutate(q1 = quantile(less_than_bach_per, 0.25) - 1.5 * IQR(less_than_bach_per)) %>%
        ungroup() 
      
     onet<-onet %>% mutate(outlier = ifelse(less_than_bach_per < q1, onet, NA))
     
     ggplot(onet, aes(x = less_than_bach_per, y = state.region) )  + geom_boxplot()+
       geom_text_repel(aes(label = outlier), na.rm = TRUE) +
      labs(title = "Share of STW Job Ads Requiring less than a\n Bachelor's Degree by Region and ONET", x= "", y = "")

![](sage_brief_analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)
