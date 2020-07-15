Gather BGT data and ONET/SOC/OCC/NCSES crosswalk:

    bgt <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT A.minedu, B.onet
      FROM bgt_job.jolts_comparison_2010 A
      JOIN bgt_job.main B
      ON A.id = B.id
      WHERE A.state IN" , paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""), sep = ""))

    xwalk <- read_xlsx("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ONETSOC_SOC_OCC_NCSES_JUL2020.xlsx", na ="NA")

Create an STW column based off of the PER.HS column. If PER.HS is
greater than or equal to 50, categorize the ONET code as STW.

    xwalk$PER.HS.STW <- ifelse(xwalk$PER.HS >= 50 & is.na(xwalk$PER.HS) == FALSE, 1, 
                               ifelse(xwalk$PER.HS < 50 & is.na(xwalk$PER.HS) == FALSE, 0, NA))

Create a table with the following:

-   ONET Code

-   Mean BGT minedu

-   Median BGT minedu

-   Min BGT minedu

-   Max BGT minedu

-   Number of BGT Observations

-   Number of Missing BGT Observations

-   Proportion of Missing BGT Observations to Total BGT Observations

-   SDAD STW

-   NCSES STW

-   Job Zone

-   ONET Survey Percent of Respondents with No More than a High School
    Degree

-   STW Based on ONET Survey

<!-- -->

    onet_distribution <- bgt %>%
      group_by(onet) %>%
      summarize(mean = round(mean(minedu, na.rm = T), 3), median = median(minedu, na.rm = T), min = ifelse(is.nan(mean) == TRUE, NA, min(minedu, na.rm = T)), 
                max = ifelse(is.nan(mean) == TRUE, NA, max(minedu, na.rm = T)), n_obs = n(), sum_nas = sum(is.na(minedu) ==TRUE), prop_nas = round(sum_nas/n_obs, 3)) %>%
      merge(xwalk[,c("SDADSTW", "NCSESSTW", "JobZone", "PER.HS", "ONETSOC", "PER.HS.STW")], by.x = "onet", by.y = "ONETSOC", all.x = T)

    head(onet_distribution)

    ##         onet   mean median min max  n_obs sum_nas prop_nas SDADSTW NCSESSTW
    ## 1 11-1011.00 16.297     16  12  21  12775    5917    0.463       0        0
    ## 2 11-1011.03 16.144     16  14  18    271      77    0.284       0        0
    ## 3 11-1021.00 15.380     16  12  21  98568   37991    0.385       0        0
    ## 4 11-2011.00 15.829     16  12  21   3545    1838    0.518       0        0
    ## 5 11-2011.01 15.797     16  12  21    154      85    0.552      NA       NA
    ## 6 11-2021.00 16.091     16  12  21 110741   42041    0.380       0        0
    ##   JobZone PER.HS PER.HS.STW
    ## 1       5  10.28          0
    ## 2       5   3.85          0
    ## 3       4  51.97          1
    ## 4       4  31.69          0
    ## 5      NA     NA         NA
    ## 6       4   9.36          0

How many BGT ONETs are not in the crosswalk?

    list_onets_not_in_xwalk <- unique(onet_distribution$onet)[!(unique(onet_distribution$onet) %in% xwalk$ONETSOC)]
    length(list_onets_not_in_xwalk)

    ## [1] 257
