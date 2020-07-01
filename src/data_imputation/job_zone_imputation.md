Load packages and BGT 2010 data:

    library(dplyr)

    conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                                   dbname = "sdad",
                                   host = "postgis1",
                                   port = 5432,
                                   user = Sys.getenv(x = "DB_USR"),
                                   password = Sys.getenv(x = "DB_PWD"))

    zones <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = "SELECT * FROM onet.job_zones")

    # retrieves onet code
    bgt <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = "SELECT A.id, A.minedu, A.soc, B.onet
      FROM bgt_job.jolts_comparison_2010 A
      JOIN bgt_job.main B
      ON A.id = B.id")

Match Job Zone to each BGT job-ad:

    bgt$job_zone<- zones$job_zone[match(bgt$onet, zones$onetsoc_code)]

Print non-matched O\*Net codes:

    nonMatch <- bgt%>%filter(is.na(onet) == FALSE & is.na(job_zone)==TRUE) %>% select(onet,soc) %>% unique()
    print(unique(nonMatch$onet))

    ##  [1] "51-7099.00" "41-9099.00" "11-9199.00" "27-1029.00" "31-9099.00"
    ##  [6] "29-1199.00" "23-2099.00" "41-3099.00" "17-2199.00" "29-1069.00"
    ## [11] "25-1199.00" "51-2099.00" "43-9199.00" "51-9199.00" "13-1199.00"
    ## [16] "15-1199.00" "55-3016.00" "55-1016.00" "39-9099.00" "19-4099.00"
    ## [21] "53-7199.00" "53-6099.00" "17-3019.00" "55-3014.00" "55-3018.00"
    ## [26] "19-1099.00" "55-1019.00" "21-1099.00" "55-3012.00" "55-3011.00"
    ## [31] "47-4099.00" "13-2099.00" "39-3019.00" "55-3017.00" "21-1029.00"
    ## [36] "17-3029.00" "55-3013.00" "47-5099.00" "25-3099.00" "29-1129.00"
    ## [41] "25-2059.00" "25-2051.00" "19-2099.00" "21-1019.00" "51-6099.00"
    ## [46] "55-3015.00" "21-2099.00" "51-8099.00" "47-3019.00" "33-9099.00"
    ## [51] "47-5049.00" "51-8099.02" "25-9099.00" "29-2099.00" "49-9099.00"
    ## [56] "53-3099.00" "29-1029.00" "19-3099.00" "13-2099.03" "45-4029.00"
    ## [61] "55-3019.00" "33-1099.00" "27-1019.00" "37-3019.00" "11-2011.01"
    ## [66] "51-4199.00" "43-2099.00" "27-2099.00" "19-3039.00" "11-9039.00"
    ## [71] "35-9099.00" "29-9099.00" "43-4199.00" "45-2099.00" "15-2099.00"
    ## [76] "55-1011.00" "11-3051.05" "55-1015.00" "17-3029.10" "53-4099.00"
    ## [81] "19-1029.00" "55-2012.00" "27-4099.00" "49-9069.00" "37-2019.00"
    ## [86] "51-3099.00" "25-1069.00" "55-2013.00" "39-3099.00"

Share of BGT Job-Ads with complete Job Zone and/or Minedu observations:

    share <- data.frame("Job Zone and Minedu" = nrow(bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE))/nrow(bgt),
                        "Minedu Only" = nrow(bgt %>% filter(is.na(job_zone)==TRUE & is.na(minedu)==FALSE))/nrow(bgt),
                        "Job Zone Only" = nrow(bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==TRUE))/nrow(bgt),
                        "Neither Job Zone nor Minedu" = nrow(bgt %>% filter(is.na(job_zone)==TRUE & is.na(minedu)==TRUE))/nrow(bgt)
    )

    knitr::kable(share)

<table>
<thead>
<tr class="header">
<th align="right">Job.Zone.and.Minedu</th>
<th align="right">Minedu.Only</th>
<th align="right">Job.Zone.Only</th>
<th align="right">Neither.Job.Zone.nor.Minedu</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">0.3999477</td>
<td align="right">0.0418556</td>
<td align="right">0.5207486</td>
<td align="right">0.0374481</td>
</tr>
</tbody>
</table>

Jobs Zone and Minedu Combination Counts:

    tbl <- bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE) %>% 
      select(job_zone, minedu, id) %>%
      group_by(job_zone, minedu) %>%
      summarise(Count.of.IDs = n())

    knitr::kable(tbl)

<table>
<thead>
<tr class="header">
<th align="right">job_zone</th>
<th align="right">minedu</th>
<th align="right">Count.of.IDs</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="right">12</td>
<td align="right">24924</td>
</tr>
<tr class="even">
<td align="right">1</td>
<td align="right">14</td>
<td align="right">625</td>
</tr>
<tr class="odd">
<td align="right">1</td>
<td align="right">16</td>
<td align="right">735</td>
</tr>
<tr class="even">
<td align="right">1</td>
<td align="right">18</td>
<td align="right">76</td>
</tr>
<tr class="odd">
<td align="right">1</td>
<td align="right">21</td>
<td align="right">8</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">12</td>
<td align="right">686662</td>
</tr>
<tr class="odd">
<td align="right">2</td>
<td align="right">14</td>
<td align="right">34906</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">16</td>
<td align="right">125131</td>
</tr>
<tr class="odd">
<td align="right">2</td>
<td align="right">18</td>
<td align="right">4822</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">21</td>
<td align="right">1830</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">12</td>
<td align="right">483281</td>
</tr>
<tr class="even">
<td align="right">3</td>
<td align="right">14</td>
<td align="right">275853</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">16</td>
<td align="right">361159</td>
</tr>
<tr class="even">
<td align="right">3</td>
<td align="right">18</td>
<td align="right">23343</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">21</td>
<td align="right">4596</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">12</td>
<td align="right">234993</td>
</tr>
<tr class="odd">
<td align="right">4</td>
<td align="right">14</td>
<td align="right">85043</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">16</td>
<td align="right">1652435</td>
</tr>
<tr class="odd">
<td align="right">4</td>
<td align="right">18</td>
<td align="right">92623</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">21</td>
<td align="right">23916</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">12</td>
<td align="right">10264</td>
</tr>
<tr class="even">
<td align="right">5</td>
<td align="right">14</td>
<td align="right">20657</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">16</td>
<td align="right">273187</td>
</tr>
<tr class="even">
<td align="right">5</td>
<td align="right">18</td>
<td align="right">168539</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">21</td>
<td align="right">84625</td>
</tr>
</tbody>
</table>
