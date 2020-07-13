Load packages and BGT 2010 data:

    library(dplyr)
    library(tidyr)

    source("job_zones.R")

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
    ## [31] "13-2099.00" "47-4099.00" "39-3019.00" "55-3017.00" "21-1029.00"
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


    job_zones(2010:2019)

    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)
    ## `summarise()` regrouping output by 'job_zone' (override with `.groups` argument)

    knitr::kable(share)

<table>
<thead>
<tr class="header">
<th align="right">Year</th>
<th align="right">Job.Zone.and.Minedu</th>
<th align="right">Minedu.Only</th>
<th align="right">Job.Zone.Only</th>
<th align="right">Neither.Job.Zone.nor.Minedu</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">2010</td>
<td align="right">0.3999477</td>
<td align="right">0.0418556</td>
<td align="right">0.5207486</td>
<td align="right">0.0374481</td>
</tr>
<tr class="even">
<td align="right">2011</td>
<td align="right">0.4460139</td>
<td align="right">0.0451622</td>
<td align="right">0.4731044</td>
<td align="right">0.0357195</td>
</tr>
<tr class="odd">
<td align="right">2012</td>
<td align="right">0.4515444</td>
<td align="right">0.0481982</td>
<td align="right">0.4632711</td>
<td align="right">0.0369863</td>
</tr>
<tr class="even">
<td align="right">2013</td>
<td align="right">0.4218999</td>
<td align="right">0.0458516</td>
<td align="right">0.4925626</td>
<td align="right">0.0396859</td>
</tr>
<tr class="odd">
<td align="right">2014</td>
<td align="right">0.4364635</td>
<td align="right">0.0483382</td>
<td align="right">0.4795670</td>
<td align="right">0.0356313</td>
</tr>
<tr class="even">
<td align="right">2015</td>
<td align="right">0.4679886</td>
<td align="right">0.0465191</td>
<td align="right">0.4525954</td>
<td align="right">0.0328969</td>
</tr>
<tr class="odd">
<td align="right">2016</td>
<td align="right">0.4557298</td>
<td align="right">0.0443789</td>
<td align="right">0.4666153</td>
<td align="right">0.0332760</td>
</tr>
<tr class="even">
<td align="right">2017</td>
<td align="right">0.4831040</td>
<td align="right">0.0455864</td>
<td align="right">0.4399093</td>
<td align="right">0.0314002</td>
</tr>
<tr class="odd">
<td align="right">2018</td>
<td align="right">0.8202069</td>
<td align="right">0.0758659</td>
<td align="right">0.0970483</td>
<td align="right">0.0068789</td>
</tr>
<tr class="even">
<td align="right">2019</td>
<td align="right">0.9156482</td>
<td align="right">0.0837551</td>
<td align="right">0.0005350</td>
<td align="right">0.0000617</td>
</tr>
</tbody>
</table>

Jobs Zone and Minedu Combination Counts:

    tbl <- bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE) %>% 
      select(job_zone, minedu, id) %>%
      group_by(job_zone, minedu) %>%
      summarise(Count.of.IDs = n())

    knitr::kable(counts_wide)

<table>
<thead>
<tr class="header">
<th align="right">job_zone</th>
<th align="right">minedu</th>
<th align="right">2010</th>
<th align="right">2011</th>
<th align="right">2012</th>
<th align="right">2013</th>
<th align="right">2014</th>
<th align="right">2015</th>
<th align="right">2016</th>
<th align="right">2017</th>
<th align="right">2018</th>
<th align="right">2019</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">455289</td>
<td align="right">686850</td>
</tr>
<tr class="even">
<td align="right">1</td>
<td align="right">12</td>
<td align="right">24924</td>
<td align="right">41603</td>
<td align="right">46455</td>
<td align="right">63099</td>
<td align="right">86633</td>
<td align="right">93425</td>
<td align="right">106586</td>
<td align="right">112643</td>
<td align="right">158821</td>
<td align="right">223398</td>
</tr>
<tr class="odd">
<td align="right">1</td>
<td align="right">14</td>
<td align="right">625</td>
<td align="right">895</td>
<td align="right">1001</td>
<td align="right">1339</td>
<td align="right">1539</td>
<td align="right">1590</td>
<td align="right">1518</td>
<td align="right">1171</td>
<td align="right">1601</td>
<td align="right">2304</td>
</tr>
<tr class="even">
<td align="right">1</td>
<td align="right">16</td>
<td align="right">735</td>
<td align="right">1329</td>
<td align="right">1693</td>
<td align="right">2900</td>
<td align="right">3740</td>
<td align="right">3621</td>
<td align="right">1972</td>
<td align="right">1500</td>
<td align="right">2098</td>
<td align="right">2822</td>
</tr>
<tr class="odd">
<td align="right">1</td>
<td align="right">18</td>
<td align="right">76</td>
<td align="right">64</td>
<td align="right">78</td>
<td align="right">75</td>
<td align="right">92</td>
<td align="right">76</td>
<td align="right">68</td>
<td align="right">62</td>
<td align="right">430</td>
<td align="right">1053</td>
</tr>
<tr class="even">
<td align="right">1</td>
<td align="right">21</td>
<td align="right">8</td>
<td align="right">25</td>
<td align="right">34</td>
<td align="right">47</td>
<td align="right">37</td>
<td align="right">38</td>
<td align="right">29</td>
<td align="right">37</td>
<td align="right">49</td>
<td align="right">43</td>
</tr>
<tr class="odd">
<td align="right">2</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">4293677</td>
<td align="right">6254538</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">12</td>
<td align="right">686662</td>
<td align="right">938249</td>
<td align="right">924955</td>
<td align="right">1368324</td>
<td align="right">1543865</td>
<td align="right">1677938</td>
<td align="right">2022935</td>
<td align="right">1946503</td>
<td align="right">2565973</td>
<td align="right">3102107</td>
</tr>
<tr class="odd">
<td align="right">2</td>
<td align="right">14</td>
<td align="right">34906</td>
<td align="right">60285</td>
<td align="right">52619</td>
<td align="right">64608</td>
<td align="right">83271</td>
<td align="right">100293</td>
<td align="right">117572</td>
<td align="right">132723</td>
<td align="right">129239</td>
<td align="right">124482</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">16</td>
<td align="right">125131</td>
<td align="right">158720</td>
<td align="right">169487</td>
<td align="right">210702</td>
<td align="right">215873</td>
<td align="right">253375</td>
<td align="right">254752</td>
<td align="right">232498</td>
<td align="right">315404</td>
<td align="right">354253</td>
</tr>
<tr class="odd">
<td align="right">2</td>
<td align="right">18</td>
<td align="right">4822</td>
<td align="right">7474</td>
<td align="right">6749</td>
<td align="right">6522</td>
<td align="right">6917</td>
<td align="right">7864</td>
<td align="right">8162</td>
<td align="right">7135</td>
<td align="right">15465</td>
<td align="right">21892</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">21</td>
<td align="right">1830</td>
<td align="right">2456</td>
<td align="right">1775</td>
<td align="right">2060</td>
<td align="right">1895</td>
<td align="right">1992</td>
<td align="right">2669</td>
<td align="right">2494</td>
<td align="right">3633</td>
<td align="right">3757</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">2239088</td>
<td align="right">3241190</td>
</tr>
<tr class="even">
<td align="right">3</td>
<td align="right">12</td>
<td align="right">483281</td>
<td align="right">612031</td>
<td align="right">651729</td>
<td align="right">844197</td>
<td align="right">928473</td>
<td align="right">1054586</td>
<td align="right">1199789</td>
<td align="right">1183578</td>
<td align="right">1565720</td>
<td align="right">1959988</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">14</td>
<td align="right">275853</td>
<td align="right">386284</td>
<td align="right">375049</td>
<td align="right">446347</td>
<td align="right">477506</td>
<td align="right">724092</td>
<td align="right">915836</td>
<td align="right">1165556</td>
<td align="right">944639</td>
<td align="right">997836</td>
</tr>
<tr class="even">
<td align="right">3</td>
<td align="right">16</td>
<td align="right">361159</td>
<td align="right">495234</td>
<td align="right">525282</td>
<td align="right">608991</td>
<td align="right">647843</td>
<td align="right">813481</td>
<td align="right">935558</td>
<td align="right">942112</td>
<td align="right">1103682</td>
<td align="right">1221737</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">18</td>
<td align="right">23343</td>
<td align="right">30159</td>
<td align="right">27884</td>
<td align="right">28365</td>
<td align="right">27946</td>
<td align="right">36709</td>
<td align="right">36716</td>
<td align="right">30253</td>
<td align="right">38985</td>
<td align="right">46401</td>
</tr>
<tr class="even">
<td align="right">3</td>
<td align="right">21</td>
<td align="right">4596</td>
<td align="right">6676</td>
<td align="right">7528</td>
<td align="right">6473</td>
<td align="right">7677</td>
<td align="right">8548</td>
<td align="right">9521</td>
<td align="right">7751</td>
<td align="right">9995</td>
<td align="right">12206</td>
</tr>
<tr class="odd">
<td align="right">4</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">2553857</td>
<td align="right">3636179</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">12</td>
<td align="right">234993</td>
<td align="right">342106</td>
<td align="right">341694</td>
<td align="right">411824</td>
<td align="right">461315</td>
<td align="right">516207</td>
<td align="right">591662</td>
<td align="right">578893</td>
<td align="right">744598</td>
<td align="right">909875</td>
</tr>
<tr class="odd">
<td align="right">4</td>
<td align="right">14</td>
<td align="right">85043</td>
<td align="right">138660</td>
<td align="right">123922</td>
<td align="right">133515</td>
<td align="right">167660</td>
<td align="right">169032</td>
<td align="right">173348</td>
<td align="right">163371</td>
<td align="right">189799</td>
<td align="right">244836</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">16</td>
<td align="right">1652435</td>
<td align="right">2239363</td>
<td align="right">2216551</td>
<td align="right">2447625</td>
<td align="right">2585882</td>
<td align="right">3027063</td>
<td align="right">3044488</td>
<td align="right">2978829</td>
<td align="right">3912072</td>
<td align="right">4578280</td>
</tr>
<tr class="odd">
<td align="right">4</td>
<td align="right">18</td>
<td align="right">92623</td>
<td align="right">114085</td>
<td align="right">125470</td>
<td align="right">115573</td>
<td align="right">111829</td>
<td align="right">128818</td>
<td align="right">126520</td>
<td align="right">124890</td>
<td align="right">150497</td>
<td align="right">174194</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">21</td>
<td align="right">23916</td>
<td align="right">29332</td>
<td align="right">34359</td>
<td align="right">30817</td>
<td align="right">28545</td>
<td align="right">32727</td>
<td align="right">52105</td>
<td align="right">44291</td>
<td align="right">49079</td>
<td align="right">57735</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">0</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">907307</td>
<td align="right">1299257</td>
</tr>
<tr class="even">
<td align="right">5</td>
<td align="right">12</td>
<td align="right">10264</td>
<td align="right">13123</td>
<td align="right">14177</td>
<td align="right">18864</td>
<td align="right">21134</td>
<td align="right">24038</td>
<td align="right">26083</td>
<td align="right">23962</td>
<td align="right">34641</td>
<td align="right">49802</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">14</td>
<td align="right">20657</td>
<td align="right">31256</td>
<td align="right">26305</td>
<td align="right">30300</td>
<td align="right">31238</td>
<td align="right">37644</td>
<td align="right">42459</td>
<td align="right">55122</td>
<td align="right">44886</td>
<td align="right">58270</td>
</tr>
<tr class="even">
<td align="right">5</td>
<td align="right">16</td>
<td align="right">273187</td>
<td align="right">358146</td>
<td align="right">349646</td>
<td align="right">406203</td>
<td align="right">453755</td>
<td align="right">525656</td>
<td align="right">518419</td>
<td align="right">505004</td>
<td align="right">635702</td>
<td align="right">754571</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">18</td>
<td align="right">168539</td>
<td align="right">198882</td>
<td align="right">202946</td>
<td align="right">231203</td>
<td align="right">259794</td>
<td align="right">331564</td>
<td align="right">348861</td>
<td align="right">352744</td>
<td align="right">355280</td>
<td align="right">401109</td>
</tr>
<tr class="even">
<td align="right">5</td>
<td align="right">21</td>
<td align="right">84625</td>
<td align="right">120098</td>
<td align="right">135183</td>
<td align="right">144622</td>
<td align="right">149876</td>
<td align="right">177012</td>
<td align="right">172405</td>
<td align="right">160680</td>
<td align="right">175631</td>
<td align="right">209915</td>
</tr>
</tbody>
</table>
