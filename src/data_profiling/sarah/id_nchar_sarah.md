    freq_table <- data.frame(Var1 = character(0), Freq = numeric(0), year = numeric(0))
    years <- 2010:2019

    for(y in years){
      
      tbl <- RPostgreSQL::dbGetQuery(
          conn = conn, 
          statement = paste("SELECT id FROM bgt_job.jolts_comparison_", y, sep = ""))
        
      freq <- as.data.frame(table(nchar(tbl$id)))

      freq$year <- y

      freq_table <- rbind(freq_table, freq)
    }

    freq_table$Var1 <- as.factor(freq_table$Var1)
    freq_table_wide <- reshape(freq_table, idvar = "year", timevar = "Var1", direction = "wide")
    freq_table_wide <- freq_table_wide[c(1, 10, 11, 12, 6,2, 7,8, 3,4,9,5)]
    freq_table_wide[is.na(freq_table_wide)] <- 0

Count of IDs by Number of Characters

    knitr::kable(freq_table_wide, row.names = FALSE)

<table>
<thead>
<tr class="header">
<th align="right">year</th>
<th align="right">Freq.1</th>
<th align="right">Freq.2</th>
<th align="right">Freq.3</th>
<th align="right">Freq.4</th>
<th align="right">Freq.5</th>
<th align="right">Freq.6</th>
<th align="right">Freq.7</th>
<th align="right">Freq.8</th>
<th align="right">Freq.9</th>
<th align="right">Freq.10</th>
<th align="right">Freq.11</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">2010</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">13</td>
<td align="right">11685996</td>
<td align="right">0</td>
<td align="right">1100</td>
</tr>
<tr class="even">
<td align="right">2011</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">2582</td>
<td align="right">42778</td>
<td align="right">270200</td>
<td align="right">3602563</td>
<td align="right">1369783</td>
<td align="right">8755076</td>
<td align="right">0</td>
<td align="right">141631</td>
</tr>
<tr class="odd">
<td align="right">2012</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">12</td>
<td align="right">13513982</td>
<td align="right">0</td>
<td align="right">576691</td>
</tr>
<tr class="even">
<td align="right">2013</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">4218538</td>
<td align="right">13103237</td>
<td align="right">1</td>
<td align="right">750268</td>
</tr>
<tr class="odd">
<td align="right">2014</td>
<td align="right">5</td>
<td align="right">34</td>
<td align="right">72</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">3</td>
<td align="right">7169577</td>
<td align="right">544216</td>
<td align="right">11312506</td>
</tr>
<tr class="even">
<td align="right">2015</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">3</td>
<td align="right">21</td>
<td align="right">20828239</td>
</tr>
<tr class="odd">
<td align="right">2016</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">27</td>
<td align="right">23500811</td>
</tr>
<tr class="even">
<td align="right">2017</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">3</td>
<td align="right">22</td>
<td align="right">22259780</td>
</tr>
<tr class="odd">
<td align="right">2018</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">27</td>
<td align="right">28769708</td>
</tr>
<tr class="even">
<td align="right">2019</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">4</td>
<td align="right">32</td>
<td align="right">33452636</td>
</tr>
</tbody>
</table>
