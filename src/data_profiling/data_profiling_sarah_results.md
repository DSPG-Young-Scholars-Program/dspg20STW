##### Profiling Function:

    completeness <- function(x){
        sum(!is.na(x))/length(x)
    }

    uniqueness <- function(x){
        sum(!is.na(unique(x)))
    }


    profile <-function(year){
      
      cols <<- c("id","jobdate", "state",   "soc", "socname", "lat", "lon",  "minedu",  "maxedu" )
      
       prof <<- data.frame(variable = cols, 
                            completeness = numeric(length = length(cols)),
                            validity = numeric(length = length(cols)), 
                            uniqueness = numeric(length = length(cols)))
       for (j in cols){

          tbl <<- RPostgreSQL::dbGetQuery(
            conn = conn, 
            statement = paste("SELECT " , j, " FROM bgt_job.jolts_comparison_", year, sep = ""))
          
          prof[prof$variable == j, "completeness"] <- completeness(tbl[, j])
          
          prof[prof$variable == j, "uniqueness"] <- uniqueness(tbl[, j])
          
          # value validity
          
          if(j == "id"){
            
            prof[prof$variable == j, "validity"] <- length(unique(tbl[, j]))/length(tbl[, j])
          
          } else if (j == "jobdate"){
            
            prof[prof$variable == j, "validity"] <- sum(tbl[, j] %in% seq(ymd(paste(as.character(year), "-01-01", sep = "")), 
                                 ymd(paste(as.character(year), "-12-31", sep = "")), '1 day'))/length(tbl[, j])
            
          } else if (j == "state"){
            
            prof[prof$variable == j, "validity"] <- (sum(tbl[, j] %in% c(state.name, "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau", "Marshall Islands", "Federated States of Micronesia")) + sum(is.na(tbl[, j])))/length(tbl[, j])
          
          } else if (j == "soc"){
            
            prof[prof$variable == j, "validity"] <- (sum(grepl("\\d{2}\\-\\d{4}", tbl[, j][!is.na(tbl[, j])])) + sum(is.na(tbl[, j])))/length(tbl[, j])
          
          } else if (j == "socname"){
            
            prof[prof$variable == j, "validity"]  <- (sum(!grepl(pattern = "\\d", tbl[, j][!is.na(tbl[, j])])) + sum(is.na(tbl[, j])))/length(tbl[, j])
          
          } else if (j == "lat"){
            
            prof[prof$variable == j, "validity"]  <- (sum(tbl[,j][!is.na(tbl[, j])] > 0) + sum(is.na(tbl[, j])))/length(tbl[, j])
          
          } else if (j == "lon"){
            
            prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[,j])] < 0 |( 133 < tbl[, j][!is.na(tbl[,j])] & tbl[, j][!is.na(tbl[,j])] < 172)) + sum(is.na(tbl[,j])))/length(tbl[,j])
            
          } else if (j == "minedu"|j == "maxedu"){
            
            prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[, j])] %in% c(0, 12, 14, 16, 18, 21)) + sum(is.na(tbl[, j])))/length(tbl[, j])
          }
          

       }
       
      assign(paste("prof", year, sep = ""), prof, envir = .GlobalEnv)  
    }

### Profiling Results by Year

##### 2010

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">11687110</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">361</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">57</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9760239</td>
<td align="right">1</td>
<td align="right">829</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9760239</td>
<td align="right">1</td>
<td align="right">829</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9728879</td>
<td align="right">1</td>
<td align="right">26580</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9728879</td>
<td align="right">1</td>
<td align="right">27906</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4418033</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1473610</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2011

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">14184613</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">362</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">55</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9761293</td>
<td align="right">1</td>
<td align="right">831</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9761293</td>
<td align="right">1</td>
<td align="right">831</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9801127</td>
<td align="right">1</td>
<td align="right">25109</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9801127</td>
<td align="right">1</td>
<td align="right">26402</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4911761</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1622305</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2012

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">14090686</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">366</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">57</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9745240</td>
<td align="right">1</td>
<td align="right">831</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9745240</td>
<td align="right">1</td>
<td align="right">831</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9826769</td>
<td align="right">1</td>
<td align="right">29499</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9826769</td>
<td align="right">1</td>
<td align="right">31360</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4997426</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1670383</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2013

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">18072049</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">59</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9707383</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9707383</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9779461</td>
<td align="right">1</td>
<td align="right">32801</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9779461</td>
<td align="right">1</td>
<td align="right">34443</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4677514</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1565640</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2014

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">19026414</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">59</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9724461</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9724461</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9801636</td>
<td align="right">1</td>
<td align="right">34392</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9801636</td>
<td align="right">1</td>
<td align="right">36237</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4848017</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1626419</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2015

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">20828263</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">59</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9746747</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9746747</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9918737</td>
<td align="right">1</td>
<td align="right">36649</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9918737</td>
<td align="right">1</td>
<td align="right">38749</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.5145077</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1734663</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2016

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">23500839</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">366</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9736071</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9736071</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9926837</td>
<td align="right">1</td>
<td align="right">38235</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9926837</td>
<td align="right">1</td>
<td align="right">40430</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.5001087</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1661879</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2017

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">22259806</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9742542</td>
<td align="right">1</td>
<td align="right">834</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9742542</td>
<td align="right">1</td>
<td align="right">834</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9918083</td>
<td align="right">1</td>
<td align="right">37936</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9918083</td>
<td align="right">1</td>
<td align="right">40190</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.5286905</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1756633</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

##### 2018

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">28769736</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9716869</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9716869</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9820593</td>
<td align="right">1</td>
<td align="right">38558</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9820593</td>
<td align="right">1</td>
<td align="right">40857</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.8960728</td>
<td align="right">1</td>
<td align="right">6</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.8236399</td>
<td align="right">1</td>
<td align="right">6</td>
</tr>
</tbody>
</table>

##### 2019

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">33452673</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">0.9999690</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9711764</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9711764</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9910704</td>
<td align="right">1</td>
<td align="right">38150</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9910704</td>
<td align="right">1</td>
<td align="right">40377</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.9994033</td>
<td align="right">1</td>
<td align="right">6</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.9999813</td>
<td align="right">1</td>
<td align="right">6</td>
</tr>
</tbody>
</table>

Note: "0" does not count as an minedu/maxedu value until 2018 and 2019
