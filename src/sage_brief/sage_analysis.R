library(dplyr)
library(ggplot2)

data <- read.csv("src/sage_brief/stw_edu_region.csv")


data$less_than_bach <- rowSums(data[, c("minedu_0", "minedu_12", "minedu_14")])


soc_region <- data %>% 
  select(state.region, X2010.SOC.Code, less_than_bach, N) %>% 
  group_by(state.region, X2010.SOC.Code)%>%
  summarise(less_than_bach = sum(less_than_bach), 
            N = sum(N), 
            per = less_than_bach/N)

ggplot(soc_region, aes(x = X2010.SOC.Code, y = less_than_bach, fill = state.region))+
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(readxl)
xwalk <- read_xls("data/ncses_stw/original/onet/2010_to_SOC_Crosswalk.xls", skip = 3)

soc_region_chart <- merge(soc_region, xwalk[, c("2010 SOC Code", "2010 SOC Title")], by.x = "X2010.SOC.Code", by.y = "2010 SOC Code", all.x = T)
 ggplot(soc_region_chart, aes(state.region, `2010 SOC Title`, fill= per)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white",high = "blue", na.value = "grey60", limits = c(0, 1))+
   scale_x_discrete(position = "top") + 
   theme_minimal() +
   labs(x = "", y = "", fill = "Share of Job Ads\n Requesting Less Than \na Bachelor's Degree")+#, 
        #title = " Share of Job Ads \nRequesting Less Than a \nBachelor's Degree by SOC Code", subtitle = "For Major Occupation Group 17 \n(Architecture and Engineering Occupations)")+
   theme(aspect.ratio = 3/1, 
         legend.position="bottom", 
         plot.margin = margin(0, 0, 0, 0, "cm"),
         legend.title = element_text(size = 8))
 
 # soc specific
 ggplot(soc_region_chart[substr(soc_region_chart$X2010.SOC.Code, start = 1, stop = 2)== "51", ], 
        aes(y = `2010 SOC Title`, x= per, fill = state.region, color = state.region)) + 
   geom_point(size = 2) + 
   scale_x_continuous(limits= c(0,1)) + 
   labs(y = "", x= "") + 
   theme_minimal()

 # all specific
 ggplot(soc_region_chart, 
        aes(y = `2010 SOC Title`, x= per, fill = state.region, color = state.region)) + 
   geom_point() + 
   scale_x_continuous(limits= c(0,1)) + 
   labs(y = "", x= "") + 
   theme_minimal()
 

 
 