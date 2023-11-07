# Code to create figures for manuscript

# 4/3/23

library("ggplot2")
library("tidyverse")
library("ggpmisc")

data <- read.csv(file="Model_data_fullcountries_92623_nocorrection.csv")

data <- data %>%
  group_by(Country, Age_Cat) %>%
  mutate(cum_deaths= cumsum(Deaths_noncum),
         cum_deaths_averted = cumsum(Deaths_Counterfactual_average)) %>%
  fill(Population, .direction="downup")


data$deaths_incidence <- (data$cum_deaths/data$Population) *100000
  
data$deaths_averted_incidence <- (data$cum_deaths_averted/data$Population) *100000

data$vaccinated_incidence <- (data$Partly_vaccinated_byAge + data$Fully_vaccinated_byAge) 



data_u60 <- data[which(data$Age_Cat=="18-59"),]
data_over60 <- data[which(data$Age_Cat=="60+"),]

ggplot(data=data_u60, aes(x=Fully_vaccinated_byAge, y= deaths_averted_incidence, color=Country)) + geom_point() +
  theme_classic() + xlab("% vaccinated") + xlim(0,1) + geom_smooth(method="lm")

DeathsAverted_byVaccinationProportion_Under60 <-ggplot(data=data_u60, aes(x=Fully_vaccinated_byAge, y= deaths_averted_incidence)) + geom_point() +
  theme_classic() + xlab("% vaccinated") + xlim(0,1) +
  stat_poly_line() +
  stat_poly_eq() 

ggplot(data=data_over60, aes(x=Fully_vaccinated_byAge, y= deaths_averted_incidence, color=Country)) + geom_point() +theme_classic() + xlab("% vaccinated") +
  xlim(0,1)

DeathsAverted_byVaccinationProportion_Over60 <-ggplot(data=data_over60, aes(x=Fully_vaccinated_byAge, y= deaths_averted_incidence)) + geom_point() +
  theme_classic() + xlab("% vaccinated") + xlim(0,1) +
  stat_poly_line() +
  stat_poly_eq() 

#  Data 2 should be just deaths averted data by country and age, so we can collapse into a general non-country specific estimate

data2 <- data[,c(2:4, 6,11,13:15)]

data5 <- data2 %>% 
  group_by (Month_Yr,Age_Cat) %>% 
  summarize(Total_Deaths_Counterfactual_average= sum(Deaths_Counterfactual_average),
            Total_Deaths_Counterfactual_low= sum(Deaths_Counterfactual_low2),
            Total_Deaths_Counterfactual_high= sum(Deaths_Counterfactual_high2),
            Total_Deaths_Observed= sum(Deaths_noncum)) %>%
  group_by(Age_Cat) %>%
  mutate(Cum_Total_Deaths_Counterfactual_average= cumsum(Total_Deaths_Counterfactual_average),
            Cum_Total_Deaths_Counterfactual_low= cumsum(Total_Deaths_Counterfactual_low),
            Cum_Total_Deaths_Counterfactual_high= cumsum(Total_Deaths_Counterfactual_high),
            Cum_Total_Deaths_Observed= cumsum(Total_Deaths_Observed))

data5$Age_Cat2 <- ifelse(data5$Age_Cat=="18-59","18-59 years","60+ years")


overall_plot_bytime <-ggplot(data=data5) + geom_line(aes(x=Month_Yr, y=Total_Deaths_Observed, group=Age_Cat), linetype="solid") +
  geom_line(aes(x=Month_Yr, y=Total_Deaths_Counterfactual_average, group=Age_Cat), linetype="dashed")+ 
  geom_ribbon(aes(x=Month_Yr, ymin=Total_Deaths_Counterfactual_low, ymax=Total_Deaths_Counterfactual_high, group=Age_Cat), alpha=0.5) +
  theme_classic() +scale_color_discrete(name="") + xlab("Date") + ylab("COVID-19 deaths")+ ggtitle("B") +
  scale_y_continuous(labels = scales::comma) + 
  labs(caption = "No correction for underreporting of COVID-19 mortality")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text=element_text(size=20)) +  
  facet_wrap(~ Age_Cat2) 

overall_plot_cumulative <-ggplot(data=data5) + geom_line(aes(x=Month_Yr, y=Cum_Total_Deaths_Observed, group=Age_Cat), linetype="solid") +
  geom_line(aes(x=Month_Yr, y=Cum_Total_Deaths_Counterfactual_average, group=Age_Cat), linetype="dashed")+ 
  geom_ribbon(aes(x=Month_Yr, ymin=Cum_Total_Deaths_Counterfactual_low, ymax=Cum_Total_Deaths_Counterfactual_high, group=Age_Cat), alpha=0.2) +
  theme_classic() +scale_color_discrete(name="") + xlab("Date") + ylab("COVID-19 deaths, cumulative over time period")  + ggtitle("A")+
  scale_y_continuous(labels = scales::comma)+ 
  labs(caption = "No correction for underreporting of COVID-19 mortality") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text=element_text(size=20)) +  
  facet_wrap(~ Age_Cat2) 


pdf(file="Output/Supplemental_Figure_1_92623.pdf", width=15)
overall_plot_cumulative
overall_plot_bytime
dev.off()

# pdf(file="Output/Supplemental_Figure_Three_4323.pdf", width=15)
# DeathsAverted_byVaccinationProportion_Under60
# DeathsAverted_byVaccinationProportion_Over60
# dev.off()

