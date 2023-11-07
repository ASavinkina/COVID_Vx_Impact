library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(reshape2)

# Import population data

urlfile_WHO <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"

WHO_deaths <-read_csv(url(urlfile_WHO))

WHO_deaths$Month_Yr <- format(WHO_deaths$Date_reported, "%Y-%m")

WHO_deaths2 <- WHO_deaths %>% 
  group_by(Month_Yr, Country) %>% 
  mutate(WHO_deaths=cumsum(New_deaths))

WHO_deaths3 <- WHO_deaths2 %>%
  group_by(Country, Month_Yr) %>% 
  slice(which.max(Cumulative_deaths))


WHO_deaths4 <- WHO_deaths3[,c(3,9,10)]

WHO_deaths4$Country <- ifelse(WHO_deaths4$Country=="Bolivia (Plurinational State of)", "Bolivia",WHO_deaths4$Country)
WHO_deaths4$Country <- ifelse(WHO_deaths4$Country=="Venezuela (Bolivarian Republic of)", "Venezuela",WHO_deaths4$Country)

Population <- read_excel('Data/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx', skip=16)

Population <- as.data.frame(Population)

Population$`Region, subregion, country or area *` <- ifelse(Population$`Region, subregion, country or area *`=="Bolivia (Plurinational State of)",
                                                            "Bolivia",Population$`Region, subregion, country or area *`)

Population$`Region, subregion, country or area *` <- ifelse(Population$`Region, subregion, country or area *`=="Venezuela (Bolivarian Republic of)",
                                                            "Venezuela",Population$`Region, subregion, country or area *`)


Population_21 <- Population[which(Population$Year=="2021"),]

countries_full2 <- c("Argentina","Brazil","Chile","Colombia",
                     "Paraguay",
                     "Uruguay", "Jamaica","Peru" )

countries_imputation <- c("Belize","Bolivia","Costa Rica","Ecuador",
                          "El Salvador","Guatemala","Honduras",
                          "Nicaragua","Venezuela","Mexico")

countries <- c(countries_full2,countries_imputation)

Population_21$Country <- Population_21$`Region, subregion, country or area *`

Population_Country <- Population_21[Population_21$Country %in% countries,]

# Subdivide age into groupings we are interested in:

Population_Country <- Population_Country %>% mutate_at(c(12:112), as.numeric,na.rm=TRUE)


Population_Country$Pop_18to59 <- rowSums(Population_Country[,c(30:71)]) # 18 to 59
Population_Country$Pop_60up <- rowSums(Population_Country[,c(72:112)]) # 60+
#Population_Country$Pop_65up <- rowSums(Population_Country[,c(77:112)]) # 65+

# Keep only data of interest

Population_Country <- Population_Country[,c(3,114:115)]

# Population is in thousands- we want full population

Population_Country[,c(2:3)] <- Population_Country[,c(2:3)]*1000

# Check to make sure this seems correct

#rowSums(Population_Country)

# The proportion of the population in each age group and total population for final dataset

Population_Country$TotalPop <- rowSums(Population_Country[,c(2:3)]) #total adult (over 18) population
Population_Country$`18-59` <- Population_Country$Pop_18to59/Population_Country$TotalPop
Population_Country$`60+` <- Population_Country$Pop_60up/Population_Country$TotalPop

Population_Country$Country <- Population_Country$`Region, subregion, country or area *`

## Read in vaccination data by age

#Vax_Age <- read.csv("Vaccine_by_Ag_2_4_23.csv")

Model_data <- read.csv("Model_data_fullcountries_4323_impute.csv")
Model_data2 <- Model_data[,c(2:7)]

Model_data2_part_vax <- dcast(Model_data2 , Country.x + Month_Yr ~ Age_Cat,
                              value.var="Partly_vaccinated_byAge")

colnames(Model_data2_part_vax)[c(3:4)]<-paste("part_vax",colnames(Model_data2_part_vax)[c(3:4)],sep="_")

Model_data2_full_vax <- dcast(Model_data2 , Country.x + Month_Yr ~ Age_Cat,
                              value.var="Fully_vaccinated_byAge")

colnames(Model_data2_full_vax)[c(3:4)]<-paste("full_vax", colnames(Model_data2_full_vax)[c(3:4)],sep="_")

Vaccination <- merge(Model_data2_full_vax, Model_data2_part_vax, by=c("Country.x", "Month_Yr"))

Vaccination$Country <- Vaccination$Country.x

Model_data_impute <- read.csv("Model_data_for_imputation_2_15_23.csv")
Model_data_impute2 <- Model_data_impute[,c(2:6)]

Model_data_impute2_part_vax <- dcast(Model_data_impute2 , Country.x + Month_Yr ~ Age_Cat,
                              value.var="Partly_vaccinated_byAge")

colnames(Model_data_impute2_part_vax)[c(3:4)]<-paste("part_vax",colnames(Model_data_impute2_part_vax)[c(3:4)],sep="_")

Model_data_impute2_full_vax <- dcast(Model_data_impute2 , Country.x + Month_Yr ~ Age_Cat,
                              value.var="Fully_vaccinated_byAge")

colnames(Model_data_impute2_full_vax)[c(3:4)]<-paste("full_vax", colnames(Model_data_impute2_full_vax)[c(3:4)],sep="_")

Vaccination_impute <- merge(Model_data_impute2_full_vax, Model_data_impute2_part_vax, by=c("Country.x", "Month_Yr"))

Vaccination_impute$Country <- Vaccination_impute$Country.x

Vaccination_full <- rbind(Vaccination, Vaccination_impute)


# Include income groups

income_group <- read.csv("Data/world_bank_income_groups.csv")
income_group$Country <- income_group$CountryName

# Regression model to predict proportion of deaths in the under 60 age group, 
# using population proportion at risk over time 

Deaths_data <- read.csv("Death_Age_Ratio.csv")
Deaths_data_long <- gather(Deaths_data, Country, Death_u60, Argentina:Peru, factor_key=TRUE)

# Merge in vaccination by age data

Deaths_data_long2 <- right_join(Deaths_data_long, Vaccination,  by=c("Country", "Month_Yr"))

# Merge in population data

Deaths_data_long3 <- merge(Deaths_data_long2, Population_Country[,c(5,7)], by="Country")



#Merge in income groups

Deaths_data_long3 <- left_join(Deaths_data_long3, income_group[,c(2,5)], by=("Country"))

colnames(Deaths_data_long3)

Deaths_data_long3$pop_u60 <- Deaths_data_long3$`18-59`
Deaths_data_long3$full_vax_le60 <- Deaths_data_long3$`full_vax_18-59`
Deaths_data_long3$part_vax_le60 <- Deaths_data_long3$`part_vax_18-59`
Deaths_data_long3$full_vax_ge60 <- Deaths_data_long3$`full_vax_60+`
Deaths_data_long3$part_vax_ge60 <- Deaths_data_long3$`part_vax_60+`

Deaths_data_long6 <- Deaths_data_long3

Deaths_data_long6$full_vax_le60 <- ifelse(Deaths_data_long3$Month_Yr < "2021-05" & is.na(Deaths_data_long3$full_vax_le60),0,
                                          Deaths_data_long3$full_vax_le60)

Deaths_data_long6$part_vax_le60 <- ifelse(Deaths_data_long3$Month_Yr < "2021-05" & is.na(Deaths_data_long3$part_vax_le60),0,
                                          Deaths_data_long3$part_vax_le60)

Deaths_data_long6$full_vax_ge60 <- ifelse(Deaths_data_long3$Month_Yr < "2021-05" & is.na(Deaths_data_long3$full_vax_ge60),0,
                                          Deaths_data_long3$full_vax_ge60)

Deaths_data_long6$part_vax_ge60 <- ifelse(Deaths_data_long3$Month_Yr < "2021-05" & is.na(Deaths_data_long3$part_vax_ge60),0,
                                          Deaths_data_long3$part_vax_ge60)


# Dataset for model building:

Deaths_Model_Build <- Deaths_data_long6[which(Deaths_data_long6$Country %in% countries_full2),]
Deaths_Model_Predict <- Deaths_data_long6[which(Deaths_data_long6$Country %in% countries_imputation),]

Deaths_Model_Build <- Deaths_Model_Build[which(Deaths_Model_Build$Month_Yr>"2021-01"),]
Deaths_Model_Predict <- Deaths_Model_Predict[which(Deaths_Model_Predict$Month_Yr>"2021-01"),]

Deaths_Model_Build <- Deaths_Model_Build[
  order( Deaths_Model_Build[,1], Deaths_Model_Build[,3] ),
]

Deaths_Model_Predict <- Deaths_Model_Predict[
  order( Deaths_Model_Predict[,1], Deaths_Model_Predict[,3] ),
]


# Create prediction model... 

model4 <- lm(Death_u60 ~  Month_Yr + pop_u60  + full_vax_ge60 +
               part_vax_ge60, data = Deaths_Model_Build)
summary(model4)


# Exclude a country

Predicted <- data.frame(matrix(nrow=length(unique(Deaths_Model_Build$Month_Yr)),ncol=0))

Predicted$Month_Yr <- paste(unique(Deaths_Model_Build$Month_Yr))


# Check model and predict missing values for "full" countries

for (i in 1:length(countries_full2)) {
  
Country <- countries_full2[i]
  
Deaths_data_long4 <- Deaths_Model_Build[!Deaths_Model_Build$Country %in% c(Country), ]
Deaths_data_long5 <- Deaths_Model_Build[which(Deaths_Model_Build$Country == Country),]

model4 <- lm(Death_u60 ~  Month_Yr + pop_u60  + full_vax_ge60 +
               part_vax_ge60, data = Deaths_data_long4)
summary(model4)
#with(summary(model4), 1 - deviance/null.deviance)

predict(model4, Deaths_data_long5)

Predicted1 <- cbind(Deaths_data_long5$Month_Yr, Deaths_data_long5$Death_u60, predict(model4, Deaths_data_long5, na.action = na.exclude))

colnames(Predicted1) <- c("Month_Yr", paste(Country),paste(Country,"_predicted"))

Predicted <- merge(Predicted,Predicted1, by="Month_Yr")

}

Predicted$Paraguay<- ifelse(is.na(Predicted$Paraguay), Predicted$`Paraguay _predicted`,Predicted$Paraguay)
Predicted$Uruguay <- ifelse(is.na(Predicted$Uruguay), Predicted$`Uruguay _predicted`,Predicted$Uruguay)
Predicted$Jamaica <- ifelse(is.na(Predicted$Jamaica ), Predicted$`Jamaica _predicted`,Predicted$Jamaica )

Predicted <- Predicted[,c(1,2,4,6,8,10,12,14,16)]

Predicted_long <- gather(Predicted, Country, Deaths_prop_u60, -Month_Yr, factor_key=TRUE)

Predicted_long <- left_join(Predicted_long, WHO_deaths4, by=c("Country","Month_Yr"))

Predicted_long$WHO_deaths_le60 <- as.numeric(Predicted_long$Deaths_prop_u60) * Predicted_long$WHO_deaths
Predicted_long$WHO_deaths_ge60 <- (1-as.numeric(Predicted_long$Deaths_prop_u60)) * Predicted_long$WHO_deaths

Predicted_full <- Predicted_long





#write.csv(Predicted, "Model_predictions_fullcountries_4323.csv")




Predicted <- data.frame(matrix(nrow=length(unique(Deaths_Model_Predict$Month_Yr)),ncol=0))

Predicted$Month_Yr <- paste(unique(Deaths_Model_Predict$Month_Yr))




# Now do the same to predict age distribution of deaths for countries with no death data

for (i in 1:length(countries_imputation)) {
  

  Country <- countries_imputation[i]
  
  Deaths_Model_Predict_2 <- Deaths_Model_Predict[which(Deaths_Model_Predict$Country == Country),]
  
  model4 <- lm(Death_u60 ~  Month_Yr + pop_u60  + full_vax_ge60 +
                 part_vax_ge60, data = Deaths_Model_Build )
  summary(model4)
  #with(summary(model4), 1 - deviance/null.deviance)
  
  Predict <- predict(model4, Deaths_Model_Predict_2)
  
  Predicted1 <- cbind(Deaths_Model_Predict_2$Month_Yr, predict(model4, Deaths_Model_Predict_2, na.action = na.exclude))
  
  colnames(Predicted1) <- c("Month_Yr", paste(Country))
  
  Predicted <- merge(Predicted, Predicted1 , by="Month_Yr", all=T)
  
}

# For Nicaragua: backfill the two places we're missing vaccination

Predicted_long <- gather(Predicted, Country, Deaths_prop_u60, -Month_Yr, factor_key=TRUE)

Predicted_long <- left_join(Predicted_long, WHO_deaths4, by=c("Country","Month_Yr"))

Predicted_long$WHO_deaths_le60 <- as.numeric(Predicted_long$Deaths_prop_u60) * Predicted_long$WHO_deaths
Predicted_long$WHO_deaths_ge60 <- (1-as.numeric(Predicted_long$Deaths_prop_u60)) * Predicted_long$WHO_deaths

#write.csv(Predicted_long, "Model_predictions_for_imputed_countries.csv")

Predicted_all <- rbind(Predicted_long, Predicted_full)

Predicted_all2 <- Predicted_all[,c(1,2,5,6)]

Predicted_all_longer <- gather(Predicted_all2, Age, Deaths_WHO, -Month_Yr, -Country, factor_key=TRUE)

Predicted_all_longer$Age_Cat <- ifelse(Predicted_all_longer$Age=="WHO_deaths_le60","18-59","60+")

Predicted_all_longer <- Predicted_all_longer[
  order( Predicted_all_longer[,2], Predicted_all_longer[,1] ),
]

Predicted_all_longer2 <- Predicted_all_longer[,c(1:2,4:5)]

Predicted_all_longer2$Deaths_WHO <- format(Predicted_all_longer2$Deaths_WHO, scientific = FALSE)


write.csv(Predicted_all_longer2, "Model_predictions_for_all_countries.csv")








