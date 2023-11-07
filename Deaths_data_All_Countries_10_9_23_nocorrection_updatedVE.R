# 10.9.23
# Alexandra Savinkina

library(tidyverse)
library(lubridate)
library(readxl)

# Read in data sources

Countries <- read.csv(file="Data/LAC Country list.csv")

Countries$X <- ifelse(Countries$X == "Bahamas, The", "Bahamas", Countries$X)
Countries$X <- ifelse(Countries$X == "Venezuela, RB", "Venezuela", Countries$X)

Countries1 <- Countries[,2]

Population <- read_excel('Data/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx', skip=16)

Population <- as.data.frame(Population)

Population$`Region, subregion, country or area *` <- ifelse(Population$`Region, subregion, country or area *`=="Bolivia (Plurinational State of)",
                                                            "Bolivia",Population$`Region, subregion, country or area *`)

Population$`Region, subregion, country or area *` <- ifelse(Population$`Region, subregion, country or area *`=="Venezuela (Bolivarian Republic of)",
                                                            "Venezuela",Population$`Region, subregion, country or area *`)


Population_21 <- Population[which(Population$Year=="2021"),]

COVerAGE <- read.csv(file='Data/COVerAGE_10.19.22_5yrage/Output_5.csv', skip=3, header=T)

PAHO_vaccinations <-read.csv(file="Data/htadf_pdf_total_filter.csv")

PAHO_vaccinations$COUNTRY_NAME <- ifelse(PAHO_vaccinations$COUNTRY_NAME=="PERÚ","PERU",PAHO_vaccinations$COUNTRY_NAME)

urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"

OWID_vaccinations <-read_csv(url(urlfile))

urlfile_WHO <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"

WHO_deaths <-read_csv(url(urlfile_WHO))

# Select out only countries

Population_Countries <- unique(Population$`Region, subregion, country or area *`)
COVerAGE_Countries <- unique(COVerAGE$Country)
WHO_Countries <- unique(WHO_deaths$Country)
PAHO_Countries <- unique(PAHO_vaccinations$COUNTRY_NAME)

PAHO_Countries <- str_to_title(PAHO_Countries)
OWID_Countries <- unique(OWID_vaccinations$location)


# All lists (14 countries):
#[1] "Argentina"          "Brazil"             "Chile"              "Colombia"          
#[5] "Costa Rica"         "Dominican Republic" "Guatemala"          "Honduras"          
#[9] "Jamaica"            "Mexico"             "Nicaragua"          "Paraguay"          
#[13] "El Salvador"        "Uruguay" 
countries_full <- Reduce(intersect, list(Countries1,Population_Countries,COVerAGE_Countries,PAHO_Countries,OWID_Countries))

#countries_full_2 <- Reduce(intersect, list(Countries1,Population_Countries))

#no_data = setdiff(Countries1,countries_full_2)

#countries_full2 <- "Peru"

countries_full2 <- c("Argentina","Brazil","Chile","Colombia",
                       "Paraguay",
                            "Uruguay", "Jamaica","Peru","Belize","Bolivia","Costa Rica","Ecuador",
                     "El Salvador","Guatemala","Honduras",
                     "Venezuela","Mexico"
                     )
  
col_names = c("Country","Observed deaths","Observed deaths, rate per 100,000 population","Deaths averted, average",
              "Deaths averted,low","Deaths averted, high","Deaths averted, average, per 100,000 pop",
              "Deaths averted,low, per 100,000 pop","Deaths averted, high, per 100,000 pop","Start Date","End Date","% over 60 vaccinated at end date",
              "% under 60 vaccinated at end date")

Deaths_averted_data <- data.frame(matrix(nrow = length(countries_full2), ncol = length(col_names))) 
colnames(Deaths_averted_data) <- col_names
plot_list <- list()

Model_data <- data.frame(matrix(nrow=0, ncol=9))
colnames(Model_data) <- c("Month_Yr"                     , "Age_Cat"         ,             
                                  "Country.x"                     ,"Partly_vaccinated_byAge"       ,"Fully_vaccinated_byAge",       
                                  "Booster_vaccinated_byAge"      ,"Country"                       ,"Deaths3",                
                                  "Deaths_noncum")
  
  

#################

# Population data


#################

for (i in 1:length(countries_full2)) {
  
Country= countries_full2[i]

#  Read in population data from UN 
#  Read in population data from UN 

#Population <- read_excel('Data/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx', skip=16)

#Population <- as.data.frame(Population)

# Keep most recent year: 2021 (close enough or do we want 2020?)

#Population_21 <- Population[which(Population$Year=="2021"),]

# Keep country/ies of interest



Population_21$Country <- Population_21$`Region, subregion, country or area *`

Population_Country <- Population_21[Population_21$Country==Country,]

# Subdivide age into groupings we are interested in:

Population_Country <- Population_Country %>% mutate_at(c(12:112), as.numeric,na.rm=TRUE)


Population_Country$Pop_18to59 <- rowSums(Population_Country[,c(30:71)]) # 18 to 59
Population_Country$Pop_60up <- rowSums(Population_Country[,c(72:112)]) # 60+
#Population_Country$Pop_65up <- rowSums(Population_Country[,c(77:112)]) # 65+

# Keep only data of interest

Population_Country <- Population_Country[,c(114:115)]

# Population is in thousands- we want full population

Population_Country <- Population_Country*1000

# Check to make sure this seems correct

#rowSums(Population_Country)

# The proportion of the population in each age group and total population for final dataset

Population_Country$TotalPop <- rowSums(Population_Country) #total adult (over 18) population
Population_Country$`18-59` <- Population_Country$Pop_18to59/Population_Country$TotalPop
Population_Country$`60+` <- Population_Country$Pop_60up/Population_Country$TotalPop


Population_Country$Country <- Country

# Put into needed format to merge later with vaccination data

Population_Country_long <- Population_Country %>% 
  pivot_longer(
    cols = `18-59`:`60+`, 
    names_to = "Age_Cat",
    values_to = "Proportion")

Population_Country_long$Population <- ifelse(Population_Country_long$Age_Cat=="18-59",
                                            Population_Country_long$Pop_18to59,
                                            ifelse(Population_Country_long$Age_Cat=="60+",
                                                   Population_Country_long$Pop_60up,NA))

#   Keep only necessary data points

Population_Country_long <- Population_Country_long[,4:7]


#################

# Population data


#################

# Upload COVerAGE deaths data in 5 year increments

#COVerAGE <- read.csv(file='Data/COVerAGE_10.19.22_5yrage/Output_5.csv', skip=3, header=T)



# Limit data to that of interest- Country, all regions, all genders

COVerAGE_Country <- COVerAGE[which(COVerAGE$Country==Country & COVerAGE$Region=="All"
                                   & COVerAGE$Sex=='b'),]

# Reformat date variable into date format

COVerAGE_Country$Date2 <- dmy(COVerAGE_Country$Date)


# Arrange by date

COVerAGE_Country<- COVerAGE_Country %>%
  arrange(., Date2)


# Deaths are cumulative- check for total number of deaths for both gut check with standard data and for later data check 

COVerAGE_Country_latest <- COVerAGE_Country %>% filter(Date2==max(Date2))
#sum(COVerAGE_Country_latest$Deaths)


# Create age categories of interest: 18-49, 50-64, 65+. We will exclude children.   

COVerAGE_Country$Age_Cat <- ifelse(COVerAGE_Country$Age <18, "Kid",
                                  ifelse(COVerAGE_Country$Age>=18 & COVerAGE_Country$Age<59, "18-59",
                                         ifelse(COVerAGE_Country$Age>=60, "60+",NA)))

# Dates data is input vary - we will standardize to monthly to make easier to match to vaccination data.

COVerAGE_Country$Month_Yr <- format(as.Date(COVerAGE_Country$Date2), "%Y-%m") 

# Sum deaths by age category and date

COVerAGE_Country <-  COVerAGE_Country %>% 
  group_by(Age_Cat,Date2) %>%
  mutate(Deaths2 = sum(Deaths))

# Collapse data by age category and month/year

COVerAGE_Country <- COVerAGE_Country %>%
  group_by(Country, Age_Cat, Month_Yr) %>% 
  summarise(Deaths3 = max(Deaths2))

# Exclude kids as we are not analyzing

COVerAGE_Country <- COVerAGE_Country[COVerAGE_Country$Age_Cat !="Kid",]

# Unsummarize the deaths data- we want it by month, not cumulative


COVerAGE_Country <- COVerAGE_Country %>% 
  group_by(Age_Cat) %>% 
  arrange(Month_Yr) %>% 
  mutate(Deaths_noncum = Deaths3 - lag(Deaths3, default = first(Deaths3)))

# The function that un-cumulates the deaths leaves the first date as 0 deaths- correct this

COVerAGE_Country$Deaths_noncum <- ifelse(COVerAGE_Country$Month_Yr=='2020-03', COVerAGE_Country$Deaths3, COVerAGE_Country$Deaths_noncum)

WHO_deaths <- read.csv(file="Model_predictions_for_all_countries.csv")

WHO_deaths <- WHO_deaths[which(WHO_deaths$Country==Country),c(2:5)]

COVerAGE_Country2 <- merge(COVerAGE_Country, WHO_deaths, by=c("Month_Yr","Country","Age_Cat"), all=T)

COVerAGE_Country2$Deaths_noncum <- ifelse(is.na(COVerAGE_Country2$Deaths_noncum), COVerAGE_Country2$Deaths_WHO,COVerAGE_Country2$Deaths_noncum)

COVerAGE_Country2$Deaths_noncum <- ifelse(COVerAGE_Country2$Country=="Honduras", COVerAGE_Country2$Deaths_WHO,COVerAGE_Country2$Deaths_noncum)


COVerAGE_Country2$Deaths_noncum <- as.numeric(COVerAGE_Country2$Deaths_noncum)
#COVEerAGE_Country2 <- ifelse(nrow(COVerAGE_Country2)==0, WHO_deaths,COVerAGE_Country2)

# Check that total number of deaths is correct/ the same as where we started!

#sum(COVerAGE_Country$Deaths_noncum)

# Check graphically

#ggplot(data=COVerAGE_Country, aes(x=Month_Yr, y=Deaths_noncum, group=Age_Cat, color=Age_Cat)) + geom_line()



###############################################################


# Import vaccinations data by age:

#PAHO_vaccinations <-read.csv(file="Data/htadf_pdf_total_filter.csv")
#
# Change all country names to lowercase to help with standardization

PAHO_vaccinations_Country <- PAHO_vaccinations[which(PAHO_vaccinations$COUNTRY_NAME==toupper(Country)),]

# Change dates to date format
# 
PAHO_vaccinations_Country$Date <- ymd(PAHO_vaccinations_Country$CUTT_OF_DATA_COUNTRY)

# Make new month/year only variable

PAHO_vaccinations_Country$Month_Yr <- format(as.Date(PAHO_vaccinations_Country$Date), "%Y-%m")

# Collapse to max vaccinations for each month/year (we only want one measure per month)

PAHO_vaccinations_Country2 <- PAHO_vaccinations_Country %>%
  group_by(COUNTRY_NAME, Month_Yr, AGE_GROUP) %>% 
  slice(which.max(PERCENT_ONE))

# Currently at least one includes anything greater than 1 vaccination- we want this to be partly
# vaccinated only

PAHO_vaccinations_Country2$AT_LEAST_ONE2 <- PAHO_vaccinations_Country2$AT_LEAST_ONE - PAHO_vaccinations_Country2$COMPLETE_SCH


# We want the sum of everyone vaccinated in that month/yr to find the proportion of vaccinations in
# each age group

PAHO_vaccinations_Country3 <- PAHO_vaccinations_Country2 %>%
  group_by(Month_Yr) %>%
  summarise(sum_AT_LEAST_ONE = sum(AT_LEAST_ONE2),
            sum_COMPLETE_SCH = sum(COMPLETE_SCH),
            sum_ADDITIONAL_1_DOSE = sum(ADDITIONAL_1_DOSE))

# Merge back together all data

PAHO_vaccinations_Country4 <- merge(PAHO_vaccinations_Country2,PAHO_vaccinations_Country3, by="Month_Yr")

# Now we will have a proportion of vaccine going to each age group
  
PAHO_vaccinations_Country4$Prop_AT_LEAST_ONE <- PAHO_vaccinations_Country4$AT_LEAST_ONE2/PAHO_vaccinations_Country4$sum_AT_LEAST_ONE
PAHO_vaccinations_Country4$Prop_COMPLETE_SCH <- PAHO_vaccinations_Country4$COMPLETE_SCH/PAHO_vaccinations_Country4$sum_COMPLETE_SCH
PAHO_vaccinations_Country4$Prop_ADDITIONAL_1_DOSE <- PAHO_vaccinations_Country4$ADDITIONAL_1_DOSE/PAHO_vaccinations_Country4$sum_ADDITIONAL_1_DOSE


# For missing values- set to 0

PAHO_vaccinations_Country4[is.na(PAHO_vaccinations_Country4)] <- 0

# For standardization, recode Age_Cat

PAHO_vaccinations_Country4$Age_Cat <- PAHO_vaccinations_Country2$AGE_GROUP

# 

PAHO_vaccinations_Country4$PERCENT_ONE_1 <- PAHO_vaccinations_Country4$PERCENT_ONE -  PAHO_vaccinations_Country4$PERCENT_COMPLETE
PAHO_vaccinations_Country4$PERCENT_COMPLETE_1 <- PAHO_vaccinations_Country4$PERCENT_COMPLETE 
#- PAHO_vaccinations_Country4$PERCENT_ADDITIONAL_1   


PAHO_vaccinations_Country5 <- PAHO_vaccinations_Country4[which(PAHO_vaccinations_Country4$Age_Cat !="<18"
                                                               & PAHO_vaccinations_Country4$Month_Yr >"2021-08" ),c(1,10,13,20:25)]

if (Country=="Mexico") {
  
  PAHO_vaccinations_Country5$Prop_AT_LEAST_ONE <- ifelse(PAHO_vaccinations_Country5$Prop_AT_LEAST_ONE=="0", NA,
                                                         PAHO_vaccinations_Country5$Prop_AT_LEAST_ONE)
  
  PAHO_vaccinations_Country5$Prop_COMPLETE_SCH <- ifelse(PAHO_vaccinations_Country5$Prop_COMPLETE_SCH=="0", NA,
                                                         PAHO_vaccinations_Country5$Prop_COMPLETE_SCH)
  
  PAHO_vaccinations_Country5$Prop_ADDITIONAL_1_DOSE <- ifelse(PAHO_vaccinations_Country5$Prop_ADDITIONAL_1_DOSE =="0", NA,
                                                         PAHO_vaccinations_Country5$Prop_ADDITIONAL_1_DOSE )

  PAHO_vaccinations_Country5 <- PAHO_vaccinations_Country5%>% 
  group_by(Age_Cat) %>% 
    fill(Prop_AT_LEAST_ONE, Prop_COMPLETE_SCH, Prop_ADDITIONAL_1_DOSE,n, .direction = "downup")
  
    
}


#& PAHO_vaccinations_Country4$Month_Yr >"2021-08" 
PAHO_vaccinations_Country5$Country <- Country

                                     
#########################################

# Import vaccinations data by age:

#library (readr)

#urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"

#OWID_vaccinations <-read_csv(url(urlfile))

# Limit to country/ies of interest

OWID_vaccinations_Country <- OWID_vaccinations[OWID_vaccinations$location==Country,]

#   Format date variables and create month/year variable

OWID_vaccinations_Country$Date2 <- ymd(OWID_vaccinations_Country$date)

OWID_vaccinations_Country$Month_Yr <- format(as.Date(OWID_vaccinations_Country$Date2), "%Y-%m")

# We want one obeservation per month (the last one)

OWID_vaccinations_Country <- OWID_vaccinations_Country %>%
  group_by(location, Month_Yr) %>% 
  slice(which.max(people_vaccinated))

# Calculate people who are partly vaccinated based on those vaccinated and those fully vaccinated

OWID_vaccinations_Country$people_partly_vaccinated <- OWID_vaccinations_Country$people_vaccinated - OWID_vaccinations_Country$people_fully_vaccinated

OWID_vaccinations_Country$people_partly_vaccinated <- ifelse(OWID_vaccinations_Country$Month_Yr=="2021-01", OWID_vaccinations_Country$people_vaccinated,
                                                            OWID_vaccinations_Country$people_partly_vaccinated)

#    Keep variables of interest

OWID_vaccinations_Country <- OWID_vaccinations_Country[,c(1:2,4:7,18:19)]

OWID_vaccinations_Country$Country <- OWID_vaccinations_Country$location

# For Venezuela: if missing # people vaccinated, put # total vaccinations

OWID_vaccinations_Country$people_partly_vaccinated <- ifelse(is.na(OWID_vaccinations_Country$people_partly_vaccinated) & !is.na(OWID_vaccinations_Country$people_vaccinated),
                                                             OWID_vaccinations_Country$people_vaccinated, OWID_vaccinations_Country$people_partly_vaccinated)


# merge vaccination data with population data 

OWID_vaccinations_Country2 <- merge(OWID_vaccinations_Country,Population_Country_long,by=c("Country"))

OWID_vaccinations_Country3 <- merge(OWID_vaccinations_Country2,PAHO_vaccinations_Country5, by=c("Month_Yr", "Age_Cat"), all=TRUE)

OWID_vaccinations_Country3 <- OWID_vaccinations_Country3%>% 
  group_by(Age_Cat) %>% 
  fill(Prop_AT_LEAST_ONE, Prop_COMPLETE_SCH, Prop_ADDITIONAL_1_DOSE,n, .direction = "downup")

#Special case- for Mexico, set some of the numbers for percent 1/percent full to NA- they are incorrect
#
#
OWID_vaccinations_Country3$PERCENT_ONE_1 <- ifelse(OWID_vaccinations_Country3$Country.x=="Mexico" &
                                                     OWID_vaccinations_Country3$PERCENT_ONE_1==0,NA,OWID_vaccinations_Country3$PERCENT_ONE_1)

OWID_vaccinations_Country3$PERCENT_COMPLETE_1 <- ifelse(OWID_vaccinations_Country3$Country.x=="Mexico" &
                                                     OWID_vaccinations_Country3$PERCENT_COMPLETE_1==0,NA,OWID_vaccinations_Country3$PERCENT_COMPLETE_1)

OWID_vaccinations_Country3$Partly_vaccinated_byAge <- ifelse(is.na(OWID_vaccinations_Country3$PERCENT_ONE_1),  
                                                             (OWID_vaccinations_Country3$people_partly_vaccinated*OWID_vaccinations_Country3$Prop_AT_LEAST_ONE)/OWID_vaccinations_Country3$n,
                                                             OWID_vaccinations_Country3$PERCENT_ONE_1/100)

OWID_vaccinations_Country3$Partly_vaccinated_byAge <- ifelse(is.na(OWID_vaccinations_Country3$PERCENT_ONE_1),  
                                                            (OWID_vaccinations_Country3$people_partly_vaccinated*OWID_vaccinations_Country3$Prop_AT_LEAST_ONE)/OWID_vaccinations_Country3$n,
                                                            OWID_vaccinations_Country3$PERCENT_ONE_1/100)

OWID_vaccinations_Country3$Fully_vaccinated_byAge <- ifelse(is.na(OWID_vaccinations_Country3$PERCENT_COMPLETE_1), 
                                                           (OWID_vaccinations_Country3$people_fully_vaccinated*OWID_vaccinations_Country3$Prop_COMPLETE_SCH)/OWID_vaccinations_Country3$n,
                                                           OWID_vaccinations_Country3$PERCENT_COMPLETE_1/100)

OWID_vaccinations_Country3$Booster_vaccinated_byAge <- ifelse(is.na(OWID_vaccinations_Country3$PERCENT_ADDITIONAL_1), 
                                                          0,
                                                           OWID_vaccinations_Country3$PERCENT_ADDITIONAL_1/100)

OWID_vaccinations_Country3$Fully_vaccinated_byAge <- ifelse(is.na(OWID_vaccinations_Country3$Fully_vaccinated_byAge) , 0,
                                                            OWID_vaccinations_Country3$Fully_vaccinated_byAge)

OWID_vaccinations_Country3$Fully_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Fully_vaccinated_byAge>1 , 1,
                                                            OWID_vaccinations_Country3$Fully_vaccinated_byAge)


# Special cases:
# 
# Colombia: and 7/2021 and 8/2021 60+ shows unexpected rates of vaccination that are not consistent with dates before and after.
# Will set both equal to months before/after:
# 

OWID_vaccinations_Country3$Fully_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Country.x=="Colombia" & OWID_vaccinations_Country3$Month_Yr=="2021-08" &
                                                              OWID_vaccinations_Country3$Age_Cat=="60+" ,
                                                            0.739, OWID_vaccinations_Country3$Fully_vaccinated_byAge)

OWID_vaccinations_Country3$Partly_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Country.x=="Colombia" & OWID_vaccinations_Country3$Month_Yr=="2021-08"&
                                                               OWID_vaccinations_Country3$Age_Cat=="60+" ,
                                                             0.119, OWID_vaccinations_Country3$Partly_vaccinated_byAge)

OWID_vaccinations_Country3$Fully_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Country.x=="Colombia" & OWID_vaccinations_Country3$Month_Yr=="2021-07"&
                                                              OWID_vaccinations_Country3$Age_Cat=="60+" ,
                                                            0.4489515648, OWID_vaccinations_Country3$Fully_vaccinated_byAge)

OWID_vaccinations_Country3$Partly_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Country.x=="Colombia" & OWID_vaccinations_Country3$Month_Yr=="2021-07"&
                                                               OWID_vaccinations_Country3$Age_Cat=="60+" ,
                                                             0.1024920179, OWID_vaccinations_Country3$Partly_vaccinated_byAge)

# Uruguay: in 8/2021 and 7/2021, 60+, 100% fully vaccinated and still some small % showing partly vaccinated. Wills set to 0.
# 



OWID_vaccinations_Country3$Partly_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Country.x=="Uruguay" & (OWID_vaccinations_Country3$Month_Yr=="2021-07"|OWID_vaccinations_Country3$Month_Yr=="2021-08")&
                                                               OWID_vaccinations_Country3$Age_Cat=="60+" ,
                                                             0, OWID_vaccinations_Country3$Partly_vaccinated_byAge)

# Bolivia : in 9/21 and 10/21 booster values don't make sense (76%-100% stated as booster-vaccinated: impossible).
# Set to 0
# 

OWID_vaccinations_Country3$Booster_vaccinated_byAge <- ifelse(OWID_vaccinations_Country3$Country.x=="Bolivia" & (OWID_vaccinations_Country3$Month_Yr=="2021-09"|OWID_vaccinations_Country3$Month_Yr=="2021-10"),
                                                             0, OWID_vaccinations_Country3$Booster_vaccinated_byAge)




# Some data generally looks a little odd (subsequent months having lower vaccination than preceeding months),
# So correcting this- code will keep higher preceeding value if present. This is only after checking for outliers (like
# in Colombia)
# 


OWID_vaccinations_Country3<- OWID_vaccinations_Country3 %>%
  group_by(Age_Cat) %>%
  mutate(Fully_vaccinated_byAge = cummax(Fully_vaccinated_byAge)
  ) %>%
  ungroup()


# Limit to data of interest

OWID_vaccinations_Country_4 <- OWID_vaccinations_Country3[which(OWID_vaccinations_Country3$Month_Yr<'2022-06'),c(1:3,12,21:23)] #23
OWID_vaccinations_Country_4$Country <- Country

OWID_vaccinations_Country_4[OWID_vaccinations_Country_4<0] <- 0

###############################################################

# Merge vaccinations and deaths data

Model_data_Country <- merge(OWID_vaccinations_Country_4,COVerAGE_Country2,by=c("Country","Month_Yr","Age_Cat"))
Model_data_Country2 <- left_join(OWID_vaccinations_Country_4,COVerAGE_Country2,by=c("Country","Month_Yr","Age_Cat"))


# Apply multiplier to account for underreporting of deaths. This is from Lancet 2021 paper on Peru underreporting of
#   COVID deaths which stated that deaths were undercounted by 37% (108k reported, 173k true)
#Model_data_Country$Deaths_noncum <- Model_data_Country$Deaths_noncum/(1-0.37)


#Model_data_Country$Fully_vaccinated_byAge <- ifelse(Model_data_Country$Month_Yr=="2021-01",0,Model_data_Country$Fully_vaccinated_byAge)

# Assign vaccine effectiveness low and high

Vaccine_effectiveness_over65_full_avg <- .8076
Vaccine_effectiveness_over65_part_avg <- .526
Vaccine_effectiveness_over65__boost_avg <- .925

Vaccine_effectiveness_over65_full_high <- .936
Vaccine_effectiveness_over65_part_high <- .737
Vaccine_effectiveness_over65__boost_high <- .99

Vaccine_effectiveness_over65_full_low <- .612
Vaccine_effectiveness_over65_part_low <- .157
Vaccine_effectiveness_over65__boost_low <- .90

Vaccine_effectiveness_under65_full_avg <- .8486
Vaccine_effectiveness_under65_part_avg <- .5534
Vaccine_effectiveness_under65__boost_avg <- .971

Vaccine_effectiveness_under65_full_high <- .945
Vaccine_effectiveness_under65_part_high <- .625
Vaccine_effectiveness_under65__boost_high <- .99

Vaccine_effectiveness_under65_full_low <- .678 
Vaccine_effectiveness_under65_part_low <- .353
Vaccine_effectiveness_under65__boost_low <- .90


# Error check- if booster vaccinated is greater than fully vaccinated, set fully vaccinated to 0

# Model_data_Country$Fully_vaccinated_byAge <- ifelse(Model_data_Country$Fully_vaccinated_byAge>Model_data_Country$Booster_vaccinated_byAge
#                                                     ,Model_data_Country$Fully_vaccinated_byAge, 0)


# Calculate counterfactual deaths given low and high effectiveness

Model_data_Country$Deaths_Counterfactual_high2 <- ifelse(Model_data_Country$Age_Cat=="18-59", Model_data_Country$Deaths_noncum/
                                                           (1-(Model_data_Country$Partly_vaccinated_byAge*Vaccine_effectiveness_under65_part_high)-
                                                              ((Model_data_Country$Fully_vaccinated_byAge-Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_under65_full_high)-
                                                              (Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_under65__boost_high),
                                                       Model_data_Country$Deaths_noncum/
                                                         (1-(Model_data_Country$Partly_vaccinated_byAge*Vaccine_effectiveness_over65_part_high)-
                                                            ((Model_data_Country$Fully_vaccinated_byAge-Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_over65_full_high)-
                                                            (Model_data_Country$Booster_vaccinated_byAge*Vaccine_effectiveness_over65__boost_high)))


Model_data_Country$Deaths_Counterfactual_low2 <- ifelse(Model_data_Country$Age_Cat=="18-59", Model_data_Country$Deaths_noncum/
                                                          (1-(Model_data_Country$Partly_vaccinated_byAge*Vaccine_effectiveness_under65_part_low)-
                                                             ((Model_data_Country$Fully_vaccinated_byAge-Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_under65_full_low)-
                                                             (Model_data_Country$Booster_vaccinated_byAge*Vaccine_effectiveness_under65__boost_low)),
                                                       Model_data_Country$Deaths_noncum/
                                                         (1-(Model_data_Country$Partly_vaccinated_byAge*Vaccine_effectiveness_over65_part_low)-
                                                            ((Model_data_Country$Fully_vaccinated_byAge-Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_over65_full_low)-
                                                            (Model_data_Country$Booster_vaccinated_byAge*Vaccine_effectiveness_under65__boost_low)))

Model_data_Country$Deaths_Counterfactual_average <- ifelse(Model_data_Country$Age_Cat=="18-59", Model_data_Country$Deaths_noncum/
                                                             (1-(Model_data_Country$Partly_vaccinated_byAge*Vaccine_effectiveness_under65_part_avg)-
                                                                ((Model_data_Country$Fully_vaccinated_byAge-Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_under65_full_avg)-
                                                                (Model_data_Country$Booster_vaccinated_byAge*Vaccine_effectiveness_under65__boost_avg)),
                                                       Model_data_Country$Deaths_noncum/
                                                         (1-(Model_data_Country$Partly_vaccinated_byAge*Vaccine_effectiveness_over65_part_avg)-
                                                            ((Model_data_Country$Fully_vaccinated_byAge-Model_data_Country$Booster_vaccinated_byAge)*Vaccine_effectiveness_over65_full_avg)-
                                                            (Model_data_Country$Booster_vaccinated_byAge*Vaccine_effectiveness_over65__boost_avg)))


Model_data_Country <- Model_data_Country[which(is.na(Model_data_Country$Partly_vaccinated_byAge & Model_data_Country$Fully_vaccinated_byAge)==0),]

# sum(Model_data_Country$Deaths_Counterfactual_high, na.rm = TRUE)
# sum(Model_data_Country$Deaths_Counterfactual_low, na.rm = TRUE)
# sum(Model_data_Country$Deaths_noncum, na.rm = TRUE)

# Calculate deaths averted by low and high vaccine effectiveness, rounded to nearest thousand

deaths_averted_high <- round(sum(Model_data_Country$Deaths_Counterfactual_high2, na.rm = TRUE) -sum(Model_data_Country$Deaths_noncum, na.rm = TRUE),-2)
deaths_averted_low <- round(sum(Model_data_Country$Deaths_Counterfactual_low2, na.rm = TRUE) -sum(Model_data_Country$Deaths_noncum, na.rm = TRUE),-2)
deaths_averted_avg <- round(sum(Model_data_Country$Deaths_Counterfactual_average, na.rm = TRUE) -sum(Model_data_Country$Deaths_noncum, na.rm = TRUE),-2)

max_deaths <- sum(Model_data_Country$Deaths_noncum)

StartDate <- min(Model_data_Country$Month_Yr)
EndDate <-max(Model_data_Country$Month_Yr)
FullyVax_60 <- max(Model_data_Country$Fully_vaccinated_byAge[which(Model_data_Country$Age_Cat=="60+")])*100
FullyVax_U60 <-max(Model_data_Country$Fully_vaccinated_byAge[which(Model_data_Country$Age_Cat=="18-59")])*100

Deaths_text <- paste("COVID-19 deaths averted by vaccination,", StartDate,"-",EndDate,":", deaths_averted_avg, "(",
                      deaths_averted_low,",",deaths_averted_high,")")


# Check graphically
# 
# Model_data_Country_graph <- Model_data_Country %>% 
#   pivot_longer(
#     cols = `Deaths_noncum`:`Deaths_Counterfactual_low`, 
#     names_to = "Scenario",
#     values_to = "Deaths")
# 
# Model_data_Country_graph$Scenario <- ifelse(Model_data_Country_graph$Scenario=="Deaths_noncum","Observed deaths",
#                                            ifelse(Model_data_Country_graph$Scenario=="Deaths_Counterfactual_high",
#                                            "Modeled deaths with no vaccination, \nhigh effectiveness of vaccine",
#                                            "Modeled deaths with no vaccination, \nlow effectiveness of vaccine"))
# 
# Model_data_Country_graph$Graph_Group <- paste0(Model_data_Country_graph$Age_Cat,",",Model_data_Country_graph$Scenario)
# 
# ggplot(data=Model_data_Country_graph, aes(x=Month_Yr, y=Deaths, group=Graph_Group, color=Scenario)) + geom_line(aes(linetype=Age_Cat))

# Plot graphically

# Country_plot <- ggplot(data=Model_data_Country) + geom_line(aes(x=Month_Yr, y=Deaths_noncum, group=Age_Cat, linetype=Age_Cat)) + 
#   geom_line(aes(x=Month_Yr, y=Deaths_Counterfactual_average, group=Age_Cat, linetype=Age_Cat), color="grey") + 
#   geom_ribbon(aes(x=Month_Yr, ymin=Deaths_Counterfactual_low, ymax=Deaths_Counterfactual_high), alpha=1) +
#   theme_classic() +scale_color_discrete(name="") + xlab("Date") + ylab("COVID-19 deaths
#   

# Honduras has a gap in population due to missing data- fill in with population numbers by age group to calculate death
# # incidence rate

if (Country=="Honduras") {
  
  Model_data_Country$Population <- ifelse(is.na(Model_data_Country$Population) & Model_data_Country$Age_Cat=="18-59", 5820614,
                                          ifelse(is.na(Model_data_Country$Population) & Model_data_Country$Age_Cat=="60+", 673334,Model_data_Country$Population))  
  
}

if (Country=="Colombia") {
  
  Model_data_Country$Population <- ifelse(is.na(Model_data_Country$Population) & Model_data_Country$Age_Cat=="18-59", 31215224,
                                          ifelse(is.na(Model_data_Country$Population) & Model_data_Country$Age_Cat=="60+", 6764869,Model_data_Country$Population))  
  
}

Model_data_Country$Age_Cat2 <- ifelse(Model_data_Country$Age_Cat=="18-59", "18-59 years", "60+ years")


Country_plot <- ggplot(data=Model_data_Country, aes(x=Month_Yr, y=Deaths_noncum, group=Age_Cat)) + geom_line(aes(linetype=Age_Cat)) + 
  geom_ribbon(aes(ymin=Deaths_noncum, ymax=Deaths_Counterfactual_average, fill=Age_Cat), alpha=0.5) +
  theme_classic() +scale_color_discrete(name="") + xlab("Date") + ylab("COVID-19 deaths") + ggtitle(Country) + 
  labs(caption = "No correction for underreporting of COVID-19 mortality")


Countryplot_bytime <-ggplot(data=Model_data_Country) + geom_line(aes(x=Month_Yr, y=(Deaths_noncum/Population)*100000, group=Age_Cat), linetype="solid") +
  geom_line(aes(x=Month_Yr, y=(Deaths_Counterfactual_average/Population)*100000, group=Age_Cat), linetype="dashed")+ 
  geom_ribbon(aes(x=Month_Yr, ymin=(Deaths_Counterfactual_low2/Population)*100000, ymax=(Deaths_Counterfactual_high2/Population)*100000, group=Age_Cat), alpha=0.2) +
  theme_classic() +scale_color_discrete(name="") + xlab("Date") + ylab("COVID-19 deaths, per 100,000 people")+guides(colour = "none", fill=guide_legend(title="Age"))+
  ggtitle(Country) + 
  labs(caption = "No correction for underreporting of COVID-19 mortality")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text=element_text(size=20)) +  
  facet_wrap(~ Age_Cat2) 



# Country_plot <- ggplot(data=Model_data_Country, aes(x=Month_Yr, y=Deaths_noncum, group=Age_Cat)) + geom_line(aes(linetype=Age_Cat)) + 
#   geom_ribbon(aes(ymin=Deaths_Counterfactual_low, ymax=Deaths_Counterfactual_high, fill=Age_Cat), alpha=0.5) +
#   theme_classic() +scale_color_discrete(name="") + xlab("Date") + ylab("COVID-19 deaths")

Country_plot1  <- Country_plot+theme(axis.text.x = element_text(angle = 90)) + annotate("text", label= Deaths_text) 
Countryplot_bytime1  <- Countryplot_bytime+theme(axis.text.x = element_text(angle = 90))

plot_list[[i]] <- Countryplot_bytime1
population <- as.numeric(Population_Country$Pop_18to59 + Population_Country$Pop_60up)

Deaths_averted_data[i,1] <- Country
Deaths_averted_data[i,2] <- sum(Model_data_Country$Deaths_noncum)
Deaths_averted_data[i,3] <- (sum(Model_data_Country$Deaths_noncum)/population)*100000
Deaths_averted_data[i,4] <- deaths_averted_avg
Deaths_averted_data[i,5] <- deaths_averted_low
Deaths_averted_data[i,6] <- deaths_averted_high
Deaths_averted_data[i,7] <- (deaths_averted_avg/population)*100000
Deaths_averted_data[i,8] <- (deaths_averted_low/population)*100000
Deaths_averted_data[i,9] <- (deaths_averted_high/population)*100000
Deaths_averted_data[i,10] <- StartDate
Deaths_averted_data[i,11] <- EndDate
Deaths_averted_data[i,12] <- FullyVax_60
Deaths_averted_data[i,13] <- FullyVax_U60

Model_data <- rbind(Model_data, Model_data_Country)

}

library(janitor)
Deaths_averted_data <- Deaths_averted_data %>%
                        adorn_totals("row")

Deaths_averted2 <- Deaths_averted_data


Deaths_averted_data[,c(2:5)] <- sapply(Deaths_averted_data[,c(2:5)], function(x) scales::comma(x)) 

#write.csv(Model_data, file="Model_data_fullcountries_92623_nocorrection.csv")

#write.csv(Model_data, file="Model_data_fullcountries_4323_impute.csv")

#plot_list
 
#write.csv(Model_data_Country, file="Model_data_Country_2.1.23.csv")

library(gridExtra)

# pdf(file="Country_Data_Table_91823_nocorrection.pdf", width=40)
# grid.table(Deaths_averted_data)
# dev.off()

#write.csv(Deaths_averted_data, "Output/Supplementary_Table_2V2_91823.csv")
  
pdf(file="Output/Supplementary_Figure_3_10923.pdf", width=20)
for (i in seq(length(plot_list))) {
  do.call("grid.arrange", plot_list[i])  
}
dev.off()
