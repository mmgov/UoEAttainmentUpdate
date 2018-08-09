# Lee's version

# Packages ----------------------------------------------------------------
library("tidyverse")

# Data --------------------------------------------------------------------

# import new raw data 2017  "Beginning with the most recent data"

NWALevel <- read_csv("Raw Data/Wales/A Level/WalesAlevelRImport.csv",
                     na = c('*','','na')) %>% 
  unite(DfENum,"LA Code","Estab Code", sep = "") %>% 
  rename(`School Name` = `Estab Name`,
         RawScore2017 = `Average wider points score for pupils aged 17`)

# import older data 2014 to 2016

OWALevel <- read_csv("Raw Data/Wales/A Level/WalesALevel2014-2016SS.csv",
                     na = c('*','','na', 'No Data', 'No data', 'DC')
) %>% 
  rename(DfENum = `EUCLID DfE Number`,
         RawScore2015 = `Average wider points score for pupils aged 17 2015`,
         RawScore2016 = `Average wider points score for pupils aged 17 2016`) %>% 
  select(-`match on school code`,
         -`school code same in both sheets`,
         -`Upload check`,
         -`Upload double check`,
         -`Change check 2014-15`,
         -`Change check 2015-16`) %>% 
  slice(1:162) %>% 
  mutate(DfENum = as.character(DfENum),
         `EUCLID School Code` = as.character(`EUCLID School Code`),
         `Average wider points score for pupils aged 17 2014` = as.numeric(`Average wider points score for pupils aged 17 2014`),
         `RawScore2015` = as.double(`RawScore2015`),
         `RawScore2016` = as.double(`RawScore2016`)
  )

# Indentify new schools ---------------------------------------------------

# Now we indentify schools which are present in the new data but not in the old data.
# This is so that they can be looked up online and a decision made on whether they should be included in the dataset (because they are new schools) or not included (as they are Pupil Referal Units or Special Schools)
# This code makes a list of the DfE Numbers which are in the new data but NOT in the old data 
# This code makes a new dataframe subsetting on the new data, including only those schools not in the new data
# Then this dataframe can be exported into a csv so that me or an intern can web search each one and establish if it should be included in the dataset

missingfromnew <- setdiff(NWALevel$DfENum,OWALevel$DfENum)

NWALevelnotinold <- NWALevel[NWALevel$DfENum %in% missingfromnew, ]

write_csv(NWALevelnotinold, path = "Raw Data/NWALevelnotinold.csv")

# Add new schools to old data ---------------------------------------------

# After the new schools have been identified they can be added to the old data

newsch <- NWALevel %>% 
  select(DfENum,`School Name`) %>%
  filter(DfENum %in% c(6675502,6644021))

OWALevel <- bind_rows(OWALevel, newsch) %>% 
  mutate(`School type`= case_when(
    DfENum == 6675502 ~ 'Comprehensive School',  
    DfENum == 6644021 ~ 'Comprehensive School',
    TRUE ~ `School type`
  )) %>% 
  mutate(`EUCLID School Code`= case_when(
    DfENum == 6675502 ~ '19611',  
    DfENum == 6644021 ~ '15821',
    TRUE ~ `EUCLID School Code`
  )) %>% 
  mutate(`EUCLID National Centre Number`= case_when(
    DfENum == 6675502 ~ '68251',
    DfENum == 6644021 ~ '68122',
    TRUE ~ `EUCLID National Centre Number`
  ))

# Merge Old and New data --------------------------------------------------

WALevel2014to2017 <- OWALevel %>% 
  left_join(select(NWALevel, DfENum, RawScore2017),
            by = 'DfENum')
  
# Rearrange columns  
WALevel2014to2017 <- WALevel2014to2017[c(1,2,3,4,5,6,7,8,9,10,11,15,12,13,14)]

# Remove schools with 3 years of no data ----------------------------------

# Now remove schools with 3 years of no data. The below code actually retains only those schools have have data in each of the three years, not what we want!

WALevel2014to2017New <- WALevel2014to2017 %>% 
  filter(!is.na(`RawScore2015`) &
           !is.na(`RawScore2016`) &
           !is.na(`RawScore2017`))

#However this code below creats a DF which only contains the schools which don't have data in each of the three years.

WALevel2014to2017NoData <- WALevel2014to2017 %>% 
  filter(is.na(`RawScore2015`) &
            is.na(`RawScore2016`) &
            is.na(`RawScore2017`))

#This can then be used remove the schools from the main dataset

removethreeyearsnodata <- setdiff(WALevel2014to2017$DfENum,WALevel2014to2017NoData$DfENum)

WALevel2014to2017 <- WALevel2014to2017[WALevel2014to2017$DfENum  %in% removethreeyearsnodata, ]

#Now we a DF with 160 schools, no schools with three years of no data remain. 

# Z Score -----------------------------------------------------------------

# R has a function called scale which calculates Z score, however  

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2017`= (scale(WALevel2014to2017$`Average wider points score for pupils aged 17 2017`,
                                center=TRUE,
                                scale=TRUE)))

# the funcion "scale" can be used for Z scores but then it turns the result into a "matrix" which can't be averaged with the other Z scores from 2015 and 2016. Also scale assumes you are using a sample and so calculates the sample SD. We have the population (as in our dataset contains all the schools we are interested in, and not just a sample of all the schools we are interested). Using sample SD means that the resulting Z score is just a little bit wrong (by 0.01 usually) For both these reasons I'm calculating the Z score by formula.

#First calculate population standard deviation

x <- WALevel2014to2017$`RawScore2017`
x <-x[!is.na(x)]
mu <- mean(x,trim = 0)
totalvar <- sum((x-mu)^2)
pop_sd <- sqrt(totalvar/length(x))

#Now calculate Z score using population SD

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2017`= (`RawScore2017`- 
                            mean(`RawScore2017`, na.rm=TRUE))/pop_sd)

#Now round to 2 decimal places
WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2017`= round(WALevel2014to2017$`Z Score 2017`,digits=2))

#Rearrange columns  

WALevel2014to2017<-WALevel2014to2017[c(1,2,3,4,5,6,7,8,9,10,11,12,16,13,14,15)]

# Recalculate Z scores for earlier years in case of mergers
#First 2016
#First calculate population standard deviation

x <- WALevel2014to2017$`RawScore2016`
x <-x[!is.na(x)]
mu <- mean(x,trim = 0)
totalvar <- sum((x-mu)^2)
pop_sd <- sqrt(totalvar/length(x))

#Now calculate Z score using population SD

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2016`= (`RawScore2016`- 
                            mean(`RawScore2016`, na.rm=TRUE))/pop_sd)

#Now round to 2 decimal places
WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2016`= round(WALevel2014to2017$`Z Score 2016`,digits=2))

# now 2015
#First calculate population standard deviation

x <- WALevel2014to2017$`RawScore2015`
x <-x[!is.na(x)]
mu <- mean(x,trim = 0)
totalvar <- sum((x-mu)^2)
pop_sd <- sqrt(totalvar/length(x))

#Now calculate Z score using population SD

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2015`= (`RawScore2015`- 
                            mean(`RawScore2015`, na.rm=TRUE))/pop_sd)

#Now round to 2 decimal places
WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2015`= round(WALevel2014to2017$`Z Score 2015`,digits=2))

# Three year average ------------------------------------------------------

#Creating new 3 year average 2015, 2016, 2017 

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate (`Average Z Score 2015-2017` = rowMeans(WALevel2014to2017[,c(9,11,13)], na.rm = TRUE))

#round to 2 decimal places
WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Average Z Score 2015-2017`= round(WALevel2014to2017$`Average Z Score 2015-2017`,digits=2))

# Expore to CSV so results can be compared to those already calculated in Excel
write.csv(WALevel2014to2017, file = "WCheck.csv")

# as R is rounding based on the three Z scores being to two decimal places. 14 out of the 160 schools come out as having a different three year average to . The difference is only by 0.01 each but this should be made consistent. The Z scores in Excel should be rounded to 2 decimal places. 

# Assign above below average value ----------------------------------------

WALevel2014to2017$`Above or Below Average new` <- NA
WALevel2014to2017$`Above or Below Average new`[WALevel2014to2017$`Average Z Score 2015-2017`<=0] <- "BELOW AVERAGE"
WALevel2014to2017$`Above or Below Average new`[WALevel2014to2017$`Average Z Score 2015-2017`>0] <- "ABOVE AVERAGE"

# Validation --------------------------------------------------------------

# Validate data 1) Validate Average, SD and Z score for each year by calculating the figures again another way 2) Volatility check, did the Z score change more than 2 (in either direction) between 2015 and 2016 or between 2016 and 2017?

#Validate 2015 mean & SD
x <- WALevel2014to2017$`RawScore2015`
x <-x[!is.na(x)]
mu <- mean(x,trim = 0)

mu==sum(x)/length(x)

#Validate 2015 SD
# Function for population SD

sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}

totalvar <- sum((x-mu)^2)
pop_sd <- sqrt(totalvar/length(x))

sd.p(x)==pop_sd

# Validate 2015 Z scores

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2015check`= (scale(WALevel2014to2017$`RawScore2015`,center=TRUE,scale=TRUE)))

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Z Score 2015check`= round(WALevel2014to2017$`Z Score 2015check`,digits=2))

# can't be turned into 2 decimal places

install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")

library("R.basic")

x <- as.data.frame(WALevel2014to2017$`RawScore2015`)

zscore
scores<-zscore(x,robust=TRUE)

# could try another way of making a z score?

# volitility check

# First 2015-2016
WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Change check 2015-16`=abs((WALevel2014to2017$`Z Score 2015`-WALevel2014to2017$`Z Score 2016`)))

WALevel2014to2017$`1516bi` <- NA
WALevel2014to2017$`1516bi`[WALevel2014to2017$`Change check 2015-16`<=2] <- "OK"
WALevel2014to2017$`1516bi`[WALevel2014to2017$`Change check 2015-16`>2] <- "Big Change"

#subset all those who are greater than 2

greaterthantwo1516 <- subset (WALevel2014to2017,`Change check 2015-16`>2)

WALevel2014to2017$`Change check 2015-16`[WALevel2014to2017$`Change check 2015-16`]

# Then 2016-2017

WALevel2014to2017 <- WALevel2014to2017 %>% 
  mutate(`Change check 2016-17`=abs((WALevel2014to2017$`Z Score 2016`-WALevel2014to2017$`Z Score 2017`)))

WALevel2014to2017$`1617bi` <- NA
WALevel2014to2017$`1617bi`[WALevel2014to2017$`Change check 2016-17`<=2] <- "OK"
WALevel2014to2017$`1617bi`[WALevel2014to2017$`Change check 2016-17`>2] <- "Big Change"

#subset all those who are greater than 2

greaterthantwo1617 <- subset (WALevel2014to2017,`Change check 2016-17`>2)

# Remove columns no longer needed -----------------------------------------

WALevel2014to2017$`Average wider points score for pupils aged 17 2014`=NULL
WALevel2014to2017$`Z score 2014`=NULL
WALevel2014to2017$`Average Z score 2014-2016`=NULL
WALevel2014to2017$`Above or Below Average`=NULL