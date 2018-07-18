# Packages ----------------------------------------------------------------

library ("tidyverse")

# Import new data ---------------------------------------------------------

# import new raw data 2017  "Beginning with the most recent data"

NNIALevel <- read.csv("Raw Data/NI/A Level/NIAlevelRImport.csv")
View(NNIALevel)

# rename "DENI.ref" from the new data to " to match existing code

colnames(NNIALevel)[colnames(NNIALevel)=="DENI.Ref"] <- "DfENum"


#Change class of DfENum
class(NNIALevel$DfENum) = "character"




# Import older data -------------------------------------------------------

# import older data 2014 to 2016

ONIALevel <- read_csv("Raw Data/NI/A Level/NIALevel2014-2016SS.csv",na=c('*','','na'))

# rename "DfE Number" to "DfENum"

colnames(ONIALevel)[colnames(ONIALevel)=="DfE Number"] <- "DfENum"

# rename "School name" to "School Name" in both old and new dataframes

colnames(ONIALevel)[colnames(ONIALevel)=="School name"] <- "School Name"
colnames(NNIALevel)[colnames(NNIALevel)=="School.Name"] <- "School Name"

# rename Average wider points score into "Raw Score  YEAR" so that we can use the same name across all 7 datasets

colnames(ONIALevel)[colnames(ONIALevel)=="Average point score 2015"] <-"RawScore2015"

colnames(ONIALevel)[colnames(ONIALevel)=="Average Point score 2016"] <-"RawScore2016"

# change class of DfENum,Euclid School Code, Euclid National centre number to character

ONIALevel$DfENum <- as.character(ONIALevel$DfENum)
class(ONIALevel$DfENum) = "character"
class(ONIALevel$DfENum)

class(ONIALevel$`School Code`) = "character"
class(ONIALevel$`School Code`)

class(ONIALevel$DENINO) = "character"
class(ONIALevel$DENINO)

class(ONIALevel$RawScore2015)
class(ONIALevel$RawScore2015) = "numeric"

class(ONIALevel$RawScore2016)
class(ONIALevel$RawScore2016) = "numeric"

class(ONIALevel$`Z Score 2015`)
class(ONIALevel$`Z Score 2015`) = "numeric"

class(ONIALevel$`Z Score 2016`)
class(ONIALevel$`Z Score 2016`) = "numeric"



# Indentify new schools ----------------------------------------------------
# Now we indentify schools which are present in the new data but not in the old data, so that they can be looked up online and a decision made on whether they should be included in the dataset (because they are new schools) or not included (as they are Pupil Referal Units or Special Schools)


#This code makes a list of the DfE Numbers which are in the new data but NOT in the old data 

missingfromnew <- setdiff(NNIALevel$DfENum,ONIALevel$DfENum)


#This code makes a new dataframe subsetting on the new data, including only those schools not in the new data
  
  NNIALevelnotinold <- NNIALevel[NNIALevel$DfENum %in% missingfromnew, ]
  
  #Then this dataframe can be exported into a csv so that me or an intern can web search each one and establish if it should be included in the dataset
  
  write.csv(NNIALevelnotinold, file = "NNIALevelnotinold.csv")
  
  
  #After web searching its been established that the school 2230131   St Joseph's Boys' School has changed its DfE/DENI number to 5230056. Change number in exisiting dataset
  
  ONIALevel <- ONIALevel %>% 
    mutate(`DfENum`= case_when(
      `School Code` == 15775 ~ '5230056',  
      TRUE ~ `DfENum`
    ))

  # After web searching its been established that the schools from the new data need to be added to the new data 3210133, 3230308 and 2420320   (2420320 is a merger of two other schools, the former individual schools and their data should be merged and removed in a later stage). Create a data frame which only contains the three new schools DfE numbers  
  
  newsch <- NNIALevel %>% 
    select(DfENum,`School Name`) %>%
    filter(DfENum %in% c(3210133,3230308,2420320))

  
    
  # bind the two new schools into OWALevel, to prepare for copying the new 2017 data into the new dataset
  
newsch$DfENum <- as.character(newsch$DfENum)

ONIALevel <- bind_rows(ONIALevel,newsch)


#Add national centre number of new schools

  
ONIALevel <- ONIALevel %>% 
    mutate(`National Centre Number`= case_when(
      DfENum == 3210133 ~ '71325',  
      DfENum == 3230308 ~ '71459',
      DfENum == 2420320 ~ '71903',
      TRUE ~ `National Centre Number`
    ))


#Add school code of new schools

ONIALevel <- ONIALevel %>% 
  mutate(`School Code`= case_when(
    DfENum == 3210133 ~ '26493',  
    DfENum == 3230308 ~ '26615',
    DfENum == 2420320 ~ '19738',
    TRUE ~ `School Code`
  ))

# Now import any notes added from the  to the CSV file

library(readr)
NNIALevelnotinoldnotes<- read_csv("NNIALevelnotinold.csv")

# now update existing dataset with new notes

test <- merge(ONIALevel,
              NNIALevelnotinoldnotes[,c("DfENum",
                                       "Notes")],
              by= "DfENum", all.x = TRUE) 

# What happens with test at the moment is that an additional notes coloum is added. I would like it to overwrite the existing notes coloum , but keep existing information in cells where they has been no change 



# Merge Old and New Data --------------------------------------------------


  
 # merge new data into the older data, joining on DENINO  Add latest year of data to “A Level Data NI 2015-2017”

NIALevel2014to2017<-merge(ONIALevel,
                         NNIALevel[,c("DfENum",
                                     "Average.point.score.per.pupil")],
                         by= "DfENum", all.x = TRUE) 


View(NIALevel2014to2017)

# all.x = TRUE makes it a left join where all the existing fields remain and if they don't appear in the new data an NA is returned.

#Rename 2017 column

colnames(NIALevel2014to2017)[colnames(NIALevel2014to2017)=="Average.point.score.per.pupil"] <-"RawScore2017"


# Rearrange columns   16 to between 12 and 13

NIALevel2014to2017<-NIALevel2014to2017[c(1,2,3,4,5,6,7,8,9,10,11,12,16,13,14,15)]

# At this point make a subset of all the schools who are in the existing data but not in the new data, so they can be then be checked on the internet 


schnonew <- NIALevel2014to2017[is.na(NIALevel2014to2017$`RawScore2017`),]

write.csv(schnonew, file = "NIAlevelOldSchoolsNoNewData.csv")

View(schnonew)

# Remove schools with 3 years of no data ----------------------------------

# Now remove schools with 3 years of no data. The below code actually retains only those schools have have data in each of the three years, not what we want!

NIALevel2014to2017New <- NIALevel2014to2017 %>% 
  filter( !is.na(`RawScore2015`)&
            !is.na(`RawScore2016`) &
            !is.na(`RawScore2017`))

#However this code below creats a DF which only contains the schools which don't have data in each of the three years.

NIALevel2014to2017NoData <- NIALevel2014to2017 %>% 
  filter(is.na(`RawScore2015`)&
           is.na(`RawScore2016`) &
           is.na(`RawScore2017`))





#This can then be used remove the schools from the main dataset

removethreeyearsnodata <- setdiff(NIALevel2014to2017$DfENum,NIALevel2014to2017NoData$DfENum)

NIALevel2014to2017 <- NIALevel2014to2017[NIALevel2014to2017$DfENum  %in% removethreeyearsnodata, ]


#Now we a DF with 160 schools, no schools with three years of no data remain. 


# Z Score -----------------------------------------------------------------







