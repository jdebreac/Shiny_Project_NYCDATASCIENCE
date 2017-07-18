setwd("~/Desktop/NYCDataScience/Bootcamp/Project_Shiny")
library(dplyr)
library(tidyr)
library(ggplot2)

##############################################################################################
#                                       POPULATION FILE                                      #
##############################################################################################
#****************************************LOADING FILES***************************************#

Total_Population=read.csv("./Total_Population/Total_Population.csv",
                          skip=3,header=FALSE,sep="," ## Skip first 3 rows
                          ,na.strings=c("", "NA"))             ##Fill the black with NA
Total_Population <- Total_Population %>% 
  select(.,1:61)                           ##Select the columns with Data
Total_Population<-Total_Population[complete.cases(Total_Population),]
?complete.cases
Total_Population<-na.omit(Total_Population)

#************************** Make the first row as the header of file*************************#
names(Total_Population) <- as.matrix(Total_Population[1,])     
Total_Population <- Total_Population[-1,]                     
Total_Population <- lapply(Total_Population, 
                           function(x) type.convert(as.character(x)))

#********************************** Rearrange data using tidyr ******************************#
Total_Population <- data.frame(Total_Population)                #Make into DataFrame
Total_Pop_Red <- Total_Population %>% 
  gather(key="Year", value = "Population", 5:61) %>% 
  select(.,Country.Name, Year, Population)                      #Change the data shape using tidyr,
                                                                #gather populations by Year
Total_Pop_Red2 <- apply(Total_Pop_Red,2,                                  #Get rid of "X" in front of year
              function(x) {x<- gsub("X","",x)})                 #as the header names are as factor

Total_Pop_Red2 <- data.frame(Total_Pop_Red2)                                        #Make set into data frame
Total_Pop_Red2 <- arrange(Total_Pop_Red2,Year)                                      #rearrange by Year

as.numeric(as.character(Total_Pop_Red2$Year))                             #Convert Years into number
as.numeric(as.character(Total_Pop_Red2$Population))                       #Convert Population into number
as.character(Total_Pop_Red2$Country.Name)                                 #Convert country names into a type of 
                                                                          #character

write.csv(Total_Pop_Red2, file="Total_Pop_Red2.csv")            #Save the finalized data set into csv file


##############################################################################################
#                              POPULATION: Country INFO                                      #
##############################################################################################
#****************************************LOADING FILES***************************************#
Country_info<-read.csv("./Total_Population/Country_code.csv",
                       na.strings=c("", "NA"),header=TRUE)
class(Country_info)
## This file contains the country code, name, region, income group

Country_info2 <- Country_info %>% select(.,1,2,3,4,5)
Country_info2<-na.omit(Country_info2)

as.character(Country_info2$Region)
as.character(Country_info2$Country.Code)
as.character(Country_info2$IncomeGroup)
names(Country_info2)[5] <- "Country.Name"

write.csv(Country_info2, file="Country_info2.csv")

##############################################################################################
#                                   POPULATION DENSITY FILE                                  #
##############################################################################################
#****************************************LOADING FILES***************************************#

Population_Density=read.csv("./Population_Density/Population_Density.csv",
                          skip=4,header=FALSE,sep="," ## Skip first 3 rows
                          ,na.strings=c("", "NA"))             ##Fill the black with NA
dim(Population_Density)
Population_Density <- Population_Density %>% 
  select(.,1:61)                           ##Select the columns with Data
Population_Density <- Population_Density %>%  select(.,-5,)
Population_Density<-na.omit(Population_Density)
sum(is.na(Population_Density))
dim(Population_Density)

#************************** Make the first row as the header of file*************************#
names(Population_Density) <- as.matrix(Population_Density[1,])     
Population_Density <- Population_Density[-1,]                     
Population_Density <- lapply(Population_Density, 
                           function(x) type.convert(as.character(x)))

#********************************** Rearrange data using tidyr ******************************#
Population_Density <- data.frame(Population_Density)                   #Make into DataFrame
dim(Population_Density)
Pop_Den_Red <- Population_Density %>% 
  gather(key="Year", value = "Population.Density", 5:60) %>% 
  select(.,Country.Name, Year, Population.Density)                      #Change the data shape using tidyr,
#gather populations by Year
Pop_Den_Red2 <- apply(Pop_Den_Red,2,                                    #Get rid of "X" in front of year
              function(x) {x<- gsub("X","",x)})                         #as the header names are as factor

Pop_Den_Red2 <- data.frame(Pop_Den_Red2)                                #Make set into data frame
Pop_Den_Red2 <- arrange(Pop_Den_Red2,Year)                              #rearrange by Year

as.numeric(as.character(Pop_Den_Red2$Year))                             #Convert Years into number
as.numeric(as.character(Pop_Den_Red2$Population.Density))               #Convert Population into number
as.character(Pop_Den_Red2$Country.Name)                                 #Convert country names into a 
                                                                        #type of character

write.csv(Pop_Den_Red2, file="Pop_Den_Red2.csv")                        #Save the finalized data set 
                                                                        #into csv file
##############################################################################################
#                                      CO2 EMISSION FILE                                     #
##############################################################################################
#****************************************LOADING FILES***************************************#

CO2_Emission=read.csv("./CO2_Emission/CO2_Emission.csv",
                            skip=4,header=FALSE,sep=","          ## Skip first 3 rows
                            ,na.strings=c("", "NA"))             ##Fill the black with NA
dim(CO2_Emission)
CO2_Emission <- CO2_Emission %>% 
  select(.,1:58)                           ##Select the columns with Data
Population_Density <- Population_Density %>%  select(.,-5,)
CO2_Emission<-na.omit(CO2_Emission)
sum(is.na(CO2_Emission))
dim(CO2_Emission)
#************************** Make the first row as the header of file*************************#
names(CO2_Emission) <- as.matrix(CO2_Emission[1,])     
CO2_Emission <- CO2_Emission[-1,]                     
CO2_Emission <- lapply(CO2_Emission, 
                             function(x) type.convert(as.character(x)))

#********************************** Rearrange data using tidyr ******************************#
CO2_Emission <- data.frame(CO2_Emission)                   #Make into DataFrame
dim(PCO2_Emission)
CO2_Emi_Red <- CO2_Emission %>% 
  gather(key="Year", value = "CO2Emission", 5:58) %>% 
  select(.,Country.Name, Year, CO2Emission)                      #Change the data shape using tidyr,
#gather populations by Year
CO2_Emi_Red <- apply(CO2_Emi_Red,2,                                    #Get rid of "X" in front of year
                      function(x) {x<- gsub("X","",x)})                         #as the header names are as factor

CO2_Emi_Red2 <- data.frame(CO2_Emi_Red)                                #Make set into data frame
CO2_Emi_Red2 <- arrange(CO2_Emi_Red2,Year)                              #rearrange by Year

CO2_Emi_Red2 <- CO2_Emi_Red2 %>% select(.,-1)
as.numeric(as.character(CO2_Emi_Red2$Year))                             #Convert Years into number
as.numeric(as.character(CO2_Emi_Red2$CO2Emission))               #Convert Population into number
as.character(CO2_Emi_Red2$Country.Name)                                 #Convert country names into a 
#type of character

write.csv(CO2_Emi_Red2, file="CO2_Emi_Red2.csv")                        #Save the finalized data set 
                                                                        #into csv file

##############################################################################################
#                                       Agriculture Land Area FILE                           #
##############################################################################################
#****************************************LOADING FILES***************************************#

Agriculture_land=read.csv("./Agriculture_Land/Agriculture_land.csv",
                      skip=4,header=FALSE,sep=","          ## Skip first 3 rows
                      ,na.strings=c("", "NA"))             ##Fill the black with NA
dim(Agriculture_land)
Agriculture_land <- Agriculture_land %>% 
  select(.,1:59)                           ##Select the columns with Data
Agriculture_land <- Agriculture_land %>%  select(.,-5,)
Agriculture_land<-na.omit(Agriculture_land)
sum(is.na(Agriculture_land))
dim(Agriculture_land)
#************************** Make the first row as the header of file*************************#
names(Agriculture_land) <- as.matrix(Agriculture_land[1,])     
Agriculture_land <- Agriculture_land[-1,]                     
Agriculture_land <- lapply(Agriculture_land, 
                       function(x) type.convert(as.character(x)))

#********************************** Rearrange data using tidyr ******************************#
Agriculture_land <- data.frame(Agriculture_land)                   #Make into DataFrame
dim(Agriculture_land)
Agri_land_Red <- Agriculture_land %>% 
  gather(key="Year", value = "Agriculture.Land", 5:58) %>% 
  select(.,Country.Name, Year, Agriculture.Land)                      #Change the data shape using tidyr,
#gather populations by Year
Agri_land_Red <- apply(Agri_land_Red,2,                                    #Get rid of "X" in front of year
                     function(x) {x<- gsub("X","",x)})                         #as the header names are as factor

Agri_land_Red2 <- data.frame(Agri_land_Red)                                #Make set into data frame
Agri_land_Red2 <- arrange(Agri_land_Red2,Year)                              #rearrange by Year


as.numeric(as.character(Agri_land_Red2$Year))                             #Convert Years into number
as.numeric(as.character(Agri_land_Red2$Agriculture.Land))               #Convert Population into number
as.character(Agri_land_Red2$Country.Name)                               #Convert country names into a 
                                                                        #type of character

write.csv(Agri_land_Red2, file="Agri_land_Red2.csv")     #Save the finalized data set 
Agri_land_Red2=read.csv("./Agri_land_Red2.csv",header=TRUE)                 #Load the finalized data file     
names(Agri_land_Red2)
Agri_land_Red2 <- Agri_land_Red2 %>% select(.,-1)
names(Agri_land_Red2)
class(Agri_land_Red2$Year)

##############################################################################################
#                                       Total Land Area FILE                                 #
##############################################################################################
#****************************************LOADING FILES***************************************#

Land_Size=read.csv("./Land_Size/Land_Size.csv",
                          skip=4,header=FALSE,sep=","          ## Skip first 3 rows
                          ,na.strings=c("", "NA"))             ##Fill the black with NA
dim(Land_Size)
Land_Size <- Land_Size %>% 
  select(.,1:61)                           ##Select the columns with Data
Land_Size <- Land_Size %>%  select(.,-5,)
Land_Size<-na.omit(Land_Size)
sum(is.na(Land_Size))
dim(Land_Size)
#************************** Make the first row as the header of file*************************#
names(Land_Size) <- as.matrix(Land_Size[1,])     
Land_Size <- Land_Size[-1,]                     
Land_Size <- lapply(Land_Size, 
                           function(x) type.convert(as.character(x)))

#********************************** Rearrange data using tidyr ******************************#
Land_Size <- data.frame(Land_Size)                   #Make into DataFrame
dim(Land_Size)
Land_Size <- Land_Size %>% 
  gather(key="Year", value = "Total.Land", 5:58) %>% 
  select(.,Country.Name, Year, Total.Land)                      #Change the data shape using tidyr,
#gather populations by Year
Land_Size_Red <- apply(Land_Size,2,                                    #Get rid of "X" in front of year
                       function(x) {x<- gsub("X","",x)})                         #as the header names are as factor

Land_Size_Red2 <- data.frame(Land_Size_Red)                                #Make set into data frame
Land_Size_Red2 <- arrange(Land_Size_Red2,Year)                              #rearrange by Year


as.numeric(as.character(Land_Size_Red2$Year))                             #Convert Years into number
as.numeric(as.character(Land_Size_Red2$Total.Land))               #Convert Population into number
as.character(Land_Size_Red2$Country.Name)                               #Convert country names into a 
#type of character

write.csv(Land_Size_Red2, file="Land_Size_Red2.csv")                        #Save the finalized data set 

#into csv file
Land_Size_Red2=read.csv("./Land_Size_Red2.csv",header=TRUE)                 #Load the finalized data file     
Land_Size_Red2 <- Land_Size_Red2 %>% select(.,-1)
names(Land_Size_Red2)

class(Land_Size_Red2$Year)
Agri_Land_Ratio <- inner_join(Land_Size_Red2,Agri_land_Red2)

Agri_Ratio <- inner_join(Agri_Land_Ratio,PD_Coun_info2)

write.csv(Agri_Land_Ratio, file='Agriculture_Land_Ratio.csv')

class(Agri_Ratio)

##############################################################################################
#                                      Forest Land Area FILE                                 #
##############################################################################################
#****************************************LOADING FILES***************************************#

Forest_Land=read.csv("./Forest_Land/Forest_Land.csv",
                   skip=4,header=FALSE,sep=","          ## Skip first 3 rows
                   ,na.strings=c("", "NA"))             ##Fill the black with NA
dim(Forest_Land)
Forest_Land <- Forest_Land %>% 
  select(.,1:60)                           ##Select the columns with Data
Forest_Land <- Forest_Land %>%  select(.,-c(5:34))
Forest_Land<-na.omit(Forest_Land)
sum(is.na(Forest_Land))
dim(Forest_Land)
#************************** Make the first row as the header of file*************************#
names(Forest_Land) <- as.matrix(Forest_Land[1,])     
Forest_Land <- Forest_Land[-1,]                     
Forest_Land <- lapply(Forest_Land, 
                    function(x) type.convert(as.character(x)))

#********************************** Rearrange data using tidyr ******************************#
Forest_Land <- data.frame(Forest_Land)                   #Make into DataFrame
dim(Forest_Land)
Forest_Land <- Forest_Land %>% 
  gather(key="Year", value = "Forest.Area", 5:30) %>% 
  select(.,Country.Name, Year, Forest.Area)                      #Change the data shape using tidyr,
#gather populations by Year
Forest_Land_Red <- apply(Forest_Land,2,                                    #Get rid of "X" in front of year
                       function(x) {x<- gsub("X","",x)})                         #as the header names are as factor

Forest_Land_Red2 <- data.frame(Forest_Land_Red)                                #Make set into data frame
Forest_Land_Red2 <- arrange(Forest_Land_Red2,Year)                              #rearrange by Year

as.numeric(as.character(Forest_Land_Red2$Year))                             #Convert Years into number
as.numeric(as.character(Forest_Land_Red2$Forest.Area))               #Convert Population into number
as.character(Forest_Land_Red2$Country.Name)                               #Convert country names into a 
#type of character

write.csv(Forest_Land_Red2, file="Forest_Land_Red2.csv")                        #Save the finalized data set 

#into csv file
Forest_Land_Red2=read.csv("./Forest_Land_Red2.csv",header=TRUE)                 #Load the finalized data file     
Forest_Land_Red2 <- Forest_Land_Red2 %>% select(.,-1)
names(Forest_Land_Red2)

class(Forest_Land_Red2$Year)


#**************************** Merge Agriculutre, Total, and Forest Land *******************************#
########################################################################################################
Agri_Forest_Land_Ratio <- inner_join(Agri_Land_Ratio,Forest_Land_Red2)                                  # 
write.csv(Agri_Forest_Land_Ratio, file='Agri_Forest_Land_Ratio.csv')                                   #
########################################################################################################

#************************ MERGE POPULATION, CO2, LAND, COUNTRY INFO ***********************************#
########################################################################################################
Total_Pop_Red3=read.csv("./Total_Pop_Red2.csv",header=TRUE)                       #Load the finalized data file         
Total_Pop_Red3 <- Total_Pop_Red3 %>% select(.,-1)

Pop_Den_Red3=read.csv("./Pop_Den_Red2.csv",header=TRUE) 
Pop_Den_Red3 <- Pop_Den_Red3 %>% select(.,-1)

CO2_Emi_Red3=read.csv("./CO2_Emi_Red2.csv",header=TRUE)                 #Load the finalized data file     
CO2_Emi_Red3 <- CO2_Emi_Red3 %>% select(.,-1)

Agri_Froest_Land_Red3=read.csv("./Agri_Forest_Land_Ratio.csv",header=TRUE)
Agri_Froest_Land_Red3 <- Agri_Froest_Land_Red3 %>% select(.,-1)

Country_info3=read.csv("./Country_info2.csv",header=TRUE)  
Country_info3 <- Country_info3 %>% select(.,6,3,4)

View(Total_Pop_Red3)
View(Pop_Den_Red3)
View(CO2_Emi_Red3)
View(Agri_Froest_Land_Red3)
View(Country_info3)

joint1<-inner_join(Total_Pop_Red3,Agri_Forest_Land_Ratio)
joint2<-inner_join(joint1,Pop_Den_Red3)
joint3<-inner_join(joint2,CO2_Emi_Red3)
joint4<-inner_join(joint3,Country_info3)

joint5<-left_join(Total_Pop_Red3,Pop_Den_Red3)
joint6<-left_join(joint5,CO2_Emi_Red3)
joint7<-left_join(joint6,Agri_Froest_Land_Red3)
joint8<-left_join(joint7,Country_info3)
dim(joint4)
dim(joint8)

write.csv(joint4,file = 'Data_TP_PD_CO2_AFL_C_Inner.csv')
write.csv(joint8,file = 'Data_TP_PD_CO2_AFL_C_Left.csv')
