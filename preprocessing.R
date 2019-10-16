library(tidyverse)
library(lubridate)
library(stringr)
library(randomForest)
library(mice)
library(dplyr)


#load data
training_data=read_csv("E:\\Trinity\\machine learning\\tcdml1920-income-ind\\training.csv")

#remove 'Instance'
training_data=subset(training_data, select = -Instance)



####################
training_data$Profession=str_to_upper(training_data$Profession)
training_data$Gender=str_to_upper(training_data$Gender)
training_data$Country=str_to_upper(training_data$Country)
training_data$`University Degree`=str_to_upper(training_data$`University Degree`)
training_data$`Hair Color`=str_to_upper(training_data$`Hair Color`)


training_data$Gender=str_trim(training_data$Gender, side = "both")
training_data$Country=str_trim(training_data$Country, side = "both")
training_data$`University Degree`=str_trim(training_data$`University Degree`, side = "both")
training_data$`Hair Color`=str_trim(training_data$`Hair Color`, side = "both")
training_data$Profession=str_trim(training_data$Profession, side = "both")

training_data$Profession=str_sub(training_data$Profession,1,4)
training_data$Profession=str_trim(training_data$Profession, side = "both")


#turn salary positive
training_data=training_data%>%
  mutate(`Income in EUR`=ifelse(`Income in EUR`<0,abs(`Income in EUR`),`Income in EUR`))


######remove category (profession) specific outliers based on categories found out of outliers(income based) of eniter dataset. 

#remove SENI with income>1500000
training_data=training_data[-which(training_data$Profession=="SENI" & training_data$`Income in EUR`>1500000),]

#remove OIL with income>290000
training_data=training_data[-which(training_data$Profession=='OIL' & training_data$`Income in EUR`>290000),]

#remove PROG with income>600000
training_data=training_data[-which(training_data$Profession=='PROG' & training_data$`Income in EUR`>600000),]

#remove SECU with income>800000
training_data=training_data[-which(training_data$Profession=='SECU' & training_data$`Income in EUR`>800000),]

#remove TRAI with income>600000
training_data=training_data[-which(training_data$Profession=='TRIA'& training_data$`Income in EUR`>600000 ),]

#remove TRAU with income>400000
training_data=training_data[-which(training_data$Profession=='TRAU' & training_data$`Income in EUR`>400000),]

#remove RADI with income>800000
training_data=training_data[-which(training_data$Profession=='RADI'&training_data$`Income in EUR`>800000),]

#remove ACCO with income>1000000
training_data=training_data[-which(training_data$Profession=='ACCO'& training_data$`Income in EUR`>1000000),]

#remove REGI with income>650000
training_data=training_data[-which(training_data$Profession=='REGI' & training_data$`Income in EUR`>650000),]

#remove OPER with income>600000
training_data=training_data[-which(training_data$Profession=='OPER' & training_data$`Income in EUR`>600000),]

#remove ANAE with income>300000
training_data=training_data[-which(training_data$Profession=='ANAE' & training_data$`Income in EUR`>300000),]

#remove SYST with income>700000
training_data=training_data[-which(training_data$Profession=='SYST' & training_data$`Income in EUR`>700000 ),]

#remove ART with income>450000
training_data=training_data[-which(training_data$Profession=='ART'& training_data$`Income in EUR`>450000),]

#remove POST with income>1000000
training_data=training_data[-which(training_data$Profession=='POST' & training_data$`Income in EUR`>1000000),]

#remove UNIT with income>480000
training_data=training_data[-which(training_data$Profession=='UNIT' & training_data$`Income in EUR`>480000),]

#remove SERV with income>900000
training_data=training_data[-which(training_data$Profession=='SERV' & training_data$`Income in EUR`>900000),]






training_data=training_data%>%
  mutate(`Year of Record`=ifelse(`Year of Record`=="#N/A",NA,`Year of Record`))%>%
  mutate(`Year of Record`=as.numeric(`Year of Record`))

training_data=training_data%>%
  mutate(Age=ifelse(Age=="#N/A",NA,Age))%>%
  mutate(Age=as.numeric(Age))


training_data=training_data%>%
  mutate(Gender=ifelse(Gender=="#N/A","UNKNOWN",Gender))
training_data=training_data%>%
  mutate(Gender=as.factor(Gender))


training_data=training_data%>%
  mutate(`University Degree`=as.factor(`University Degree`))
#training_data=training_data%>%
  #mutate(`University Degree`=ifelse(`University Degree`=="#N/A",NA,`University Degree`))

training_data=training_data%>%
  mutate(`Wears Glasses`=ifelse(`Wears Glasses`==0,FALSE,TRUE))


training_data=training_data%>%
  mutate(`Hair Color`=ifelse(`Hair Color`=="0","xyz","abc"))%>%
  mutate(`Hair Color`=as.factor(`Hair Color`))

#replace year and age by their medians
training_data=training_data%>%
  mutate(`Year of Record`=ifelse(is.na(`Year of Record`),1999,`Year of Record`))%>%
  mutate(Age=ifelse(is.na(Age),37,Age))

  
summary(training_data)
#####rename columns
training_data=training_data%>%
  rename(year=`Year of Record`,size_city=`Size of City`,hair_color=`Hair Color`,height=`Body Height [cm]`,income=`Income in EUR`)


#################
##################
###################
#Target Mean Encoding

lookup_country = training_data %>%
  group_by(Country) %>%
  summarise(country_encoded = mean(income))


lookup_profession = training_data %>%
  group_by(Profession) %>%
  summarise(profession_encoded = mean(income))


lookup_degree=training_data %>%
  group_by(`University Degree`) %>%
  summarise(degree_encoded = mean(income))

write_csv(lookup_country,"E:\\Trinity\\machine learning\\tcdml1920-income-ind\\lookup_country.csv")
write_csv(lookup_degree,"E:\\Trinity\\machine learning\\tcdml1920-income-ind\\lookup_degree.csv")
write_csv(lookup_profession,"E:\\Trinity\\machine learning\\tcdml1920-income-ind\\lookup_profession.csv")



#####joining encoded values using left join
training_data=left_join(training_data, lookup_country)
training_data=left_join(training_data, lookup_degree)
training_data=left_join(training_data, lookup_profession)

#####Drop 3 columns encoded and wear glasses
training_data=subset(training_data,select = -c(`University Degree`,Profession,Country,`Wears Glasses`))

View(training_data)

write_csv(training_data,"E:\\Trinity\\machine learning\\tcdml1920-income-ind\\preprocessed_training_data.csv")

summary(training_data)































############
############
#################
##################
boxplot(training_data$`Income in EUR`~training_data$`Hair Color`, ylim=c(0,200000))


training_data %>%
  group_by(`Hair Color`) %>%
  summarise(degree_encoded = range(`Income in EUR`)[2])


length(training_data$`University Degree`[which(training_data$`University Degree`=="No")])

boxplot(training_data$Age)
summary(training_data$Age)

View(training_data[which(training_data$`Income in EUR`>2000000),])

View(training_data[which(training_data$Profession=='OIL' ),])
boxplot(training_data[which(training_data$Profession=='SERV'),11], ylim=c(1000000,3000000))
summary(training_data[which(training_data$Profession=='SECU'),11])
length(training_data$Profession[which(training_data$Profession=="PROG")])

summary(training_data$`Income in EUR`)
