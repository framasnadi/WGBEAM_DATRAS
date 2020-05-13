##############################################################################################
#                                                                                            #
#   From Trust to DATRAS                                                                     #
#   How to convert Adriatic Sea Survey (SoleMon) data into DATRAS format                     #
#                                                                                            #
#   February 2020                                                                            #
#                                                                                            #
#   Authors:                                                                                 #
#   Francesco Masnadi (CNR-IRBIM, Ancona)                                                    #
#                                                                                            #
#   Required files: TA, TC (Medits format) + Biological Data (Export from Trust)             # 
#                                                                                            #
#  At the end, put the three files together (following the order HH, HL and CA)              # 
#  by deleting the header line of each file and saving the final file as a                   #
#  comma-separated file (CSV).                                                               #
#                                                                                            #
##############################################################################################

library(readr)
library(readxl)
library(tidyr)
library(dplyr)

rm(list=ls(all=TRUE))
setwd("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/WGBEAM/solemon per WGBEAM/DATRAS/final file Datras")
# Selected Years 
yrs <- 2019  # 2018  # 2017  # 2016
srv <- "SOLEMON2019" # "SOLEMON2018_b" # "SOLEMON2017_b" # "SOLEMON2016_b"
# Select the specie by the AphiaID code from WORMS: https://www.marinespecies.org/aphia.php?p=webservice
gen <-"SOLE"
spc <-"VUL"  
cspc <- "SOLEVUL"
SpcAphiaID <- "127160"
###################################

#--------------------------------------------------------------
# Creation of HH - Haul Information
#--------------------------------------------------------------
dataTA <- read_excel("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/SOLEMON/dati/TA_TB_TC/TA.xlsx") %>% dplyr::filter(YEAR == yrs) %>% dplyr::filter(COUNTRY == "ITA17") 

dataTA <- dataTA %>% mutate(RecordType =  replace(TYPE_OF_FILE, TYPE_OF_FILE == "TA", "HH"))
dataTA$Quarter <- rep(4, length(dataTA$TYPE_OF_FILE))
dataTA <- dataTA %>% mutate(Country =  replace(COUNTRY, COUNTRY == "ITA17", "IT"))
dataTA$Ship <- rep("48DP", length(dataTA$TYPE_OF_FILE))
dataTA$Gear <- rep("RAPIA", length(dataTA$TYPE_OF_FILE))
dataTA$SweepLngt <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$GearExp <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$DoorType <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$StNo <-  dataTA$HAUL_NUMBER;dataTA$HaulNo<-  dataTA$HAUL_NUMBER
dataTA$Year <- dataTA$YEAR
dataTA$Month <- dataTA$MONTH
dataTA$Day <- dataTA$DAY
dataTA$TimeShot <- ifelse(nchar(as.character(dataTA$SHOOTING_TIME))<4, paste("0",sep = "",as.character(dataTA$SHOOTING_TIME))  ,as.character(dataTA$SHOOTING_TIME))
dataTA$Stratum <- rep(0, dim(dataTA)[1])
for (i in 1:(dim(dataTA)[1])) {
  if(dataTA$NUMBER_OF_THE_STRATUM[i] == "STR1_17" ) 
  {dataTA$Stratum[i] <- "STR1"} else
    if(dataTA$NUMBER_OF_THE_STRATUM[i] == "STR2_17") 
    {dataTA$Stratum[i] <- "STR2"} else
      if(dataTA$NUMBER_OF_THE_STRATUM[i] == "STR3_17") 
      {dataTA$Stratum[i] <- "STR3"} 
}
dataTA$HaulDur <- dataTA$HAUL_DURATION
dataTA$DayNight <-  rep("D", length(dataTA$TYPE_OF_FILE))
dataTA$ShootLat <- as.numeric(dataTA$SHOOTING_LATITUDE)/100
dataTA$ShootLong <- as.numeric(dataTA$SHOOTING_LONGITUDE)/100
dataTA$HaulLat <- as.numeric(dataTA$HAULING_LATITUDE)/100
dataTA$HaulLong <- as.numeric(dataTA$HAULING_LONGITUDE)/100
dataTA$StatRec <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$Depth <- (as.numeric(dataTA$SHOOTING_DEPTH) + as.numeric(dataTA$HAULING_DEPTH) )/2
dataTA$HaulVal <- rep("V", length(dataTA$TYPE_OF_FILE))
dataTA$HydroStNo <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$StdSpecRecCode <- rep(1, length(dataTA$TYPE_OF_FILE))
dataTA$BycSpecRecCode <- rep(0, length(dataTA$TYPE_OF_FILE))
dataTA$DataType <- rep("R", length(dataTA$TYPE_OF_FILE))
dataTA$Netopening <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$Rigging <- rep("D", length(dataTA$TYPE_OF_FILE))
dataTA$Tickler <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$Distance <- dataTA$DISTANCE
dataTA$Warplngt <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$Warpdia <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$WarpDen <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$DoorSurface <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$DoorWgt <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$DoorSpread <- dataTA$VERTICAL_OPENING
dataTA$WingSpread <-  dataTA$WING_OPENING
dataTA$Buoyancy <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$KiteDim <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$WgtGroundRope <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$TowDir <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$GroundSpeed <- rep(5.5, length(dataTA$TYPE_OF_FILE))
dataTA$SpeedWater <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$SurCurDir <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$SurCurSpeed <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$BotCurDir <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$BotCurSpeed <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$WindDir <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$WindSpeed <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$SwellDir <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$SwellHeight <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$SurTemp <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$BotTemp <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$SurSal <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$BotSal <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$ThermoCline <- rep(-9, length(dataTA$TYPE_OF_FILE))
dataTA$ThClineDepth <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$CodendMesh <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$SecchiDepth <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$Turbidity <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$TidePhase <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$TideSpeed <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$PelSampType <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$MinTrawlDepth <- rep(-9, length(dataTA$TYPE_OF_FILE))
#dataTA$MaxTrawlDepth <- rep(-9, length(dataTA$TYPE_OF_FILE))
## conversion of coordinates to decimal degrees
convcoord<-function(had)  
{
  latdg=(had$ShootLat)/1
  LATz=floor(had$ShootLat)
  LATzz=latdg-LATz
  lat_min=(LATzz*100)/60
  LAT=LATz+lat_min
  
  hlatdg=(had$HaulLat)/1
  hLATz=floor(had$HaulLat)
  hLATzz=hlatdg-hLATz
  hlat_min=(hLATzz*100)/60
  hLAT=hLATz+hlat_min
  
  londg=(had$ShootLong)/1
  LONz=floor(had$ShootLong)
  LONzz=londg-LONz
  lon_min=(LONzz*100)/60
  LON=LONz+lon_min
  
  hlondg=(had$HaulLong)/1
  hLONz=floor(had$HaulLong)
  hLONzz=hlondg-hLONz
  hlon_min=(hLONzz*100)/60
  hLON=hLONz+hlon_min
  
  had$ShootLat = LAT
  had$ShootLong = LON
  had$HaulLat = hLAT
  had$HaulLong = hLON
  return(had)
}
# run the function
dataTA <-convcoord(dataTA)
# save HH
HH<-dataTA[,45:103]; write_excel_csv(HH, "HH.csv")


#--------------------------------------------------------------
# Creation of HL - Length Frequency Dist. Information
#--------------------------------------------------------------
dataTC <- read_excel("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/SOLEMON/dati/TA_TB_TC/TC.xlsx") %>% dplyr::filter(YEAR == yrs) %>% dplyr::filter(COUNTRY == "ITA17") %>% dplyr::filter(GENUS == gen) %>% dplyr::filter(SPECIES == spc)  %>% dplyr::select(-ID,-MATURITY, -WEIGHT_OF_THE_FRACTION,-WEIGHT_OF_THE_SAMPLE_MEASURED) %>% dplyr::group_by(TYPE_OF_FILE,	COUNTRY,VESSEL,	YEAR,	MONTH,	DAY,GENUS	,SPECIES,	LENGTH_CLASSES_CODE, NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,`MATSUB##` ,HAUL_NUMBER,SEX,LENGTH_CLASS)%>% dplyr::summarise(N_INDIVIDUALS_AT_LENCLASS_AND_MATSTAGE=sum(as.numeric(N_INDIVIDUALS_AT_LENCLASS_AND_MATSTAGE)))

BIO <- read_excel("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/SOLEMON/dati/TA_TB_TC/BIOLOGICAL.xlsx") %>% dplyr::filter(f902_survey_name == srv) %>% dplyr::filter(f911_area_code == "ITA17")  %>% dplyr::filter(SpeciesCode == cspc) %>% dplyr::filter(f104_weight > 1)%>% dplyr::select(-f104_notes) 
BIO$weight <- as.numeric(BIO$f104_weight)* as.numeric(BIO$f104_num)
BIO <- BIO %>% dplyr::select(-Stage, -Gear) %>% dplyr::group_by(f001_station_name , f971_sex_code) %>% dplyr::summarise(weight=sum(weight)) %>% dplyr::rename("StNo" = "f001_station_name") %>% dplyr::rename("SEX" = "f971_sex_code")

dataTC <- dataTC %>% mutate(RecordType =  replace(TYPE_OF_FILE, TYPE_OF_FILE == "TC", "HL"))
dataTC$Quarter <- rep(4, length(dataTC$TYPE_OF_FILE))
dataTC <- dataTC %>% mutate(Country =  replace(COUNTRY, COUNTRY == "ITA17", "IT"))
dataTC$Ship <- rep("48DP", length(dataTC$TYPE_OF_FILE))
dataTC$Gear <- rep("RAPIA", length(dataTC$TYPE_OF_FILE))
dataTC$SweepLngt <- rep(-9, length(dataTC$TYPE_OF_FILE))
dataTC$GearExp <- rep(-9, length(dataTC$TYPE_OF_FILE))
dataTC$DoorType <- rep(-9, length(dataTC$TYPE_OF_FILE))
dataTC$StNo <-  dataTC$HAUL_NUMBER;dataTC$HaulNo<-  dataTC$HAUL_NUMBER
dataTC$Year <- dataTC$YEAR
dataTC$SpecCodeType <- rep("W", length(dataTC$TYPE_OF_FILE))
dataTC$SpecCode <-rep(SpcAphiaID, length(dataTC$TYPE_OF_FILE))
dataTC$SpecVal <- rep(1, length(dataTC$TYPE_OF_FILE))
dataTC$Sex <- rep(0, dim(dataTC)[1])
for (i in 1:(dim(dataTC)[1])) {
  if(dataTC$SEX[i] == "F" ) 
  {dataTC$Sex[i] <- "F"} else
    if(dataTC$SEX[i] == "M") 
    {dataTC$Sex[i] <- "M"} else
      if(dataTC$SEX[i] == "I") 
      {dataTC$Sex[i] <- "U"} else
        if(dataTC$SEX[i] == "N") 
        {dataTC$Sex[i] <- -9}
}
dataTC$TotalNo <- dataTC$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED
dataTC$CatIdentifier <-  rep(1, length(dataTC$TYPE_OF_FILE))
dataTC$NoMeas <-  dataTC$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED
dataTC$SubFactor <-  rep(1, length(dataTC$TYPE_OF_FILE))
dataTC<-inner_join(dataTC,BIO) %>% rename("SubWgt" = "weight")
dataTC$CatCatchWgt <- dataTC$SubWgt
dataTC$LngtCode <- dataTC$LENGTH_CLASSES_CODE
dataTC$LngtClass <- dataTC$LENGTH_CLASS
dataTC$HLNoAtLngt <- dataTC$N_INDIVIDUALS_AT_LENCLASS_AND_MATSTAGE
#dataTC$DevStage   <-  rep(-9, length(dataTC$TYPE_OF_FILE))
#dataTC$LenMeasType  <-  rep(1, length(dataTC$TYPE_OF_FILE))
# save HH
HL<-dataTC[,16:39]; write_excel_csv(HL, "HL.csv")


#--------------------------------------------------------------
# Creation of CA - Age base Information
#--------------------------------------------------------------
dataTC2 <- read_excel("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/SOLEMON/dati/TA_TB_TC/TC.xlsx") %>% dplyr::filter(YEAR == yrs) %>% dplyr::filter(COUNTRY == "ITA17") %>% dplyr::filter(GENUS == gen) %>% dplyr::filter(SPECIES == spc) %>% dplyr::select(-ID,-WEIGHT_OF_THE_FRACTION,-WEIGHT_OF_THE_SAMPLE_MEASURED) %>% dplyr::group_by(TYPE_OF_FILE,	COUNTRY,VESSEL,	YEAR,	MONTH,	DAY,GENUS	,SPECIES,	LENGTH_CLASSES_CODE, NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,`MATSUB##`,HAUL_NUMBER,SEX,LENGTH_CLASS, MATURITY)%>% dplyr::summarise(N_INDIVIDUALS_AT_LENCLASS_AND_MATSTAGE=sum(as.numeric(N_INDIVIDUALS_AT_LENCLASS_AND_MATSTAGE)))

dataTC2 <- dataTC2 %>% mutate(RecordType =  replace(TYPE_OF_FILE, TYPE_OF_FILE == "TC", "CA"))
dataTC2$Quarter <- rep(4, length(dataTC2$TYPE_OF_FILE))
dataTC2 <- dataTC2 %>% mutate(Country =  replace(COUNTRY, COUNTRY == "ITA17", "IT"))
dataTC2$Ship <- rep("48DP", length(dataTC2$TYPE_OF_FILE))
dataTC2$Gear <- rep("RAPIA", length(dataTC2$TYPE_OF_FILE))
dataTC2$SweepLngt <- rep(-9, length(dataTC2$TYPE_OF_FILE))
dataTC2$GearExp <- rep(-9, length(dataTC2$TYPE_OF_FILE))
dataTC2$DoorType <- rep(-9, length(dataTC2$TYPE_OF_FILE))
dataTC2$StNo <-  dataTC2$HAUL_NUMBER;dataTC2$HaulNo<-  dataTC2$HAUL_NUMBER
dataTC2$Year <- dataTC2$YEAR
dataTC2$SpecCodeType <- rep("W", length(dataTC2$TYPE_OF_FILE))
dataTC2$SpecCode <-rep(SpcAphiaID, length(dataTC2$TYPE_OF_FILE))
dataTC2$AreaType    <-	rep(25, length(dataTC2$TYPE_OF_FILE))
dataTC2$AreaCode    <-	rep(17, length(dataTC2$TYPE_OF_FILE))
dataTC2$LngtCode <- dataTC2$LENGTH_CLASSES_CODE
dataTC2$LngtClass <- dataTC2$LENGTH_CLASS
dataTC2$Sex <- rep(0, dim(dataTC2)[1])
for (i in 1:(dim(dataTC2)[1])) {
  if(dataTC2$SEX[i] == "F" ) 
  {dataTC2$Sex[i] <- "F"} else
    if(dataTC2$SEX[i] == "M") 
    {dataTC2$Sex[i] <- "M"} else
      if(dataTC2$SEX[i] == "I") 
      {dataTC2$Sex[i] <- "U"} else
        if(dataTC2$SEX[i] == "N") 
        {dataTC2$Sex[i] <- -9}
}
dataTC2 <- dataTC2 %>% mutate(Maturity =  replace(MATURITY, MATURITY == "ND", -9))
dataTC2$PlusGr <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
dataTC2$AgeRings  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
dataTC2$CANoAtLngt	<- dataTC2$N_INDIVIDUALS_AT_LENCLASS_AND_MATSTAGE
dataTC2$IndWgt  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$MaturityScale  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$FishID  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$GenSamp  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$StomSamp  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$AgeSource  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$AgePrepMet  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$OtGrading  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
#dataTC2$ParSamp  <-	rep(-9, length(dataTC2$TYPE_OF_FILE))
# save CA
CA<-dataTC2[,17:39]; write_excel_csv(CA, "CA.csv")










