library(data.table)
library(plyr)
library(gtools)
library(rjson)

#setwd("/media/Kellyn/F20E17B40E177139/kpmontgo@ncsu.edu/Research/APHIS_Pathways/analysis")
setwd("Q:/Team Drives/APHIS  Private Data/Pathways")
#setwd("I:/Ongoing Team Projects/Exclusion Support/AQI Cooperative Agreement")
F280 <- fread("F280_CF_FY2014_to_FY2018.csv")

# Add day column
day = F280[,substr(REPORT_DT, 9, 10)]
F280$DAY <- day

# Add column for data cleaning (FALSE/TRUE), FALSE - do not use, TRUE - use
F280$CLEAN <- "TRUE"

# Add notes column to document reason for omitting records, or what has been changed
F280$NOTES <- ""

# Change number of shipments to 1, except for PPQ (land border POE at Mexican border). Add column
# for total (quantity*num_shipments)
state = F280[,substr(LOCATION, 1, 2)]
F280$STATE <- state
borderStates <- c("TX", "AZ", "NM")
F280[!(PATHWAY == "Land Border" & STATE %in% borderStates),]$NUM_SHIP <- 1
F280$TOTAL <- F280$QUANTITY*F280$NUM_SHIP

# Import Plantae Kingdom taxonomy from ITIS (https://www.itis.gov/hierarchy.html)
jsonFamily <- fromJSON(file="Summaries/familyDict.json")
jsonOrder <- fromJSON(file="Summaries/orderDict.json")
jsonClass <- fromJSON(file="Summaries/classDict.json")

familyDF <- ldply(jsonFamily, data.frame, stringsAsFactors = F)
names(familyDF) <- c("Family", "Genus")

# Omit duplicate genus synonym names
genusCt <- count(familyDF$Genus)
genusCt$x <- as.character(genusCt$x)
dupGenus <- as.data.table(genusCt)[freq > 1,]

familyDF_noDup <- familyDF[!familyDF$Genus %in% dupGenus$x,]

# Merge taxonomic family data to F280 records
F280_merge <- merge(F280, familyDF_noDup, by.x="COMMODITY", by.y = "Genus", all.x = TRUE)

# Export list of genuses without family match with at least 300 records and manually specify family
missingGenus <- F280_merge[is.na(Family)][CLEAN=="TRUE"]
countGenus <- count(missingGenus$COMMODITY)
addGenus <- countGenus[countGenus$freq > 300,]
names(addGenus) <- c("Genus", "freq")
#write.csv(addGenus, "addGenus.csv")

addGenus$Family <- c("Orchidaceae","Orchidaceae","Asparagaceae","Asparagaceae","Bruniaceae","Alstroemeriaceae","Caryophyllaceae",
                     "Caryophyllaceae","Liliaceae","Mixed", "Asteraceae", "Rosaceae", "Acanthaceae", "Bruniaceae","Plumbaginaceae", "Asteraceae","Asteraceae",
                     "Asparagaceae","Caryophyllaceae","Asphodelaceae", "Asparagaceae",  "Onagraceae", "Hydrangeaceae","Proteaceae", "Amaryllidaceae", "Gentianaceae","Brassicaceae",
                     "Orchidaceae", "Apiaceae", "Asteraceae","Arecaceae", "Cyperaceae", "Orchidaceae", "Asparagaceae","Colchicaceae",
                     "Proteaceae","", "Asteraceae", "Stemonaceae", "Apocynaceae", "Campanulaceae", "Asphodelaceae")

addGenus$Genus <- as.character(addGenus$Genus)
addGenus <- addGenus[,c(1,3)]

familyDF_noDup <- rbind(familyDF_noDup, addGenus)

F280_merge <- merge(F280, familyDF_noDup, by.x = "COMMODITY", by.y = "Genus", all.x = TRUE)
missingGenus <- F280_merge[is.na(Family)][CLEAN=="TRUE"]

# Add order and class 

orderDF <- ldply(jsonOrder, data.frame, stringsAsFactors = F)
names(orderDF) <- c("Order", "Family")

classDF <- ldply(jsonClass, data.frame, stringsAsFactors = F)
names(classDF) <- c("Class", "Order")

taxoDF <- merge(orderDF, classDF, by = "Order", all.x = T)

F280_merge <- merge(F280_merge, taxoDF, by = "Family", all.x = T)
F280_merge[Family == "Mixed",]$Order <- "Mixed"
F280_merge[Family == "Mixed",]$Class <- "Mixed"

F280 <- F280_merge
F280[is.na(Family)]$Family <- ""
F280[Family == ""][CLEAN=="TRUE"]$NOTES <- "Unmatched Genus"
F280[Family == ""]$CLEAN <- "FALSE"

orderCount <- count(F280$Order)
names(orderCount) <- c("Order", "Count")
orderCount[order(-orderCount$Count),]

# Add columns for grouped disposition codes
disp_lookup <- read.csv("Summaries/disp_group_lookup.csv")
F280 <- join(F280, disp_lookup, by = "DISP_CD")

# Which records have CFRP disposition codes but should not?
CFRP_DISP <- c("REAR", "IRAR", "DEAR", "FUAR", "OTAR", "RXAR")
CFRP_countries <- c("Colombia", "Ecuador", "Dominican Republic", "Costa Rica")
CFRP_POE <- c("TX Houston Air CBP", "GA Atlanta CBP", "NY JFK Air Cargo CBP", "NY JFK CBP", 
              "CA Los Angeles CBP", "FL Miami Air CBP", "FL Miami Air Cargo CBP", "PR San Juan Air CBP")
CFRP_commodoties <- c("Dianthus", "Liatris", "Lilium", "Rosa", "Bouquet, Rose", "Zantedeschia")
CFRP_omit <- F280[DISP_CD %in% CFRP_DISP][!(ORIGIN_NM %in% CFRP_countries)][!(LOCATION %in% CFRP_POE)][!(COMMODITY %in% CFRP_commodoties)]
F280$CLEAN[F280$F280_ID %in% CFRP_omit$F280_ID] <- "FALSE"
F280$NOTES[F280$F280_ID %in% CFRP_omit$F280_ID] <- "CFRP disp code but not in CFRP"

# Which records have preclearance disp code but should not?
Preclear_DISP <- c("PCIR", "PCNA")
Preclear_countries <- c("Jamaica", "Chile")
# All commodities from Chile are precleared. Need to filter out some commodities from Jamaica.See CF manual pg 50
Preclear_commodities <- c("Alpinia", "Anthurium", "Croton", "Cordyline", "Cyperus","Dracaena",  "Gerbera",
                          "Gladiolus", "Heliconia", "Pandanus", "Phaeomeria", "Rosa", "Rumohra", "Strelitzia reginae")
Preclear_families <- c("Orchidaceae")
Preclear_omit <-  F280[DISP_CD %in% Preclear_DISP][!(ORIGIN_NM %in% Preclear_countries)][!(COMMODITY %in% Preclear_commodities | Family == "Orchidaceae")]
F280$CLEAN[F280$F280_ID %in% Preclear_omit$F280_ID] <- "FALSE"
F280$NOTES[F280$F280_ID %in% Preclear_omit$F280_ID] <- "Not preclearance"

# Remove records without disposition code (n=5), pathway (35), location (1299), commodity (4), or origin (150).
# 1,423 records removed in total (some records had missing values in multiple columns)
F280[DISP_CD==""]$CLEAN <- "FALSE"
F280[PATHWAY==""]$CLEAN <- "FALSE"
F280[LOCATION==""]$CLEAN <- "FALSE"
F280[COMMODITY==""]$CLEAN <- "FALSE"
F280[ORIGIN_NM==""]$CLEAN <- "FALSE"
F280[DISP_CD==""]$NOTES <- "Missing DISP_CD"
F280[PATHWAY==""]$NOTES <- "Missing PATHWAY"
F280[LOCATION==""]$NOTES <- "Missing LOCATION"
F280[COMMODITY==""]$NOTES <- "Missing COMMODITY"
F280[ORIGIN_NM==""]$NOTES <- "Missing ORIGIN_NM"

# Remove disposition code with unclear pest presence (pest.found categories ? (756) and na (2))
F280[Pest.found=="?"]$CLEAN <- "FALSE"
F280[Pest.found=="n/a"]$CLEAN <- "FALSE"
F280[Pest.found=="?"]$NOTES <- "Unclear pest presence"
F280[Pest.found=="n/a"]$NOTES <- "Unclear pest presence"

# Change US origin to correct origin (based on guidance from APHIS). All remaining USA origin changed to clean=F
F280[ORIGIN_NM=="United States of America"][LOCATION=="MI Port Huron CBP"]$NOTES <- "Changed origin from US to Canada. Port Huron POE."
F280[ORIGIN_NM=="United States of America"][COMMODITY=="Aspidistra"]$NOTES <- "Changed origin from US to Netherlands. Aspidistra."
F280[ORIGIN_NM=="United States of America"][LOCATION=="MI Port Huron CBP"]$ORIGIN_NM <- "Canada"
F280[ORIGIN_NM=="United States of America"][COMMODITY=="Aspidistra"]$ORIGIN_NM <- "Netherlands"
F280[ORIGIN_NM=="United States of America"]$NOTES <- "USA origin"
F280[ORIGIN_NM=="United States of America"]$CLEAN <- "FALSE"

# Remove records with origin=destination, suspicious
F280[ORIGIN_NM == DEST_NM]$NOTES <- "Origin=destination"
F280[ORIGIN_NM == DEST_NM]$CLEAN <- "FALSE"


# Outlier detection for data cleaning
# Loop through each combo and find outliers in QUANTITY column

# countries <- unique(F280$ORIGIN_NM)
# 
# hist(F280[ORIGIN_NM=="Colombia" & COMMODITY == "Alstroemeria",]$QUANTITY, breaks = 45)
# 
# 
# library(outliers)
# data <- F280[ORIGIN_NM=="Colombia" & COMMODITY == "Alstroemeria",]$QUANTITY
# zScores <- scores(data, type = "z", prob=0.999)
# 
# count(zScores)
# max(data[zScores])
# 
# outliers <- function(shipments){
#   x <- shipments$QUANTITY
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#   H <- 1.5 * IQR(x, na.rm = T)
# }
# 
# outlier_ids <- ""
# for (country in countries){
#   shipments <- F280[ORIGIN_NM == country,]
#   flowers <- unique(shipments$COMMODITY)
#   for (flower in flowers){
#     flower_shipments <- shipments[COMMODITY == flower,]
#     x <- flower_shipments$QUANTITY
#     qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#     #caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#     H <- 1.5 * IQR(x, na.rm = T)
#     outlier_ids <- append(outlier_ids, flower_shipments[x < (qnt[1] - H)]$F280_ID)
#     outlier_ids <- append(outlier_ids, flower_shipments[x > (qnt[2] + H)]$F280_ID)
#   }
#   
# }
# 
# F280$NOTES[F280$F280_ID %in% outlier_ids] <- "Outlier"
