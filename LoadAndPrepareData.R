require(plyr)

#if (!file.exists("StormData.bz2"))
#  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
#                "StormData.bz2")

stormDataRaw <- read.csv(bzfile("StormData.bz2"))

# Load Data
stormData <- stormDataRaw[stormDataRaw$FATALITIES > 0 | stormDataRaw$INJURIES > 0 | 
                          stormDataRaw$PROPDMG > 0 | stormDataRaw$CROPDMG  > 0, 
                          c("BGN_DATE", "END_DATE", "COUNTY", "STATE", "EVTYPE", "FATALITIES", 
                            "INJURIES", "PROPDMG", "CROPDMG", "PROPDMGEXP", "CROPDMGEXP")]

# Copy the the Property and Crop damage columns to new columns and multiply according to the 
# Exponent/Expansion value
stormData$PropDamage <- stormData$PROPDMG
stormData$CropDamage <- stormData$CROPDMG

expMultipliers <- data.frame(expCode=c("H", "K", "M", "B"), multiplier=c(100, 1000, 1000000, 1000000000))
for (exp in 1:9) 
  expMultipliers <- rbind(expMultipliers, data.frame(expCode=as.character(exp), multiplier=10^exp))

for (i in 1:length(expMultipliers$expCode)) {
  filterRows <- which(toupper(stormData$PROPDMGEXP) == expMultipliers$expCode[i])
  stormData$PropDamage[filterRows] <- stormData$PropDamage[filterRows] * expMultipliers$multiplier[i]
  filterRows <- which(toupper(stormData$CROPDMGEXP) == expMultipliers$expCode[i])
  stormData$CropDamage[filterRows] <- stormData$CropDamage[filterRows] * expMultipliers$multiplier[i]
}

# Uppercase EventType, replace punctuation with spaces, remove numbers, and remove leading/trailing 
# spaces and double spaces
stormData$EventType <- gsub("^ *|(?<= ) | *$", perl=T, "",gsub("[[:digit:]]", "", gsub("[[:punct:]]", " ", toupper(as.character(stormData$EVTYPE)))))

# Confirm the percentage of data covered by the top 48 Event Types
print(max(head(cumsum(sort(table(stormData$EventType), decreasing=T)) / length(stormData$EVTYPE), n = 48)))

#damageByEventType <- ddply(stormData, 
#                           .(eventType=toupper(gsub("/", " ", gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", tolower(EVTYPE), perl=TRUE)))), 
#                           summarise, 
#                           meanFatalities=mean(FATALITIES), sumFatalities=sum(FATALITIES), 
#                           meanInjuries=mean(INJURIES), sumInjuries=sum(INJURIES), 
#                           meanPropertyDamage=mean(PROPDMG), sumPropertyDamage=sum(PROPDMG), 
#                           meanCropDamage=mean(CROPDMG), sumCropDamage=sum(CROPDMG))
