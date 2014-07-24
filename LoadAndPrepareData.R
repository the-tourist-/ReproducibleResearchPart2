require(plyr)

if (!file.exists("StormData.bz2"))
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.bz2")

stormData <- read.csv(bzfile("StormData.bz2"))

damageByEventType <- ddply(stormData, 
                           .(eventType=toupper(gsub("/", " ", gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", tolower(EVTYPE), perl=TRUE)))), 
                           summarise, 
                           meanFatalities=mean(FATALITIES), sumFatalities=sum(FATALITIES), 
                           meanInjuries=mean(INJURIES), sumInjuries=sum(INJURIES), 
                           meanPropertyDamage=mean(PROPDMG), sumPropertyDamage=sum(PROPDMG), 
                           meanCropDamage=mean(CROPDMG), sumCropDamage=sum(CROPDMG))
