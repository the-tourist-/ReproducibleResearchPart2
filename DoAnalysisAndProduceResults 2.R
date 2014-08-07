par(mfrow=c(2,2), mar=c(3.1,7.1,3.1,1.1))

damageByEventType <- ddply(stormData[stormData$INJURIES > 0 | stormData$FATALITIES > 0,], 
                           .(EventType), 
                           summarise, 
                           sumInjuriesAndFatalities=sum(INJURIES+FATALITIES), 
                           sumInjuries=sum(INJURIES),
                           sumFatalities=sum(FATALITIES), 
                           noOfEvents=length(EventType))

filter <- tail(damageByEventType$EventType[order(damageByEventType$sumInjuriesAndFatalities)][damageByEventType$noOfEvents>=10], n=20)
with(damageByEventType, 
     sorting <<- EventType[EventType %in% filter]
     [order(damageByEventType[EventType %in% filter,]$sumInjuriesAndFatalities)]
)
damageByEventType <- damageByEventType[damageByEventType$EventType %in% filter,]
damageByEventType <- damageByEventType[match(sorting, damageByEventType$EventType),]

barplot(damageByEventType$sumInjuriesAndFatalities, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("green", "green3"), beside=TRUE, xlab=NA, ylab=NA,
        main="Total Injuries and Fatalities", 
        cex.names=0.6, las=1)

barplot(damageByEventType$noOfEvents, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("blue", "blue4"), beside=TRUE, xlab=NA, ylab=NA,
        main="No of Events with Injuries or Fatalities", 
        cex.names=0.6, las=1)

barplot(damageByEventType$sumInjuries, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("purple", "purple3"), beside=TRUE, xlab=NA, ylab=NA,
        main="Total Injuries", 
        cex.names=0.6, las=1)

barplot(damageByEventType$sumFatalities, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("red", "red3"), beside=TRUE, xlab=NA, ylab=NA,
        main="Total Fatalities", 
        cex.names=0.6, las=1)
