par(mfrow=c(3,2), mar=c(2.1,7.6,3.1,1.1))

damageByEventType <- ddply(stormData[stormData$CropDamage > 0 | stormData$PropDamage > 0,], 
                           .(EventType), 
                           summarise, 
                           meanCropAndPropertyDamage=mean(CropDamage+PropDamage),
                           medianCropAndPropertyDamage=median(CropDamage+PropDamage), 
                           sumCropAndPropertyDamage=sum(CropDamage+PropDamage), 
                           meanCropDamage=mean(CropDamage),
                           medianCropDamage=median(CropDamage[CropDamage>0]), 
                           sumCropDamage=sum(CropDamage), 
                           noOfEvents=length(EventType),
                           noOfEventsOverHundredMillion=sum(CropDamage+PropDamage>1e8),
                           noOfCropEvents=sum(CropDamage>0),
                           meanPropDamage=mean(PropDamage),
                           medianPropDamage=median(PropDamage[PropDamage>0]), 
                           sumPropDamage=sum(PropDamage), 
                           noOfPropEvents=sum(PropDamage>0)
                           )

damageByEventType$medianCropDamage[is.na(damageByEventType$medianCropDamage)] <- 0
damageByEventType$medianPropDamage[is.na(damageByEventType$medianPropDamage)] <- 0

filter <- tail(damageByEventType$EventType[order(damageByEventType$meanCropAndPropertyDamage*damageByEventType$noOfEvents)][damageByEventType$noOfEvents>=10], n=20)
with(damageByEventType, 
     sorting <<- EventType[EventType %in% filter]
                          [order(damageByEventType[EventType %in% filter,]$medianCropAndPropertyDamage*
                                   damageByEventType[EventType %in% filter,]$noOfEvents)]
)
damageByEventType <- damageByEventType[damageByEventType$EventType %in% filter,]
damageByEventType <- damageByEventType[match(sorting, damageByEventType$EventType),]
barplot(damageByEventType$medianCropAndPropertyDamage*damageByEventType$noOfEvents, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("green1", "green3"), beside=TRUE, xaxt="n", xlab=NA, ylab=NA,
        main="Estimated Property and Crop Damage\nMedian x No of Events (US$ Billion)", 
        cex.names=0.7, las=1)
axis(1, axTicks(1), parse(text=sprintf("%0.1f", axTicks(1)/1e9)))

barplot(damageByEventType$meanCropAndPropertyDamage*damageByEventType$noOfEvents, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("red", "red3"), beside=TRUE, xaxt="n", xlab=NA, ylab=NA,
        main="Estimated Property and Crop Damage\nTotal - Mean x No of Events (US$ Billion)", 
        cex.names=0.7, las=1)
axis(1, axTicks(1), parse(text=sprintf("%0.1f", axTicks(1)/1e9)))

barplot(damageByEventType$medianCropDamage*damageByEventType$noOfCropEvents, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("purple", "purple3"), beside=TRUE, xaxt="n", xlab=NA, ylab=NA,
        main="Estimated Crop Damage\nMedian x No of Events (US$ Billion)", 
        cex.names=0.7, las=1)
axis(1, axTicks(1), parse(text=sprintf("%0.1f", axTicks(1)/1e9)))

barplot(damageByEventType$medianPropDamage*damageByEventType$noOfPropEvents, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("brown", "brown3"), beside=TRUE, xaxt="n", xlab=NA, ylab=NA,
        main="Estimated Property Damage\nMedian x No of Events (US$ Billion)", 
        cex.names=0.7, las=1, xlim=c(0,1e9))
axis(1, axTicks(1), parse(text=sprintf("%0.1f", axTicks(1)/1e9)))

barplot(damageByEventType$noOfEvents, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("blue", "blue4"), beside=TRUE, xlab=NA, ylab=NA,
        main="No of Events", 
        cex.names=0.7, las=1)

barplot(damageByEventType$noOfEventsOverHundredMillion, 
        horiz=TRUE, names.arg=damageByEventType$EventType,
        col=c("orange", "orange3"), beside=TRUE, xlab=NA, ylab=NA,
        main="No of Events causing over $100 million in Damages", 
        cex.names=0.7, las=1)