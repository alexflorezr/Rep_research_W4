rm(list = ls())
# read data
setwd("~/Downloads/")
storm <- read.csv("repdata%2Fdata%2FStormData.csv", header = T)
X <- length(sort(table(grep("THUNDERSTORM", as.character(storm$EVTYPE), value = T)), decreasing=T))
storm$EVTYPE <- gsub("TSTM", "THUNDERSTORM", storm$EVTYPE)
storm$EVTYPE <- gsub("WINDS", "WIND", storm$EVTYPE)
ev_20 <- sort(table(storm$EVTYPE), decreasing = T)[1:20]
Ev_20_names <- as.vector(dimnames(ev_20)[[1]])
storm_20 <- storm[storm$EVTYPE %in% Ev_20_names,]
W <- paste(round(sum(ev_20)/dim(storm)[1]*100), "%", sep="")

# health plot
storm_20_health <- data.frame(matrix(nrow = length(Ev_20_names), ncol = 3))
colnames(storm_20_health) <- c("EVTYPE", "FAT_percentage", "INJ_percentage")
storm_20_health$EVTYPE <- Ev_20_names
tmp_D <- aggregate(storm_20$FATALITIES, list(storm_20$EVTYPE), sum)
storm_20_health$FAT_percentage <- tmp_D$x[match(storm_20_health$EVTYPE, tmp_D$Group.1)]*100/sum(storm_20$FATALITIES)
tmp_E <- aggregate(storm_20$INJURIES, list(storm_20$EVTYPE), sum)
storm_20_health$INJ_percentage <- tmp_E$x[match(storm_20_health$EVTYPE, tmp_E$Group.1)]*100/sum(storm_20$INJURIES)
par(mar=c(10,3,3,3))
bp <- barplot(rbind(storm_20_health$INJ_percentage,storm_20_health$FAT_percentage),
              col=c("#838B8B", "#EE7600"), border = F, beside=T)
text(bp[seq(1,length(bp), 2)]+0.5, par('usr')[3], labels = Ev_20_names, srt = 60, adj = c(1.1,1.1), xpd = TRUE, cex=.7)
par(srt=0)
legend("topright", c("Injuries", "Fatalities"), fill=c("#838B8B", "#EE7600"), border = F, bty = "n")


# the economic plot
storm_20$PROPDMGEXP <- toupper(storm_20$PROPDMGEXP)
storm_20$CROPDMGEXP <- toupper(storm_20$CROPDMGEXP)
storm_20_econo <- data.frame(matrix(nrow = length(Ev_20_names), ncol = 3))
colnames(storm_20_econo) <- c("EVTYPE", "Prop_percentage", "crop_percentage")
storm_20_econo$EVTYPE <- Ev_20_names
valid_exp <- c("B", "M", "K", "H")
number_exp <- c(1000000000, 1000000, 1000, 100)
storm_20_valid_prop <- storm_20[storm_20$PROPDMGEXP %in% valid_exp,]
total_prop <- storm_20_valid_prop$PROPDMG*number_exp[match(storm_20_valid_prop$PROPDMGEXP, valid_exp)]
storm_20_valid_prop$total_prop <- total_prop
tmp_P <- aggregate(storm_20_valid_prop$total_prop, list(storm_20_valid_prop$EVTYPE), sum)
storm_20_econo$Prop_percentage <- tmp_P$x[match(storm_20_econo$EVTYPE, tmp_D$Group.1)]*100/sum(storm_20_valid_prop$total_prop)
barplot(storm_20_econo$Prop_percentage)
storm_20_valid_crop <- storm_20[storm_20$CROPDMGEXP %in% valid_exp,]
total_crop <- storm_20_valid_crop$CROPDMG*number_exp[match(storm_20_valid_crop$CROPDMGEXP, valid_exp)]
storm_20_valid_crop$total_crop <- total_crop
tmp_Cr <- aggregate(storm_20_valid_crop$total_crop, list(storm_20_valid_crop$EVTYPE), sum)
storm_20_econo$Crop_percentage <- tmp_Cr$x[match(storm_20_econo$EVTYPE, tmp_Cr$Group.1)]*100/sum(storm_20_valid_crop$total_crop)
barplot(storm_20_econo$Crop_percentage)
par(mar=c(10,5,3,3))
bp <- barplot(rbind(storm_20_econo$Prop_percentage,storm_20_econo$Crop_percentage),
              col=c("#8B7355", "#556B2F"), border = F, beside=T)
mtext(side = 2, "percentage of total economic impact", line=3)
text(bp[seq(1,length(bp), 2)]+0.5, par('usr')[3], labels = Ev_20_names, srt = 60, adj = c(1.1,1.1), xpd = TRUE, cex=.7)
par(srt=0)
legend("topright", c("Properties", "Crops"), fill=c("#8B7355", "#556B2F"), border = F, bty = "n")


