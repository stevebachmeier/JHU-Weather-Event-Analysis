# ---- SETUP ----
# Import libraries
library(dplyr)

# Download raw data
if (!dir.exists("data/")) {
    dir.create("data/")
}

download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              destfile="data/StormData.bz2", mode="wb")

# Import raw data frame
raw <- read.table(file="data/StormData.bz2", header=TRUE, sep=",")

# ---- CLEAN DATA ----
# Extract relevant data
names(raw)
keep <- c("REFNUM", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
          "CROPDMG", "CROPDMGEXP")

df <- select(raw, keep)

# Check for NAs
print(paste0("Percentage of NAs in the data frame: ",
             round(sum(is.na(df))/(nrow(df)*ncol(df))*100, 1),"%"))

# Convert factor variables to character strings
df$EVTYPE <- as.character(df$EVTYPE)
df$PROPDMGEXP <- as.character(df$PROPDMGEXP)
df$CROPDMGEXP <- as.character(df$CROPDMGEXP)

# ---- ANALYSIS ----
# Health
df <- mutate(df, TOTAL_EFFECTED=FATALITIES+INJURIES)
df1 <- summarize(group_by(df, EVTYPE), TOTAL_FATALITIES=sum(FATALITIES)) %>%
    filter(TOTAL_FATALITIES>0) %>% arrange(desc(TOTAL_FATALITIES))
df2 <- summarize(group_by(df, EVTYPE), TOTAL_INJURIES=sum(INJURIES)) %>%
    filter(TOTAL_INJURIES>0) %>% arrange(desc(TOTAL_INJURIES))
df3 <- summarize(group_by(df, EVTYPE), TOTAL=sum(TOTAL_EFFECTED)) %>%
    filter(TOTAL>0) %>% arrange(desc(TOTAL))

par(mar=c(9,4,2,1))
par(mfrow=c(1,3))
barplot(height=df1$TOTAL_FATALITIES[1:5]/1000, names.arg=df1$EVTYPE[1:5], 
        ylab="Number (thousands)", las=2, main="Fatalities")
barplot(height=df2$TOTAL_INJURIES[1:5]/1000, names.arg=df2$EVTYPE[1:5], 
        ylab="Number (thousands)", las=2, main="Injuries")
barplot(height=df3$TOTAL[1:5]/1000, names.arg=df3$EVTYPE[1:5], 
        ylab="Number (thousands)", las=2, main="Total")

# Economy
# Convert the "" exponents to " "
df$PROPDMGEXP[df$PROPDMGEXP==""] <- " "
df$CROPDMGEXP[df$CROPDMGEXP==""] <- " "

# Map the damage exponents to real multipliers
EXPMAP <- c(0,0,0,1,10,10,10,10,10,10,10,10,10,100,100,1000,1000,
            1e6,1e6,1e9,1e9)
names(EXPMAP) <- c("-","?"," ","+","0","1","2","3","4","5","6","7","8","H","h",
                   "K","k","M","m","B","b")

# Create the multiplier columns
df$PROPDMGMULTIPLIER <- EXPMAP[df$PROPDMGEXP]
df$CROPDMGMULTIPLIER <- EXPMAP[df$CROPDMGEXP]

# Create the monetary damage columns
df$PROPDMGUSD <- df$PROPDMG * df$PROPDMGMULTIPLIER
df$CROPDMGUSD <- df$CROPDMG * df$CROPDMGMULTIPLIER
df$TOTALDMGUSD <- df$PROPDMGUSD + df$CROPDMGUSD

# Summarize
df4 <- summarize(group_by(df, EVTYPE), PROP_DAMAGE=sum(PROPDMGUSD)) %>%
    filter(PROP_DAMAGE>0) %>% arrange(desc(PROP_DAMAGE))
df5 <- summarize(group_by(df, EVTYPE), CROP_DAMAGE=sum(CROPDMGUSD)) %>%
    filter(CROP_DAMAGE>0) %>% arrange(desc(CROP_DAMAGE))
df6 <- summarize(group_by(df, EVTYPE), TOTAL_DAMAGE=sum(TOTALDMGUSD)) %>%
    filter(TOTAL_DAMAGE>0) %>% arrange(desc(TOTAL_DAMAGE))

par(mar=c(11,4,2,3))
par(mfrow=c(1,3))
barplot(height=df4$PROP_DAMAGE[1:5]/1e9, names.arg=df4$EVTYPE[1:5], 
        ylab="billions USD", las=2, main="Property damage")
barplot(height=df5$CROP_DAMAGE[1:5]/1e9, names.arg=df5$EVTYPE[1:5], 
        ylab="billions USD", las=2, main="Crop damage")
barplot(height=df6$TOTAL_DAMAGE[1:5]/1e9, names.arg=df6$EVTYPE[1:5], 
        ylab="billions USD", las=2, main="Total (property and crop) damage")
