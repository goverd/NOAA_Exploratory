# Initial quick and dry exploration of the NOAA database producing plots of 3 events
# for every year from 1996 to 2015 as well as an approximate map of states most affected
# by droughts as indicated by crop damages due to droughts
require(lubridate)
require(ggplot2)
require(RCurl)
require(Quandl)
require(reshape2)
require(dplyr)

# Get new data and archive old
getData <- function(remoteFiles, localFiles, wD, Year, dataUrl, fileDir) {
    archiveDir <- "Archive"
    
    if (!dir.exists(file.path(wD, fileDir))) {
        dir.create(file.path(wD, fileDir))
    }
    
    locF <- localFiles[grepl(paste("d", Year, sep=""), localFiles)]
    
    if (!(locF[1] %in% remoteFiles)) {
        destFile <- remoteFiles[grepl(paste("d", Year, sep=""), remoteFiles)]
        destDir <- file.path(wDir, fileDir, destFile)
        download.file(paste(dataUrl, destFile, sep=""), destDir)
        if (!is.na(locF[1])) {
            if (!dir.exists(file.path(wDir, fileDir, archiveDir))) {
                dir.create(file.path(wDir, fileDir, archiveDir))
            } 
            file.rename(file.path(wDir, fileDir, locF),
                        file.path(wDir, fileDir, archiveDir, locF))
        }
    }
}

# Setup directories
wDir <- "/Users/Valentin/Documents/Education/Data Incubator/Interview"
dataURL <- c("ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/")
detailDir <- "Details"
fatalDir <- "Fatalities"
setwd(wDir)

# Setup dates of interest
sDate <- "01/01/1996"
eDate <- "31/12/2015"
sYear <- year(dmy(sDate))
eYear <- year(dmy(eDate))

# Get remote fatalities and details file names from FTP server
filenames <- strsplit(getURL(dataURL, ftp.use.epsv = FALSE, dirlistonly = TRUE), split = "\n")
filenames <- unlist(filenames)
fatalFNames <- filenames[grepl("fatalities", filenames)]
detailFNames <- filenames[grepl("details", filenames)]

# Get local fatalities and details file names
localDetFNames <- list.files(file.path(wDir, detailDir))
localFatFNames <- list.files(file.path(wDir, fatalDir))

# Download the missing/new data and archive old
for (i in sYear:eYear) {
    getData(detailFNames, localDetFNames, wDir, i, dataURL, detailDir)
    getData(fatalFNames, localFatFNames, wDir, i, dataURL, fatalDir)
}

# Get Consumer Price Index for All Urban Consumers: All Items (BLS)
cpiData <- Quandl("FRED/CPIAUCNS", start_date=as.character(dmy(sDate)), 
                  end_date=as.character(dmy(eDate)))
cpiData <- cbind(cpiData, cpiData[,2]/cpiData[1,2])
names(cpiData) <- c("Date", "CPI", "CPI2015")
cpiData <- tbl_df(cpiData)

cpiData <- mutate(cpiData, Year = as.integer(year(Date))) %>% 
           mutate(Month = month(Date, label=TRUE, abbr = FALSE)) %>%
           select(Year, Month, CPI2015)

# Get updated local fatalities and details file names
localDetFNames <- list.files(file.path(wDir, detailDir))
localFatFNames <- list.files(file.path(wDir, fatalDir))
localDetFNames <- localDetFNames[grep(".csv.gz",localDetFNames)]
localFatFNames <- localFatFNames[grep(".csv.gz",localFatFNames)]

# Read in the data and convert to tbl_df
noaaDet <- data.frame()
noaaFat <- data.frame()
for (i in 1:length(localDetFNames)) {
    noaaDet <- rbind(noaaDet, read.csv(file.path(wDir, detailDir, localDetFNames[i])))
    noaaFat <- rbind(noaaFat, read.csv(file.path(wDir, fatalDir, localFatFNames[i])))
}
noaaDet<-tbl_df(noaaDet)
noaaFat<-tbl_df(noaaFat)

noaaClnDet <- mutate(noaaDet, propKdmg=as.numeric(gsub("^0","1",1e3*grepl("(K$)",DAMAGE_PROPERTY)))) %>%
    mutate(propMdmg=as.numeric(gsub("^0","1",1e6*grepl("(M$)",DAMAGE_PROPERTY)))) %>%
    mutate(propBdmg=as.numeric(gsub("^0","1",1e9*grepl("(B$)",DAMAGE_PROPERTY)))) %>%
    mutate(propTdmg=as.numeric(gsub("^0","1",1e12*grepl("(T$)",DAMAGE_PROPERTY)))) %>%
    mutate(numDmgProp = gsub("(K$)|(M$)|(B$)|(T$)","",DAMAGE_PROPERTY)) %>%
    mutate(numDmgProp = as.numeric(gsub("^$","0",numDmgProp))*propKdmg*propMdmg*propBdmg) %>%
    
    mutate(cropKdmg=as.numeric(gsub("^0","1",1e3*grepl("(K$)",DAMAGE_CROPS)))) %>%
    mutate(cropMdmg=as.numeric(gsub("^0","1",1e6*grepl("(M$)",DAMAGE_CROPS)))) %>%
    mutate(cropBdmg=as.numeric(gsub("^0","1",1e9*grepl("(B$)",DAMAGE_CROPS)))) %>%
    mutate(cropTdmg=as.numeric(gsub("^0","1",1e12*grepl("(T$)",DAMAGE_CROPS)))) %>%
    mutate(numDmgCrop = gsub("(K$)|(M$)|(B$)|(T$)","",DAMAGE_CROPS)) %>%
    mutate(numDmgCrop = as.numeric(gsub("^$","0",numDmgCrop))*cropKdmg*cropMdmg*cropBdmg) %>%
    mutate(econDmg = numDmgCrop+numDmgProp)

#Remove California flood data, which seems like an outlier
noaaClnDet <- filter(noaaClnDet, DAMAGE_PROPERTY != '115B')

noaaClnDet <- merge(noaaClnDet, cpiData, by.x = c('YEAR', 'MONTH_NAME'), by.y = c('Year', 'Month'))
noaaClnDet <- tbl_df(noaaClnDet) %>%
            mutate(dmgPropCPI = numDmgProp/CPI2015) %>%
            mutate(dmgCropCPI = numDmgCrop/CPI2015) %>%
            mutate(econdDmgCPI = econDmg/CPI2015)

# Analyse crop damages
topEvents <- group_by(noaaClnDet, YEAR, EVENT_TYPE) %>%
            tally(dmgCropCPI, sort = TRUE) %>% ungroup()

top3Events <- data.frame()
for (i in sYear:eYear) {
    temp <- filter(topEvents, YEAR == i) %>%
            group_by(YEAR, EVENT_TYPE = factor(c(as.character(EVENT_TYPE[1:3]), rep("Other", n() - 3)),
                                         levels = c(as.character(EVENT_TYPE[1:3]), "Other"))) %>%
            tally() %>% ungroup()
    top3Events <- rbind(top3Events, temp)
}

top3Events <- group_by(top3Events, YEAR) %>% mutate(totYearDmg = sum(nn))

p <- ggplot(top3Events, aes(x = YEAR, y = nn/1000000000, fill = EVENT_TYPE)) +
    geom_bar(stat = "identity", colour="black", size = 0.1) +
    scale_x_continuous(breaks = seq(sYear, eYear, 2)) +
    ylab('Split of each year\'s crop damages')

barPalette <- c("#fdbf6f", "#1f78b4", "#b2df8a", "#ffffff", "#fb9a99", "#33a02c", 
                "#a6cee3", "#ff7f00", "#cab2d6", "#6a3d9a", "#313695", "#b15928",
                "#e31a1c", "#74add1", "#5e4fa2")

p + scale_fill_manual(values=barPalette)

require(choroplethr)

map <- filter(noaaClnDet, EVENT_TYPE=="Drought") %>%
    group_by(STATE) %>% dplyr::summarise(droughtDmgState = sum(dmgCropCPI)) %>%
    arrange(desc(droughtDmgState))

region<-c(tolower(map$STATE))
value<-c(map$droughtDmgState)
mapDf<-data.frame(region,value)

mapPalette <- c("#ffffd4", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404")
choro = StateChoropleth$new(mapDf)
choro$title = "Cumulative non-adjusted crop damages from drought (1996-2014)"
choro$ggplot_scale = scale_fill_manual(name="Crop Damages US$", values=mapPalette, drop=FALSE)
choro$render()




