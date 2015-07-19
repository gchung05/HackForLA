# Hack for LA
# June 30, 2015 Event
# Author: Gary Chung
# ===================

# Required packages
require(dplyr)
# require(pacman)

# # Import data from CHHS
# # ---------------------
# p_load(RSocrata)
# # Dataset 1
# # California Hospital Inpatient Mortality Rates and Quality Ratings, 2012-2013
# mortq <- read.socrata("https://chhs.data.ca.gov/resource/rpkf-ugbp.csv", 
#                       app_token="ZMR5ynqHns9pJXIrVESvzW9kP")
# # Dataset 2
# # Ischemic Stroke 30-Day Mortality and 30-Day Readmission Rates and Ratings for 
# # California Hospitals, 2011 - 2012
# stroke <- read.socrata("https://chhs.data.ca.gov/resource/6yg2-gk2b.csv")
# 
# # Save for later access
# save(mortq, stroke, file="Data/sourceData.Rdata")

# Load previously saved data to save time
load("Data/sourceData.Rdata")


# Join Stroke data to Mortality data
# ----------------------------------

# First manipulate stroke dataset
hold <- stroke %>% filter(!is.na(OSHPDID)) %>% 
  mutate(Procedure.Condition=paste("Stroke", Measure),
         Num.Events=X..of.Deaths.Readmissions,
         Num.Cases=X..of.Cases,
         Latitude=as.numeric(matrix(unlist(strsplit(Location.1, "[(,)]")), 
                                    ncol=3, byrow=T)[, 2]),
         Longitude=as.numeric(matrix(unlist(strsplit(Location.1, "[(,)]")), 
                                     ncol=3, byrow=T)[, 3])) %>%
  select(County, Hospital, OSHPDID, Procedure.Condition, Risk.Adjusted.Rate,
         Num.Events, Num.Cases, Latitude, Longitude)

# Next manipulate mortality dataset
hold2 <- mortq %>% group_by(OSHPDID, County, Hospital, Latitude, Longitude, 
                            Procedure.Condition) %>%
  summarise(Risk.Adjusted.Rate=mean(Risk.Adjusted.Mortality.Rate, na.rm=T),
            Num.Events=sum(X..of.Deaths, na.rm=T),
            Num.Cases=sum(X..of.Cases, na.rm=T))

# Combine
aSet <- rbind.data.frame(hold, hold2) %>% filter(!is.na(Risk.Adjusted.Rate))

# Create an aggregated measure of Risk Adjusted Rate
# To do this, each Procedure-Condition needs to be centered and scaled
# Borrowing a Quality-of-Life scoring concept, I'll center to 50 with a
# standard deviation of 5
ctsc <- by(aSet, aSet$Procedure.Condition, 
           function(x) cbind(Procedure.Condition=x$Procedure.Condition[1],
                             Center=attr(scale(x$Risk.Adjusted.Rate), 
                                         "scaled:center"),
                             Scale=attr(scale(x$Risk.Adjusted.Rate), 
                                        "scaled:scale")))
ctsc <- do.call(rbind.data.frame, ctsc)
ctsc$Center <- as.numeric(as.character(ctsc$Center))
ctsc$Scale <- as.numeric(as.character(ctsc$Scale))
aSet <- merge(aSet, ctsc, all.x=T)
aSet$Rate.Norm <- with(aSet, ((Risk.Adjusted.Rate - Center) / Scale) * 10 + 50)

# Create a weighted average of the normalized Risk Adjusted Rate across all
# Procedure-Conditions. The weighting factor is the Number of Cases.
aSet.all <- aSet %>% group_by(OSHPDID, County, Hospital, Latitude, Longitude) %>%
  summarise(Rate.Norm=weighted.mean(Rate.Norm, Num.Cases),
            Num.Events=sum(Num.Events),
            Num.Cases=sum(Num.Cases))
aSet.all$Procedure.Condition <- "All"

# Invert the normalized rate to approximate a 0-100 scale
aSet.all$Rate.Norm <- 100 - aSet.all$Rate.Norm

# Set the red-green color palette
pal <- colorNumeric(palette = c("#FF0000", "#00C000"), domain = aSet.all$Rate.Norm)
# Set the pop-up message
content <- paste(sep="<br/>",
                 ~Hospital,
                 paste("Score:", ~Rate.Norm))

leaflet(aSet.all) %>% setView(-118.243685, 34.052234, zoom=11) %>%
  addProviderTiles("Stamen.Toner") %>%
  addCircles(weight = 1,
             radius = ~Num.Cases/12 + 100, 
             fillOpacity = 0.7,
             popup = ~paste("<b>", Hospital, "</b><br/>",
                            paste("Score:", round(Rate.Norm, 0))),
             color = ~pal(Rate.Norm)
  )

save(aSet.all, file="Data/analysisSet.RData")
write.xlsx(aSet.all, sheetName="Data", file="Data/analysisSet.xlsx")
write.xlsx(aSet, sheetName="Data", file="Data/allProcedures.xlsx")

aSet <- rbind.data.frame(aSet, aSet.all) %>% arrange(OSHPDID, Procedure.Condition)
rm(hold, hold2)

require(googleVis)
aSet$LatLong <- with(aSet, paste(Latitude, Longitude, sep=":"))
foo <- gvisGeoChart(aSet, "LatLong", "Rate.Norm", "Num.Cases",
                          options=list(region="US-CA", 
                                       displayMode="markers",
                                       resolution="metros",
                                       width=700, height=500))
plot(foo)
    
# EXPLORATORY
# Look at Stroke correlation between mortality and readmission ratings
# -------------------

require(reshape)

corr <- stroke %>% filter(!is.na(OSHPDID)) %>%
  select(OSHPDID, Measure, Risk.Adjusted.Rate) %>%
  melt(id=c("OSHPDID", "Measure")) %>% cast(OSHPDID~Measure)
plot(corr[, 2], corr[, 3])
rm(corr)
# Does not look to have any correlation.



hold2 <- mortq %>% 
  mutate(Risk.Adjusted.Rate=Risk.Adjusted.Mortality.Rate,
         Procedure.Condition=paste(Procedure.Condition, "Mortality"),
         Num.Events=X..of.Deaths,
         Num.Cases=X..of.Cases) %>%
  select(County, Hospital, OSHPDID, Procedure.Condition, Risk.Adjusted.Rate,
         Num.Events, Num.Cases, Latitude, Longitude)

temp <- mortq %>% group_by(Hospital.Ratings) %>% 
  summarise(Mort.Rate=mean(Risk.Adjusted.Mortality.Rate, na.rm=T))
Hospital.Ratings
Risk.Adjusted.Mortality.Rate

Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
               options=list(dataMode='regions'))
# Display chart
plot(Geo) 
# Create Google Gadget
cat(createGoogleGadget(Geo), file="geomap.xml")
