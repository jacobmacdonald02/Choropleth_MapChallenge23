
# Code that extracts any pubs from the list which has a distinct colour word in its name, and then associates
#  the respective hex code for this observation. 
# Using the mean_col() function in the 'gridpattern' library, we can then calculate the 'average' hex code for
#  an aggregation at the LAD level (based on the RGBA values of respective hex codes)

# Nov. 14, 2023

library(gridpattern)
library(sf)
sf_use_s2(FALSE)

# SET WORKING DIRECTORY
wd <- "/User/..." 

# Openly available pubs data available from a previous Kaggle challenge: 
#       https://www.kaggle.com/datasets/rtatman/every-pub-in-england
#   The list is quite dated from 2015 and things have very obviously changed since then
#   VOA data may be a good alternative with some cleaning and geocoding in the same way as for the retail centres project:
#       https://voaratinglists.blob.core.windows.net/html/rlidata.htm

PUBS <- read.csv(paste0(wd, "/open_pubs.csv"))

# UK Boundaries from the ONS:
#       https://geoportal.statistics.gov.uk/ 
UK_LAD <- st_read(paste0(wd, "/LAD_MAY_2022_UK_BFE_V3.shp"))
UK_LAD <- st_transform(UK_LAD, crs = 4326)
UK_LAD <- UK_LAD[,c("LAD22CD")]


PUBS$hex <- NA
PUBS$hex <- ifelse(grepl("\\bBlack\\b|\\bblack\\b", PUBS$name), "#000000", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bWhite\\b|\\bwhite\\b", PUBS$name), "#FFFFFF", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bGrey\\b|\\bgrey\\b", PUBS$name), "#808080", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bBrown\\b|\\bbrown\\b", PUBS$name), "#964B00", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bRed\\b|\\bred\\b", PUBS$name), "#FF0000", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bOrange\\b|\\borange\\b", PUBS$name), "#FFA500", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bYellow\\b|\\byellow\\b", PUBS$name), "#FFFF00", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bGreen\\b|\\bgreen\\b", PUBS$name), "#008000", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bBlue\\b|\\bblue\\b", PUBS$name), "#0000FF", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bIndigo\\b|\\bindigo\\b", PUBS$name), "#4B0082", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bViolet\\b|\\bviolet\\b", PUBS$name), "#EE82EE", PUBS$hex)
PUBS$hex <- ifelse(grepl("\\bPurple\\b|\\bpurple\\b", PUBS$name), "#800080", PUBS$hex)

PUBS$longitude <- as.numeric(as.character(PUBS$longitude))
PUBS$latitude <- as.numeric(as.character(PUBS$latitude))
PUBS <- PUBS[!is.na(PUBS$hex),]
PUBS <- PUBS[!is.na(PUBS$latitude),]


PUBS <- st_as_sf(PUBS, coords = c("longitude", "latitude"), crs = 4326)


PUBS <- st_intersection(PUBS, UK_LAD)
PUBS <- aggregate(PUBS$hex, list(PUBS$LAD22CD), FUN=function(x) { mean_col(x) })
names(PUBS) <- c("LAD22CD", "AvgCol")

UK_LAD <- merge(UK_LAD, PUBS, by="LAD22CD", all.x=T)

st_write(UK_LAD, paste0(wd, "/LAD_PUBCols.shp"))
