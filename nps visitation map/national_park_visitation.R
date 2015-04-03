library(ggmap)

# Bubble map of 2014 NPS visitation 
# Found here, export to csv and trim the top.
# https://irma.nps.gov/Stats/SSRSReports/National%20Reports/Recreation%20Visitation%20By%20State%20and%20By%20Park%20(1979%20-%20Last%20Calendar%20Year)

nps_data <- read.csv("~/Downloads//RecreationVisitation.csv")

# Fixing NPS designations for geocode
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NMP",replacement="National Military Park"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NPRES",replacement="National Preserve"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="PRES",replacement="Preserve"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NRES",replacement="National Reserve"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NRRA",replacement="National River & Recreation Area"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NRR",replacement="National Recreational River"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NMEM",replacement="National Memorial"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="MEM",replacement="Memorial"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NEM",replacement="National Expansion Memorial"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NHS",replacement="National Historic Site"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NHS",replacement="National Historic Site"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NBP",replacement="National Battlefield Park"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NHP",replacement="National Historical Park"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NRA",replacement="National Recreation Area"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NSR",replacement="National Scenic Riverway"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="EHP",replacement="Ecological & Historic Preserve"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="S&RR",replacement="Scenic & Recreational River"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="PKWY",replacement="Parkway"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="W&SR",replacement="Wild & Scenic River"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NR",replacement="National River"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NS",replacement="National Seashore"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NP",replacement="National Park"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NM",replacement="National Monument"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NL",replacement="National Lakeshore"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="NB",replacement="National Battlefield"))
nps_data <- as.data.frame(sapply(nps_data,gsub,pattern="HS",replacement="Historical Site"))

nps_data$geocode <- geocode(as.character(nps_data$Field1))

# Cleaned up ZERO_RESULTS issues.
nps_data$geocode[nps_data$Field1=="Abraham Lincoln Birthplace National Historical Park",]<- c(lon = -85.73718,lat = 37.53091)
nps_data$geocode[nps_data$Field1=="Ford's Theatre National Historic Site",]<- c(lon = -77.0264675,lat = 38.8966533)
nps_data$geocode[nps_data$Field1=="Pu'ukohola Heiau National Historic Site",]<- c(lon = -155.821808,lat = 20.025581)
nps_data$geocode[nps_data$Field1=="Frederick Law Olmsted National Historic Site",]<- c(lon = -71.132551,lat = 42.325443)
nps_data$geocode[nps_data$Field1=="Springfield Armory National Historic Site",]<- c(lon = -72.581665,lat = 42.107258)
nps_data$geocode[nps_data$Field1=="Home of Franklin D. Roosevelt National Historic Site",]<- c(lon = -73.932905,lat = 41.77043)
nps_data$geocode[nps_data$Field1=="First Ladies National Historic Site",]<- c(lon = -81.375722,lat = 40.796572)
nps_data$geocode[nps_data$Field1=="Allegheny Portage Railroad National Historic Site",]<- c(lon = -78.5497341,lat = 40.4583206)
nps_data$geocode[nps_data$Field1=="Fort Donelson National Battlefield",]<- c(lon = -87.8679668,lat = 36.4849535)
nps_data$geocode[nps_data$Field1=="Bluestone National Scenic Riverway",]<- c(lon = -80.984345,lat = 37.567468)

# Let fate decide your next vacation!
np = nps_data[sample(1:nrow(nps_data), 1,replace=FALSE),]


# Plot map.
p <- ggmap(get_map(location = "united states", source = "google",zoom = 3, crop=FALSE,maptype="roadmap"))
p <- p + geom_point(aes(x = geocode$lon, y = geocode$lat, size=0.1),data = nps_data,alpha = as.numeric(nps_data$Field2)/400, color="midnightblue")
p <- p + geom_text(data = np, aes(x = geocode$lon, y = geocode$lat, label = Field1, colour="orange"), size = 3, vjust = 0, hjust = 0)
p <- p + theme_nothing(legend = FALSE)
print(p)

