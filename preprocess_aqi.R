library(tidyverse)
library(ropenaq)

(tableResults <- aq_measurements(city = "Chicago-Naperville-Joliet",date_from = as.character(Sys.Date()-7), date_to = as.character(Sys.Date())))
a <- as.data.frame(tableResults)
drops <- c("unit","country","city","dateUTC","cityURL")
a<-a[ , !(names(a) %in% drops)]
data_wide <- spread(a, parameter, value)

# vars.to.replace <- c("bc","co","no2","o3","pm10","pm25","so2")
# df2 <- data_wide[vars.to.replace]
# df2[is.na(df2)] <- "FALSE"
# data_wide[vars.to.replace] <- df2
# 
# vars.to.replace <- c("bc","co","no2","o3","pm10","pm25","so2")
# df2 <- data_wide[vars.to.replace]
# df2[df2>0 & df2!="FALSE"] <- "TRUE"
# # df2 <- df2 %>% mutate_if(is.numeric, ~1 * (. > 0))
# data_wide[vars.to.replace] <- df2

locations <- unique(data_wide$location)
df_total = data.frame(city=character(),latitude=character(),longitude=character(),bc=character(),co=character(),no2=character(),o3=character(),pm10=character(),pm25=character(),so2=character())
names(df_total) = c("location","latitude","longitude","bc","no2","so2","co","pm10","pm25","o3")

for(location_t in locations)
{
  print(location_t)
  pollutants = c("bc","no2","so2","co","pm10","pm25","o3")
  p = c("FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE")
  names(p) = c("bc","no2","so2","co","pm10","pm25","o3")
  df1 = subset(a,location==location_t)
  lat = df1$latitude[1]
  long= df1$longitude[1]
  for(pollutant in pollutants)
  {
    df = subset(a,location==location_t & parameter==pollutant)
      if(is.data.frame(df) && nrow(df)==0)
      {
        next
      }
    else
    {
      p[pollutant] = "TRUE"
    }
  }
  df_row = data.frame(location_t,lat,long,p[1],p[2],p[3],p[4],p[5],p[6],p[7])
  names(df_row) = c("location","latitude","longitude","bc","no2","so2","co","pm10","pm25","o3")
  df_total <- rbind(df_total,df_row)   
}
# df_total$location = stringr::str_to_title(df_total$location)
names(df_total) = c("location","latitude","longitude","bc","no2","so2","co","pm10","pm2.5","o3")
library(fst)
fileName = "/fst/openaq.fst"
write.fst(df_total, fileName)
