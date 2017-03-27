library(sf)
library(dplyr)
library(ggplot2)

pluto = st_read("/data/nyc_parking/pluto_manhattan/", stringsAsFactors = FALSE, quiet = TRUE)

plot(st_geometry(st_centroid(pluto)), pch=16, cex=0.1, col=adjustcolor("black", alpha.f = 0.1))

pluto_df = pluto %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  cbind(data.frame(Address=pluto$Address), .) %>%
  mutate(Address = tolower(Address))

plot(pluto_df[,2:3], pch=16, cex=0.1, col=adjustcolor("black", alpha.f = 0.1), asp=1)



load("/data/nyc_parking/NYParkingViolations.Rdata")

nyc_df = nyc %>%
  select(Violation.Precinct, House.Number, Street.Name) %>%
  mutate(Address = paste(House.Number, Street.Name)) %>%
  select(Precinct = Violation.Precinct, Address) %>%
  mutate(Address = tolower(Address))



full = inner_join(nyc_df, pluto_df)

ggplot(full, aes(x=X, y=Y, color=as.factor(Precinct))) +
  geom_point(size=0.1)

