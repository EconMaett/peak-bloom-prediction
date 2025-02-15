# Berkely Global Earth Data Air Pollution PM2.5 concentrations
base_url <- "https://data.berkeleyearth.org/air-quality/local/"

location_names <- c("kyoto", "liestal", "newyorkcity", "vancouver", "washingtondc")

text_files <- c(
  "Japan/Kyoto/Kyoto.txt",
  "Switzerland/Basel-City/Basel.txt",
  "United_States/New_York/New_York_City.txt",
  "Canada/British_Columbia/Vancouver.txt",
  "United_States/Washington,_D.C./Washington,_D.C..txt"
)

paste0(base_url, text_files)

airpollution <- tibble()

for(i in seq_along(location_names)) {
  
  print(paste0("Loading airpollution data for ", location_names[i]))
  
  temp_file <- readr::read_table(
    col_names = c("year", "month", "day", "hour", "pm2.5", "pm10mask", "retr"),
    file = paste0(base_url, text_files[i]), 
    skip = 10, 
    col_types = cols("d", "d", "d", "d", "d", "d", "d")
  )
  
  temp_file$location <- location_names[i]
  
  airpollution <- bind_rows(airpollution, temp_file)
}


head(airpollution)

unique(airpollution$location)

airpollution |> 
  mutate(date = make_date(year, month, day)) |> 
  select(date, location, pm2.5) |> 
  group_by(location, date) |> 
  summarise(pm2.5 = mean(pm2.5)) |> 
  ggplot(mapping = aes(x = date, y = pm2.5)) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, colour = "red", span = 0.1) +
  facet_grid(rows = vars(location))
