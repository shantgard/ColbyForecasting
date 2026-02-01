#Start source setup
source("setup.R")

#load brickman data 
DB = brickman_database() |>
  print()
#read stats of brickman 
db = DB |>
  dplyr::filter(interval == "static")
static_vars = read_brickman(db)
static_vars
#read speciifc to what we want 
db = DB |>
  dplyr::filter(scenario == "RCP45", 
                year == 2055,
                interval == "mon")
x = read_brickman(db)
x
#buoys 
buoys = gom_buoys()
buoys

#filter to m01
buoy_M01 = buoys |>
  filter(id == "M01")
#plot 
plot(st_geometry(coast), col = "orange", lwd = 2, add = TRUE)
plot(st_geometry(buoys), pch = 16, col = "purple", add = TRUE)
