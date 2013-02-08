rm(list=ls())

library(grid)
library(plyr)

source('./dimension_functions.r')
source('./graphics_functions.r')

################
# Data munging #
################

flight_data <- read.csv('../data/average_gate_runway_delay_by_dep_airport.csv')

popular_airports <- c('KJFK', 'KSEA', 'KLAX', 'KDCA', 'KBOS', 
                      'KSFO', 'KABE', 'KABI', 'KADW', 'KSFB')

selected_airports <- 
  flight_data[flight_data$departure_airport_icao_code %in% popular_airports,]

selected_airports <- subset(selected_airports, select = -c(total_delay))

selected_airports[, 2:5] <- selected_airports[, 2:5] / 60.0

colnames(selected_airports)[1] <- "Departure\n Airport"
colnames(selected_airports)[2] <- "Departure Gate Delay,\n minutes"
colnames(selected_airports)[3] <- "Departure Runway Delay,\n minutes"
colnames(selected_airports)[4] <- "Arrival Gate Delay,\n minutes"
colnames(selected_airports)[5] <- "Arrival Runway Delay,\n minutes"

selected_airports <- selected_airports[,c(1,2,3,5,4)]

# selected_airports <- selected_airports[sample(nrow(selected_airports)),]

##############
# Dimensions #
##############
# pdf size
width <- 20
height <- 15

# The column names for the slope graph
# The last column is the same as the first
# by design
cols <- c(colnames(selected_airports), 
          colnames(selected_airports)[1])

# The number of rows we need
nrows = nrow(selected_airports)

# The widths of the columns
colwidths <- unit(rep(1, length(cols)),
                  as.vector(rep("strwidth", length(cols))),
                  data=as.list(cols))

# The heights of the rows
# We take the maximum value that occurs along the row
# and subtract the minimum value along the row to get 
# the necessary row height
rowheights <- row_height_function(selected_airports)


############
# Graphics #
############

filename <- "prim.pdf"
pdf(filename, width=width, height=height)

plot.new()

# Graphics go here
overlay <- grid.layout(nrow=nrows + 1,
                       ncol=length(cols),
                       widths=colwidths,
                       heights=rowheights,
                       respect=TRUE)

pushViewport(viewport(layout=overlay,
                      width = unit(1, "npc"), 
                      height = unit(0.75, "npc"),
                      xscale=c(0,1),
                      yscale=c(0,1)
                      ))

# List of the airports for convenience
airports <- selected_airports[,1]

####################
# Create the plots #
####################

for(i in 1:length(cols)) {
  write_column_names(cols[i], i)
}

for(i in 1:nrows) {
  row <- as.matrix(selected_airports[i,2:5])
  total <- abs(sum(row))
  h <- row / total
  
  ma <- max(h)
  mi <- min(h)
  
  write_noun_names(substr(airports[i],2,4), i + 1, 1, h[1], ma, mi)
  write_noun_names(substr(airports[i],2,4), i + 1, length(cols), h[length(h)], ma, mi)
  
  for(j in 1:(length(h) - 1)) {
    print_points_in_column(row[j], h[j], i + 1, j + 1, ma, mi)
  }
  print_points_in_column(row[length(h)], h[length(h)], i + 1, length(h) + 1, ma, mi)
  
  for(j in 1:(length(h) - 1)) {
    print_lines_between_columns(row[j], h[j], h[j + 1], i + 1, c(j + 1, j + 2), ma, mi)
  }
  
  
}

dev.off()