rm(list=ls())

library(grid)
library(plyr)

source('./dimension_functions.r')
source('./graphics_functions.r')

################
# Data munging #
################

flight_data <- read.csv('../data/average_gate_runway_delay_by_dep_airport.csv')

popular_airports <- c('KJFK', 'KSEA', 'KLAX', 'KDCA', 'KBOS', 'KSFO', 'KABE', 'KABI')

selected_airports <- 
  flight_data[flight_data$departure_airport_icao_code %in% popular_airports,]

selected_airports <- subset(selected_airports, select = -c(total_delay))

selected_airports[, 2:5] <- selected_airports[, 2:5] / 60.0

colnames(selected_airports)[1] <- "Departure\n Airport"
colnames(selected_airports)[2] <- "Departure Gate Delay,\n minutes"
colnames(selected_airports)[3] <- "Departure Runway Delay,\n minutes"
colnames(selected_airports)[4] <- "Arrival Gate Delay,\n minutes"
colnames(selected_airports)[5] <- "Arrival Runway Delay,\n minutes"




##############
# Dimensions #
##############
# pdf size
width <- 8
height <- 8

# The column names for the slope graph
# The last column is the same as the first
# by design
cols <- c(colnames(selected_airports), 
          colnames(selected_airports)[1])

# The widths of the columns
colwidths <- unit(rep(1.5, length(cols)),
                  as.vector(rep("strwidth", length(cols))),
                  data=as.list(cols))



rows <- apply(as.matrix(selected_airports[,2:5],rownames.force=FALSE), 
              MARGIN=1, 
              FUN=single_row_height)
total_height <- sum(rows)
h_factor <- height / total_height
rowheights <- unit(c(2, h_factor * rows),
                  as.vector(c("strheight", rep("inches", length(rows)))),
                  data=as.list(cols[1], rep(NULL, length(rows))))


############
# Graphics #
############

filename <- "prim.pdf"

#pdf(filename, width=width, height=height)
plot.new()

# Graphics go here
overlay <- grid.layout(nrow=nrow(selected_airports) + 1,
                       ncol=length(cols),
                       widths=colwidths,
                       heights=rowheights,
                       respect=FALSE)

pushViewport(viewport(layout=overlay))

# List of the airports for convenience
airports <- selected_airports[,1]

for(i in 1:length(airports)) {
  # Left hand side noun list
  write_noun_names(overlay, airports[i], i + 1, 1, 0.15)
  # Right hand side noun list
  write_noun_names(overlay, airports[i], i + 1, length(cols), 0.9)
}


for(i in 1:length(cols)) {
  write_column_names(overlay, cols[i], i)
}

for(i in 1:length(rows)) {
  print_points_in_column
}




#grid.show.layout(overlay)

#dev.off()