rm(list=ls())

library(grid)
library(plyr)

source('./dimension_functions.r')
source('./graphics_functions.r')

################
# Data munging #
################

flight_data <- read.csv('../data/average_gate_runway_delay_by_dep_airport.csv')

popular_airports <- c('KJFK', 'KSEA', 'KLAX', 'KDCA', 'KBOS', 'KSFO', 'KABE', 'KABI', 'KADW', 'KSFB')

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

# The number of rows we need
nrows = nrow(selected_airports)

# The widths of the columns
colwidths <- unit(rep(1.5, length(cols)),
                  as.vector(rep("strwidth", length(cols))),
                  data=as.list(cols))


# The heights of the rows
# We take the maximum value that occurs along the row
# and subtract the minimum value along the row to get 
# the necessary row height
row_min_max_diff <- apply(as.matrix(selected_airports[,2:5], rownames.force=FALSE), 
                          MARGIN=1, FUN=single_row_height)
total_height <- abs(sum(row_min_max_diff))
h_factor <- height / total_height
row_mmd <- h_factor * abs(row_min_max_diff)
rowheights <- unit(c(2, row_mmd),
                   as.vector(c("strheight", rep("inches", length(row_mmd)))),
                   data=as.list(cols[1], rep(NULL, length(row_mmd))))


############
# Graphics #
############

filename <- "prim.pdf"

#pdf(filename, width=width, height=height)
plot.new()

# Graphics go here
overlay <- grid.layout(nrow=nrows + 1,
                       ncol=length(cols),
                       widths=colwidths,
                       heights=rowheights,
                       respect=TRUE)

pushViewport(viewport(layout=overlay))

# List of the airports for convenience
airports <- selected_airports[,1]

for(i in 1:nrows) {
  # If the left hand side value is greater than the right
  # hand side value put the left name higher than the right
  # otherwise right higher than the left
  if(selected_airports[i,2] >= selected_airports[i,5]) {
    # Left hand side noun list
    write_noun_names(airports[i], i + 1, 1, 0.85)
    # Right hand side noun list
    write_noun_names(airports[i], i + 1, length(cols), 0.15)
  } 
  else {
    # Left hand side noun list
    write_noun_names(airports[i], i + 1, 1, 0.15)
    # Right hand side noun list
    write_noun_names(airports[i], i + 1, length(cols), 0.85)    
  }
}


for(i in 1:length(cols)) {
  write_column_names(cols[i], i)
}

for(i in 1:nrows) {
  row <- as.matrix(selected_airports[i,2:5])
  total <- abs(sum(row))
  h <- row / total
  pheight <- cumsum(h)
  for(j in 1:(length(h) - 1)) {
    print_points_in_column(pheight[j], pheight[j + 1], i + 1, j + 1)
  }
}




#grid.show.layout(overlay)

#dev.off()