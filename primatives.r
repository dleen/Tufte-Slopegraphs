rm(list=ls())

library(grid)
library(plyr)

source('./dimension_functions.r')
source('./graphics_functions.r')

################
# Data munging #
################

flight_data <- read.csv('./data/average_gate_runway_delay_by_dep_airport.csv')

popular_airports <- c('KSEA', 'KATL', 'KORD', 'KLAX', 'KDFW', 
                      'KDEN', 'KJFK', 'KSFO', 'KLAS', 'KPHX',
                      'KDCA')

# SEA - Seattle
# ATL - Atlanta
# ORD - Chicago
# LAX - Los Angeles
# DFW - Dallas Forth Worth
# DEN - Denver
# JFK - John F Kennedy
# SFO - San Francisco
# LAS - Las Vegas
# PHX - Phoenix
# DCA - Washington DC


selected_airports <- 
  flight_data[flight_data$departure_airport_icao_code %in% popular_airports,]

selected_airports <- subset(selected_airports, select = -c(total_delay))

selected_airports[, 2:5] <- selected_airports[, 2:5] / 60.0

colnames(selected_airports)[1] <- "Departure\n Airport"
colnames(selected_airports)[2] <- "Departure Gate\n delay (minutes)"
colnames(selected_airports)[3] <- "Departure Runway\n delay (minutes)"
colnames(selected_airports)[4] <- "Arrival Gate\n delay (minutes)"
colnames(selected_airports)[5] <- "Arrival Runway\n delay (minutes)"

selected_airports <- selected_airports[,c(1,2,3,5,4)]

selected_airports <- selected_airports[sample(nrow(selected_airports)),]


##############
# Dimensions #
##############
# pdf size
width <- 8
height <- 10

title <- 'The breakdown of flight delays by departure and arrival components.\n'

# The column names for the slope graph
# The last column is the same as the first
# and both are blank
cols <- c("",colnames(selected_airports)[2:5],"")

# The number of rows we need
nrows = nrow(selected_airports)

# The widths of the columns
colwidths <- unit(c(0.2,1,1.1,1,1.2,0.2),
                  as.vector(rep("strwidth", length(cols))),
                  data=as.list(cols))

# The heights of the rows
# We take the maximum value that occurs along the row
# and subtract the minimum value along the row to get 
# the necessary row height
rowheights <- row_height_function(selected_airports, title, cols[2])

# List of the airports for convenience
airports <- selected_airports[,1]

############
# Graphics #
############

filename <- "prim.pdf"
pdf(filename, width=width, height=height)

plot.new()

# Main graphic overlay
overlay <- grid.layout(nrow=nrows + 2,
                       ncol=length(cols),
                       widths=colwidths,
                       heights=rowheights,
                       respect=TRUE)

pushViewport(viewport(layout=overlay,
                      width = unit(1, "native"), 
                      height = unit(1, "native"),
                      xscale=c(0,1),
                      yscale=c(0,1)))

####################
# Create the plots #
####################

# Write the title
write_title(title, 1, 1:length(cols))

# Write the column names
for(i in 1:length(cols)) {
  write_column_names(cols[i], 2, i)
}

# 
for(i in 1:nrows) {
  # Each row
  row <- as.matrix(selected_airports[i,2:5])
  # Sum of row
  total <- abs(sum(row))
  # Normalize
  h <- row / total
  # Initialize h_next to current height
  # for safety
  h_next <- h
  # Max height
  ma <- max(h)
  # Where max height occurs
  max_ind <- which.max(h)
  # Similarly...
  mi <- min(h)
  min_ind <- which.min(h)
  
  # If we're not at the last row:
  if(i < nrows) {
    # Load the next row
    row_next <- as.matrix(selected_airports[i + 1,2:5])
    # Repeat previous quantities
    total <- abs(sum(row_next))
    h_next <- row_next / total
    max_ind_next <- which.max(h_next)
    min_ind_next <- which.min(h_next)  
  }
  
  # Keep track of whether the minimum value
  # of this row coincides with the maximum
  # value of the next row
  adjust <- min_ind == max_ind_next
  
  # Skip 2 rows: title, colnames
  offset <- 2
  
  # Write the airport names
  # In the left column
  write_noun_names(substr(airports[i],2,4), row = i + offset, 
                   1, h[1], ma, mi)
  # In the right column
  write_noun_names(substr(airports[i],2,4), row = i + offset, 
                   length(cols), h[length(h)], ma, mi, adjust)
  
  # For each point in the row:
  for(j in 1:(length(h))) {
      # Print the point and adjust if needed
      print_points_in_column(row[j], h[j], row = i + offset, 
                             j + 1, ma, mi, j == min_ind && adjust)
  }
  for(j in 1:(length(h) - 1)) {
    print_lines_between_columns(row[j], h[j], h[j + 1], row = i + offset, 
                                c(j + 1, j + 2), # Print across two columns
                                ma, mi, j == (min_ind - 1) && adjust, 
                                j == min_ind && adjust)
  }
}

dev.off()