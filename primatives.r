rm(list=ls())

library(grid)
library(plyr)

################
# Data munging #
################

flight_data <- read.csv('./data/average_gate_runway_delay_by_dep_airport.csv')

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

############
# Graphics #
############

filename <- "prim.pdf"

# Dimensions
width <- 16
height <- 8

# The column names for the slope graph
# The last column is the same as the first
cols <- c(colnames(selected_airports), 
          colnames(selected_airports)[1])

# The widths of the columns
# colwidths <- unit.c(unit(1.5, "strwidth", cols[1]))
# for(c in cols[2:length(cols)]) {
#   colwidths <- unit.c(colwidths, unit(1.5, "strwidth", c))
# }
colwidths <- unit(rep(1.5, length(cols)), as.vector(rep("strwidth", length(cols))), data=as.list(cols))

# The heights or the rows
single_row_height <- function(row) {
  max = max(row)
  min = min(row)
  return(max - min)
}

rows <- apply(as.matrix(selected_airports[,2:5],rownames.force=FALSE), MARGIN=1, FUN=single_row_height)
total_height <- sum(rows)
h_factor <- 8 / total_height
rowheights <- unit.c(unit(2, "strheight", cols[1]))
for(r in rows) {
  rowheights <- unit.c(rowheights, unit(h_factor * r, "inches"))
}

write_noun_names <- function(layout, name, row, col) {
  pushViewport(viewport(layout=layout))
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=12),
                        name="slope_row",
                        clip="on")
  )
  
  grid.text(name,
            x=unit(0.5,"npc"),
            y=unit(0.5,"npc")
  )
  
  upViewport(1)
}

write_column_names <- function(layout, name, col) {
  pushViewport(viewport(layout=layout))
  pushViewport(viewport(layout.pos.row=1,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=12),
                        name="first_slope_row",
                        clip="on")
  )
  
  grid.text(name,
            x=unit(0.5,"npc"),
            y=unit(0.5,"npc")
  )
  
  upViewport(1) 
}



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
  write_noun_names(overlay, airports[i], i + 1, 1)
  # Right hand side noun list
  write_noun_names(overlay, airports[i], i + 1, length(cols))
}


for(i in 1:length(cols)) {
  write_column_names(overlay, cols[i], i)
}




#grid.show.layout(overlay)

#dev.off()