# The plot title
write_title <- function(title,row, cols) {
  require(grid)  
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=cols,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=14),
                        name="title",
                        clip="off"))
  grid.text(title, x=unit(0.5,"native"), y=unit(1,"native"))
  upViewport() 
}

# The names of the columns
write_column_names <- function(name, row, col) {
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,unit(1, "strwidth", name)),
                        gp=gpar(fontsize=12),
                        name="column names",
                        clip="on"))
  grid.text(name, x=unit(0.5,"native"), y=unit(0.75,"native"))
  
  # Draw lines under column names
  if(col > 1 & col < length(cols)) {
    grid.lines(x=unit(c(0.15,0.85),"native"),
               y=unit(c(0.3,0.3),"native"),
               gp=gpar(lwd=1,alpha=0.25))
  }
  upViewport() 
}

# Place the 'nouns' (Tufte's terminology)
write_noun_names <- function(name,
                             row, col,
                             txt_pos,
                             max, min,
                             adjust=F) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=12),
                        name="nouns",
                        clip="off"))
  
  # If adjust is true, adjust the label down slightly.
  # This occurs when the min value of one row aligns
  # with the max value of the next row
  if(adjust) {
    grid.text(name,
              x=unit(0.5,"native"),
              y=unit(txt_pos,"native") - 
                unit(0.75, "strheight", name))    
  }
  else {
    grid.text(name,
              x=unit(0.5,"native"),
              y=unit(txt_pos,"native"))    
  }

  upViewport()
}

# Print the data points in each column
print_points_in_column <- function(label,
                                   height,
                                   row, col,
                                   max, min,
                                   adjust=F) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=10),
                        name="data points",
                        clip="off"))
  # Only display the 'important' digits
  # ala Feinberg & Weiner 2011. 
  th <- as.integer(label)

  # If adjust is true, adjust the label up slightly.
  # This occurs when the min value of one row aligns
  # with the max value of the next row  
  if(adjust) {
    grid.text(th,
              x=unit(0.5,"native"),
              y=(unit(height,"native") + 
                   unit(1.5, "strheight", toString(label)))) 
  } 
  else {
    grid.text(th,
              x=unit(0.5,"native"),
              y=unit(height,"native"))    
  }
  upViewport() 
}

# Print the lines between each column
print_lines_between_columns <- function(label,
                                        height, h_next,
                                        row, col,
                                        max, min,
                                        adjust_left=F,
                                        adjust_right=F) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=10),
                        name="slope lines",
                        clip="off"))
  
  # For the lines if adjust is true we need to either
  # adjust the end of the line on the left or the 
  # start of the line on the right.
  if(adjust_left) {
    grid.lines(x=unit.c(unit(0.25,"native") + unit(1, "strwidth", '888'),
                        unit(0.75,"native") - unit(1, "strwidth", '888')),
               y=unit.c(unit(height,"native"),
                        unit(h_next,"native") + unit(1.5, "strheight", toString(label))))    
  }
  else if(adjust_right) {
    grid.lines(x=unit.c(unit(0.25,"native") + unit(1, "strwidth", '888'),
                        unit(0.75,"native") - unit(1, "strwidth", '888')),
               y=unit.c(unit(height,"native") + unit(1.5, "strheight", toString(label)),
                        unit(h_next,"native")))        
  }
  else {
    grid.lines(x=unit.c(unit(0.25,"native") + unit(1, "strwidth", '888'),
                        unit(0.75,"native") - unit(1, "strwidth", '888')),
               y=unit.c(unit(height,"native"),
                        unit(h_next,"native")))    
  }
  upViewport() 
}