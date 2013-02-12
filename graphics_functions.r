write_title <- function(title, 
                        row, 
                        cols) {
  require(grid)  
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=cols,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=14),
                        name="first_slope_row",
                        clip="off")
  )
  
  grid.text(title,
            x=unit(0.5,"native"),
            y=unit(1,"native")
  )
  
  upViewport() 
}

# The names of the columns
write_column_names <- function(name,
                               row,
                               col) {
  
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,unit(1, "strwidth", name)),
                        gp=gpar(fontsize=12),
                        name="first_slope_row",
                        clip="off")
  )
  
  grid.text(name,
            x=unit(0.5,"native"),
            y=unit(1,"native")
  )
  
  if(col > 1 & col < length(cols)) {
    grid.lines(x=unit(c(0.15,0.85),"native"),
               y=unit(c(0.5,0.5),"native"),
               gp=gpar(lwd=1,alpha=0.25))
  }
  
  upViewport() 
}

# The nouns according to Tufte
write_noun_names <- function(name,
                             row,
                             col,
                             txt_pos,
                             max,
                             min,
                             truth=F) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=12),
                        name="slope_row",
                        clip="off")
  )
  
  if(truth) {
    grid.text(name,
              x=unit(0.5,"native"),
              y=unit(txt_pos,"native") - unit(0.75, "strheight", name)
    )    
  }
  else {
    grid.text(name,
              x=unit(0.5,"native"),
              y=unit(txt_pos,"native")
    )    
  }

  
  upViewport()
}

# Print the data points in each column
print_points_in_column <- function(label,
                                   height,
                                   row,
                                   col,
                                   max,
                                   min,
                                   shift=F) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=10),
                        name="arf",
                        clip="off")
  )
  th <- as.integer(label)

  if(shift) {
    grid.text(th,
              x=unit(0.5,"native"),
              y=(unit(height,"native") + unit(1.5, "strheight", toString(label)))) 
  } 
  else {
    grid.text(th,
              x=unit(0.5,"native"),
              y=unit(height,"native"))    
  }
  
  upViewport() 
}

# Print the data points in each column
print_lines_between_columns <- function(label,
                                        height,
                                        h_next,
                                        row,
                                        col,
                                        max,
                                        min,
                                        shift1=F,
                                        shift2=F) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=10),
                        name="slope lines",
                        clip="off")
  )
  th <- as.integer(label)
  th <- toString(th)
  
  if(shift1) {
    grid.lines(x=unit.c(unit(0.25,"native") + unit(1, "strwidth", '888'),
                        unit(0.75,"native") - unit(1, "strwidth", '888')),
               y=unit.c(unit(height,"native"),
                        unit(h_next,"native") + unit(1.5, "strheight", toString(label))))    
  }
  else if(shift2) {
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