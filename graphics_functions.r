# The names of the columns
write_column_names <- function(name, 
                               col) {
  
  pushViewport(viewport(layout.pos.row=1,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=12),
                        name="first_slope_row",
                        clip="off")
  )
  
  grid.text(name,
            x=unit(0.5,"native"),
            y=unit(0.8,"native")
  )
  
  if(col > 1 & col < length(cols)) {
    grid.lines(x=unit(c(0.15,0.85),"npc"),
               y=unit(c(0.3,0.3),"npc"),
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
                             min) {
  require(grid)
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(min, max),
                        gp=gpar(fontsize=12),
                        name="slope_row",
                        clip="off")
  )
  
  grid.text(name,
            x=unit(0.5,"npc"),
            y=unit(txt_pos,"native")
  )
  
  upViewport()
}

# Print the data points in each column
print_points_in_column <- function(label,
                                   height,
                                   row,
                                   col,
                                   max,
                                   min) {
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

  grid.text(th,
            x=unit(0.5,"npc"),
            y=unit(height,"native"))    
  
  upViewport() 
}

# Print the data points in each column
print_lines_between_columns <- function(label,
                                        height,
                                        h_next,
                                        row,
                                        col,
                                        max,
                                        min) {
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
  th <- toString(th)
  
  grid.lines(x=unit.c(unit(0.25,"npc") + unit(1, "strwidth", '888'),
                      unit(0.75,"npc") - unit(1, "strwidth", '888')),
             y=unit.c(unit(height,"native"),
                      unit(h_next,"native")))
  
  
  upViewport() 
}