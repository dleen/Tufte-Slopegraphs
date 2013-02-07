# The nouns according to Tufte
write_noun_names <- function(layout, 
                             name,
                             row,
                             col,
                             txt_pos) {
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
            y=unit(txt_pos,"npc")
  )
  
  upViewport()
}

# The names of the columns
write_column_names <- function(layout, 
                               name, 
                               col) {
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
            y=unit(0.75,"npc")
  )
  
  if(col > 1 & col < length(cols)) {
    grid.lines(x=unit(c(0.15,0.85),"npc"),
               y=unit(c(0.2,0.2),"npc"),
               gp=gpar(lwd=1,alpha=0.25))
  }
  
  upViewport() 
}

# Print the data points in each column
print_points_in_column <- function(layout,
                                   height,
                                   row,
                                   column) {
  pushViewport(viewport(layout.pos.row=1,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=12),
                        name="first_slope_row",
                        clip="on")
  )
}