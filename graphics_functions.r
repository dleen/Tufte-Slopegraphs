# The nouns according to Tufte
write_noun_names <- function(name,
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
            x=unit(0.5,"npc"),
            y=unit(0.8,"npc")
  )
  
  if(col > 1 & col < length(cols)) {
    grid.lines(x=unit(c(0.15,0.85),"npc"),
               y=unit(c(0.3,0.3),"npc"),
               gp=gpar(lwd=1,alpha=0.25))
  }
  
  upViewport() 
}

# Print the data points in each column
print_points_in_column <- function(height,
                                   h_next,
                                   row,
                                   col) {
  pushViewport(viewport(layout.pos.row=row,
                        layout.pos.col=col,
                        xscale=c(0,1),
                        yscale=c(0,1),
                        gp=gpar(fontsize=10),
                        name="arf",
                        clip="off")
  )
  if( height >= 0) {
    th <- toString(height)
    if(substr(th,1,1) == '0') th <- substr(th,1,4)
    else if(substr(th,2,2) == '.') th <- substr(th,1,1)
    else th <- substr(th,1,2)
#     grid.points(x=unit(0.1,"npc"),
#                 y=unit(height,"npc")
#     )
    grid.text(th,
              x=unit(0.1,"npc"),
              y=unit(height,"npc"))    
    grid.lines(x=unit.c(unit(1.1, "strwidth", th) + unit(0.1,"npc"),
                        unit(0.9,"npc")),
               y=unit(c(height,h_next),"npc"))
  }
  else {
    grid.points(x=unit(0.1,"npc"),
                y=unit(1 + height,"npc")
    )    
  }
  
  upViewport() 
}