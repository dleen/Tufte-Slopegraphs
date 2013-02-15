# The heights of the data from a single row
single_row_height <- function(row) {
  max = max(row)
  min = min(row)
  return(max - min)
}

# A vector of units of the row heights
row_height_function <- function(df, title, colname) {
  require(grid)
  # Height of each row
  row_min_max_diff <- apply(as.matrix(df[,2:5], rownames.force=FALSE), 
                            MARGIN=1, FUN=single_row_height)
  # Max height over all rows
  max_row <- max(row_min_max_diff)
  # The height needed for each row, including title and column headings
  rowheights <- unit(c(1, 2, rep(2 * max_row, length(row_min_max_diff))),
                     as.vector(c("strheight", 
                                 "strheight", 
                                 rep("native", length(row_min_max_diff)))),
                     data=as.list(c(title, 
                                    colname, 
                                    rep(NULL, length(row_min_max_diff)))))
return(rowheights)
}