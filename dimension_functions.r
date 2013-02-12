# The heights or the rows
single_row_height <- function(row) {
  max = max(row)
  min = min(row)
  return(max - min)
}

row_height_function <- function(df, title, colname) {
  require(grid)
  row_min_max_diff <- apply(as.matrix(df[,2:5], rownames.force=FALSE), 
                            MARGIN=1, FUN=single_row_height)
  total_height <- sum(row_min_max_diff)
#   row_mmd <- abs(row_min_max_diff) / total_height
  row_mmd <- max(row_min_max_diff)
  rowheights <- unit(c(1, 2, rep(2 * row_mmd, length(row_min_max_diff))),
                     as.vector(c("strheight", "strheight", rep("native", length(row_min_max_diff)))),
                     data=as.list(c(title, colname, rep(NULL, length(row_min_max_diff)))))
return(rowheights)
}