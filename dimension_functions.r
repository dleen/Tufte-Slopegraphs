# The heights or the rows
single_row_height <- function(row) {
  max = max(row)
  min = min(row)
  return(max - min)
}

row_height_function <- function(df) {
  require(grid)
  row_min_max_diff <- apply(as.matrix(df[,2:5], rownames.force=FALSE), 
                            MARGIN=1, FUN=single_row_height)
  total_height <- abs(sum(row_min_max_diff))
  row_mmd <- abs(row_min_max_diff) / total_height
  rowheights <- unit(c(2, row_mmd),
                     as.vector(c("strheight", rep("npc", length(row_mmd)))),
                     data=as.list(cols[1], rep(NULL, length(row_mmd))))
  return(rowheights)
}