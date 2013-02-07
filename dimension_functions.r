# The heights or the rows
single_row_height <- function(row) {
  max = max(row)
  min = min(row)
  return(max - min)
}