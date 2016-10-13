regexcols <- function(dataset1=NULL, pattern)
{
  cols <- grep(pattern, names(dataset1));
  selected <- dataset1[cols];
  remaining <- dataset1[setdiff(names(dataset1), names(selected))];

  return (list(selected, remaining))
}