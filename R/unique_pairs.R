uniqueConversationPairs <- function(pairs_to_dedupe) {

  unique_pairs <- pairs_to_dedupe[!duplicated(
    t( # transpose
      apply(pairs_to_dedupe, 1, sort) # for every row in the tbl, sort the columns
    )), ]

  return(unique_pairs)
}
