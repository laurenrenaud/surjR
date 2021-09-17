runAndCheckPairs <- function(signup_list = signup.list) {

  # check number of conversations
  print(paste0("sum convo count = ", sum(signup_list$num_convos)))

  # if odd number of conversations, drop one convo from someone who has large # requests
  if (sum(signup_list$num_convos) %% 2 != 0) {

    signup_list %<>%
      dplyr::arrange(desc(num_convos))

    signup_list$num_convos[1] <- signup_list$num_convos[1] - 1

  }

  # get initial pairs
  pairs <- createPairs(signup_list = signup_list)

  # if there are duplicates, re-run
  i <- 0
  while (sum(duplicated(pairs)) > 0 & i < 20) {

    pairs <- createPairs(signup_list = signup_list)
    print(paste0("New duplicates: ", sum(duplicated(pairs))))
    i + 1

    # check interests
    # pairs %>%
    #   dplyr::left_join(select(signup_list, name, interests), by = c("Partner #1" = "name")) %>%
    #   dplyr::left_join(select(signup_list, name, interests), by = c("Partner #2" = "name")) %>%
    #   dplyr::select(`Partner #1`, `Partner #2`, interests.x, interests.y) %>% View()
  }

  return(pairs)

}
