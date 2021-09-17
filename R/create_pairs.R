#' Create Pairs
#'
#' Uses 1:1 Sign Up list to pair SURJers for conversations.
#'
#' Currently matches based on (a) number of conversations requested and (b) preferance in talking to
#' a newer or older SURJ member.
#'
#'
#'
#' @param signup_list dataframe containing at least `record_id`, `num_convos`, `preferance`, `engagement`
#'
#' @return dataframe of pairs for conversations
#' @export
createPairs <- function(signup_list) {

  # record for each convo requested
  # https://github.com/mrdwab/splitstackshape
  convo.list <- expandRows(signup_list, count = "num_convos", drop = F)

  pairing.list <- convo.list %>%
    dplyr::group_by(record_id) %>%
    dplyr::mutate(rand_set = runif(1)) %>% # used to randomize within newbie/scoobie groups
    dplyr::arrange(preferance, engagement, rand_set)

  partner1.list <- pairing.list[1:(floor(nrow(pairing.list)/2)),
                                c("name", "preferance", "engagement")] %>%
    dplyr::rename("Partner #1" = "name", "Partner 1 Eng" = "engagement", "Partner 1 Pref" = "preferance")
  # randomize the order of participants, but keep participants in a chunk together


  # other half of the list, but inverted
  partner2.list <- pairing.list[nrow(pairing.list):((floor(nrow(pairing.list)/2))+1),
                                c("name", "preferance", "engagement")] %>%
    dplyr::rename("Partner #2" = "name", "Partner 2 Eng" = "engagement", "Partner 2 Pref" = "preferance") %>%
    # create a new randomized order
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rand_set = runif(1)) %>%
    dplyr::arrange(rand_set) %>%
    dplyr::select(-rand_set)

  # combine left and right sides of the pairs lists
  pairs <- bind_cols(partner1.list, partner2.list)

  # check pair requests are met
  pairs %<>%
    dplyr::mutate(partner_1_req = `Partner 1 Pref` == `Partner 2 Eng` | `Partner 1 Pref`== "either",
                  partner_2_req = `Partner 2 Pref` == `Partner 1 Eng` | `Partner 2 Pref`== "either")

  print(paste0("Partner 1 request misses: ", sum(!pairs$partner_1_req)))
  print(paste0("Partner 2 request misses: ", sum(!pairs$partner_2_req)))

  return(pairs)
}
