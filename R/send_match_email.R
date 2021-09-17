sendMatchEmails <- function(pairs, signup_list = signup.list) {

  # create unique id for each pair
  pairs %<>%
    dplyr::mutate(match_id = row_number())
  print("add match_ids")

  # send email for each match
  for (match in pairs$match_id) {

    partner1 = pairs$`Partner #1`[pairs$match_id==match]
    partner2 = pairs$`Partner #2`[pairs$match_id==match]

    email <- createMatchEmail(partner1, partner2, signup_list)
    print(paste("match email #", match))

    # send email
    email %>%
      smtp_send(
        to = c(signup_list$email[signup_list$name==partner1],
               signup_list$email[signup_list$name==partner2]),
        from = c("Lauren at SURJ" = Sys.getenv("EMAIL_USER")),
        subject = "Match for SURJ 1:1s",
        credentials = creds_envvar(
          user = Sys.getenv("EMAIL_USER"),
          pass_envvar = "SMTP_PASSWORD",
          provider = "gmail",
          host = "smtp.gmail.com",
          port = 465, use_ssl = FALSE
        )
      )

    print(paste0("Email sent to ", partner1, " and ", partner2))

    # update record in airtable
    partner1_record <- signup_list$record_id[signup_list$name==partner1]
    partner2_record <- signup_list$record_id[signup_list$name==partner2]

    update_records(records = data.frame(record_id = c(partner1_record, partner2_record),
                                        # sending as character to avoid type errors
                                        # within Airtable
                                        meetings_initiated = as.character(Sys.Date())),
                   ids = record_id)

    print(paste0("Airtable updated for ", partner1, " and ", partner2))

  }
}
