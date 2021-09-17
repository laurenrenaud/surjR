createMatchEmail <- function(partner1, partner2, signup_list) {


  partner1_name <- partner1
  partner2_name <- partner2

  partner1_email <- signup_list$email[signup_list$name==partner1]
  partner2_email <- signup_list$email[signup_list$name==partner2]

  partner1_phone <- signup_list$phone[signup_list$name==partner1]
  partner2_phone <- signup_list$phone[signup_list$name==partner2]

  ## TO DO ##
  # Add interest into body of email?

  email <- compose_email(
    body = md(glue::glue(
      "Hi!

      You've been matched for our July of 1:1s!

      <b>{partner1_name}</b> and <b>{partner2_name}</b>

      You can reply directly in this email (feel free to drop me from the CC!) or call or text:<br>
      {partner1_name}: {partner1_phone}<br>
      {partner2_name}: {partner2_phone}

      Reach out in the new week or so and try to schedule your conversation some time in July.

      Here are some <a href='https://docs.google.com/document/d/1G4xco-Q0Qn5-uCvn1V84BveQcqDB-5aNqwof5HB4STg/edit?usp=sharing'>questions</a> to get you thinking.

      You can use <a href='https://airtable.com/shrjyI1W0sOjUI2Iz'>this form</a> to share back some highlights.

      Have fun!")),
    footer = md(glue::glue("Showing Up for Racial Justice @ Sacred Heart<br>www.surjatsacredheart.org"))
  )

}
