This application sets up a webservice for using CAT techniques to determine question ordering on a survey.
It is configured to run on heroku.com and scale using their web workers.  

Current API:

/next_question
params:
* dis - An array of discrimination parameters (comma separated)
*  dif - An array of difficulty parameters (comma separated)
*  g   - An array of guessing parameters (comma separated)
*  ids - Question IDs, these are internal to the service consumer but when specified will cause the service to return a usable question ID (comma separated)
*  a   - An array of question answers, should be 0, 1, or NA.  Where 1 is a correct answer.  (comma separated)
*  uaq - An array of unasked questions, should be 0, 1 where 1 signifies that the question has not yet been asked. (comma separated)
*  th  - (optional) The current value of theta hat.  Optional but makes web service calls faster
  
Returns a JSON blob with the following attributes:
*  theta_hat - The value of theta_hat given the question responses so far
*  next_item:
  *  item_id - The question id to ask next (as passed in by the service consumer)
  *  EPV     - Expected posterior variance after administering this question.  Can be used for client side stopping criterion.
  