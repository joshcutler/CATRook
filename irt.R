require('Rook')

library(Rook)
library(rjson)
library(catR)

rook = Rhttpd$new()
rook$add(
  name ="next_question",
  app  = function(env) {
    req = Rook::Request$new(env)
    
    #Construct the item bank
    discrimination = as.numeric(unlist(strsplit(req$params()$dis, ",")))
    difficulty = as.numeric(unlist(strsplit(req$params()$dif, ",")))
    guessing = as.numeric(unlist(strsplit(req$params()$g, ",")))
    ids = unlist(strsplit(req$params()$ids, ","))
    inattention = c(1)
    questions = data.frame(a=discrimination, b=difficulty, c=guessing, d=inattention, ids=ids)

    #Figure out which questions have been answered
    answers = as.numeric(unlist(strsplit(req$params()$a, ",")))
    unasked_questions = as.logical(as.numeric(unlist(strsplit(req$params()$uaq, ","))))
    #If an answer was NA it was not administered or they skipped it and we can't use it for computing theta
    answered_questions = !is.na(answers)

    #Either take \hat{\theta} as a param or compute it
    if("th" %in% names(req$params()))
    {
      theta_hat = as.numeric(req$params()$th)
    }
    else
    {
      theta_hat = thetaEst(questions[answered_questions,], answers[answered_questions], method="EAP")
    }

    results = list()
    results$theta_hat = theta_hat
    
    #Compute the next item and return it
    items = createItemBank(items=questions)
    next_item = nextItem(items, theta_hat, criterion="MEPV", out=as.numeric(rownames(questions[!unasked_questions,])))
    
    #Convert item response to proper "ID"
    next_id = questions[as.numeric(next_item["item"]),"ids"]
    next_item_index = next_item["item"]$item
    next_item["item"] = next_id
    
    results$next_item = list()
    results$next_item$item_id = next_item$item
    results$next_item$EPV = next_item$info
    results$next_item$criterion = next_item$criterion

    #Compute some results to savea  web service call
    question_index = as.numeric(rownames(questions[questions$ids == as.character(next_id),]))
    hyp_answered_questions = answered_questions
    hyp_answered_questions[question_index] = T
    hyp_answers = answers
    
    #Compute values if item is correct
    hyp_answers[question_index] = 1
    hyp_theta_hat_correct = thetaEst(questions[hyp_answered_questions,], hyp_answers[hyp_answered_questions], method="EAP")
    results$if_correct = list(theta_hat=hyp_theta_hat_correct)
    
    #Compute values if item is incorrect
    hyp_answers[question_index] = 0
    hyp_theta_hat_incorrect = thetaEst(questions[hyp_answered_questions,], hyp_answers[hyp_answered_questions], method="EAP")
    results$if_incorrect = list(theta_hat=hyp_theta_hat_incorrect)
    
    res = Rook::Response$new()
    res$write(toJSON(results))
    res$finish()
  }
)

rook$start(listen='0.0.0.0', port=as.numeric(Sys.getenv("PORT")))

while(T) {
  Sys.sleep(10000)
}