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
 
    questions = data.frame(a=discrimination, b=difficulty, c=guessing, d=inattention)
    items = createItemBank(items=questions)
 
    #Figure out which questions have been answered
    answers = as.numeric(unlist(strsplit(req$params()$a, ",")))
    answered_questions = !is.na(answers)
    
    #Either take \hat{\theta} as a param or compute it
    if("th" %in% names(req$params()))
    {
      theta_hat = as.numeric(req$params()$th)
    }
    else
    {
      theta_hat = eapEst(questions[answered_questions,], answers[answered_questions])
    }

    #Compute the next item and return it
    next_item = nextItem(items, theta_hat, criterion="MEPV")
    
    res = Rook::Response$new()
    res$write(toJSON(next_item))
    res$finish()
  }
)

rook$start(listen='0.0.0.0', port=as.numeric(Sys.getenv("PORT")))

while(T) {
  Sys.sleep(10000)
}