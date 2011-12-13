require('Rook')

library(Rook)
library(rjson)
library(catR)

rook = Rhttpd$new()
rook$add(
  name ="next_question",
  app  = function(env) {
    req = Rook::Request$new(env)
    
    discrimination = as.numeric(unlist(strsplit(req$params()$dis, ",")))
    difficulty = as.numeric(unlist(strsplit(req$params()$dif, ",")))
    guessing = as.numeric(unlist(strsplit(req$params()$g, ",")))
    ids = unlist(strsplit(req$params()$ids, ","))
    inattention = c(1)
 
    questions = data.frame(a=discrimination, b=difficulty, c=guessing, d=inattention)
    items = createItemBank(items=questions)
 
    answers = as.numeric(unlist(strsplit(req$params()$a, ",")))
    answered_questions = !is.na(answers)
    
    theta_hat = eapEst(questions[answered_questions,], answers[answered_questions])
    eapSem(theta_hat, questions[answered_questions,], answers[answered_questions])
 
    responses = c(0,1)
    posterior_variance_estimates = {}
    
    for (i in seq(1, length(ids))) {
      if (!answered_questions[i]) {
        posterior_variance_estimates[[ids[i]]] = EPV(items, i, responses, theta_hat, questions[i,])
      }
    }
    
    res = Rook::Response$new()
    res$write(toJSON(posterior_variance_estimates))
    res$finish()
  }
)

rook$start(listen='0.0.0.0', port=as.numeric(Sys.getenv("PORT")))

while(T) {
  Sys.sleep(10000)
}