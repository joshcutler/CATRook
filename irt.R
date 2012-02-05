require('Rook')

library(Rook)
library(rjson)
library(catR)
library(ltm)

source("poly.R")

rook = Rhttpd$new()
rook$add(
  name ="next_question",
  app  = function(env) {
    req = Rook::Request$new(env)
    
    poly = F
    if ("ro" %in% names(req$params())) 
    {
      ro = as.numeric(unlist(strsplit(req$params()$ro, ",")))
      if (max(ro) > 2) 
      {
        poly = T  
      }
    }
    
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
    results = list()
 
    if (poly)
    {
      #Get the fitted model from the passed parameters
      source(textConnection(req$params()$fit))
      
      #Get the list of question indices to consider
      #TODO: Refactor this (possibly just use uaq as is and refactor next.item.grm)
      remaining_indices = c()
      for (i in 1:length(unasked_questions)) {
        if (unasked_questions[i]) {
          reamaining_indices = append(remaining_indices, i)
        }
      }
      next_item = next.item.grm(my.fit=fit, so.far=answers, resp.options=ro, remaining.items=remaining_indices)
      
      theta_hat = next_item$theta.est
      results$next_item = list()
      #Convert item response to proper "ID"
      next_id = questions[as.numeric(next_item["item"]),"ids"]
 
      results$next_item$item_id = next_id
    }
    else
    {
      if("th" %in% names(req$params()))
      {
        theta_hat = as.numeric(req$params()$th)
      }
      else
      {
        theta_hat = thetaEst(questions[answered_questions,], answers[answered_questions], method="EAP")
      }

      #Compute the next item and return it
      items = createItemBank(items=questions)
      next_item = nextItem(items, theta_hat, criterion="MEPV", out=as.numeric(rownames(questions[!unasked_questions,])))
    
      #Convert item response to proper "ID"
      next_id = questions[as.numeric(next_item["item"]),"ids"]
      next_item_index = next_item["item"]$item
      next_item["item"] = next_id
      
      #Compute some results to save a web service call
      question_index = as.numeric(rownames(questions[questions$ids == as.character(next_id),]))
      hyp_answered_questions = answered_questions
      hyp_answered_questions[question_index] = T
      hyp_answers = answers
      
      #Compute values if item is correct
      hyp_answers[question_index] = 1
      hyp_theta_hat_correct = thetaEst(questions[hyp_answered_questions,], hyp_answers[hyp_answered_questions], method="EAP")
    
      #Compute values if item is incorrect
      hyp_answers[question_index] = 0
      hyp_theta_hat_incorrect = thetaEst(questions[hyp_answered_questions,], hyp_answers[hyp_answered_questions], method="EAP")
    
      results$theta_hat = theta_hat

      results$next_item = list()
      results$next_item$item_id = next_item$item
      results$next_item$EPV = next_item$info
      results$next_item$criterion = next_item$criterion

      results$if_correct = list(theta_hat=hyp_theta_hat_correct)
      results$if_incorrect = list(theta_hat=hyp_theta_hat_incorrect)
    }
    
    res = Rook::Response$new()
    res$write(toJSON(results))
    res$finish()
  }
)

#Test Function
rook$add(
  name ="load_grm",
  app  = function(env) {
    req = Rook::Request$new(env)
    
    source(textConnection(req$params()$fit))
    
    res = Rook::Response$new()
    res$write(fit)
    res$finish()
  }
)

rook$add(
  name ="fit_grm",
  app  = function(env) {
    req = Rook::Request$new(env)
    
    for (col in names(req$params())) {
      values = as.double(unlist(strsplit(req$params()[[col]], ",")))
      
      if (!exists("dataf")) {
        dataf = data.frame(values)
      }
      else {
        dataf = cbind(dataf, values)
      }
      
      names(dataf)[[ncol(dataf)]] = col
    }
    
    cleaned = dataf[complete.cases(dataf),]
    print(cleaned)
    fit = grm(cleaned)
    dump(c('fit'), "test.R")
    lines = readLines("test.R")
     
    res = Rook::Response$new()
    res$write(lines)
    res$finish()
  }
)

rook$start(listen='0.0.0.0', port=as.numeric(Sys.getenv("PORT")))

while(T) {
  Sys.sleep(10000)
}