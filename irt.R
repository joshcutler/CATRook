require('Rook')

library(Rook)
library(rjson)
library(ltm)

source('CATSurv.R')

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
    print(req$params())
    #Construct the item bank
    discrimination = as.numeric(unlist(strsplit(req$params()$dis, ",")))
    difficulty = as.numeric(unlist(strsplit(req$params()$dif, ",")))
    guessing = as.numeric(unlist(strsplit(req$params()$g, ",")))
    ids = as.character(unlist(strsplit(req$params()$ids, ",")))
    inattention = c(1)
    questions = data.frame(a=discrimination, b=difficulty, c=guessing, d=inattention, ids=ids, stringsAsFactors=FALSE)

    #Figure out which questions have been answered
    answers = as.numeric(unlist(strsplit(req$params()$a, ",")))
    unasked_questions = as.logical(as.numeric(unlist(strsplit(req$params()$uaq, ","))))
    
    #If an answer was NA it was not administered or they skipped it and we can't use it for computing theta
    answered_questions = !is.na(answers)
    
    #Either take \hat{\theta} as a param or compute it    
    results = list()
 
    if (poly)
    {
      print("Polytonomous case not yet implemented")
    }
    else
    {
      print("Dichotomous case")
      ourPrior = req$params()$prior
      if (is.null(ourPrior)) {
        ourPrior = "normal"
      }
      print(paste("Prior: ", ourPrior))
      
      ourPriorParams = req$params()$priorParams
      if (is.null(ourPriorParams)) {
        ourPriorParams = c(0, 1.75)
        if (ourPrior == "cauchy") {
          # Cauchy spazzes at 0
          ourPriorParams = c(0.01, 1.75)
        }  
      } else {
        priorSE = req$params()$priorParamSE
        if (is.null(priorSE)) {
          priorSE = sqrt(1.75)
        }
        ourPriorParams = c(as.numeric(req$params()$priorParams), as.numeric(priorSE)^2)
        if (!is.null(req$params()$priorParamsDF)) {
          ourPriorParams = c(ourPriorParams, as.numeric(req$params()$priorParamsDF))
        }
      }
      print(paste("Prior Params: ", ourPriorParams))
      
      #Convert all skips/timeouts to wrong answers
      answers[is.na(answers) & !unasked_questions] = 0
      
      #Compute the next item and return it
      items = data.frame(difficulty=difficulty, discrimination=discrimination, guessing=guessing, answers=answers)
      cat = new("CATsurv", questions=items, priorName=ourPrior, priorParams=ourPriorParams)
      print("CAT created.")
      
      if("th" %in% names(req$params()))
      {
        theta_hat = as.numeric(req$params()$th)
      }
      else
      {
        theta_hat = estimateTheta(cat)
      }
      print(paste("Theta: ", theta_hat))
      results$theta_hat = theta_hat
      
      if (sum(unasked_questions) > 0) {
        print(paste("Unasked Questions: ", sum(unasked_questions)))
        next_item = nextItem(cat, theta_hat, D=1.7)
        print(paste("Next Item: ", next_item))
        
        #Convert item response to proper "ID"
        next_id = questions[as.numeric(next_item["next.item"]),"ids"]
        next_item["item"] = next_id
        
        results$next_item = list()
        results$next_item$item_id = next_id
        results$next_item$EPV = next_item$info
        results$next_item$criterion = next_item$criterion
      }
      print("Done.")
    }
    
    res = Rook::Response$new()
    res$write(toJSON(results))
    res$finish()
  }
)

rook$start(listen='0.0.0.0', port=as.numeric(Sys.getenv("PORT")))

while(T) {
  Sys.sleep(10000)
}