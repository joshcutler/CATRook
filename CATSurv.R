library(sfsmisc)

class.name = "CATsurv"
setClass(class.name,
         representation=representation(
           questions="data.frame",
           priorName="character",
           priorParams="numeric"
           ),
         prototype=prototype(
           priorName="normal",
           priorParams=c(1,1)
           )
         )

setValidity(class.name, function(object) {
  cols = names(object@questions)
  if (!("difficulty" %in% cols))
    return("No difficulty column detected in @questions")
  if (!("discrimination" %in% cols))
    return("No discrimination column detected in @questions")
  if (!("guessing" %in% cols))
    return("No guessing column detected in @questions")
  if (!("answers" %in% cols))
    return("No answers column detected in @questions")

  return(TRUE)
})

setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

setGeneric("three.pl", function(cat, theta, difficulty, discrimination, guessing, D=1){standardGeneric("three.pl")})
setMethod(f="three.pl", signature=class.name, definition=function(cat, theta, difficulty, discrimination, guessing, D=1) {
  exp.portion = exp(D*discrimination*(theta - difficulty))
  prob = guessing + (1 - guessing)*(exp.portion / (1 + exp.portion))
})

setGeneric("likelihood", function(cat, theta, items, D=1){standardGeneric("likelihood")})
setMethod(f="likelihood", signature=class.name, definition=function(cat, theta, items, D=1) {
  probabilities = three.pl(cat, theta, items$difficulty, items$discrimination, items$guessing, D)
  prod(probabilities^items$answers * (1 - probabilities)^(1 - items$answers))
})

setGeneric("prior", function(cat, values, name, params){standardGeneric("prior")})
setMethod(f="prior", signature=class.name, definition=function(cat, values, name, params) {
  prior.value = switch(name,
                       normal = dnorm(values, params[1], params[2]),
                       cauchy = dcauchy(values, params[1], params[2]),
                       t = 1 / params[2] * dt((values - params[1]) / params[2], df=params[3])
                      )
  return(prior.value)
})

setGeneric("estimateTheta", function(cat, D=1, priorName=NULL, priorParams=NULL, lowerBound=-4, upperBound=4, quadPoints=33, ...){standardGeneric("estimateTheta")})
setMethod(f="estimateTheta", signature=class.name, definition=function(cat, D=1, priorName=NULL, priorParams=NULL, lowerBound=-4, upperBound=4, quadPoints=33, ...) {
  X = seq(from=lowerBound, to=upperBound, length=quadPoints)
  applicable_rows = cat@questions[!is.na(cat@questions$answers), ]

  priorName = if (!is.null(priorName)) priorName else cat@priorName
  priorParams = if (!is.null(priorParams)) priorParams else cat@priorParams
  prior.values = prior(cat, X, priorName, priorParams)
  likelihood.values = rep(NA, times=length(X))
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, X[i], applicable_rows, D)
  }

  results = integrate.xy(X, X*likelihood.values*prior.values) / integrate.xy(X, likelihood.values*prior.values)
  return(results)
})

setGeneric("estimateSE", function(cat, theta.hat, D=1, priorName=NULL, priorParams=NULL, lowerBound=-4, upperBound=4, quadPoints=33, ...){standardGeneric("estimateSE")})
setMethod(f="estimateSE", signature=class.name, definition=function(cat, theta.hat, D=1, priorName=NULL, priorParams=NULL, lowerBound=-4, upperBound=4, quadPoints=33, ...) {
  X = seq(from=lowerBound, to=upperBound, length=quadPoints)
  applicable_rows = cat@questions[!is.na(cat@questions$answers), ]

  priorName = if (!is.null(priorName)) priorName else cat@priorName
  priorParams = if (!is.null(priorParams)) priorParams else cat@priorParams
  prior.values = prior(cat, X, priorName, priorParams)
  likelihood.values = rep(NA, times=length(X))
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, X[i], applicable_rows, D)
  }

  results = sqrt(integrate.xy(X, (X - theta.hat)^2*likelihood.values*prior.values) / integrate.xy(X, likelihood.values*prior.values))
  return(results)
})

setGeneric("expectedPV", function(cat, item, theta.est, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("expectedPV")})
setMethod(f="expectedPV", signature=class.name, definition=function(cat, item, theta.est, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  prob.correct = three.pl(cat, theta.est, cat@questions[item,]$difficulty, cat@questions[item,]$discrimination, cat@questions[item,]$guessing, D)
  prob.incorrect = 1 - prob.correct

  old_val = cat@questions[item, 'answers']

  cat@questions[item, 'answers'] = 1
  theta.correct = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  variance.correct = estimateSE(cat, theta.correct, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)^2

  cat@questions[item, 'answers'] = 0
  theta.incorrect = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  variance.incorrect = estimateSE(cat, theta.incorrect, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)^2

  cat@questions[item, 'answers'] = if (is.null(old_val) || is.na(old_val)) NA else old_val

  return(prob.correct*variance.correct + prob.incorrect*variance.incorrect)
})

setGeneric("nextItem", function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("nextItem")})
setMethod(f="nextItem", signature=class.name, definition=function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  available_questions = cat@questions[!(cat@questions$answers %in% c(0, 1)), ]

  if (is.na(theta.est)) {
    theta.est = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  }
  available_questions$epv = NA
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$epv = expectedPV(cat, as.numeric(row.names(available_questions[i,])), theta.est)
  }

  next.item = available_questions[available_questions$epv == min(available_questions$epv), ]
  to.return = list(all.estimates=available_questions, next.item=row.names(next.item))

  return(to.return)
})

setGeneric("storeAnswer", function(cat, item, answer){standardGeneric("storeAnswer")})
setMethod(f="storeAnswer", signature=class.name, definition=function(cat, item, answer) {
  cat@questions[item, 'answers'] = answer
  return(cat)
})

setGeneric("debugNextItem", function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("debugNextItem")})
setMethod(f="debugNextItem", signature=class.name, definition=function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  if (is.na(theta.est)) {
    theta.est = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  }

  next.item = nextItem(cat, theta.est, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)

  plot(next.item$all.estimates$difficulty, next.item$all.estimates$epv, type="n", xlab="Difficulty", ylab="EPV")
  lines(next.item$all.estimates$difficulty, next.item$all.estimates$epv)
  segments(theta.est, 0, theta.est, 10, lty=2)
  points(next.item$all.estimates[next.item$next.item,]$difficulty, next.item$all.estimates[next.item$next.item,]$epv, col="red")

  return(next.item)
})