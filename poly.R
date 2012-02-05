##### December 17th, 2011
#### Jacob Montgomery
#### Half-assed attempt to pull apart the ltm package to estimate theta with ordinal indicators


#I am going to (1) strip out any code that does not seem essential and (2) bring the code for fscores directly into this and (3) make it so it only does EAP

#Haven't quite figured out what this funciton is doing....but is called below during the integral approximation
cprobs <-function (item.params, z, eps = .Machine$double.eps^(1/3)) {
  lapply(item.params,
         function (x, z) {
           nx <- length(x)
           out <- plogis(x[-nx] - matrix(x[nx] * z, nx - 1, length(z), TRUE))
           if (any(ind <- out == 1))
             out[ind] <- 1 - eps
           if (any(ind <- out == 0))
             out[ind] <- eps
           rbind(out, 1)        
         }, z = z)
}



pred.future <-function (object, resp.patterns = NULL, method = c("EAP"), B = 5, 
                              prior = TRUE, return.MIvalues = FALSE, ...) {
                                
  item.params <- object$coefficients # Make object named item.params
  fits <- fitted(object, resp.patterns = resp.patterns) #This is an excessively elaborate call to re-format the data and get the EXP object below REMOVE THIS
  X <- fits[, -ncol(fits), drop = FALSE] # Just pulling out numerica versions of the response pattern for use below
  nx <- nrow(X) # number of rows in the response pattern being estimated
  p <- length(item.params) # 
  method <- match.arg(method)
  #  Obs <- observedFreqs(object, X)
  res <- data.frame(X, Exp = fits[, ncol(fits)])
  names(res)[1:p] <- names(item.params)
  #  environment(fscores.g) <- environment()
  #res <- fscores.g(item.params, X, method)

  # Begin code taked out of fscores.g()
  # I am pretty sure this is doing the needed integrals using Hermit-Gaussian quadrature.  I am not satisfied with the degree tow which I know what the hell this is doing.
  Z <- object$GH$Z
  GHw <- object$GH$GHw
  cpr <- cprobs(item.params, Z)
  diff.cprs <- lapply(cpr, function (x) rbind(x[1, ], diff(x)))
  log.diff.cprs <- lapply(diff.cprs, log)
  log.p.xz <- matrix(0, nrow(X), length(Z))
  for (j in 1:p) {
      log.pr <- log.diff.cprs[[j]]
      xj <- X[, j]
      na.ind <- is.na(xj)
      log.pr <- log.pr[xj, , drop = FALSE]
      if (any(na.ind))
          log.pr[na.ind, ] <- 0
      log.p.xz <- log.p.xz + log.pr
  }
  p.xz <- exp(log.p.xz)
  p.x <- c(p.xz %*% GHw)
  p.zx <- p.xz / p.x
  res$z1 <- c(p.zx %*% (Z * GHw))
  res$se.z1 <- sqrt(c(p.zx %*% (Z * Z * GHw)) - res$z1^2)        
  # End code taken out of fscores.g()

  #out <- list(score.dat = res, method = method, B = B, call = object$call, resp.pats = !is.null(resp.patterns))
  out<-res
  out
  # So now we have an output that has the point estimate and variance, but we haven't actually calculated the expected posterior variance for each possible item.
  # And we also need to find the minimum posterior variance.
}


next.item.grm<-function(my.fit, so.far, resp.options, remaining.items){
  p<-length(so.far)
  remain<-length(remaining.items)

  theta.est<-pred.future(my.fit, matrix(so.far, nrow=1))$z1
  item.params<-my.fit$coefficients

  foo<-function(X) {
    out<-matrix(so.far, ncol=p, nrow=resp.options[X], byrow=T)
    out[,X]<-c(1:resp.options[X])
    out
  }
  possible.responses <- lapply(remaining.items, foo)
  estimates<-lapply(possible.responses, pred.future, obj=my.fit)
  names(estimates)<-names(my.fit$coefficients[remaining.items])
  estimates

  bar<-function(X){
    index<-remaining.items[X]
    num.resp<-resp.options[index]
    this.item<-item.params[index]
    ks=unlist(this.item)[-num.resp]
    this.scale<-1/unlist(this.item)[num.resp]
    edge.probs<-c(0,plogis(ks, location=theta.est, scale=this.scale),1)
    probs<-diff(edge.probs)
    names(probs)<-c(1:num.resp)
    probs
  }

  probs<-lapply(1:length(remaining.items), bar)
  names(probs)<-c(1:length(remaining.items))
  combined<-lapply(1:length(remaining.items), function(X) cbind(estimates[[X]], probs[[X]]))

  epv <- function(X){
    index.probs<-ncol(combined[[X]])
    index.var<-index.probs-1
    combined[[X]][,index.probs] %*% combined[[X]][,index.var]
  }

  post.var <- unlist(lapply(1:remain, epv))
  use.this.one<-which(post.var==min(post.var))

  index<-remaining.items[use.this.one]
  out<-list(index=index, num.resp.options=resp.options[index],
            theta.est=combined[[use.this.one]][,c("z1")],
            post.var=combined[[use.this.one]][,c("se.z1")]^2) # put more here.
  out
}

### Example applicaiton
#library(ltm)
#data(Environment)
#fit1<-grm(Environment)
#next.item.grm(my.fit=fit1, so.far=c(NA,NA,3,1,1,2), resp.options=c(3,3,3,3,3,3), remaining.items=c(1,2))