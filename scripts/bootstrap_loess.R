boot.resample <- function(lo.object, R, rows, new.xpts, weights) {
  f <- formula(lo.object)
  orig.data <- data.frame(lo.object$y, lo.object$x)
  names(orig.data) <- all.vars(f)
  boot.list <- vector("list", length = R)
  names(boot.list) <- as.character(seq_len(R))
  
  for(i in 1:R) {
    boot.idx <- sample(seq_len(nrow(orig.data)),
                       replace = TRUE, prob = weights)
    mf <- orig.data[boot.idx, ]
    rs.lo <- loess(f, data = mf, span = lo.object$pars$span) #span = lo.object$pars$span
    rval <- list(rss = sum(residuals(rs.lo)^2),
                 fitted = predict(rs.lo, data.frame(new.xpts)))
    boot.list[[i]] <- rval
  }
  boot.list
}



l.boot <- function(lo.object, R, rows = TRUE, 
                       new.xpts = NULL, weights = NULL) {
  boot.result <- list()
  
  if(is.null(new.xpts)) {
    new.xpts <- data.frame(seq(min(lo.object$x), max(lo.object$x),
                               len = lo.object$n))
    names(new.xpts) <- colnames(lo.object$x)
  }
  if(is.null(weights))
    weights <- rep(1, length(lo.object$x))
  boot.list <- boot.resample(lo.object, R, rows, new.xpts, weights)
  
  boot.result$method <- ifelse(rows, "rows", "residuals")
  boot.result$boot.list <- boot.list
  boot.result$orig.loess <- lo.object
  boot.result$new.xpts <- new.xpts
  boot.result$R <- R
  class(boot.result) <- c("loess.simpleboot")
  boot.result
}


plot.loess.simpleboot <- function(x, add = FALSE, all.lines = FALSE, ...) {
  xpts <- x$new.xpts    
  bmat <- sapply(x$boot.list, "[[", "fitted")
  std <- apply(bmat, 1, sd, na.rm = TRUE)
  orig.pred <- predict(x$orig.loess, data.frame(xpts))
  
  if(!add) {
    xdata <- x$orig.loess$x
    ydata <- x$orig.loess$y
    plot(xdata, ydata, ...)
  }
  lines(as.matrix(xpts), as.matrix(orig.pred))
  
  if(!all.lines) {
    lines(as.matrix(xpts), as.matrix(orig.pred) + 2*std, lty=3, col = "red")
    lines(as.matrix(xpts), as.matrix(orig.pred) - 2*std, lty=3, col = "red")
  }
  else {
    blist <- x$boot.list
    
    for(i in seq_len(x$R))
      lines(xpts, blist[[i]]$fitted)
  }
  invisible()          
}