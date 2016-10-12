generate <- function(mu=0, sigma=1, dist="normal", nocols=5, norows=100, min=0, max=1, sd=3000) {
  set.seed(sd)
  if (dist == "uniform") {
    df <- replicate(nocols,runif(norows)*(min+(max-min)))
    return(df)
  }
  if (dist == "normal") {
    df <- replicate(nocols,rnorm(norows, mu, sigma)*(min+(max-min)))
    return (df)
  }
  if (dist == "weibull") {
    shape <- if (mu == 0) 1 else mu
    scale <- sigma
    
    df <- replicate(nocols,rweibull(norows, shape, scale)*(min+(max-min)))
    return(df)
  }
  if (dist == "beta") {
    shape1<-mu
    shape2<-sigma
    df<-replicate(nocols,rbeta(norows, shape1, shape2)*(min+(max-min)))
    return(df)
  }
  if (dist == "poisson") {
    lbda <- if (mu==0) 1 else mu
    df <- replicate(nocols,rpois(norows, lbda))
    return (df)
  }
  if(dist == "exponential") {
    r <- mu
    df <- replicate(nocols,rexp(norows, r)*(min+(max-min)))
    return(df)
  }
  if(dist == "gamma") {
    r <- if (mu==0) 1 else mu
    shape<-sigma
    df <- replicate(nocols,rgamma(norows, shape, r)*(min+(max-min)))
    return(df)
  }
  if(dist == "geometric") {
    p <- if (mu==0) 0.1 else mu
    
    df <- replicate(nocols,rgeom(norows, p))
    return(df)
  }
  if(dist == "hypergeometric") {
    m <- if (mu==0) 1 else mu
    df <- replicate(nocols,rhyper(norows, max, min, m))
    return(df)
  }
  if(dist == "logistic") {
    df <- replicate(nocols,rlogis(norows, mu, sigma))
    return(df)
  }
  if(dist == "log normal") {
    df <- replicate(nocols,rlnorm(norows, mu, sigma))
    return(df)
  }
  
}

createdistribution <- function(mu=0, sigma=1, dist="normal", nocols=5, norows=100, min=0, max=1, sd=3000) {
  df<-generate(mu, sigma, dist, nocols, norows, min, max, sd)
  hist(df, main="histogram of complete dataframe", col="red")
  hist(df[,1], main="histogram of first column", col="blue")
  
  return(as.data.frame(df))
}


