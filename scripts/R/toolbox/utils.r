miqr <- function(x, precision=1, na.rm=T)
{
     paste(round(median(x, na.rm=na.rm),precision), " (", round(quantile(x, 0.25, na.rm=na.rm),precision), " to ", round(quantile(x, 0.75, na.rm=na.rm),precision) , ")", sep="")
}
#
mrge <- function(x, precision=1, na.rm=T)
{
     paste(round(median(x, na.rm=na.rm),precision), " (", round(range(x, na.rm=na.rm)[1],precision), " to ", round(range(x, na.rm=na.rm)[2],precision) , ")", sep="")
}
#
msd <- function(x, precision=1, na.rm=T)
{
     paste(round(mean(x, na.rm=na.rm),precision), " (", round(sd(x, na.rm=na.rm),precision), ")", sep="")
}
#
rr.calc <- function(beta, se, p0, alpha=0.05)
{
    or <- exp(beta)
    orl <- exp(beta - qnorm(1-alpha/2)*se)
    ors <- exp(beta + qnorm(1-alpha/2)*se)
    rr <- or/((1-p0)+(p0*or))
    rrl <- orl/((1-p0)+(p0*orl))
    rrs <- ors/((1-p0)+(p0*ors))
    print(paste(round(rr,2), " (", round(rrl,2), " to ", round(rrs,2), ")", sep=""))

    res <- cbind("RR"=rr, "Lower"=rrl, "Upper"=rrs)
}
#
or.lrm <- function(obj, alpha=0.05, signe=1, shrk=1)
{
    se <- shrk*sqrt(diag(obj$var))
    res <- data.frame(OR=round(exp(signe*shrk*obj$coef),2), lower=round(exp(signe*shrk*obj$coef-qnorm(1-alpha/2)*se),2), upper=round(exp(signe*shrk*obj$coef+qnorm(1-alpha/2)*se),2), p.value=signif(1-pchisq((shrk*obj$coef/se)^2,1),2))
    return(res)
}
#
rr.or <- function(or, lower, upper, p0)
{
    rr <- or/((1-p0)+p0*or)
    rlow <- lower/((1-p0)+p0*lower)
    rupp <- upper/((1-p0)+p0*upper)
    return(cbind(rr,rlow,rupp))
}
#
rr.lrm <- function(orl, p0)
{
    or <- orl[,1]
    lower <- orl[,2]
    upper <- orl[,3]
    rr <- or/((1-p0)+p0*or)
    rlow <- lower/((1-p0)+p0*lower)
    rupp <- upper/((1-p0)+p0*upper)
    res <- data.frame(RR=rr, Lower=rlow, Upper=rupp)
    print(round(res,2))
    return(res)
}
#
or.glm <- function(obj, alpha=0.05, signe=1)
{   
    se <- sqrt(diag(summary(obj)$cov.scaled))
    pv <- signif(1-pchisq((obj$coef/se)^2,1),2)
    for(i in 1:length(se))
    {
          print(paste(names(obj$coef)[i], " ", round(exp(signe*obj$coef[i]),2), " (", round(exp(signe*obj$coef[i]-qnorm(1-alpha/2)*se[i]),2), " to ", round(exp(signe*obj$coef[i]+qnorm(1-alpha/2)*se[i]),2), "); P=", pv[i],  sep=""))
    }
}
#
# Exact CI for a proportion
# Collett, D. Modeling Binary Data, London: Chapman & Hall (1991), pp 23-25.
# It is also referenced as the source for a SAS macro EXACTPCI, V1.2
binom.conf <-function(y, n, alpha = 0.05)
{
        lower <- rep(0, length(y))
        upper <- rep(1, length(y))
        lower[y>0&!is.na(y)] <- y[y>0&!is.na(y)]/(y[y>0&!is.na(y)] + (n[y>0&!is.na(y)] - y[y>0&!is.na(y)] + 1) * qf(1 - (alpha/2), 2 * (n[y>0&!is.na(y)] - y[y>0&!is.na(y)] + 1), 2 * y[y>0&!is.na(y)]))
        upper[y<n&!is.na(y)] <- (y[y<n&!is.na(y)] + 1)/(y[y<n&!is.na(y)] + 1 + (n[y<n&!is.na(y)] - y[y<n&!is.na(y)])/qf(1 - (alpha/2), 2 * (y[y<n&!is.na(y)] + 1), 2 * (n[y<n&!is.na(y)] - y[y<n&!is.na(y)])))
        est <- y/n
        lower[is.na(y)] <- NA
        upper[is.na(y)] <- NA
        cbind(lower, est, upper)
}
## Computes confidence intervals within the range 0-1 for cumulative incidence
# usage = confint.cuminc(x$"1 1"), where x is an cuminc object
confint.cuminc <- function(obj, alpha=0.05)
{
  require(msm)
  essai <- cbind(obj$time,obj$est,obj$var)
  lse <- apply(essai,1,function(x){deltamethod(~log(-log(x1)),x[2],x[3])})
  lse[is.na(lse)] <- 0
  essai <- cbind(essai,exp(-exp(log(-log(essai[,2]))+qnorm(1-alpha/2)*lse)))
  essai <- cbind(essai,exp(-exp(log(-log(essai[,2]))-qnorm(1-alpha/2)*lse)))
  essai <- as.data.frame(essai)
  names(essai) <- c("time","est","var","lower","upper")
  return(essai)
}
## old  confidence interval for CIF at given times
cif.ci <- function(y, times, alpha=0.05)
{
  require(cmprsk)
  require(msm)
  x <- timepoints(y, times)
  nr <- nrow(x$est)
  data.frame(est=round(100*x$est,1), lower=round(100*pmax(rep(0,nr), as.vector(x$est-qnorm(1-alpha/2)*sqrt(x$var))),1), upper=round(100*pmin(rep(1,nr), as.vector(x$est+qnorm(1-alpha/2)*sqrt(x$var))),1))
}
## new: confidence intervals within the range 0-1 for cumulative incidence
cif.ci <- function(y, times, alpha=0.05, prec=1)
{
  require(cmprsk)
  require(msm)
  x <- timepoints(y, times)
  lse <- apply(cbind(x$est,x$var),1,function(x){deltamethod(~log(-log(x1)),x[1],x[2])})
  lse[is.na(lse)] <- 0
  nr <- nrow(x$est)
  data.frame(est=round(100*x$est,prec), lower=round(100*exp(-exp(log(-log(x$est))+qnorm(1-alpha/2)*lse)),prec), upper=round(100*exp(-exp(log(-log(x$est))-qnorm(1-alpha/2)*lse)),prec))
}
## A vérifier tout de même
cif.cimax <- function(x, alpha=0.05)
{
  require(cmprsk)
  if (!is.null(x$Tests)) {
        nc <- length(x) - 1
    }
    else {
        nc <- length(x)
    }
  tmax <- y <- z <- rep(NA, nc)
  for (i in 1:nc)
    tmax[i] <- max(x[[i]]$time)
  cif <- timepoints(x, tmax - 1e-6)

  for (i in 1:nc)
  {
    y[i] <- cif[[1]][i,(tmax[i]- 1e-6)==sort(unique(tmax - 1e-6))]
    z[i] <- sqrt(cif[[2]][i,(tmax[i]- 1e-6)==sort(unique(tmax - 1e-6))])
  }
  data.frame(est=round(100*y,1), lower=round(100*pmax(rep(0,nc), as.vector(y-qnorm(1-alpha/2)*z)),1), upper=round(100*pmin(rep(1,nc), as.vector(y+qnorm(1-alpha/2)*z)),1))
}
## New: confidence intervals within the range 0-1 for cumulative incidence
cif.cimax <- function(x, alpha=0.05, prec=1)
{
  require(cmprsk)
  require(msm)
  if (!is.null(x$Tests)) {
        nc <- length(x) - 1
    }
    else {
        nc <- length(x)
    }
  tmax <- y <- z <- lse <- low <- upp <- rep(NA, nc)
  for (i in 1:nc)
    tmax[i] <- max(x[[i]]$time)
  cif <- timepoints(x, tmax - 1e-6)

  for (i in 1:nc)
  {
    y[i] <- cif[[1]][i,(tmax[i]- 1e-6)==sort(unique(tmax - 1e-6))]
    z[i] <- cif[[2]][i,(tmax[i]- 1e-6)==sort(unique(tmax - 1e-6))]
  }
  lse <- apply(cbind(y,z),1,function(x){deltamethod(~log(-log(x1)),x[1],x[2])})
  lse[is.na(lse)] <- 0
  low <- exp(-exp(log(-log(y))+qnorm(1-alpha/2)*lse))
  upp <- exp(-exp(log(-log(y))-qnorm(1-alpha/2)*lse))
  data.frame(est=round(100*y,prec), lower=round(100*low,prec), upper=round(100*upp,prec))
}
##
hr.ci <- function(obj, alpha=0.05)
{
  beta <- obj$coef
  se <- sqrt(diag(obj$var))
	hr <- exp(beta)
	lower <- exp(beta - qnorm(1-alpha/2)*se)
	higher <- exp(beta + qnorm(1-alpha/2)*se)
	pval <- 1-pchisq((beta/se)^2,1)

	paste(round(hr,2), " (", round(lower,2), ";", round(higher,2), "), p=", signif(pval,2), sep="")
}

# ------------------------------------------------------------------------------


##
# x : le nom de la variable à décrire
# y : le nom de la variable de groupage (NULL si inutile)
# type :
#   "m": polytomique ou binaire, et on veut toutes les classes
#   "b": binaire et on ne veut que la modalité "categ"
#   "c": quantitative
# usage :
# temp <- NULL
# temp <- descr("Age2", "inot_levo", icas_inotlev3, temp)
# temp <- descr("Sex", "inot_levo", icas_inotlev3, temp, type="b",categ="FEMALE")
# temp <- descr("InitialWeight.kg.", "inot_levo", icas_inotlev3, temp, type="c",desc="med")
descr <- function(x, y=NULL, dat, res, type="m", categ=NULL, desc="msd", prec=1, test=NULL)
{
 require(gmodels)
 if(!is.null(y))
 {
   if(is.null(test))
    pval <- NULL
    else
    {
    xv <- dat[,match(x,names(dat))]
    yv <- dat[,match(y,names(dat))]
    if(test=="student")
      pval <- signif(t.test(xv~yv)$p.value,2)
    if(test=="wilcoxon")
      pval <- signif(wilcox.test(xv~yv)$p.value,2)
    if(test=="kruskal")
      pval <- signif(kruskal.test(xv~yv)$p.value,2)
    if(test=="anova")
      pval <- signif(summary(lm(xv~factor(yv)))$coef[2,4],2)
    if(test=="chisq"&type=="m")
      pval <- signif(chisq.test(xv,yv,correct=F)$p.value,2)
    if(test=="fisher"&type=="m")
      pval <- signif(fisher.test(xv,yv)$p.value,2)
    if(test=="chisq"&type=="b")
      pval <- signif(chisq.test(as.factor(xv)==categ,yv,correct=F)$p.value,2)
    if(test=="fisher"&type=="b")
      pval <- signif(fisher.test(as.factor(xv)==categ,yv)$p.value,2)
    }

   if(type=="m")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],dat[,match(y,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- rownames(aa$t)
    res2 <- res
    for (i in 1:dim(aa$t)[1])
    {
      resi <- paste(x, ": ", noms[i], sep="")
      for(j in 1:dim(aa$t)[2])
        resi <- c(resi,paste(aa$t[i,j]," (",round(aa$prop.col[i,j]*100,prec),")",sep=""))
      resi <- c(resi,pval)
      res2 <- rbind(res2,resi)
    }
    if(is.null(pval))
      colnames(res2) <- c("Variable",colnames(aa$t))
    else
      colnames(res2) <- c("Variable",colnames(aa$t),"P.value")
    return(res2)
   }

   if(type=="b")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],dat[,match(y,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- rownames(aa$t)
    resi <- paste(x, ": ", categ, sep="")
    for(j in 1:dim(aa$t)[2])
      resi <- c(resi,paste(aa$t[match(categ,noms),j]," (",round(aa$prop.col[match(categ,noms),j]*100,prec),")",sep=""))
    resi <- c(resi,pval)
    res2 <- rbind(res,resi)
    if(is.null(pval))
      colnames(res2) <- c("Variable",colnames(aa$t))
    else
      colnames(res2) <- c("Variable",colnames(aa$t),"P.value")
    return(res2)
   }

   if(type=="c")
   {
     if(desc=="med")
     {
      aa <- tapply(dat[,match(x,names(dat))],dat[,match(y,names(dat))], miqr, precision=prec)
      resi <- paste(x)
      for(j in 1:length(aa))
        resi <- c(resi,paste(aa[j]))
      res2 <- rbind(res,c(resi,pval))
      if(is.null(pval))
        colnames(res2) <- c("Variable",names(aa))
      else
        colnames(res2) <- c("Variable",names(aa),"P.value")
      return(res2)
     }
     if(desc=="moy"|desc=="msd")
     {
      aa <- tapply(dat[,match(x,names(dat))],dat[,match(y,names(dat))], msd, precision=prec)
      resi <- paste(x)
      for(j in 1:length(aa))
        resi <- c(resi,paste(aa[j]))
      res2 <- rbind(res,c(resi,pval))
      if(is.null(pval))
        colnames(res2) <- c("Variable",names(aa))
      else
        colnames(res2) <- c("Variable",names(aa),"P.value")
      return(res2)
     }
     if(desc=="medrg")
     {
      aa <- tapply(dat[,match(x,names(dat))],dat[,match(y,names(dat))], mrge, precision=prec)
      resi <- paste(x)
      for(j in 1:length(aa))
        resi <- c(resi,paste(aa[j]))
      res2 <- rbind(res,c(resi,pval))
      if(is.null(pval))
        colnames(res2) <- c("Variable",names(aa))
      else
        colnames(res2) <- c("Variable",names(aa),"P.value")
      return(res2)
     }
   }
 }
 if(is.null(y))
 {
   if(type=="m")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- colnames(aa$t)
    res2 <- res
    for (i in 1:dim(aa$t)[2])
    {
      res2 <- rbind(res2,c(paste(x, ": ", noms[i], sep=""),paste(aa$t[i]," (",round(aa$prop.row[i]*100,prec),")",sep="")))
    }
    return(res2)
   }

   if(type=="b")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- colnames(aa$t)
    return(rbind(res,c(paste(x, ": ", categ, sep=""),paste(aa$t[match(categ,noms)]," (",round(aa$prop.row[match(categ,noms)]*100,prec),")",sep=""))))
   }

   if(type=="c")
   {
     if(desc=="med")
     {
      aa <- miqr(dat[,match(x,names(dat))], precision=prec)
      return(rbind(res,c(paste(x),paste(aa))))
     }
     if(desc=="moy"|desc=="msd")
     {
      aa <- msd(dat[,match(x,names(dat))], precision=prec)
      return(rbind(res,c(paste(x),paste(aa))))
     }
     if(desc=="medrg")
     {
      aa <- mrge(dat[,match(x,names(dat))], precision=prec)
      return(rbind(res,c(paste(x),paste(aa))))
     }
   }
 }

}
# Ancienne version
ex.descr <- function(x, y=NULL, dat, res, type="m", categ=NULL, desc="moy", prec=1, test=NULL)
{
  require(gmodels)
 if(!is.null(y))
 {
   if(is.null(test))
    pval <- NULL
    else
    {
    xv <- dat[,match(x,names(dat))]
    yv <- dat[,match(y,names(dat))]
    if(test=="student")
      pval <- signif(t.test(xv~yv)$p.value,2)
    if(test=="wilcoxon")
      pval <- signif(wilcox.test(xv~yv)$p.value,2)
    if(test=="kruskal")
      pval <- signif(kruskal.test(xv~yv)$p.value,2)
    if(test=="anova")
      pval <- signif(summary(lm(xv~factor(yv)))$coef[2,4],2)
    if(test=="chisq")
      pval <- signif(chisq.test(xv,yv,correct=F)$p.value,2)
    if(test=="fisher")
      pval <- signif(fisher.test(xv,yv)$p.value,2)
    }

   if(type=="m")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],dat[,match(y,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- rownames(aa$t)
    res2 <- res
    for (i in 1:dim(aa$t)[1])
    {
      res2 <- rbind(res2,c(paste(x, ": ", noms[i], sep=""),paste(aa$t[i,1]," (",round(aa$prop.col[i,1]*100,prec),")",sep=""),
      paste(aa$t[i,2]," (",round(aa$prop.col[i,2]*100,prec),")",sep=""),pval))
    }
    return(res2)
   }

   if(type=="b")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],dat[,match(y,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- rownames(aa$t)
    return(rbind(res,c(paste(x, ": ", categ, sep=""),paste(aa$t[match(categ,noms),1]," (",round(aa$prop.col[match(categ,noms),1]*100,prec),")",sep=""),
      paste(aa$t[match(categ,noms),2]," (",round(aa$prop.col[match(categ,noms),2]*100,prec),")",sep=""),pval)))
   }

   if(type=="c")
   {
     if(desc=="med")
     {
      aa <- tapply(dat[,match(x,names(dat))],dat[,match(y,names(dat))], miqr, precision=prec)
      return(rbind(res,c(paste(x),paste(aa[1]),paste(aa[2]),pval)))
     }
     if(desc=="moy")
     {
      aa <- tapply(dat[,match(x,names(dat))],dat[,match(y,names(dat))], msd, precision=prec)
      return(rbind(res,c(paste(x),paste(aa[1]),paste(aa[2]),pval)))
     }
     if(desc=="medrg")
     {
      aa <- tapply(dat[,match(x,names(dat))],dat[,match(y,names(dat))], mrge, precision=prec)
      return(rbind(res,c(paste(x),paste(aa[1]),paste(aa[2]),pval)))
     }
   }
 }
 if(is.null(y))
 {
   if(type=="m")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- colnames(aa$t)
    res2 <- res
    for (i in 1:dim(aa$t)[2])
    {
      res2 <- rbind(res2,c(paste(x, ": ", noms[i], sep=""),paste(aa$t[i]," (",round(aa$prop.row[i]*100,prec),")",sep="")))
    }
    return(res2)
   }

   if(type=="b")
   {
    aa<-CrossTable(dat[,match(x,names(dat))],prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE)
    noms<- colnames(aa$t)
    return(rbind(res,c(paste(x, ": ", categ, sep=""),paste(aa$t[match(categ,noms)]," (",round(aa$prop.row[match(categ,noms)]*100,prec),")",sep=""))))
   }

   if(type=="c")
   {
     if(desc=="med")
     {
      aa <- miqr(dat[,match(x,names(dat))], precision=prec)
      return(rbind(res,c(paste(x),paste(aa))))
     }
     if(desc=="moy")
     {
      aa <- msd(dat[,match(x,names(dat))], precision=prec)
      return(rbind(res,c(paste(x),paste(aa))))
     }
     if(desc=="medrg")
     {
      aa <- mrge(dat[,match(x,names(dat))], precision=prec)
      return(rbind(res,c(paste(x),paste(aa))))
     }
   }
 }
 
}
#
# Dummy variables pour un factor
dummies <- function(x)
{
 nl <- length(levels(x))
 xx <- matrix(NA, nrow=length(x), ncol=nl-1)
 for(i in 2:nl)
 {
  xx[,i-1] <- as.numeric(x==levels(x)[i])
 }
  return(xx)
}

###-----------------------------------------------------------------------------
# conf.t: times at which drawing confidence intervals
plot.cif <- function (x, wh, lgd=(missing(wh)|(!missing(wh)&length(wh)>1)), curvlab, conf.t, add=F, ylim = c(0, 1), xlim,
    xlab = "Years", ylab = "Cumulative incidence", lty = 1:length(x),
    color = 1, lwd = par("lwd"), ...)
{
    require(gplots)
    if (!is.null(x$Tests))
        x <- x[names(x) != "Tests"]
    nc <- length(x)
    if(missing(wh))
      wh <- 1:nc

    if (length(lty) < length(wh))
        lty <- rep(lty[1], length(wh))
    else lty <- lty[1:nc]
    if (length(lwd) < length(wh))
        lwd <- rep(lwd[1], length(wh))
    else lwd <- lwd[1:length(wh)]
    if (length(color) < length(wh))
        color <- rep(color[1], length(wh))
    else color <- color[1:length(wh)]
    if (missing(curvlab)) {
        if (mode(names(x)) == "NULL") {
            curvlab <- as.character(wh)
        }
        else curvlab <- names(x)[wh]
    }
    if (missing(xlim)) {
        xmax <- 0
        for (i in wh) {
            xmax <- max(c(xmax, x[[i]][[1]]))
        }
        xlim <- c(0, xmax)
    }
    if(add==F)
      plot(x[[1]][[1]], x[[1]][[2]], type = "n", ylim = ylim, xlim = xlim,
        xlab = xlab, ylab = ylab, bty = "l", ...)

    if(lgd)
    {
    u <- list(...)
    if (length(u) > 0) {
        i <- pmatch(names(u), names(formals(legend)), 0)
        do.call("legend", c(list(x = xlim[1], y = ylim[2], legend = curvlab,
            col = color, lty = lty, lwd = lwd, bty = "n", bg = -999999),
            u[i > 0]))
    }
    else {
        do.call("legend", list(x = xlim[1], y = ylim[2], legend = curvlab,
            col = color, lty = lty, lwd = lwd, bty = "n", bg = -999999))
    }
    }
    for (i in 1:length(wh)) {
        lines(x[[wh[i]]][[1]], x[[wh[i]]][[2]], lty = lty[i], col = color[i],
            lwd = lwd[i], ...)
    }
    if(!missing(conf.t))
    {
      for(j in conf.t)
      {
        ii <- 1
        jk <- seq(j-(length(wh)-1)*0.002*diff(xlim),j+(length(wh)-1)*0.002*diff(xlim),length=length(wh))
        for (i in 1:length(wh))
        {
          plotCI(jk[ii],cif.ci(x,j)[wh[i],1]/100,ui=cif.ci(x,j)[wh[i],3]/100,li=cif.ci(x,j)[wh[i],2]/100, gap=0, pch="", col = color[i], add=T)
          ii <- ii+1
        }
      }
    }
}


clep <- function(x,y)
{
a <- which(y<=x)
b <- rep(0,length(y))
b[a[length(a)]] <- 1
return(b)
}
tps.surv <- function(obj, times)
{
 y <- obj
 names(y) <- c("time","surv","lower","upper","se")
 y$indic <- apply(sapply(times,clep,y=y[,1]),1,sum)
 y <- y[y$indic==1,]
 y <- cbind(times,y)
 names(y)[1:2] <- c("time","lastev.time")
 return(y[,1:6])
}


# obj: survfit object
# times: times at which compute surival, 95%CI and SE
tp.surv <- function(obj, times)
{
x <- summary(obj)
if(is.null(x$strata))
{
  y <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err))
  res <- tps.surv(y,times)
} else
{
   res <- NULL
   ld <- c(0,cumsum(table(x$strata)))
   for(i in 1:length(table(x$strata)))
   {
     y <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err)[(1+ld[i]):ld[i+1],])
     res[[i]] <- tps.surv(y,times)
   }
}
return(res)
}


REPRO <- function(VAR1,VAR2,LIMX,LIMY,CEX.LAB,CEX.AXIS)
{
source('C:/Users/Etienne/Documents/MES SCRIPTS R/BlandAltman_II.r')
library(psy)
library(boot)

toto <- BlandAltman(VAR1,VAR2,labx=" ",laby=" ",maintit=" ",limx=LIMX,limy=LIMY,CEX=3,CEX.LAB=CEX.LAB,CEX.AXIS=CEX.AXIS,LWD=3)
RES1 <- c(toto$difference.mean,toto$upper.agreement.limit,toto$lower.agreement.limit)

Diff <- NA
for(i in 1:20)
{
Diff[i] <- 100*(abs(VAR1[i]-VAR2[i])/((VAR1[i]+VAR2[i])/2))
}
RES2 <- c(median(Diff,na.rm=T),sd(Diff,na.rm=T))

DATA_icc1 <- data.frame(VAR1,VAR2)
icc.boot1 <- function(data,x) {icc(data[x,])[[7]]}
res <- boot(DATA_icc1,icc.boot1,1000)
RES3 <- c(as.numeric(as.character(icc(DATA_icc1)$icc.agreement)),as.numeric(as.character(quantile(res$t,c(0.025,0.975)))))

RESULT <- c(RES1,RES2,RES3)
names(RESULT) <- c("BIAS","UPPER LIMIT","LOWER LIMIT","CV","SE CV","ICC","ICC_low","ICC_high")
return(RESULT)
}

TEST_DIAG <- function(val_test,val_diag)
{
toto <- table(val_test,val_diag)

VN <- toto[1,1]
FP <- toto[2,1]
FN <- toto[1,2]
VP <- toto[2,2]

Se <- VP/(VP+FN)
Spe <- VN/(VN+FP)
VPP <- VP/(VP+FP)
VPN <- VN/(VN+FN)
ACC <- (VP+VN)/(VP+VN+FP+FN)

return(c(Se,Spe,VPP,VPN,ACC))
}
