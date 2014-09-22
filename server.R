library(shiny)
library(maps)

shinyServer(function(input, output) {
  params <- reactive({
    alpha <- input$alpha
    return(list(alpha=alpha))
  })
  
  output$plot <- renderPlot({
    params <- params()
    alpha <- params$alpha
    
    cb <- c("#000000",  "#999999",  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    names(cb) <- c("black", "grey", "orange", "turquoise", "green", "yellow", "blue", "red", "pink")
    par(mfrow=c(3,1), tck=-0.03, mar=c(5, 3.2, 4, 1), mgp=c(3, 0.5, 0), xaxs="i", yaxs="i", cex=1.2)
    x=seq(-5, 5, length=1000)
    mu0=0
    muA=1.5
    sigma=1
    y_h0=dnorm(x, mean=mu0, sd=sigma)
    
    #H0
    crit2_h0=qnorm(p = alpha/2, mean = mu0, sd = sigma)
    crit1_h0=qnorm(p = 1-alpha/2, mean = mu0, sd = sigma)
    
    area2_h0=pnorm(q = crit1_h0, mean = mu0, sd = sigma, lower.tail = F)
    area1_h0=pnorm(q = crit2_h0, mean = mu0, sd = sigma, lower.tail=F)
    plot(x, y_h0, t="l", yaxt="n", ylab="", xlab="", xpd=NA, xaxt="n", bty="n",
         main = expression(paste(H[0], " is true:")))
    axis(1, at=c(seq(-5, mu0), muA, seq(3, 5)),
         lab=c(seq(-5, -1), expression(tau[0]==0), expression(tau==1.5), seq(3, 5)))
    x.type1=c(crit1_h0, seq(crit1_h0, 15, 0.01), 15)
    y.type1=c(0, dnorm(seq(crit1_h0, 15, 0.01), mean=mu0, sd = sigma), 0) 
    polygon(x.type1, y.type1, col=rgb(0.8, 0.4, 0, 0.5), border=NA)
    
    x.type12=c(-5, seq(-5, crit2_h0, 0.01), crit2_h0)
    y.type12=c(0, dnorm(seq(-5, crit2_h0, 0.01), mean=mu0, sd = sigma), 0) 
    polygon(x.type12, y.type12, col=rgb(0.8, 0.4, 0, 0.5), border=NA)
    
    abline(v=crit1_h0, lwd=2, lty=2, col=cb["red"])
    text(x=crit1_h0, y = max(y_h0), labels = expression(tau[C]), col=cb["red"], xpd=NA, pos=3, offset = 0.5)
    abline(v=crit2_h0, lwd=2, lty=2, col=cb["red"])
    text(x=crit2_h0, y = max(y_h0), labels = expression(tau[C]), col=cb["red"], xpd=NA, pos=3, offset = 0.5)
    
    arrows(x0=-5, y0=-0.09, x1=crit2_h0, y1=-0.09, col=cb["red"], length=0.1, lwd=2, xpd=NA, code=3)
    text(x=mean(c(-5, crit2_h0)), y=0, pos=1, offset=3, 
         label=expression(paste("Reject ", H[0])), col=cb["red"], font=1, xpd=NA)
    text(x=mean(c(-5, crit2_h0)), y=0, pos=1, offset=4, 
         label="when it is true", col=cb["red"], font=1, xpd=NA)
    
    arrows(x0=crit2_h0, y0=-0.09, x1=crit1_h0, y1=-0.09, col="black", length=0.1, lwd=2, xpd=NA, code=3)
    text(x=mean(c(crit1_h0, crit2_h0)), y=0, pos=1, offset=3, 
         label=expression(paste("Reject ", H[0])), col="black", xpd=NA)
    text(x=mean(c(crit1_h0, crit2_h0)), y=0, pos=1, offset=4, 
         label="when it is false", col="black", xpd=NA)
    
    arrows(x0=crit1_h0, y0=-0.09, x1=5, y1=-0.09, col=cb["red"], length=0.1, lwd=2, xpd=NA, code=3)
    text(x=mean(c(crit1_h0, 5)), y=0, pos=1, offset=3, 
         label=expression(paste("Reject ", H[0])), col=cb["red"], font=2, xpd=NA)
    text(x=mean(c(crit1_h0, 5)), y=0, pos=1, offset=4, 
         label="when it is true", col=cb["red"], font=1, xpd=NA)
    # Type I arrows
    arrows(x0=mean(c(crit1_h0, 5)), y0=max(y_h0)/2, x1=mean(c(crit1_h0, 5))-0.5, 
           y1=0.01, code=2, len=0.2, lwd=2, col=cb["red"])
    text(x=mean(c(crit1_h0, 5)), y=max(y_h0)/2, label=expression(paste("Type I error (", alpha/2, ")")), font=1, 
         col=cb["red"], pos=3, offset=0.5)
    arrows(x0=mean(c(crit2_h0, -5)), y0=max(y_h0)/2, 
           x1=mean(c(crit2_h0, -5))+0.5, y1=0.01, code=2, len=0.2, lwd=2, col=cb["red"])
    text(x=mean(c(crit2_h0, -5)), y=max(y_h0)/2, label=expression(paste("Type I error (", alpha/2, ")")), 
         font=1, col=cb["red"], pos=3, offset=0.5)
    # Ha
    y_hA=dnorm(x, mean=muA, sd=sigma)
    crit2_hA=qnorm(p = alpha/2, mean = muA, sd = sigma)
    crit1_hA=qnorm(p = 1-alpha/2, mean = muA, sd = sigma)
    
    area2_hA=pnorm(q = crit1_hA, mean = muA, sd = sigma, lower.tail = F)
    area1_hA=pnorm(q = crit2_hA, mean = muA, sd = sigma, lower.tail=F)
    plot(x, y_hA, t="l", yaxt="n", ylab="", xlab="", xpd=NA, xaxt="n", bty="n",
         main = expression(paste(H[a], " is true:")))
    axis(1, at=c(seq(-5, mu0), muA, seq(3, 5)),
         lab=c(seq(-5, -1), expression(tau[0]==0), expression(tau==1.5), seq(3, 5)))
    
    #text(x=-3.5, y=max(y_hA), expression(paste(H[a], " is true:")), font=2, xpd=NA)    
    x.type1=c(crit1_h0, seq(crit1_h0, 15, 0.01), 15)
    y.type1=c(0, dnorm(seq(crit1_h0, 15, 0.01), mean=muA, sd=sigma), 0) 
    polygon(x.type1, y.type1, col=rgb(0.8, 0.4, 0, 0.5), border=NA)
    
    x.type12=c(-4, seq(-4, crit2_h0, 0.01), crit2_h0)
    y.type12=c(0, dnorm(seq(-4, crit2_h0, 0.01), mean=muA, sd=sigma), 0) 
    polygon(x.type12, y.type12, col=rgb(0.8, 0.4, 0, 0.5), border=NA)
    
    x.power=c(crit2_h0, seq(crit2_h0, crit1_h0, 0.01), crit1_h0)
    y.power=c(0, dnorm(seq(crit2_h0, crit1_h0, 0.01), mean=muA, sd=sigma), 0)
    polygon(x.power, y.power, col=rgb(0, 0.45, 0.7, 0.5), border=NA)
    
    power1=pnorm(q=crit2_h0, mean=muA, sd=sigma, lower.tail = T)
    power2=pnorm(q=crit1_h0, mean=muA, sd=sigma, lower.tail =F)
    power=power1+power2
    beta=pnorm(q=crit1_h0, mean=muA, sd=sigma, lower.tail = T)
    beta=beta-power1
    
    abline(v=crit1_h0, lwd=2, lty=2, col=cb["red"])
    text(x=crit1_h0, y = max(y_hA), labels = expression(tau[C]), col=cb["red"], 
         xpd=NA, pos=3, offset = 0.5)
    abline(v=crit2_h0, lwd=2, lty=2, col=cb["red"])
    text(x=crit2_h0, y = max(y_hA), labels = expression(tau[C]), col=cb["red"], 
         xpd=NA, pos=3, offset = 0.5)
    arrows(x0=-5, y0=-0.09, x1=crit2_h0, y1=-0.09, col=cb["red"], length=0.1, 
           lwd=2, xpd=NA, code=3)
    text(x=mean(c(-5, crit2_h0)), y=0, pos=1, offset=3, 
         label=expression(paste("Reject ", H[0])), col=cb["red"], xpd=NA)
    text(x=mean(c(-5, crit2_h0)), y=0, pos=1, offset=4, 
         label="when it is false", col=cb["red"], xpd=NA)
    
    
    arrows(x0=crit2_h0, y0=-0.09, x1=crit1_h0, y1=-0.09, col="black", length=0.1, 
           lwd=2, xpd=NA, code=3)
    text(x=mean(c(crit1_h0, crit2_h0)), y=0, pos=1, offset=3, 
         label=expression(paste("Fail to reject ", H[0])), 
         col="black", font=2, xpd=NA)
    text(x=mean(c(crit1_h0, crit2_h0)), y=0, pos=1, offset=4, 
         label="when it is false", col="black", font=1, xpd=NA)
    
    arrows(x0=crit1_h0, y0=-0.09, x1=5, y1=-0.09, col=cb["red"], length=0.1, lwd=2, 
           xpd=NA, code=3)
    text(x=mean(c(crit1_h0, 5)), y=0, pos=1, offset=3, 
         label=expression(paste("Reject ", H[0])), col=cb["red"], 
         font=1, xpd=NA)
    text(x=mean(c(crit1_h0, 5)), y=0, pos=1, offset=4, 
         label="when it is false", col=cb["red"], 
         font=1, xpd=NA)
    # Power arrows
    arrows(x0=mean(c(crit1_h0, 5)), y0=max(y_hA)/2, x1=mean(c(crit1_h0, 5))-0.5, 
           y1=0.01, code=2, len=0.2, lwd=2, col=cb["red"])
    text(x=mean(c(crit1_h0, 5)), y=max(y_hA)/2, label=paste0("Power (", format(power2, digit=2), ")"), 
         font=1, 
         col=cb["red"], pos=3, offset=0.5)
    arrows(x0=mean(c(crit2_h0, -5)), y0=max(y_hA)/2, 
           x1=mean(c(crit2_h0, -5))+0.5, y1=0.01, code=2, len=0.2, lwd=2, col=cb["red"])
    text(x=mean(c(crit2_h0, -5)), y=max(y_hA)/2, label=paste0("Power (", format(power1, digit=2), ")"), 
         font=1, col=cb["red"], 
         pos=3, offset=0.5)
    # Type II error
    arrows(x0=mean(c(crit2_h0, crit1_h0)), y0=max(y_hA)/2-0.05, 
           x1=mean(c(crit2_h0, crit1_h0)), y1=0.01, code=2, len=0.2, lwd=2, 
           col=cb["blue"])
    text(x=mean(c(crit2_h0, crit1_h0)), y=max(y_hA)/2, 
         label="Type II error", font=2, col=cb["blue"], pos=3, offset=0.5)
    text(x=mean(c(crit2_h0, crit1_h0)), y=max(y_hA)/2, 
         label=substitute(paste("(", beta == bval, ")"), 
                          list(bval=format(beta, digit=2))), font=2, col=cb["blue"], pos=3, offset=-0.75)
    
    # Panel 3
    alphas <- seq(from=0.005, to=1, len=100)
    crit2_h0 <- qnorm(p = alphas/2, mean = mu0, sd = sigma)
    crit1_h0 <- qnorm(p = 1-alphas/2, mean = mu0, sd = sigma)    
    powers1 <- pnorm(q=crit2_h0, mean=muA, sd=sigma, lower.tail = T)
    powers2 <- pnorm(q=crit1_h0, mean=muA, sd=sigma, lower.tail =F)
    powers <- powers1+powers2
    betas <- pnorm(q=crit1_h0, mean=muA, sd=sigma, lower.tail = T)
    betas <- betas-powers1
    scaling=par()$cex
    plot(alphas, betas, t="l", ylim=c(0, 1), col=cb["black"], bty="n",
         xlab="", ylab="", xaxt="n", yaxt="n") 
    axis(1, col=cb["red"], col.ticks = cb["red"],  col.axis=cb["red"], at = seq(0, 1, by=0.2))
    axis(2, col=cb["blue"], col.ticks = cb["blue"],  col.axis=cb["blue"], at = seq(0, 1, by=0.2))
    mtext(1, line = 2, text=expression(paste("Type I error (", alpha, ")")), 
          col=cb["red"], cex=scaling, xpd=NA)
    mtext(2, line = 2, text=expression(paste("Type II error (", beta, ")")), 
          col=cb["blue"], cex=scaling, xpd=NA)
    #abline(v=0.05, lty=2, lwd=2, col=cb["red"])
    #text(x=0.05, y=1, pos=3, offset=0.5, labels = expression(alpha==0.05), col=cb["red"], xpd=NA)
    abline(v=alpha, lwd=2, col=cb["red"], lty=2)
    text(x=alpha, y=1, pos=3, offset=1.5, 
         labels = "Simulated", 
         col=cb["red"], xpd=NA)
    text(x=alpha, y=1, pos=3, offset=0.5, 
         labels = substitute(alpha==alp, list(alp=format(alpha, dig=3))), 
         col=cb["red"], xpd=NA)
    
    #box()#which="plot", lty=1)
        
  })
})
