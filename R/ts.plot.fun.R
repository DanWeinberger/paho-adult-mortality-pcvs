ts.plot.fun <- function(ds,time.var=date, yvar,multiple_group=F, group.var, ylab1 ){
  p1 <- ggplot(ds, aes_string(x=time.var, y=yvar)) +
    geom_line() +
    ylab(ylab1) +
    xlab("Date") +
    theme_classic() +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
    geom_hline(yintercept=0, col='gray', lty=2) +
    ylim(0,NA)+
    facet_wrap(country~group.var , scales='free') +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) 
  return(p1)
}

