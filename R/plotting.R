varplot <- function(Wstar,n1,n2) {
  n.perm <- length(Wstar)
  W.var <- sapply(2:n.perm,function(i) var(Wstar[1:i]))
  var.theoretical <- exp(log(n1) + log(n2) + log(n1+n2+1) - log(12))
  plot(2:n.perm,W.var,ylim=range(c(W.var,var.theoretical)),pch=".",
       xlab="Permutation number",ylab="Var(W*)",main="Estimate of Var(W*) vs number of permutations",
       sub="(dotted line shows theoretical value)")
  abline(h=var.theoretical,lty=3)  
}
