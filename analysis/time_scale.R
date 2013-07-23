# functions for the timescales
# carl simpson

tscales <- function(
  #Pmax,
  #Pmin,
  top,
  #bot,
  s.bot,
  #s.period,
  reso,
  cex,
  lwd,
  text.col = "grey45"
  ){
# s.period gives the starting period.
# reso can be stages or periods  
  stages <- c(542, 521, 501, 488, # Cambrian
  472, 461, 444, # Ordovician
  428, 423, 416, # Silurian
  398, 385, 359, # Devonian
  318, 299, # Carboniferous
  271, 260, 251, # Permian
  245, 235, 201.6, # Triassic
  176, 161, 145.5, # Jurassic
  99.6, 65.5, # Cretaceous
  55.8, 33.9, 23.0, # Paleogene
  5.3 ,1.8,0) #Neogene
  
  epochs  <- c(65.5, 61.7, 58.7, 55.8, # Paleocene
              48.6, 37.2, 33.9, # Eocene
              28.4, 23.0, # Oligocene
              16.0, 11.6, 5.3, #Miocene
              1.8,0) #Plio-Pleistocene
  tsx <- c("P", "E", "O", "M", "Pl", "")
  
  periods <- c(542, 488, 444, 416, 359, 299, 251, 201.6, 145.5, 65.5, 23, 0)
  tpx <- c("Cm", "O", "S", "D", "C", "P", "Tr", "J", "K", "Pg", "Ng")

  if(reso=='stages'){
     bases <- stages
     bases1 <- epochs
     tx <- tsx}
  else{ bases <- periods
        bases1 <- periods
        tx <- tpx
  }
    

  cc <- rep(c("grey10","grey85 "),length(bases))

  #bt <- bot
  #to <- top
  #for(b in 1:length(cc))
  #{
    #rect(xleft = bases1[b], ybottom = bt, xright = bases1[b+1], ytop = to, col = cc[b], border=NA) 
  #}


  is.odd <- function(x) x %% 2 != 0

  bt <- s.bot
  to <- top
  for(b in 1:length(bases))
  {
    col <- ifelse(is.odd(b), "white", "grey90")
    rect(xleft = bases[b], ybottom = bt, xright = bases[b+1], ytop = to,border='grey80', lwd=lwd, col = col) 
  }


  bt <- (bt+to)/2
  tpl <- bases+c(diff(bases)/2,0)
  c1 <- length(tpl)-length(tx)
  c2 <- length(tpl)
  tpl <- tpl[c1:c2]
  
  for(t in 1:length(tx))
  {
    text(x=tpl[t], y=bt, labels=tx[t],cex=cex, col = text.col)
  }
}
#plot(1:600, 1:600)
#tscales(top = 20, s.bot = 0, reso = "periods", cex = 0.8, lwd = 1)
