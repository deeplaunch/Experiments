rm(list=ls())

setwd("\\\\data4/users6/PZhao/My Documents/R/Option")

library(ggplot2)
library(lubridate)
library(dplyr)
library(readxl)
library(RND)


######################################### Currency Option Functions #########################


get.strike = function(vol, delta, S0, fwd, rf, Tenor){
  # Inputs
  #   vol:     implied volatility
  #   S0 :     spot exchange rate
  #   fwd:     forward exchange rate
  #   rf:      foreign currency interest rate
  #   Tenor:   time to maturity of the option
  #
  # Note: since we use the forward rate, the domestic
  #       interest rate is not needed
  aux = qnorm(delta*exp(rf*Tenor))
  aux = aux*vol*sqrt(Tenor)
  aux = aux - 0.5*vol*vol*Tenor
  K = exp(-aux)*fwd
  return(K)
}


GKoption.premium = function(K,sigma,S,Tenor,fwd,rf,option_type){
  if (option_type =="c") {w=1}
  if (option_type =="p") {w=-1}
  d1 = log(fwd/K)+0.5*sigma*sigma*Tenor
  d1 = d1/(sigma*sqrt(Tenor))
  d2 = d1 - sigma*sqrt(Tenor)
  rd = log(fwd/S)/Tenor + rf
  premium = exp(-rd*Tenor)*(w*fwd*pnorm(w*d1) - w*K*pnorm(w*d2))
  return(premium)
  
}

######################################    Load Data        ################################

filename="HKDUSD.xlsx"

Currency <- read_excel(filename, sheet = 'Data', col_names =TRUE) #View(Currency)

Krange = seq(7.6,8.1,0.0005)


############################################### Pre ###############################################

Currency_pre_c <- Currency%>%filter(Date == min(Currency$Date),delta> 0)%>%arrange(delta)

K_pre_c =mapply(get.strike, Currency_pre_c$imp_vol, Currency_pre_c$delta, 
              S0 = Currency_pre_c$spot[1], fwd = Currency_pre_c$forward[1],
              rf =Currency_pre_c$rf[1], Tenor =Currency_pre_c$te[1])            #Get strike

Prem_pre_c = mapply(GKoption.premium, K_pre_c, Currency_pre_c$imp_vol,
                  S= Currency_pre_c$spot[1],Tenor=Currency_pre_c$te[1],
                  fwd = Currency_pre_c$forward[1],rf=Currency_pre_c$rf[1],option_type="c") #Get call option premium


Currency_pre_p <- Currency%>%filter(Date == min(Currency$Date),delta < 0)%>%arrange(delta)

K_pre_p =mapply(get.strike, Currency_pre_p$imp_vol, - Currency_pre_p$delta, 
                S0 = Currency_pre_p$spot[1], fwd = Currency_pre_p$forward[1],
                rf =Currency_pre_p$rf[1], Tenor =Currency_pre_p$te[1])        #Get strike

Prem_pre_p = mapply(GKoption.premium, K_pre_p, Currency_pre_p$imp_vol,
                    S= Currency_pre_p$spot[1],Tenor=Currency_pre_p$te[1],
                    fwd = Currency_pre_p$forward[1],rf=Currency_pre_p$rf[1],option_type="p") #Get put option premium

#################################

r = Currency_pre_c$rd[1]
te= Currency_pre_c$te[1]
y= Currency_pre_c$rf[1]

s0 = Currency_pre_c$spot[1]

call.premium = Prem_pre_c
call.strikes = K_pre_c

put.premium <- Prem_pre_p
put.strikes <- K_pre_p

gb.Currency = extract.gb.density(initial.values=c(NA,NA,NA,NA), r=r, te=te, y=y, s0=s0,
                               market.calls=call.premium, call.strikes = call.strikes, call.weights =1,
                               market.puts = put.premium, put.strikes = put.strikes, put.weights = 1,
                               lambda=1, hessian.flag=F)

gb = gb.Currency

gb.rnd.pre = dgb(Krange, gb$a,gb$b, gb$v, gb$w)



###############################################   post   ##############


Currency_pos_c <- Currency%>%filter(Date == max(Currency$Date),delta> 0)%>%arrange(delta)

K_pos_c =mapply(get.strike, Currency_pos_c$imp_vol, Currency_pos_c$delta, 
                S0 = Currency_pos_c$spot[1], fwd = Currency_pos_c$forward[1],
                rf =Currency_pos_c$rf[1], Tenor =Currency_pos_c$te[1])            #Get strike

Prem_pos_c = mapply(GKoption.premium, K_pos_c, Currency_pos_c$imp_vol,
                    S= Currency_pos_c$spot[1],Tenor=Currency_pos_c$te[1],
                    fwd = Currency_pos_c$forward[1],rf=Currency_pos_c$rf[1],option_type="c") #Get call option premium


Currency_pos_p <- Currency%>%filter(Date == max(Currency$Date),delta < 0)%>%arrange(delta)

K_pos_p =mapply(get.strike, Currency_pos_p$imp_vol, - Currency_pos_p$delta, 
                S0 = Currency_pos_p$spot[1], fwd = Currency_pos_p$forward[1],
                rf =Currency_pos_p$rf[1], Tenor =Currency_pos_p$te[1])        #Get strike

Prem_pos_p = mapply(GKoption.premium, K_pos_p, Currency_pos_p$imp_vol,
                    S= Currency_pos_p$spot[1],Tenor=Currency_pos_p$te[1],
                    fwd = Currency_pos_p$forward[1],rf=Currency_pos_p$rf[1],option_type="p") #Get put option premium

#################################

r = Currency_pos_c$rd[1]
te= Currency_pos_c$te[1]
y= Currency_pos_c$rf[1]

s0 = Currency_pos_c$spot[1]
call.premium = Prem_pos_c
call.strikes = K_pos_c

put.premium <- Prem_pos_p
put.strikes <- K_pos_p

gb.Currency = extract.gb.density(initial.values=c(NA,NA,NA,NA), r=r, te=te, y=y, s0=s0,
                                 market.calls=call.premium, call.strikes = call.strikes, call.weights =1,
                                 market.puts = put.premium, put.strikes = put.strikes, put.weights = 1,
                                 lambda=1, hessian.flag=F)



gb = gb.Currency
gb.rnd.post = dgb(Krange, gb$a,gb$b, gb$v, gb$w)



##################################  combine pre and post ###########################


gb.rnd.Currency = data.frame(Krange, gb.rnd.pre, gb.rnd.post)


ggplot(data=gb.rnd.Currency, aes(x= Krange)) + 
  geom_line(aes(y= gb.rnd.pre), col="red", size= 1.2) +
  geom_line(aes(y= gb.rnd.post), col="blue", size= 1.2) +
  #geom_vline(xintercept =Currency_pre_c$forward[1], col ="red", linetype ="longdash") +
  #geom_vline(xintercept =Currency_pos_c$forward[1], col ="blue", linetype ="longdash") +
  labs(x="HKDUSD", y="3-month risk-neutral density") +
  ggtitle(paste("Past (Red,",min(Currency$Date),") vs. Current (Blue,", max(Currency$Date),") Option-implied Probability")) +
  theme(legend.position = "bottom")










