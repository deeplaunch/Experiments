
######################################### Equity Option #############################

rm(list=ls())

library(ggplot2)
library(lubridate)
library(dplyr)
library(readxl)
library(RND)


filename="FB.xlsx"

Equity <- read_excel(filename)
View(Equity)

r = Equity$imp_rd[1]
te= Equity$te[1]/360
y= 0
s0 = Equity$spot[1]
call.premium = Equity$call_prem
call.strikes = Equity$call_strike
put.premium = Equity$put_premium
put.strikes =Equity$put_strike

gb.Equity = extract.gb.density(initial.values=c(NA,NA,NA,NA), r=r, te=te, y=y, s0=s0,
                               market.calls=call.premium, call.strikes = call.strikes, call.weights =1,
                               market.puts = put.premium, put.strikes = put.strikes, put.weights = 1,
                               lambda=1, hessian.flag=F)

Krange = seq(80,220,1)
gb = gb.Equity
gb.rnd.Equity = dgb(Krange, gb$a,gb$b, gb$v, gb$w)

gb.rnd.Equity = data.frame(Price = Krange, Probability = gb.rnd.Equity)

ggplot(data=gb.rnd.Equity, aes(x= Krange)) + geom_line(aes(y= gb.rnd.Equity$Probability), col="red", size=1.25)
