rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
library(reshape2)

tau<-seq(0,10)/11
dis<-seq(0,10)/11
pol<-c(rep(0,6),rep(1,5))
disc<-NULL
polc<-NULL
for(i in tau){
  disc<-c(disc,sum(dis>i))
}
for(i in tau){
  polc<-c(polc,sum(pol>i))
}


load("full_data.Rda")
full_data<-full_data[!(full_data$session.code=="49mqxl65" & full_data$subsession.round_number==9),]

#full_data_late<-full_data
full_data_late<-full_data[full_data$marca %% 2 ==0,]

full_data_late <- full_data_late%>%group_by(session.code, subsession.round_number) %>% mutate(policy_cond = sum(player.vote_cond*player.finalassets, na.rm=T))

#We first assume that everyone should get 100 which is true when cost is 60 
full_data_late$profit_eq <- 100

#now, for other costs theis multiplicity, so given the current shareholder base, we assume they sincerely vote ,
## and determine the policy outcome (captured in variable policy_cond) 

full_data_late<- full_data_late %>% mutate(profit_eq = ifelse(group.costo<60,
                            player.finalassets*(100)-(player.finalassets-1)*(100-group.costo*(policy_cond>5)) + (player.lama*100-group.costo*player.finalassets)*(policy_cond>5), profit_eq)) 

full_data_late$profi_dif<-full_data_late$player.payoff-full_data_late$profit_eq
parsito<-full_data_late$group.uniforme==1
parsi<-full_data_late$group.uniforme==0

shareholders <- full_data_late$player.finalassets>0
sellers <- full_data_late$player.finalassets<1

aprobado <- full_data_late$group.policy>0
desaprobado <- full_data_late$group.policy==0

ahu<-tapply(full_data_late$player.payoff[parsito & shareholders & aprobado]-full_data_late$profit_eq[parsito& shareholders & aprobado],full_data_late$group.costo[parsito& shareholders &aprobado],mean,na.rm=T)
ahu_n<-tapply(full_data_late$player.payoff[parsito & shareholders & aprobado],full_data_late$group.costo[parsito& shareholders &aprobado],mean,na.rm=T)

anu<-tapply(full_data_late$player.payoff[parsito & sellers & aprobado]-full_data_late$profit_eq[parsito& sellers & aprobado],full_data_late$group.costo[parsito& sellers &aprobado],mean,na.rm=T)
anu_n<-tapply(full_data_late$player.payoff[parsito & sellers & aprobado],full_data_late$group.costo[parsito& sellers &aprobado],mean,na.rm=T)

dhu<-tapply(full_data_late$player.payoff[parsito & shareholders & desaprobado]-full_data_late$profit_eq[parsito& shareholders & desaprobado],full_data_late$group.costo[parsito& shareholders &desaprobado],mean,na.rm=T)
dhu_n<-tapply(full_data_late$player.payoff[parsito & shareholders & desaprobado],full_data_late$group.costo[parsito& shareholders &desaprobado],mean,na.rm=T)

dnu<-tapply(full_data_late$player.payoff[parsito & sellers & desaprobado]-full_data_late$profit_eq[parsito& sellers & desaprobado],full_data_late$group.costo[parsito& sellers &desaprobado],mean,na.rm=T)
dnu_n<-tapply(full_data_late$player.payoff[parsito & sellers & desaprobado],full_data_late$group.costo[parsito& sellers &desaprobado],mean,na.rm=T)





df1 <- as.data.frame(round(cbind(ahu, anu), 2))
df2 <- as.data.frame(round(cbind(dhu, dnu), 2))

final_df <- merge(df1, df2, by = "row.names", all = TRUE)
rownames(final_df) <- final_df$Row.names
final_df$Row.names <- NULL

df1 <- as.data.frame(round(cbind(ahu_n,anu_n),2))
df2 <- as.data.frame(round(cbind(dhu_n,dnu_n),2))

final_df3 <- merge(df1, df2, by = "row.names", all = TRUE)
rownames(final_df3) <- final_df3$Row.names
final_df3$Row.names <- NULL




ahu<-tapply(full_data_late$player.payoff[parsi & shareholders & aprobado]-full_data_late$profit_eq[parsi& shareholders & aprobado],full_data_late$group.costo[parsi& shareholders &aprobado],mean,na.rm=T)
ahu_n<-tapply(full_data_late$player.payoff[parsi & shareholders & aprobado],full_data_late$group.costo[parsi& shareholders &aprobado],mean,na.rm=T)

anu<-tapply(full_data_late$player.payoff[parsi & sellers & aprobado]-full_data_late$profit_eq[parsi& sellers & aprobado],full_data_late$group.costo[parsi& sellers &aprobado],mean,na.rm=T)
anu_n<-tapply(full_data_late$player.payoff[parsi & sellers & aprobado],full_data_late$group.costo[parsi& sellers &aprobado],mean,na.rm=T)

dhu<-tapply(full_data_late$player.payoff[parsi & shareholders & desaprobado]-full_data_late$profit_eq[parsi& shareholders & desaprobado],full_data_late$group.costo[parsi& shareholders &desaprobado],mean,na.rm=T)
dhu_n<-tapply(full_data_late$player.payoff[parsi & shareholders & desaprobado],full_data_late$group.costo[parsi& shareholders &desaprobado],mean,na.rm=T)

dnu<-tapply(full_data_late$player.payoff[parsi & sellers & desaprobado]-full_data_late$profit_eq[parsi& sellers & desaprobado],full_data_late$group.costo[parsi& sellers &desaprobado],mean,na.rm=T)
dnu_n<-tapply(full_data_late$player.payoff[parsi & sellers & desaprobado],full_data_late$group.costo[parsi& sellers &desaprobado],mean,na.rm=T)

df1 <- as.data.frame(round(cbind(ahu, anu), 2))
df2 <- as.data.frame(round(cbind(dhu, dnu), 2))

final_df1 <- merge(df1, df2, by = "row.names", all = TRUE)
rownames(final_df1) <- final_df1$Row.names
final_df1$Row.names <- NULL

##TABLE B.2
xtable(rbind(final_df,final_df1))


df1 <- as.data.frame(round(cbind(ahu_n,anu_n),2))
df2 <- as.data.frame(round(cbind(dhu_n,dnu_n),2))

final_df4 <- merge(df1, df2, by = "row.names", all = TRUE)
rownames(final_df4) <- final_df4$Row.names
final_df4$Row.names <- NULL


##TABLE B.1
xtable(rbind(final_df3,final_df4))


plot(ecdf(full_data_late$player.payoff[parsito & full_data_late$group.costo==20]-full_data_late$profit_eq[parsito& full_data_late$group.costo==20]))
plot(ecdf(full_data_late$player.payoff[parsito & full_data_late$group.costo==35]-full_data_late$profit_eq[parsito& full_data_late$group.costo==35]))


plot(ecdf(full_data_late$player.payoff[parsi& full_data_late$group.costo==20]-full_data_late$profit_eq[parsi& full_data_late$group.costo==20]))
plot(ecdf(full_data_late$player.payoff[parsi & full_data_late$group.costo==35]-full_data_late$profit_eq[parsi& full_data_late$group.costo==35]))

#profit_low<-tapply(full_data_late$player.payoff[parsito & shareholders]-full_data_late$profit_eq[parsito& shareholders],full_data_late$group.costo[parsito& shareholders],mean,na.rm=T)
#profit_high<-tapply(full_data_late$player.payoff[parsi]-full_data_late$profit_eq[parsi],full_data_late$group.costo[parsi],mean,na.rm=T)

profit_low<-tapply(full_data_late$player.payoff[parsito & shareholders],full_data_late$group.costo[parsito& shareholders],mean,na.rm=T)
profit_high<-tapply(full_data_late$player.payoff[parsi]-full_data_late$profit_eq[parsi],full_data_late$group.costo[parsi],mean,na.rm=T)



###
# For each market, compute player i deviation to a new bid (smaller by 1). 
#Compute prices, trade vectors, voting sincerely, and payoff. 

sesion=unique(full_data_late$session.code)
ronda = unique(full_data_late$subsession.round_number)
result=NULL
for (u in c(0,1)){
  desviacion = NULL
  no_cambio=NULL
  
  for (i in 1:length(sesion)){
    for (r in 1:length(ronda)){
      da = filter(full_data_late, session.code == sesion[i] & subsession.round_number==ronda[r] & group.uniforme==u &group.policy==1)
      if (dim(da)[1]>0){
        for (j in 1:dim(da)[1]){
          df = da
          df$player.bid[j]<-df$player.bid[j]-5
          noisy<-runif(dim(df)[1], min = 0, max = 1)
          df$player.bid<-df$player.bid+noisy
          precio = median(df$player.bid)
          aprobado=(2*sum(df$player.lama[df$player.bid>precio]>=2*df$group.costo[1]/100)+
            sum(df$player.lama[df$player.bid==precio]>=df$group.costo[1]/100))>5
          if(df$player.bid[j]>precio){
            t=1
          }
            else if(df$player.bid[j]<precio){
            t=-1
            }
          else{
            t=0
          }
          new_pay= 100+t*(100-floor(precio))+(100*df$player.lama[j]-(1+t)*df$group.costo[1])*aprobado      
          desviacion= rbind(desviacion,new_pay-df$player.payoff[j])
          no_cambio=rbind(no_cambio,aprobado)
          if(new_pay-df$player.payoff[j]<0){
            a=df$player.id_in_group[j]
            }
        }
      }
    }
  }
  simpleton<-desviacion
  simpleton[simpleton<0]<--1
  simpleton[simpleton==0]<-0
  simpleton[simpleton>0]<- 1
  temporal=table(simpleton)/length(simpleton)
  
  result=rbind(result,temporal)
}

#Table B.4
xtable(round(result,2))

#table(no_cambio)
#table(no_cambio[simpleton==0])
#table(simpleton[no_cambio==FALSE])/length(simpleton)

