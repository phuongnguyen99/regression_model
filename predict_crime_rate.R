# preprocessing
library(car)
data = read.csv('Crime.csv', header = T)
names(data)
dim(data)
summary(data)
sum(is.na(data) == TRUE)

# check for invalid observations 
sum(data$prbarr > 1)
sum(data$prbconv > 1)
data = data[-which(data$prbarr >1),]
data = data[-which(data$prbconv >1),]
dim(data)
summary(data)

# define the variables
y = data$crmrte
year = data$year ; prbarr = data$prbarr ; prbconv = data$prbconv ; prbpris = data$prbpris ; avgsen = data$avgsen ; polpc = data$polpc
density = data$density ; taxpc = data$taxpc ; pctmin = data$pctmin ; wcon = data$wcon ; wtuc = data$wtuc ; wtrd = data$wtrd ; wfir = data$wfir ; wser = data$wser 
wmfg = data$wmfg ; wfed = data$wfed ; wsta = data$wsta ; wloc = data$wloc ; mix = data$mix ; pctymle = data$pctymle
pairs(y~ year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
        wser + wmfg + wfed + wsta + wloc + mix + pctymle)
# full model 
full = lm(y~ year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
            wser + wmfg + wfed + wsta + wloc + mix + pctymle)
summary(full)
res_full = full$residuals
fits_full = full$fitted.values
plot(fits_full, res_full, main = 'Residuals vs fitted values for Full model')

# check for normality and constant variance 
qqnorm(res_full)
qqline(res_full)
shapiro.test(res_full)
ncvTest(full)

# chek for multicollinearity 
vif(full)

# plot residuals vs indiviudal regressor to check for any needed transformation
par(mfrow = c(2,3))
plot(year, res_full, main = 'Res vs year')
plot(prbarr, res_full, main = 'Res vs prob of arrest')
plot(prbconv, res_full, main = 'Res vs prob of conviction')
plot(prbpris, res_full, main = 'Res vs prob of prision')
plot(avgsen, res_full, main = 'Res vs average prision in days')
plot(polpc, res_full, main = 'Res vs number of police')

par(mfrow = c(2,3))
plot(density, res_full, main = 'Res vs density')
plot(taxpc, res_full, main = 'Res vs tax revenue')
plot(pctmin, res_full, main = 'Res vs percentage minority')
plot(wcon, res_full, main = 'Res vs weekly wage in construction')
plot(wtuc, res_full, main = 'Res vs weekly wage in trans, utilities, communications')
plot(wtrd, res_full, main = 'Res vs weekly wage in whole sales')

par(mfrow = c(3,3))
plot(wfir, res_full, main = 'Res vs weekly wage in finance')
plot(wser, res_full, main = 'Res vs weekly wage in service')
plot(wmfg, res_full, main = 'Res vs weekly wage in manufacturing')
plot(wfed, res_full, main = 'Res vs weekly wage of federal employees')
plot(wsta, res_full, main = 'Res vs weekly wage of state employees')
plot(wloc, res_full, main = 'Res vs weekly wage of local govern employees')
plot(mix, res_full, main = 'Res vs offence mix')
plot(pctymle, res_full, main = 'Res vs percentage of young males')

# transformation model 
library(MASS)
result = boxcox(y~ year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
                  wser + wmfg + wfed + wsta + wloc + mix + pctymle, lambda = seq(-2,1,by = 0.01))
trans = result$x[result$y == max(result$y)]
trans
new_y = y^0.5
trans_model = lm(new_y~ year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
                   wser + wmfg + wfed + wsta + wloc + mix + pctymle)
summary(trans_model)
res_trans = trans_model$residuals
fits_trans = trans_model$fitted.values
plot(fits_trans, res_trans, main = 'Res vs fits for transformation model')

qqnorm(res_trans)
qqline(res_trans)
shapiro.test(res_trans)
ncvTest(trans_model)
vif(trans_model)
#
############################################################################################### 
# All-possible-regressions
kk=20; n=length(y); y=new_y
models=matrix(F, 2^kk, kk)
dimnames(models)=list(NULL, c('year','prbarr','prbconv','prbpris','avgsen','polpc','density','taxpc','pctmin','wcon','wtuc','wtrd','wfir',
                                'wser','wmfg','wfed','wsta','wloc','mix','pctymle'))
row=0
for (a in c(F,T)){
  for (b in c(F,T)){
    for (c in c(F,T)){
      for (d in c(F,T)){
        for (e in c(F,T)){
          for (f in c(F,T)){
            for(g in c(F,T)){
              for (h in c(F,T)){
                for (i in c(F,T)){
                  for (j in c(F,T)){
                    for (k in c(F,T)){
                      for (l in c(F,T)){
                        for (m in c(F,T)){
                          for (nn in c(F,T)){
                            for (o in c(F,T)){
                              for (pp in c(F,T)){
                                for (q in c(F,T)){
                                  for (t in c(F,T)){
                                    for (x in c(F,T)){
                                      for (z in c(F,T)){
                                      
            
            row=row+1
            models[row, ]=c(a, b, c, d, e, f, g, h, i, j, k, l, m, nn, o, pp, q, t, x, z)
                                          }
                                         }
                                        }
                                      }
                                    }                   
                                  }
                                 }
                               }
                              }
                            }
                           }
                          }
                        }
                      }
                    }
                  }
                 }
               }
              }
             }


# look at first few rows
models[1:4, ]

# set up a matrix to hold the results, and compute the estimate of sigma^2
# for the full model (neede for Cp)
results=matrix(NA, 2^kk, 8)
dimnames(results)=list(NULL, c('p', 'R2', 'R2.adj', 'MSE', 'PRESS', 'AIC', 'BIC', 'Cp'))

# MSE for the full model
x = cbind(1,year,prbarr, prbconv, prbpris, avgsen, polpc, density, taxpc, pctmin, wcon, wtuc, wtrd, wfir, wser, wmfg, wfed, wsta, wloc, mix, pctymle )
MSE.full=(summary(trans_model)$sigma)^2

# fit all possible models
for (i in 1:(2^kk)){
  # pull out the ith row of models and append T for the intercept
  which=c(T,models[i,])
  # fit the model and compute the fit measures
  tmp=lm(y~x[,which]-1)
  p=sum(which)
  SST=(n-1)*var(y)
  MST=var(y)
  SSE=sum(tmp$residuals^2)
  MSE=SSE/(n-p)
  R2=1-SSE/SST
  R2.adj=1-MSE/MST
  hat=x[,which]%*%solve(t(x[,which])%*%x[,which])%*%t(x[,which])
  hi=diag(hat)
  res.PRESS=tmp$residuals/(1-hi)
  PRESS=sum(res.PRESS^2)
  AIC=n*log(SSE/n)+2*p
  BIC=n*log(SSE/n)+p*log(n)
  Cp=(SSE/MSE.full)-(n-2*p)
  
  # save the results
  results[i,1]=p
  results[i,2]=R2
  results[i,3]=R2.adj
  results[i,4]=MSE
  results[i,5]=PRESS
  results[i,6]=AIC
  results[i,7]=BIC
  results[i,8]=Cp
}

results

# summarize the results
p=results[,1]
R2=results[,2]
R2.adj=results[,3]
MSE=results[,4]
PRESS=results[,5]
AIC=results[,6]
BIC=results[,7]
Cp=results[,8]

# model with best R2.adj
index1=R2.adj==max(R2.adj)
(1:2^kk)[index1]
models[index1,]
which(models[index1,] == TRUE)
# model with best MSE
index2=MSE==min(MSE)
(1:2^kk)[index2]
models[index2,]
which(models[index2,] == TRUE)

# model with best PRESS
index3=PRESS==min(PRESS)
(1:2^kk)[index3]
models[index3,]
which(models[index3,] == TRUE)

# model with best AIC
index4=AIC==min(AIC)
(1:2^kk)[index4]
models[index4,]
which(models[index4,] == TRUE)

# model with best BIC
index5=BIC==min(BIC)
(1:2^kk)[index5]
models[index5,]
which(models[index5,] == TRUE)

# model with best Cp
index6=Cp==min(Cp)
(1:2^kk)[index6]
models[index6,]
which(models[index6,] == TRUE)
###############################################################################################

forw=step(lm(new_y~year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
               wser + wmfg + wfed + wsta + wloc + mix + pctymle), direction='forward')
summary(forw)

back=step(lm(new_y~year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
               wser + wmfg + wfed + wsta + wloc + mix + pctymle), direction='backward')
summary(back)

step=step(lm(new_y~year + prbarr + prbconv + prbpris + avgsen + polpc + density + taxpc + pctmin + wcon + wtuc + wtrd + wfir +
               wser + wmfg + wfed + wsta + wloc + mix + pctymle), direction='both')
summary(step)

## summarize the best models
# R2.adj, MSE
models[1046558,]
results[1046558,]
# PRESS, AIC, Cp, step, back
models[981022,]
results[981022,]
# BIC
models[948254,]
results[948254,]

# final model
#final=lm(new_y~ year + prbarr + prbconv + avgsen + polpc + density + taxpc + pctmin + wfir + wser + wfed + wsta + wloc + pctymle)
final=lm(new_y~ year + prbarr + prbconv + avgsen+ polpc + density + taxpc + pctmin+ wfed +  wsta + wloc + pctymle)
summary(final)

# plots 
final_res = final$residuals
final_fits = final$fitted.values
plot(final_fits, final_res, main = 'Residuals vs fitted values for final model')

# check for normality and constant variance
qqnorm(final_res)
qqline(final_res)
shapiro.test(final_res)
library(lmtest)
ncvTest(final)
bptest(final)

# check for multicollinearity
vif(final)

# Investigating outliers
# residual analysis
X=cbind(1,year,prbarr,prbconv,avgsen,polpc,density,taxpc,pctmin,wfir,wser,wfed,wsta,wloc,pctymle)

hat=X%*%solve(t(X)%*%X)%*%t(X)
lev=diag(hat)
press=final_res/(1-lev)

par(mfrow=c(2,2))
plot(final_res, press)
plot(final_res, lev); abline(h=2*14/n)
plot(final, which = 4)

cooks.distance(final)[1:10]
sum(cooks.distance(final) >1)




