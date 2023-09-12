#from https://rstudio-pubs-static.s3.amazonaws.com/9687_cc323b60e5d542449563ff1142163f05.html


library(R2jags)

# prepare data
dat<-read_csv("0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/Master_data.csv")

dat1<-dat[c(2, 3,4, 9, 15, 21, 27, 33)]%>%na.omit()


zst <- matrix(1, nrow = nrow(adjmat), ncol = ncol(adjmat))
inits <- function() {
  list(z = zst)
  
  
cova<-dat1[c(2:8)]
n_covs<-ncol(cova)
n_obs<-length(rownames(dat1))
lst_use <- list(resp=dat1$PIWOcount,cov=cova,nobs=n_obs,ncovs=n_covs)


# Parameters monitored
params <- c("alpha", "beta", "sigma")

# params <- c("psi1", "alpha.lphi", "beta.lphi", "alpha.lgamma", "beta.lgamma", 
#             "p", "z")


ni <- 2500
nt <- 2
nb <- 500
nc <- 3

# Call JAGS from R
outJAGS <- jags(data=lst_use, inits=NULL, params, "0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/piwo_mod_poi_simple.txt", n.chains = nc, n.thin = nt, 
                n.iter = ni, n.burnin = nb)

predictions <- predict(outJAGS, newdata = predictors, n.iter = ni, thin = nt)




moduse_poi[[w,e]]<-jags.model(file="0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/piwo_mod_poi_simple.txt",n.chain=nchains,data=lst_use)
print("Initializing intensity model (NegBin)")