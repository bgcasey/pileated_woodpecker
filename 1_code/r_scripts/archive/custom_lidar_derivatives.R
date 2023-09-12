
# custom metrics

library(lidR)
# library(moments) # for skewness, kurtosis, and entropy

##Define functions for metrics
f <- function(z) {
  list(
    zmean = mean(z), # mean height
    zsd = sd(z), # standard deviation of height
    # zcv = sd(z)/mean(z)*100, 
    zmax = max(z), # max height
    # zskew = skewness(z),
    # zkurt = kurtosis(z),
    # zentropy = entropy(z),
    zq50 = quantile(z, 0.5), #50th percentile of height
    zq95 = quantile(z, 0.95), #95th percentile of height
    pzabovezmean = sum(z>mean(z))/length(z)*100, # percent of height returns above mean height
    pzabovez3 = sum(z>3)/length(z)*100, # percent of returns greater than 3 m
    pz_0_to_1 = sum(0<z & z<=1)/length(z)*100, #proportion between 0 and 1 m
    # pz_0_to_3 = sum(0<=z & z<=3)/length(z)*100, #proportion between 0 and 3 m
    pz_1_to_3 = sum(1<z & z<=3)/length(z)*100 # proportion between 1 and 3 m
    # pz_3_to_5 = sum(3<z & z<=5)/length(z)*100 # proportion between 3 and 5 m
  )
}

m <- pixel_metrics(normalized_vegetation_las, func = ~f(Z), res=1)

