
nCores <- detectCores() -1
library(foreach)
library(doParallel)
cl <- makeCluster(nCores)
clusterSetRNGStream(cl, 51)
registerDoParallel(cl)

library(mice)

imp_mice <-
  foreach(no = 1:nCores,
          .combine = ibind,
          .export = "mammalsleep",
          .packages = "mice") %dopar%
          {
            mice(mammalsleep, m = 10, printFlag = FALSE)
          }
stopCluster(cl)

# Complete data
complete(imp_mice)

imp_mice$method
imp_mice$data
imp_mice$m  etc.


library(parallel)
nCores <- detectCores() -1

cl <- makeCluster(nCores)
clusterSetRNGStream(cl, 51)
clusterExport(cl, "mammalsleep")
clusterEvalQ(cl, library(mice))

imp_pars <-
  parLapply(cl = cl, X = 1:nCores, fun = function(no){
    mice(mammalsleep, m = 10, printFlag = FALSE)
  })
stopCluster(cl)

# Complete data (doesn't work)
complete(imp_pars)


