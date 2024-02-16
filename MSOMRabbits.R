library(unmarked)
library(ggplot2)
#find rows with no detction info
NArows <- which(apply(GreyDetect2, 1, function(x) all(is.na(x))))
rownames(GreyDetect2)[NArows]
#Detection/non-detection data in named list
y_list <- list(GreyFox = GreyDetect2, #[-NArows,]
               Coyote = CoyoteDetect2) #[-NArows,]

saveRDS(y_list,"./y_list.rds")
y_list <- readRDS("./y_list.rds")
#observation covariates
det_list <- precip5
#site covariates
CoVs <- readRDS("./CoVsRabbitCo.rds")
which(GreyDetect2[NArows,])
which(RedDetect2[NArows,])
which(CoyoteDetect2[NArows,])
Covs2 <- CoVs[!CoVs$Camera == "BUSH1" & !CoVs$Camera == "BUSH28",]
Covs2$naiveoccuC <- rowSums(CottonDetect2[-NArows,], na.rm=T)
Covs2$naiveoccuJ <- rowSums(JackDetect2[-NArows,], na.rm=T)
Covs2$naiveoccuC <- ifelse(Covs2$naiveoccuC > 0 , 1, 0)
Covs2$naiveoccuJ <- ifelse(Covs2$naiveoccuJ > 0 , 1, 0)

colnames(Covs2)
ggplot(Covs2, aes(x=Dist2Trail, y=naiveoccuC)) + geom_point()
ggplot(Covs2, aes(x=Dist2Trail, y=naiveoccuJ)) + geom_point()

Covs3 <- CoVs
scaleind <- sapply(Covs3, is.numeric)
Covs3[scaleind] <- lapply(Covs3[scaleind], scale)



# combine data into an unmarkedFrameOccuMulti object:
msom_data <- unmarkedFrameOccuMulti(y = y_list,
                                    siteCovs = Covs3)

#null model, intercept only model assuming independence
null <- occuMulti(detformulas = c('~1', '~1'),
                   stateformulas = c('~1', '~1'),
                   maxOrder = 1,
                   data = msom_data)
summary(null)

#null model, intercept only model assuming dependence
fit_2 <- occuMulti(detformulas = c('~1', '~1'),
                   stateformulas = c('~1', '~1','~1'),
                   maxOrder = 2,
                   data = msom_data)
summary(fit_2)


#include some covariates
fit_3 <- occuMulti(detformulas = c('~Brand', '~Brand'),
                   stateformulas = c('~Dist2Trail', '~Dist2Trail', '~LengthGrid'),
                   maxOrder = 2,
                   data = msom_data)
summary(fit_3)


#
fit_4 <- occuMulti(detformulas = c('~Brand + slope100', '~Brand + slope100'),
                   stateformulas = c('~LengthGrid', '~LengthGrid', '~lc250.Shrub'),
                   maxOrder = 2,
                   data = msom_data)
summary(fit_4)






