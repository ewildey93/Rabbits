library(unmarked)
library(ggplot2)
#find rows with no detction info
NArows <- which(apply(CottonDetect2, 1, function(x) all(is.na(x))))
rownames(CottonDetect2)[NArows]
#Detection/non-detection data in named list
y_list <- list(jackrabbit = JackDetect2[-NArows,],
               cottontail = CottonDetect2[-NArows,])

#observation covariates
det_list <- precip5
#site covariates
CoVs <- readRDS("./CoVsRabbitCo.rds")
Covs2 <- CoVs[!CoVs$Camera == "BUSH1" & !CoVs$Camera == "BUSH28",]
Covs2$naiveoccuC <- rowMeans(CottonDetect2[-NArows,], na.rm=T)
Covs2$naiveoccuJ <- rowMeans(JackDetect2[-NArows,], na.rm=T)

colnames(Covs2)
ggplot(Covs2, aes(x=Brand, y=naiveoccuC)) + geom_point()
ggplot(Covs2, aes(x=Brand, y=naiveoccuJ)) + geom_point()

Covs3 <- Covs2
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






