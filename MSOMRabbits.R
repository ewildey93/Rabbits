library(unmarked)

#find rows with no detction info
NArows <- which(apply(CottonDetect2, 1, function(x) all(is.na(x))))
rownames(CottonDetect2)[NArows]
#Detection/non-detection data in named list
y_list <- list(jackrabbit = JackDetect2,
               cottontail = CottonDetect2)

#observation covariates
det_list <- precip5
#site covariates
CoVs <- readRDS("./CoVsRabbitCo.rds")



# combine data into an unmarkedFrameOccuMulti object:
msom_data <- unmarkedFrameOccuMulti(y = y_list,
                                    siteCovs = CoVs)

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
                   stateformulas = c('~lc100.Forest', '~lc100.Shrub', '~Length150'),
                   maxOrder = 2,
                   data = msom_data)
summary(fit_3)
