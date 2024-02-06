library(unmarked)

#Detection/non-detection data in named list
y_list <- list(jackrabbit = JackDetect[["detection_history"]],
               cottontail = CottontDetect[["detection_history"]])

#detection covariates

#site covariates

# combine data into an unmarkedFrameOccuMulti object:
msom_data <- unmarkedFrameOccuMulti(y = y_list,
                                    siteCovs = occ_covs,
                                    obsCovs = det_list)

#null model, intercept only model assuming independence
null <- occuMulti(detformulas = c('~1', '~1', '~1'),
                   stateformulas = c('~1', '~1', '~1'),
                   maxOrder = 1,
                   data = msom_data)
summary(null)

#null model, intercept only model assuming dependence
fit_2 <- occuMulti(detformulas = c('~1', '~1', '~1'),
                   stateformulas = c('~1', '~1', '~1',
                                     '~1', '~1', '~1'),
                   maxOrder = 2,
                   data = msom_data)
summary(fit_2)