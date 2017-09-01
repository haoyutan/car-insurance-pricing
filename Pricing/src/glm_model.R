pricing.glm.times.train <-
function(feature, times, feature.include, distribution)
{
  feature.new <- pricing.glm.feature.preprocess(feature)

  data.times <- feature.new[, feature.include]
  data.times$Y_TIMES <- times

  if (distribution == 'Poisson') {
    model.times <- glm(Y_TIMES ~ ., data = data.times,
                        family = poisson(link = 'log'))
  } else {
    stop("Error: unsupported distribution.")
  }

  model.times
}


pricing.glm.times.predict <-
function(model.times, feature)
{
  feature.new <- pricing.glm.feature.preprocess(feature)
  pred.times <- predict(model.times, feature.new, type = 'response')
}


pricing.glm.avgpay.train <-
function(feature, avgpay, feature.include, distribution)
{
  feature.new <- pricing.glm.feature.preprocess(feature)

  data.avgpay <- feature.new[, feature.include]
  data.avgpay$Y_AVGPAY <- avgpay

  if (distribution == 'Gamma') {
    model.avgpay <- glm(Y_AVGPAY ~ ., data = data.avgpay,
                        family = Gamma(link = 'log'))
  } else if (distribution == 'inverse.gaussian'){
    model.avgpay <- glm(Y_AVGPAY ~ ., data = data.avgpay,
                        family = inverse.gaussian(link = 'log'))
  } else {
    stop("Error: unsupported distribution.")
  }

  model.avgpay
}


pricing.glm.avgpay.predict <-
function(model.avgpay, feature)
{
  feature.new <- pricing.glm.feature.preprocess(feature)
  pred.avgpay <- predict(model.avgpay, feature.new, type = 'response')
}


pricing.glm.feature.preprocess <-
function(feature)
{
  feature.new <- feature

  # relevel all the factors such that the base level of each factor
  #   corresponds to the largest group
  for(i in colnames(feature)) {
    if (!is.factor(feature[, i]))
      next
    ref <- which.max(table(feature[, i]))
    feature.new[, i] <- relevel(feature[, i], ref[[1]])
  }

  feature.new
}
