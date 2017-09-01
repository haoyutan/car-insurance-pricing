demo.main <-
function(base.dir = './')
{
  # load raw data from file
  data.raw <- read.csv(file.path(base.dir, 'data/ht-d1-v0.3.csv.gz'))

  # load feature extraction functions
  source(file.path(base.dir, 'src/data_preparation.R'))

  # perform data cleaning, feature extraction, etc.
  data <- pricing.prepare.data(data.raw)

  # load glm model functions and their dependencies
  source(file.path(base.dir, 'src/utils.R'))
  source(file.path(base.dir, 'src/glm_model.R'))

  # train the model for estimating number of incidents
  model.times <- pricing.glm.times.train(data$feature, data$times)

  # predict the times using model.times
  pred.times <- pricing.glm.times.predict(model.times, data$feature)

  # train the model for estimating average payment
  model.avgpay <- pricing.glm.avgpay.train(data$feature, data$avgpay)

  # predict avarage payment using model.avgpay
  pred.avgpay <- pricing.glm.avgpay.predict(model.avgpay, data$feature)

  # return as much information as possible
  demo.info <- list(
    data.raw               = data.raw,
    data.ready             = data,
    model.times            = model.times,
    pred.times             = pred.times,
    model.avgpay           = model.avgpay,
    pred.avgpay            = pred.avgpay
  )
  invisible(demo.info)
}
