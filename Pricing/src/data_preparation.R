pricing.prepare.data <-
function(data.raw, clean.func = pricing.default.data.clean)
{
  data.clean <- clean.func(data.raw)
  feature <- feature.extract.all(data.clean)

  if ('TIMES' %in% colnames(data.clean))
    output.times <- output.get.times(data.clean)
  else
    output.times <- NULL

  if (all(c('TIMES', 'SUMPAY') %in% colnames(data.clean)))
    output.avgpay <- output.get.avgpay(data.clean)
  else
    output.avgpay <- NULL

  list(feature = feature, times = output.times, avgpay = output.avgpay)
}


pricing.default.data.clean <-
function(data.raw)
{
  data.clean <- subset(data.raw, INSUREDMONTH == 12 & RISKCODE == 1224)
}


feature.extract.all <-
function(data)
{
  d <- data
  invisible(data.frame(
    SEQNO             = 1:nrow(d),
    POLICYNO          = feature.get.policyno(d),
    RISKCODE          = feature.get.riskcode(d),
    SEATCOUNT         = feature.get.seatcount(d),
    EXHAUSTSCALE      = feature.get.exhaustscale(d),
    NEWCARFLAG        = feature.get.newcarflag(d),
    RUNAREACODE       = feature.get.runareacode(d),
    COUNTRYNATURE     = feature.get.countrynature(d),
    USEYEARS          = feature.get.useyears(d),
    PURCHASEPRICE     = feature.get.purchaseprice(d),
    AGREEDRIVERFLAG   = feature.get.agreedriverflag(d),
    BUSINESSNATURE    = feature.get.businessnature(d),
    RENEWALFLAG       = feature.get.renewalflag(d),
    COMPULSORY        = feature.get.compulsory(d),
    BRANDNAME         = feature.get.brandname(d)
  ))
}


feature.get.policyno <-
function(d)
{
  as.character(d$POLICYNO)
}


feature.get.riskcode <-
function(d)
{
  as.factor(d$RISKCODE)
}


feature.get.seatcount <-
function(d, splitting = c(0, 5, 7, Inf))
{
  feature.helper.splitintofactor(d$SEATCOUNT, splitting)
}


feature.get.exhaustscale <-
function(d, splitting = c(-0.01, 1, 1.5, 1.6, 1.8, 2, 2.4, Inf))
#   function(d, splitting = c(-0.01, 1.6, 2.0, 2.4, Inf))
{
  feature.helper.splitintofactor(d$EXHAUSTSCALE, splitting)
}


feature.get.newcarflag <-
function(d)
{
  as.factor(d$NEWCARFLAG)
}


feature.get.runareacode <-
function(d)
{
  as.factor(d$RUNAREACODE)
}


feature.get.countrynature <-
function(d)
{
  z <- as.character(d$COUNTRYNATURE)
  z[z == ''] <- 'A'
  as.factor(z)
}


feature.get.useyears <-  
function(d, splitting = c(-0.01, 2, 4, 5, 6, 8, Inf))
{
  feature.helper.splitintofactor(d$USEYEARS, splitting)
}


feature.get.purchaseprice <-
function(d, splitting = 
           c(-0.01, 50000, 100000, 
             150000, 200000, 250000, 300000, 400000, 
             600000, 1000000, Inf))
{
  feature.helper.splitintofactor(d$PURCHASEPRICE, splitting)
}


feature.get.agreedriverflag <-
function(d)
{
  as.factor(d$AGREEDRIVERFLAG)
}


feature.get.businessnature <-
function(d, splitting = c(-0.01, 2, 3, Inf))
{
  feature.helper.splitintofactor(d$BUSINESSNATURE, splitting)
}


feature.get.renewalflag <-
function(d)
{
  z <- as.integer(d$RENEWALFLAG)
  z[z == 2] <- 1
  as.factor(z)
}


feature.get.compulsory <-
function(d)
{
  as.factor(d$COMPULSORY)
}


feature.get.brandname <-
function(d)
{
  z <- as.integer(d$BRANDNAME)
  stat <- table(z)
  nm.big <- as.integer(names(stat)[stat > 1000])
  z[!z %in% nm.big] <- length(nm.big) + 1
  as.factor(z)
}


feature.helper.splitintofactor <-
function(x, splitting)
{
  z <- rep(NA, length(x))
  for (i in 1:(length(splitting) - 1)) {
    z[splitting[i] < x & x <= splitting[i + 1]] <- i
  }
  as.factor(z)
}


output.get.times <-
function(d)
{
  ceiling(d$TIMES * 12 / d$INSUREDMONTH)
}


output.get.avgpay <-
function(d)
{
  z <- rep(NA, length(d$SUMPAY))
  idx <- d$TIMES > 0
  z[idx] <- d$SUMPAY[idx] / d$TIMES[idx]
  z
}
