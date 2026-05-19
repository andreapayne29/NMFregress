## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 9, fig.height = 6)

## ----message = FALSE----------------------------------------------------------
library(NMFregress)

## -----------------------------------------------------------------------------
head(acts)
tail(acts)

## -----------------------------------------------------------------------------
dim(Romeo_and_Juliet_tdm)
# a block from the TDM
Romeo_and_Juliet_tdm[312:318, 184:193]

## -----------------------------------------------------------------------------

my_input <- create_input(Romeo_and_Juliet_tdm,
                         vocab = rownames(Romeo_and_Juliet_tdm),
                         covariates = acts,
                         topics = 25)
my_input |> names()

## ----results="hide"-----------------------------------------------------------
my_output <- solve_nmf(my_input)

## -----------------------------------------------------------------------------
names(my_output)
class(my_output)

## -----------------------------------------------------------------------------
print_top_words(my_output, n = 10)

## -----------------------------------------------------------------------------
my_input2 <- create_input(Romeo_and_Juliet_tdm,
                          vocab = rownames(Romeo_and_Juliet_tdm),
                          covariates = acts,
                          topics = 25)
my_output2 <- solve_nmf(my_input2)

## -----------------------------------------------------------------------------
get_reconstruction_error(my_output, my_input)

## ----message = FALSE, warning=FALSE, results = FALSE--------------------------
OLS_bootstrap <- boot_reg(my_output, samples = 1000, model = "OLS")

# Test the input of a subset of topics

## ----message = FALSE----------------------------------------------------------

# plot the raw OLS coefficients:

brett_plot(OLS_bootstrap, "juliet")
brett_plot(OLS_bootstrap, "dead")

# or plot the model fit by including
# new data at which to evaluate the OLS model:
newdata <- my_output$covariates |> unique()
# rownames are used as labels for the new data values for the plots when
# new data is included.
rownames(newdata) <- c("act1", "act2", "act3", "act4", "act5")


brett_plot(OLS_bootstrap, "juliet", newdata)
brett_plot(OLS_bootstrap, "dead", newdata)


bootstrap_error_bars(OLS_bootstrap,
                     topicvector = c("dead")) |>
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~round(., digits = 4))
    )


## ----message = FALSE, warning=FALSE, results = FALSE--------------------------

# just the model without bootstrap
beta_asymptotic <- get_regression_coefs(my_output,
                                        model = "BETA",
                                        return_just_coefs = FALSE,
                                        topics = c("night",
                                                   "love",
                                                   "juliet",
                                                   "death",
                                                   "dead"))

# using bootstrap for the mean of Beta regression is slow
# change the 100 samples to something slower (and more reasonable) like 5000+.
# If you want fast, then use the asymptotic MLE tools.
beta_boot <- boot_reg(my_output,
                      samples = 100, model = "BETA",
                      return_just_coefs = TRUE,
                      topics = c("night",
                                 "love",
                                 "juliet",
                                 "death",
                                 "dead"))

brett_plot(beta_asymptotic, "juliet")
brett_plot(beta_asymptotic, "dead")

# or include new observations for the model:
newdata <- my_output$covariates |> unique()
rownames(newdata) <- c("act1",
                       "act2",
                       "act3",
                       "act4",
                       "act5")

brett_plot(beta_asymptotic,
           topic = "juliet",
           newdata = newdata)
brett_plot(beta_asymptotic,
           topic = "dead",
           newdata = newdata)

brett_plot(beta_boot, "juliet")
brett_plot(beta_boot, "dead")

# or include new observations for the model:
brett_plot(beta_boot, "juliet", newdata)
brett_plot(beta_boot, "dead", newdata)

## -----------------------------------------------------------------------------
beta_asymptotic$death |> summary()

## -----------------------------------------------------------------------------
beta_asymptotic$juliet |> summary()
beta_asymptotic$death |> summary()

## ----message = FALSE, warning = FALSE, results = FALSE------------------------
# Each document is a line from the play,
# so let's define line number as the fraction of the way through the
# particular act.
#
my_output$covariates <-  matrix( acts_continuous,
                                 ncol = 1,
                                 dimnames = list(NULL,"acts"))
colnames(my_output$covariates) <- "acts"
beta_gam <- get_regression_coefs(output = my_output,
                                 model =  "GAM",
                                 return_just_coefs = FALSE,
                                 topics = c("night",
                                            "love",
                                            "juliet",
                                            "death",
                                            "dead"))
brett_plot(beta_gam,
           topic = "death",
           newdata = data.frame(acts = 1:5))
brett_plot(beta_gam,
           topic = "juliet",
           newdata = data.frame(acts = 1:5))

