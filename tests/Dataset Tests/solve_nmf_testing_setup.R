library(tidyverse)
library(glue) # working with dynamic code
library(lubridate) # working with dates
library(readxl) # read excel files
library(tidymodels) # a framework for modelling in tidyverse
library(glmnet)    # for regularized regression
library(selectiveInference) # for inference
library(ranger)

setwd("C:/Users/andie/Documents/server")
# create_input <- function(tdm,
#                          vocab,
#                          topics,
#                          project = FALSE,
#                          proj_dim = NULL,
#                          covariates = NULL) {
#
#   ##### check basic input types
#   stopifnot(is.matrix(tdm))
#   stopifnot(is.character(vocab))
#   stopifnot(is.numeric(topics))
#   topics <- as.integer(topics)
#   stopifnot(is.logical(project))
#
#   ##### make sure the user doesn't specify any contradictory inputs
#   if (project == TRUE && is.null(proj_dim)) {
#     stop("No projection dimensions specified.")
#   }
#   if (project == TRUE && !(is.null(proj_dim))) {
#     proj_dim <- as.integer(proj_dim)
#     if (!(proj_dim > topics && proj_dim < ncol(tdm))) {
#       stop("Projection dimension must be between the number of topics and
#            the number of documents.")
#     }
#   }
#   if (project == FALSE) {
#     proj_dim <- NULL
#   }
#   if (!(is.null(covariates))) {
#     if (!(is.matrix(covariates))) {
#       stop("Covariates must be in the form of a matrix.")
#     }
#     if (nrow(covariates) < ncol(tdm)) {
#       stop("Design matrix must have at least as many
#            rows as there are documents.")
#     }
#   }
#   if (!(topics < ncol(tdm) && topics < nrow(tdm))) {
#     stop("Number of topics must be less than the both the
#          number of rows and columns in the TDM.")
#   }
#   if (length(vocab) != nrow(tdm)) {
#     stop("Vocab must be the same length as the number of rows in the TDM.")
#   }
#
#   ##### return object of class nmf_input
#   to_return <- list(tdm = tdm,
#                     vocab = vocab,
#                     topics = topics,
#                     project = project,
#                     proj_dim = proj_dim,
#                     covariates = covariates)
#   class(to_return) <- "nmf_input"
#   return(to_return)
# }



impute_quarterly_data <- function(date_col, quart_col){

  for(i in 1:length(date_col)){
    if(is.na(quart_col[i])){
      quart_year <- year(date_col[i])
      quart_month <- month(date_col[i])
      if(quart_month %in% c(1, 2)){
        if (!is.na(which(date_col == ym(paste(quart_year, "3", sep ="-"))))){
          quart_col[i] = quart_col[which(date_col == ym(paste(quart_year, "3", sep ="-")))]
        }else{NA}
      }else if(quart_month %in% c(4, 5)){
        if (!is.na(which(date_col == ym(paste(quart_year, "6", sep ="-"))))){
          quart_col[i] = quart_col[which(date_col == ym(paste(quart_year, "6", sep ="-")))]
        }else{NA}
      }else if(quart_month %in% c(7, 8)){
        if (!is.na(which(date_col == ym(paste(quart_year, "9", sep ="-"))))){
          quart_col[i] = quart_col[which(date_col == ym(paste(quart_year, "9", sep ="-")))]
        }else{NA}
      } else if(quart_month %in% c(10, 11)){
        if (!is.na(which(date_col == ym(paste(quart_year, "12", sep ="-"))))){
          quart_col[i] = quart_col[which(date_col == ym(paste(quart_year, "12", sep ="-")))]
        }else{NA}
      }
    }
  }
  return(quart_col)
}

G7_countries <- c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States")


covariates_file <- "macro_control_vars.xlsx"
countries2use_abbr <- c("CA", "FR", "DE", "IT", "JP", "GB", "US")


realGDP <- read_xlsx(covariates_file, sheet = "realGDP") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

creditgrowth <- read_xlsx(covariates_file, sheet = "creditgrowth") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

headlineInflation <- read_xlsx(covariates_file, sheet = "headlineInflation") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

policyRate <- read_xlsx(covariates_file, sheet = "policyRate") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

usdExchangeRate <- read_xlsx(covariates_file, sheet = "usdExchangeRate") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

stock <- read_xlsx(covariates_file, sheet = "stock") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

vol <- read_xlsx(covariates_file, sheet = "vol") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

FCI <- read_xlsx(covariates_file, sheet = "FCI") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))

QEQT <- read_xlsx(covariates_file, sheet = "QE&QT") |>
  dplyr::select(contains(c(countries2use_abbr, "...1"))) |>
  rename("date" = `...1`) |> mutate(date = ym(paste(year(date), month(date), sep = "-"))) |>
  mutate(dates_in_use_decimal = decimal_date(date))
colnames(QEQT)[-c(which(colnames(QEQT) == "date"), which(colnames(QEQT) == "dates_in_use_decimal"))] <- str_c("QEQT_", colnames(QEQT)[-c(which(colnames(QEQT) == "date"), which(colnames(QEQT) == "dates_in_use_decimal"))])

covariates <- full_join(realGDP, creditgrowth) |>
  full_join(headlineInflation) |>
  full_join(policyRate) |>
  full_join(usdExchangeRate) |>
  full_join(stock) |>
  full_join(vol) |>
  full_join(FCI) |>
  full_join(QEQT) |>
  dplyr::select(contains("date") | tidyselect::ends_with(c(countries2use_abbr)))

all_speeches <- read.csv("speeches-g20bis-meta.csv")
all_speeches <- all_speeches |> mutate(date = date |> ymd() |> floor_date(unit = "month")) |>
  mutate(dates_in_use_decimal = decimal_date(date))
merged_speech_covariate <- all_speeches |>
  #dplyr::select(-c("author", "institution", "country")) |>
  left_join(covariates) |>
  filter(dates_in_use_decimal >= 2008)
merged_speech_covariate <- merged_speech_covariate |> column_to_rownames("doc")

country_covariates <- merged_speech_covariate |>
  dplyr::select(country) |>
  #column_to_rownames("doc") |>
  filter(country %in% G7_countries) |>
  #rowwise() |>
  mutate(Canada = ifelse(country == "Canada", 1, 0)) |>
  mutate(France = ifelse(country == "France", 1, 0)) |>
  mutate(Germany = ifelse(country == "Germany", 1, 0)) |>
  mutate(Italy = ifelse(country == "Italy", 1, 0)) |>
  mutate(Japan = ifelse(country == "Japan", 1, 0)) |>
  mutate(United_Kingdom = ifelse(country == "United Kingdom", 1, 0)) |>
  mutate(United_States = ifelse(country == "United States", 1, 0)) |>
  dplyr::select(-country) |>
  as.matrix()



data_tdm <- readRDS("speeches-g20bis-tdm.rds")
data_tdm <- data_tdm[, rownames(country_covariates)]
input <- create_input(tdm = data_tdm,
                      vocab = rownames(data_tdm),
                      topics = 40,
                      covariates = country_covariates)


# testing just year
# mat_speech_covs <- merged_speech_covariate |>
#   mutate(dates_in_use_decimal = floor(dates_in_use_decimal)) |>
#   dplyr::select(dates_in_use_decimal) |>
#   as.matrix()

# user_covariates <- mat_speech_covs

# solve_nmf(input = input, user_anchors = NULL, user_covariates = user_covariates)

user_anchors <- c("brexit", "climat", "covid", "crisi", #"digital_currency",
                  "inflat", #"interest_rate", "macroprudential_policy",
                  #"monetary_policy",
                  "ukrain")

#### No covariate Impact ####
nmf_country_no_covs <- solve_nmf(input,
                                 user_anchors = user_anchors,
                                 covariate_impact = "none")

#### Gamma Varying Only ####
nmf_country_gamma <- solve_nmf(input,
                               user_anchors = user_anchors,
                               covariate_impact = "gamma")

#### Both Vary ####
nmf_country_both <- solve_nmf(input,
                              user_anchors = user_anchors,
                              covariate_impact = "both")


#### comparison of top words ####
none_words <- print_top_words(nmf_country_no_covs)
both_words <- cov_print_top_words(nmf_country_both)
gamma_words <- cov_print_top_words(nmf_country_gamma)

selected_anchors <- nmf_country_no_covs$anchors

inflat_gamma_compare_top_words <- data.frame("No_covariate" = none_words$inflat,
                                     "Gamma_only:Canada" = gamma_words$Canada$inflat,
                                     "Gamma_only:France" = gamma_words$France$inflat,
                                     "Gamma_only:Germany" = gamma_words$Germany$inflat,
                                     "Gamma_only:Italy" = gamma_words$Italy$inflat,
                                     "Gamma_only:Japan" = gamma_words$Japan$inflat,
                                     "Gamma_only:United_Kingdom" = gamma_words$United_Kingdom$inflat,
                                     "Gamma_only:United_States" = gamma_words$United_States$inflat)

inflat_both_compare_top_words <- data.frame("No_covariate" = none_words$inflat,
                                             "Both:Canada" = both_words$Canada$inflat,
                                             "Both:France" = both_words$France$inflat,
                                             "Both:Germany" = both_words$Germany$inflat,
                                             "Both:Italy" = both_words$Italy$inflat,
                                             "Both:Japan" = both_words$Japan$inflat,
                                             "Both:United_Kingdom" = both_words$United_Kingdom$inflat,
                                             "Both:United_States" = both_words$United_States$inflat)

get_top_word_comparison_gamma <- function(anchor_term){
  eval(parse(text = glue("gamma_compare_top_words <- data.frame(\"No_covariate\" = none_words${anchor_term},
                                               \"Gamma_only:Canada\" = gamma_words$Canada${anchor_term},
                                               \"Gamma_only:France\" = gamma_words$France${anchor_term},
                                               \"Gamma_only:Germany\" = gamma_words$Germany${anchor_term},
                                               \"Gamma_only:Italy\" = gamma_words$Italy${anchor_term},
                                               \"Gamma_only:Japan\" = gamma_words$Japan${anchor_term},
                                               \"Gamma_only:United_Kingdom\" = gamma_words$United_Kingdom${anchor_term},
                                               \"Gamma_only:United_States\" = gamma_words$United_States${anchor_term})")))

  return(gamma_compare_top_words)
}
get_top_word_comparison_both <- function(anchor_term){
  eval(parse(text = glue("both_compare_top_words <- data.frame(\"No_covariate\" = none_words${anchor_term},
                                               \"Both_only:Canada\" = both_words$Canada${anchor_term},
                                               \"Both_only:France\" = both_words$France${anchor_term},
                                               \"Both_only:Germany\" = both_words$Germany${anchor_term},
                                               \"Both_only:Italy\" = both_words$Italy${anchor_term},
                                               \"Both_only:Japan\" = both_words$Japan${anchor_term},
                                               \"Both_only:United_Kingdom\" = both_words$United_Kingdom${anchor_term},
                                               \"Both_only:United_States\" = both_words$United_States${anchor_term})")))
  return(both_compare_top_words)
}

get_top_word_comparison_gamma("brexit")
get_top_word_comparison_gamma("euro")

get_top_word_comparison_both("brexit")
get_top_word_comparison_both("euro")


#### Lambdas #####
none_lambda <- get_lambda(nmf_country_no_covs)
both_lambda <- cov_get_lambda(nmf_country_both)
gamma_lambda <- cov_get_lambda(nmf_country_gamma)

compare_lambda <- none_lambda
colnames(compare_lambda) <- c("anchors", "No_covariates")
gamma_compare_lambda <- gamma_lambda$Canada
colnames(gamma_compare_lambda) <- c("anchors", "Gamma_only")

both_vary_Canada <- both_lambda$Canada
colnames(both_vary_Canada) <- c("anchors", "Both_vary: Canada")

both_vary_France <- both_lambda$France
colnames(both_vary_France) <- c("anchors", "Both_vary: France")

both_vary_Germany <- both_lambda$Germany
colnames(both_vary_Germany) <- c("anchors", "Both_vary: Germany")

both_vary_Italy <- both_lambda$Italy
colnames(both_vary_Italy) <- c("anchors", "Both_vary: Italy")

both_vary_Japan <- both_lambda$Japan
colnames(both_vary_Japan) <- c("anchors", "Both_vary: Japan")

both_vary_United_Kingdom <- both_lambda$United_Kingdom
colnames(both_vary_United_Kingdom) <- c("anchors", "Both_vary: United_Kingdom")

both_vary_United_States <- both_lambda$United_States
colnames(both_vary_United_States) <- c("anchors", "Both_vary: United_States")

compare_lambda <- compare_lambda |>
  full_join(gamma_compare_lambda, by = "anchors") |>
  full_join(both_vary_Canada, by = "anchors") |>
  full_join(both_vary_France, by = "anchors") |>
  full_join(both_vary_Germany, by = "anchors") |>
  full_join(both_vary_Italy, by = "anchors") |>
  full_join(both_vary_Japan, by = "anchors") |>
  full_join(both_vary_United_Kingdom, by = "anchors") |>
  full_join(both_vary_United_States, by = "anchors")
