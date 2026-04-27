# --------------------------------------------------------------------------
#                 Interpretable Regional Descriptors (IRD)
#                       German Credit Aplication
# --------------------------------------------------------------------------

# This example illustrates how to compute interpretable regional descriptors
# (IRDs) for a credit-risk prediction using the user-facing sugar functions
# `find_ird()` and `postprocess_box()`.

# --- setup ---------------------------------------------------------------

library("mlr3")
library("mlr3learners")
library("iml")
library("mlr3pipelines")
library("devtools")
load_all()

# --- load and prepare data ----------------------------------------------

credit = read.csv(
  "inst/examples/data/german_credit_data.csv",
  row.names = 1,
  stringsAsFactors = TRUE
)

# remove incomplete rows
credit = na.omit(credit)

# merge rare categories
levels(credit$Purpose) = c(
  "others", "car", "others", "others",
  "furniture", "radio/TV", "others", "others"
)
levels(credit$Saving.accounts) = c("little", "moderate", "rich", "rich")
credit$Job = factor(
  credit$Job,
  levels = c(0, 1, 2, 3),
  labels = c("unskilled", "unskilled", "skilled", "highly skilled")
)

# standardize names and drop unused levels
names(credit) = tolower(names(credit))
credit = droplevels.data.frame(credit)

# ordered factors
credit$saving.accounts = as.ordered(credit$saving.accounts)
credit$checking.account = as.ordered(credit$checking.account)

# --- define observation of interest -------------------------------------

x_interest = credit[1,]

# use case: young woman applying for a car loan
x_interest$credit.amount = 4000L
x_interest$purpose = factor("car", levels = levels(credit$purpose))
x_interest$housing = factor("rent", levels = levels(credit$housing))
x_interest$duration = 30L

# --- train prediction model ---------------------------------------------

task = TaskClassif$new(
  id = "credit",
  backend = credit,
  target = "risk"
)

mod = lrn("classif.ranger", predict_type = "prob")

set.seed(12005L)
mod$train(task)

# --- create iml predictor ------------------------------------------------

pred = Predictor$new(
  model = mod,
  data = credit,
  y = "risk",
  type = "classification",
  class = "good"
)

pred$predict(x_interest)

# ------------------------------------------------------------------------
#                       Compute IRD with PRIM
# ------------------------------------------------------------------------

set.seed(12005L)
system.time({
  prim_res = find_ird(
    predictor = pred,
    x_interest = x_interest,
    method = "prim",
    desired_range = c(0.3, 0.6)
  )
})

prim_res
prim_res$evaluate()
prim_res$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

# inspect metadata stored on the descriptor
prim_res$history
prim_res$calls_fhat
prim_res$method_parameters

# --- post-process PRIM descriptor ---------------------------------------

prim_post = postprocess_box(
  object = prim_res,
  desired_range = c(0.3, 0.6)
)

prim_post
prim_post$evaluate()
prim_post$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

# ------------------------------------------------------------------------
#                       Compute IRD with MaxBox
# ------------------------------------------------------------------------

set.seed(12005L)
system.time({
  maxbox_res = find_ird(
    predictor = pred,
    x_interest = x_interest,
    method = "maxbox",
    desired_range = c(0.3, 0.6),
    method_args = list(
      quiet = FALSE,
      strategy = "traindata"
    )
  )
})

maxbox_res
maxbox_res$evaluate()
maxbox_res$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

# --- post-process MaxBox descriptor -------------------------------------

maxbox_post = postprocess_box(
  object = maxbox_res,
  desired_range = c(0.3, 0.6)
)

maxbox_post
maxbox_post$evaluate()
maxbox_post$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

# ------------------------------------------------------------------------
#                       Compute IRD with Maire
# ------------------------------------------------------------------------

set.seed(12005L)
system.time({
  maire_res = find_ird(
    predictor = pred,
    x_interest = x_interest,
    method = "maire",
    desired_range = c(0.3, 0.6),
    method_args = list(
      num_of_iterations = 100L,
      convergence = TRUE,
      quiet = FALSE,
      strategy = "traindata"
    )
  )
})

maire_res
maire_res$evaluate()
maire_res$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

# inspect metadata stored on the descriptor
maire_res$history
maire_res$calls_fhat
maire_res$method_parameters

# --- post-process MAIRE descriptor --------------------------------------

maire_post = postprocess_box(
  object = maire_res,
  desired_range = c(0, 0.5),
  method_args = list(
    subbox_relsize = 0.1
  )
)

maire_post
maire_post$evaluate()
maire_post$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

# ------------------------------------------------------------------------
#       Optional: Compute IRD with Anchors (manual instantiation)
# ------------------------------------------------------------------------

anch = Anchor$new(predictor = pred)

set.seed(12345L)
system.time({
  anchor_res = anch$find_box(
    x_interest = x_interest,
    desired_range = c(0, 0.5)
  )
})

anchor_res
anchor_res$evaluate()
anchor_res$plot_surface(
  feature_names = c("duration", "credit.amount"),
  surface = "range"
)

