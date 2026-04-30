# --------------------------------------------------------------------------
#                 Interpretable Regional Descriptors (IRD)
#                Brief comparison on the German Credit example
# --------------------------------------------------------------------------

# This script uses the same data, x_interest, model and desired range as
# `inst/examples/credit_example.R`. It compares PRIM, MAIRE and MaxBox before
# and after post-processing in terms of runtime, impurity, coverage and the
# number of model-evaluation calls recorded by the package.

# --- setup ---------------------------------------------------------------

library("mlr3")
library("mlr3learners")
library("iml")
library("mlr3pipelines")
library("devtools")
load_all()

desired_range = c(0.3, 0.6)
fit_seed = 12005L
evaluation_seed = 12005L
evaluation_n = 1000L
coverage_seed = 12005L
coverage_n = 5000L

# --- load and prepare data ----------------------------------------------

credit = read.csv(
  "inst/examples/data/german_credit_data.csv",
  row.names = 1,
  stringsAsFactors = TRUE
)

credit = na.omit(credit)

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

names(credit) = tolower(names(credit))
credit = droplevels.data.frame(credit)

credit$saving.accounts = as.ordered(credit$saving.accounts)
credit$checking.account = as.ordered(credit$checking.account)

# --- define observation of interest -------------------------------------

x_interest = credit[1, ]
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

set.seed(fit_seed)
mod$train(task)

pred = Predictor$new(
  model = mod,
  data = credit,
  y = "risk",
  type = "classification",
  class = "good"
)

# ---------------------------------------------------------------------------
#      Comparison helpers!
# ---------------------------------------------------------------------------

method_specs = list(
  PRIM = list(
    method = "prim",
    method_args = list(quiet = TRUE)
  ),
  Maire = list(
    method = "maire",
    method_args = list(
      num_of_iterations = 100L,
      convergence = TRUE,
      quiet = TRUE,
      strategy = "traindata"
    )
  ),
  MaxBox = list(
    method = "maxbox",
    method_args = list(
      quiet = TRUE,
      strategy = "traindata"
    )
  )
)

postprocess_args = list(
  quiet = TRUE
)

sampled_coverage = function(result, n_samples, seed) {
  set.seed(seed)

  reference_box = get_max_box(
    x_interest = result$x_interest,
    fixed_features = result$fixed_features,
    predictor = result$predictor,
    desired_range = result$desired_range,
    param_set = make_param_set(result$predictor$data$X),
    resolution = 500L
  )

  sampled = SamplerUnif$new(reference_box)$sample(n = n_samples)$data
  sampled = reference_box$extra_trafo(x = sampled, predictor = result$predictor)

  mean(identify_in_box(result$box, sampled))
}

summarise_descriptor = function(method,
                                stage,
                                result,
                                runtime,
                                eval_n,
                                seed,
                                coverage_n,
                                coverage_seed,
                                total_runtime = NULL,
                                total_calls_fhat = NULL) {
  set.seed(seed)
  sampled_eval = result$evaluate(n_samples = eval_n)
  train_eval = result$evaluate_train()
  sampled_cov = sampled_coverage(
    result = result,
    n_samples = coverage_n,
    seed = coverage_seed
  )

  if (is.null(total_runtime)) {
    total_runtime = unname(runtime[["elapsed"]])
  }
  if (is.null(total_calls_fhat)) {
    total_calls_fhat = result$calls_fhat
  }

  data.frame(
    method = method,
    stage = stage,
    stage_time_sec = unname(runtime[["elapsed"]]),
    total_time_sec = total_runtime,
    impurity = unname(sampled_eval[["impurity"]]),
    dist = unname(sampled_eval[["dist"]]),
    train_precision = unname(train_eval[["precision"]]),
    train_coverage = unname(train_eval[["coverage"]]),
    sampled_coverage = sampled_cov,
    stage_calls_fhat = result$calls_fhat,
    total_calls_fhat = total_calls_fhat,
    stringsAsFactors = FALSE
  )
}

fit_method = function(label, spec) {
  set.seed(fit_seed)
  runtime = system.time({
    result = find_ird(
      predictor = pred,
      x_interest = x_interest,
      method = spec$method,
      desired_range = desired_range,
      method_args = spec$method_args
    )
  })

  list(
    result = result,
    runtime = runtime,
    summary = summarise_descriptor(
      method = label,
      stage = "initial",
      result = result,
      runtime = runtime,
      eval_n = evaluation_n,
      seed = evaluation_seed,
      coverage_n = coverage_n,
      coverage_seed = coverage_seed
    )
  )
}

postprocess_method = function(label, initial) {
  set.seed(fit_seed)
  runtime = system.time({
    post = postprocess_box(
      object = initial$result,
      desired_range = desired_range,
      method_args = postprocess_args
    )
  })

  list(
    result = post,
    summary = summarise_descriptor(
      method = label,
      stage = "postprocessed",
      result = post,
      runtime = runtime,
      eval_n = evaluation_n,
      seed = evaluation_seed,
      coverage_n = coverage_n,
      coverage_seed = coverage_seed,
      total_runtime = unname(initial$runtime[["elapsed"]]) +
        unname(runtime[["elapsed"]]),
      total_calls_fhat = initial$result$calls_fhat + post$calls_fhat
    )
  )
}

# ---------------------------------------------------------------------------
#             Run the initial method (PRIM, MAIRE, Maxbox)
# ---------------------------------------------------------------------------

initial_results = Map(fit_method, names(method_specs), method_specs)

initial_comparison = do.call(
  rbind,
  lapply(initial_results, function(x) x$summary)
)
rownames(initial_comparison) = NULL
cols_to_round = c(
  "stage_time_sec",
  "total_time_sec",
  "impurity",
  "dist",
  "train_precision",
  "train_coverage",
  "sampled_coverage"
)
initial_comparison[cols_to_round] = lapply(
  initial_comparison[cols_to_round],
  round,
  digits = 4L
)

initial_comparison

# ---------------------------------------------------------------------------
#             Run the post-processed box
# ---------------------------------------------------------------------------

post_results = Map(
  postprocess_method,
  names(initial_results),
  initial_results
)

postprocessed_comparison = do.call(
  rbind,
  lapply(post_results, function(x) x$summary)
)
rownames(postprocessed_comparison) = NULL
postprocessed_comparison[cols_to_round] = lapply(
  postprocessed_comparison[cols_to_round],
  round,
  digits = 4L
)

postprocessed_comparison

# ---------------------------------------------------------------------------
#             Compare the descriptor ranges side by side
# ---------------------------------------------------------------------------

descriptor_ranges = function(results, suffix = "") {
  boxes = lapply(results, function(x) describe_box(x$result$box))

  tables = Map(
    function(box, method) {
      out = data.frame(
        feature = box$id,
        range = box$range,
        stringsAsFactors = FALSE
      )
      names(out) = c("feature", paste0(method, suffix))
      out
    },
    boxes,
    names(results)
  )

  ranges = Reduce(
    function(left, right) merge(left, right, by = "feature", all = TRUE),
    tables
  )

  ranges[order(ranges$feature), ]
}

initial_descriptor_ranges = descriptor_ranges(
  results = initial_results
)
rownames(initial_descriptor_ranges) = NULL

initial_descriptor_ranges

postprocessed_descriptor_ranges = descriptor_ranges(
  results = post_results,
  suffix = "_postprocessed"
)
rownames(postprocessed_descriptor_ranges) = NULL

postprocessed_descriptor_ranges
