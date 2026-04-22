#' PRIM
#'
#' @description
#' Regional descriptor method based on PRIM (Patient Rule Induction Method),
#' adapted to search for interpretable regional descriptors (IRDs).
#'
#' The method starts from a large box around `x_interest`, then iteratively
#' peels away regions to improve purity and pastes back regions as long as the
#' resulting box remains homogeneous. In this package, PRIM is adapted to return
#' a box that contains `x_interest` and aims for high coverage under full
#' precision.
#'
#' @references
#' Friedman, J. H., & Fisher, N. I. (1999).
#' Bump hunting in high-dimensional data.
#' *Statistics and Computing*, 9(2), 123–143.
#' \url{https://link.springer.com/article/10.1023/A:1008894516817}
#'
#' Dandl, S., Casalicchio, G., Bischl, B., & Bothmann, L. (2023).
#' Interpretable Regional Descriptors: Hyperbox-Based Local Explanations.
#' \url{https://arxiv.org/abs/2305.02780}
#'
#' @export
Prim = R6::R6Class("Prim", inherit = RegDescMethod,

  public = list(
    #' @description
    #' Creates a new `Prim` object.
    #'
    #' @param predictor (`iml::Predictor`) \cr
    #'   The object (created with `iml::Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param subbox_relsize (`numeric(1)`) \cr
    #'   Relative size used to define candidate subboxes during peeling and
    #'   pasting.
    #'   For numeric features, it determines the quantiles used for candidate
    #'   splits (peeling) and the number of observations added (pasting).
    #'   Smaller values lead to smaller candidate changes and larger values lead
    #'   to coarser steps.
    #'   Default is `0.05`.
    #' @param strategy (`character(1)`) \cr
    #'   Data source used during the search.
    #'   `"traindata"` uses the available training data.
    #'   `"sampled"` uses newly sampled points from the largest local box around
    #'   `x_interest`.
    #' @param num_sampled_points (`numeric(1)`) \cr
    #'   Number of sampled points used when `strategy = "sampled"`.
    #' @param quiet (`logical(1)`) \cr
    #'   Should progress messages be suppressed?
    #'
    #' @return A \link{RegDesc} object.
    #' @export
    initialize = function(predictor,
                          subbox_relsize = 0.05,
                          strategy = "traindata",
                          num_sampled_points = 500L,
                          quiet = FALSE) {
      # input checks
      super$initialize(predictor, quiet)
      checkmate::qassert(subbox_relsize, "N?(0,1)")
      checkmate::assert_character(strategy, len = 1L)
      checkmate::assert_choice(strategy, choices = c("traindata", "sampled"))

      # assign private attr
      private$subbox_relsize = subbox_relsize
      private$strategy = strategy
      if (strategy == "sampled") {
        assert_numeric(num_sampled_points, lower = 1, finite = TRUE, len = 1L, null.ok = FALSE)
        private$num_sampled_points = num_sampled_points
      }
    }),
  private = list(
    subbox_relsize = NULL,
    strategy = NULL,
    num_sampled_points = NULL,
    lookup_sizes = NULL,
    i = NULL,
    evalcurrent = NULL,
    box_largest = NULL,
    run = function(){

      # Get data (either training or newly sampled depending on strategy)
      if (is.null(private$obsdata)) {
        private$obsdata = sampling(predictor = private$predictor, x_interest = private$x_interest,
          fixed_features = private$fixed_features, desired_range = private$desired_range,
          param_set = private$param_set, num_sampled_points = private$num_sampled_points,
          strategy = private$strategy)
      }
      private$obsdata = rbind(private$x_interest, private$obsdata[, private$predictor$data$feature.names, with = FALSE])
      private$.calls_fhat = private$.calls_fhat + nrow(private$obsdata)
      private$obsdata = private$obsdata[, positive := predict_range(private$predictor, newdata = private$obsdata, range = private$desired_range)]

      # estimate largest box according to ice values
      # get ranges for features
      if (is.null(private$box_largest)) {
        private$box_largest = get_max_box(private$x_interest, private$fixed_features,
          predictor = private$predictor,
          param_set = private$param_set, desired_range = private$desired_range,
          resolution = 500L) # <FIXME:> is this a good parameter value??
      }

      assert_true(identify_in_box(private$box_largest, private$x_interest))

      # features to change
      vars_diff = setdiff(private$predictor$data$feature.names, private$fixed_features)

      # # define bounds of removed boxes using quantiles
      private$lookup_sizes = lapply(vars_diff, FUN = function(j) {
        ps = private$param_set$clone()$subset(j)
        if (ps$all_numeric) {
          (ps$upper - ps$lower)/(1/private$subbox_relsize)
        }
      })
      names(private$lookup_sizes) = vars_diff

      history = data.table()
      box_new = private$box_largest$clone()
      private$i = 0L

      ### PEELING ###

      heterogeneous = sum(private$obsdata$positive == 0) > 0
      # Main algorithm for removing boxes
      while (heterogeneous) {
        a = lapply(sample(vars_diff), FUN = function(j) {
          res = data.table(var = character(), lower = numeric(), upper = numeric(),
            val = character(), impurity = numeric(), coverage = numeric(), size = numeric())
          if (box_new$is_categ[[j]]) {
            res = data.table()
            for (cat in setdiff(box_new$levels[[j]], private$x_interest[[j]])) {
              subbox = update_box(current_box = box_new, j = j, lower = NULL,
                upper = NULL, val = setdiff(box_new$levels[[j]], cat),
                complement = FALSE)
              size = (1/private$param_set$nlevels[[j]])/private$subbox_relsize
              eval = private$evaluate_box(box = subbox)
              resrow = data.table(var = j, lower = NA, upper = NA, val = cat,
                impurity = eval["impurity"], coverage = eval["coverage"], size = size)
              res = rbind(res, resrow)
            }
          } else {

            # boxsubset = box_new$clone()$subset(setdiff(vars_diff, j))
            inbox = identify_in_box(box_new, private$obsdata)
            boxdata = copy(private$obsdata)[(inbox),]

            # get quantile
            lower = boxdata[, lapply(.SD, quantile, prob = private$subbox_relsize),  .SDcols = j][[1]]
            upper = boxdata[, lapply(.SD, quantile, prob = 1-private$subbox_relsize),  .SDcols = j][[1]]

            selection = c("lower", "upper")

            # get next larger/lower value
            idl = which(boxdata[[j]] > lower)
            if (length(idl) > 0) {
              lower = min(boxdata[idl, j, with = FALSE])
            } else {
              selection = selection[selection != "lower"]
            }
            idu = which(boxdata[[j]] < upper)
            if (length(idu) > 0) {
              upper = max(boxdata[idu, j, with = FALSE])
            } else {
              selection = selection[selection != "upper"]
            }

            # set manually to x_interest value such that x_interest is covered!
            lower = min(lower, private$x_interest[[j]])
            upper = max(upper, private$x_interest[[j]])

            for (l in selection) {
              bound = switch(l,
                "lower" = lower,
                "upper" = upper)
              if (l == "lower") {
                if (box_new$lower[[j]] == lower) {
                  next
                }
                subbox = update_box(current_box = box_new, j = j, lower = bound)
                size =  (subbox$lower[[j]] - box_new$lower[[j]])/private$lookup_sizes[[j]]
              } else {
                if (box_new$upper[[j]] == upper) {
                  next
                }
                subbox = update_box(current_box = box_new, j = j, upper = bound)
                size = (box_new$upper[[j]]- subbox$upper[[j]])/private$lookup_sizes[[j]]
              }
              eval = private$evaluate_box(subbox)
              resrow = data.table(var = j, lower = NA, upper = NA, val = NA,
                impurity = eval["impurity"], coverage = eval["coverage"], size = size)
              resrow[, (l) := bound]
              res = rbind(res, resrow)
            }
          }
          return(res)
        })
        res_table = rbindlist(a)

        if (nrow(res_table) == 0) break

        res_table$mode = "peeling"

        # #remove the categories with coverage == 0
       #  res_table = res_table[coverage != 0, ]

        # get best (low impurity, high coverage)
        best = res_table[order(impurity, -coverage)[1]]
        box_new = update_box(current_box = box_new, j = best$var, lower = best[["lower"]],
          upper = best[["upper"]], val = setdiff(box_new$levels[[best$var]], best[["val"]]), complement = FALSE)

        ## Save info in history
        history = rbind(history, best)
        private$i = private$i+1L
        if (!private$quiet) {
          message(paste("peeling iteration", private$i, "with peeling variable", best$var, "and impurity = ", best$impurity))
        }
        if (best$impurity == 0) {
          heterogeneous = FALSE
        }
      }

      private$i = 0L
      ### PASTING ###
      homogeneous = TRUE

      # Main algorithm for removing boxes
      while (homogeneous) {
        # evaluate current best box
        # private$evalcurrent = private$evaluate_box(box_new)

        a = lapply(sample(vars_diff), FUN = function(j) {
          res = data.table(var = character(), lower = numeric(), upper = numeric(),
            val = character(), impurity = numeric(), coverage = numeric(), size = numeric())
          if (box_new$is_categ[[j]]) {
            res = data.table()
            for (cat in setdiff(private$box_largest$levels[[j]], box_new$levels[[j]])) {
              subbox = update_box(current_box = box_new, j = j, lower = NULL,
                upper = NULL, val = c(box_new$levels[[j]], cat),
                complement = FALSE)
              size = (1/private$param_set$nlevels[[j]])/private$subbox_relsize
              eval = private$evaluate_box(box = subbox)
              resrow = data.table(var = j, lower = NA, upper = NA, val = cat,
                impurity = eval["impurity"], coverage = eval["coverage"], size = size)
              res = rbind(res, resrow)
            }
          } else {

            selection = vector()

            if (box_new$lower[[j]] != private$box_largest$lower[[j]]) selection = c(selection, "lower")
            if (box_new$upper[[j]] != private$box_largest$upper[[j]]) selection = c(selection, "upper")

            if (length(selection) > 0) {
              boxsubset = box_new$clone()$subset(setdiff(vars_diff, j))
              inbox = identify_in_box(boxsubset, private$obsdata)
              boxdata = copy(private$obsdata)[(inbox),]

              # number of observations to add
              num = max(round(sum(identify_in_box(box = box_new, data = private$obsdata))*private$subbox_relsize), 1)

              if ("lower" %in% selection) {
              # get next lower num observations
                boxdatalower = boxdata[which(boxdata[[j]] < box_new$lower[[j]]), ]
                if (nrow(boxdatalower) > 0) {
                  setorderv(boxdatalower, j, order = -1)
                  lower = min(head(boxdatalower, n = num)[[j]])
                } else {
                  selection = selection[selection != "lower"]
                }
              }

              if ("upper" %in% selection) {
                # get next upper num observations
                boxdataupper = boxdata[which(boxdata[[j]] > box_new$upper[[j]]), ]
                if (nrow(boxdataupper) > 0) {
                  setorderv(boxdataupper, j)
                  upper = max(head(boxdataupper, n = num)[[j]])
                } else {
                  selection = selection[selection != "upper"]
                }
              }

              for (l in selection) {
                bound = switch(l,
                  "lower" = lower,
                  "upper" = upper)
                if (l == "lower") {
                  if (box_new$lower[[j]] == lower) next
                  subbox = update_box(current_box = box_new, j = j, lower = bound)
                  size =  (box_new$lower[[j]] - subbox$lower[[j]])/private$lookup_sizes[[j]]
                } else {
                  if (box_new$upper[[j]] == upper) next
                  subbox = update_box(current_box = box_new, j = j, upper = bound)
                  size = (subbox$upper[[j]] - box_new$upper[[j]])/private$lookup_sizes[[j]]
                }
                eval = private$evaluate_box(subbox)
                resrow = data.table(var = j, lower = NA, upper = NA, val = NA,
                  impurity = eval["impurity"], coverage = eval["coverage"], size = size)
                resrow[, (l) := bound]
                res = rbind(res, resrow)
              }
            }

          }
          return(res)
        })

        res_table = rbindlist(a)

        # Stop when there are no candidates left
        if (nrow(res_table) == 0) break

        res_table$mode = "pasting"

        # #remove the categories with coverage == 0
        # res_table = res_table[coverage != 0, ]

        # get best (low impurity, high coverage)
        best = res_table[order(impurity, -coverage)[1]]

        # If best box is impure, then stop pasting!
        if (best$impurity > 0) {
          break
        }

        box_new = update_box(current_box = box_new, j = best$var, lower = best[["lower"]],
          upper = best[["upper"]], val = best[["val"]], complement = TRUE)

        ## Save info in history
        history = rbind(history, best)
        private$i = private$i+1L
        if (!private$quiet) {
          message(paste("pasting iteration", private$i, "with pasting variable", best$var,"and impurity = ", best$impurity))
        }
      }


      private$.history = history
      return(box_new)
    },
    evaluate_dataset = function(evaldt) {
      impurity = sum(evaldt$positive == 0)/nrow(evaldt)
      coverage = nrow(evaldt)/nrow(private$obsdata)
      return(c(impurity = impurity, coverage = coverage))
    },
    evaluate_box = function(box) {
      inbox = identify_in_box(box, private$obsdata[, !"positive"])
      evaldt = private$obsdata[inbox,]
      impurity = sum(evaldt$positive == 0)/sum(inbox)
      coverage = sum(inbox)/nrow(private$obsdata)

      return(c(impurity = impurity, coverage = coverage))
    },
    declutter_searchspace = function(searchspace, var) {
      s = searchspace[[var]]
      searchspace[[var]] = s[lapply(s, length) > 0]
      searchspace = searchspace[lapply(searchspace, length) > 0]
      return(searchspace)
    },
    print_parameters = function() {
      cat(" - subbox_relsize: ", private$subbox_relsize, "\n")
    },
    .get_parameters = function() {
      list(
        subbox_relsize = private$subbox_relsize
      )
    }
  )
)
