# tf = tf$compat$v1
# tf$enable_eager_execution()

#' Maire
#'
#' @description
#' Regional descriptor method based on MAIRE (Model-Agnostic Interpretable
#' Rule Extraction), adapted to search for interpretable regional descriptors
#' (IRDs).
#'
#' MAIRE is a bottom-up method: it starts from a very small box around
#' `x_interest` and expands the box boundaries by gradient-based optimization.
#' In this package, the method is used to search for a box with high coverage
#' while keeping predictions within the desired range and ensuring that
#' `x_interest` remains inside the box. The original MAIRE approach is discussed
#' in Sharma et al. (2021), and its adaptation to IRDs is described in the IRD
#' paper.
#'
#' @references
#' Sharma, R., Reddy, N., Kamakshi, V., Krishnan, N. C., & Jain, S. (2021).
#' MAIRE - a model-agnostic interpretable rule extraction procedure for
#' explaining classifiers.
#' In Machine Learning and Knowledge Extraction, Lecture Notes in Computer
#' Science, 12844, 329--349.
#' \url{https://link.springer.com/chapter/10.1007/978-3-030-84060-0_21}
#'
#' Dandl, S., Casalicchio, G., Bischl, B., & Bothmann, L. (2023).
#' Interpretable Regional Descriptors: Hyperbox-Based Local Explanations.
#' \url{https://arxiv.org/abs/2305.02780}
#'
#' @export
Maire = R6::R6Class("Maire",
  inherit = RegDescMethod,
  public = list(
    #' @description
    #' Creates a new `Maire` object.
    #'
    #' @param predictor (`iml::Predictor`) \cr
    #'   The object (created with `iml::Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param num_of_iterations (`integer(1)`) \cr
    #'   Maximum number of optimization iterations. Default is `1000`.
    #' @param convergence (`logical(1)`) \cr
    #'   Whether `num_of_iterations` should be counted only after the precision
    #'   threshold has been violated for the first time.
    #'   This is used as a convergence-style stopping rule.
    #' @param strategy (`character(1)`) \cr
    #'   Data source used during optimization.
    #'   `"traindata"` uses the available training data.
    #'   `"sampled"` uses newly sampled points from the largest local box around
    #'   `x_interest`.
    #' @param num_sampled_points (`numeric(1)`) \cr
    #'   Number of sampled points used when `strategy = "sampled"`.
    #' @param lambda_val_1 (`numeric(1)`) \cr
    #'   Weight of the precision term in the loss function relative to coverage.
    #'   Larger values penalize imprecise boxes more strongly.
    #' @param lambda_val (`numeric(1)`) \cr
    #'   Weight of the locality constraint ensuring that `x_interest` stays
    #'   inside the box.
    #' @param threshold (`numeric(1)`) \cr
    #'   Minimum acceptable precision level used during optimization.
    #'   Default is `0.9999999`.
    #' @param c1 (`numeric(1)`) \cr
    #'   Shape parameter used in the smooth approximation of indicator functions.
    #' @param c2 (`numeric(1)`) \cr
    #'   Shape parameter controlling the steepness of the approximation.
    #' @param c3 (`numeric(1)`) \cr
    #'   Shape parameter used in the indicator approximation.
    #' @param c4 (`numeric(1)`) \cr
    #'   Shape parameter used in the indicator approximation.
    #' @param c5 (`numeric(1)`) \cr
    #'   Shape parameter used in the indicator approximation.
    #' @param cl (`numeric(1)`) \cr
    #'   Lower offset parameter used in the approximation.
    #' @param ch (`numeric(1)`) \cr
    #'   Upper threshold parameter used in the approximation.
    #' @param quiet (`logical(1)`) \cr
    #'   Should progress messages be suppressed?
    #'
    #' @details
    #' MAIRE relies on a differentiable approximation of box membership and
    #' optimizes box boundaries with ADAM in a transformed feature space.
    #' Continuous features are scaled, and categorical features are transformed
    #' internally before optimization.
    #'
    #' @return A \link{RegDesc} object.
    #' @importFrom keras k_placeholder
    #' @import tensorflow
    initialize = function(predictor,
                          num_of_iterations = 1000L,
                          convergence = FALSE,
                          strategy = "traindata",
                          num_sampled_points = NULL,
                          lambda_val_1 = 20,
                          lambda_val = 100L,
                          threshold = 0.9999999,
                          c1 = 0.4,
                          c2 = 15,
                          c3 = 0.6,
                          c4 = 0.5,
                          c5 = 0.5,
                          cl = 0.02,
                          ch = 0.82,
                          quiet = FALSE) {

      super$initialize(predictor, quiet)
      checkmate::assert_integerish(num_sampled_points, null.ok = TRUE)
      checkmate::assert_names(strategy, subset.of = c("traindata", "sampled"))
      checkmate::assert_character(strategy, len = 1L, null.ok = FALSE)
      if (strategy == "sampled" & is.null(num_sampled_points)) {
        stop("'num_sampled_points' needs to be specified")
      }

      # assign private attr
      private$dimension = private$predictor$data$n.features
      private$num_of_iterations = num_of_iterations
      private$convergence = convergence
      private$strategy = strategy
      private$num_sampled_points = num_sampled_points
      # private$lambda_val_1 = tf$cast(tf$constant(lambda_val_1), dtype = "float64")
      private$lambda_val_1 = as.numeric(lambda_val_1)
      # private$lambda_val = tf$cast(tf$constant(lambda_val),  dtype = "float64")
      private$lambda_val = as.numeric(lambda_val)
      # private$threshold = threshold
      private$threshold = as.numeric(threshold)
      # private$threshold_tensor = tf$cast(tf$constant(private$threshold), "float64")
      private$threshold_tensor = NULL # should be built in fit_explanation()
      private$c1 = c1
      private$c2 = c2
      private$c3 = c3
      private$c4 = c4
      private$c5 = c5
      private$cl = cl
      private$ch = ch
    }
  ),
  private = list(
    num_of_iterations = NULL,
    convergence = NULL,
    strategy = NULL,
    num_sampled_points = NULL,
    lambda_val_1 = NULL,
    lambda_val = NULL,
    threshold = NULL,
    threshold_tensor = NULL,
    c1 = NULL,
    c2 = NULL,
    c3 = NULL,
    c4 = NULL,
    c5 = NULL,
    cl = NULL,
    ch = NULL,
    dimension = NULL,
    categorylist = list(),
    expldata = NULL,
    explain_point = NULL,
    l_vec = NULL,
    u_vec = NULL,
    clip_l = NULL,
    clip_u = NULL,
    f_value_explain_point_int = NULL,
    f_value_explain_point = NULL,
    f_values_obsdata = NULL,
    train_one_iteration = NULL,
    loss_tensor = NULL,
    cov_tensor = NULL,
    prec_tensor = NULL,
    analytic_cov = NULL,
    analytic_prec = NULL,
    session = NULL,
    l_vec_values = NULL,
    u_vec_values = NULL,
    ana = NULL,
    max_cov_above_prec_threshold = NULL,
    lambda_val_1_tensor = NULL,
    lambda_val_tensor = NULL,

    get_tf = function() {
      tf = tensorflow::tf$compat$v1
      # TF1-style graph mode, not eager!
      if (isTRUE(tf$executing_eagerly())) tf$compat$v1$disable_eager_execution()
      tf
    },

    run = function() {

      tf = private$get_tf()

      frac = 0.01
      f_value_explain_point = 1

      if (is.null(private$obsdata)) {
        private$obsdata = sampling(predictor = private$predictor,
                                   x_interest = private$x_interest,
                                   fixed_features = private$fixed_features,
                                   desired_range = private$desired_range,
                                   param_set = private$param_set,
                                   num_sampled_points = private$num_sampled_points,
                                   strategy = private$strategy
                                   )
      }

      private$.calls_fhat = private$.calls_fhat + nrow(private$obsdata)
      private$f_values_obsdata = data.table(predict_range(private$predictor,
        private$obsdata, range = private$desired_range))

      if (private$strategy == "traindata") {
        private$num_sampled_points = nrow(private$obsdata)
      }

      # create transformed training dataset
      # factor variables numeric according to class
      expldata = transform_for_explanation(data = private$obsdata,
                                           predictor = private$predictor,
                                           x_interest = private$x_interest,
                                           version = 3,
                                           frac = frac)
      private$expldata = expldata$expldata
      private$explain_point = expldata$explx_interest
      private$categorylist = attr(expldata, "categorylist")
      private$obsdata = rbind(private$x_interest, private$obsdata)

      private$dimension = ncol(private$expldata)

      # ## get lower and upper values of feature of explain_point (e.g., for explain_point[0] = 0.363 it will be l_vec[0] = 0.353 and u_vec[0] = 0.373 --> we start very small!
      private$l_vec = tf$Variable(
        as.matrix(private$explain_point) - rep(1, private$dimension) * 0.01,
        "float64"
      )
      private$u_vec = tf$Variable(
        as.matrix(private$explain_point) + rep(1, private$dimension) * 0.01,
        "float64"
      )
      ## if l_vec and u_vec are not between 0 and 1, clip them between 0 and 1
      private$clip_l = private$l_vec$assign(tf$clip_by_value(private$l_vec, 0.0, 1.0))
      private$clip_u = private$u_vec$assign(tf$clip_by_value(private$u_vec, 0.0, 1.0))
      ## save inputs in tensorflow format
      private$f_value_explain_point_int = f_value_explain_point
      private$f_value_explain_point = tf$cast(tf$constant(f_value_explain_point), "float64")
      # private$construct_graph()
      # tf_graph = tf_function(private$construct_graph)
      # tf_graph()
      private$fit_explanation(num_of_iterations = private$num_of_iterations)
      names(private$l_vec_values) = names(private$expldata)
      names(private$u_vec_values) = names(private$expldata)
      current_box = make_param_set(private$x_interest)

      # l_values_list = rep(NA, private$predictor$data$n.features)
      # u_values_list = rep(NA, private$predictor$data$n.features)

      # data = private$predictor$data$get.x()
      # l_vec_values = private$l_vec_values
      # u_vec_values = private$u_vec_values
      for (fnam in private$predictor$data$feature.names) {
        if (fnam %in% private$fixed_features) next
        fcol = private$obsdata[[fnam]]
        j = which(private$predictor$data$feature.names == fnam)

        is_num = is.numeric(fcol) || is.integer(fcol)
        if (is_num) {
          l_val = (max(private$l_vec_values[[fnam]], frac - 0.001) - frac) / (1.0 - 2 * frac) * (max(fcol) - min(fcol)) + min(fcol)
          u_val = (min(private$u_vec_values[[fnam]], (1.0 - frac) + 0.001) - frac) / (1.0 - 2 * frac) * (max(fcol) - min(fcol)) + min(fcol)
          assert_true(l_val <= u_val)
          if (is.character(all.equal((max(private$explain_point[[fnam]], frac - 0.001) - frac) /
            (1.0 - 2 * frac) * (max(fcol) - min(fcol)) + min(fcol),
          private$x_interest[[fnam]]))) {
            stop("Retransformation of box boundaries did not work properly.")
          }
          if (is.integer(fcol)) {
            l_val = ceiling(l_val)
            u_val = floor(u_val)
          }
          current_box = update_box(current_box,
                                   j = j,
                                   lower = l_val,
                                   upper = u_val,
                                   complement = TRUE)
        } else {
          #
          if (!is.ordered(fcol)) {
            if (length(unique(fcol)) == 2L) {
              id = which(c(0.33, 0.66) > private$l_vec_values[[fnam]] & c(0.33, 0.66) < private$u_vec_values[[fnam]])
              val = unique(levels(fcol)[id])

            } else {
              explnames = names(private$l_vec_values)
              colnams = explnames[grepl(paste0("^", fnam, "_"), explnames)]
              if (length(colnams) == 0L) {
                stop(
                  "MAIRE: expected one-hot columns for feature '", fnam,
                  "' (prefix '", fnam, "_') but none found. Check transform_for_explanation()."
                )
              }
              oneinc = sapply(colnams, FUN = function(nam) {
                private$l_vec_values[[nam]] < 0.66 &
                  0.66 < private$u_vec_values[[nam]]
              }, simplify = TRUE)
              val = sub(paste0(fnam, "_"), replacement = "", x = colnams[oneinc])
            }
          } else {
            fcol = as.numeric(fcol)
            l_val = (max(private$l_vec_values[[fnam]], frac - 0.001) - frac) / (1.0 - 2 * frac) * (max(fcol) - min(fcol)) + min(fcol)
            u_val = (min(private$u_vec_values[[fnam]], (1.0 - frac) + 0.001) - frac) / (1.0 - 2 * frac) * (max(fcol) - min(fcol)) + min(fcol)
            val = private$categorylist[[fnam]][ceiling(l_val):(floor(u_val))]
          }
          current_box = update_box(current_box,
                                   j = j,
                                   val = val,
                                   complement = TRUE)
        }
      }
      return(current_box)
      # ## identify points in dataset that lie within box
      # ids = apply(apply(private$predictor$data$get.x(), MARGIN = 1L,
      #   FUN = function(row) {
      #     row >= l_values_list & row <= u_values_list
      #   } ), MARGIN = 2L, all)
    },
    fit_explanation = function(num_of_iterations) {
      tf = private$get_tf()

      # Create constant tensors
      private$lambda_val_1_tensor = tf$constant(private$lambda_val_1, dtype = "float64")
      private$lambda_val_tensor = tf$constant(private$lambda_val, dtype = "float64")
      private$threshold_tensor = tf$constant(private$threshold, dtype = "float64")

      num_sampled_points_updated = as.integer(private$num_sampled_points) + 1L
      ### Construct Graph
      sampled_points_placeholder = tf$placeholder(dtype = "float64",
        shape = list(num_sampled_points_updated,
          private$dimension))
      ## b) placeholder for predicted values of sampled points
      sampled_points_f_values_placeholder = tf$placeholder(dtype = "float64",
        shape = list(num_sampled_points_updated, 1L))
      # ## init loss, coverage and precision with placeholders
      private$loss_tensor = private$loss(sampled_points_placeholder,
        sampled_points_f_values_placeholder)
      private$cov_tensor = private$cov(sampled_points_placeholder)
      private$prec_tensor = private$prec(sampled_points_placeholder,
        sampled_points_f_values_placeholder)
      ## init gradients
      gradients = tf$gradients(private$loss_tensor, list(private$l_vec, private$u_vec))
      ## use ADAM as optimizer
      optimizer = tf$train$AdamOptimizer()
      ## optimize based on gradients and lower, upper values (1 iteration)
      private$train_one_iteration = optimizer$apply_gradients(zip_lists(gradients, list(private$l_vec, private$u_vec)))
      ## true coverage, not approximated one --> Eq. (1) in paper
      private$analytic_cov = tf$reduce_sum(
        tf$cast(
          tf$reduce_all(
            tf$logical_and(
              tf$greater(sampled_points_placeholder, private$l_vec),
              tf$less(sampled_points_placeholder, private$u_vec)),
            1L),
          "float64")) / num_sampled_points_updated
      # ## true precision
      private$analytic_prec = tf$reduce_sum(tf$cast(tf$reduce_all(
        tf$logical_and(
          tf$greater(sampled_points_placeholder, private$l_vec),
          tf$less(sampled_points_placeholder, private$u_vec)), 1L), "float64") *
        tf$transpose(1.0 - tf$square(sampled_points_f_values_placeholder - private$f_value_explain_point))) /
        tf$reduce_sum(
          tf$cast(
            tf$reduce_all(
              tf$logical_and(
                tf$greater(sampled_points_placeholder, private$l_vec),
                tf$less(sampled_points_placeholder, private$u_vec)), 1L), "float64"))

      # ## tf$Session encapsulates the environment in which Operation objects are executed, and Tensor objects are evaluated
      session = tf$Session()
      session$run(tf$initialize_all_variables())

      ## end construct graph

      max_cov_above_prec_threshold = 0.0
      history = data.table(iteration = numeric(), loss = numeric(), cov = numeric(),
        prec = numeric(), analytic_cov = numeric(), analytic_prec = numeric())
      featnams = names(private$expldata)
      best_coverage = 0
      counter = iteration = 1L
      converged = FALSE
      sampled_points = rbind(private$expldata, private$explain_point)
      f_values_sampled_points = rbind(private$f_values_obsdata, list(1L))

      while (iteration <= private$num_of_iterations) {
        temp_vec_values = session$run(list(private$l_vec, private$u_vec))
        temp_l_vec_values = temp_vec_values[[1]]
        temp_u_vec_values = temp_vec_values[[2]]
        ## sample point from available dataset & return its features + prediction class
        ## for adult example, we just use the whole dataset and evaluate precision and coverage on this dataset!
        ## get approx values of coverage and precision + loss value
        # loss_value, cov_value, prec_value, _ =
        values = session$run(list(private$loss_tensor,
          private$cov_tensor,
          private$prec_tensor,
          private$train_one_iteration),
        feed_dict = dict(sampled_points_placeholder = as.matrix(sampled_points),
          sampled_points_f_values_placeholder = as.matrix(f_values_sampled_points)))
        loss_value = values[[1]]
        cov_value = values[[2]]
        prec_value = values[[3]]
        session$run(list(private$clip_l, private$clip_u))
        ## get true values for coverage and precision
        analytic_values = session$run(list(private$analytic_cov,
          private$analytic_prec),
        feed_dict = dict(sampled_points_placeholder = as.matrix(sampled_points),
          sampled_points_f_values_placeholder = as.matrix(f_values_sampled_points)))
        analytic_cov_value = analytic_values[[1]]
        analytic_prec_value = analytic_values[[2]]
        ## update history list
        lowerdat = data.table(temp_l_vec_values)
        names(lowerdat) = paste0("l_", featnams)
        upperdat = data.table(temp_u_vec_values)
        names(upperdat) = paste0("u_", featnams)
        if (counter == 1) {
          history = rbind(history, list(counter, loss_value, cov_value, prec_value,
            analytic_cov_value, analytic_prec_value))
          history = cbind(history, lowerdat, upperdat)
        } else {
          history = rbind(history, c(list(counter, loss_value, cov_value, prec_value,
            analytic_cov_value, analytic_prec_value), as.list(temp_l_vec_values), as.list(temp_u_vec_values)))
        }

        ## print iteration history
        if (!private$quiet) {
          message(paste0(capture.output(history[counter]), collapse = "\n"))
        }
        ## if true precision > threshold --> update l_vec and u_vec values --> if not, do not update them
        if (is.nan(analytic_prec_value)) analytic_prec_value = 0
        # CHANGE TO ORIGINAL: only update if coverage higher than current best
        if ((analytic_prec_value >= private$threshold & analytic_cov_value >= best_coverage)) { # and analytic_cov_value > self.max_cov_above_prec_threshold:
          # vec_values = session$run(list(private$l_vec, private$u_vec))
          private$l_vec_values = temp_vec_values[[1]]
          private$u_vec_values = temp_vec_values[[2]]
          private$ana = c(analytic_prec_value, counter, private$l_vec_values, private$u_vec_values)
          private$max_cov_above_prec_threshold = analytic_cov_value

          if (all(private$l_vec_values == 0) & all(private$u_vec_values == 1)) {
            break
          }

        }
        if (analytic_prec_value < private$threshold) {
          converged = TRUE
        }

        if (!(private$convergence) | converged) {
          iteration = iteration + 1L
        }
        counter = counter + 1L

        if (counter == 10000) {
          message("Maire did not terminated after 10000 steps, return best solution found so far.")
          break
        }

      }

      private$.history = history
    },

    fit_explanation_debug = function(num_of_iterations) {
      browser()

      max_cov_above_prec_threshold = 0.0
      history = data.table(iteration = numeric(), loss = numeric(), cov = numeric(),
        prec = numeric(), analytic_cov = numeric(), analytic_prec = numeric())

      sampled_points = rbind(private$expldata, private$explain_point)
      f_values_sampled_points = rbind(private$f_values_obsdata, list(1L))

      for (iteration in seq_len(num_of_iterations)) {

        ## sample point from available dataset & return its features + prediction class
        ## for adult example, we just use the whole dataset and evaluate precision and coverage on this dataset!
        # sampled_points_list = private$sampling(num_sampled_points_updated,
        #   desired_range = private$desired_range, strategy = private$strategy)
        # sampled_points = tf$Variable(sampled_points_list[[1]])
        # f_values_sampled_points = tf$Variable(sampled_points_list[[2]])
        # eq. (4) in paper
        private$loss_tensor = private$loss(sampled_points,
          f_values_sampled_points)
        # approximated coverage and precision
        private$cov_tensor = private$cov(sampled_points)
        private$prec_tensor = private$prec(sampled_points,
          f_values_sampled_points)
        ## init gradients
        gradients = tf$gradients(private$loss_tensor, list(private$l_vec, private$u_vec))
        ## use ADAM as optimizer
        optimizer = tf$train$AdamOptimizer()
        ## optimize based on gradients and lower, upper values (1 iteration)
        private$train_one_iteration = optimizer$apply_gradients(zip_lists(gradients, list(private$l_vec, private$u_vec)))
        ## true coverage, not approximated one --> Eq. (1) in paper
        private$analytic_cov = tf$reduce_sum(
          tf$cast(
            tf$reduce_all(
              tf$logical_and(
                tf$greater(sampled_points, private$l_vec),
                tf$less(sampled_points, private$u_vec)),
              1L),
            "float64")) / num_sampled_points_updated
        if (!tf$equal(private$analytic_cov, 0)) {
          # ## true precision
          private$analytic_prec = tf$reduce_sum(
            tf$cast(
              tf$reduce_all(
                tf$logical_and(
                  tf$greater(sampled_points, private$l_vec),
                  tf$less(sampled_points, private$u_vec)
              ), 1L), "float64"
            ) * tf$transpose(1.0 - tf$square(f_values_sampled_points - private$f_value_explain_point))) /
            tf$reduce_sum(
              tf$cast(
                tf$reduce_all(
                  tf$logical_and(
                    tf$greater(sampled_points, private$l_vec), tf$less(sampled_points, private$u_vec)
                  ), 1L
              ), "float64")
            )
        } else {
          private$analytic_prec = tf$constant(0, dtype = "float64")
        }

        temp_l_vec_values = private$l_vec
        temp_u_vec_values = private$u_vec
        private$clip_l
        private$clip_u

        ## get true values for coverage and precision
        analytic_cov_value = private$analytic_cov
        analytic_prec_value = private$analytic_prec
      }
      private$.history = history
    },

    #    #' Internal loss function for the Maire optimizer.
    #    #' @param sampled_points (Tensor) \cr currently used dataset
    #    #' @param f_values_sampled_points (Tensor) \cr predicted labels of sampled points
    loss = function(sampled_points, f_values_sampled_points) {
      tf = private$get_tf()
      # true precision
      analytic_prec = tf$reduce_sum(
        tf$cast(
          tf$reduce_all(
            tf$logical_and(
              tf$greater(sampled_points, private$l_vec),
              tf$less(sampled_points, private$u_vec)
          ), 1L), "float64"
        ) * tf$transpose(1.0 - tf$square(f_values_sampled_points - private$f_value_explain_point))) /
        tf$reduce_sum(
          tf$cast(
            tf$reduce_all(
              tf$logical_and(
                tf$greater(sampled_points, private$l_vec), tf$less(sampled_points, private$u_vec)
              ), 1L
          ), "float64")
        )
      # eq (4) in paper, but minimizes it
      # if prec > P --> only optimize coverage
      # if prec < P --> optimize both precision and coverage
      lossf = -1.0 * private$cov(sampled_points) -
        1.0 * (1.0 - tf$cast(is.nan(analytic_prec), "float64")) *
          tf$multiply(
            private$lambda_val_1_tensor * (0.5 + tf$sign(-analytic_prec + private$threshold_tensor) * 0.5),
            private$prec(sampled_points, f_values_sampled_points)
          ) + private$constraint_sum()
      return(lossf)
    },
    h = function(sampled_points) {
      tf = private$get_tf()
      return(private$A(
        tf$concat(list(private$G(
          sampled_points, private$l_vec),
        private$GE(private$u_vec, sampled_points)), axis = 1L)))
    },
    cov = function(sampled_points) {
      tf = private$get_tf()
      return(tf$reduce_mean(private$h(sampled_points)))
    },
    prec = function(sampled_points, f_values_sampled_points) {
      h_values = private$h(sampled_points)
      h_mul_values = tf$multiply(h_values, tf$transpose(1.0 - tf$square(f_values_sampled_points - private$f_value_explain_point)))
      return(tf$reduce_mean(h_mul_values) / tf$reduce_mean(h_values))
    },
    constraint_sum = function() {
      tf = private$get_tf()
      tf_explain_point = tf$Variable(as.matrix(private$explain_point))
      return(tf$reduce_sum(
        tf$multiply(
          private$lambda_val_tensor, tf$nn$relu(
            private$l_vec - tf_explain_point))) +
        tf$reduce_sum(tf$multiply(private$lambda_val_tensor, tf$nn$relu(tf_explain_point - private$u_vec))))
    },
    step_function = function(input_var) {
      return(tf$sigmoid(private$c2 * input_var) * private$c1 + private$c3 * (tf$sign(input_var) * private$c4 + private$c5))
    },
    G = function(x, y) {
      return(private$step_function(x - y))
    },
    GE = function(x, y) {
      return(private$step_function(x - y + private$cl))
    },
    A = function(x) {
      return(private$step_function(tf$reduce_mean(x, 1L) - private$ch))
    },
    print_parameters = function() {
      params = private$.get_parameters()

      if (length(params) == 0L) {
        cat(" - none\n")
        return(invisible(self))
      }

      for (nm in names(params)) {
        value = params[[nm]]

        if (is.null(value)) {
          value = "NULL"
        } else if (length(value) > 1L) {
          value = paste(value, collapse = ", ")
        }
        cat(" - ", nm, ": ", value, "\n", sep = "")
      }

      invisible(self)
    },
    .get_parameters = function() {
      list(
        num_of_iterations = private$num_of_iterations,
        convergence = private$convergence,
        strategy = private$strategy,
        num_sampled_points = private$num_sampled_points,
        lambda_val_1 = private$lambda_val_1,
        lambda_val = private$lambda_val,
        threshold = private$threshold,
        c1 = private$c1,
        c2 = private$c2,
        c3 = private$c3,
        c4 = private$c4,
        c5 = private$c5,
        cl = private$cl,
        ch = private$ch
      )
    }
  )
)
