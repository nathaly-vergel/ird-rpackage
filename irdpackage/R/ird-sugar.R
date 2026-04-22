# Contains the user-facing "shortcuts" to compute an IRD without manually
# creating the R6 method object -> syntactic sugar

#' Find an interpretable regional descriptor
#'
#' @description
#' Compute an interpretable regional descriptor (IRD) for an observation of
#' interest using one of the available IRD methods. An IRD describes a region
#' in feature space in which changes to the feature values do not move the
#' prediction outside the desired range.
#'
#' @param predictor (`iml::Predictor`) \cr
#'   Object containing the trained model and the data. It is used to evaluate
#'   predictions and determine whether points belong to the IRD.
#'
#' @param x_interest (`data.frame` | `data.table`) \cr
#'   A single observation for which the IRD is computed.
#'
#' @param method (`character(1)`) \cr
#'   Method used to construct the IRD. One of `"prim"`, `"maxbox"`, or `"maire"`.
#'
#' @param desired_range (`numeric(2)` | `NULL`) \cr
#'   Interval of prediction values that defines when a point is considered
#'   similar to the prediction of `x_interest`. All points inside the IRD must
#'   have predictions within this range. This corresponds to the closeness
#'   region used to define precision/impurity.
#'
#' @param desired_class (`character(1)` | `NULL`) \cr
#'   Class of interest for classification models. The IRD is constructed such
#'   that predictions for this class stay within `desired_range`. Not used for
#'   regression models.
#'
#' @param fixed_features (`character()` | `NULL`) \cr
#'   Names of features that are not allowed to change. For these features,
#'   the IRD will only include the value observed in `x_interest`.
#'
#' @param obsdata (`data.frame` | `data.table` | `NULL`) \cr
#'   Data used to evaluate candidate regions during the search. If not provided,
#'   the data stored in `predictor` is used.
#'
#' @param box_largest (`paradox::ParamSet` | `NULL`) \cr
#'   Optional outer region that restricts the search space. The IRD will be
#'   contained within this region.
#'
#' @param method_args (`list`) \cr
#'   Additional arguments passed to the selected IRD method. These control
#'   how the method searches for the IRD (for example, search resolution or
#'   stopping criteria).
#'
#' @seealso
#' \code{\link{Prim}}, \code{\link{MaxBox}}, \code{\link{Maire}}
#'
#' @return
#' A `RegDesc` object. Additional metadata from the fitted method, such as
#' the optimization history, number of prediction calls, and method object,
#' are attached as attributes.
#'
#' @export
find_ird = function(
    predictor,
    x_interest,
    method = c("prim", "maxbox", "maire"),
    desired_range = NULL,
    desired_class = NULL,
    fixed_features = NULL,
    obsdata = NULL,
    box_largest = NULL,
    method_args = list()) {

  method = match.arg(method)

  checkmate::assert_class(predictor, "Predictor")
  checkmate::assert_list(method_args, names = "named")

  ctor_args = c(list(predictor = predictor), method_args)

  method_object = switch(
    method,
    "prim" = do.call(Prim$new, ctor_args),
    "maxbox" = do.call(MaxBox$new, ctor_args),
    "maire" = do.call(Maire$new, ctor_args)
  )

  method_object$find_box(
    x_interest = x_interest,
    desired_range = desired_range,
    desired_class = desired_class,
    fixed_features = fixed_features,
    obsdata = obsdata,
    box_largest = box_largest
  )
}


#' Post-process an interpretable regional descriptor
#'
#' @description
#' Refine the boundaries of an existing interpretable regional descriptor (IRD)
#' using the post-processing procedure. The procedure improves a given box by
#' peeling impure parts and pasting adjacent homogeneous parts.
#'
#' @param object (`RegDesc`) \cr
#'   An existing regional descriptor to refine.
#'
#' @param predictor (`iml::Predictor` | `NULL`) \cr
#'   Model and data used to evaluate predictions. Only required if it cannot
#'   be recovered from `object`.
#'
#' @param x_interest (`data.frame` | `data.table` | `NULL`) \cr
#'   Observation for which the IRD was computed. Only required if it cannot
#'   be recovered from `object`.
#'
#' @param desired_range (`numeric(2)` | `NULL`) \cr
#'   Interval of prediction values that defines when a point is considered
#'   part of the IRD. Used to check whether regions are homogeneous.
#'
#' @param desired_class (`character(1)` | `NULL`) \cr
#'   Class of interest for classification models. Not used for regression models.
#'
#' @param fixed_features (`character()` | `NULL`) \cr
#'   Features that must remain fixed during post-processing.
#'
#' @param box_largest (`paradox::ParamSet` | `NULL`) \cr
#'   Optional outer region restricting how far the IRD can be expanded.
#'
#' @param method_args (`list`) \cr
#'   Named list of additional arguments passed to `PostProcessing$new()`.
#'   Some arguments include:
#'   \describe{
#'     \item{subbox_relsize}{Relative size of candidate subboxes for numeric features.}
#'     \item{evaluation_n}{Number of sampled points used to evaluate a candidate subbox.}
#'     \item{paste_alpha}{Minimum relative step size used during pasting.}
#'     \item{strategy_ties}{How ties between equally good candidates are broken.}
#'     \item{quiet}{Whether to suppress messages.}
#'   }
#'
#' @seealso
#' \code{\link{PostProcessing}}
#'
#' @return
#' A `RegDesc` object. Additional metadata from the post-processing method,
#' such as the optimization history, number of prediction calls, and method
#' object, are attached as attributes.
#'
#' @export
postprocess_box = function(
    object,
    predictor = NULL,
    x_interest = NULL,
    desired_range = NULL,
    desired_class = NULL,
    fixed_features = NULL,
    box_largest = NULL,
    method_args = list()) {

  checkmate::assert_class(object, "RegDesc")
  checkmate::assert_list(method_args, names = "named")

  box_init = object$box

  if (is.null(predictor)) {
    predictor = object$predictor
  }
  if (is.null(x_interest)) {
    x_interest = object$x_interest
  }

  if (is.null(predictor)) {
    stop("Could not infer `predictor` from `object`. Please provide it explicitly.",
         call. = FALSE)
  }
  if (is.null(x_interest)) {
    stop("Could not infer `x_interest` from `object`. Please provide it explicitly.",
         call. = FALSE)
  }

  method_object = do.call(
    PostProcessing$new,
    c(list(predictor = predictor), method_args)
  )

  method_object$find_box(
    x_interest = x_interest,
    desired_range = desired_range,
    desired_class = desired_class,
    fixed_features = fixed_features,
    box_init = box_init,
    box_largest = box_largest
  )
}

