# object=gasl_fit
#' @export
predict.gasl <- function(object, newdata = NULL, type = c("link", "response")) {
    type <- match.arg(type)
    if (is.null(newdata)) {
        link <- object$additive.predictors
    } else {
        new_gam_data <- split_x(newdata, object$factor_vars)

        mt <- terms(object$formula, gasl.slist, data = new_gam_data)
        mt <- delete.response(mt)
        mf <- model.frame(mt, new_gam_data)

        all_preds <- foreach(j = seq(object$smoother_fits)) %do% {
            preds <- predict(object$smoother_fits[[j]], newdata = mf[[j]])
            if (is.list(preds)) {
                preds <- preds$pred
            }

            preds
        }
        predmat <- do.call(cbind, all_preds)


        link <- rowSums(predmat) + object$coefficients
    }
    if (type == "link") {
        return(link)
    }

    if (type == "response") {
        response <- object$family$linkinv(link)
        return(response)
    } else {
        stop("unspported prediction type")
    }

    smoother_preds <- lapply(object$smoother_fits, predict, newdata = newdata)
}
