
# This file is a generated template, your changes will not be overwritten

quickpsyfitClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "quickpsyfitClass",
    inherit = quickpsyfitBase,
    private = list(
        .run = function() {
            
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          
          if (nrow(self$data) == 0) {
            stop('Data contains no (complete) rows')
          }
          # k
          if (is.null(self$options$k)) {
            return()
          }
          k <- jmvcore::toNumeric(self$data[[self$options$k]])
          if (is.null(k) || all(is.na(k))) {
            return()
          }
          # x
          if (is.null(self$options$x)) {
            return()
          }
          x <- jmvcore::toNumeric(self$data[[self$options$x]])
          if (is.null(x) || all(is.na(x))) {
            return()
          }
          # N
          if (is.null(self$options$n)) {
            n <- NULL
          } else {
            n <- jmvcore::toNumeric(self$data[[self$options$n]])
          }
          # dependent variable
          if (all(unique(k) %in% c(0, 1))) {
            
          } else {
            if (is.null(n)) {
              jmvcore::reject("N must be specified if k is not 0/1")
            }
          }
          
          # dataset
          if (is.null(n)) {
            dat <- data.frame(x, k)
          } else {
            dat <- data.frame(x, k, n)
          }
          
          # groups
          if (is.null(self$options$group) && length(self$options$group == 0)) {
            group <- NULL
          } else {
            group <- self$options$group
            for (g in group) {
              dat[[g]] <- as.character(self$data[[g]])
            }
          }
          
          if (self$options$fun == "cum_normal_fun")
            usefun <- "Cumulative normal"
          if (self$options$fun == "logistic_fun")
            usefun <- "Logistic"
          if (self$options$fun == "weibull_fun")
            usefun <- "Weibull"
          
          # fit ------------------------------------------------
          if (!is.null(n)) {
            fit <- quickpsy::quickpsy_(
              dat, x="x", k="k", n="n",
              grouping = group,
              fun = self$options$fun,
              log = self$options$log,
              guess = self$options$guess,
              lapses = self$options$lapses
            )
          } else {
            fit <- quickpsy::quickpsy_(
              dat, x="x", k="k", 
              grouping = group,
              fun = self$options$fun,
              log = self$options$log,
              guess = self$options$guess,
              lapses = self$options$lapses
            )
          }

          # results ---------------------------------------------------
          ng <- length(group)
          tpar <- fit$par
          params <- self$results$params
          for (r in 1:nrow(fit$par)) {
            params$addRow(
              rowKey = r,
              values = list(
                cond = ifelse(ng > 0, paste(tpar[r, 1:ng], collapse = " x "), "all"),
                par = tpar$parn[r],
                value = tpar$par[r],
                low = tpar$parinf[r],
                high = tpar$parsup[r]
              ))
          }

          tth <- fit$thresholds
          thresh <- self$results$threshold
          for (r in 1:nrow(fit$thresholds)) {
            thresh$addRow(
              rowKey = r,
              values = list(
                cond = ifelse(ng > 0, paste(tth[r, 1:ng], collapse = " x "), "all"),
                prob = tth$prob[r],
                value = tth$thre[r],
                low = tth$threinf[r],
                high = tth$thresup[r]
              )
            )
          }
          if (self$options$plotcurve) {
            self$results$plotcurve$setState(fit)
          }

          results <- paste("Using ", usefun, " function.") 
          self$results$text$setContent(results)
        },
        
    .plotCurve = function(image, ggtheme, theme, ...) {
      fit <- image$state
      if (!is.null(fit)) {
        plot <- quickpsy::plotcurves(fit) + ggtheme
        print(plot)
        TRUE
      } else {
        FALSE
      }
    }
    )
)
