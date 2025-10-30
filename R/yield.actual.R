#' Calculate Yield (IRR) Using Actual Calendar Dates
#'
#' Calculates the internal rate of return for a series of cash flows using
#' actual calendar dates and specified compounding convention. This is equivalent
#' to yield-to-maturity for bonds and effective yield for loan participations.
#'
#' @param cf Data frame with 'date' and 'amount' columns, or a named vector
#'   with dates as names and amounts as values
#' @param pv Present value (initial investment or purchase price). Should be positive.
#' @param start_date Date of investment. If NULL, uses the first cash flow date.
#'   Can be Date object or string in format "YYYY-MM-DD".
#' @param compounding Compounding frequency: "monthly" (12x/year), "semiannual" (2x/year),
#'   "quarterly" (4x/year), "annual" (1x/year), or "continuous"
#' @param max_iter Maximum iterations for root finding (default: 100)
#' @param tol Convergence tolerance (default: 1e-8)
#'
#' @return Annual yield as decimal (e.g., 0.0564 = 5.64%)
#' 
#' @details
#' The 'amount' column in cf should represent cash flows received by the investor
#' (positive values). The initial investment (pv) is specified separately as a 
#' positive number and should not be included in the cf data frame.
#' 
#' **Choosing the compounding convention:**
#' The compounding frequency should match the market convention for quoting yields
#' on similar instruments, not necessarily the frequency of cash flows:
#' \itemize{
#'   \item Use "monthly" for mortgages, auto loans, and most consumer/commercial loans
#'   \item Use "semiannual" for US Treasury bonds and most corporate bonds
#'   \item Use "annual" for a simple effective annual rate
#'   \item The cash flows themselves can be irregular regardless of compounding choice
#' }
#' 
#' This function solves for the yield y that satisfies:
#' \deqn{PV = \sum_{i=1}^{n} \frac{CF_i}{(1 + y/m)^{m \times t_i}}}
#' where m is the compounding frequency and t_i is the time in years to cash flow i.
#' 
#' For continuous compounding:
#' \deqn{PV = \sum_{i=1}^{n} CF_i \times e^{-y \times t_i}}
#' 
#' Time periods are calculated using actual calendar days divided by 365.25.
#' 
#' @seealso \code{\link{irr}} for period-based IRR without dates
#' @seealso \code{\link{npv}} for net present value calculation
#' 
#' @export
#' @examples
#' # Example 1: Loan participation with monthly cash flows
#' # Investor pays $100,000 and receives $8,500/month for 12 months
#' cash_flows <- data.frame(
#'   date = seq(as.Date("2025-02-01"), by = "month", length.out = 12),
#'   amount = rep(8500, 12)  # Cash received each month
#' )
#' yield.actual(cash_flows, pv = 100000, start_date = "2025-01-01")
#' 
#' # Example 2: Bond with semiannual coupons
#' # Purchase price $98,000, receive $2,500 coupons + $100,000 at maturity
#' bond_cfs <- data.frame(
#'   date = as.Date(c("2025-07-01", "2026-01-01", "2026-07-01")),
#'   amount = c(2500, 2500, 102500)  # Coupons received + final principal
#' )
#' yield.actual(bond_cfs, pv = 98000, compounding = "semiannual")
#' 
#' # Example 3: Irregular cash flows
#' irregular_cfs <- data.frame(
#'   date = as.Date(c("2025-03-15", "2025-06-30", "2025-12-31")),
#'   amount = c(5000, 5000, 95000)  # Payments received on various dates
#' )
#' yield.actual(irregular_cfs, pv = 100000, start_date = "2025-01-01",
#'              compounding = "monthly")
#' 
#' # Example 4: Using named vector format
#' # Same as Example 1 but more compact
#' cf_vector <- setNames(
#'   rep(8500, 12),
#'   seq(as.Date("2025-02-01"), by = "month", length.out = 12)
#' )
#' yield.actual(cf_vector, pv = 100000, start_date = "2025-01-01")
yield.actual <- function(cf, 
                         pv,
                         start_date = NULL,
                         compounding = c("monthly", "semiannual", "quarterly", 
                                         "annual", "continuous"),
                         max_iter = 100, 
                         tol = 1e-8) {
  
  # Match compounding argument
  compounding <- match.arg(compounding)
  
  # Set compounding frequency
  comp_freq <- switch(compounding,
                      monthly = 12,
                      quarterly = 4,
                      semiannual = 2,
                      annual = 1,
                      continuous = Inf)
  
  # Input validation
  if (missing(pv) || is.null(pv)) {
    stop("pv (present value) is required")
  }
  
  if (pv <= 0) {
    stop("pv must be positive")
  }
  
  # Handle different cf input formats
  if (is.data.frame(cf)) {
    # Check for required columns
    if (!all(c("date", "amount") %in% names(cf))) {
      stop("cf data frame must contain 'date' and 'amount' columns")
    }
    dates <- as.Date(cf$date)
    amounts <- cf$amount
  } else if (is.vector(cf) && !is.null(names(cf))) {
    # Named vector: names are dates, values are amounts
    dates <- as.Date(names(cf))
    amounts <- as.numeric(cf)
  } else {
    stop("cf must be a data frame with 'date' and 'amount' columns, or a named vector")
  }
  
  # Clean cash flows
  valid_idx <- !is.na(amounts) & is.finite(amounts) & amounts != 0 & !is.na(dates)
  dates <- dates[valid_idx]
  amounts <- amounts[valid_idx]
  
  if (length(amounts) == 0) {
    warning("No valid cash flows provided")
    return(NA_real_)
  }
  
  # Sort by date
  sort_idx <- order(dates)
  dates <- dates[sort_idx]
  amounts <- amounts[sort_idx]
  
  # Set start date
  if (is.null(start_date)) {
    start_date <- min(dates)
  } else {
    start_date <- as.Date(start_date)
  }
  
  # Calculate time periods in years
  t_years <- as.numeric(difftime(dates, start_date, units = "days")) / 365.25
  t_years <- pmax(t_years, 1/365.25)  # Minimum time period
  
  # Sanity check on cash flows
  total_cash <- sum(amounts)
  if (total_cash < pv * 0.5) {
    warning("Total cash flows (", round(total_cash, 2), 
            ") are less than 50% of present value (", round(pv, 2), 
            "). Yield may be very negative.")
  }
  
  # Yield function: PV of cash flows = pv
  if (compounding == "continuous") {
    # Continuous compounding: PV = CF * e^(-y*t)
    yield_function <- function(y) {
      pv_calc <- sum(amounts * exp(-y * t_years))
      return(pv_calc - pv)
    }
  } else {
    # Discrete compounding: PV = CF / (1 + y/m)^(m*t)
    yield_function <- function(y) {
      if (y <= -comp_freq) return(Inf)  # Prevent invalid rates
      
      pv_calc <- sum(amounts / (1 + y / comp_freq)^(comp_freq * t_years))
      return(pv_calc - pv)
    }
  }
  
  # Find bracketing interval
  lower <- -0.10  # Allow negative yields (rare but possible)
  upper <- 0.50   # Max 50% yield
  iter <- 0
  
  while (iter < max_iter) {
    lower_val <- yield_function(lower)
    upper_val <- yield_function(upper)
    
    # Check validity
    if (!is.finite(lower_val) || !is.finite(upper_val)) {
      warning("Yield function produced non-finite values")
      return(NA_real_)
    }
    
    # Check if root exists in interval (opposite signs)
    if (lower_val * upper_val < 0) {
      break
    }
    
    # Expand interval
    lower <- lower - 0.05
    upper <- upper + 0.05
    iter <- iter + 1
    
    # Prevent unreasonable intervals
    if (lower < -0.95 || upper > 2.0) {
      warning("Could not find valid yield interval. Check inputs.")
      return(NA_real_)
    }
  }
  
  if (iter == max_iter) {
    warning("Yield function does not cross zero after interval expansion")
    return(NA_real_)
  }
  
  # Solve for yield using root finding
  tryCatch({
    result <- uniroot(
      yield_function, 
      interval = c(lower, upper), 
      tol = tol,
      maxiter = max_iter
    )
    
    yield <- result$root
    
    # Sanity check result
    if (yield < -0.50 || yield > 1.0) {
      warning("Calculated yield of ", round(yield * 100, 2), 
              "% seems unrealistic. Verify inputs.")
    }
    
    return(yield)
    
  }, error = function(e) {
    warning("Yield calculation failed: ", e$message)
    return(NA_real_)
  })
}
