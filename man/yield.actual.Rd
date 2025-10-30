\name{yield.actual}
\alias{yield.actual}
\title{Calculate Yield (IRR) Using Actual Calendar Dates}
\usage{
yield.actual(
  cf,
  pv,
  start_date = NULL,
  compounding = c("monthly", "semiannual", "quarterly", "annual", "continuous"),
  max_iter = 100,
  tol = 1e-08
)
}
\arguments{
\item{cf}{Data frame with 'date' and 'amount' columns, or a named vector
with dates as names and amounts as values}

\item{pv}{Present value (initial investment or purchase price). Should be positive.}

\item{start_date}{Date of investment. If NULL, uses the first cash flow date.
Can be Date object or string in format "YYYY-MM-DD".}

\item{compounding}{Compounding frequency: "monthly" (12x/year), "semiannual" (2x/year),
"quarterly" (4x/year), "annual" (1x/year), or "continuous"}

\item{max_iter}{Maximum iterations for root finding (default: 100)}

\item{tol}{Convergence tolerance (default: 1e-8)}
}
\value{
Annual yield as decimal (e.g., 0.0564 = 5.64\%)
}
\description{
Calculates the internal rate of return for a series of cash flows using
actual calendar dates and specified compounding convention. This is equivalent
to yield-to-maturity for bonds and effective yield for loan participations.
}
\details{
The 'amount' column in cf should represent cash flows received by the investor
(positive values). The initial investment (pv) is specified separately as a
positive number and should not be included in the cf data frame.

\strong{Choosing the compounding convention:}
The compounding frequency should match the market convention for quoting yields
on similar instruments, not necessarily the frequency of cash flows:
\itemize{
  \item Use "monthly" for mortgages, auto loans, and most consumer/commercial loans
  \item Use "semiannual" for US Treasury bonds and most corporate bonds
  \item Use "annual" for a simple effective annual rate
  \item The cash flows themselves can be irregular regardless of compounding choice
}

This function solves for the yield y that satisfies:
\deqn{PV = \sum_{i=1}^{n} \frac{CF_i}{(1 + y/m)^{m \times t_i}}}
where m is the compounding frequency and t_i is the time in years to cash flow i.

For continuous compounding:
\deqn{PV = \sum_{i=1}^{n} CF_i \times e^{-y \times t_i}}

Time periods are calculated using actual calendar days divided by 365.25.
}
\examples{
# Example 1: Loan participation with monthly cash flows
cash_flows <- data.frame(
  date = seq(as.Date("2025-02-01"), by = "month", length.out = 12),
  amount = rep(8500, 12)
)
yield.actual(cash_flows, pv = 100000, start_date = "2025-01-01")

# Example 2: Bond with semiannual coupons
bond_cfs <- data.frame(
  date = as.Date(c("2025-07-01", "2026-01-01", "2026-07-01")),
  amount = c(2500, 2500, 102500)
)
yield.actual(bond_cfs, pv = 98000, compounding = "semiannual")
}
\seealso{
\code{\link{irr}} for period-based IRR without dates

\code{\link{npv}} for net present value calculation
}
