#' Data of California Counties by Census Tract
#'
#' The data is from the ACS 2016-2020 year
#'
#' @format A data frame with  1,095,480 rows and 5 columns:
#' \describe{
#'   \item{United Statues}{California}
#'   \item{data downloaded using the get_acs fuction from the tidycensus package}{get_acs}
#'   \item{2016}{2020}
#' }
#' @source U.S. Census
"countyDat"

#' Data of California Counties by Census Tract
#'
#' The data is from the ACS 2016-2020 year
#'
#' @format A data frame with  27,850 rows and 4 columns:
#' \describe{
#'   \item{United Statues}{California}
#'   \item{data downloaded using the get_acs fuction from the tidycensus package}{get_acs}
#'   \item{2016}{2020}
#' }
#' @source U.S. Census
"variables"


#' Data of California Counties by Census Tract
#'
#' The data is from the ACS 2016-2020 year
#'
#' @format A data frame with 18,262  rows and 6 columns:
#' \describe{
#'   \item{GEOID}{A number composed of State Code, first two digits, then County Code, next two digits, then Census Tract number}
#'   \item{NAME}{Desctiption of the Tract number}
#'   \item{variable}{Varible name code}
#'   \item{estimate}{Estimate of the variable}
#'   \item{moe}{Margin of Error of variable}
#'   \item{geometry}{geometry of the Census Tract}
#' }
#' @source U.S. Census
"countyDatMap"
