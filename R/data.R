#' Production and farm value of maple products in Canada
#'
#' @details The functions provided for you in this assignment will be using data from
#' the US National Highway Traffic Safety Administration's Fatality Analysis Reporting
#' System, which is a nationwide census providing the American public yearly data
#' regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @source National Highway Traffic Safety Administration.
#' \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#' @format A data frame with columns:
#' \describe{
#'  \item{STATE}{A value between 1 and 56 with exception {3,7,14,43,52}}
#'  \item{ST_CASE}{A value with 6 digits each one.}
#'  \item{VE_TOTAL}{A value between {1,2,3,4,5,6,17,7,8,12,22,11,15,9,90,14}.}
#'  \item{VE_FORMS}{A value between {1,2,3,4,5,6,17,7,8,12,22,11,15,9,90,14}}
#'  \item{PVH_INVL}{A value between 0 to 8 and 11}
#'  \item{PEDS}{A value between 0 to 5, 7 and 9.}
#'  \item{PERNOTMVIT}{A value between 0 and 9.}
#'  \item{PERMVIT}{A value between 0 and {25, 27,37,39,42,46,127}}
#'  \item{PERSONS}{A value between 0 and {25, 27,37,39,42,46,127}}
#'  \item{COUNTY}{A value between 0 and 999.}
#'  \item{CITY}{A value between 0 and 9999.}
#'  \item{DAY}{A value between 1 and 31.}
#'  \item{MONTH}{A value between 1 and 12}
#'  \item{YEAR}{The database will contain 2013, 2014 or 2015.}
#'  \item{DAY_WEEK}{A value between 1 and 7.}
#'  \item{HOUR}{A value between 0 and 23 included 99.}
#'  \item{MINUTE}{A value between 0 and 59 included 99}
#'  \item{NHS}{A value between {0,1,9}}
#'  \item{ROAD_FNC}{A value between 1 and 16, included 19 and 99.}#'
#'  \item{ROUTE}{A value between 1 and 9.}
#'  \item{TWAY_ID}{Contains some strings about names of directions.}
#'  \item{TWAY_ID2}{Contains some strings about names of directions.}
#'  \item{MILEPT}{A value between 0 and 99999.}
#'  \item{LATITUDE}{Contain some latitude in a spatial manner.}
#'  \item{LONGITUD}{Contain some longitud in a spatial manner.}
#'  \item{SP_JUR}{A value between 0 and 9.}
#'  \item{HARM_EV}{A value between 1 and 99.}#'
#'  \item{MAN_COLL}{Values between {0 , 1 , 2 , 6 , 7,  8,  9, 11, 98, 99}.}
#'  \item{RELJCT1}{A value in {0,1,8}.}
#'  \item{TYP_INT}{A value between 1 and 7 included 10, 98 and 99.}
#'  \item{WRK_ZONE}{A value between 0 and 4.}
#'  \item{REL_ROAD}{A value between 1 and 11 included 98 and 99.}
#'  \item{LGT_COND}{A value between 1 and 9}
#'  \item{WEATHER1}{A value between  1 and 12 included 98 and 99.}
#'  \item{WEATHER2}{A value between  1 and 12 included 99.}
#'  \item{WEATHER}{A value between  1 and 12 included 98 and 99.}
#'  \item{SCH_BUS}{A value in {0,1}.}
#'  \item{RAIL}{Some strings with 6 letters and a number.}
#'  \item{NOT_HOUR}{A value between 0 and 23 included 88 and 99.}
#'  \item{NOT_MIN}{A value between 0 and 59 included 88 and 99.}
#'  \item{ARR_HOUR}{A value between 1 and 23 and 99}
#'  \item{ARR_MIN}{A value between 0 and 59 included 88 and 99.}
#'  \item{HOSP_HR}{A value in {88,99}.}
#'  \item{HOSP_MN}{A value in {88,99}.}
#'  \item{CF1}{A value in {0,20}}
#'  \item{CF2}{Value 0.}
#'  \item{CF3}{Value 0.}
#'  \item{FATALS}{A value between 1 and 8.}
#'  \item{DRUNK_DR}{A value between 0 and 4.}
#' }
##
"accident_2013.csv.bz2"
"accident_2014.csv.bz2"
"accident_2015.csv.bz2"

