#' get data from ACS using tidycensus functions

#' @details get data from ACS using tidycensus functions
#' @importFrom tidycensus get_acs
#' @importFrom data.table fread rbindlist
#' @importFrom readxl read_excel
#' @importFrom R.utils withTimeout
#' @examples
#'\dontrun{
#' getACSData()
#'  }
#' @export

getACSData <- function(){

  if(FALSE){

    rm(list = ls()) ;gc()

    # library(tidycensus)
    # # library(tigris)
    # # library(sf)
    # # library(doBy)
    # library(data.table)

    tmp = tempfile(fileext = ".xlsx")

    download.file(url = "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/counties/totals/co-est2021-pop-06.xlsx",
                  destfile = tmp, mode="wb")

    tmp <- read_excel(tmp, col_names = FALSE
                      ,col_types =  "text",skip = 5)

    setnames(tmp, c("CountyName", "Base202004", "Base202007", "Base2021"))

    tmp <- na.omit (tmp, "Base202004")

    tmp <- tmp[, c("CountyName","Base2021")]

    tmp$CountyName <- tmp$CountyName %>% gsub("\\.|, California","",.)

    tmp$Base2021 <- tmp$Base2021 %>% as.numeric(.)

    countyCodes <- fread("https://www2.census.gov/geo/docs/reference/codes/files/st06_ca_cousub.txt") %>%
      .[, .(V1,V2,V3,V4)] %>%
      unique(.)

    setnames(countyCodes, c("State", "StateCode", "CountyCode", "CountyName"))

    countyCodes$CountyCode<- sprintf("%03d", countyCodes$CountyCode )

    tmp <- tmp[, c("CountyName",  "Base2021")]

    countyCodes <- merge(tmp, countyCodes, by="CountyName", all.data=TRUE)

    countyCodes <- setorder(countyCodes, -Base2021)

    rm(list=setdiff(ls(), c("countyCodes")))

    gc()

    census_api_key(Sys.getenv("CENSUS_API_KEY"), install=TRUE, overwrite=TRUE)

    censusVars <- data.frame(varlist = c('B19001_001', 'B01001_001'),
                             varnames = c('totHH', 'totPop'))

    mystate = "CA"
    mycounty = "Imperial"
    myyear2021 = 2021
    myyear = 2020
    mysurvey = "acs5"

    variables2021 <- load_variables(myyear2021, mysurvey, cache = TRUE)

    variables2020 <- load_variables(myyear, mysurvey, cache = TRUE)
    variables <- variables2020 %>%
      semi_join(censusVars %>% select(name=1))

    # curl::curl_options()

    # require(R.utils)

    startTime <- Sys.time()

    tmp_ <- list()

    for(i in seq_along(countyCodes$CountyCode)){

      timediff <- Sys.time() - startTime

      print(paste0(countyCodes$CountyName[[i]],"--",
                   i," of ", length(countyCodes$CountyCode)
                   ," "
                   ,as.double(timediff, units = "auto") %>% signif(.,3)
                   ," "
                   ,units(timediff) ))

      x <- tryCatch( withTimeout(
        get_acs(
          geography = "tract",
          state = mystate,
          cache_table = TRUE,
          county = countyCodes$CountyName[[i]],
          variables = censusVars$varlist %>% sort,
          year = myyear,
          survey = mysurvey,
          geometry = TRUE),
        substitute = FALSE,
        timeout = 60, onTimeout = "warning")
        , error=function(err) data.frame(GEOID = NA, NAME = NA))

      tmp_[[i]] <- x

    }

    tmp_ %>%
      data.table::rbindlist(., fill = TRUE, idcol = "iteration") -> countyDat1

    Sys.time() - startTime # Time difference of 9.201096 secs

    orig_countyCodes <- countyCodes

    countyCodes <- countyCodes %>%
      anti_join (enframe(countyDat1$NAME %>% gsub(".*, (.*),.*","\\1",.) %>%
                           unique(), name = "name", value = "CountyName"))

    startTime <- Sys.time()

    tmp_ <- list()

    for(i in seq_along(countyCodes$CountyCode)){

      timediff <- Sys.time() - startTime

      print(paste0(countyCodes$CountyName[[i]],"--",
                   i," of ", length(countyCodes$CountyCode)
                   ," "
                   ,as.double(timediff, units = "auto") %>% signif(.,3)
                   ," "
                   ,units(timediff) ))

      x <- tryCatch( withTimeout(
        get_acs(
          geography = "tract",
          state = mystate,
          cache_table = TRUE,
          county = countyCodes$CountyName[[i]],
          variables = censusVars$varlist %>% sort,
          year = myyear,
          survey = mysurvey,
          geometry = TRUE),
        substitute = FALSE,
        timeout = 60, onTimeout = "warning")
        , error=function(err) data.frame(GEOID = NA, NAME = NA))

      tmp_[[i]] <- x

    }

    tmp_ %>%
      data.table::rbindlist(., fill = TRUE, idcol = "iteration") -> countyDat2

    Sys.time() - startTime #Time difference of 18.2565 secs

    data.table::rbindlist(list(countyDat2,
                               countyDat1), fill = TRUE) %>%
      select(-iteration) -> countyDatMap

    #check county data is extracted

    if(nrow(orig_countyCodes %>%
            anti_join(enframe(countyDat$NAME %>% gsub(".*, (.*),.*","\\1",.) %>%
                              unique(), name = "name", value = "CountyName"))) ==0 ) message("all county data is there")


    rm(list=setdiff(ls(), c("countyDatMap")))

    gc()

    # usethis::use_data(countyDatMap, overwrite = TRUE, compress = "xz")
    #  tools::checkRdaFiles("data/countyDatMap.rda")

  }
}

