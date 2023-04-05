#' get data from ACS using tidycensus functions

#' @details get data from ACS using tidycensus functions
#' @importFrom tidycensus get_acs
#' @importFrom data.table fread rbindlist
#' @importFrom readxl read_excel
#' @examples
#'\dontrun{
#' getACSData()
#'  }
#' @export

getACSData <- function(){

if(FALSE){

  rm(list = ls()) #start with empty workspace

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

  censusVars <- data.frame(varlist = c('B19001_001', 'B01002_001', 'B03002_001', 'B03002_003', 'B03002_004', 'B03002_012', 'B05001_001', 'B05001_006',
                                       'B07001_001', 'B07001_017', 'B07001_033', 'B07001_049', 'B07001_065', 'B08014_001', 'B08014_002', 'B08014_003',
                                       'B08014_004', 'B08014_005', 'B08014_006', 'B08014_007', 'B08101_001', 'B08101_009', 'B08101_033', 'B08101_017',
                                       'B08101_025', 'B08101_041', 'B08101_049', 'B15003_001', 'B15003_002', 'B15003_003', 'B15003_004', 'B15003_005',
                                       'B15003_006', 'B15003_007', 'B15003_008', 'B15003_009', 'B15003_010', 'B15003_011', 'B15003_012', 'B15003_013',
                                       'B15003_014', 'B15003_015', 'B15003_016', 'B15003_017', 'B15003_018', 'B15003_019', 'B15003_020', 'B15003_021',
                                       'B15003_022', 'B15003_023', 'B15003_024', 'B15003_025', 'B19001_002', 'B19001_003', 'B19001_004', 'B19001_005',
                                       'B19001_006', 'B19001_007', 'B19001_008', 'B19001_009', 'B19001_010', 'B19001_011', 'B19001_012', 'B19001_013',
                                       'B19001_014', 'B19001_015', 'B19001_016', 'B19001_017', 'B19013_001', 'B23025_001', 'B23025_002', 'B25031_001',
                                       'B25077_001', 'B25034_001', 'B25034_002', 'B25034_003', 'B25034_004', 'B25034_005', 'B25034_006', 'B25034_007',
                                       'B25034_008', 'B25034_009', 'B25034_010', 'B25034_011', 'B25035_001', 'B25024_002', 'B25024_003', 'B25024_004',
                                       'B25024_005', 'B25024_006', 'B25024_007', 'B25024_008', 'B25024_009', 'B25024_010', 'B25024_011', 'B25003_001',
                                       'B25003_002', 'B25003_003', 'B01001_003', 'B01001_004', 'B01001_005', 'B01001_006', 'B01001_020', 'B01001_021',
                                       'B01001_022', 'B01001_023', 'B01001_024', 'B01001_025', 'B01001_027', 'B01001_028', 'B01001_029', 'B01001_030',
                                       'B01001_044', 'B01001_045', 'B01001_046', 'B01001_047', 'B01001_048', 'B01001_049', 'B22001_002',  'B01001_001',
                                       'B22003_003','B22003_004'),
                           varnames = c('totHH', 'medage', 'tot4race', 'wnh', 'bnh', 'hisp', 'tot4_cit', 'noncit', 'tot4_mv', 'no_mv',
                                        'in_cnty_mv', 'in_st_mv', 'out_st_mv', 'tot4_vehic', 'vehic0', 'vehic1', 'vehic2', 'vehic3', 'vehic4', 'vehic5pl',
                                        'tot4_comm', 'comm_sov', 'comm_walk', 'comm_pool', 'comm_trans', 'comm_oth', 'comm_wah', 'tot4_educ', 'educ_none',
                                        'educ_nurs', 'educ_kind', 'educ_1st', 'educ_2nd', 'educ_3rd', 'educ_4th', 'educ_5th', 'educ_6th', 'educ_7th', 'educ_8th',
                                        'educ_9th', 'educ_10th', 'educ_11th', 'educ_12th', 'educ_hs', 'educ_ged', 'educ_cnud1', 'educ_some', 'educ_asso',
                                        'educ_bach', 'educ_mast', 'educ_prof', 'educ_doct', 'hincund10', 'hinc1014', 'hinc1519', 'hinc2024',
                                        'hinc2529', 'hinc3034', 'hinc3539', 'hinc4044', 'hinc4549', 'hinc5059', 'hinc6074', 'hinc7599', 'hinc100124', 'hinc125149',
                                        'hinc150199', 'hinc200pl', 'medhhinc', 'tot4_work', 'emp', 'medrent', 'medhoval', 'totHU', 'HU2014', 'HU1013', 'HU9',
                                        'HU9099', 'HU8089', 'HU7079', 'HU6069', 'HU5059', 'HU4049', 'HU1939', 'medyrblt', 'HU_sfd', 'HU_sfa', 'HU_mf2', 'HU_mf3_4',
                                        'HU_mf5_9', 'HU_mf1019', 'HU_mf2049', 'HU_mf50pl', 'HU_mob', 'HU_boatRV', 'tot4_tenu', 'ownerHH', 'renterHH',
                                        'male0005', 'male0509', 'male1014', 'male1517', 'male6566', 'male6769', 'male7074', 'male7579', 'male8084',
                                        'male8500', 'female0005','female0509', 'female1014', 'female1517', 'female6566', 'female6769', 'female7074',
                                        'female7579', 'female8084', 'female8500','hRcvdSNAP','totPop','hhIncBelowPvrLevl','hhIncAtOrAbvPvrLevl'))

  mystate = "CA"
  mycounty = "Imperial"
  myyear2021 = 2021
  myyear = 2020
  mysurvey = "acs5"

  variables2021 <- load_variables(myyear2021, mysurvey, cache = TRUE)

  variables2020 <- load_variables(myyear, mysurvey, cache = TRUE)

  variables <- variables2021 %>%
    semi_join(censusVars %>% select(name=1))

  # curl::curl_options()

  require(R.utils)

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
        geometry = FALSE),
      substitute = FALSE,
      timeout = 60, onTimeout = "warning")
      , error=function(err) data.frame(GEOID = NA, NAME = NA))

    tmp_[[i]] <- x

  }

tmp_ %>%
  data.table::rbindlist(., fill = TRUE, idcol = "iteration") -> countyDat1

Sys.time() - startTime # Time difference of 25.2794 mins

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
      geometry = FALSE),
    substitute = FALSE,
    timeout = 60, onTimeout = "warning")
    , error=function(err) data.frame(GEOID = NA, NAME = NA))

  tmp_[[i]] <- x

}

tmp_ %>%
  data.table::rbindlist(., fill = TRUE, idcol = "iteration") -> countyDat2

Sys.time() - startTime # Time difference of 8.573738 mins

countyCodes <- countyCodes %>%
  anti_join(enframe(countyDat2$NAME %>% gsub(".*, (.*),.*","\\1",.) %>%
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
      geometry = FALSE),
    substitute = FALSE,
    timeout = 60, onTimeout = "warning")
    , error=function(err) data.frame(GEOID = NA, NAME = NA))

  tmp_[[i]] <- x

}

tmp_ %>%
  data.table::rbindlist(., fill = TRUE, idcol = "iteration") -> countyDat3

Sys.time() - startTime # Time difference of 25.2794 mins

data.table::rbindlist(list(countyDat2,
                           countyDat1,
                           countyDat3), fill = TRUE) -> countyDat

#check county data is extracted

if(nrow(orig_countyCodes %>%
        anti_join(enframe(countyDat$NAME %>% gsub(".*, (.*),.*","\\1",.) %>%
                          unique(), name = "name", value = "CountyName"))) ==0 ) message("all county data is there")


rm(list=setdiff(ls(), c("countyDat", "variables")))

gc()

countyDat2021 <- countyDat
# usethis::use_data_raw("countyDat2021")
# usethis::use_data_raw("variables")

}
}

