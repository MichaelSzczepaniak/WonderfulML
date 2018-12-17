library(data.table, quietly = TRUE)


Address1 <- c("786, GALI NO 5, XYZ","rambo, 45, strret 4, atlast, pqr","23/4, 23RD FLOOR, STREET 2, ABC-E, PQR","45-B, GALI NO5, XYZ","HECTIC, 99 STREET, PQR")
AREACODE <- c('10','10','14','20','30')
Year1 <- c(2001:2005)

Address2 <- c("abc, pqr, xyz","786, GALI NO 4 XYZ","45B, GALI NO 5, XYZ","del, 546, strret2, towards east, pqr","23/4, STREET 2, PQR","abc, pqr, xyz","786, GALI NO 4 XYZ","45B, GALI NO 5, XYZ","del, 546, strret2, towards east, pqr","23/4, STREET 2, PQR")
Year2 <- c(2001:2010)
AREA_CODE <- c('10','10','10','20','30','40','50','61','64', '99')

data1 <- data.table(Address1, Year1, AREACODE)
data2 <- data.table(Address2, Year2, AREA_CODE)
data2[, unique_id := sprintf("%06d", 1:nrow(data2))]

v1=data1; v2=data2; ignore_case_ = FALSE; method_ = "dl"; max_dist_ = 99;
distance_col_ = "dist"

# data1 has 5 obs and data2 has 10, so he's expecting data1 to be recycled

match_fun_stringdist <- function(v1=data1$Address1, v2=data2$Address2,
                                 ignore_case_ = FALSE,
                                 method_ = "dl",
                                 max_dist_ = 99,
                                 distance_col_ = "dist") {
    
    if (ignore_case_) {
        v1 <- stringr::str_to_lower(v1)
        v2 <- stringr::str_to_lower(v2)
    }
    
    dists <- stringdist::stringdist(v1, v2, method = method_)
    
    ret <- dplyr::data_frame(include = (dists <= max_dist_))
    if (!is.null(distance_col_)) {
        ret[[distance_col_]] <- dists
    }
    
    return(ret)
}

# custom binary operator
'%sd%' <- function(var1, var2, ignore_case__, method__,
                   max_dist__, distance_col__) {
    match_fun_stringdist(var1, var2, ignore_case__, method__,
                         max_dist__, distance_col__)
}

# Call fuzzy_join
fuzzy_join(x = data1, y = data2, 
           by = list(x = c("Address1", "AREACODE", "Year1"),
                     y = c("Address2", "AREA_CODE", "Year2")),
           match_fun = list(`sd`, `==`, `<=`),
           mode = "left",
           ignore_case_ = FALSE,
           method_ = "dl",
           max_dist_ = 99,
           distance_col_ = "dist"
) %>%
  group_by(Address1, Year1, AREACODE) %>%
  top_n(1, -Address1.dist) %>%
  top_n(1, Year2) %>%
  select(unique_id, Address1.dist, everything())

