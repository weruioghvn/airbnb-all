# This script is dedicated to analyze scraped Airbnb webpages in order
# to provide some insights about Airbnb pricing, Airbnb location and
# even daily monitor dashboards. 

library(dplyr)
library(ggplot2)
library(RgoogleMaps)
library(ggmap)
library(ggrepel)
library(maps)
library(sp)
library(gstat)
library(gridExtra)
library(RPostgreSQL)
library(reshape2)
library(xtable)
library(RcppRoll)
library(scales)

setwd("/media/sean/disk2/desktop/airbnb-invest/code")
source("estimated_return.R", local = FALSE)
source("zillow_price.R", local = FALSE)

args <- commandArgs(TRUE)

# Constants
kCityName <- args[1]
kCityNameUnderscored <- gsub(" ", "-", kCityName)
kZoom <- 11
kMonthDays <- 30
kProjectDir <- "/media/sean/disk2/desktop/airbnb-invest"
kPlotDir <- file.path(kProjectDir, paste0("plot/", kCityNameUnderscored))

# Command line 
setwd(kProjectDir)
system(sprintf("rm -r %s", kPlotDir))
system(paste0("mkdir -p ", kPlotDir))

getMostRecentFile <- function(){
    dir <- paste0(kProjectDir, "/data/scrape_data/")
    filenames <- list.files(path = dir)
    re <- sprintf("^%1$s_80_pages_\\d{8}_search.csv|^%1$s_80_pages_\\d{8}_daily.csv",
                  kCityNameUnderscored)
    matched <- grep(re, filenames, perl = TRUE, value = TRUE)
    dates <- gsub(".*_(\\d{8})_.*.csv", "\\1", matched)
    targeted <- sort(matched[dates == max(dates)])
    if (length(targeted) != 2) stop("Should only return 1 search data and 1 daily data!!")
    result <- list(daily = file.path(dir, targeted[1]),
                   search = file.path(dir, targeted[2]))
    return(result)
}
kDailyFilename <- getMostRecentFile()$daily
kSearchFilename <- getMostRecentFile()$search


loadData <- function() {
    # daily data
    datDaily <- unique(read.csv(kDailyFilename))
    datDaily$date <- as.Date(datDaily$date)
    datDaily <- datDaily %>%
        mutate(occupied = ifelse(availability == "False", 1, 0)) %>%
        select(-availability)
    # search data
    datSearch <- read.csv(kSearchFilename)
    datSearch$base_rate <- datSearch$rate
    datSearch$rate <- NULL
    datSearch <- datSearch %>%
        arrange(page, counter) %>%
        select(page, counter, everything())

    datReview <<- datSearch %>% select(listing_id, reviews_count)
    datDaily <<- datDaily
    datSearch <<- datSearch
}

loadData()

# Utils
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

simpleUnderscore <- function(x) {
    return(gsub(" ", "_", tolower(x)))
}

flatten2String <- function(x) {
    result <- ""

    f <- function(name) sprintf("%s(%s)", name, x[[name]])
    paste(sapply(names(x), f), collapse = " ")
}

communicateData <- function(dat_daily, dat_search) {
    dat_daily <- dat_daily %>% filter(listing_id %in% dat_search$listing_id)
    dat_grouped <- dat_daily %>%
        group_by(listing_id) %>%
        summarise(rate = mean(rate),
                  occupancy_rate = mean(occupied),
                  occupied_days = kMonthDays * occupancy_rate)
    
    dat_search <- merge(dat_search, dat_grouped, all = TRUE) %>% arrange(page, counter)
    return(list(dat_daily = dat_daily,
                dat_search = dat_search))
}

# Evaluation Plots
getFilteredData <- function(search_filter, daily_filter) { 
    dscopy <- datSearch
    for (filter in search_filter) {
        dscopy <- filter(dscopy)
    }
    ddcopy <- datDaily
    for (filter in daily_filter) {
        ddcopy <- filter(ddcopy)
    }
    result <- list(dat_search = dscopy, dat_daily = ddcopy)
    return(result)
}

getFilteredDataByKey <- function(...) {
    search_filter <- list()
    daily_filter <- list()
    filters <- list(...)
    for (key in names(filters)) {
        if (key == "dedup") {
            if (filters[[key]] == TRUE) {
                search_filter <- c(search_filter, list(dedupFilter()))
            }
            next
        }

        if (key == "review") {
            search_filter <- c(search_filter, list(reviewFilter(filters[[key]])))
            next
        }

        if (key == "spam") {
            daily_filter <- c(daily_filter, list(spamFilter(filters[[key]])))
            next
        }
    }
    
    return(do.call(getFilteredData, args = list(search_filter = search_filter,
                                         daily_filter = daily_filter)))
}



# Filters
# Search Filter
reviewFilter <- function(reviewLb = 0) {
    force(reviewLb)
    function(x) {
        result <- x %>% filter(reviews_count >= reviewLb)
        return(data.frame(result))
    }
}

# Search Filter
dedupFilter <- function() {
    function(x) {
        if (length(setdiff(c("page", "counter", "listing_id"),
                           colnames(x))) != 0) {
            stop("Wrong data frame to filter!!")
        }
        result <- x %>%
            group_by(listing_id) %>%   
            arrange(page, counter) %>%
            filter(row_number() == 1) %>%
            ungroup %>%
            arrange(page, counter)
        return(data.frame(result))
    }
}

# Daily Filter
spamFilter <- function(n = 0) {
    force(n)  # R function will by lazy evaluated if not force n to materialize
    function(x) {
        if (n > nrow(x) | n == 0) return(data.frame(x))
        expr <- quote(.[c(rep(TRUE, n - 1),
                          !(roll_sum(.$occupied, n) == n)),])
        dat_filtered <- x %>%
            group_by(listing_id) %>%
            arrange(date) %>%
            do(eval(expr)) %>%
            ungroup
        return(data.frame(dat_filtered))
    }
}

plotRateQuantileByDay <- function(percent, ...) {
    # Function that plot rate by date. For given date, it calculates all quantiles
    # for given percentiles in the given listing rates on that day.
    #
    # Takeaways:
    #     1. Shows the rate trends for each percentile. Each percentile could be
    #        treated as listings at certain quality tier.
    #     2. Percentile is relatively robust to outliers.

    dat_full <- do.call(communicateData, getFilteredDataByKey(dedup = TRUE, ...))
    dat_search <- dat_full$dat_search
    dat_daily <- dat_full$dat_daily
    
    title = sprintf("rate quantile by day with filter %s", flatten2String(list(...)))
    percent.truncated = round(percent, 2)
    colNames = paste0("quantile", floor(100 * percent.truncated))
    dat <- dat_daily %>%
        group_by(date) %>%
        do(as.data.frame(as.list(quantile(.$rate, percent.truncated))))
    colnames(dat) <- c("date", colNames)
    dat.melted <- melt(dat, id.vars = c("date"))
    g <- ggplot(aes(x = date, y = value, col = variable), data = dat.melted) +
        geom_line() +
        ggtitle(simpleCap(title))
    ggsave(filename = file.path(kPlotDir, paste0(simpleUnderscore(title), ".png")), g,
           width = 10, height = 5)
}

getLikeliForFTL <- function() {
    # Get likelihood for each listing on whether it's a full time listing or not.
}

plotOcpyByDay <- function() {
    # Get empirical occupancy rate by day.
    #
    # Goal:
    #     1. variation of occupancy rate by day of week.

    title = "Average Occupied Days By Day"
    tbl <- getFilteredData(TRUE, 1)

    g <- ggplot(aes(x = date, y = occupied_days), data = tbl) +
        geom_line() +
        geom_abline(intercept = kMonthDays) +
        ggtitle(simpleCap(title)) +
        scale_x_date(date_breaks = "1 month") +
        scale_y_continuous(limits = c(0, kMonthDays)) +
        theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 13))
}

plotRateRandomByDay <- function(k) {
    # Same as plotRateQuantileByDay except we pick k random listings from the pool
    # so as to illustrate the pattern of rate trend of actual listings. 
}

bucketizeByRatePercentile <- function(percent) {
    # The purpose of this function is to give you a better understanding of listings
    # at each percentile level by showing you all the listing IDs at that level. It
    # lets you evaluate the predicted daily rate of your potential property by putting
    # it to the right bucket.
}

bucketizeByOcpyPrecentile <- function(percent) {
    # Same as bucketizeByRatePerentile except you look at occupancy this time.
}

getReviewDistribution <- function() {
    # Show distribution of number of reviews. Estimate serious host.

    title <- "host percentage and counts based on number of reviews"
    dat_full <- do.call(communicateData, getFilteredDataByKey(dedup = TRUE))
    dat_search <- dat_full$dat_search
    tbl <- dat_search %>% mutate(bucketizedReviews =
                       ifelse(reviews_count == 0, "0",
                       ifelse(reviews_count <= 10, "0 < reviews <= 10",
                       ifelse(reviews_count <= 20, "10 < reviews <= 20",
                              "20 < reviews"))))
    result <- tbl %>%
        group_by(bucketizedReviews) %>%
        summarise(count = n(),
                  freq = n() / nrow(tbl))
    
    resultXtable <- xtable(result, caption = title)
    print(resultXtable, file = paste0(kPlotDir, "/", simpleUnderscore(title), ".html"),
          type = "html")
}

showListingTable <- function(..., append = FALSE) {
    # Show features per listing in a tabular form.

    title = sprintf("Listing table with filter %s", flatten2String(list(...)))
    dat_full <- do.call(communicateData, getFilteredDataByKey(...))
    dat_search <- dat_full$dat_search
    dat_daily <- dat_full$dat_daily
    resultXtable <- xtable(dat_search, caption = title)
    cat(print(resultXtable, type = "html"),
        file = paste0(kPlotDir, "/", simpleUnderscore(title), ".html"),
          append = append)
}

showSummaryStats <- function(percent, ..., append = TRUE) {
    # Show a bunch of tables
    #
    # Tables to Show:
    #     1. Quantile of rate
    #     2. Quantile of occupied days and occupancy rate
    #     3. Quantile of monthly gross income

    dat_full <- do.call(communicateData, getFilteredDataByKey(dedup = TRUE, ...))
    dat_search <- dat_full$dat_search
    dat_daily <- dat_full$dat_daily
    title = sprintf("Listing table with filter %s and sample size %s", flatten2String(list(...)), nrow(dat_search))
    
    result <- dat_search %>%
        do(data.frame(
            percent = percent,
            rate = quantile(.$rate, percent, names = FALSE),
            occupied_days = quantile(.$occupied_days, percent, names = FALSE),
            occupancy_rate = quantile(.$occupancy_rate, percent, names = FALSE),
            monthly_income = quantile(.$occupied_days * .$rate, percent, names = FALSE)))
   
    resultXtable <- xtable(result, caption = title)
    cat(print(resultXtable, type = "html"), "<br><br>",
         file = paste0(kPlotDir, "/", simpleUnderscore("Summary Statistics"), ".html"),
        append = append)
}

getMap <- function(data, google = TRUE) {
    if (!missing(data)) {
        
        bbox <- make_bbox(lon = lon, lat = lat, data, f = 0.10)
        if (google) {
            map <- get_map(bbox, maptype = "roadmap", source = "google")
        } else {
            map <- get_map(bbox, source = "osm")
        }
        
    } else {
        if (kCityName %in% us.cities$name) {
            print("City coordinates provided by data.frame us.cities")
            city_coord <- us.cities %>% filter(name == kCityName) %>% select(lat, lon = long)
        } else {
            print("City coordinates provided by geocode")
            city_coord <- geocode(kCityName)
        }
        
        map <- get_googlemap(center = c(city_coord$lon, city_coord$lat),
                             zoom = 10, maptype = 'roadmap')
    }
    return(map)
}

showMapPlots <- function(auto = TRUE, google = TRUE) {
    title = sprintf("Maps with args %s", flatten2String(as.list(environment())))
    dat_full <- do.call(communicateData, getFilteredDataByKey(dedup = TRUE))
    dat <- dat_full$dat_search
    
    if (auto) {
        map <- getMap(data = dat %>% select(lon = longitude, lat = latitude),
                      google = google)
    } else {
        map <- getMap()
    }

    midRate <- median(dat$rate)
    rateLimits <- c(0, 2 * midRate)
    midOccupancy <- median(dat$occupied_days)
    occupancyLimits <- c(max(0, 2 * midOccupancy - kMonthDays),
                         min(kMonthDays, 2 * midOccupancy))
    # Rate
    e <- parent.frame(n = 2)
    e$gg1 <- ggplot(aes_string(y = "latitude", x = "longitude", color = "rate"), data = dat)   
    g1 <- ggmap(map, base_layer = gg1, alpha = 0.1, darken = c(0.2, 'black')) +
        geom_point(alpha = 1, size = 3) +
        scale_colour_gradient2(midpoint = midRate,
                               low = 'blue', high = 'red', mid = 'white',
                               limits = rateLimits) +
        theme(legend.position = 'top',
              legend.key.width = unit(2, 'cm'))

    # Occupied Days
    e$gg2 <- ggplot(data = dat, aes(y = latitude, x = longitude, color = occupied_days))
    g2 <- ggmap(map, base_layer = gg2, alpha = 0.1, darken = c(0.3, 'black')) +
        geom_point(alpha = 1, size = 3) +
        scale_colour_gradient2(midpoint = midOccupancy,
                               low = 'blue', high = 'red', mid = 'white',
                               limits = occupancyLimits) +
        theme(legend.position = 'top',
              legend.key.width = unit(2, 'cm'))

    g <- grid.arrange(g1, g2, ncol = 2)
    ggsave(filename = file.path(kPlotDir, paste0(simpleUnderscore(title), ".png")), g,
           width = 16, height = 8)
}


plotRateToOccupiedDays <- function(ylimits = c(NA, 250)) {
    g <- ggplot(aes(x = occupied_days, y = rate, label = listing_id), data = dailyAggr) +
        geom_point() +
        geom_text(hjust = 0, nudge_x = 0.1) +
        scale_y_continuous(limits = ylimits) +
        ggtitle("Daily Rate VS Occupied Days")
    return(g)
}

plotOccupiedDaysByDay <-function(...) {
    title = sprintf("occupancy trend with filter %s", flatten2String(list(...)))
    dat_full <- do.call(communicateData, getFilteredDataByKey(dedup = TRUE, ...))
    dat_search <- dat_full$dat_search
    dat_daily <- dat_full$dat_daily
    
    dat <- dat_daily %>%
        group_by(date) %>%
        summarise(n = n(),
                  occupied = mean(occupied)) %>%
        ungroup

    dat$weekend <- weekdays(dat$date) %in% c("Friday", "Saturday")
    
    g <- ggplot(aes(x = date, y = occupied), data = dat) +
        geom_step() +
        scale_x_date(date_breaks = "1 week", labels = date_format("%m/%d"),
                     minor_breaks = NULL) +
        geom_vline(xintercept = as.numeric(Sys.Date()), color = "red", alpha = 0.5) +
        geom_step(aes(y = weekend * 1), alpha = 0.5) +
        theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 13))
    ggsave(filename = file.path(kPlotDir, paste0(simpleUnderscore(title), ".png")), g,
           width = 10, height = 5)
}

subdivideSFH <- function(...) {
    title <- sprintf("Investment Return For SFH subdivision with filter %s", flatten2String(list(...)))
    dat_full <- do.call(communicateData, getFilteredDataByKey(dedup = TRUE, ...))
    dat_search <- dat_full$dat_search
    dat_daily <- dat_full$dat_daily

    rate <- median(dat_search$rate)
    occupiedDays <- median(dat_search$occupied_days)
    price <- tryCatch(getData(metro2Code(kCityName))$Value[1],
             error = function(cond) {
                 message(sprintf("Housing Data is not available for %s",
                                 kCityName))
                 return(0)
             })
    if (price == 0) return
    price_per_unit <- price / 2
    # New instance
    inv <- new("Investment",
               price = price_per_unit,
               down = 0.3,
               furniture = 8000,
               renovation = 10000,
               apr = 0.038,
               year = 30,
               taxRate = 0.027,
               maintenanceFee = 150,
               hoa = 0,
               occupiedDays = occupiedDays,
               rate = rate,
               priceLift = 0.00,
               investYears = 5)

    price_lb <- price_per_unit / 2
    price_ub <- price_per_unit * 2
    g <- plotPriceRange(inv, price_lb, price_ub, hoas = 0,
                        downs = c(0.2, 0.3, 1))
    ggsave(filename = file.path(kPlotDir, paste0(simpleUnderscore(title), ".png")), g,
           width = 10, height = 5)
}

main <- function() {
    plotRateQuantileByDay(seq(0.1, 0.9, by = 0.1), review = 10)
    plotRateQuantileByDay(seq(0.1, 0.9, by = 0.1), review = 1)
    
    showListingTable(dedup = FALSE)
    showListingTable(dedup = TRUE, review = 0)
    showListingTable(dedup = TRUE, review = 1)
    showListingTable(dedup = TRUE, review = 5, spam = 7)
    showListingTable(dedup = TRUE, review = 10)
    
    showSummaryStats(seq(0.1, 0.9, by = 0.1), review = 0, append = FALSE)
    showSummaryStats(seq(0.1, 0.9, by = 0.1), review = 1)
    showSummaryStats(seq(0.1, 0.9, by = 0.1), review = 5, spam = 7)
    showSummaryStats(seq(0.1, 0.9, by = 0.1), review = 5)
    showSummaryStats(seq(0.1, 0.9, by = 0.1), review = 10)
    
    getReviewDistribution()
    plotOccupiedDaysByDay(spam = 7, review = 1)

    subdivideSFH(review = 5)
    
    showMapPlots(google = FALSE)
    showMapPlots(auto = FALSE)
}

main()
