library(Quandl)
library(ggplot2)
library(dplyr)
library(XML)
library(tidyr)

Quandl.api_key("X1-ZWz1feae0myuiz_a80zd")

# Constants
kProjectDir <- "/media/sean/disk2/desktop/airbnb-invest"
kDataDir <- file.path(kProjectDir, "data/zillow_data/")
kZwsID <- "X1-ZWz1feae0myuiz_a80zd"
kSFHZipFile <- file.path(kDataDir, "miscellaneous", "single_family_house_price_by_zipcode.csv")
kCONZipFile <- file.path(kDataDir, "miscellaneous", "condo_price_by_zipcode.csv")

loadData <- function() {
    # Get area code from quandl API
    state_codes <<-
        read.csv(file.path(kDataDir,
                 "quandl_index/state_codes.csv"), header = TRUE, sep = "|",
                 quote = "\"", stringsAsFactors = FALSE)
    county_codes <<-
        read.csv(file.path(kDataDir,
                 "quandl_index/county_codes.csv"), header = TRUE,
                 sep = "|",  quote = "\"", stringsAsFactors = FALSE)
    metro_codes <<-
        read.csv(file.path(kDataDir,
                 "quandl_index/metro_codes.csv"), header = TRUE, sep = "|",
                 quote = "\"", stringsAsFactors = FALSE)
    hood_codes <<-
        read.csv(file.path(kDataDir,
                 "quandl_index/hood_codes.csv"), header = TRUE, sep = "|",
                 quote = "\"", stringsAsFactors = FALSE)
    city_codes <<-
        read.csv(file.path(kDataDir,
                 "quandl_index/city_codes.csv"), header = TRUE, sep = "|",
                 quote = "\"", stringsAsFactors = FALSE)

    # Metropolitan data
    metro_map <<- metro_codes$Code
    f <- function(x) sub(",", "", x)
    names(metro_map) <<- sapply(metro_codes$Region, f)

    # Zip Codes
    dt_sfh_zip <<- read.csv(kSFHZipFile)
    dt_con_zip <<- read.csv(kCONZipFile)
}

metro2Code <- function(..., type = "SF") {
    metro_name <- c(...)
    metro_names <- names(metro_map)
    for (m in metro_name) {
        if (!(m %in% metro_names)) {
            stop(sprintf("%s is not a valid name for a metropolitan", m))
        }
    }
    return(paste(sprintf("M%05d", metro_map[metro_name]), type, sep = "_"))
}

getZip <- function(zip, type = "sfh") {
    if (type == "con") {
        result <- dt_con_zip %>%
        filter(RegionName %in% zip) %>%
        gather(date, price, starts_with("X")) %>%
        mutate(date = as.Date(gsub("X(\\d{4}).(\\d{2})", "\\1-\\2-01", date, perl = TRUE))) %>%
        select(Date = date, Value = price)
    } else if (type == "sfh") {
        result <- dt_sfh_zip %>%
        filter(RegionName %in% zip) %>%
        gather(date, price, starts_with("X")) %>%
        mutate(date = as.Date(gsub("X(\\d{4}).(\\d{2})", "\\1-\\2-01", date, perl = TRUE))) %>%
        select(Date = date, Value = price)
    }
    
    return(result)
}

getData <- function(code, new = TRUE, dir = file.path(kDataDir, "downloaded")) {
  # if code starts with "Z" (zipcode data), it will only give you SFH data
  code_list <- list.files(path = dir)
  if (!new & (paste0(code, ".csv") %in% code_list)) {
    f <- file.path(dir, paste0(code, ".csv"))
    dat <- read.csv(file = f)
    return(dat)
  } else {
    f <- file.path(dir, paste0(code, ".csv"))
    if (!grepl("^Z", code)) {
        tryCatch(dat <- Quandl(paste0("ZILL/", code)),
                 error = function(e)
                 {stop(sprintf("%s is not available in zillow API", code))})
        write.csv(dat, file = f, row.names = FALSE)
        return(dat)
    } else {
        if (grepl("^Z(\\d{5})_SF$", code, perl = TRUE)) {
            tryCatch(dat <- getZip(gsub("^Z(\\d{5}).*$", "\\1", code, perl = TRUE), "sfh"),
                     error = function(e)
                     {stop(sprintf("%s is not available in zillow zipcode csv", code))})
            write.csv(dat, file = f, row.names = FALSE)
        } else if (grepl("^Z(\\d{5})_C$", code, perl = TRUE)) {
            tryCatch(dat <- getZip(gsub("^Z(\\d{5}).*$", "\\1", code, perl = TRUE), "con"),
                     error = function(e)
                     {stop(sprintf("%s is not available in zillow zipcode csv", code))})
            write.csv(dat, file = f, row.names = FALSE)
        }
        return(dat)
    }
  }
}

getPercentChange <- function(x) 100 * (x - 1)
plotPriceTrend <- function(codes, start_date, end_date) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    date_format <- "2000-01-01"
    
    if (!missing(end_date)) {
        l <- nchar(end_date)
        full_end_date <- paste0(end_date, substr(date_format, l + 1, 10))
        tryCatch(dat <- dat %>% filter(Date <= as.Date(full_end_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    if (!missing(start_date)) {
        l <- nchar(start_date)
        full_start_date <- paste0(start_date, substr(date_format, l + 1, 10))
        tryCatch(dat <- dat %>% filter(Date >= as.Date(full_start_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }
   
    g <- ggplot(aes(x = Date, y = Value, color = code), data = dat) + geom_line() +
        scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 18),
              title = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
        ylab("housing price (thousands)") + xlab("date") + ggtitle(title)
    g
}

plotPriceTrendWithBase <- function(codes, base = NULL) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    if (is.null(base)) {
        base_year <- data.frame(dat %>%
            filter(!is.na(Value)) %>%
            group_by(code) %>%
            summarise(Date = min(Date, na.rm = TRUE)) %>% ungroup %>%
            summarise(Date = max(Date)) %>%
            select(Date))[1, "Date"]
    } else {
        base_year <- base
    }
    dat <- dat %>%
        filter(Date >= as.Date(base_year)) %>%
        arrange(code, Date) %>%
        group_by(code) %>%
        mutate(Value = Value / first(Value)) %>% ungroup
    
    date_format <- "2000-01-01"
   
    g <- ggplot(aes(x = Date, y = Value, color = code), data = dat) + geom_line() +
    scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
    scale_y_continuous(limits = c(0, NA)) +
    theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 18),
              title = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
    ylab("housing price index") + xlab("date") + ggtitle(title)
    g
}

plotPriceTrendWithCalib <- function(codes, calib_number = 12) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    
    base_year <- data.frame(dat %>%
                            filter(!is.na(Value)) %>%
                            group_by(code) %>%
                            summarise(Date = min(Date, na.rm = TRUE)) %>% ungroup %>%
                            summarise(Date = max(Date)) %>%
                            select(Date))[1, "Date"]
    
    dat <- dat %>%
        filter(Date >= as.Date(base_year)) %>%
        arrange(code, Date) %>%
        group_by(code) %>%
        mutate(Value = Value / sum(ifelse(row_number() <= calib_number,
                                          Value, 0)) * calib_number) %>% ungroup
    
    date_format <- "2000-01-01"
   
    g <- ggplot(aes(x = Date, y = Value, color = code), data = dat) + geom_line() +
    scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
    scale_y_continuous(limits = c(0, NA)) +
    theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 18),
          title = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
    ylab("housing price index") + xlab("date") + ggtitle(title)
    g
}

plotAnnualRate <- function(codes, start_date, end_date) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    dat <- dat %>% filter(as.numeric(format(as.Date(Date), "%m")) == 1)
    date_format <- "2000-01-01"

    # Get data 12 months earlier
    dat_before <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() > 1) %>%
        ungroup() %>%
        select(-Date, -code) %>% 
        rename(value_before = Value)

    dat_after <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() <= n() - 1) %>%
        rename(value_after = Value)

    dat_combined <- cbind(dat_before, dat_after)
    dat_new <- dat_combined %>%
        mutate(percent_change = getPercentChange(value_after / value_before))
    
    
    if (!missing(end_date)) {
        l <- nchar(end_date)
        full_end_date <- paste0(end_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date <= as.Date(full_end_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    if (!missing(start_date)) {
        l <- nchar(start_date)
        full_start_date <- paste0(start_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date >= as.Date(full_start_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    
    g <- ggplot(aes(x = Date, y = percent_change, color = code), data = dat_new) +
        geom_line() +
        scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
        scale_y_continuous() +
        geom_abline(intercept = 0, slope = 0, alpha = 0.5) +
        theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 18),
              title = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
        ylab("annual increase rate (percent)") + xlab("date") + ggtitle(title)
    g
}

plotMonthRate <- function(codes, start_date, end_date) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    date_format <- "2000-01-01"

    # Get data 1 month earlier
    dat_before <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() > 1) %>%
        ungroup() %>%
        select(-Date, -code) %>% 
        rename(value_before = Value)

    dat_after <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() <= n() - 1) %>%
        rename(value_after = Value)

    dat_combined <- cbind(dat_before, dat_after)
    dat_new <- dat_combined %>%
        mutate(percent_change = getPercentChange(value_after / value_before))
    
    
    if (!missing(end_date)) {
        l <- nchar(end_date)
        full_end_date <- paste0(end_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date <= as.Date(full_end_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    if (!missing(start_date)) {
        l <- nchar(start_date)
        full_start_date <- paste0(start_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date >= as.Date(full_start_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    
    g <- ggplot(aes(x = Date, y = percent_change, color = code), data = dat_new) +
        geom_line() +
        scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
        scale_y_continuous() +
        geom_abline(intercept = 0, slope = 0, alpha = 0.5) +
        theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 18),
              title = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
        ylab("annual increase rate (percent)") + xlab("date") + ggtitle(title)
    g
}

getMetroInfo <- function() {
    dat <- data.frame()
    for (m in names(metro_map)[1:100]) {
        d <- data.frame(name = m, price = getData(metro2Code(m))[1, "Value"])
        dat <- rbind(dat, d)
    }

    write.csv(dat %>% arrange(price),
              file = file.path(kDataDir, "zillow_data/miscellaneous/metro_rank_by_price.csv"),
              row.names = FALSE)
}

getRentInfo <- function(address, citystatezip) {
    url <- sprintf("http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id=%1$s&address=%2$s&citystatezip=%3$s&rentzestimate=true",
                   kZwsID, URLencode(address), URLencode(citystatezip))
    dat <- xmlToList(xmlTreeParse(url, useInternal = TRUE))
    if (dat$message$code == "508") stop("Couldn't get the rent information!!")
    rent_est <- as.numeric(dat$response$results$result$rentzestimate$amount$text)
    rent_low <- as.numeric(dat$response$results$result$rentzestimate$valuationRange$low$text)
    rent_high <- as.numeric(dat$response$results$result$rentzestimate$valuationRange$high$text)
    return(c(rent_est, rent_low, rent_high))  
}

loadData()

main <- function() {
    plotPriceTrend(c("N00622_SF", "S00001_SF"))
    plotAnnualRate(c("S00001_SF", "S00002_SF"))
    plotMonthRate(c("M00022_SF"))
    plotPriceTrend(c("M00022_SF", "M00007_SF"))
    plotAnnualRate(c("M00022_SF", "M00007_SF"))
    plotPriceTrend(metro2Code("Little Rock AR"))
    getRentInfo("5075 Indian River Dr 173", "Las Vegas NV")

    zips <- c("Z95014_SF", "Z94555_SF", "Z94301_SF", "Z94536_SF", "Z94401_SF",
              "Z94601_SF", "Z94121_SF", "Z95070_SF", "Z95008_SF", "Z94002_SF",
              "Z94010_SF", "Z94043_SF")
    plotPriceTrend(zips)
    plotPriceTrendWithBase(zips)
    plotPriceTrendWithCalib(zips, 6)
    plotAnnualRate(zips)
    plotMonthRate(c("Z94301", "Z94303"))
}

# main()

#zip <- paste0("Z", (dt_sfh_zip %>% filter(Metro == "San Francisco"))$RegionName)
#plotPriceTrendWithBase(zip)
#ggplot(aes(x = as.factor(code), y = percent_change), data = dat_new) +
#    geom_boxplot() + geom_jitter() +
#    geom_hline(yintercept = 0, linetype = "dashed")

#d <- dat_new %>%
#    group_by(code) %>%
#    summarise(mean = mean(percent_change),
#              std = sd(percent_change))

#ggplot(aes(x = mean, y = std, label = code), data = d) +
#    geom_point() +
#    geom_text()
