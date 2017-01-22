# Script to scrape Airbnb page.
# Example:
#     python scrapeAirbnb.py --city "Austin TX"
#     Rscript show_metrics.R 'Austin TX'

import mechanize
import os
import cookielib
import datetime
from lxml import html
import numpy as np
import csv
import cStringIO
import codecs
from random import randint
from time import sleep
from lxml.etree import tostring
import bs4
import json
import random
import psycopg2
import re
import logging
import collections
import calendar
import pandas
import urllib
import copy
import sets
import argparse
import subprocess
from airbnb_utils import *
DEBUG = True

parser = argparse.ArgumentParser()
parser.add_argument('-c', '--city', type = str, required = True,
                    help = "City name with state code")
parser.add_argument('-p', '--page', type = int, required = False,
                    default = 80, help = "Number of pages to scrape")
parser.add_argument('-g', '--guests', type = int, required = False,
                    default = 4, help = "Number of guests to accommodate")
parser.add_argument('-s', '--sleep', type = int, required = False,
                    default = 0, help = "Sleep time between scrapes")
parser.add_argument('-r', '--radius', type = int, required = False,
                    default = 1, help = "Radius around the queried city (mile)")
feature_parser = parser.add_mutually_exclusive_group(required=False)
feature_parser.add_argument('--fixed-window', dest='fixed-window', action='store_true')
feature_parser.add_argument('--no-fixed-window', dest='fixed-window', action='store_false')
parser.set_defaults(fixed_window = False)

args = vars(parser.parse_args())

# Constants
kApiKey = 'd306zoyjsyarp7ifhu67rjxn52tv0t20'
kRoomTypes = ["Entire home/apt", "Private room", "Shared room"]
kFileDir = '../data/scrape_data/'
kSleep = args['sleep']
kLogging = 'debug.log'

logging.basicConfig(filename = kLogging, level = logging.DEBUG)

def addMonths(sourcedate, months):
    month = sourcedate.month - 1 + months
    year = int(sourcedate.year + month / 12 )
    month = month % 12 + 1
    day = min(sourcedate.day,calendar.monthrange(year,month)[1])
    return datetime.date(year,month,day)

def getDailyInformation(ListingID, date = None):
    # Function to get daily rate and availability information for specified
    # listing. 
    if date is None:
        currentDate = datetime.date.today()
        year = addMonths(currentDate, -1).year
        month = addMonths(currentDate, -1).month
    else:
        year = date.year
        month = date.month
#
    url = "https://www.airbnb.com/api/v2/calendar_months?key=" + kApiKey + \
          "&currency=USD&locale=en&listing_id=" + str(ListingID) + "&month=" + \
          str(month) + "&year=" + str(year) + "&count=3&_format=with_conditions"
#
    page = getPage(url)
    time.sleep(np.random.rand(1)[0] * kSleep)
#
    datesJson = json.loads(page.text)
    dailyInfo = []
#
    if "error_code" in datesJson:
        print "Do not have permission to access the data."
        return None
#
    datesMonth = datesJson['calendar_months']
    currentDates = []
    for i in range(len(datesMonth)):
        datesDay = datesMonth[i]['days']
        for j in range(len(datesDay)):
            if datesDay[j]['date'] not in currentDates:
                row = {}
                row['date'] = datesDay[j]['date']
                row['rate'] = datesDay[j]['price']['native_price']
                row['availability'] = datesDay[j]['available']
                row['listing_id'] = ListingID
                currentDates.append(row['date'])
                dailyInfo.append(row)
#
    return dailyInfo

def scrapeDailyStats(listingIds, date = None):
    result = [] 
    for i, listingId in enumerate(listingIds):
        result += getDailyInformation(listingId, date = date)
        print "Scrape %s list out of %s lists" % (i + 1, len(listingIds))
    return pandas.DataFrame(result)
        
def getSearchStats(page):
    tree = html.fromstring(page.content)
    text = tree.xpath('//div[@class = "map-search"]/@data-bootstrap-data')[0]
    # logging.debug(text[26400:26500])
    js = json.loads(text)
    with open("debug/prettified_json.txt", "wb") as f:
        f.write(json.dumps(js, indent = 4))
    searchResults = js["results_json"]["search_results"]
    numResults = len(searchResults)
    listings = []
    currentListings = []
    counter = 0
    for i in searchResults:
        counter = counter + 1
        if i["listing"]["id"] not in currentListings:
            listing = {}
            listing["rate"] = i["pricing_quote"]["rate"]["amount"]
            listing["guests"] = i["pricing_quote"]["guests"]
            listing["can_instant_book"] = i["pricing_quote"]["can_instant_book"]
            listing["reviews_count"] = i["listing"]["reviews_count"]
            listing["host_id"] = i["listing"]["primary_host"]["id"]
            listing["longitude"] = i["listing"]["lng"]
            listing["latitude"] = i["listing"]["lat"]
            listing["listing_id"] = i["listing"]["id"]
            listing["star_rating"] = i["listing"]["star_rating"]
            listing["room_type"] = i["listing"]["room_type"]
            listing["bedrooms"] = i["listing"]["bedrooms"]
            listing["counter"] = counter
            currentListings.append(listing["listing_id"])
            listings.append(listing)
    return listings

def getSearchUrl(city, roomType = 0, guests = 4, **kwargs):
    # Construct url to be scraped. Arguments could be checkin, checkout
    # sw_lat, sw_lon, ne_lat, ne_lon and so on.
    params = {'guests': guests, 'room_types': kRoomTypes[roomType]}
    params.update(kwargs)
    
    return kBaseUrlPrefix + city + '?' + urllib.urlencode(params)

def scrapeSearchStats(base_url, page = 10):
    # Scrape specific number of search pages based on base_url
    result = []
    for i in range(1, page + 1):
        url = base_url + "&page=" + str(i)
        pg = getPage(url)
        time.sleep(np.random.rand(1)[0] * kSleep)
        for row in getSearchStats(pg):
            row.update({"page": i})
            result.append(row)
        print "Scrape %s page out of %s pages" % (i, page)
    return pandas.DataFrame(result)

def scrapeAllAndDownload(filename, base_url, page = 10):
    # Scrape search pages and detailed pages together
    file_dir = kFileDir + filename
    
    searchData = scrapeSearchStats(base_url, page = page)
    listingIds = list(set(searchData['listing_id']))
    dailyData = scrapeDailyStats(listingIds)
    today = datetime.date.today().isoformat().replace("-", "")
    searchData.to_csv(file_dir + "_" + today + "_search.csv", index = False)
    dailyData.to_csv(file_dir + "_" + today + "_daily.csv", index = False)

def saveToCsv(city, page, guests, roomType = 0, fixed_window = False, radius = 1):
    if fixed_window == False:
        base_url = getSearchUrl(city.replace(' ', '-'), guests = guests, roomType = roomType)
    else:
        coords_str = subprocess.check_output(["Rscript", "get_boundary.R", city, str(radius)])
        base_url = getSearchUrl(city.replace(' ', '-'), guests = guests, 
                                roomType = roomType, **json.loads(coords_str))
    filename = city.replace(' ', '-') + '_' + str(page) + '_pages'
    print(base_url)
    scrapeAllAndDownload(filename, base_url, page)

def main():
    saveToCsv(args['city'], page = args['page'], guests = args['guests'],
              fixed_window = args['fixed-window'], radius = args['radius'])

if __name__ == '__main__':
    main()
