import mechanize
import os
import cookielib
import datetime
from lxml import html
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
import collections
import calendar
import pandas
import urllib
import copy
from airbnb_utils import *
DEBUG = True

# Constants
kApiKey = 'd306zoyjsyarp7ifhu67rjxn52tv0t20'
kRoomTypes = ["Entire home/apt", "Private room", "Shared room"]


def monkeypatch_mechanize():
    """Work-around for a mechanize 0.2.5 bug. See: https://github.com/jjlee/mechanize/pull/58"""
    import mechanize
    if mechanize.__version__ < (0, 2, 6):
        from mechanize._form import SubmitControl, ScalarControl

        def __init__(self, type, name, attrs, index=None):
            ScalarControl.__init__(self, type, name, attrs, index)
            # IE5 defaults SUBMIT value to "Submit Query"; Firebird 0.6 leaves it
            # blank, Konqueror 3.1 defaults to "Submit".  HTML spec. doesn't seem
            # to define this.
            if self.value is None:
                if self.disabled:
                    self.disabled = False
                    self.value = ""
                    self.disabled = True
                else:
                    self.value = ""
            self.readonly = True

        SubmitControl.__init__ = __init__

class redshiftConnection:
    def __init__(self):
        self.host = "airbnbrdb.cvuvrunlpgtc.us-west-2.rds.amazonaws.com"
        self.user = "sean"
        self.port = 5432
        self.password = "waterice1"
        self.dbname = "airbnbdb"
        self.conn = psycopg2.connect(host=self.host, \
                                     user=self.user, \
                                     port=self.port, \
                                     password=self.password, \
                                     dbname=self.dbname)

    def runQuery(self, query, returnData = True):
        if returnData:
            cur = self.conn.cursor()
            cur.execute(query)
            dat = cur.fetchall()
            cur.close()
            return dat
        else:
            cur = self.conn.cursor()
            cur.execute(query)
            self.conn.commit()
            cur.close()

    def sendQuery(self, data, tableName, isChar = True, nLoop = 5):
        ## data should be array
        nrow = len(data)
        ncol = len(data[0])
        if isChar == True:
            isChar = [True] * ncol
        formatter = "(" + ",".join(["'%s'" if i else "%s" for i in isChar]) + "),"
        ## redshift could only take query with size 16M, break the query down to send to insights
        for k in range(nLoop):
            query = "insert into " + tableName + " values "
            for i in range(k * nrow / nLoop, (k + 1) * nrow / nLoop):
                insert = formatter % tuple(
                    re.sub("'", "''", str(q)) if p else q for p, q in zip(isChar, data[i]))
                query += insert
            query = query[:-1] + ';'
            self.runQuery(query, returnData = False)


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
#
    datesJson = json.loads(page.text)
    dailyInfo = []
#
    if "error_code" in datesJson:
        print "Do not have permission to access the data."
        return None
#
    datesMonth = datesJson['calendar_months']
    for i in range(len(datesMonth)):
        datesDay = datesMonth[i]['days']
        for j in range(len(datesDay)):
            row = {}
            row['date'] = datesDay[j]['date']
            row['rate'] = datesDay[j]['price']['native_price']
            row['availibility'] = datesDay[j]['available']
            row['listing_id'] = ListingID
            dailyInfo.append(row)
#
    return dailyInfo

def scrapeDailyStats(listingIds, date = None):
    result = [] 
    for i, listingId in enumerate(listingIds):
        result += getDailyInformation(listingId, date = date)
        print "Scrape %s page out of %s pages" % (i + 1, len(listingIds))
    return pandas.DataFrame(result)
        
def getSearchStats(page):
    tree = html.fromstring(page.content)
    text = tree.xpath('//div[@class = "map-search"]/@data-bootstrap-data')[0]
    text = re.sub("&quot;", '"', text)
    js = json.loads(text)
    searchResults = js["results_json"]["search_results"]
    numResults = len(searchResults)
    listings = []
    for i in searchResults:
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
        listing["beds"] = i["listing"]["beds"]
#        
        listings.append(listing)
    return listings

def getSearchUrl(city, roomType = 0, guests = 4, \
                  checkin = None, checkout = None, page = 1):
    params = {'checkin': checkin, 'checkout': checkout, 'guests': guests, \
              'room_types': kRoomTypes[roomType], 'page': page}
    paramsCopy = copy.deepcopy(params)
    for k, v in paramsCopy.iteritems():
        if v is None:
            params.pop(k, None)
#
    return kBaseUrlPrefix + city + '?' + urllib.urlencode(params)

def scrapeSearchStats(city, roomType = 0, guests = 4, \
                      checkin = None, checkout = None, page = 10):
    result = []
    for i in range(1, page + 1):
        url = getSearchUrl(city, roomType = roomType, guests = guests, \
                 checkin = checkin, checkout = checkout, page = i)
        pg = getPage(url)
        result += getSearchStats(pg)
        print "Scrape %s page out of %s pages" % (i, page)
    return pandas.DataFrame(result)

def scrapeSearchPage(city, guests = 4, checkin = "", checkout = "", page = 1):
    url = "https://www.airbnb.com/"
    br.open(url)
    br.select_form(nr = 0)
    br["location"] = city
    br["checkin"] = checkin
    br["checkout"] = checkout
    br["guests"] = [str(guests)]
    response = br.submit()
    sleep(3)

    if DEBUG:
        print "Filled the forms and Submit"
    urlPrefix = br.geturl()

    listingTable = []
    scrapeTable = []
    currentTime = str(datetime.datetime.now())[:19]

    scrapeTable.append(("DEFAULT",
                        city,
                        "NULL" if checkin == '' else "\'" + checkin + "\'",
                        "NULL" if checkout == '' else "\'" + checkout + "\'",
                        page,
                        guests,
                        currentTime))

    sql = "SELECT max(scrape_id) FROM scrape_plan;"
    r = redshiftConnection()
    isChar = [False, True, False, False, False, False, True]
    r.sendQuery(scrapeTable, "scrape_plan", isChar, 1)
    scrapeId = r.runQuery(sql)[0][0]

    for i in range(1, page + 1):
        pageUrl = "&page=" + str(i)
        url = urlPrefix + pageUrl
        if DEBUG:
            print "search page number =", i
        try:
            br.open(url)
        except:
            continue
        linkList = list(br.links())
        listDetail = collections.defaultdict(dict)
        for l in linkList:
            m = re.search(r"/(\d+)\?", l.url)
            if m:
                id = int(m.group(1))
                m1 = re.search(r"^\$(\d+)$", l.text)
                if m1:
                    listDetail[id]["Price"] = int(l.text[1:])
                    continue

                if l.text.endswith("review") or \
                   l.text.endswith("reviews"):
                    m1 = re.search("\xc2\xb7 (\d+) review", l.text)
                    listDetail[id]["Reviews"] = int(m1.group(1))
                    continue

        counter = 0
        for l in linkList:
            if l.text is not None and l.text != "" and l.text[0] == "$":
                counter += 1
                m = re.search(r"/(\d+)\?", l.url)
                id = int(m.group(1))
                listingTable.append((scrapeId,
                                     id,
                                     i,
                                     counter,
                                     listDetail[id]["Price"],
                                     0 if "Reviews" not in listDetail[id].keys() \
                                         else listDetail[id]["Reviews"]))

    r = redshiftConnection()
    isChar = [False, False, False, False, False, False]
    r.sendQuery(listingTable, "search_page", isChar, 1)

    return scrapeId, listingTable


def scrapeDetailPage(scrapeId, listingTable):
    finalResults = []
    counter = 0
    baseURL = 'https://www.airbnb.com/rooms/'
    listingId = [row[1] for row in listingTable]
    month = add_months(datetime.date.today(), -2).month
    year = add_months(datetime.date.today(), -2).year
    r = redshiftConnection()

    for listing in listingId:
        finalResults = []
        counter += 1
        if DEBUG:
            print 'Processing Listing %s out of %s' % (str(counter), str(len(listingId)))
        priceDict, availabilityDict = getDailyInformation(listing, month, year)

        for key in priceDict.keys():
            finalResults.append((scrapeId,
                                 listing,
                                 key,
                                 int(priceDict[key]),
                                 availabilityDict[key]))

        isChar = [False, False, True, False, False]
        r.sendQuery(finalResults, "detail_page", isChar, 1)

def saveToCsv(city, guests = 4, checkin = "", checkout = "", page = 10):
    filename = '../data/' + city + '_' + str(page) + '_pages'
    searchData = scrapeSearchStats(city, checkin = checkin, checkout = checkout, \
                                   guests = guests, page = page)
    listingIds = searchData['listing_id']
    dailyData = scrapeDailyStats(listingIds)
    searchData.to_csv(filename + "_search.csv")
    dailyData.to_csv(filename + "_daily.csv")
    
def main():
    city = "Houston-tx"
    saveToCsv(city, page = 1)

if __name__ == '__main__':
    main()

