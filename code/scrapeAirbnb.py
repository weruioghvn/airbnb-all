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
import psycopg2
import re
import collections

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


# Browser
br = mechanize.Browser()


#learned necessary configuration from
#http://stockrt.github.io/p/emulating-a-browser-in-python-with-mechanize/

# Allow cookies
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)
br.set_proxies({"http": "167.114.104.163:3128"})

# Follows refresh 0 but not hangs on refresh > 0
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)
#specify browser to emulate
br.addheaders = [('User-agent',
'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

url = "https://www.airbnb.com/s/Las-Vegas--NV--United-States?guests=4"
br.open(url)

form = br.select_form(nr = 1)
br["checkin"] = "2016-06-01"
br["checkout"] = "2016-06-02"
br["guests"] = ["1"]
response = br.submit()
print response.read()

for link in br.links():
    print link.text, link.url

url = "https://www.airbnb.com/"
br.open(url)

form = br.select_form(nr = 1)
br["checkin"] = "2016-06-01"
br["checkout"] = "2016-06-02"
br["guests"] = ["1"]
response = br.submit()
print response.read()

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

    urlPrefix = br.geturl()

    listingTable = []
    currentTime = str(datetime.datetime.now())[:19]

    for i in range(1, page + 1):
        pageUrl = "&page=" + str(i)
        url = urlPrefix + pageUrl
        try:
            br.open(url)
        except:
            continue
        linkList = br.links()
        for l in linkList:
            print l.text, l.url

        counter = 0
        for l in linkList:
            if l.text is not None and l.text != "" and l.text[0] == "$":
                counter += 1
                m = re.search(r"/(\d+)\?", l.url)
                listingTable.append((int(m.group(1)),
                                     l.url,
                                     i,
                                     counter,
                                     "NULL" if checkout == '' else checkin,
                                     "NULL" if checkout == '' else checkin,
                                     guests,
                                     city,
                                     currentTime))

    r = redshiftConnection()
    isChar = [True, True, False, False, False, False, False, True, True]
    r.sendQuery(listingTable, "search_page", isChar, 1)




