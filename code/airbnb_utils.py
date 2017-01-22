# Utility functions for Airbnb investment project.

from lxml import html
import re
import requests
import numpy as np
import copy
import time

# Constants
kRoomUrlPrefix = "https://www.airbnb.com/rooms/"
kBaseUrlPrefix = "https://www.airbnb.com/s/"
kProxiesFile = "proxyList.txt"
kTimeOut = 4

# Get proxy list
proxyList = []
with open(kProxiesFile, "r") as f:
    while True:
        line = f.readline()
        if not line:
            break
        proxyList.append(line.strip())


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

def getListingUrl(listingId):
    return kRoomUrlPrefix + str(listingId)

def getPage(url, local = True):
    if local:
        while True:
            try:
                page = requests.get(url)
                return page
            except:
                pass
                

    while True:
        proxy = np.random.choice(proxyList)
        try:
            proxies = {'https': proxy}
            page = requests.get(url, proxies = proxies, 
                                timeout = (kTimeOut, kTimeOut))
            print proxy + " succeeded"
            return page
        except:
            print proxy + " failed"
            proxyList.remove(proxy)
            print "Proxy list has %s remaining" % str(len(proxyList))

def getCoordinates(page):
    floatPattern = "\s*(-?\d*.\d+)"

    def getCoordinate(shorthand):
        pattern = "offset_" + shorthand + "&quot;:" + floatPattern
        return float(re.search(pattern, page.text).group(1))
    
    latitude = getCoordinate("lat")
    longitude = getCoordinate("lng")
    return (latitude, longitude)

def getBaseUrl(city, guests, checkIn, checkOut, roomType, page = 1):
    guestsUrl = "guests=" + str(guests) + "&"
    
    page = "page=" + str(page) + "&"
