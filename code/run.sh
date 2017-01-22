#!/bin/bash

echo "" > output.txt

low=(
"Bozeman MT"
"Boise ID"
"Santa Fe NM"
"Reno NV"
"Salt Lake City UT"
"Phoenix AZ"
"New Orleans LA"
"Houston TX"
"Las Vegas NV"
"Colorado Springs CO"
"Austin TX"
"Portland OR"
"Sacramento CA"
"Denver CO"
"Seattle WA"
"San Francisco CA"
)


for i in "${low[@]}";
do
    echo python scrape_airbnb.py --city "$i"  
    python scrape_airbnb.py --city "$i" >>output.txt 2>&1
    echo Rscript show_metrics.R "$i"
    Rscript show_metrics.R "$i" >>output.txt 2>&1
done
