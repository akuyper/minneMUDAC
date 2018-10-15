#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 14 17:20:02 2018

@author: ziyilu
"""

import urllib
import json
import pandas as pd
import csv
import unicodecsv
import numpy as np 
import pandas_datareader.data as web
import datetime

def search(term):
        
    rooturl = 'https://api.stlouisfed.org/fred/series/search?search_text=' 
    ftype= 'json' # A json file type can be called like a dataframe in pandas (by its label)
    apikey = '7531e59cd91f845d43fb1c7ac9535c57' #This won't work until you update it
    
    url = '{0}{1}&file_type={2}&api_key={3}'.format(rooturl,term,ftype,apikey)
    data = json.load(urllib.urlopen(url))    

    results = data['seriess']
    pairs = [ (x['id'],x['title']) for x in results]
     
    return pairs

# Load in county names of MN
path = "~/Desktop/MinnesodaCountryName&ID.csv"
ct_names = pd.read_csv(path)
start = datetime.datetime(1991, 1, 1)
end = datetime.datetime(2018, 10, 15)

for i in range(ct_names.shape[0]):
    data_total = pd.DataFrame()
    
    county = ct_names.iloc[i,1]
    search_term = county + ', MN'
    d = search(search_term)
    df = pd.DataFrame(d)
    df.columns = ['ID', 'Title']
    df = df.drop_duplicates(subset='Title')
    id=df['ID']

    county_ts = web.DataReader(id, 'fred', start, end)
    
    # data cleaning
    
    county_ts.to_csv(county + '_Data.csv',encoding='utf-8')
