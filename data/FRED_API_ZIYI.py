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

# Class Fred
class Fred:
    
    def __init__(s):
        s.rooturl  = 'https://api.stlouisfed.org/fred/series/observations?' 
        s.api_key = '7531e59cd91f845d43fb1c7ac9535c57' 
        s.file_type = 'json'
        s.offset = 0
        s.units = 'lin'
        s.sord = 'asc'

    # GET DATA
    def getData(s, seriesid, start='', end='', limit=10000):
        
        # Packaging request
        params = [
            "api_key={0}".format(s.api_key),
            "series_id={0}".format(seriesid),
            "file_type={0}".format(s.file_type),
            "sort_order={0}".format(s.sord),
            "observation_start={0}".format(start),
            "offset={0}".format(s.offset),
            "limit={0}".format(limit),
            "units={0}".format(s.units),  
            "observation_end={0}".format(end)              
        ]
        rurl = s.rooturl + "&".join(params)            

        # Making call
        s.data = json.load(urllib.urlopen(rurl))
        
        return s.data        
        
    
    def cleanData(s):
        obs = s.data['observations']
        ts_index = pd.to_datetime([d['date'] for d in obs], infer_datetime_format=True)
        ts = pd.Series([d['value'] for d in obs], index=ts_index)
        ts = pd.to_numeric(ts)
        
        # Rolling to monthly
        ts_monthly = ts.resample('M').mean()
        cleaned = ts_monthly.interpolate(method='spline', order=3) #using cubic spline method
       
        return cleaned


def search(term):
        
    rooturl = 'https://api.stlouisfed.org/fred/series/search?search_text=' 
    ftype= 'json' # A json file type can be called like a dataframe in pandas (by its label)
    apikey = '7531e59cd91f845d43fb1c7ac9535c57' #This won't work until you update it
    
    url = '{0}{1}&file_type={2}&api_key={3}'.format(rooturl,term,ftype,apikey)
    data = json.load(urllib.urlopen(url))    

    results = data['seriess']
    pairs = [ (x['id'],x['title']) for x in results]
     
    return pairs


d = search('minnesota')
df = pd.DataFrame(d)
df.columns = ['ID', 'Title']
df = df.drop_duplicates(subset='Title')

# clear out already written file
file_path = '/Users/ziyilu/Desktop/data_list.csv'
f = open(file_path, "w+")
f.close()
df.to_csv(file_path, encoding='utf-8') # list of tuple pairs of ID and Titles


id_total_lst = []
term_total_lst = []
for x in d:
    id_total_lst += df
    

# create csv files for data
f = Fred()
directory = '/Users/ziyilu/Desktop/Data'
for i in df['ID']:
    id = i
    rawdata = f.getData(id, start = '1991-01-01', end = '2018-10-15')
    cleandata = f.cleanData()
    path = '{0}/{1}.csv'.format(directory,id)
    cleandata.to_csv(path, encoding='utf-8')
