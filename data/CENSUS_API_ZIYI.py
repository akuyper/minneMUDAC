# -*- coding: utf-8 -*-
'''
Created on Oct 22, 2018
@author: Ziyi Lu

This python code collects json data from https://www.census.gov/data/developers/data-sets/acs-1year.html
example of urls: https://api.census.gov/data/2016/acs/acs1.html
definition of variables: https://api.census.gov/data/2016/acs/acs1/variables.html
census state code:https://www.census.gov/geo/reference/ansi_statetables.html
census cunty code:https://www.census.gov/geo/reference/codes/cou.html
get your Census API key from https://api.census.gov/data/key_signup.html
'''


from urllib import request
import json
import pandas as pd

# =============================================================================
# Create a list of vars to search on Census
# definition of variables can be searched by year 
# e.g. https://api.census.gov/data/2016/acs/acs1/variables.html

var_name_path = "~/Desktop/MinneMUDAC/Census_Variables_codebook.csv"
var_names = pd.read_csv(var_name_path)
var_idx_lst = var_names.iloc[:, 0].tolist()
var_to_search = ','.join(var_idx_lst)

# =============================================================================
# Fetching Census Data
census_api_key = '0659f23ef7e8130a5dcf5bc4cfedf19126fdc2e5' 
var_to_search = ','.join(var_idx_lst[:48])
for year in range(2014,2018):
    url_str = 'https://api.census.gov/data/' + str(year) + '/acs/acs1?get='+ var_to_search + ',NAME&for=county:*&in=state:*&key=' + census_api_key
    response = request.urlopen(url_str) # read the response into computer
    html_str = response.read().decode("utf-8") # convert the response into string
    json_data = json.loads(html_str) # convert the string into json
    df = pd.DataFrame(json_data[1:])
    df.columns = json_data[0]
    df['year'] = year
    df.to_csv(str(year) + '_MNByCountyCensusData.csv',encoding='utf-8')

# Census provides additional data on voters for year 2017
additional_var_to_search_2017 = ','.join(var_idx_lst[48:])
url_str = 'https://api.census.gov/data/2017/acs/acs1?get='+ additional_var_to_search_2017 + ',NAME&for=county:*&in=state:*&key=' + census_api_key
response = request.urlopen(url_str) 
html_str = response.read().decode("utf-8") 
json_data = json.loads(html_str)
df = pd.DataFrame(json_data[1:])
df.columns = json_data[0]
df['year'] = 2017
df.to_csv('2017voters_MNByCountyCensusData.csv',encoding='utf-8')