#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 22 23:13:41 2022

@author: canferakbulut
"""
from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd

with open('subreddit-groupings.txt') as f:
    lines = f.readlines()

sub_groupings = [s.strip() for s in lines]

def make_urls(sub_groupings):
    base_url = "https://zissou.infosci.cornell.edu/convokit/datasets/subreddit-corpus/subreddit-corpus/subreddit-corpus/subreddit-corpus/subreddit-corpus/subreddit-corpus/subreddit-corpus/corpus-zipped/"
    urls = []
    for group in sub_groupings:
        x = base_url + group
        urls.append(x)
    return urls

def get_properties(url):
    html = urlopen(url).read()
    soup = BeautifulSoup(html, features="html.parser")

    # kill all script and style elements
    for script in soup(["script", "style"]):
        script.extract()    # rip it out

    # get text
    text = soup.get_text()

    # break into lines and remove leading and trailing space on each
    lines = (line.strip() for line in text.splitlines())
    # break multi-headlines into a line each
    chunks = (phrase.strip() for line in lines for phrase in line.split("  "))
    # drop blank lines
    text = '\n'.join(chunk for chunk in chunks if chunk)
    properties_list = text.split('\n')[2:]
    return(properties_list)

def properties_dic(properties_list):
    n = len(properties_list)
    prop_dic = {'sub_name': [properties_list[i] for i in range(0, n, 3)], 
               'date': [properties_list[i] for i in range(1, n, 3)],
               'mb': [properties_list[i] for i in range(2, n, 3)]}
    return prop_dic

def dict_to_df(prop_dic):
    sub_df = pd.DataFrame(prop_dic)
    sub_df['sub_name'] = sub_df['sub_name'].str.replace('\\.corpus\\.zip', '', regex=True)
    return(sub_df)

def main():
    urls = make_urls(sub_groupings)
    full_prop_list = []
    for url in urls:
        full_prop_list.extend(get_properties(url))
    prop_dic = properties_dic(full_prop_list)
    sub_df = dict_to_df(prop_dic)
    return sub_df

if __name__ == "__main__":
    main()
        