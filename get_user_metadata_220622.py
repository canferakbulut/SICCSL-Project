#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 22 18:24:56 2022

@author: canferakbulut
"""
import praw, prawcore
import pandas as pd
import os
import numpy as np
import datetime

user_list = pd.read_csv("<Path_Name>")

def get_credentials():
    # in terminal, run: 
        # os.environ['client_id'] = <your_client_id>
        # os.environ['client_secret'] = <your_client_secret>
        # os.environ['user_agent'] = <your_user_agent>
    client_id = os.environ.get("client_id")
    client_secret = os.environ.get("client_secret")
    user_agent = os.environ.get("user_agent")
    reddit = praw.Reddit(client_id=client_id, 
                     client_secret=client_secret, 
                     user_agent=user_agent)
    
    return reddit

def get_user_meta(user_list, reddit):
    meta_dict = {'username': user_list, 'karma': [], 'created_utc': []}
    for username in user_list:
        user = reddit.redditor(username)
        
        try:
            user.comment_karma
        except prawcore.exceptions.NotFound:
            meta_dict['karma'].append(np.nan)
            meta_dict['created_utc'].append(np.nan)
        else:
            meta_dict['karma'].append(user.comment_karma)
            meta_dict['created_utc'].append(user.created_utc)
            
    meta_df = pd.DataFrame(meta_dict)
    
    return meta_df

def main():
    reddit = get_credentials()
    meta_df = get_user_meta(user_list, reddit)
    return meta_df


if __name__ == "__main__":
    main()
        
    
    