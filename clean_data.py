from os import write
import pandas as pd
import re

def news():
    # open csv file and load into pandas dataframe
    real = pd.read_csv('./data/True.csv')
    fake = pd.read_csv('./data/Fake.csv')

    real_text = [re.sub('[^A-Za-z0-9 \']+', '', row['title'])
                for _, row in real.iterrows()]
    fake_text = [re.sub('[^A-Za-z0-9 \']+', '', row['title'])
                for _, row in fake.iterrows()]

    # list of string to lowercase
    real_text = [x.lower().strip() for x in real_text]
    fake_text = [x.lower().strip() for x in fake_text]

    lim = 100
    real_text, fake_text = real_text[:lim], fake_text[:lim]

    print(sum([len(x.split()) for x in real_text]) + sum([len(x.split()) for x in fake_text]))

    with open('./data/fake_title_sm.txt', 'w') as f:
        f.write('\n'.join(fake_text))
    with open('./data/real_title_sm.txt', 'w') as f:
        f.write('\n'.join(real_text))

def heart():
    raw = pd.read_csv('./data/heart.csv')
    # count unique values in each column
    print(raw.nunique())

heart()