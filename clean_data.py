from os import write
import pandas as pd
import re
# open csv file and load into pandas dataframe
real = pd.read_csv('./data/True.csv')
fake = pd.read_csv('./data/Fake.csv')

real_text = [re.sub('[^A-Za-z0-9 \']+', '', row['title'])
             for _, row in real.iterrows()]
fake_text = [re.sub('[^A-Za-z0-9 \']+', '', row['title'])
             for _, row in fake.iterrows()]

# list of string to lowercase
real_text = [x.lower() for x in real_text]
fake_text = [x.lower() for x in fake_text]

with open('./data/fake_title.txt', 'w') as f:
    f.write('\n'.join(fake_text[:5000]))
with open('./data/real_title.txt', 'w') as f:
    f.write('\n'.join(real_text[:5000]))
