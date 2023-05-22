# imports
import pandas as pd
import numpy as np

# define filename
fn1 = 'data/athlete_events.csv'
fn2 = 'data/noc_regions.csv'

# load data
df = pd.read_csv(fn1)
df_ = pd.read_csv(fn2)
df = pd.merge(left = df, right = df_)

# check out data
df.describe()


### data wrangling ========================================================

df.Age # select Age
df[['Age', 'Sport']] # select Age and Sport
df.drop(columns='Age') # suppress Age
df['Age2'] = (df.Age)**2 # create new variable, Age squared, called Age2
df[df.Age>40,] # filter on rows by Age value

# compute the number of instances of Year (with *count* method)
df.Year.value_counts()

# compute the number of instances of Year (without relying on *count* method)
df.groupby('Year').agg({'NOC': ['size', 'nunique'], 'Age': ['mean', 'std']})

# select 10 lines with highest age values, focusing then on variables Sport, Age, Year
indexes = df.Age.sort_values(ascending=False).index[0:4]
df.loc[indexes, ['Sport', 'Name', 'NOC', 'Age', 'Year']]


