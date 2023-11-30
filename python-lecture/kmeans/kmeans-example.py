import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#import psycopg2
#conn_string = "host='localhost' dbname='imdb'\
#user='julia' password='k81&hDf5'"
#conn = psycopg2.connect(conn_string)

from sqlalchemy import create_engine
engine = create_engine('postgresql+psycopg2://julia:k81&hDf5\
@localhost/imdb')

conn = engine.connect()

sql_str = "select id, year, length from imdb.movie where year is not null and length is not null"
movies = pd.read_sql(sql_str, conn)
conn.close()

movies['year'] = pd.to_numeric(movies['year'])
print(movies.head())

random_clusters = np.random.randint(0, 4, size = movies.shape[0])
random_clusters

fig, ax = plt.subplots()
ax.scatter(movies['year'], movies['length'], c = random_clusters)
plt.tight_layout()
plt.show()

# Single point
movies.iloc[4][['year', 'length']].values

# All points
X = movies[['year', 'length']].to_numpy()
print(X)

# Return points which belong to cluster 0
indices_0 = [i for i, x in enumerate(random_clusters) if x == 0]
points_0 = X[indices_0]
print(points_0)

# Return points which belong to cluster 'c'
def get_points(data, clusters, label):
    indices = [i for i, x in enumerate(clusters) if x == label]
    return data[indices]

points_0 = get_points(X, random_clusters, 0)
print(points_0)

# Compute distance between two arrays
def distance(x, y):
    return ((x - y) ** 2).sum()

def RSS(clusters, data):
    rss = 0
    labels = set(clusters)
    for label in labels:
        cluster = get_points(data, clusters, label)
        x_avg = cluster.mean(axis = 0)
        for point in cluster:
            rss += distance(point, x_avg)
    return rss

RSS(random_clusters, X)

from UDFs import KMeans
km = KMeans(k = 4)
km.fit(X = X)