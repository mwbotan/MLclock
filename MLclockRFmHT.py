import pandas as pd
import numpy as np
import math
from sklearn.ensemble import RandomForestRegressor
from sklearn.multioutput import MultiOutputRegressor
from sklearn.model_selection import RandomizedSearchCV
from scipy.stats import randint as sp_randint
from sklearn.metrics.scorer import make_scorer
import pickle

# data preparation
x = pd.read_csv('rf.train.txt')
x_copy = x
x = x.sample(frac=1,random_state=20)
y_copy = x_copy['hour']
y_copy.reset_index(drop=True, inplace=True)
y = x['hour']
y_cos = np.cos(2*np.pi*y/24)
y_sin = np.sin(2*np.pi*y/24)
y = np.stack((y_cos, y_sin)).T
x.drop(columns='hour', inplace=True)
x_copy.drop(columns='hour', inplace=True)

x_test = pd.read_csv('rf.test.txt')

def distance_loss(y_true,y_pred):
    error=0
    for i in range(y_pred.shape[0]):
        error_temp = (y_pred[i, 0] - y_true[i, 0])**2 + (y_pred[i, 1] - y_true[i, 1])**2
        error += error_temp
    return error/y_true.shape[0]

def report(results, n_top=3):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results['rank_test_score'] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
                  results['mean_test_score'][candidate],
                  results['std_test_score'][candidate]))
            print("Parameters: {0}".format(results['params'][candidate]))
            print("")


my_scorer = make_scorer(distance_loss, greater_is_better=False)

clf = MultiOutputRegressor(RandomForestRegressor(random_state=15))

# specify parameters and distributions to sample from
param_dist = {"estimator__max_depth": [3,6,9, None],
              "estimator__max_features": [2,4,8,16,'auto'],
              "estimator__min_samples_leaf": [1, 2, 4, 6, 8, 10],
              "estimator__min_samples_split": sp_randint(2, 11),
              "estimator__bootstrap": [True, False],
              "estimator__n_estimators": sp_randint(500, 5000)}

n_iter_search = 50
random_search = RandomizedSearchCV(clf, param_distributions=param_dist, scoring=my_scorer,
                                   n_iter=n_iter_search, cv=3, iid=False, verbose=10,random_state=15)
random_search.fit(x, y)
report(random_search.cv_results_)

reg = random_search.best_estimator_

pickle.dump(reg,open('reg.MLclockRFmHT_2025dist5000.sav','wb'))
