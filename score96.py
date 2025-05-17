import pandas as pd
import numpy as np
import pickle
from sklearn.model_selection import KFold
import seaborn as sn
import matplotlib.pyplot as plt


#modelfile = 'reg.MLclockGBmHT.sav'
modelfile = 'reg.MLclockRFmHT.sav'
model = pickle.load(open(modelfile, 'rb'))



x = pd.read_csv('MLclock_train.csv')
y = x['hour']
res_cv = np.array(y)
res_cv = res_cv.reshape(96,1)
y_cos = np.cos(2*np.pi*y/24)
y_sin = np.sin(2*np.pi*y/24)
y = np.stack((y_cos, y_sin)).T
x.drop(columns='hour', inplace=True)

x_test = pd.read_csv('MLclock_test.csv')
res_test = np.full((len(x_test),1),12)

#for i in range(1,10):

# validation settings
n_folds = 96
folds=KFold(n_splits=n_folds, shuffle=True, random_state=47)


error=0
all_preds = np.zeros((y.shape[0], 2))

for n_fold, (train_idx,valid_idx) in enumerate(folds.split(x,y)):
    print("%%% FOLD{} %%%".format(n_fold))
    train_x, train_y = x.iloc[train_idx], y[train_idx]
    valid_x, valid_y = x.iloc[valid_idx], y[valid_idx]
    model.fit(train_x.values.astype('float64'), train_y.astype('float64'))
    preds=model.predict(valid_x.values.astype('float64'))
    all_preds[valid_idx] = preds

pred_hr = np.arctan2(all_preds[:, 1], all_preds[:, 0]) % (2 * np.pi) * 24 / (2 * np.pi)
res_cv = np.hstack((res_cv, pred_hr.reshape(96, 1)))

model.fit(x,y)
train_preds=model.predict(x)
test_preds=model.predict(x_test)

test_hr=np.arctan2(test_preds[:,1],test_preds[:,0])%(2*np.pi)/(2*np.pi)*24
res_test = np.hstack((res_test, test_hr.reshape(88, 1)))
ax = sn.lineplot(np.arange(test_hr.shape[0]), test_hr)
plt.show()

np.savetxt("score96_reg.MLclockGBmHT.sav_cv_.txt", res_cv)
np.savetxt("score96_reg.MLclockGBmHT.sav_test_.txt", res_test)

