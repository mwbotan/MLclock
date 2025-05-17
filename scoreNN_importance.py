
from sklearn.preprocessing import normalize
import seaborn as sn
import matplotlib.pyplot as plt
from numpy.ma.bench import timer
from sklearn.model_selection import cross_val_predict, KFold
import math
from keras.models import Sequential
from keras.layers import Dense, BatchNormalization, Dropout
import tensorflow as tf
import numpy as np
import pandas as pd
from tensorflow.keras import Sequential
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras.layers import Dense, Dropout, BatchNormalization
from tensorflow.keras.optimizers import Adam
from tensorflow import keras

x = pd.read_csv('MLclock_train.cs')
y_copy = x['hour']
y_copy.reset_index(drop=True, inplace=True)
y = x['hour']
res_cv = np.array(y)
res_cv = res_cv.reshape(96,1)
res_error = res_cv
y_cos = np.cos(2*np.pi*y/24)
y_sin = np.sin(2*np.pi*y/24)

res_cvx = np.array(y_cos)
res_cvx = res_cvx.reshape(96,1)

res_cvy = np.array(y_cos)
res_cvy = res_cvy.reshape(96,1)

y = np.stack((y_cos, y_sin)).T
x.drop(columns='hour', inplace=True)

x_test = pd.read_csv('MLclock_test.csv')
res_test = np.full((len(x_test),1),12)
res_testx = np.cos(np.full((len(x_test),1),np.pi))
res_testy = np.sin(np.full((len(x_test),1),np.pi))

patience=800
epoch = 1500
batch=128
mode=="ang"

error=0
all_preds = np.zeros((y.shape[0], 2))
all_error = np.zeros((y.shape[0], 1))

feats = x.columns.to_list()
feature_importance_df = pd.DataFrame()
adam = Adam(lr=0.0003, beta_1=0.9, beta_2=0.999, amsgrad=False)
early_stop = EarlyStopping(patience=patience, restore_best_weights=True, monitor='val_loss', mode='min')

def custom_loss(y_true,y_pred):
    error = tf.abs(tf.atan2(y_pred[:, 1], y_pred[:, 0])-tf.atan2(y_true[:, 1], y_true[:, 0]))
    indices = tf.multiply(tf.cast(tf.greater_equal(error,math.pi),'float32'),2*math.pi)
    error = tf.abs(tf.subtract(indices, error))
    return error**2

def larger_model():
    model = Sequential()
    model.add(Dense(2048, kernel_initializer='normal', activation='relu'))
    model.add(Dropout(0.4))
    model.add(BatchNormalization())
    model.add(Dense(1024, kernel_initializer='normal', activation='relu'))
    model.add(Dropout(0.3))
    model.add(BatchNormalization())
    model.add(Dense(512, kernel_initializer='normal', activation='relu'))
    model.add(Dropout(0.2))
    model.add(BatchNormalization())
    model.add(Dense(256,kernel_initializer='normal', activation='relu'))
    model.add(Dropout(0.1))
    model.add(BatchNormalization())
    model.add(Dense(2, kernel_initializer='normal'))
    if(mode=="ang"):
        model.compile(loss=custom_loss, optimizer=adam)
    if(mode=="dis"):
        model.compile(loss="mse", optimizer=adam)
    return model


imp = np.full((96, 204),0).astype('float32')


for i in range(0,96):
    train_idx = np.delete(np.arange(0,96),obj=i)
    valid_idx = np.array([i])
    train_x, train_y = x.iloc[train_idx], y[train_idx]
    valid_x, valid_y = x.iloc[valid_idx], y[valid_idx]
    model=larger_model()
    model.fit(train_x.values.astype('float64'), train_y.astype('float64'), validation_data=(valid_x.values.astype('float64'), valid_y.astype('float64')), batch_size=batch, epochs=epoch, callbacks=[early_stop])
    preds=model.predict(valid_x.values.astype('float64'))
    all_preds[i] = preds
    mse_base = (cyclical_loss(valid_y, preds.astype('float64')))*24/(2*np.pi)
    mse_alt = 0
    print(i+1,mse_base)

    for j in range(0,204):
        mse_alt = 0
        for altv in x.iloc[train_idx,j].values:
            valid_x_alt = x.iloc[valid_idx]
            valid_x_alt.iat[0,j] = altv
            preds_alt = model.predict(valid_x_alt.values.astype('float64'))
            mse_alt += (cyclical_loss(valid_y, preds_alt.astype('float64')))*24/(2*np.pi) - mse_base
        imp[i,j] = mse_alt/95

pred_hr = np.arctan2(all_preds[:, 1], all_preds[:, 0])%(2*math.pi)*24/ (2 * np.pi)
res_cv = np.hstack((res_cv, pred_hr.reshape(96, 1)))
res_cvx = np.hstack((res_cvx, all_preds[:, 0].reshape(96,1)))
res_cvy = np.hstack((res_cvy, all_preds[:, 1].reshape(96,1)))


early_stop = EarlyStopping(patience=patience, restore_best_weights=True, monitor='loss', mode='min')

np.savetxt(f'NN{mode}_e{epoch}_b{batch}_imp10_cv.txt'  ,res_cv)
np.savetxt(f'NN{mode}_e{epoch}_b{batch}_imp10_cvx.txt' ,res_cvx)
np.savetxt(f'NN{mode}_e{epoch}_b{batch}_imp10_cvy.txt' ,res_cvy)
np.savetxt(f'NN{mode}_e{epoch}_b{batch}_imp10_imp.txt' ,imp)
