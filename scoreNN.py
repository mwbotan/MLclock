import seaborn as sn
import matplotlib.pyplot as plt
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

tnum = 96
trdata="all"

traindata="MLclock_train.csv"
x_test = pd.read_csv('MLclock_test.csv')

patience=800
epoch = 1500

adam = Adam(lr=0.0003, beta_1=0.9, beta_2=0.999, amsgrad=False)
early_stop = EarlyStopping(patience=patience, restore_best_weights=True, monitor='val_loss', mode='min')

def custom_loss(y_true,y_pred):
    error = tf.abs(tf.atan2(y_pred[:, 1], y_pred[:, 0])-tf.atan2(y_true[:, 1], y_true[:, 0]))
    indices = tf.multiply(tf.cast(tf.greater_equal(error,math.pi),'float32'),2*math.pi)
    error = tf.abs(tf.subtract(indices, error))
    return error**2

for mode in["ang","dis"]:

    x = pd.read_csv(traindata)
    y_copy = x['hour']
    y_copy.reset_index(drop=True, inplace=True)
    y = x['hour']
    res_cv = np.array(y)
    res_cv = res_cv.reshape(tnum, 1)
    y_cos = np.cos(2 * np.pi * y / 24)
    y_sin = np.sin(2 * np.pi * y / 24)

    res_cvx = np.array(y_cos)
    res_cvx = res_cvx.reshape(tnum, 1)

    res_cvy = np.array(y_cos)
    res_cvy = res_cvy.reshape(tnum, 1)

    y = np.stack((y_cos, y_sin)).T
    x.drop(columns='hour', inplace=True)

    res_test = np.full((len(x_test), 1), 12)
    res_testx = np.cos(np.full((len(x_test), 1), np.pi))
    res_testy = np.sin(np.full((len(x_test), 1), np.pi))

    def larger_model():
        model = Sequential()
        model.add(Dense(2048, kernel_initializer='normal',activation='relu'))
        model.add(Dropout(0.4))
        model.add(BatchNormalization())
        model.add(Dense(1024,kernel_initializer='normal',activation='relu'))
        model.add(Dropout(0.3))
        model.add(BatchNormalization())
        model.add(Dense(512,kernel_initializer='normal',activation='relu'))
        model.add(Dropout(0.2))
        model.add(BatchNormalization())
        model.add(Dense(256, kernel_initializer='normal', activation='relu'))
        model.add(Dropout(0.1))
        model.add(BatchNormalization())
        model.add(Dense(2, kernel_initializer='normal'))
        if(mode=="ang"):
            model.compile(loss=custom_loss, optimizer=adam)
        if(mode=="dis"):
            model.compile(loss="mse", optimizer=adam)
        return model

    early_stop = EarlyStopping(patience=patience, restore_best_weights=True, monitor='loss', mode='min')


    #batch = 1024
    #for batch in [16,32,64,128,256,512,1024,2048, 4096]:
    for batch in [128]:
        for i in range(0,30):
            model=larger_model()
            x_train = x.iloc
            model.fit(x.values.astype('float64'), y.astype('float64'), batch_size=batch, epochs=epoch, callbacks=[early_stop])
            train_preds = model.predict(x)
            test_preds = model.predict(x_test)
            ax = sn.scatterplot(train_preds[:,0],train_preds[:,1])
            ax = sn.scatterplot(test_preds[:,0],test_preds[:,1])
            plt.show()

            pred_hr = np.arctan2(train_preds[:, 1], train_preds[:, 0]) % (2 * math.pi) * 24 / (2 * np.pi)
            res_cv = np.hstack((res_cv, pred_hr.reshape(tnum, 1)))
            res_cvx = np.hstack((res_cvx, train_preds[:, 0].reshape(tnum, 1)))
            res_cvy = np.hstack((res_cvy, train_preds[:, 1].reshape(tnum, 1)))

            test_hr = np.arctan2(test_preds[:,1],test_preds[:,0])%(2*np.pi)*24/(2 * np.pi)

            res_test = np.hstack((res_test, test_hr.reshape(88, 1)))
            res_testx = np.hstack((res_testx, test_preds[:, 0].reshape(88,1)))
            res_testy = np.hstack((res_testy, test_preds[:, 1].reshape(88,1)))

        np.savetxt(f'NN{mode}_e{epoch}_b{batch}_{trdata}_train.txt',res_cv)
        np.savetxt(f'NN{mode}_e{epoch}_b{batch}_{trdata}_trainx.txt',res_cvx)
        np.savetxt(f'NN{mode}_e{epoch}_b{batch}_{trdata}_trainy.txt',res_cvy)

        np.savetxt(f'NN{mode}_e{epoch}_b{batch}_{trdata}_test.txt',res_test)
        np.savetxt(f'NN{mode}_e{epoch}_b{batch}_{trdata}_testx.txt',res_testx)
        np.savetxt(f'NN{mode}_e{epoch}_b{batch}_{trdata}_testy.txt',res_testy)
