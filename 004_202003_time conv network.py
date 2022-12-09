# coding: UTF-8
import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler

# deep learning
import torch
import torch.nn as nn
from torch.autograd import *
import torch.optim as optim
import torch.nn.functional as F
import matplotlib.pyplot as plt

# global para
batch_size = 50
target_X = ['MeanHR', 'SDNN', 'RMSSD', 'TP'] # HRV para
target_Y = ['LFNorm']

# self defined model
# model 1
def ToVariable(x):
    tmp = torch.FloatTensor(x)
    return Variable(tmp)

# model 2
# 降维Xception
class CNN_Series(nn.Module):
    def __init__(self):
        super(CNN_Series, self).__init__()
        self.conv1 = nn.Sequential(
            nn.Conv1d(
                in_channels = 4, # 4 * 12
                out_channels = 1, # 1 * 12
                kernel_size = 1
                )
            )
        self.conv2 = nn.Sequential(
            nn.Conv1d(
                in_channels = 1, # 1 * 12 
                out_channels = 1, # 1 * 4  
                kernel_size = 3,
                dilation = 4
                ),
            nn.BatchNorm1d(num_features=1), 
            nn.ReLU()
            )
        self.fc1 = nn.Linear(4,1) 
        # self.fc2 = nn.Sigmoid() # 针对二分类

    def forward(self,indata):  
        x = self.conv1(indata)
        x = self.conv2(x)
        x = x.view(x.size(0),-1) 
        out = self.fc1(x)
        return out

# i/o
window =12
train_portion = 0.9
model_scaler = MinMaxScaler()
path = os.getcwd() + '/data/'

# input
files = os.listdir(path)
data = pd.read_excel(path + files[0], sheet_name='Sheet1')
data_Y = data.loc[:,target_Y]
data_Y = data_Y.values
data_Y = data_Y.reshape(-1, 1)
data_Y = model_scaler.fit_transform(data_Y)
data_Y = data_Y[window:,:]
# print(data_Y.shape) # (163, 1)
data_X_temp = data.loc[:, target_X]
data_X_temp = data_X_temp.values
for col in range(data_X_temp.shape[1]):
    temp = model_scaler.fit_transform(data_X_temp[:, col].reshape(-1, 1))
    data_X_temp[:, col] = temp[:, 0]
data_X = np.zeros([data_X_temp.shape[0] - window, data_X_temp.shape[1], window]) # (163, 7, 12)
data_X_temp = data_X_temp.T
# print(data_X_temp.shape) # (7, 175)
# print(data_X.shape)
for i in range(data_X.shape[0]):
    j = i + window
    data_X[i,:,:] = data_X_temp[:,j-window:j]
# print(data_X_temp[:,12-window:12])
# print(data_X[0,:,:])
# print(type(data_X))
data_X = ToVariable(data_X)
print(data_X.shape)

model = CNN_Series()
lossfunc = nn.MSELoss()
optimizer = torch.optim.SGD(model.parameters(), lr = 0.1) # adam 调整

epochs = 2 # epochs十几次不收敛(mse > 1)，达到200-300看到了收敛趋势（mse < 0.01）
batch_num = data_X.shape[0] // batch_size
for epoch in range(epochs):
    print("Epoch: [{}/{}]".format(epoch + 1,epochs))
    for batch_idx in range(batch_num):
        seq = data_X[batch_idx*batch_size:(batch_idx+1)*batch_size,:,:]
        # print(seq.shape)
        out = data_Y[batch_idx*batch_size:(batch_idx+1)*batch_size]
        seq = ToVariable(seq)
        # seq = seq.unsqueeze(-1)
        # print(seq.shape)   
        out = ToVariable(out)
        optimizer.zero_grad()
        model.train() # necessary to set the model state to train mode.
        modelout = model.forward(seq)
        # print(modelout.shape)  # (10,1,4) # 模型训练输出的大小，有问题，应是(10,1)
        # print(out.shape) # (10,1)，原始数据
        loss = lossfunc(modelout, out)
        loss = loss.view([1]) 
        print("Batch:" + str(batch_idx) + "\n误差是：" + str(loss.data.numpy()[0]) )  
        loss.backward()  
        optimizer.step()
        
        # eval
        model.eval() # necessary to set the model state to eval mode.
        with torch.no_grad():
            pass
        

