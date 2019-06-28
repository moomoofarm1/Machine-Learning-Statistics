# Note: 功能为输出excel文件中字段的最小最大值，基于sklean增强学习
# copyright @ ZhuoJun Gu，仅供研究之用，复制与传播必须遵守GNU V3.0协议 
# http://www.gnu.org/licenses/gpl-3.0.html

# coding: UTF-8
import warnings
warnings.filterwarnings("ignore") # 不显示warning

import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler

temp = os.getcwd() + "/aaa/"
files = os.listdir(temp)
model = MinMaxScaler()
columns_output = [aaa]

jilushu = 0
for file in files:
    jilushu += 1
    print('当前第'+str(jilushu)+'个文档，名字：'+file+',共'+str(len(files))+'个文件')
    temp1 = pd.read_excel(temp+file,sheet_name='aaa')
    temp1 = temp1.loc[:,'aaa':'aaa']
    temp1 = temp1.values
    temp1 = np.maximum(temp1,2)
    model.partial_fit(temp1)
    temp1 = pd.read_excel(temp+file,sheet_name='aaa')
    temp1 = temp1.loc[:,'aaa':'aaa']
    temp1= temp1.values
    temp1 = np.maximum(temp1, 2)
    model.partial_fit(temp1)
    print(model.data_min_)
    print(model.data_max_)

data_output = np.zeros([2,7])
data_output[0,:] = model.data_min_
data_output[1,:] = model.data_max_

data_output = pd.DataFrame(data_output,columns=columns_output)
data_output.to_excel(os.getcwd()+'aaa.xls',index=False)
