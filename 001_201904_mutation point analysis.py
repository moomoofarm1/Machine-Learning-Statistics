# 参考：  http://blog.sina.com.cn/s/blog_81a55b0c0100v0md.html
# 英文论文 “Bernaola-Galvan-2001-Phys.Rev.Lett.-87-art. no_Scale Invariance in the Nonstationarity of Human Heart Rate.pdf”
#          《Scale Invariance in the Nonstationarity of Human Heart Rate》
# 中文论文 《基于启发式分割算法的气候突变检测研究》

#coding: UTF-8
import pandas as pd
import numpy as np

filepath = 'd:/Python/data.csv'
data = pd.read_csv(filepath)
data = data[['data']]

L0 = 25
P0 = 0.95

def Tseries(data1): #返回P(T_Max)的序列T最大值、概率、原序列data1的位置 和 序列T值
	N = data1.shape[0]
	T = np.zeros((N,1))
	for i in range(2,N-3):  #首尾两个数值是没有T值的，循环用来计算T值
		data_left = data1[0:i]
		N_left = data_left.shape[0]
		data_right = data1[i:N-1]
		N_right = data_right.shape[0]
		SD = np.sqrt(1/N_left + 1/N_right) * np.sqrt(((N_left - 1) * np.std(data_left, ddof=1) ** 2 + (N_right -1) * np.std(data_right, ddof=1) ** 2) / (N_left + N_right -2))
		T[i] = abs((np.mean(data_left) - np.mean(data_right)) / SD)
	T_Max = np.max(T)
        #获取最大值所在的索引值
	T_list = list(T)
	position_Max = T_list.index(T_Max)
	Eta = 4.19 * np.log(N) - 11.54
	Delta = 0.4
	v = N - 2
	c = v / (v + T_Max ** 2)
	P_T_Max = (1 - betainc(Delta * v, Delta, c)) ** Eta
	P_T_Max = np.minimum(1, P_T_Max)
	P_T_Max = np.maximum(0, P_T_Max)
	
	#return T_Max, P_T_Max, position_Max, T
	return P_T_Max, position_Max

# 正式算法: 1.初始化
N = data.shape[0]
FLAG = []
ALL_T = []
location = 0
j = 0

# 正式算法: 2.产生初始突变点，并对序列分割
result = Tseries(data)
if result[0] < P0:
	FLAG.append(0)
else:
	FLAG.append(0)
        FLAG.append(result[1])	
	while 1:
		j += 1
		ALL_T = []
		TC = 0 # 临时计数器
		counter = len(FLAG)
		FLAG.sort()
		#在突变点ID已知时，将data按断点分解成若干序列
		for i in range(len(FLAG)):
			if i == 0: ALL_T.append(data[0:FLAG[i+1]])
			elif i == len(FLAG) - 1: ALL_T.append(data[FLAG[i]:N])
			else: ALL_T.append(data[FLAG[i]:FLAG[i+1]])
		#进一步分解
		for i in range(len(ALL_T)):
			if len(ALL_T[i]) < L0:
				TC += 1
			else:
				result = Tseries(ALL_T[i])
				if result[0] < P0: TC += 1
				else:				
					location = FLAG[i] + result[1]
					FLAG.append(location)
		print(j)
		if TC == counter: break #停止条件：若所有序列满足：1.长度小于25；或2.P_T_Max < P0，则停止
	FLAG.sort()
	ALL_T = []
	for i in range(len(FLAG)):
		if i == 0: ALL_T.append(data[0:FLAG[i+1]])
		elif i == len(FLAG) - 1: ALL_T.append(data[FLAG[i]:N])
		else: ALL_T.append(data[FLAG[i]:FLAG[i+1]])

#均值输出
FLAG.append(data.shape[0] - 1)
aa = []
for i in range(len(ALL_T)):
    aa.append(np.mean(ALL_T[i]))
aaa = np.zeros((data.shape[0],1))
for i in range(data.shape[0]):
    for j in range(len(FLAG)-1):
	    if FLAG[j] <= i < FLAG[j+1]: aaa[i,0] = aa[j]
    if i == data.shape[0] - 1: aaa[i,0] = aa[len(FLAG)-2]
aaa = pd.DataFrame(aaa)
aaa.to_excel('d:/Python/aaa.xls')
	
