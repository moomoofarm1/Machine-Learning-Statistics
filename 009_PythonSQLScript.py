# -*- coding:utf-8 -*-

import os
import pymysql
import numpy as np
import pandas as pd
from sqlalchemy import create_engine

pymysql.install_as_MySQLdb()
engine = create_engine("mysql://root:123456@localhost:3306/test1?charset=utf8")

path = "folderURL"
files = os.listdir(path)

for file in files:
    print(file)
    NewFile = path + file
    data = pd.read_excel(NewFile)
    data.replace("nan", np.nan, inplace=True)
    data.replace("NaN", np.nan, inplace=True)
    data = data.dropna(axis=0)
    data.to_sql(name="sample", con=engine, index=False, if_exists="append")


