import pandas as pd
import numpy as np

from matplotlib import pyplot

from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OrdinalEncoder
from sklearn.preprocessing import OneHotEncoder

from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2

from sklearn.linear_model import LogisticRegression


df = pd.read_csv("../4.a.Detail_Incident.csv", parse_dates=['Open_Time',  'Resolved_Time','Close_Time'])

# Prepare y
y = df.SLAFail.values
df = df.drop(['SLAFail'], axis='columns')

# Prepare X

## create lists of columns based on dtype and cardinality for different treatment later on
dfColsCat = list(df.select_dtypes(include='object').columns)
dfColsCont =  df.select_dtypes(include=['int64', 'float64']).columns
dfColsDates =  df.select_dtypes(include='datetime').columns
dfColsVeryHiCard =  df[dfColsCat].ix[:, df.apply(lambda x: x.nunique()) >= 1000].columns
dfColsHighCard = df[dfColsCat].ix[:, ( df.apply(lambda x: True if ( x.nunique() < 1000) & (x.nunique() >= 200  ) else False ) ) ].columns
dfColsMedCard = df[dfColsCat].ix[:, ( df.apply(lambda x: True if ( x.nunique() < 200) & (x.nunique() >= 25  ) else False ) ) ].columns


# force all fields to strings
# X = X.astype(str)

# Encode X using OrdinalEncoder
# oe = OrdinalEncoder()
# oe.fit(X)

# dummy encoding for categorical features
# ohe = OneHotEncoder(sparse=False)



# Try Chi-Squared Statistic
# feature extraction
# fs = SelectKBest(score_func=chi2, k='all')
# fs.fit(X, y)
### fails TypeError: '<' not supported between instances of 'numpy.ndarray' and 'int'

# Try Logistic Regression
logreg = LogisticRegression()

X = df.loc[:, dfColsCont]

logreg.fit(X,y)
