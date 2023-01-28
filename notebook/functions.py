import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression

# Function for merging closing price
def merging_CP(firm_CP, market_CP):
    merged_df = pd.merge(firm_CP, market_CP, left_index = True, right_index = True)
    merged_df.columns = ['firm_CP', 'market_CP']
    return merged_df

# Function for making the data for regression
def making_data_reg(merged_CP):
    '''
    Input: closing price of firm's stock, market stock
    Output: X and y
    '''
    # Merging two dataframe by matching indexes
    df = pd.DataFrame()
    df['y'] = merged_CP['firm_CP'].pct_change()

    market_return = merged_CP['market_CP'].pct_change()
    idx = ['x'+i for i in ['-2','-1','0','+1','+2']]
    for i,j in zip(idx,range(-2,3,1)):
        df[i] = market_return.shift(j)
    return df

# Fitting multiple regression and getting the error part
def reg_residuals(X, y):
    '''
    Input: Dependent variable and design matrix
    Output: Residual = (y-y_hat)
    '''
    # beta_hat = np.linalg.inv(X.T @ X)@X.T@y
    # y_hat = X@beta_hat
    mlr = LinearRegression()
    mlr.fit(X,y)
    y_hat = mlr.predict(X)
    return y-y_hat

# Function to calculate firm-specific daily return
def firm_specific_return(residuals):
    '''
    Input: Residuals (e_tau)
    Output: w = ln(1+e_tau)
    '''
    return np.log(1 + residuals)

# First measure: Binary
def COUNT(w):
    '''
    Input: (x) firm-specific daily return
    Output: 0 or 1
    '''
    q_01 = np.mean(w)-3.09*np.std(w)
    q_99 = np.mean(w)+3.09*np.std(w)
    return np.sum(w < q_01) - np.sum(w > q_99)

# Second measure: NSKEW
def NSKEW(w):
    '''
    Input: (w) firm-specific daily return
    Output: -(n*(n-1)*np.sum(w**3))/((n-1)*(n-2)*(np.sum(w**2)**1.5))
    '''
    n = len(w)
    numerator = n*((n-1)**1.5)*np.sum(w**3)
    denominator = (n-1)*(n-2)*(np.sum(w**2)**1.5)
    return -numerator/denominator

# Third measure: DUVOL
def DUVOL(df):
    '''
    Input: (w) firm-specific daily return
    Output: log(((n_u-1)*np.sum(w_downdays**2))/(n_d-1)*np.sum(w_updays**2))
    '''
    mean = np.mean(df['firm_return'])
    # select index of days having above or below the annual mean
    updays = df['firm_return'][df['firm_return'] > mean].index; n_u = len(updays)
    downdays = df['firm_return'][df['firm_return'] < mean].index; n_d = len(downdays)
    # getting returns of specific indices
    w_updays = df['w'][updays]; w_downdays = df['w'][downdays]
    if n_u in [0,1] or n_d in [0,1]:
        return np.nan
    else:
        numerator = (n_u-1)*np.sum(w_downdays**2)
        denominator = (n_d-1)*np.sum(w_updays**2)
        temp = numerator/denominator
        return np.log(temp)

# Final function to get the required output
def crash_risk(firm_CP, market_CP):
    merged_CP = merging_CP(firm_CP, market_CP)
    reg_data = making_data_reg(merged_CP)
    reg_data.dropna(inplace=True)
    X, y = reg_data.drop(['y'], axis=1).values, reg_data['y'].values
    residuals = reg_residuals(X, y)
    df = pd.DataFrame({'firm_return':reg_data['y'],'residuals':residuals, 
                'w':firm_specific_return(residuals)})
    count = COUNT(df['w'])
    nskew = NSKEW(df['w'])
    duvol = DUVOL(df)
    return count, nskew, duvol