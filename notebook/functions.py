import numpy as np
import pandas as pd

# Function for making the data for regression
def making_data_reg(firm_CP, market_CP):
    '''
    Input: closing price of firm's stock, market stock
    Output: X and y
    '''
    df = pd.DataFrame()
    df['y'] = firm_CP.pct_change()

    market_return = market_CP.pct_change()
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
    beta_hat = np.linalg.inv(X.T @ X)@X.T@y
    y_hat = X@beta_hat
    return y-y_hat

# Function to calculate firm-specific daily return
def firm_specific_return(residuals):
    '''
    Input: Residuals (e_tau)
    Output: w = ln(1+e_tau)
    '''
    return np.log(1 + residuals)

# First measure: Binary
def BINARY(w):
    '''
    Input: (x) firm-specific daily return
    Output: 0 or 1
    '''
    q_99 = np.mean(w)-3.09*np.std(w)
    if np.sum(w < q_99) > 0:
        CR = 1
    else:
        CR = 0
    return CR

# Second measure: NSKEW
def NSKEW(w):
    '''
    Input: (w) firm-specific daily return
    Output: -(n*(n-1)*np.sum(w**3))/((n-1)*(n-2)*(np.sum(w**2)**1.5))
    '''
    n = len(w)
    numerator = n*(n-1)*np.sum(w**3)
    denominator = (n-1)*(n-2)*(np.sum(w**2)**1.5)
    return -numerator/denominator

# Third measure: DUVOL
def DUVOL(w):
    '''
    Input: (w) firm-specific daily return
    Output: log(((n_u-1)*np.sum(w_downdays**2))/(n_d-1)*np.sum(w_updays**2))
    '''
    n = len(w)
    mean = np.mean(w)
    # select index of days having above or below the annual mean
    updays = np.argwhere(w > mean).flatten(); n_u = len(updays)
    downdays = np.argwhere(w < mean).flatten(); n_d = len(downdays)
    # getting returns of specific indices
    w_updays = w[updays]; w_downdays = w[downdays]
    numerator = (n_u-1)*np.sum(w_downdays**2)
    denominator = (n_d-1)*np.sum(w_updays**2)
    return np.log(numerator/denominator)

# Final function to get the required output
def crash_risk(X, y):
    residuals = reg_residuals(X, y)
    w = firm_specific_return(residuals)
    binary = BINARY(w)
    nskew = NSKEW(w)
    duvol = DUVOL(w)
    return binary, nskew, duvol