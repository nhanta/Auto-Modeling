import sklearn 
from sklearn import datasets
from scipy import stats
import numpy as np
from numpy.linalg import inv, multi_dot
import matplotlib.pyplot as plt
from math import exp
from scipy.sparse import diags
import pandas as pd
import statsmodels.formula.api as smf
import statsmodels.api as sm
from sklearn import linear_model
from sklearn.linear_model import LogisticRegression

def read_data():
    
    data = pd.read_csv ("7_8.csv")
    return (data)

# Before splitting point is numeric variable, after that one is factor variable
    
splitting_point = input ("Input splitting point: ")
alpha = input("Input alpha: ")  
result_1 = []
result_2 = []
result_3 = []
df = pd.DataFrame()
t = ''
   
# To identify type of distribution of variable
     
def get_variable_distribution():
    
    # fitdistr (x, "weibull")
    for i in range(read_data().shape[1]):
        
        norm_distr = stats.kstest (read_data().iloc[:, i], 
                                   "norm", stats.norm.fit(read_data().iloc[:, i]))
        expon_distr = stats.kstest (read_data().iloc[:, i],
                                
                                    "expon", stats.expon.fit(read_data().iloc[:, i]))
        gamma_distr = stats.kstest (read_data().iloc[:, i], 
                                    "gamma", stats.gamma.fit(read_data().iloc[:, i]))
    
        min_error = min (norm_distr[0], expon_distr[0], gamma_distr[0])
        
        if norm_distr[0] == min_error and norm_distr[1] > alpha:
            print ( 
                    read_data().columns.values[i], "Variable is normal distribution with D = %s, p_value = %s" 
                    % (norm_distr)
                    )
        elif expon_distr[0] == min_error and expon_distr[1] > alpha:
            print (
                       read_data().columns.values[i], "Variable is exponential distribution with D = %s, p_value = %s" 
                       % (expon_distr)
                    )
        elif gamma_distr[0] == min_error and gamma_distr[1] > alpha:
            print (
                    read_data().columns.values[i], "Variable is gamma distribution with D = %s, p_value = %s" 
                       % (gamma_distr)
                     )
        
        if i == read_data().shape[1] - 1 and norm_distr[0] == min_error and norm_distr[1] > alpha:
            t = 'norm'
    
    for i in range (read_data().shape[0]):
        if read_data().iloc[i, read_data().shape[1] - 1] == 0 or read_data().iloc[i, read_data().shape[1] - 1] == 1:
            t = 'binary'
            
    return(t)
    
# To choose explanatory variable to add to model
          
def get_model_dataframe (): 

# Using cortest to identify correlation between numeric variable and response variable
    
    for i in range (splitting_point + 1):
        
        cor = stats.pearsonr (read_data().iloc[:, i], read_data().iloc[:, read_data().shape[1]-1])
        if cor[1] < 0.05:
            result_1.append([cor[0], cor[1], i]) 
            df[read_data().columns.values[i]] = pd.Series(
                    read_data().iloc[:, i])

# Using Anova test to identify affection of factor variable to response variable
            
    for i in range(splitting_point + 1, read_data().shape[1] - 1): 
        
        result = smf.ols (formula = "read_data().iloc[:, read_data().shape[1] - 1] ~ pd.Categorical(read_data().iloc[:, i]) ", 
                          data = read_data()
                          ).fit()
        aov_table = sm.stats.anova_lm(result, typ = 2)
        if  aov_table["PR(>F)"][0] > 0.05:
            result_2.append([aov_table["PR(>F)"][0], i])
            df[read_data().columns.values[i]] = pd.Categorical(read_data().iloc[:, i])

# Using cortest to identify affection among numeric variables    
            
    for i in range (read_data().shape[1]):
        for j in range (read_data().shape[1]):
            
            cor = stats.pearsonr (read_data().iloc[:, i], read_data().iloc[:, j])
            if cor[0] > 0.8 and i<j:
                result_3.append([cor[0], i, j])    
                df[read_data().columns.values[i] + "*" + read_data().columns.values[j]] = read_data().iloc[:, i]* read_data().iloc[:, j]
    df[read_data().columns.values[read_data().shape[1]-1]] = pd.Series(read_data().iloc[:, read_data().shape[1]-1])                            
    return (df)             
    print ("Correlation, p_value, order of important explanatory variables to affect response variables are: ", result_1)
    print ("F_test, order of important factor variable are: ", result_2)
    print ("Correlation, order of explanatory variables to affect each other are: ", result_3 )

# Getting model if response variable is normal distribution

def get_normal_distribution_model ():
    
    l = get_model_dataframe().shape[1]  
    x = np.array([get_model_dataframe().iloc[: , i] for i in range(l-1)]).T
    y = np.array(get_model_dataframe().iloc[:, l-1]).T
    result_1 = smf.ols ("y ~ x", data = get_model_dataframe()).fit()  
                    
    return(result_1)


# Getting model if response variable is binary distribution    

def get_binary_distribution_model ():  
    
    l = read_data().shape[1]  
    x = np.array([read_data().iloc[: , i] for i in range(l-1)]).T
    y = np.array(read_data().iloc[:, l-1]).T
    result_2 = smf.logit ("y ~ x", data = read_data()).fit()
    
    return (result_2)
    
# Getting model after indentifying the distribution of response variable
    
def get_model ():
    
    if get_variable_distribution() == 'norm':
        model = get_normal_distribution_model()
    elif get_variable_distribution() == 'binary':
        print(read_data().columns.values[read_data().shape[1] - 1], 'is binary variable')
        model = get_binary_distribution_model ()
    return (model)


print(get_model().summary())
        

