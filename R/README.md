## scripts 

- `ComputeDistance.R`  
    Description: Compute distance for each path from the true damage.  
    Output: file df_Distance.csv that contains a 1560x8 matrix in which distances are calculated.  

- `remove_bias and_filter.R`  
    Description: Example routine of actuator 21, that shows how to perfom signals cleaning which include:
    
    - bias removing 
    - filtering 
    - rephasing 
    - normalizing 
    
    and also compute the area differences. 

-`CreateComplete_df.R`  
    Description: Add the column AreaDiff to df_Distance.csv  
    Output: file completeDf.csv that contain a 1560x9 matrix.  

-`ModelComparison.R`   
    Input: completeDf.csv  
    Description: compare quadratic fully, linear fully, quadratic tuned models.  

-`testModel.R`  
    Input: completeDf.csv  
    Description: predict damage position as barycenter of damaged path intersections.  

-`probabilityContour.R`  
    Input: completeDf.csv  
    Description: predict damage position as a region with defined confidence level.  

## Execution 

### SETUP

1) Set "/DATI" as working directory 
2) ComputeDistance.R 
3) CreateComplete_df.R 

### RESULTS

4) testModel.R
5) probabilityContour.R
















