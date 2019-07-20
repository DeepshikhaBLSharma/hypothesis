hypothesis<-function(data,target)  ## data is  dataframe containing all the predictors and target is the target variable from the dataframe
{
  if(!is.data.frame(data))   #### Checks if the argement data is a dataframe, if not the functions stops right there
    stop("The given object is not a data frame")
  if (length(unique(target))==2 & is.numeric(target))  ## checks if the target is a binary numeric value, if yes, it will go to the next level
  {
    p_values=c()     ### a new vector for storing the p-values generated from statistical testing is created
    for (i in 1:ncol(data)) ## iterates from column 1 to last column, to check each predictor variable
    {

      if (is.factor(data[,i])) ## checks if the column is  categorical(character in nature)
      {                             ### if yes, it will do  chi-square test of target and predictor variable
        p_values[i] = chisq.test(table(data[,i], target))$p.value ## p values from chi-square test is stored in the ith row of the  new variable(p_values)
      }
      else if(is.numeric(data[,i]))  ## checks if the predictor is numeric,
      {                                ## if yes will check the following conditions
        if(length(unique(data[,i]))==2)  ## checks if the predictor has only 2 categories in it
        {                                ## if yes, will do t test for the predictor and target variables
          p_values[i] = t.test(target~ data[,i])$p.value   ## p value generated is stored in ith row of a vector(p_values), created earlier
        }
        else                    ### if predictor is categorical in nature, will go for annova
        {
          aovRes = aov(target ~ data[,i])   ### annova test for predictor and target variable
          fv = summary(aovRes)[[1]][[1,"F value"]]
          p_values[i] =summary(aovRes)[[1]][[1,"Pr(>F)"]]   ### p value is stored in the ith row of the vector created to store all p values(p_values)
        }
      }
    }
    Variables = names(data)   ### create a new variable, which will store the names of all the predictors in sequential order
    return(data.frame(Variables,p_values))  ## the variable(which stores the names of predictors) and vector(which stores all the p_values) are joined to form a new dataframe
  }   ### output is a dataframe which contains 2 columns, the names of the variables and p_ values generated from different tests
  else
  {
    print("Target variable is not binary")   ### message to the user saying the target variable is not binary
  }
}
