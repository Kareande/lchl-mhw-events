# Predictability of Compound Low Chlorophyl and Marine Heatwave Events Using Random Forests Models in R
This repo is for processing low chlorophyl (LChl) and marine heatwave (MHW) data, and for using random forest models to test the predictability of compound events given various oceanic and atmospheric variables.

Over the last two decades, marine heatwaves (MHWs) have been observed in all major ocean basins, and have impacted both our environment and society. The occurrence and severity of marine heatwaves has increased due to anthropogenic global warming. Reported effects on marine life include harmful algal blooms, shifts in species range, and even local extinctions. There is, however, little research considering the impacts of compound events. Our ability to predict compound extremes such as LChl events and MHWs can ultimately inform adaptive action, helping to reduce the vulnerability of human communities dependent upon the sea.

**extractNC** contains code for:
    (1)saving .NC data in readable viewable text file,
    (2)extracing relevant variables from .NC file,
    (3)converting time to date time,
    (4)altering dimensions of latitude/longitude,
    **For reference only, not runnable!**

**cmpndDataProcessing** contains code for:
    (1)categorizing the LChl and MHW data as either "event" or "no event" in their respective datasets,
    (2)condensing day, month, and year data into "days since" column
    (3)condensing lat and lon data into one "location" column
    (4)adding lags to the LChl and MHW datasets
    (5)categorizing events within the compound dataset as "no event", "mhw event", "lchl event" or "compound event",
    (6)splitting the compound dataset into training and testing datasets
    (6)balancing the training dataset

**cmpndExplTrainRF** contains code for:
    (1)using grid search for paramaterization of compound RF model,
    (2)graphing accuracy of mtry and min n parameters

**cmpndRefTrainTestRF** contains code for:
    (1)refined training of compound RF model based on exploratory results,
    (2)graphing of optimal combinations of mtry and min n parameters,
    (3)testing accuracy of compound RF model predictions using randomForests package,
    (4)producing confusion matrix to visualize model accuracy

**cmpndRefTrainTestTidyRF** contains code for:
    (1)refined training of compound RF model based on exploratory results,
    (2)graphing optimal combinations of mtry and min n parameters,
    (3)testing accuracy of compound RF model predictions using tidymodels parsnip,
    (4)producing confusion matrix to visualize model accuracy
    
**cmpndUnlaggedRF** contains code for:
    (1)categorizing the LChl and MHW data as either event or not event in their respective datasets,
    (2)categorizing events within the compound dataset as no event, mhw event, lchl event or compound event,
    (3)balancing the compound dataset,
    (4)training compound event RF model,
    (5)testing compound event RF model
    **The following sections must be ran independently: Exploratory Train, Refined Train, and Test.**

**lchlRF** contains code for:
    (1)categorizing Lchl data as negligable, moderate, severe, and extreme,
    (2)balancing LChl dataset,
    (3)training LChl event RF model,
    (4)testing LChl event RF model
    **The following sections must be ran independently: Exploratory Train, Refined Train, and Test.**

Unprocessed, unbalanced data can be found here: https://www.dropbox.com/sh/3locafvuzq9tylg/AAB5bwDgFJCzlJ-NLKiNwmbTa?dl=0What
All other datasets (balanced, categorized, compounded) are created within the code.
