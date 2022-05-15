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
    (1)categorizing LChl and MHW observations as "event" or "no event" in their respective datasets,
    (2)splitting the LChl and MHW datasets into training and testing datasets,
    (3)adding lags to the LChl and MHW testing datasets,
    (4)merging the LChl and MHW data to create compound training and testing datasets,
    (5)categorizing compound observations as "no event", "mhw event", "lchl event" or "compound event",
    (6)balancing the categories in the LChl and MHW training datasets,

**cmpndExplTrainRF** contains code for:
    (1)using grid search for tuning parameters of RF model,
    (2)graphing accuracy of mtry and min n parameters

**cmpndRefTrainTestRF** contains code for:
    (1)refined training of compound RF model based on exploratory results,
    (2)graphing of optimal combinations of mtry and min n parameters,
    (3)graphing of variable importance,
    (3)testing accuracy of model predictions on several lags,
    (4)producing confusion matrix to visualize model accuracy
    
**lchlRF** contains code for:
    (1)categorizing Lchl data as negligable, moderate, strong, severe, or extreme,
    (2)splitting of LChl dataset into training and testing sets,
    (3)balancing the LChl training dataset,
    (3)training LChl RF model,
    (4)testing LChl RF model
    **The following sections must be ran independently: (1) Exploratory Train, (2) Refined Train and Testing.**

Raw or unprocessed data can be found here: https://www.dropbox.com/sh/3locafvuzq9tylg/AAB5bwDgFJCzlJ-NLKiNwmbTa?dl=0What
All other datasets (balanced, categorized, compounded) are created within the code.
