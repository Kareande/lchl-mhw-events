# Predictability of Compound Low Chlorophyl and Marine Heatwave Events Using Random Forests Models in R
This repo is for processing low chlorophyl (LChl) and marine heatwave (MHW) data and for using random forest models to test the predictability of compound events given various oceanic variables.

Over the last two decades, marine heatwaves (MHWs) have been observed in all major ocean basins, and have impacted both our environment and society. The occurrence and severity of marine heatwaves has increased due to anthropogenic global warming. Reported effects on marine life include harmful algal blooms, shifts in species range, and even local extinctions. There is, however, little research considering the impacts of compound events. Our ability to predict compound extremes such as LChl events and MHWs can ultimately inform adaptive action, helping to reduce the vulnerability of human communities dependent upon the sea.

**compRF** contains code for:
    (1)categorizing the LChl and MHW data as either event or not event in their respective datasets,
    (2)categorizing events within the combined compound dataset as no event, mhw event, lchl event or compound event,
    (3)balancing the compound dataset,
    (4)training compound event RF model,
    (5)testing compound event RF model
    
**extractNC** contains code for:
    (1)saving .NC data in readable viewable text file,
    (2)extracing relevant variables from .NC file,
    (3)convertime time to date time,
    (4)altering dimensions of latitude/longitude,
    **For reference only, not runnable!**
    
**lchlRF** contains code for:
    (1)categorizing Lchl data as negligable, moderate, severe, and extreme,
    (2)balancing Lchl dataset,
    (3)training lchl event RF model,
    (4)testing lchl event RF model
    

Processed, unbalanced data can be found here: https://www.dropbox.com/sh/3locafvuzq9tylg/AAB5bwDgFJCzlJ-NLKiNwmbTa?dl=0What
All other datasets (balanced, categorized, compounded) are created within the code.
