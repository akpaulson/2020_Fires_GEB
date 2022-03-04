Code for: “The 2020 California fire season: A year like no other, a
return to the past, or a harbinger of the future?”
================

***Description:*** This document outlines the R code used for data
preparation, analysis, and figure production for the following
manuscript:

Safford, Hugh D., A.K. Paulson, Z.L. Steel, D.J.N. Young, and R. Wayman.
XXXX. The 2020 California fire season: A year like no other? A return to
the past? Or a harbinger of the future? XXXX XX:XX-XX.

***Code contributors:*** [Alison K.
Paulson](https://github.com/akpaulson), [Zachary L.
Steel](https://github.com/zacksteel), [Derek J. N.
Young](https://github.com/youngdjn)

***Code Descriptions:***

-   ***Data Preparation:***

    -   **get\_fire\_perim\_make\_90m\_grid.R:** This code extracts the
        fire perimeters for the 40 largest California fires in 2020 and
        creates a grid of points with 90 m spacing within each fire
        perimeter.

    -   **combine\_fire\_progressions.R:** Creates a raster with the
        date of fire progression for each of the 40 largest California
        fires in 2020.

    -   **extract\_gridMET.R:** Extract
        [gridMET](http://www.climatologylab.org/gridmet.html) weather
        data for each point in the 90m grid (from
        get\_fire\_perim\_make\_90m\_grid.R) based on the date of
        burning (from combine\_fire\_progressions.R).

    -   **combine\_severity\_rasters.R:** Combine all rasters with
        estimates of initial fire severity for the 40 largest 2020 CA
        fires. Severity data from
        [RAVG](https://burnseverity.cr.usgs.gov/products/ravg) and a
        modified version of the Google Earth Engine method published by
        [Parks et al. 2018](https://www.mdpi.com/2072-4292/10/6/879).

    -   **combine\_pfr.R:** Combines presettlement fire regime (PFR)
        data from all CalVeg Zones. PFR data are from the [Fire Return
        Interval Departure Database,
        FRID](https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836).

    -   **calveg\_cwhr\_lookuptable.R:** Create a lookup table for
        [CALVEG](https://www.fs.usda.gov/detail/r5/landmanagement/resourcemanagement/?cid=stelprdb5347192)
        CWHR codes for rasterizing cwhr data.

    -   **extract\_veg\_tslf\_sev.R:** This code extracts the values for
        presettlement fire regime (PFR from
        [FRID](https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836)),
        vegetation cover type (Existing Vegetation Type from Landfire
        ([EVT](https://landfire.gov/evt.php)), and CWHR type from
        [CALVEG](https://www.fs.usda.gov/detail/r5/landmanagement/resourcemanagement/?cid=stelprdb5347192)),
        fire severity (from combine\_severity\_rasters.R), time since
        last fire from
        [FRID]((https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836)),
        and fire regime condition class from
        [FRID](https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836)
        for each of the points in the 90m grid across the 40 largest
        fires in California in 2020. Grid was made in the
        get\_fire\_perim\_make\_90m\_grid.R code.

    -   **extract\_mortality.R:** Extract and combine [USFS Aerial
        Detection
        Monitoring](https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696)
        data in California from 2012-2019 (very coarse estimate of tree
        mortality). Extract mortality estimate for each point in the 90m
        grid.

    -   **data\_prep.R:** Combine all of the different data sets
        together and prepare data for modeling (remove NA values and
        problematic fires - Mountain View and Bond).

    -   **FINAL DATASET:** We archived the final, derived dataset used
        for modeling in this manuscript at **INSERT CITATION.**

-   ***Spatial autocorrelation:***

    -   asdf

    -   asdf

-   ***Modeling fire severity:***

    -   **model\_build.R:** Organize data and run two Bayesian models.

    -   **par\_ests.R:** Pull draws from model fits to generate
        parameter estimate summaries and dot plots.

    -   **marg\_effs.R:** Pull veg model draws, make predictions and
        plot marginal effects for select vegetation types.

    -   **fuel\_v\_weather.R:** Looks at importance of predictor
        variables at the fire level. Ultimately produces a figure
        ranking fires for each variable and produces some tabular
        summaries along the way.

-   ***Additional Figure and Table Preparation:***

    -   **calc\_percent\_high\_sev.R:** Code to calculate the percent
        fire area that burned at unchanged, low, moderate, or high
        severity. Presented in Figure S3.

    -   **econlosses\_fig.R:** loess plots for qualitative look at
        economic costs over time.

-   \*\_\_Extraneous code:\*

    -   **compare\_ravg\_vs\_gee.Rmd:** Simple comparison of initial
        fire severity estimates produced using the RAVG vs Google Earth
        Engine methods.

    -   **paper\_facts.Rmd:** Some misc code for values needed during
        writing.
