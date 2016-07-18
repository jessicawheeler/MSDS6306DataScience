Exploratory Analysis of time Series: MCD
----------------------------------------

### Financial Series: The McDonald's Corporatin Common Stock Historical Stock Prices

> Here we will use exploratory data analysis to analyze the volatility
> in McDonald's stock over time. We will use a continuous lookback
> window and graph the volatility with different decaying factors.

    #Install the tseries package since we will be working with time series data.
    require("tseries")

    ## Loading required package: tseries

    #Download the data then create vectors for log returns and volatility:
    MCDdata <- get.hist.quote('MCD',quote="Close")
    MCDret <- log(lag(MCDdata)) - log(MCDdata)
    MCDvol <- sd(MCDret) * sqrt(250) * 100
    #Note: there are 250 trading days per year x 100 to make it a percentage.

    #Check the length of the dataset:
    length(MCDdata)

    ## [1] 6435

    #Check length of log return MCD:
    length (MCDret)

    ## [1] 6434

    #Estimate of the overall MCD volatility:
    MCDvol

    ## [1] 31.26512

> The MCD volatility is about 31% in fluctuations.

### Volatility

> Volatility is the amount of change in a return. We will calculate the
> volatility estimate, which is a running estimate of the changes-
> usually measured as a standard deviation. Here we particularly want to
> look at volatility in a continuous lookback window:

    #Create a function that gets the volatility of the time series:
    getVol <- function(d, logrets){
        var = 0
        lam = 0
        varlist <- c()
        for (r in logrets) {
            lam = lam*(1 - 1/d) + 1
        var = (1 - 1/lam)*var + (1/lam)*r^2
            varlist <- c(varlist, var)
        }
        sqrt(varlist)}


    #Recreate Figure 6.12 in the text on page 155, volatlility estimates when d=10, 30, and 100:
    volest <- getVol(10,MCDret)
    volest2 <- getVol(30,MCDret)
    volest3 <- getVol(100,MCDret)

    #Plot the volatility estimates above:
    plot(volest,type="l", main="Volatility Estimates of MCD Data")
    lines(volest2,type="l",col="red")
    lines(volest3, type = "l", col="blue")

![](JWheelerVolatilityMCD_files/figure-markdown_strict/unnamed-chunk-3-1.png)
JWheelerVolatilityMCD_files/figure-markdown_strict/unnamed-chunk-3-1.png
MSDS6306DataScience/unnamed-chunk-3-1.png

> The graph above shows the volatility estimates when the decay factors
> = 10 (in black), 30 (in red), and 100 (in blue) in a continous
> lookback window. We can see that as the decay factor increases, the
> volatility estimate decreases - the peaks are much smoother. Most of
> the times, the fluctuations are fairly stable but there are two large
> peaks around 1000 and a little after 2000, indicating a large
> fluctuation at those two times. In conclusion, looking at volatility
> shows patterns of fluctuations over time. When the decay factor
> increases, less weight is placed on older time points and results can
> be observed in the plot above.

------------------------------------------------------------------------

-   References:
    -   *Doing Data Science* by O'Neil and Schutt
    -   SMU Doing Data Science Course, Unit 9.5: Preparing Financial
        Data
