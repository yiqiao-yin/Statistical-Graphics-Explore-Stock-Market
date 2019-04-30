# Statistical Graphics: Explore-Stock-Market

This Github repo presents different sources of statistical graphics that can be adopted in visualizing stock market data.

# Introduction

Scholars, traders, and investors study market for decades. Before we can start quantitative analysis, sufficient amount of exploratory data analysis is much desired. What can we do better visualize the time-series phenomenon in stock market? This project we will be working with a small group of stocks and we will explore the tools that we can use to visualize the market environment in statistical graphics. Not only do we provide original work developed by ourselves we believe our visualization can discover some of the insights to drive quantitative analysis. This notebook is coded for this presentation and for any audience to reproduce our work. Through multidimensional lenses of ours, we help visualize and potentially unleash the secrets hidden in stock data so that scholars and practitioners can better do the job in quantitative analysis.

# Sample Graphics


## QQPlot: to visualize relationship of returns

Let us creat a simple portfolio using equal weight on each stock. That is, for a portfolio of total of 9 stocks, each stock gets a weight of 1/9=11.1%. For this portfolio (let us call it equal weight portfolio), we can compare expected return with that of market and standard normal distribution. We can attempt to plot qqplot to compare the results.

From statsitical point of view, we can observe the graphs below that most of the dots fall on the 45 degree line which tells us strong correlation between the two objects used in each plot. For example, the first one (the left one in the grid below) is portfolio (equal weight) return versus standard normal. We constructed 9 plots (that is, 3 different pairs for 3 time frames). As one can see, the longer the days of data we collect the better the dots fall on 45 degree line. In other words, we can claim that there could be a positively correlated relationship between those two data sets, i.e. they may come from the same distribution.

<p align="center"><img src="https://github.com/yiqiao-yin/StatisticalGraphics-Explore-Stock-Market/blob/master/figs/figs-qqplot-3-by-3.PNG"></p>

## 3D Bar Plot: to visualize multidimensional returns

By using cloud() function, the algorithm for identifying which edges of the bounding box should be drawn before the points are plotted fails in some cases. This school of thought presents data using three-dimensional bar plots. The z-axis represents return in numerical values. The x-axis and y-axis are stocks and type (past week, month, quarter, or year). There are three plots while each represents a sector.

For example, we immediately see that Facebook had the best quarter in Technology Sector. We can also tell that Starbucks had above 20/% return for the past year.

<p align="center"><img src="https://github.com/yiqiao-yin/StatisticalGraphics-Explore-Stock-Market/blob/master/figs/figs-barplot-3D.PNG"></p>

# Conclusion

This notebook investigates the potential graphics that one can discover and explore before entering the quantitative analysis of stock market or so called fintech area.

- We present real world context by directly downloading data from internet live with codes to properly clean up and handle missing values.

- The notebook also provides explanations where to call or source the functions which is stored in another script for convenience and reproducibility purpose.

- Our analysis covers multidimensional aspects include but not limited to time-series plot, qq-plot, ggplot-, parcoords, and three-dimensional bar plot.

- We have at least a paragraph of explanation in front of a plot to explain the purpose and observation including design, forms, as well as designated metrics, for each plot.

- We wrap story up with an interactive app coded through the Shiny platform.

Throughout such practice, we unlock the hidden secrets in stock data such as the correlation between an equal portfolio and market index fund as well as the inverse return path between Technology Sector and Consumer Sector.
