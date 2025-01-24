# Multiple Regression Analysis of Macroeconomic Variables and Small Cap vs. Large Cap Spread Returns
This project explored the relationship between various macroeconomic variables and the monthly spread return between the Russell 2000 index and the S&P 500 index. Using multiple regression, the project identified key variables that have statistically significant (α<0.05) relationships with the spread returns. In the PDF attached, this work is done under all of the "Second Hypothesis" subheadings.

## Null Hypothesis and Alternative Hypothesis
The null hypothesis tested in this project was as follows:

Variables such as commodity prices, US government treasury bonds, corporate bond yields, and categorical market descriptors are not statistically significantly related (linearly and/or non-linearly) to the monthly spread return between the Russell 2000 index (small-cap companies) and the S&P 500 index (large-cap companies).

The alternative hypothesis was that at least one variable is statistically significantly related to the spread return.

## Approach
Monthly data of 20 variables including bond yields, commodity prices, and market trends from 1997 to 2024 were downloaded. Similarly, price data for the Russell 2000 and S&P 500 were also found.

I also wanted to ensure that the data was stationary. Originally, the monthly prices were converted to natural logs of the prices. However, this lead to values less than -1 (indicating a monthly return of below -100%) for some bond yields during the Covid-19 shock to rates in March 2020. As a result, the percent change in prices was opted for instead and this did a better job of ensuring stationarity in the data.

The issue of multicollinearity was also addressed before fitting the multiple regression model. This was handled by calculating variance inflation factors (VIF) and removing any variables with a VIF above 10.

After fitting the model, significant relationips were found and simulations involving missing data were conducted to see how the relationships would be impacted.

## Key Findings
Several variables were found to be statistically significant:
1. Yields of corporate bonds rated CCC and lower
2. Market price of crude oil
3. Yields of U.S. 2-year, 5-year, and 30-year Treasury bonds.

Quadratic terms were considered and analyzed but found to have no statistically significant relationship to any variables, so the model was purely linear.

After simulating three variables (CCC yields, the price of copper, and the price of gold) to be missing completely at random (MCAR), the new list of significant variables was:
1. Yields of corporate bonds rated BBB
2. Yields of high yield corporate bonds
3. Yields of U.S. 2-year, 5-year, and 30-year Treasury bonds.

Next, a simulation of non-ignorable missing values (MNAR) was done on the same three variables which were used to simulate MCAR. The scenario made here was that at times of extreme volatility, brokerages that provide market data may be overwhemled with people selling their assets, rendering market data unavailable. So, instead of filling in missing values with their historical median of returns (up to the point in time of the missingness), missing values were filled in with the historical 5th percentile of returns. In this case, the missingness relates to some shock market event which occurs only 5% of the time. 
The new list of significant variables was:
1. Yields of high yield corporate bonds
2. Market price of crude oil
3. Yields of U.S. 2-year, 5-year, and 30-year Treasury bonds.

From these results, it was clear that there is a fairly strong relationship between yields of U.S. Treasury bonds with varying maturities and the Russell 2000/S&P 500 relative return.

## Takeaways
This project contributed to understanding the dynamics between macroeconomic variables (particularly the yield curve) and small-cap vs. large-cap relative performance. The findings may be valuable for fund managers or traders who have a particular outlook on U.S. Treasury yields.

Potential extensions of this work could include modeling the entire yield curve for predictive insights, which may subsequently be applied to predicting the future small-cap vs. large-cap spread.
