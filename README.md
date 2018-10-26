# Analyzing the impact of Obamacare and Trumpcare on healthcare securities

Public health policies in the U.S.A is a very shaky business. One of the major moves made by the Obama administration while it was in office, was that they tried to bring a major change to health insurance policies through the "Patient Protection and Affordable Care Act" or nicknamed "ObamaCare". I'm not going into the details into what the attributes of this act was, however it is enough to know that it was vehemently opposed by the Republican Party. Enter Donald Trump, the current president(at least as of when I am writing this) of the United States, who zealously announced that one of the very first moves he would make as President would be to remove ObamaCare and bring in another act in its place(which ultimately did not work). 

Now, while policy changes were being made in office, the news of such changes were naturally broadcasted on newspapers and news channels all around. The purpose of this project is to analyze the impact, if any, of these policy changes on the stock market, specifically on Pharmaceutical, Biotechnology and Health Insurance companies (Or at least on the stocks of companies who are in some way engaged in these )

Methodology:
1) The first step was to create a timeline of events of policy changes made by the Obama and Trump administration. The "Events" are taken as months, and not specific days on which they were announced. 
The time in my data varies from "2009-01-01" to "2017-12-31".  

2) Because there are a large number of public Pharmaceutical and Biotechnology companies, I have seperated the stocks trading on NASDAQ and NYSE, i.e I have seperately taken Pharma stocks on NASDAQ, Pharma stocks on NYSE, Biotech stocks on NASDAQ and Biotech stocks on NYSE.
Since there aren't a whole lot of "Health" Insurance stocks, I have taken stocks trading on both NASDAQ and NYSE together.

3) For each of these, the first step was to form an index. This was done simply by taking a Price-Weighted average (similar to how the Dow Jones Industrial Average is calculated)

4) Now that we have the index values for our timeline, we calculate returns of our index and model for volatility in our index. This is done using ARMA-GARCH volatility modelling. The "ARMA" part of it is done to remove any serial dependence in the returns, if any. GARCH is then applied to model volatility. 

5) We shall then have a measure of volatility of each (Trading)day from "2009-01-01" to "2017-12-31". Now we can begin testing the "Impact" of policy changes on Healthcare Stocks. 
How this is done is as follows:
- If the news of policy changes does indeed have an impact on HealthCare Securities, then it would cause a rise in volatility for any given month. 
- Therefore we will measure the %change in volatility between months where the announcement was made, the month before it, and the month after it. If there indeed was an impact on these securities, the the % change in volatity would have been high betweeen a "No-News-Month" and a "News-Month". 

6) When done for all the events in our timeline, we will have a "Distribution" of % changes" through our sample. We can then perform a one-sample t-test, by testing the hypothesis:
Null Hypothesis: There is no impact of policy changes on HealthCare stocks; implying that %Change of Volatility = 0.
Alternative Hypothesis: There is an impact of policy changes on Healthcare stocks, impying that %Changes of Voltility != 0

7) Finally, calculate the test statitic of the t-test, and check whether it is sufficient enough to  reject the Null Hypothesis.
