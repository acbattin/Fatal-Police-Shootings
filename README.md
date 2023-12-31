# Fatal-Police-Shootings
Explore fatal police shootings in the United States using the Washington Post's Fatal Force Database.
* Code Author: Andie Battin
* Last Updated: December 20, 2023

# Methods
* Download the Fatal Force data from Git and read into RStudio
* Explore the data and generate visualizations
* Forecasting, Poisson distribution, ARIMA modeling

# Goals
* Explore the Washington Post's Fatal Force data and using RStudio, write R code to generate visualizations that abide by proper guidelines.
* Import U.S. Census data to account for differences in population across states. 
* Summarize findings, provide recommendations

# Visualizations
**Table 1.** Flee-Status Table.
I used R to manipulate the data and compute the distribution of deaths by the victims flee status between racial identities. The racial groups are mapped to this table and ordered in descending order by the variable level “Not”, meaning that these people were shot and killed by a police officer and there was no attempt made by that person prior to the incident to evade the police.
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/FleeTable.png?raw=true" width="600" height="200"/>
</p>
The reason I wanted to make sure to conduct and shape my analysis in this way is because there has been a lot of media attention surrounding the ongoing cases and trials involving officer shootings. In these cases, the defense must prove that it was in the best interest in keeping the peace, for that officer to shoot and kill the victim or that there was no other option available. The results of this analysis are troublesome, given that out of the seven racial groups identified in Table 1 there are four racial groups in which more than half of the people are not fleeing yet are being shot and killed by police officers. Asian victims of police shootings lead the racial groups in terms of having the highest distribution of victims that did not flee.

**Figure 1.** Stacked Bar Chart. The goal of this plot is to visualize the disparity among the racial groups in addition to explore the distribution of gender within each racial group.
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/GenderStacked.png?raw=true" width="600" height="500"/>
</p>

The number of people killed by firearms by the police January 1, 2015, to December 1, 2023, were grouped into five different races. The entries that were missing data for the race variable were not plotted, and since there is only one singular entry for “Black and Hispanic” was grouped into the “Other” variable for the purpose of this plot. We see that the data collected is predominately of white victims, and overall, the distribution of race seems similar to that of the United States on average (U.S. Census, 2023). Figure 1 also provides us with the added information, males are far more often the victims of a fatal police shooting than women are across all racial groups.

**Figure 2.** Age Distribution across Racial Groups. Goal is to compare the age distributions across racial groups and if anything can be determined or suggested.
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/AgeRaceBox.png?raw=true" width="800" height="500"/>
</p> 
Figure 2 demonstrates clustering the reported fatal police victims by race and displaying the distribution of age across these racial groups. The only racial group with a median and mean age higher than that of the data’s overall average is the white race. There is only one observation for the racial group “Black and Hispanic” but after this, “Black” people are victimized by police officers at younger ages more often than the other racial groups, including “Other” or non-identified persons. This result brings merit to the proposed legislation regarding hate-crime legislation and racial awareness, framed as officer- improvement.

**Figure 3.** Age distribution and threat-type
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/ThreatPlot.png?raw=true" width="800" height="500"/>
</p> 
I generated clusters largely based on the category assessment , Figure 3, shows the distribution of age group and threat levels. As presented in Figure 3, the frequency of victims of a select age group vary across the reported “threat type”. The shape of the data provides insight into the distribution of victims’ age for the perceived threat posed by the victim. The age group of civilians under 18 years old are most frequently killed by accident and very rarely killed because they pose a threat or are attempting to flee the scene. The data for victims reported shot and killed by police by accident are skewed to the left which indicates the younger age groups overrepresentation.

**Figure 4.** Aggregate annual police fatal shootings in the U.S. since January 1, 2015 as of December 1, 2023.
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/AnnualCounts.png?raw=true" width="800" height="500"/>
</p>
I used clustering to generate a proper vertical bar plot to visualize the total annual counts for all the fatal police shootings in the United States from January 1, 2015, to December 1, 2023. As demonstrated by the plot, the average annual count of fatal police shootings has been above 1,000 incidents since it first crossed that threshold in 2020. Trends in crime are difficult to curb because determining their underlying factor is time and money-consuming and it is difficult to change a constant trend like this.

**Figure 5.** Time series analysis
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/MonthlyCounts.png?raw=true" width="800" height="500"/>
</p>
Figure 5 is a time-series plot that shows the aggregate total number of fatal police shootings on a monthly basis from January 2015 to December 1, 2023. The lack of data entries for December 2023 is the reason it appears as though there has been a down-trend in fatal police shootings in the U.S., but this is simply because there is only data for December 1, 2023. There is no apparent trend or seasonality present with the fatal officer shooting data as demonstrated by Figure 5.

# Models
**Figure 6.** Forecasting Crime Data: ARIMA Model
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/ARIMAplot.png?raw=true" width="800" height="500"/>
</p>
The following plot uses an ARIMA model to forecast the total number of fatal police shootings in the United States next month, January 2024. The model generated a prediction for the total number of police shootings for next month, displayed as a blue bar filled from the minimum to maximum predicted number of fatal police shootings in the United States with the predicted value. The model used the data for fatal shootings from January 2015 to December 1, 2023, and using this, the forecasted total number of fatal police shootings for January 2024 is approximately 84 fatal police shootings, with a 95% prediction interval of (57.63, 111.27).

**Figure 7.** ptools in R: Checking Poisson Distribution
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/PoissonFitOutput.png?raw=true" width="800" height="400"/>
</p>
When checking to see if the data is Poisson distributed, we first check the fit of the Poisson distribution for annual data and find that the mean and variance are not close enough, the mean is reported as 1011.778 fatal shootings annually and the variance is 1638.44 fatal shootings annually. After manipulating the date variable in the data, I was able to use grouping to sum the number of incidents for each day in the United States. Using R, I checked the fit for the day-level data and found the mean and variance to be very close, mean is 2.8 and variance is 2.9 the unit is fatal police shootings in the U.S. daily, not on an annual basis like we saw previously which was overdispersed as demonstrated by the variance being a higher value than the mean.

<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/ChiS.png?raw=true" width="800" height="100"/>
</p>
The Chi- squared goodness of fit test for the day count data was run to confirm the accuracy of what was previously presented in the Poisson fit table, that the p-value is 0.038.

# Concluding Remarks
Keeping in mind that all of the data is for on-duty, specifically, is very troublesome because the analysis of fatal police shootings in the U.S. since January 1, 2023, show that the proportion of officers involved wearing a camera is less than 20% of all of the officers. Body camera footage plays a significant role in determining whether charges are filed against the officer that discharged their weapon, and if it is available, it is often used in court to determine the justification for the shooting, and in order to promote an atmosphere of accountability, there should be more stringent requirements placed on law enforcement officers so that they are more often than not, wearing a body camera in case something was to happen.

The powerful solutions of our analysis may hold a future implication on police training and may justify national agencies that already collect firearm data such as the CDC and the FBI to expand the required data collected from local police departments to include information regarding fatal officer shootings in their annual violence and firearm-reports.
