# Fatal-Police-Shootings
Explore fatal police shootings in the United States using the Washington Post's Fatal Force Database.
* Code Author: Andie Battin
* Last Updated: December 20, 2023

# Methods
* Download the Fatal Force data from Git and read into RStudio
* Explore the data and generate visualizations
* Forecasting 

# Goals
* Explore the data and write R code to generate visualizations
* Import U.S. Census data to account for differences in population across states
* Summarize findings, provide recommendations

# Visualizations
**Table 1.** Flee-Status Table.
I used R to manipulate the data and compute the distribution of deaths by the victims flee status between racial identities. The racial groups are mapped to this table and ordered in descending order by the variable level “Not”, meaning that these people were shot and killed by a police officer and there was no attempt made by that person prior to the incident to evade the police.
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/FleeTable.png?raw=true" width="800" height="200"/>
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
Figure 2. demonstrates clustering the reported fatal police victims by race and displaying the distribution of age across these racial groups. The only racial group with a median and mean age higher than that of the data’s overall average is the white race. There is only one observation for the racial group “Black and Hispanic” but after this, “Black” people are victimized by police officers at younger ages more often than the other racial groups, including “Other” or non-identified persons. This result brings merit to the proposed legislation regarding hate-crime legislation and racial awareness, framed as officer- improvement.

**Figure 3.** Age distribution and threat-type
<p align="center">
<img src="https://github.com/acbattin/Fatal-Police-Shootings/blob/main/ThreatPlot.png?raw=true" width="800" height="500"/>
</p> 
I generated clusters largely based on the category assessment , Figure 3, shows the distribution of age group and threat levels. As presented in Figure 3, the frequency of victims of a select age group vary across the reported “threat type”. The shape of the data provides insight into the distribution of victims’ age for the perceived threat posed by the victim. The age group of civilians under 18 years old are most frequently killed by accident and very rarely killed because they pose a threat or are attempting to flee the scene. The data for victims reported shot and killed by police by accident are skewed to the left which indicates the younger age groups overrepresentation.
 
