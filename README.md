# Fatal-Police-Shootings
Explore fatal police shootings in the United States using the Washington Post's Fatal Force Database.
* Code Author: Andie Battin
* Last Updated: December 14, 2023

# Methods
* Download the Fatal Force data from Git and read into RStudio
* Explore the data and generate visualizations
* Forecasting 

# Goals
*  

# Visualizations
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
