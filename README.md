# Beheading a Hydra: Kingpin Extradition, Homicides, Education Outcomes, and the End of Medellin’s Pax Mafiosa
#### Highlighted Skills: Causal inference, quantitative research, R, GIS and geospatial analysis

Some highlights from my work on a long-term project studying how the extradition of a major crime lord impacted gang dynamics in Medellin, Colombia. 
Note: Due to privacy concerns, I cannot share the raw individual-level data used in the paper. This code will not exactly reproduce what we have in the paper. It is intended only as a demonstration of my skills in R.

### Context
What happens when the leadership is severed from a highly consolidated, violent mafia organization? If it increases violence, are the consequences confined to gang-affiliated individuals, or are there costs for society as a whole?

In 2008, a major narcotics kingpin named Don Berna was extradited from Colombia to the United States. In his stronghold of Medellin, the resulting power vacuum in the
city’s criminal economy set off an internecine war for power that saw homicide rates in the city double, before organized crime governance structures finally re-consolidated and violence returned to pre-extradition levels.

![HomicidesPer](/ImagesForReadMe/img/HomicidesPer.png)


### My Work
I identified the areas of the city controlled by Don Berna and studied the evolution of homicides before and after the extradition in the areas under his control and in those not under his control. The maps below suggest homicides increased much more in the Berna-controlled neighborhoods.

![BernaMap](/ImagesForReadMe/img/BernaMap.png)

I then compared the monthly trends in homicides for Berna-controlled areas to other other areas. As you can see, the trends run parallel right up until the extradition, at which point homicides spike much more in the Berna-controlled areas.

![BernaTrend](/ImagesForReadMe/img/BernaTrend.png)

I was curious how this increased students' exposure to violence and whether increased exposure to violence impacted education outcomes. To do this, I counted the annual number of homicides within 250 meters for each public school in the city. I then used quasi-experimental difference-in-differences and instrumental variables strategies to estimate the effect of the extradition on exposure to violence, test scores, dropout, and teacher turnover.

### Result
My findings suggest that Don Berna’s extradition caused an average of 1.2 more homicides within 250 meters of a school each year during the years immediately following the extradition. The results also show that each additional homicide within 250 meters of a school led to a 1.1 percent decline in math test scores, equivalent to 0.05 standard deviations.

You can find the full paper [here](https://dx.doi.org/10.2139/ssrn.4880218). I presented results at the 2023 Meeting of the Latin American and Caribbean Economic Association. A research brief was published by the Cato Institute and the paper is currently under peer review at _Journal of Development Economics_.
