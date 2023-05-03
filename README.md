## Data science support for marine conservation research activities
## Rshiny app to improve work flow

## Project Description
The project's goal was to develop a prototype R Shiny app that utilizes public data. This prototype serves as the foundation for establishing a robust pipeline. Once the pipeline is successfully established, we can tailor it to address specific practical use cases at the Duke Marine Lab.

## Rshiny App Demo

https://user-images.githubusercontent.com/89367409/235847889-ef817af5-d25a-4dbd-9624-ed0b74e3df6b.mp4


## Description of the Rshiny app

* The code generates a stacked bar plot with error bars using the ggplot2 package in R. The data used for the plot has variables: "year", "mean_measure_norm", "indicator", "lower_ci", "upper_ci", and "parent_area".

+ The x-axis of the plot represents the years, the y-axis represents the normalized mean measure, and the fill color represents the different indicators. The bars in the plot are stacked on top of each other and represent the mean measure values for each year for each indicator.

- Additionally, the plot includes error bars that represent the lower and upper confidence intervals of the mean measure values. The width of the error bars is set to 0.2 and they are positioned in the middle of the stacked bars.

* The colors for the fill of the bars are manually specified using the scale_fill_manual function, with "#619CFF", "#FF8C61", "#61FFB4", and "#999999".

* The title of the plot is "Measures between 2015 and 2021", and the x-axis label is "Year", the y-axis label is "Measure", and the fill color legend is labeled as "Indicator". The theme_bw() function sets a white and black theme for the plot.

Finally, the facet_wrap() function is used to create a grid of multiple plots, with each plot representing a different parent_area. The scales on the y-axis are set to "free_y" to allow for differences in the range of the y-axis between each plot. The ncol argument specifies the number of columns in the grid of plots, which is set to 1 in this case.

## Installation
* Install R and R studio
* Install different libraries in app under code folder above
* Run the app script under code folder above 

## Usage
The Rshiny app allows users to select certain parent areas and indicators for which they want to display and analyze data between 2015 and 2021. Here's how a user can use this app:

* First, the user must run the app in the R environment.

* The app's user interface (UI) will be displayed. The UI has two panels: a sidebar panel and a main panel.

* In the sidebar panel, the user can select the parent areas of interest by ticking the appropriate checkboxes under "Select parentarea". They can choose to select one, two or all three options.

* In the same sidebar panel, the user can select the indicators they want to analyze by ticking the appropriate checkboxes under "Select indicators". They can choose to select one, two, three or all four options.

* Once the user has selected the parent areas and indicators of interest, they can select the type of file they want to download using the radio button under "Select the file type for download". The user can choose either a CSV or TXT file type.

* Finally, the user can click the "Download selected data" button to download the filtered data as a CSV or TXT file.

* The main panel of the app displays the epicurve plot and a table that shows the filtered data based on the user's input. The epicurve plot shows the selected measures between 2015 and 2021. The table displays the filtered data in tabular form.

* The user can interact with the plot by hovering over the bars to display the measure value for each year and parent area, and also compare the selected indicators.

* The user can also interact with the table by sorting the data by different columns, searching for specific values, and changing the number of rows displayed.

* If the user wants to change their selection, they can go back to the sidebar panel and update their choices. The plot and table will automatically update based on the new selection.

## Credits
* Duke marine conservation
* Dr. David Gill - Assistant Professor, Duke University

## Contact
* Duke email - ga107@duke.edu
* Personal - tukumbogodwin@gmail.com

## Additional Resources
* App website - https://marinelab.shinyapps.io/deployed_app4/
