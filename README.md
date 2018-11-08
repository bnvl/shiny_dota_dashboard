# Web dashboard for Dota 2 hero usage statistics

The goal of this project was to replicate the full experience of going from collecting data off the Web, to preparing it for analysis to creating a fully functional web-based dashboard that allows visual exploration of collected data. 

Subject matter was chosen based on familiarity and availability of resources on the Internet. 

You can see the finished app by visiting [this link]( https://bnvl.shinyapps.io/shiny_dota_dashboard/)

## Data preprocessing

### Aquiring the data

After some searching, we decided to use dotabuff.com as the source of the data. While the information about games on the website is structured and contains everything we are interested in regarding hero usage, it would still take some work to extract the data in a format that is optimal for our purpose. Practicing collecting data from the Web is one of the goals of this project.

To get the data that would make sense to explore, we decided to use complete set of 193 games from the biggest Dota 2 tournament of the 17-18 season - The International (that corresponds to 95 game series during group stage and bracket stages).

### Extracting the data

Before starting extracting the data we are interested in, we should decide on the format that would be easy for us to work with. As we are only interested in hero usage statistics – i.e. picks and bans, we decided to create a table where every row corresponds to a single pick or ban that was made during any game. The actual format and description for each of the columns are provided below.

| Game    | Character | Win\Loss | Side    | First pick status | Pick Type | Pick Order |
|:-------:|:---------:|:--------:|:-------:|:-----------------:|:---------:|:----------:|
| game-id | name-1    |   Win    | Radiant | Yes               | pick      | 7          |
| game-id | name-2    |   Loss   |   Dire  | No                | ban       | 2          |

⋅⋅* Game - a unique game id to be able to identify separate games
⋅⋅* Character - a unique character name to identify the character
⋅⋅* Win\Loss - whether the team that picked or banned this hero won or lost
⋅⋅* Side - which side of the map (known as the Radiant or the Dire) the team that picked or banned this hero played on
⋅⋅* First pick status – whether the team that picked or banned this hero was the one that had the first pick during the drafting phase
⋅⋅* Pick Type - whether this was a pick or a ban
⋅⋅* Pick Order - what is the absolute order of this pick or ban in the sequence of picks and bans during the game

Let us explore the way this data is stored on dotabuff. Every series of games between two teams has its own page, with data on each single game presented in the table form shown below.

![alt text](https://github.com/bnvl/shiny_dota_dashboard/misc/page_sample.jpg "dotabuff game information")

As you can see, table with the game data already has all the information that we require. Picks are the bigger colored images, bans – the smaller grayscale images, pick order is available as mouse over tooltip. We also have information about game id, which side the team played on, whether the team played first and who was the winner of the game. 

To extract this information we used python and BeautifulSoup4 package. Code for parsing the pages and creating csv with the required information is available in parse_tables.py in data_preprocess folder of this repository.

The whole parsing process was pretty straightforward minus the win\loss status of the pick – as the actual row of the table on the page contained only the name of the winner team, but not the name of the picking team. We solved this by using position of the cells of the table to figure out whether name of the team from the header of the table for that column is the same as the name of the winner team.

### Creating the "Picked together" table

While the parsed data would be enough to get the basic overview of the hero usage during the TI8, we still need to go through some more preprocessing in order to get more detailed statistics for specific heroes. 

All of the following operations were performed in R using the tidyverse library, mainly the dplyr packasge. Code is available in data_preprocess.R in the data_preprocess folder of this repository.

Seeing which heroes are usually picked alongside or against every  hero would be interesting, but is a pretty demanding computation, so it is not optimal to perform it in App every time. 

Thus, to get the data on heroes picked together, we need to create one more data table containing pairs of heroes, whether one hero was picked against or alongside another and pick rates.

To do this, there are two major steps. First is changing the format of our parsed data from long to wide with the help of tidyr *spread* function – thus ensuring that every row is a single game, columns are each single hero, and values are the result of the game. We will use the values to determine if the heroes were picked together or against each other.

The second step is using dplyr in for loops in order to create the data table with percentages of occurrences of each hero pair. The biggest hurdle during this step was figuring out how to use tidyverse packages programmatically.

Usually, when using dplyr, we pass names of the columns without the quotes, as functions quote the inputs themselves. But when iterating over column names, iterator passes in quoted column names, thus preventing dplyr from working correctly. The correct approach in this case turned out to be casting the output of iterator as symbol, manually quoting it using dplyr finction *enquo* and then telling the actual dplyr functions we pass the output not to quote it using !! notation.

After iterating over our wide table with the dplyr, we get a table with information on pairs of heroes and rates of occurrence of said pairs. We filter top 5 most picked with heroes and top 5 most picked against heroes for every hero and save it as a csv file to be used in our App.

### Converting to relative pick order

Lastly, we need to add one more column to our csv with parsed data from dotabuff pages – relative pick order. In each game both teams take turns to pick 5 heroes and ban 6 heroes, totaling 22 choices in all. Still, every pick and ban has relative order as well – for example, while the first ban for the team that has first pick will be first in the relative order as well, the first pick for the same team will be the seventh in absolute order. So, in order to more easily compare heroes by pick order, we need to convert absolute order into relative order – which is achievable using the *case_when* function from the dplyr package.


## Building the App

The App itself was built with the help of the shinydashboard library and published using the free plan on shinyapps.io.

To create the basic structure of the App as well as input widgets we used shinydashboard, to create graph outputs – ggplot2 library and to create datatable outputs – DT library.

For analyzing the data we acquired during the preprocess step, we used the tidyverse library. 
