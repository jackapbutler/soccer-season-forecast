# Soccer-Season-Prediction
To summarise, there are multiple Excel files where I have calculated the final season 1 league standings from the raw results, the stage at which the league winners secured the title and the "biggest upset" in the season 1 fixture results. 

I then used a Poisson Generalised Linear Model to predict goals scored in a certain fixture. This is because the goals scored in a 90 minute soccer match is approximately distrbuted by a Poisson distribution as we can see below. I fitted this model to season 1 results and then used it to predict season 2.

![Image of framework](https://github.com/jackapbutler/Soccer-Season-Prediction/blob/master/Elements/goalplot.png)

# Stochastic League Table Simulation
After performing these predictions on the second season fixtures I wanted to generate a stochastic simulation of potential league table outcomes. This was completed using a Poisson distribution where the rate parameter is the average home/away goals scored by each individual team based on season 1.

![Image of framework](https://github.com/jackapbutler/Soccer-Season-Prediction/blob/master/Elements/season2_standings_predicted_sorted.PNG)
