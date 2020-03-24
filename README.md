# Soccer-Season-Prediction
To summarise, I used a Poisson Generalised Linear Model to predict goals scored in a certain fixture. This is because the goals scored in a 90 minute soccer match is approximately distrbuted by a Poisson distribution as we can see below. I fitted this model to season 1 results and then used this model to predict season 2.


![Image of framework](https://github.com/jackapbutler/Soccer-Season-Prediction/blob/master/Elements/Poisson.png)


After performing these prediciton on the second season fixtures I wanted to generate a stochastic simulation of potential league table outcomes. This was completed using a Poisson distribution where the rate parameter for the fixture is the average home/away goals scored by each individual team based on season 1.

