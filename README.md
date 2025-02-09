# Portfolio_optimization_project
University project on analyzing stock returns of US healthcare companies and building optimal portfolio of these stocks
## Instructions
- To run the code, you must save all files from the Data folder in the default working directory.
- If you want to change any parameters in the model (e.g. start and end dates), please run everything up to line 239 in data_project.R, then run optimization.R, and then run the rest of data_project.R. Since the optimization task requires some computation time, I saved the code for the optimization in a separate file and uploaded sims.csv with the results of the optimization task to the Data folder
## Results
- For optimization purposes I calculated rolling tangency portfolios without short-selling. I maximized mean excess return over equally weighted benchmark over the testing period (2000-2013) by changing two parameters: __frequency of changing the weights__ and __tangency portfolio window__
-  __frequency of changing the weights__ means how often the weights of assets in portfolio are adjusted (every month, every 12 months, every 60 months, etc.)
- __tangency portfolio window__ indicates time interval, which is taken into account in the calculation of the tangency portfolio at each rebalance
- The optimal parameters are then extraploted on out_of_sample period (2013-2014)
