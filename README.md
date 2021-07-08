# Heart-Attack-Prediction

The project focused on the heart attack prediction. Done by Chongyu Qu, Camille Duan.<br/>
Chongyu Qu contribution: Data preprocessing, feature plots and correlation matrix plot
## Heart attack background and Data explanation
### Heart attack background
Information from CDC, heart attack, also called a myocardial infarction, happens when a part of the heart muscle doesn't get enough blood.
The more time that passes without treatment to restore blood flow, the greater the damage to the heart muscle.<br/>
Symptoms include: <br/>
**Chest pain of discomfort**: most heart attacks involve discomfort in the center or left side of the chest
that lasts for more than a few minutes or that goes away and comes back. The discomfort can feel like uncomfortable pressure, squeezing, fullness, or pain.<br/>
**Feeling weak, light-headed, or faint.**<br/>
**Pain or discomfort in the jaw, neck, or back.**<br/>
**Pain or discomfort in one or both arms or shoulders.**<br/>
**Shortness of breath.**<br/>

### Data overview
[The link to the kaggle dataset](https://www.kaggle.com/rashikrahmanpritom/heart-attack-analysis-prediction-dataset)<br/>

The data has 303 samples and 13 features, where 5 are numerical features and 8 are categorical features. The labels of the data are [0,1] where 0 = less chance of heart attack
1 = more chance of heart attack.<br/>
We did the plot for each feature (ignore feature slope, it may not have any impacts on our prediction) under each condition(0 or 1) in our shinyapp. I will show them in the video. 

### Rstudio Version and Library
The R version: 4.0.3 (2020-10-10),<br/>
platform: x86_64-apple-darwin17.0,<br/>
Library: tidyverse, patchwork, plotly, shiny, rsconnect, shinythemes, markdown, reshape2, caret

### Shinyapp explanation
There are two buttons next to the title heart attack prediction, **Feature Analysis** and **Predictions.**<br/>
For the **Feature Analysis** page, you can select one categorical and one numerical feature to display their corresponding count or density plot.<br/>
The main panel has two buttons, **Feature Plots** and **Correlation Matrix.** Choose feature plots, the plots will be displayed, where the top one is the categorical feature count plot, 
the bottom one is the numerical feature density plot. Choose correlation matrix, a correlation matrix of every feature will be displayed. <br/>

For the **Predictions** page, you can select or type in your sex, age, maximum heart rate and chest pain type. A logistic regression classifier will be performed on these four features
of our dataset, then based on your input, the probability of heart attack risk will be predicted.<br/>
[The link to the live shinyapp](https://camille306.shinyapps.io/advanceds_final_project/)

### Video Presentation
The video will give the example of how to use this shinyapp.<br/>
[The link to the live video presentation](https://drive.google.com/file/d/1j-y-PDegjmksYU1PFKzfJbn_oNL_Hl-Z/view?usp=sharing)
