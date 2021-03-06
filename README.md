# TripleD-app
Shiny app to visualize data from the [TripleD database](https://www.github.com/dswdejonge/TripleD).

## How to use?
To use the Shiny app, you need data generated by the [TripleD R-package](https://www.github.com/dswdejonge/TripleD), specifically the file `database.rda`. If you do not have this file, please refer to the TripleD package for  more info on how to obtain it. If you have this file, follow these steps to set up and run the Shiny app:
1. From the [TripleD-app GitHub repo](https://www.github.com/dswdejonge/TripleD-app) click the bright green button in the top-right corner 'Clone or Download' and download the files as a zip-file.  

![download zip](https://raw.githubusercontent.com/dswdejonge/TripleD-app/master/www/zip.png)

2. Unzip the file. The folder that is created will be referred to as your working directory.
3. Place the file `database.rda` as generated by the TripleD package in the folder `data` contained in your working directory.
4. Open the file `app.R` stored in the working directory in R-studio.  
5. Make sure all required libraries are installed. You can use the `install.packages()` statements written at the top of the document.  
6. Run the app by clicking the button 'Run App' with the small green triangle. If you specifically select 'Run External' the app will open in your browser.   

![run app button](https://raw.githubusercontent.com/dswdejonge/TripleD-app/master/www/run-app.png)

6. Now you can visually browse the TripleD data!

## Preview
The interface looks something like the preview below. To view an interactive preview with randomly generated data, click the image below.  

<a href="https://dswdejonge.shinyapps.io/app_tripled/" target="_blank"><img src="https://raw.githubusercontent.com/dswdejonge/TripleD-app/master/www/preview.png"></a>
