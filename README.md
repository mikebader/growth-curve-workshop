# Growth Curve Modeling

This page contains information for the [Epidemiology and Public Health Summer Institute at Columbia University][epic] course on [growth curve modeling][gmm] taught by [Michael Bader][mdmb]. 

[epic]: https://www.mailman.columbia.edu/research/epidemiology-and-population-health-summer-institute-columbia-university-epic
[gmm]: https://reg.abcsignup.com/reg/event_page.aspx?ek=0013-0020-5f2290882f884f149df911a9c58516a7
[mdmb]: https://mikebader.net/

## Code Files

The course demonstrates the principles of growth modeling using simulations and analysis of [publicly available][zillow-data] data from [Zillow][]. Code for all of the simulations and analysis examples can be found in the `R/` directory in this repository. 

[zillow-data]: https://www.zillow.com/research/data/
[Zillow]: http://www.zillow.com/

### Downloading R & RStudio
To follow along in the course, you should download both [R][] and RStudio before attending the class. 

[R]: https://ctan.r-project.org/
[RStudio]: https://www.rstudio.com/

To download R, visit: https://cran.r-project.org/mirrors.html and pick the mirror you would like to use to download the package for your operating system (it doesn't matter which one, they all serve the same files, but generally ones geographically closer to you will be faster)

**After** you have downloaded and installed R on your machine, you should download and install RStudio. To do so visit: https://www.rstudio.com/products/rstudio/download/ and download RStudio Desktop (the free one!)

Install both according to your operating system.

### Obtaining the Course Files

To get the files for the course, you may click on the green button that says "Clone or Download" at the top of this page to download a zipped package containing the files in the repository. Unpack the files into a directory on your computer that will be easy to access. Alternatively, you may type the following into a Terminal session (if you don't know what that is, use the button above): 

	git clone https://github.com/mikebader/growth-curve-workshop

### Initializing the files for class

The `R/_init.R` file will download the data and set several settings in R that will help you run the code smoothly in class. Open the file in RStudio (you might need to use the "Open with" option on your computer). In the first line of code, you will see the following: 

	wd <- '' ## Paste the path to the class directory you downloaded in the quotes

Inside the quotations, paste the path name of the directory where you installed the files for the course. For example, on a Mac, it might look something like: 

	wd <- '/Users/mikebader/work/growth-curve-workshop/'

Save the file and then source the file by clicking the "Source" button or pressing ctrl-A (cmd-A on Mac) and then ctrl-Enter (cmd-Return on Mac). This will download the data from Zillow and save it to the `Data/` directory in a format that R reads very efficiently.

## Slides

After the course ends, the slides that I used in class will be available here: <http://mikebader.net/teaching/growth-curve-modeling>.


