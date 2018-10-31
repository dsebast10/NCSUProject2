README
================

Evaluating the Movement of Crime Guns in the US
===============================================

All data displayed is publically available from the ATF: <https://www.atf.gov/resource-center/data-statistics>

Crime Guns
----------

A crime gun is any gun that was used in a crime or is possessed illegally.

When a crime gun is recovered by a law enforcement agency, it's serial number should be submitted to the ATF's eTrace program to find who the initial buyer was. This information is important to identify potential sources of gun trafficking.

It is important to note that not all law enforcement agencies submit recovered firearms to the ATF for tracing, so there is a significant observer bias.

The app defaults to the source states for the District of Columbia for a few reasons:

1.  The District of Columbia has a very interesting relationship with firearms. There is only one Federal Firearms Licesnsee (FFL), aka "Gun Dealer", in the entire city. It is also in close proximity to several gunshows that occur frequently in the Northern Virginia area. While some cities like New York and Chicago make it very difficult to buy and posess guns, they are within larger states with more lax gun laws.

2.  The author works for the Metropolitan Police Department (MPD) in the District of Columbia

Required Packages
-----------------

-   `tidyverse`
-   `shiny`
-   `shinydashboard`
-   `readxl`
-   `plotly`
-   `magrittr`
-   `DT`
