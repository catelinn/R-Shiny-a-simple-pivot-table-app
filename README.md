# a simple pivot data app
This is a shiny web app that allows user to upload csv or excel file for pivot table viewing.

To download a copy of this app and run locally, please `runGist("edaa916edf572b3a9db331ab6ac7ce74")`. Make sure you have R / RStudio IDE on your computer with `shiny` package loaded.

When the app runs locally, you can do the following:
- upload any file in csv or excel format (the app will take first row of data in csv file as header, otherwise, you need to add "header = FALSE" in argument text field)
- choose columns to display
- filter data by date columns if any
- filter data by other columns in the datatable directly
- run pivottable over the data created above
- download pivottable data

However, if this app is deployed to (http://www.shinyapps.io/) , pivottable stops working because it's linked to data for download, which requires `rvest` to convert html table to data frame, and `rvest` seems not working properly on shinyapp.io.

