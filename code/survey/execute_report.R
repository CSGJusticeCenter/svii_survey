#OUTPUT_FORMAT
#change output format to either be a word docx or html

#OUTPUT_FILE
#change second argument of paste0 string to label version of output

#PARAMS
#set_title - title of report
#yearofsurvey - this is the latest data year collected from the MCLC survey (numeric)

rmarkdown::render('05_multiple_imputation.Rmd',
                  #output_format = Gmisc::docx_document(),
                  output_format  = "html_document",
                  output_file = paste0(
                    'SVIIestimates_v',
                    "6",
                    "_",
                    Sys.Date(),
                    '.html'
                  ),
                  params = list(
                    set_title    = "2024 MCLC Survey Results",
                    yearofsurvey = 2023
                  )
)


#v1 - 1st draft of data from Martha (github main branch)
#v2 - 2nd draft of data from Martha, plus special missings code (github taskcsgadj branch)
#v3 - 3rd draft of data from Josh/Jess (using 2nd draft of data from Martha, plus additional cleaning, NO special missings code)
#v4 - 4th draft of data from Josh/Jess (using 2nd draft of data from Martha, plus additional cleaning, NO special missings code, only change 2022-2023 data)
#v5 - 5th draft of data from Martha (changes from 3rd draft, plus BJS data inclusion and other small changes from Martha)
#v6 - 6th draft of data from Martha (changes from 5th draft, plus making ME supervision values as non-missing since they abolished parole)