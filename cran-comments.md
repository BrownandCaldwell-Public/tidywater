## Resubmission

This is a resubmission. In this version I have:

* Changed references in the DESCRIPTION to match the form: authors (year, ISBN:...) or authors (year) <[https:...]https:...>. (No doi available)

* Removed unnecessary line breaks in the DESCRIPTION

* Added the following Rd-tags:
      balance_ions_chain.Rd: \value
      convert_watermg.Rd: \value
      
* Replaced \dontrun with \donttest in examples requiring >5 sec to execute.

* Added `workers = 2` to all examples and vignettes that use `plan(multisession)` to ensure no more than 2 cores are used.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Possibly misspelled words in DESCRIPTION:
   Borchardt (22:24)
   Crittenden (21:5)
   McGraw (20:58)
   Tchobanoglous (22:5)
   Trussell (21:24)
   USEPA (24:5)
  
  These are author names used to cite sources in the DESCRIPTION.
