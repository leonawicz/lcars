## Test environments

* local Windows 10 install, R 4.3.1
* win-builder
* Ubuntu 20.04 devel, oldrel, release

## Update release

* Added required package alias per CRAN request.

Note: Because this is a Shiny add-on package, examples are not executed if they require an interactive session.
For consistency, I structured the code of Shiny examples to follow the conditional execution style used in examples in the `shiny` package.

## R CMD check results

0 errors | 0 warnings | 0 notes

I have also run R CMD check on downstream dependencies 
(https://github.com/leonawicz/lcars/blob/master/revdep/). 
All packages passed.
