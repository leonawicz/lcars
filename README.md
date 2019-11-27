
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lcars <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://leonawicz.github.io/blog/)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<image class="orcid" src="https://members.orcid.org/sites/default/files/vector_iD_icon.svg" height="16"></a>
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis build
status](https://travis-ci.org/leonawicz/lcars.svg?branch=master)](https://travis-ci.org/leonawicz/lcars)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/leonawicz/lcars?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/lcars)
[![Codecov test
coverage](https://codecov.io/gh/leonawicz/lcars/branch/master/graph/badge.svg)](https://codecov.io/gh/leonawicz/lcars?branch=master)

[![CRAN
status](http://www.r-pkg.org/badges/version/lcars)](https://cran.r-project.org/package=lcars)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/lcars)](https://cran.r-project.org/package=lcars)
[![Github
Stars](https://img.shields.io/github/stars/leonawicz/lcars.svg?style=social&label=Github)](https://github.com/leonawicz/lcars)

[![Donate](https://img.shields.io/badge/Donate-Buy%20me%20a%20coffee-yellowgreen.svg)](https://ko-fi.com/leonawicz)

## Library Computer Access/Retrieval System ([LCARS](https://en.wikipedia.org/wiki/LCARS))

The `lcars` package provides simple approximations to LCARS style and
appearance to give static plots and interactive data analysis an LCARS
theme.

  - One key feature in `lcars` is the ability to wrap ggplot objects in
    an LCARS-themed border using `lcars_border`.
  - There is also support for an LCARS-themed Shiny UI using `lcarsPage`
    and related functions.

While the former is fun and useful for static graphs, the latter is much
more exciting because it means you can make something with the look and
feel of LCARS that also *actually functions*\!

![](man/figures/README-banner.png)

That’s right. Make your Shiny app, view it on a touch screen device like
your very own Federation issue data PADD, and transport yourself into
the future of the Star Trek universe with this 1980s sci-fi television
aesthetic\!

The static plot border and Shiny `lcarsBox` widget both give plenty of
control over the style. Given their different fundamental designs, they
provide somewhat different aspects of design control via their
respective function arguments.

### Function names

On the topic of the split between Shiny and non-Shiny functions, most of
the package is Shiny-related. There is less focus on the ggplot border
effect and related border component functions. Shiny functions use camel
case like `lcarsBox` whereas non-Shiny functions use snake case like
`lcars_border`.

### Shiny functions

There are several Shiny functions available. These are UI containers and
widgets that provide various LCARS aesthetics.

UI setup and containers:

  - `lcarsPage`
  - `lcarsBox`
  - `lcarsSweep`
  - `lcarsBracket`
  - `lcarsWell`
  - `inputColumn`

Headers and indicators:

  - `lcarsHeader`, `lcarsh1` through `lcarsh6`.
  - `lcarsRect`, `lcarsPill`, etc.

Input controls

  - `lcarsButton`
  - `lcarsCheckbox`
  - `lcarsToggle`
  - `lcarsRadio`
  - `lcarsRadioToggle`

The key widgets are `lcarsHeader`, `lcarsBox` and `lcarsSweep`. An
example of the header and box is shown above. Here is a combined LCARS
sweep and reverse sweep:

![](man/figures/README-sweep.png)

The screenshots above are taken directly from demo apps included in the
package. See `?lcarsApp` for details.

## Installation

You can install the released version of `lcars` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lcars")
```

You can install the development version of `lcars` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("leonawicz/lcars")
```

## Example

Below is a basic example using `lcars_border` to wrap a basic LCARS-like
border around a ggplot object and draw the new plot.

``` r
library(lcars)
library(ggplot2)

g <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point() + facet_wrap(~Species, 2) + theme_lcars_light()

len_frac <- c(0.3, 0.5, 0.2, 0.4, 0.3, 0.2, 0.1, 0.3)
n_seg <- c(1, 2, 0, 8)
lcars_border(g, corners = 1:3, length_frac = len_frac, side_n_segments = n_seg)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

The appearance can be improved and customized significantly from the
basic example above, as shown in the vignette. However, the Shiny
widgets are not only interactive, which is a critical feature for an
LCARS display, but they also look much better. When sharing a static
plot, you may still find it preferable to take a screenshot of a shiny
widget rather than using `lcars_border` with a ggplot object.

For more detailed examples including Shiny app examples, see the package
[vignette](https://leonawicz.github.io/lcars/articles/lcars.html).

## Packages in the trekverse

<div class="row">

<div class="col-sm-2">

<a href="https://github.com/leonawicz/rtrek"><img src="https://raw.githubusercontent.com/leonawicz/rtrek/master/man/figures/logo.png" style="margin-right:20px;margin-bottom:0;" width="60" align="left"></a>

</div>

<div class="col-sm-10">

<h4 style="padding:30px 0 0 0;margin-top:5px;margin-bottom:5px;">

<a href="https://github.com/leonawicz/rtrek">rtrek</a>: The core Star
Trek package

</h4>

Datasets related to Star Trek, API wrappers to external data sources,
and more.

</div>

</div>

<br/>

<div class="row">

<div class="col-sm-2">

<a href="https://github.com/leonawicz/trekcolors"><img src="https://raw.githubusercontent.com/leonawicz/trekcolors/master/man/figures/logo.png" style="margin-right:20px;margin-bottom:0;" width="60" align="left"></a>

</div>

<div class="col-sm-10">

<h4 style="padding:30px 0 0 0;margin-top:5px;margin-bottom:5px;">

<a href="https://github.com/leonawicz/trekcolors">trekcolors</a>: A
color palette package

</h4>

Predefined and customizable Star Trek themed color palettes and related
functions.

</div>

</div>

<br/>

<div class="row">

<div class="col-sm-2">

<a href="https://github.com/leonawicz/trekfont"><img src="https://raw.githubusercontent.com/leonawicz/trekfont/master/man/figures/logo.png" style="margin-right:20px;margin-bottom:0;" width="60" align="left"></a>

</div>

<div class="col-sm-10">

<h4 style="padding:30px 0 0 0;margin-top:5px;margin-bottom:5px;">

<a href="https://github.com/leonawicz/trekfont">trekfont</a>: A fonts
package

</h4>

True (Trek) type fonts to style your Star Trek themed graphics text.

</div>

</div>

-----

*If you enjoy my R community contributions, consider* ***[buying me a
coffee in Ko-fi](https://ko-fi.com/leonawicz)*** *so I can keep
developing and maintaining this and other packages :)*

-----

Please note that the `lcars` project is released with a [Contributor
Code of
Conduct](https://github.com/leonawicz/lcars/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
