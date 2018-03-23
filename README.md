# An R Package for Fitting Models to 2-Dimensional (x-y) Data

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/regressoR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/regressoR/)

## Introduction

In many scenarios, we have two-dimensional data in the form of `(x, y)` tuples. We then
want to know the relationship between the `x` and the `y` coordinates. The resulting
model could be any type of function `f`. In the ideal case, `f(x)=y` will hold, but of
course if the data results from measurements, there may be some errors and deviations
in it. The goal is then to find the function `f` for which these deviations are as small
as possible.

However, this goal could be misleading: If we have `n` points, we can always use a polynomial
of degree `n-1` to go exactly through all points: a constant for one point, a linear function
through two points, a quadratic function through three points, a cubic polynomial through
four points, etc. Such a model would not necessarily make much sense. Actually, such a
model would not give us any new information since it basically just "encodes" the `(x, y)`
tuples. This is called overfitting, because this model fits well to the data that we know
but won't fit to data that we will measure in the future from the same source. What we
want are thus models that *a)* fit well to the observed data but *b)* are not overfitted,
i.e., are likely to generalize and to fit to future observations. Ideally, of course, *c)* the
models should have few parameters and be compact.

Here, we try to make a package for getting such models automatically. Our package uses
a library of models which it applies to the measured data. It uses cross-validation to
pick a model which seems to generalize well and then trains it again on the complete
data set. Our library furthermore attempts to deal with the fact that sometimes, models
will not fit well on the raw data but on a log-scaled version of the data. It does so
by creating multiple different representations of the data and include these representations
and the models in the learning step. If two models seemingly fit similarly well to the
data, then the smaller one will be selected.

Our package is still in a very early version, it is not efficient, it is slow. But it
already somewhat works.

## Motivating Example

Let's say we have some data `dx` and `dy` of more or less unknown nature and want to find
a function `f` which can properly represent the relationships between `dx` and `dy`. The
easiest way to do this with our package is simply invoking `regressoR.learn(x=dx, y=dy)`.
This will apply the complete palette of solvers and models to the data, internally perform
cross-validation to fit the best fitting model, fit this model again on the complete data,
and return it.

    dx <- rnorm(100);
    dy <- rnorm(n=100, mean=50*dx*dx-33);
    plot(dx, dy)
    result <- regressoR.learn(x=dx, y=dy);
    result@f
    # function (x)
    # -32.5442186855071 + (x * (0.776119279549966 + (x * 49.7907873618706)))
    result@quality
    # [1] 0.2455075
    dx.sorted <- sort(dx)
    lines(dx.sorted, result@f(dx.sorted), col="red")

Of course, the approach is limited to the functions currently in the model library. But it
also automatically tries to apply the models to transformed versions of the data, such as
log-scaled variant. This way, even with just a linear and a quadratic model in the library,
we can represent exponential relationships:
    
    dx <- runif(100) + 0.1
    dy.raw <- 1 + 0.4* exp(5 - 3*dx + 0.6*dx*dx)
    dy <- rnorm(n=100, mean=dy.raw)
    plot(dx, dy)
    result <- regressoR.learn(x=dx, y=dy);
    dx.sorted <- sort(dx)
    lines(dx.sorted, result@f(dx.sorted), col="blue")
    result@f
    # function (x) 
    # exp(x = ((1.14651435794791 + (x * (-1.4311431348238 + (x * 0.439385151660909)))) * 
    #     4.51412008649805) - 0.112652709296488)
    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/regressoR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.
    
## Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
