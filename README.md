Abadie (2005) Replication
================

In this 1986 article, LaLonde took a randomized controlled trial in
which volunteers were randomly assigned to a job trainings program or
nothing. The treatment was the program. But to illustrate the problem
with econometrics at the time, LaLonde then dropped the experimental
control group and replaced it with the PSID and the CPS in separate
analysis. Both were a random sample of the entire country. The
experimental results showed that the treatment effect was positive, but
when he used econometric modeling with a *non-experimental* control
group, the effect was consistently negative. A couple of papers by
economists Dehejia and Wahba in the late 1990s and early 2000s
introduced a new method called “propensity score based analysis”.
Sometimes it’s called matching and sometimes weighting. That’s because
there are numerous ways to use the propensity score. One the propensity
score is trimmed, this analysis regularly finds estimates that are
nearly the same as the experimental results.

For this project, I implement Abadie’s 2005 ReStud on semiparametric
diff-in-diff.

1.  Calculate a propensity score using the same covariates as used in
    the mixtape only use a series of polynomials for each one. I do the
    following analysis twice: once using a logit, once using OLS, to fit
    the propensity score.

    A. I fit a linear probability model (OLS) for one of the following
    and you will fit a logit for the second.

    B. I fit one propensity score using up to a quadratic for each
    variable for one set of analysis, and a cubic for a separate set of
    analysis.

    C. I create a histogram showing the distribution of the propensity
    score for the treatment and control group. I show the max and min
    values of the propensity score for the treatment group and for the
    control group?

    D. I drop all units whose propensity scores are less than 0.1 and
    more than 0.9 then repeat 1C.

2.  I calculate a before and after first difference for each unit.

3.  I construct a weighted difference-in-differences using the first
    equation following this
    [entry](https://causalinf.substack.com/p/callaway-and-santanna-dd-estimator).
