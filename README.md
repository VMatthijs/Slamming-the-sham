# Slamming the sham: A Bayesian model for adaptive adjustment with noisy control data

  It is not always clear how to adjust for control data in causal inference,
  balancing the goals of reducing bias and variance.  We show how, in a setting with repeated
  experiments, Bayesian hierarchical modeling yields an adaptive procedure that
  uses the data to determine how much adjustment to perform.
  The result is a novel analysis with increased statistical efficiency compared to the 
  default analysis based on difference estimates.
  We demonstrate this procedure on two real examples, as well as on a series of simulated datasets.
  We show that the increased efficiency can have real-world consequences in terms of the conclusions
  that can be drawn from the experiments.
  We also discuss the relevance of this work to causal inference and statistical design and analysis more
  generally.

[Andrew Gelman](http://www.stat.columbia.edu/~gelman/) and [Matthijs Vákár](http://www.columbia.edu/~mv2745/), *Slamming the Sham:
A Bayesian model for adaptive adjustment with noisy control data.* (2020)

**Corresponding authors**:

* [Andrew Gelman](gelman@stat.columbia.edu), Department of Statistics and Department of Political Science, Columbia University
* [Matthijs Vákár](matthijs.vakar@gmail.com), Department of Information and Computing Sciences, Utrecht University

## Installation & reproducing the results in this paper

There are couple of dependencies to install, in order to explore the Bayesian models we present:

* First, you need an installation of [R](https://www.r-project.org/) to run our script.
* Second, you need to have the R library [RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) installed to run the Stan models from R.

With these dependencies present, it should be possible to run the scripts `berlim.R` and `chickens.R` to reproduce the results in this paper.

## Citation

If you find this repo useful in your research, please consider citing our work:

    @inproceedings{gelman2020slamming,
        title={Slamming the sham: A Bayesian model for adaptive adjustment with noisy control data},
        author={Gelman, Andrew and V{\'{a}}k{\'{a}}r, Matthijs},
        year={2020}
    }

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see  <http://www.gnu.org/licenses/>.
