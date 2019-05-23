# Slamming the sham: A Bayesian model for adaptive adjustment with noisy control data

  It is not always clear how to adjust for control data in causal inference,
  balancing the goals of reducing bias and variance.  In a setting with
  repeated experiments, Bayesian hierarchical modeling yields an adaptive
  procedure that uses the data to determine how much adjustment to perform.  We
  demonstrate this procedure on the example that motivated this work, a much-cited
  series of  experiments on the effects of low-frequency magnetic fields on chick
  brains. We also discuss the relevance of this work to causal inference and
  statistical design and analysis more generally.

[Andrew Gelman](http://www.stat.columbia.edu/~gelman/) and [Matthijs Vákár](http://www.columbia.edu/~mv2745/), *Slamming the Sham:
A Bayesian model for adaptive adjustment with noisy control data.* (2019)

**Corresponding authors**:

* [Andrew Gelman](gelman@stat.columbia.edu), Department of Statistics and Department of Political Science, Columbia University
* [Matthijs Vákár](matthijs.vakar@gmail.com), Department of Statistics, Columbia University

## Installation & reproducing the results in this paper

There are couple of dependencies to install, in order to explore the Bayesian models we present:

* First, you need an installation of [R](https://www.r-project.org/) to run our script.
* Second, you need to have the R library [RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) installed to run the Stan models from R.

With these dependencies present, it should be possible to run the script `chickens.R` to reproduce the results in this paper.

## Citation

If you find this repo useful in your research, please consider citing our work:

    @inproceedings{gelman2019slamming,
        title={Slamming the sham: A Bayesian model for adaptive adjustment with noisy control data},
        author={Gelman, Andrew and V{\'{a}}k{\'{a}}r, Matthijs},
        year={2019}
    }

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see  <http://www.gnu.org/licenses/>.
