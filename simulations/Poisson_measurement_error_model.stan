//modelling uncertainty in a Poisson predictor variable

// data being input
data {
  // number of data points
  int <lower=0> N;
  
  // chick body mass
  vector[N] a; 

  // number_of_visits
  int <lower=0> v[N] ;
} 


// parameters to be estimated
parameters {
  real beta_0; 
  real beta_1; 
//  real <lower=0> alpha;
//  real <lower=0> beta;
  real <lower=0> meanPR;
  real <lower=0> sigmaPR;
  vector <lower=0> [N] PR;
  real <lower=0> sigma; 
} 

model {
// prior on residual variance
//  sigma ~ inv_gamma(0.001,0.001);

// number of visits (v) is drawn from a poisson distribution with mean PR (i.e. expected provisioning rate)
//  PR ~ gamma(alpha,beta);
  PR ~ lognormal(meanPR,sigmaPR);
  v ~ poisson(PR);

// residual error
  a ~ normal(beta_0 + beta_1 * PR,sigma);
}
