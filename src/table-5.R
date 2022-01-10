exp(-0.0950786 - 0.1132972)-1
get_reform_effect <- function(y98, y96){
  100*(exp(y98 - y96) - 1)
}
# Hurdle NB
# left: -3.481858
get_reform_effect(-0.0747395, -0.0393003)
# right: -18.80981
get_reform_effect(-0.0950786, 0.1132972)

# Hurdle Probit Poisson Log Normal 
# left: -5.010136
get_reform_effect(-7.964e-02 , -2.824e-02)
# right: -11.89052
get_reform_effect(-5.948e-02, 6.711e-02)


# left: -8.573883
get_reform_effect(-8.766e-02 , 1.979e-03)
# right: -6.185429
get_reform_effect(-8.813e-02, -2.428e-02)


# left: -8.573883
get_reform_effect(5.323e-01 , 6.586e-02)
# right: -6.185429
get_reform_effect(-7.810e-02 , 4.045e-01)
