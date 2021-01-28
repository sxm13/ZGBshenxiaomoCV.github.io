SUBROUTINE calc_fugacity_1(p,Ads_spec)
!
! The subroutine calculates the vapour fugacity using the Peng-Robinson EOS.
! The equations for the pure components  are taken from Sandler "Chemical
! and Engineering Thermodynamics" (1989) page 148, 149, 182, 222, the
! equations for the mixture are taken from Prausnitz et al. "Molecular 
! Thermodynamics of Fluid-Phase Equilibria" (1986). 
! The fugacity is used to calculate the activity in the main program.
! 
!
! fuga_coeff    	real    fugacity coefficient
! Sandler
! kappa         	real    equation 4.7-4
! alphaT        	real    equation 4.7-3
! agreek        	real    equation 4.7-1
! bgreek        	real    equation 4.7-2
! A, B          	real    equation 4.7-5
! alpha, beta, gamma   	real    table 4.4-3
! Prausnitz
! agreek_sys            real 	equation 10.3-14/16
! b_greek_sys		real   	equation 10.3-13/15 (c12 = 0, simple mixture
!				of nonpolar components)
! Conv_Crit     	real	convergence criteria for Newton Rhapson method
! Z0, Z1        	real    approximation of zero, z: compressibility factor
!
! R			real    universial gas constant
! p            input    integer pressure point
! ads_species  input    integer adsorbed species
! 
 USE fuga_mod
 USE molecules_prop
 USE diverses
!
 IMPLICIT NONE
!
 REAL(kind = realkind)                        :: fuga_coeff
 REAL(kind = realkind)                        :: alpha, beta_f, gamma
 REAL(kind = realkind), DIMENSION(no_species) :: kappa, alphaT, agreek, bgreek
 REAL(kind = realkind)                        :: agreek_sys, bgreek_sys
 REAL(kind = realkind)                        :: A, B
 REAL(kind = realkind)                        :: Z0, Z1
 REAL(kind = realkind)                        :: sum_za
 INTEGER                       	              :: i,j, spec
 INTEGER                                      :: p, ads_spec
!
 REAL(kind = realkind), PARAMETER  :: R = 8.314           
 REAL(kind = realkind), PARAMETER  :: Conv_Crit = 1e-5
!
 DO spec = 1, no_species
    kappa(spec) = 0.37464 + 1.54226*acc_factor(spec) - &
                  0.26992*acc_factor(spec)*acc_factor(spec)
    alphaT(spec) = (1 + kappa(spec) * (1 - sqrt(Temp/T_crit(spec))))**2
!
! calculate a (a_greek), b (b_greek) for pure components
!
    agreek(spec) = 0.45724 * (R * T_crit(spec))**2 / P_crit(spec) * alphaT(spec)
    bgreek(spec) = 0.0778 * R * T_crit(spec) / P_crit(spec)
 END DO
!
! calculate the system a and b factors
! 
 agreek_sys = 0.
 bgreek_sys = 0.
 IF (no_species == 1) THEN
   agreek_sys = agreek(1)
   bgreek_sys = bgreek(1)
 ELSE 
   DO i = 1, no_species
     DO j = 1, no_species
       agreek_sys = agreek_sys + &
                    y_gas(i)*y_gas(j)*SQRT(agreek(i)*agreek(j))*(1-k(i,j))
     END DO
       bgreek_sys = bgreek_sys + y_gas(i)*bgreek(i)
   END DO
 END IF
!
! calculate A and B
!
    A = agreek_sys * Press(p) / (R * Temp)**2
    B = bgreek_sys * Press(p) / R / Temp
!
! calculate alpha, beta, gamma
!
 alpha = B - 1
 beta_f = A - 3*B*B - 2*B
 gamma = -A*B + B*B + B*B*B

!
! Searching for zero using Newton-Rhapson
! Z**3 + alpha*Z**2 + beta_f*Z + gamma = 0
! Z: compressibility facor
!
 Z1 = 1.0
 DO
    Z0 = Z1
    Z1 = Z0 - (Z0**3 + alpha*Z0**2 + beta_f*Z0 + gamma) / (3*Z0**2 + 2*alpha*Z0 &
            +beta_f)
    IF (ABS(Z1 - Z0) < Conv_Crit)  EXIT
 END DO
!
! calculate the sum(aik*zi)
!
 sum_za = 0.0
 DO spec = 1, no_species
    sum_za = sum_za + y_gas(spec)*SQRT(agreek(ads_spec)*agreek(spec))*(1-K(ads_spec,spec))
 END DO
!
! calculate fugacity coefficient
!
 fuga_coeff = EXP(bgreek(ads_spec)/bgreek_sys*(Z1-1) - LOG(Z1 - B) - &
              agreek_sys/(2.*SQRT(2.0)*bgreek_sys*R*temp)* &
              (2*sum_za/agreek_sys-bgreek(ads_spec)/bgreek_sys) &
              * LOG( (Z1+(1+SQRT(2.0))*B)/(Z1+(1-SQRT(2.0))*B)))
!	      
 fugacity(ads_spec) = fuga_coeff *y_gas(ads_spec) * Press(p) 
!
END SUBROUTINE calc_fugacity_1
