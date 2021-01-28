MODULE fuga_mod
!
! Contains data for calculating the fugacity for different pressures
!
! Acc_factor		real    accentric factor
! k			real 	binary parameter in fugacity calculation
! T_crit		real    critical temperature
! Temp			real    temperature
! beta			real    1 / (k_b Temp)
! P_crit		real    critical pressure
! Press(i)              real    Pressure 
! Presspoint            integer number of pressure points to calculate the
!				adsorption isotherm
! fugacity              real    fugacity

 Use diverses
 IMPLICIT NONE
 SAVE
!
 REAL(kind = realkind)                             :: Temp, beta
 REAL(kind = realkind), DIMENSION(:), ALLOCATABLE  :: Acc_factor
 REAL(kind = realkind), DIMENSION(:), ALLOCATABLE  :: P_crit, T_crit
 REAL(kind = realkind), DIMENSION(:), ALLOCATABLE  :: fugacity
 REAL(kind = realkind), DIMENSION(:), ALLOCATABLE  :: Press
 REAL(kind = realkind), DIMENSION(:,:), ALLOCATABLE:: k
 INTEGER                                           :: Presspoint
!
 END MODULE fuga_mod
	
