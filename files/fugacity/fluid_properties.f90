SUBROUTINE fluid_properties
!
! critical data from: Reid, Prausnitz, Poling 'The properties of gases and
! liquids', Mc Graw-Hill, 1987
!
USE diverses
USE fuga_mod
USE molecules_prop
!
IMPLICIT NONE
!
INTEGER     :: i
!
! Module fuga_mod
!
! Acc_factor            real    accentric factor
! T_crit                real    critical temperature in K
! P_crit                real    critical pressure in Pa
! k(i,j)		real 	binary coefficient in Peng-Robinson EOS
!
 ALLOCATE(acc_factor(no_species), P_crit(no_species), T_crit(no_species))
 ALLOCATE(k(no_species,no_species))
!

DO i = 1, no_species
!
   SELECT CASE(name_spec(i))
    CASE('CH4') 
          Acc_factor(i) = 0.011
          T_crit(i) = 190.4
          P_crit(i) = 4.6e6
    CASE('C2H6')   
          Acc_factor(i) = 0.099 
          T_crit(i) = 305.4
          P_crit(i) = 4.88e6
    CASE default
         Write(*,*) name_spec(i), ' not in critical data database'
         Write(*,*) 'Add in subroutine fluid_properties.f90'
         STOP
   END SELECT
END DO
!
 k = 0.0
! Values from SI Sandler, Chemical and Engineering Thermodynamics
! John Wiley, New York
IF(no_species == 2) THEN
   If(name_spec(1) == 'CH4' .And. name_spec(2) == 'C2H6') Then
      k(1,2) = -0.003
   ELSE
      Write(*,*) 'No binary coefficient defined for Peng-robinson EOS'
      Write(*,*) 'Add parameter for ',name_spec(1),' ,',name_spec(2), &
                 ' or check that you can use a default value of zero'
      Write(*,*) 'Make changes in subroutine fluid_properties.f90.'
      STOP
   END IF
   k(2,1) = k(1,2)
END IF
!
!  
END SUBROUTINE fluid_properties 
