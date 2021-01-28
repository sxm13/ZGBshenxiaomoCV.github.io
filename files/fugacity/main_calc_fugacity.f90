Program calc_fugacity
!
! Calculates fugacity from pressure data with 
! Peng Robinson equation of state
!
Use molecules_prop
Use fuga_mod
Use diverses

IMPLICIT NONE

Character(len = 100)                            :: file_press, file_fuga
Integer                                        :: i, j, no_points, decision
Real(kind = realkind)                          :: p_total
Real(kind=realkind), Dimension(:), Allocatable :: fuga, p_array, y_array
Real(kind=realkind), Dimension(:,:), Allocatable :: fuga_bin
Character(len = 12), Dimension(:), Allocatable :: char_fuga, name
Character(len=9)                               :: fmt
Character(len=15)                              :: string
Character(len=2)                               :: comma

Read(*,'(/,A)') file_press
Read(*,'(/,A)') file_fuga
Read(*,'(/,I2)') no_species

Allocate(name_spec(no_species)) 
Allocate(fugacity(no_species))
If(no_species == 1) THEN
   y_gas(1) = 1.0
   y_gas(2) = 0.0
End If

Do i = 1,no_species
   Read(*,'(/,A)') name_spec(i)
End Do

Read(*,'(/,F7.3)') temp

IF(no_species == 2) THEN
  Read(*,'(/,I3)') decision
  IF(decision == 1) THEN
    Read(*,'(/,F8.3)') p_total
  ELSE IF(decision == 2) THEN
    Read(*,'(/,F8.3)') y_gas(1)
    y_gas(2) = 1. - y_gas(1)
  ELSE
    Write(*,*) 'error in decision variable',decision
    Write(*,*) ' should be 1 (constant pressure) or 2 (const. mole fraction)'
    STOP
  END IF
END IF

Call fluid_properties
   
Open(40, file = file_press, Status = 'OLD')
Open(60, file = file_fuga)

Allocate(name(no_species))
Read(40,*) name
Read(40,*) no_points

Allocate(press(no_points), fuga(no_points), fuga_bin(no_species,no_points))
Allocate(char_fuga(no_points))
Allocate(y_array(no_points), p_array(no_points))
IF (no_species == 1) THEN
  Read(40,*) press
ELSE
  IF(decision == 1) THEN
    Read(40,*) y_array
    press = p_total
  ELSE
    Read(40,*) p_array
    press = p_array
  END IF
END IF

Write(*,*) 
Write(*,*) '=====================================================' 
Write(*,*) ' Calculating the fugacity with the Peng Robinson EOS '
Write(*,*) '====================================================='
Write(*,*) 
Write(*,*) 'Pressure input file: ',Trim(file_press)
Write(*,*)
IF(no_species == 2) THEN
   IF (decision == 1) THEN
      Write(*,'(A,F10.3,A)') 'Binary mixture with constant pressure of ',p_total, ' kPa'
   ELSE
      Write(*,'(A,F5.2,A)') 'Binary mixture with constant mole fraction of in the bulk phase',y_gas(1)
   END IF
END IF
Write(*,*)

press = press * 1000.

Write(*,*) ' i   y(1)    fugacity/kPa  pressure/kPa'
Do j = 1, no_species
 Write(*,*) name_spec(j)
 Do i = 1, no_points
    IF(no_species == 2) THEN
      If(decision == 1) THEN
         y_gas(1) = y_array(i)
         y_gas(2) = 1 - y_gas(1)
       END IF
    END IF
    Call calc_fugacity_1(i, j)
    Write(*,'(I3,2X,G9.3,2(G13.6))') i, y_gas(1), fugacity(j)/1.E3, press(i)/1.E3
    IF(no_species == 1) THEN
      fuga(i) = fugacity(j)
    ELSE
      fuga_bin(j,i) = fugacity(j)
    END IF
 End Do
END DO

fmt = '(G10.4)'
comma = ', '

IF(no_species == 1) THEN
  Write(60,*) name(1)
  Write(60,*) no_points 
  Do i =1 , no_points-1
    Write(string,fmt) fuga(i)/1000.
    char_fuga(i) = TRIM(string)//comma
  END DO

  Write(string,fmt) fuga(no_points)/1000.
  char_fuga(i) = string

  string = char_fuga(2)
 
  Write(60,'(100(A))') (char_fuga(i), i = 1, no_points)
ELSE
 Do j = 1, no_species
  Write(60,*) name(j)
  Write(60,*) no_points 
  Do i =1 , no_points-1
    Write(string,fmt) fuga_bin(j,i)/1000.
    char_fuga(i) = TRIM(string)//comma
  END DO

  Write(string,fmt) fuga_bin(j,no_points)/1000.
  char_fuga(i) = string

  string = char_fuga(2)
 
  Write(60,'(100(A))') (char_fuga(i), i = 1, no_points)
 END DO
END IF
END Program



