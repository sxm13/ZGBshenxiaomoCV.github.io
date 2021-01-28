MODULE Molecules_Prop
!
! Mol_No_Max		integer	maximum number of molecules (size of array)
! Mol_Ini		integer initial number of molecules
! Mol_No		integer actual number of molecules
! no_species		integer	number of species (molecule)
! no_part_spec		integer	number of species of kind i in pore
! Mol_X		                cartesian coordinates of molecule i
! Mol_Y(i,j) 		real    j = 0: position of center of mass
! Mol_Z				j /= 0: position of LJ center j 
! phi(i), psi(i)        real	Euler angles of particle i
! cos_theta(i)
! unit_vector(3,LJC)    real    coordinates of unit vector of particle with
!				more than one LJ centre. The orientation
!				does not matter. 1: x, 2:y, 3:z
! kind_of_species(i)	integer array that stores identity of molecule i
! name_spec		character charater name of species (molecule)
! Sigma			real	Matrix of sigma ij
! Eps			real 	Lennard Jones Parameter of atom i and j
! Sigma_kind    	real    Lennard Jones Parameter for different kind of atoms
! eps_over_kb   	real    Lennard Jones Parameter for different kind of atoms
! y_gas			real	mole fraction of species (molecule) i
! Min_Sep_FF		real    minimum separation distance between two molecules
! R_cut			real    cutoff radius
! sqr_r_cut     	real    r_cut * r_cut
! Max_move		real	maximum displacement of particle
! dphi_max, dpsi_max    real    maximum change in Euler angle
! dcos_theta_max
! LJ_centers		integer number of Lennard Jones centers of the molecule species
! max_LJ_centers	integer maximal number of LJ centers of a molecule in the
!				mixture
! mass_of_LJ_C		real    mass of each LJ center of each species
! mass_of_species	real	mass of species 1 or 2
!
 USE diverses
 IMPLICIT NONE
 SAVE
!
 INTEGER                                            :: no_species
 REAL(kind = realkind), DIMENSION(2)                :: y_gas
 Real(kind = realkind), Allocatable, Dimension(:)   :: mass_of_species
 CHARACTER(10), ALLOCATABLE, DIMENSION(:)           :: name_spec
!
END MODULE Molecules_Prop   
