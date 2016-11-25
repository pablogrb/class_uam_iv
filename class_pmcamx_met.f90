!	------------------------------------------------------------------------------------------
!	PMCAMx Meteorological File Module
!	------------------------------------------------------------------------------------------
!
!	Developed by Pablo Garcia
!	pablogar@andrew.cmu.edu / bitbucket.org/pablogrb
!
!	This module uses F03 and F08 features
!

MODULE class_PMCAMx_MET
IMPLICIT NONE

!	Public variables
	PUBLIC :: PMCAMx_ZP

	TYPE :: PMCAMx_ZP
! 		File
		CHARACTER(LEN=256) :: in_file				! Input filename
		INTEGER :: unit								! Input unit
! 		Grid
		INTEGER :: nx, ny, nz						! Number of cells in EW, NS and z directions
! 		Timing
		INTEGER :: update_times = 24				! Number of records (frames) per file
! 		Record control
		REAL :: hour								! Hour of the z/p record
		INTEGER :: idate							! Date of the z/p record
! 		Height and pressure
		REAL, ALLOCATABLE :: height(:,:,:,:)		! Layer interface height array
													! (col, row, layer, hour)
		REAL, ALLOCATABLE :: press(:,:,:,:)			! Layer pressure array
													! (col, row, layer, hour)
													! Assumes 24 hours, change as necessary
	END TYPE PMCAMx_ZP

!	Public methods

!	Private methods

CONTAINS

!	------------------------------------------------------------------------------------------
!	Height / Pressure
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_zp(fl,in_file,unit,nx,ny,nz)

		TYPE(PMCAMx_ZP), INTENT(INOUT) :: fl

		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, OPTIONAL :: unit
		INTEGER, OPTIONAL :: nx, ny, nz

! 		Check for optionals
		IF PRESENT(in_file) THEN
			fl%in_file = in_file
		END IF
		IF PRESENT(unit) THEN
			fl%unit = unit
		END IF
		IF (PRESENT(nx) .AND. PRESENT(ny) .AND. PRESENT(nz)) THEN
			fl%nx = nx
			fl%ny = ny
			fl%nz = nz
		ELSE IF (PRESENT(nx) .OR. PRESENT(ny) .OR. PRESENT(nz)) THEN
			WRITE(*,0) 'class_PMCAMx_MET error: Either all or none of nx, ny, nz must be present'
			CALL EXIT(0)
		END IF

	END SUBROUTINE read_zp

!	------------------------------------------------------------------------------------------
!	File Opening
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_open_file(fl)

		CLASS(*), INTENT(INOUT) :: fl

		SELECT TYPE (fl)
		CLASS IS (PMCAMx_ZP)
			WRITE(*,*) 'File type is Height /  Pressure'
		CLASS DEFAULT
			WRITE(*,0) 'class_PMCAMx_MET error: Not a valid file type'
		END SELECT

! 		Open the file
		OPEN(NEWUNIT=fl%unit,FILE=TRIM(fl%in_file),FORM='UNFORMATTED',STATUS='OLD')
		WRITE(*,*) 'Opened file: ', TRIM(fl%in_file)

	END SUBROUTINE read_open_file

	SUBROUTINE close_file(fl)

		TYPE(PMCAMx_MET), INTENT(IN) :: fl

! 		Close the unit
		CLOSE(fl%met_par%unit)
		WRITE(*,*) 'Closed file: ', TRIM(fl%met_par%in_file)

	END SUBROUTINE close_file

!	------------------------------------------------------------------------------------------
!	Height / pressure
!	------------------------------------------------------------------------------------------


END MODULE
