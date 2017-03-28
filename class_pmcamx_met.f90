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
		REAL, ALLOCATABLE :: hour(:,:,:)			! Hour of the z/p record (layer, frame, var)
		INTEGER, ALLOCATABLE :: idate(:,:,:)		! Date of the z/p record (layer, frame, var)
! 		Height and pressure
		REAL, ALLOCATABLE :: height(:,:,:,:)		! Layer interface height array
													! (col, row, layer, frame)
		REAL, ALLOCATABLE ::  press(:,:,:,:)		! Layer pressure array
													! (col, row, layer, frame)
													! Assumes 24 hours, change as necessary
	END TYPE PMCAMx_ZP

!	Public methods

	PUBLIC read_zp

!	Private methods
	PRIVATE read_open_file_wiu, read_open_file_wou, close_file

CONTAINS

!	------------------------------------------------------------------------------------------
!	Height / Pressure
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_zp(fl,in_file,nx,ny,nz,unit)

		TYPE(PMCAMx_ZP), INTENT(INOUT) :: fl

		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, INTENT(IN), OPTIONAL :: unit
		INTEGER, INTENT(IN), OPTIONAL :: nx, ny, nz

		INTEGER :: i_nx, i_ny, i_nz, i_hr

! 		Check for optionals
		IF (PRESENT(in_file)) THEN
			fl%in_file = in_file
		END IF
		IF (PRESENT(nx) .AND. PRESENT(ny) .AND. PRESENT(nz)) THEN
			fl%nx = nx
			fl%ny = ny
			fl%nz = nz
		ELSE IF (PRESENT(nx) .OR. PRESENT(ny) .OR. PRESENT(nz)) THEN
			WRITE(0,*) 'class_PMCAMx_MET error: Either all or none of nx, ny, nz must be present'
			CALL EXIT(0)
		END IF
		IF (PRESENT(unit)) THEN
			fl%unit = unit
! 			Open the file providig the unit
			CALL read_open_file_wiu(fl%in_file,fl%unit)
		ELSE
! 			Open the file with automatic unit assignement
			CALL read_open_file_wou(fl%in_file,fl%unit)
		END IF

! 		Allocate memory
		ALLOCATE(fl%hour(fl%nz, fl%update_times+1, 2))
		ALLOCATE(fl%idate(fl%nz, fl%update_times+1, 2))
		ALLOCATE(fl%height(fl%nx,fl%ny,fl%nz,fl%update_times+1))
		ALLOCATE( fl%press(fl%nx,fl%ny,fl%nz,fl%update_times+1))

! 		Loop through frames
		DO i_hr = 1, fl%update_times+1
! 		Sanity output
		WRITE(*,*) 'Reading frame: ', i_hr-1
! 			Loop through layers
			DO i_nz = 1, fl%nz
! 				Sanity output
! 				WRITE(*,*) 'Reading layer', i_nz

! 				Read the heights
				READ(fl%unit) fl%hour(i_nz,i_hr,1), fl%idate(i_nz,i_hr,1), &
								& ((fl%height(i_nx,i_ny,i_nz,i_hr),i_nx=1,fl%nx),i_ny=1,fl%ny)
! 				Read the pressures
				READ(fl%unit) fl%hour(i_nz,i_hr,2), fl%idate(i_nz,i_hr,2), &
								& (( fl%press(i_nx,i_ny,i_nz,i_hr),i_nx=1,fl%nx),i_ny=1,fl%ny)
			END DO
		END DO


	END SUBROUTINE read_zp

!	------------------------------------------------------------------------------------------
!	File Opening
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_open_file_wiu(in_file,in_unit)

		CHARACTER(LEN=256), INTENT(IN) :: in_file
		INTEGER, INTENT(IN) :: in_unit

!		Open the unit
		OPEN(UNIT=in_unit,FILE=TRIM(in_file),FORM='UNFORMATTED',STATUS='OLD')
		WRITE(*,*) 'Opened file: ', TRIM(in_file)

	END SUBROUTINE read_open_file_wiu

	SUBROUTINE read_open_file_wou(in_file,in_unit)

		CHARACTER(LEN=256), INTENT(IN) :: in_file
		INTEGER, INTENT(OUT) :: in_unit

!		Open the unit
		OPEN(NEWUNIT=in_unit,FILE=TRIM(in_file),FORM='UNFORMATTED',STATUS='OLD')
		WRITE(*,*) 'Opened file: ', TRIM(in_file)

	END SUBROUTINE read_open_file_wou

	SUBROUTINE close_file(unit)

		INTEGER, INTENT(IN) :: unit

! 		Close the unit
		CLOSE(unit)
		WRITE(*,*) 'Closed file'

	END SUBROUTINE close_file

!	------------------------------------------------------------------------------------------
!	Height / pressure
!	------------------------------------------------------------------------------------------


END MODULE
