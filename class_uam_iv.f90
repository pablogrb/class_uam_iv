!	------------------------------------------------------------------------------------------
!	Point Source File Module
!	------------------------------------------------------------------------------------------

MODULE class_ptfile
IMPLICIT NONE

! 	Public variables
	PUBLIC :: UAM_IV

!	UAM-IV Derived Type Structure					! Input files
	TYPE :: UAM_IV

! 		Header
		CHARACTER(LEN=256) :: in_file				! Input filename
		INTEGER :: unit								! Input unit
		CHARACTER(LEN=4), DIMENSION(10) :: fname	! Name array
		CHARACTER(LEN=4), DIMENSION(60) :: note		! Note array
		INTEGER :: nseg,nspec,idate,jdate			! Species number and dates
		REAL :: begtim, endtim						! Hours
		INTEGER :: iutm,nx,ny						! UTM zone, horizontal grid
		INTEGER :: nz,nzlo,nzup						! Vertical grid
		INTEGER :: hts,htl,htu						! Others ???
		INTEGER :: i1,j1,nx1,ny1					! Others ???
		REAL :: orgx,orgy,utmx,utmy,dx,dy			! Grid origin and spacing

! 		Species
		CHARACTER(LEN=4), ALLOCATABLE :: spname(:,:)	! Species name array
		CHARACTER(LEN=10), ALLOCATABLE :: c_spname(:)	! Species name array (in single char)
		CHARACTER(LEN=10) :: ftype					! File type
		LOGICAL lhdrspec, l3d, lbndry, lptsrc		! Filetype logicals
! 		l3d may not be necessary

! 		Stack Parameters
		INTEGER :: nstk								! Number of stacks
		REAL, ALLOCATABLE :: xstk(:), ystk(:)		! Stack location
		REAL, ALLOCATABLE :: hstk(:), dstk(:)		! Stack height and diameter
		REAL, ALLOCATABLE :: tstk(:), vstk(:)		! Stack temperature and velocity

! 		Stack Emissions
		INTEGER :: update_times = 24				! Number of emissions data points per file
		INTEGER, ALLOCATABLE :: ibgdat(:), iendat(:)! Beggining and end date of emission records
		REAL, ALLOCATABLE :: nbgtim(:), nentim(:)	! Beggining and end time of emission records
		INTEGER, ALLOCATABLE :: icell(:),jcell(:)	! idum in the CAMx manual
		INTEGER, ALLOCATABLE :: kcell(:)			! Ignored, except as flag for OSAT
		REAL, ALLOCATABLE :: flow(:)				! Stack flow rate (m3 /hr)
		REAL, ALLOCATABLE :: plmht(:)				! Effective plume height override (m)
		REAL, ALLOCATABLE :: ptemis(:,:,:)			! Species point emission rate (mol/time 
													! period for gases, g/time period for
													! aerosols) update_times x nstk x nspec

	END TYPE UAM_IV

! 	Public methods
	PUBLIC :: read_ptfile

! 	Private methods
	PRIVATE :: open_file, read_header, read_species, read_stack_param, read_stack_emis

CONTAINS

	SUBROUTINE read_ptfile(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt

! 		Open the file
		CALL open_file(pt)
! 		Read the header
		CALL read_header(pt)
! 		Read the species names
		CALL read_species(pt)
! 		Read the stack parameters
		CALL read_stack_param(pt)
! 		Read the emission records
		CALL read_stack_emis(pt)

	END SUBROUTINE read_ptfile

	SUBROUTINE open_file(pt)

		TYPE(UAM_IV), INTENT(IN) :: pt

! 		Open the files
		OPEN (pt%unit,FILE=TRIM(pt%in_file),FORM='UNFORMATTED',STATUS='old')
		WRITE(*,*) 'Opened file: ',TRIM(pt%in_file)

	END SUBROUTINE open_file

	SUBROUTINE read_header(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt
		INTEGER :: i
		CHARACTER(LEN=41) :: h1format, h2format

! 		Set the format strings
		h1format='(10a1,60a1,/,i2,1x,i3,1x,i6,f6.0,i6,f6.0)'
		h2format='(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)'

! 		Read the first header
		READ (pt%unit) pt%fname,pt%note,pt%nseg,pt%nspec,pt%idate,pt%begtim,pt%jdate,&
			&pt%endtim
		WRITE(*,h1format) pt%fname,pt%note,pt%nseg,pt%nspec,pt%idate,pt%begtim,pt%jdate,&
			&pt%endtim
		WRITE(pt%ftype,'(10a1)') (pt%fname(i),i=1,10)
		WRITE(*,*) 'File type is ',pt%ftype

! 		Read the second header
		READ (pt%unit) pt%orgx,pt%orgy,pt%iutm,pt%utmx,pt%utmy,pt%dx,pt%dy,pt%nx,pt%ny,pt%nz,&
			&pt%nzlo,pt%nzup,pt%hts,pt%htl,pt%htu
		WRITE(*,h2format) pt%orgx,pt%orgy,pt%iutm,pt%utmx,pt%utmy,pt%dx,pt%dy,pt%nx,pt%ny,pt%nz,&
			&pt%nzlo,pt%nzup,pt%hts,pt%htl,pt%htu
		READ (pt%unit) pt%i1,pt%j1,pt%nx1,pt%ny1

	END SUBROUTINE read_header

	SUBROUTINE read_species(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt
		INTEGER :: i,j

! 		Allocate memory for the species arrays
		ALLOCATE(pt%spname(10,pt%nspec))
		ALLOCATE(pt%c_spname(pt%nspec))

! 		Read the species records
		READ (pt%unit) ((pt%spname(i,j),i=1,10),j=1,pt%nspec)
		WRITE(pt%c_spname,'(10a1)') ((pt%spname(i,j),i=1,10),j=1,pt%nspec)
		WRITE(*,*) pt%c_spname
! 		WRITE(*,'(10a1)') ((pt%spname(i,j),i=1,10),j=1,pt%nspec)

	END SUBROUTINE read_species

	SUBROUTINE read_stack_param(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt
		INTEGER :: i_stk
		INTEGER :: ione
! 		CHARACTER(LEN=20) :: stkformat

! 		Set the format strings
! 		stkformat = '(2(f16.5,1x),4e14.7)'

! 		Read the number of stacks
		READ (pt%unit) ione,pt%nstk
		WRITE (*,*) ione, pt%nstk, TRIM(pt%in_file)

! 		Allocate the stack parameter arrays
		ALLOCATE(pt%xstk(pt%nstk), pt%ystk(pt%nstk))
		ALLOCATE(pt%hstk(pt%nstk), pt%dstk(pt%nstk))
		ALLOCATE(pt%tstk(pt%nstk), pt%vstk(pt%nstk))

! 		Read the stack parameter records
		READ (pt%unit) (pt%xstk(i_stk),pt%ystk(i_stk),pt%hstk(i_stk),pt%dstk(i_stk),&
			&pt%tstk(i_stk),pt%vstk(i_stk),i_stk=1,pt%nstk)

! 		DO i_stk = 1,pt%nstk
! 			WRITE(*,stkformat) pt%xstk(i_stk),pt%ystk(i_stk),pt%hstk(i_stk),&
! 				&pt%dstk(i_stk),pt%tstk(i_stk),pt%vstk(i_stk)
! 		END DO

	END SUBROUTINE read_stack_param

	SUBROUTINE read_stack_emis(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt

		INTEGER :: i_hr, i_stk, i_sp, ione
		CHARACTER(LEN=4) :: temp_spname(10)
		INTEGER :: j
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

		ALLOCATE(pt%ibgdat(pt%update_times), pt%iendat(pt%update_times))
		ALLOCATE(pt%nbgtim(pt%update_times), pt%nentim(pt%update_times))
		ALLOCATE(pt%icell(pt%nstk),pt%jcell(pt%nstk),pt%kcell(pt%nstk))
		ALLOCATE(pt%flow(pt%nstk),pt%plmht(pt%nstk))

		ALLOCATE(pt%ptemis(pt%update_times,pt%nstk,pt%nspec))

! 		Loop over hours
		DO i_hr = 1,pt%update_times	! Update times is default 24
! 			Read the section header
			READ (pt%unit) pt%ibgdat(i_hr), pt%nbgtim(i_hr), pt%iendat(i_hr), pt%nentim(i_hr)
! 			Output to screen
			WRITE(*,hformat) pt%ibgdat(i_hr), pt%nbgtim(i_hr),&
				&pt%iendat(i_hr), pt%nentim(i_hr)

! 			Read the stack number
			READ (pt%unit) ione, pt%nstk
! 			Read the point source descriptions
			READ (pt%unit) (pt%icell(i_stk),pt%jcell(i_stk),pt%kcell(i_stk),pt%flow(i_stk),&
				&pt%plmht(i_stk),i_stk=1,pt%nstk)

! 			Loop though species
			DO i_sp = 1, pt%nspec
				READ (pt%unit) ione, (temp_spname(j),j=1,10), (pt%ptemis(i_hr,i_stk,i_sp),&
					&i_stk=1,pt%nstk)
			END DO
		END DO

	END SUBROUTINE read_stack_emis

END MODULE
