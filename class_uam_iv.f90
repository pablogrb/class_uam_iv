!	------------------------------------------------------------------------------------------
!	Point Source File Module
!	------------------------------------------------------------------------------------------

MODULE class_ptfile
IMPLICIT NONE

	PRIVATE

! 	Public variables
	PUBLIC :: UAM_IV

!	UAM-IV											! Input files
	TYPE :: UAM_IV

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

		CHARACTER(LEN=4), ALLOCATABLE :: spname(:,:)	! Species name array
		CHARACTER(LEN=10), ALLOCATABLE :: c_spname(:)	! Species name array (in single char)
		CHARACTER(LEN=10) :: ftype					! File type
		LOGICAL lhdrspec, l3d, lbndry, lptsrc		! Filetype logicals
! 		l3d may not be necessary

	END TYPE UAM_IV

! 	Public methods
	PUBLIC :: open_file, read_header, read_species

CONTAINS

	SUBROUTINE open_file(pt)

		TYPE(UAM_IV), INTENT(IN) :: pt

		OPEN (pt%unit,FILE=TRIM(pt%in_file),FORM='UNFORMATTED',STATUS='old')
		WRITE(*,*) 'Opened file: ',TRIM(pt%in_file)

	END SUBROUTINE open_file

	SUBROUTINE read_header(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt
		INTEGER :: i
		CHARACTER(LEN=41) :: h1format, h2format

		h1format='(10a1,60a1,/,i2,1x,i3,1x,i6,f6.0,i6,f6.0)'
		h2format='(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)'

		READ (pt%unit) pt%fname,pt%note,pt%nseg,pt%nspec,pt%idate,pt%begtim,pt%jdate,&
			&pt%endtim
		WRITE(*,h1format) pt%fname,pt%note,pt%nseg,pt%nspec,pt%idate,pt%begtim,pt%jdate,&
			&pt%endtim
		WRITE(pt%ftype,'(10a1)') (pt%fname(i),i=1,10)
		WRITE(*,*) 'File type is ',pt%ftype

		READ (pt%unit) pt%orgx,pt%orgy,pt%iutm,pt%utmx,pt%utmy,pt%dx,pt%dy,pt%nx,pt%ny,pt%nz,&
			&pt%nzlo,pt%nzup,pt%hts,pt%htl,pt%htu
		WRITE(*,h2format) pt%orgx,pt%orgy,pt%iutm,pt%utmx,pt%utmy,pt%dx,pt%dy,pt%nx,pt%ny,pt%nz,&
			&pt%nzlo,pt%nzup,pt%hts,pt%htl,pt%htu
		READ (pt%unit) pt%i1,pt%j1,pt%nx1,pt%ny1

	END SUBROUTINE read_header

	SUBROUTINE read_species(pt)

		TYPE(UAM_IV), INTENT(INOUT) :: pt
		INTEGER :: i,j

		ALLOCATE(pt%spname(10,pt%nspec))
		ALLOCATE(pt%c_spname(pt%nspec))
		READ (pt%unit) ((pt%spname(i,j),i=1,10),j=1,pt%nspec)
		WRITE(pt%c_spname,'(10a1)') ((pt%spname(i,j),i=1,10),j=1,pt%nspec)
		WRITE(*,*) pt%c_spname
! 		WRITE(*,'(10a1)') ((pt%spname(i,j),i=1,10),j=1,pt%nspec)

	END SUBROUTINE read_species

END MODULE
