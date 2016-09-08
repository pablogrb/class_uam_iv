!	------------------------------------------------------------------------------------------
!	Point Source File Module
!	------------------------------------------------------------------------------------------

MODULE class_ptfile
IMPLICIT NONE

	PRIVATE
	PUBLIC UAM_IV

!	UAM-IV											! Input files
	TYPE :: UAM_IV
		CHARACTER(LEN=256) :: in_file				! Input filename
		INTEGER :: file_unit						! Input unit
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

CONTAINS

END MODULE
