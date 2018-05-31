!	------------------------------------------------------------------------------------------
!	UAM-IV Utilities
!	------------------------------------------------------------------------------------------

MODULE utils_UAM_IV

USE class_UAM_IV

IMPLICIT NONE

! Public methods
PUBLIC :: clone_header

CONTAINS

!	------------------------------------------------------------------------------------------
!	clone_header
!		Copies the header of an UAM-IV type object into another
!	------------------------------------------------------------------------------------------

SUBROUTINE clone_header(fl_inp, fl_out)

	! UAM-IV objects for IO
	TYPE(UAM_IV), INTENT(IN) :: fl_inp
	TYPE(UAM_IV), INTENT(INOUT) :: fl_out

	! ------------------------------------------------------------------------------------------
	! Entry Point

	! Build the output file header using the header of the first file and the user dates
	fl_out%ftype  = fl_inp%ftype
	fl_out%update_times = fl_inp%update_times
	! Header 1
	fl_out%fname  = fl_inp%fname
	fl_out%note   = fl_inp%note
	fl_out%nseg   = fl_inp%nseg
	fl_out%nspec  = fl_inp%nspec
	fl_out%idate  = fl_inp%idate
	fl_out%begtim = fl_inp%begtim
	fl_out%jdate  = fl_inp%jdate
	fl_out%endtim = fl_inp%endtim
	! Header 2
	fl_out%orgx = fl_inp%orgx
	fl_out%orgy = fl_inp%orgy
	fl_out%iutm = fl_inp%iutm
	fl_out%utmx = fl_inp%utmx
	fl_out%utmy = fl_inp%utmy
	fl_out%dx   = fl_inp%dx
	fl_out%dy   = fl_inp%dy
	fl_out%nx   = fl_inp%nx
	fl_out%ny   = fl_inp%ny
	fl_out%nz   = fl_inp%nz
	fl_out%nzlo = fl_inp%nzlo
	fl_out%nzup = fl_inp%nzup
	fl_out%hts  = fl_inp%hts
	fl_out%htl  = fl_inp%htl
	fl_out%htu  = fl_inp%htu
	! Header 3
	fl_out%i1  = fl_inp%i1
	fl_out%j1  = fl_inp%j1
	fl_out%nx1 = fl_inp%nx1
	fl_out%ny1 = fl_inp%ny1

END SUBROUTINE

!	------------------------------------------------------------------------------------------
!	lintrans
!		Applies a linear transformation to the species of a UAM-IV type object
!	------------------------------------------------------------------------------------------

SUBROUTINE lintrans(fl_inp, mat_file)

	! UAM-IV object
	TYPE(UAM_IV), INTENT(INOUT) :: fl_inp			! Input UAM-IV object
	TYPE(UAM_IV) :: fl_out							! Dummy intermediate, cloned to fl_inp for output
	
	! Conversion matrix
	CHARACTER(LEN=265) :: mat_file					! Matrix file name
	INTEGER :: mat_unit								! Matrix file unit
	INTEGER :: n_out_spec							! Number of output species
	CHARACTER(LEN=10), ALLOCATABLE :: s_inp_spec(:)	! Species array of the input UAM_IV file
	CHARACTER(LEN=10), ALLOCATABLE :: s_out_spec(:)	! Species array of the output
	REAL, ALLOCATABLE :: conv_matrix(:,:)			! Linear transformation matrix for species

	! Control
	LOGICAL :: file_exists

	! Counters
	INTEGER :: i_sp_o, i_sp_i
	INTEGER :: i
	INTEGER :: i_nx, i_ny, i_hr
	INTEGER :: i_stk

	! ------------------------------------------------------------------------------------------
	! Entry Point

	! Check the inputs
	! Test for species list allocation, don't work with an empty object
	IF (.NOT. ALLOCATED(fl_inp%c_spname)) THEN
		WRITE(0,*) 'The species list was not allocated'
		CALL EXIT(1)
	END IF
	! Check if the matrix file exists
	INQUIRE(FILE=TRIM(mat_file), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,*) 'Matrix file ', TRIM(mat_file), ' does not exist'
		CALL EXIT(1)
	END IF

	! Read the species matrix file
	OPEN(NEWUNIT=mat_unit,FILE=TRIM(mat_file),STATUS='OLD')
	! Read the number of out species.
	READ (mat_unit,*) n_out_spec
	WRITE(*,*) 'Number of output species:', n_out_spec
	REWIND mat_unit

	! Allocate the vectors and arrays
	ALLOCATE(s_inp_spec(fl_inp%nspec))
	ALLOCATE(s_out_spec(n_out_spec))
	ALLOCATE(conv_matrix(fl_inp%nspec,n_out_spec))
	! Read the out species list
	READ (mat_unit,*) n_out_spec, (s_out_spec(i_sp_o), i_sp_o=1,n_out_spec)
	! Read the matrix
	DO i_sp_i = 1, fl_inp%nspec
		READ (mat_unit,*) s_inp_spec(i_sp_i), (conv_matrix(i_sp_i,i_sp_o), i_sp_o=1,n_out_spec)
	END DO

	! Build the output file
	! Clone the objects
	fl_out = fl_inp

	! Build the species list
	DEALLOCATE(fl_out%spname, fl_out%c_spname)
	ALLOCATE(fl_out%spname(10,n_out_spec))
	ALLOCATE(fl_out%c_spname(n_out_spec))
	DO i_sp_o = 1, n_out_spec
		DO i = 1,10
			fl_out%spname(i,i_sp_o) = s_out_spec(i_sp_o)(i:i)
		END DO
	END DO
	fl_out%c_spname = s_out_spec
	fl_out%nspec = n_out_spec

	! ------------------------------------------------------------------------------------------
	! Calculate the new emissions
	! ------------------------------------------------------------------------------------------
	SELECT CASE (fl_inp%ftype)

	! ------------------------------------------------------------------------------------------
	! 2D Emissions AKA Area emissions files
	CASE ('EMISSIONS ')
		WRITE(*,*) 'Converting the species'
		WRITE(*,'(I3,A)') 0, "% done"
!		Start parallel section
!$OMP 	PARALLEL SHARED(fl_inp, fl_out)

		!Loop through the rows, columns and hours
!$OMP 	DO SCHEDULE(DYNAMIC)
		DO i_nx = 1,fl_out%nx
			DO i_ny = 1,fl_out%ny
				DO i_hr = 1,fl_out%update_times
					fl_out%aemis(i_nx, i_ny, i_hr,:) = &
						& MATMUL(fl_inp%aemis(i_nx, i_ny, i_hr,:),conv_matrix)
				END DO
			END DO
		END DO

!$OMP 	END DO NOWAIT
!		End of the parallel section
!$OMP 	END PARALLEL

	! ------------------------------------------------------------------------------------------
	! Point source emissions
	CASE ('PTSOURCE  ')
		WRITE(*,*) 'Converting the species'
		WRITE(*,'(I3,A)') 0, "% done"
!		Start parallel section
!$OMP 	PARALLEL SHARED(fl_inp, fl_out)

		! Loop through the hours and stacks
		! This parallelization scheme only scales up to fl_out%update_times threads
		! This value is normally 24
!$OMP 	DO SCHEDULE(DYNAMIC)
		DO i_hr = 1,fl_out%update_times
			DO i_stk = 1, fl_out%nstk
				fl_out%ptemis(i_hr,i_stk,:) = &
					& MATMUL(fl_inp%ptemis(i_hr,i_stk,:),conv_matrix)
			END DO
		END DO

!$OMP 	END DO NOWAIT
!		End of the parallel section
!$OMP 	END PARALLEL

	END SELECT

	! Update the original object for return
	! Clone the objects
	fl_inp = fl_out

END SUBROUTINE lintrans

END MODULE utils_UAM_IV
