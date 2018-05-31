!	------------------------------------------------------------------------------------------
!	UAM-IV Utilities
!	------------------------------------------------------------------------------------------

MODULE utils_UAM_IV

USE class_UAM_IV

IMPLICIT NONE

! Public methods
PUBLIC :: clone_header
PUBLIC :: clone_species
PUBLIC :: lintrans
PUBLIC :: concatenate
PUBLIC :: average
PUBLIC :: totalize

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
!	clone_species
!		Copies the header of an UAM-IV type object into another
!	------------------------------------------------------------------------------------------
SUBROUTINE clone_species(fl_inp, fl_out)

	! UAM-IV objects for IO
	TYPE(UAM_IV), INTENT(IN) :: fl_inp
	TYPE(UAM_IV), INTENT(INOUT) :: fl_out

	! ------------------------------------------------------------------------------------------
	! Entry Point

	! Check the inputs
	! Test for species list allocation, don't work with an empty object
	IF (.NOT. ALLOCATED(fl_inp%c_spname)) THEN
		WRITE(0,*) 'The species list was not allocated'
		CALL EXIT(1)
	END IF

	! Clone the species
	ALLOCATE(fl_out%spname(10,fl_inp%nspec))
	ALLOCATE(fl_out%c_spname(fl_inp%nspec))
	fl_out%spname = fl_inp%spname
	fl_out%c_spname = fl_inp%c_spname

END SUBROUTINE clone_species

!	------------------------------------------------------------------------------------------
!	lintrans
!		Applies a linear transformation to the species of a UAM-IV type object
!	------------------------------------------------------------------------------------------
SUBROUTINE lintrans(fl_inp, fl_out, mat_file)

	! UAM-IV object
	TYPE(UAM_IV), INTENT(IN) :: fl_inp			! Input UAM-IV object
	TYPE(UAM_IV), INTENT(OUT) :: fl_out			! Dummy intermediate, cloned to fl_inp for output

	! Conversion matrix
	CHARACTER(LEN=265), INTENT(IN) :: mat_file		! Matrix file name
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


	! ------------------------------------------------------------------------------------------
	! Unsupported ftypes
	CASE DEFAULT
		WRITE(0,*) 'File type ', TRIM(fl_inp%ftype), ' is not supported'
		CALL EXIT(1)

	END SELECT

END SUBROUTINE lintrans

!	------------------------------------------------------------------------------------------
!	concatenate
!		Takes a set of UAM-IV objects and concatenates them in time into a new object
!	------------------------------------------------------------------------------------------
SUBROUTINE concatenate(fl_inp, fl_out)

	! UAM-IV object
	TYPE(UAM_IV), ALLOCATABLE, INTENT(IN) :: fl_inp(:)		! Input UAM-IV object
	TYPE(UAM_IV), INTENT(OUT) :: fl_out			! Dummy intermediate, cloned to fl_inp for output

	! Output frames
	INTEGER :: n_frames							! Number of output frames

	! Counters
	INTEGER :: i_fl
	INTEGER :: i_sp

	! ------------------------------------------------------------------------------------------
	! Entry Point

	! Check the input file array
	IF (SIZE(SHAPE(fl_inp)) > 1) THEN
		WRITE(0,*) 'The input UAM-IV object array must be a vector'
		CALL EXIT(1)
	END IF

	! Check the files
	DO i_fl = 1, SIZE(fl_inp)
		! Test for species list allocation, don't work with an empty object
		IF (.NOT. ALLOCATED(fl_inp(i_fl)%c_spname)) THEN
			WRITE(0,*) 'The species list was not allocated'
			CALL EXIT(2)
		END IF
		! Check for ftype
		IF (fl_inp(1)%ftype .NE. fl_inp(i_fl)%ftype) THEN
			WRITE(0,*) 'All UAM-IV objects must be of the same type'
		END IF
	END DO

	! Get the total number of frames
	n_frames = 0
	DO i_fl = 1, SIZE(fl_inp)
		n_frames = n_frames + fl_inp(i_fl)%update_times
	END DO

	! Clone the header
	CALL clone_header(fl_inp(1), fl_out)
	fl_out%update_times = n_frames
	! Clone the species
	CALL clone_species(fl_inp(1), fl_out)

	! Allocate the time headers
	ALLOCATE(fl_out%ibgdat(n_frames),fl_out%iendat(n_frames))
	ALLOCATE(fl_out%nbgtim(n_frames),fl_out%nentim(n_frames))

	! ------------------------------------------------------------------------------------------
	! Do files by ftype
	SELECT CASE (fl_inp(1)%ftype)
	CASE ('EMISSIONS ')
		! Allocate the emissions array
		ALLOCATE(fl_out%aemis(fl_out%nx,fl_out%ny,n_frames,fl_out%nspec))
		fl_out%aemis = 0
		! Work though the files
		DO i_fl = 1, SIZE(fl_inp)
			WRITE(*,'(A,I2,A,I2,A)') 'Working on ', i_fl, ' of ', SIZE(fl_inp),' files'
			! Get the time variant headers
			fl_out%ibgdat(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%ibgdat
			fl_out%iendat(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%iendat
			fl_out%nbgtim(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%nbgtim
			fl_out%nentim(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%nentim
			! Get the emissions
			!            x y frames                species
			! fl_out%aemis(:,:,24*(i_fl-1)+1:24*i_fl,:) = fl_inp(i_fl)%aemis
			! This segfaults mysteriously on some files
			DO i_sp = 1, fl_inp(i_fl)%nspec
				fl_out%aemis(:,:,24*(i_fl-1)+1:24*i_fl,i_sp) = fl_inp(i_fl)%aemis(:,:,:,i_sp)
			END DO
		END DO
	CASE ('PTSOURCE ' )
		! Get the number of stacks
		fl_out%nstk = fl_inp(1)%nstk
		! Allocate the stack parameter arrays
		ALLOCATE(fl_out%xstk(fl_out%nstk), fl_out%ystk(fl_out%nstk))
		ALLOCATE(fl_out%hstk(fl_out%nstk), fl_out%dstk(fl_out%nstk))
		ALLOCATE(fl_out%tstk(fl_out%nstk), fl_out%vstk(fl_out%nstk))
		! Clone the stack parameter arrays
		fl_out%xstk = fl_inp(1)%xstk
		fl_out%ystk = fl_inp(1)%ystk
		fl_out%hstk = fl_inp(1)%hstk
		fl_out%dstk = fl_inp(1)%dstk
		fl_out%tstk = fl_inp(1)%tstk
		fl_out%vstk = fl_inp(1)%vstk

		! Allocate the stack description arrays
		ALLOCATE(fl_out%icell(fl_out%update_times,fl_out%nstk))
		ALLOCATE(fl_out%jcell(fl_out%update_times,fl_out%nstk))
		ALLOCATE(fl_out%kcell(fl_out%update_times,fl_out%nstk))
		ALLOCATE(fl_out%flow (fl_out%update_times,fl_out%nstk))
		ALLOCATE(fl_out%plmht(fl_out%update_times,fl_out%nstk))
		! Allocate the emissions array
		ALLOCATE(fl_out%ptemis(fl_out%update_times,fl_out%nstk,fl_out%nspec))

		! Work thorugh the files
		DO i_fl = 1, SIZE(fl_inp)
			WRITE(*,'(A,I2,A,I2,A)') 'Working on ', i_fl, ' of ', SIZE(fl_inp),' files'
			! Get the time variant headers
			fl_out%ibgdat(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%ibgdat
			fl_out%iendat(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%iendat
			fl_out%nbgtim(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%nbgtim
			fl_out%nentim(24*(i_fl-1)+1:24*i_fl) = fl_inp(i_fl)%nentim
			! Get the stack description arrays
			fl_out%icell(24*(i_fl-1)+1:24*i_fl,:) = fl_inp(i_fl)%icell
			fl_out%jcell(24*(i_fl-1)+1:24*i_fl,:) = fl_inp(i_fl)%jcell
			fl_out%kcell(24*(i_fl-1)+1:24*i_fl,:) = fl_inp(i_fl)%kcell
			fl_out%flow (24*(i_fl-1)+1:24*i_fl,:) = fl_inp(i_fl)%flow
			fl_out%plmht(24*(i_fl-1)+1:24*i_fl,:) = fl_inp(i_fl)%plmht
			! Get the emissions
			DO i_sp = 1, fl_inp(i_fl)%nspec
				fl_out%ptemis(24*(i_fl-1)+1:24*i_fl,:,i_sp) = fl_inp(i_fl)%ptemis(:,:,i_sp)
			END DO
		END DO
	CASE DEFAULT
		WRITE(0,*) 'The UAM-IV type ', TRIM(fl_inp(1)%ftype), ' is not supported'
	END SELECT

	! Update the end dates
	fl_out%jdate  = fl_out%iendat(n_frames)
	fl_out%endtim = fl_out%nentim(n_frames)

END SUBROUTINE concatenate

!	------------------------------------------------------------------------------------------
!	average
!		Takes an UAM-IV object and averages it over time
!	------------------------------------------------------------------------------------------
SUBROUTINE average(fl_inp, fl_out)

	! UAM-IV objects
	TYPE(UAM_IV), INTENT(IN) :: fl_inp			! Input UAM-IV object
	TYPE(UAM_IV), INTENT(OUT) :: fl_out			! Dummy intermediate, cloned to fl_inp for output

	! ------------------------------------------------------------------------------------------
	! Entry Point
	CALL totalize(fl_inp, fl_out)

	! ------------------------------------------------------------------------------------------
	! Do files by ftype
	SELECT CASE (fl_inp%ftype)
	CASE ('EMISSIONS ')
		! Average the totals
		fl_out%aemis = fl_out%aemis/fl_inp%update_times
	CASE ('PTSOURCE  ')
		! Average the totals
		fl_out%ptemis = fl_out%ptemis/fl_inp%update_times
	CASE DEFAULT
		WRITE(0,*) 'The UAM-IV type ', TRIM(fl_inp%ftype), ' is not supported'
	END SELECT

END SUBROUTINE average

!	------------------------------------------------------------------------------------------
!	totalize
!		Takes an UAM-IV object and totalizes it over time
!	------------------------------------------------------------------------------------------
SUBROUTINE totalize(fl_inp, fl_out)

	! UAM-IV object
	TYPE(UAM_IV), INTENT(IN) :: fl_inp			! Input UAM-IV object
	TYPE(UAM_IV), INTENT(OUT) :: fl_out			! Dummy intermediate, cloned to fl_inp for output

	! ------------------------------------------------------------------------------------------
	! Entry Point

	! Test for species list allocation, don't work with an empty object
	IF (.NOT. ALLOCATED(fl_inp%c_spname)) THEN
		WRITE(0,*) 'The species list was not allocated'
		CALL EXIT(2)
	END IF

	! Clone the header
	CALL clone_header(fl_inp, fl_out)
	fl_out%update_times = 1
	! Clone the species
	CALL clone_species(fl_inp, fl_out)

	! Allocate the time headers
	ALLOCATE(fl_out%ibgdat(1),fl_out%iendat(1))
	ALLOCATE(fl_out%nbgtim(1),fl_out%nentim(1))

	! ------------------------------------------------------------------------------------------
	! Do files by ftype
	SELECT CASE (fl_inp%ftype)
	CASE ('EMISSIONS ')
		! Allocate the emissions array
		ALLOCATE(fl_out%aemis(fl_out%nx,fl_out%ny,1,fl_out%nspec))
		! Get the time variant headers
		fl_out%ibgdat(1) = fl_inp%ibgdat(1)
		fl_out%iendat(1) = fl_inp%iendat(fl_inp%update_times)
		fl_out%nbgtim(1) = fl_inp%nbgtim(1)
		fl_out%nentim(1) = fl_inp%nentim(fl_inp%update_times)
		! Get the emissions
		fl_out%aemis(:,:,1,:) = SUM(fl_inp%aemis,3)
	CASE ('PTSOURCE  ')
		! Get the time variant headers
		fl_out%ibgdat(1) = fl_inp%ibgdat(1)
		fl_out%iendat(1) = fl_inp%iendat(fl_inp%update_times)
		fl_out%nbgtim(1) = fl_inp%nbgtim(1)
		fl_out%nentim(1) = fl_inp%nentim(fl_inp%update_times)

		! Get the number of stacks
		fl_out%nstk = fl_inp%nstk
		! Allocate the stack parameter arrays
		ALLOCATE(fl_out%xstk(fl_out%nstk), fl_out%ystk(fl_out%nstk))
		ALLOCATE(fl_out%hstk(fl_out%nstk), fl_out%dstk(fl_out%nstk))
		ALLOCATE(fl_out%tstk(fl_out%nstk), fl_out%vstk(fl_out%nstk))
		! Clone the stack parameter arrays
		fl_out%xstk = fl_inp%xstk
		fl_out%ystk = fl_inp%ystk
		fl_out%hstk = fl_inp%hstk
		fl_out%dstk = fl_inp%dstk
		fl_out%tstk = fl_inp%tstk
		fl_out%vstk = fl_inp%vstk

		! Allocate the stack description arrays
		ALLOCATE(fl_out%icell(1,fl_out%nstk))
		ALLOCATE(fl_out%jcell(1,fl_out%nstk))
		ALLOCATE(fl_out%kcell(1,fl_out%nstk))
		ALLOCATE(fl_out%flow (1,fl_out%nstk))
		ALLOCATE(fl_out%plmht(1,fl_out%nstk))
		! Calculate the average stack description
		fl_out%icell(1,:) = SUM(fl_inp%icell,1)/fl_inp%update_times
		fl_out%jcell(1,:) = SUM(fl_inp%jcell,1)/fl_inp%update_times
		fl_out%kcell(1,:) = SUM(fl_inp%kcell,1)/fl_inp%update_times
		fl_out%flow (1,:) = SUM(fl_inp%flow ,1)/fl_inp%update_times
		fl_out%plmht(1,:) = SUM(fl_inp%plmht,1)/fl_inp%update_times

		! Allocate the emissions array
		ALLOCATE(fl_out%ptemis(fl_out%update_times,fl_out%nstk,fl_out%nspec))
		! Get the emissions
		fl_out%ptemis(1,:,:) = SUM(fl_out%ptemis,1)
	CASE DEFAULT
		WRITE(0,*) 'The UAM-IV type ', TRIM(fl_inp%ftype), ' is not supported'
	END SELECT

END SUBROUTINE totalize

END MODULE utils_UAM_IV
