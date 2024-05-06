# README

The UAM\_IV file module defines a *class like* derived type that implements the UAM\_IV file format as defined in Ramboll Environ's CAMx. Provides an *object like* data structure for all types and *method like* subroutines for IO. It was inspired by Dr. Barron Henderson's [pseudonetcdf](https://github.com/barronh/pseudonetcdf) python CAMx compatibly layer and serves as a modern Fortran IO support module.

An additional utilites module contains a group of programs that facilitate the manipulation and conversion of UAM\_IV files.

This module was developed by [Pablo Garcia](pablogar@andrew.cmu.edu) at the Center for Atmospheric Particle Studies at Carnegie Mellon University.

## Example usage
```fortran
PROGRAM example
USE class_UAM_IV

IMPLICIT NONE
TYPE(UAM_IV) :: fl
CHARACTER(LEN=256) :: in_file
CHARACTER(LEN=256) :: out_file

    in_file = "emis.bin"
    CALL read_uamfile(fl, in_file)

    ! This multiplies all emissions by 2
    fl%aemis = fl%aemis * 2

    out_file = "emis2.bin"
    CALL write_uamfile(fl, out_file)

END PROGRAM example
```

## Data structure

The UAM\_IV derived type can be used to create variables of the `UAM_IV` type. It can contain the data from any UAM\_IV file type supported by CAMx in the following variables, here shown by groups. The variable names follow the convention from the published CAMx manuals where possible. More detail on their function and use is described there.

### File descriptors

#### Main Header
* `in_file`: Input file path. Supports up to 256 characters
* `unit`: Input file fortran unit. Can be automatically assigned or user set.
* `fname`: File type in the CAMx style character array
* `ftype`: File type
* `note`: File description note in CAMx style character array
* `nseg`: Unused / No documentation from CAMx
* `nspec`: Number of species
* `idate`, `jdate`: Start and end dates
* `begtim`, `endtim`: Start and end times
* `iutm`: Time zone
* `nx`, `ny`, `nz`: EW, NS, and up-down grid dimensions
* `nzlo`, `nzup`: Unused / No documentation from CAMx
* `hts`, `htl`, `htu`: Unused / No documentation from CAMx
* `i1`, `j1`, `nx1`, `ny1`: Unused / No documentation from CAMx
* `orgx`, `orgy`: Unused / No documentation from CAMx
* `utmx`, `utmy`: Grid origin of the SW corner of the domain in projection coordinates, `xorg` and `yorg` in the CAMx manual
* `dx`, `dy`: Cell dimensions in projected coordinates

The UAM_IV format, as implemented by CAMx, does not contain the number of data frames in the file header. This module assumes 24 data frames per file, adjust according to your application.

* `update_times`: Number of data frames in the file. Default value = 24

#### Species
* `spname`: Species name array in CAMx style character array
* `c_spname`: Species name array, size `nspec`

#### Time-variant frame headers
* `ibgdat`, `iendat`: Start and end date of the data frame, size `update_times`
* `nbgtim`, `ientim`: Start and end time of the data frame, size `update_times`

### Data types

#### AVERAGE and AIRQUALITY

* `conc`: 3D concentration array
	* Size: `nx`, `ny`, `nz`, `update_times`, `nspec`
	* Dimensions: EW cell (column), NW cell (row), layer, hour, species

#### EMISSIONS

* `aemis`: 2D area emissions array
	* Size: `nx`, `ny`, `update_times`, `nspec`
	* Dimensions: EW cell (column), NW cell (row), hour, species

#### PTSOURCE
Stack parameters

* `nstk`: Number of stacks
* `xstk`, `ystk`: Stack location in projection coordinates, size `nstk`
* `hstk`, `dstk`: Stack height and diameter, size `nstk`
* `tstk`, `vstk`: Stack temperature and velocity, size `nstk`

Stack emissions

* `icell`, `jcell`: Unused / No documentation from CAMx
* `kcell`: Unused except as flag for OSAT
* `flow`: Stack flow rate (m³/hr, size `update_times`, `nstk`
* `plmht`: Effective plume height override (m), size `update_times`, `nstk`
* `ptemis`: Species emission rate (mol/time period for gases, g/time period for particles), size size `update_times`, `nstk`, `nspec`

#### BOUNDARY
The `BOUNDARY` type uses the `UAM_BC_PAR` derived type, its properties are described here as a subset of the `bc` array.

* `bc`: Boundary condition derived type array, size 4
	* `iedge`: Edge number (1 = West, 2 = East, 3 = South, 4 = North)
	* `ncell`: Number of cells
	* `iloc`: Index of first cell modeled (edges 1,3), or last cell modeled (edges 2,4): if=0, this row/column is omitted from the simulation. `icell` in the CAMx manual
	* `bc_conc`: Boundary concentration array (ppm for gases, μg/m3 for particles)
		* Size: `ncell`, `nz`, `update_times`, `nspec`
		* Dimensions: Cell, layer, hour, species

## Methods
### File IO
#### Subroutine: `read_uamfile`, method `read`
Takes a `UAM_IV` type object and populates its properties with information from a compatible UAM\_IV CAMx file. The file type will be determined automatically and fail gracefully if not recognized.

```fortran
! Subroutince call
CALL read_uamfile(fl)
CALL read_uamfile(fl, in_file, unit)
! Method call
CALL fl%read()
CALL fl%read(in_file, unit)
```
##### Required
* `fl`: A `UAM_IV` type *object like* variable
##### Optional
* `in_file`: Path to the input file. If not provided the method will use the value of the `in_file` property of the `fl` object.
* `unit`: Fortran unit for IO. If not provided the method will assign a Fortran unit to the `unit` property of the `fl` object automatically.

#### Subroutine: `write_uamfile`, method `write`
Takes a `UAM_IV` type object and writes its properties with to UAM\_IV CAMx file. The file type will be determined by the `ftype` parameter of the input object and fail gracefully if not recognized.

```fortran
! Subroutince call
CALL write_uamfile(fl)
CALL write_uamfile(fl, in_file, unit)
! Method call
CALL fl%write()
CALL fl%write(in_file, unit)
```
##### Required
* `fl`: A `UAM_IV` type *object like* variable
##### Optional
* `in_file`: Path to the input file. If not provided the method will use the value of the `in_file` property of the `fl` object.
* `unit`: Fortran unit for IO. If not provided the method will assign a Fortran unit to the `unit` property of the `fl` object automatically.

#### Subroutine `inquire_header`
Reads the main header of a UAM\_IV CAMx file without reading the data frames, produces no terminal output.

```fortran
CALL inquire_header(fl)
CALL inquire_header(fl, in_file, unit)
```
##### Required
* `fl`: A `UAM_IV` type *object like* variable
##### Optional
* `in_file`: Path to the input file. If not provided the method will use the value of the `in_file` property of the `fl` object.
* `unit`: Fortran unit for IO. If not provided the method will assign a Fortran unit to the `unit` property of the `fl` object automatically.

### Indexing

#### Function `fl_spindex`, method `spindex`
Ouputs the species index corresponding to a provided species name string that matches one of the species names in the c_spname array

````fortran
! Function call
spec = 'TERP'
species_index = fl_spindex(fl,spec)
! Method call
terp_emis = fl%aemis(:,:,:,fl%spindex(spec))
````
##### Required
* `fl`: A `UAM_IV` type *object like* variable
* `spec`: Species name, must be in the c_spname list

## Utilities
These utilities are defined in the `utils_uam_iv` module

### Cloning

#### Subroutine `clone_header`
Copies the main header information from an source `UAM_IV` *object like* variable into a destination variable of the same type.

```fortran
CALL clone_header(fl_inp, fl_out)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable
* `fl_out`: Destination `UAM_IV` *object like* variable

#### Subroutine `clone_species`
Copies the species lists of a `UAM_IV` *object like* variable into a destination variable of the same type.

```fortran
CALL clone_header(fl_inp, fl_out)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable
* `fl_out`: Destination `UAM_IV` *object like* variable

### Transformation

#### Subroutine `lintrans`
Applies a linear transformation from an input matrix file to the species axis of a `UAM_IV` *object like* variable

```fortran
CALL lintrans(fl_inp, fl_out, mat_file)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable
* `fl_out`: Destination `UAM_IV` *object like* variable
* `mat_file`: Linear transformation matrix for species

### Aggregation

#### Subroutine `concatenate`
Takes a set of `UAM_IV` *object like* variables and concatenates them in time into a new object. For example, to create a weekly file from daily files.

```fortran
CALL concatenate(fl_inp, fl_out)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable vector
* `fl_out`: Destination `UAM_IV` *object like* variable

#### Subroutine `average`
Takes a `UAM_IV` *object like* variable and averages it over the time axis.

```fortran
CALL average(fl_inp, fl_out)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable
* `fl_out`: Destination `UAM_IV` *object like* variable

#### Subroutine `totalize`
Takes a `UAM_IV` *object like* variable and totalizes it over the time axis.

```fortran
CALL totalize(fl_inp, fl_out)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable
* `fl_out`: Destination `UAM_IV` *object like* variable

#### Subroutine `flatten`
Takes a `UAM_IV` *object like* variable of the `PTSOURCE  ` `ftype`, i.e. a Point Source emissions file, and adds all stack emissions into a `UAM_IV` *object like* variable of the `EMISSIONS` `ftype`, i.e. an Area emissions file.

```fortran
CALL totalize(fl_inp, fl_out)
```
##### Required
* `fl_inp`: Source `UAM_IV` *object like* variable of the `PTSOURCE  ` `ftype`
* `fl_out`: Destination `UAM_IV` *object like* variable of the `EMISSIONS` `ftype`
