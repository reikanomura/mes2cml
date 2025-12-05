program mes2cml
implicit none
integer :: i, j, k
integer :: node, nelem
integer, allocatable :: nc(:,:)
double precision, allocatable :: xy(:,:)

double precision :: rhos, cyoung, rpoisson, cthermal

integer :: elem_type
integer :: melem, nen, ngauss
integer :: nprop, idprop, nmat, idmat, idmod, icond
integer :: npara, nstatev
double precision :: deps

character(40) :: mesfile, cmlfile

open(10, file='file.dat', status='old', action='read')
  read(10,'(a)')mesfile
  read(10,*) rhos, cyoung, rpoisson, cthermal
  read(10,'(a)')cmlfile
close(10)

!------------------------------------------------
write(6,*) 'Input element type & Press Enter key'
write(6,*) 'Press 1 : Tetra  4-nodes (1st order)'
write(6,*) 'Press 2 : Tetra 10-nodes (2nd order)'
read(5,*) elem_type

call make_parameter(elem_type, melem, nen, ngauss, icond, &
                    nprop, idprop, nmat, idmat, deps, idmod, npara, nstatev)

!------------------------------------------------

open(11, file = mesfile, status = 'old', form='unformatted')
 read(11)node, nelem
 allocate(xy(3,node), nc(nen,nelem))
 read(11)((xy(j,i), j=1,3), i = 1,node)
 read(11)((nc(j,i), j=1,nen), i = 1, nelem)
close(11)

write(6,*) 'Done reading: ', mesfile

open(12, file=cmlfile, status='replace', action='write')
 !------------------
 write(12,'(a)') "/TITLE/"
 write(12,'(a)') "This CML-formatted file was translated from mesh.bin format by mes2cml.f90 for FEMsolid"

 !------------------

 write(12,'(a)') "/PROPE/"
 write(12,'(i5)') nprop
 write(12,'(i5)') idprop
 write(12, '(5i5)') idmat, melem, nen, ngauss, icond
 write(12, '(5E15.5)') rhos,  deps,  0.0,  0.0, 0.0

 !------------------
 write(12, '(a)') "/MATER/"
 write(12, '(i5)') nmat
 write(12, '(i5)') idmat
 write(12, '(5i5)') idmod, 0, 0, 0, 0
 write(12, '(5i5)') npara, nstatev, 0, 0, 0
 write(12, '(3E15.5)') cyoung,  rpoisson, cthermal

 !------------------
 
 write(12, '(a)') '/COORDS/'
 write(12, '(i10)') node
 do i = 1, node
  write(12, '(i10, 3E15.5)') i, (xy(j,i), j = 1, 3)
 end do
    
 !------------------
 write(12, '(a)') '/ELEMT/'
 write(12, '(i10)') nelem
 do i = 1, nelem
   write(12, '(i10,4i5,4i10 )') i, idprop, 0, 0, 0, (nc(j,i), j =1,nen)
 end do

 !------------------
  
 write(12, '(a)') '/ENDOF/'
close(12)

write(6,*) 'Done writing: ', cmlfile

end program

!------------------------------------------------------------------
subroutine make_parameter(elem_type, melem, nen, ngauss, icond, &
                          nprop, idprop, nmat, idmat, deps, &
                          idmod, npara, nstatev)
!------------------------------------------------------------------
implicit none
integer, intent(in) :: elem_type
integer, intent(out) :: melem, nen, ngauss, icond
integer, intent(out) :: nprop, idprop, nmat, idmat
integer, intent(out) :: idmod, npara, nstatev
double precision, intent(out) :: deps


select case(elem_type)
  case(1)  ! Tetra  4-nodes 1st order

     melem = 30
     nen = 4
     ngauss = 1
     icond = 0
     nprop = 1
     idprop = 1  
     nmat  = 1
     idmat = 1    
     
     deps = 0.0d0

     idmod = 1   ! Constitutive law : #1 Linear elastic
     npara = 3   ! Number of parameter (Yound modulue, Poisson ratio, Thermal coefficient)
     nstatev = 0  ! Number of internal parameters

  case(2)  ! Tetra 10-nodes 2nd order

     melem = 30
     nen = 10
     ngauss = 2
     icond = 0
     nprop = 1
     idprop = 1
     nmat = 1
     idmat = 1

     deps = 0.0d0

     idmod = 1   ! Constitutive law : #1 Linear elastic
     npara = 3    ! Number of parameter (Yound modulue, Poisson ratio, Thermal coefficient)
     nstatev = 0  ! Number of internal parameters

end select


end subroutine
