       subroutine read_threshold(f, A)
         ! Sep. 2024 Hayeon Noh (PKNU)
         ! This subroutine reads a 3D array A (ix, il, 12) from a NetCDF file
         ! based on the file number (f).
       
#include "qq.h"
#include "netcdf.inc"
       
         integer :: test, ncid, aid
         integer :: f, i, j, m
         real, dimension(ix, il, 12) :: A                  ! 3D threshold 
       
         ! NetCDF 파일 열기
         if (f .EQ. 11) then
           test = nf_open('std1.nc', nf_nowrite, ncid) ! Open NetCDF file
           if (test /= nf90_noerr) then
             print *, 'Error opening file std1.nc'
             stop
           end if
       


            ! Get variable ID from NetCDF file
            test = nf_inq_varid(ncid, 'hgt', aid) ! Get ID for variable name 'A'
            if (test /= nf90_noerr) then
              print *, 'Error finding variable A in file std1.nc'
              test = nf_close(ncid)  ! Close file (in case of error)
              if (test /= nf90_noerr) then
                print *,'Error closing file std1.nc'
              end if
              stop
            end if

       

           ! Read 3D array variable 'A' from NetCDF file
           test = nf_get_var_real(ncid, aid, A)
           if (test /= nf90_noerr) then
             print *, 'Error reading variable A from file std1.nc'
             test = nf_close(ncid)  ! Close file (in case of error)
             if (test /= nf90_noerr) then
               print *, 'Error closing file std1.nc'
             end if
             stop
           end if
       
            ! Close NetCDF file
           test = nf_close(ncid) 
           if (test /= nf90_noerr) then
             print *, 'Error closing file std1.nc'
             stop
           end if
       
           ! Print variable A (for verification)
           do m = 1, 1
             do i = 30, 180, 50
               do j = 150, 150
                 print *, 'A(', i, ',', j, ',', m, ') = ', A(i, j, m)
               end do
             end do
           end do
       
         else
           print *, 'Invalid file number. No data read.'
         end if
       
         return
       end subroutine read_threshold
