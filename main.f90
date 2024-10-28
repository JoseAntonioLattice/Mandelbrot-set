program main

  use iso_fortran_env, only : dp => real64
  implicit none

  
  integer, parameter :: n = 10
  integer, parameter :: l = 5000
  integer, parameter :: max_iterations = 100
  complex(dp) :: z, c
  real(dp), dimension(l) :: x, y
  integer :: i,j,k,p, un

  x = linspace(-2.0_dp,1.5_dp, l)
  y = linspace(-2.0_dp,2.0_dp, l)

  open(newunit = un, file = 'madelbrot.dat')
  do i = 1, l
     do j = 1, l
        c = cmplx(x(i),y(j),dp)
        z = (0.0_dp,0.0_dp)
        iter: do k = 1, max_iterations
           if( abs(z) >= 2.0_dp)then
              p = k
              exit iter
           else
              p = 0
              z = z**2 + c
           end if
        end do iter
        write(un,*) x(i), y(j), abs(z), real(p)
     end do
     write(un,'(3/)')
  end do

contains

  function linspace(x,y,n)
    real(dp), intent(in) :: x, y
    integer, intent(in) :: n
    real(dp), dimension(n) :: linspace
    real(dp) :: dx

    dx = (y-x)/(n-1)
    linspace = [(x + dx*i, i = 0, n - 1 )]
  end function linspace
  
end program main
