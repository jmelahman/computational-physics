program proj2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Prjoect II: The Structure of White Dwarf Stars
!   Uses the central density of a star to determine
!   the total mass, radius, gravitational energy,
!   kinetic energy of electrons and total energy.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none
    real :: rho                     !Scaled rho
    real :: rho1,rho2               !Initial and final rho
    real :: rho0                    !Rho scaling factor
    real :: Ye                      !Electron to nucleon ratio
    real :: h                       !Logarithmic step for rho
    integer :: i                    !Counter
    integer :: imax                 !Number of models

10  print *,'Enter the value of Ye'
!    read *,Ye
20  print *,'Enter the intitial density RHO > 1E7'
!    read *,rho1
!    print *,'Enter the final density RHO < 1E18'
!    read *,rho2
!    print *,'Enter the number of points'
!    read *,imax
    Ye = 1.0
    imax = 4.0
    rho1 = 1.0E5
    rho2 = 1.0E10

!Makes sure that 0 < Ye <= 1
    if (Ye > 1.0 .OR. Ye <= 0) then
        print *,'0 < Ye <= 1'
        goto 10
    end if
!Makes sure that rho2 > rho1
    if (rho2 <= rho1)then
        print *,'Final density > initial'
        goto 20
    end if

    rho0 = 9.79E8/Ye
    rho1 = rho1/rho0
    rho2 = rho2/rho0
    h = log(rho2-rho1)/(imax-1)

    do i=0,imax-1
        rho = exp(i*h)+rho1-1.0
        print *,i+1
        print *,'Central density     =',rho
        call step2(rho,Ye)
    end do
end program

subroutine step2(rho,Ye)
implicit none
    real :: r                       !Radius of the star
    real :: rho                     !Density of the star
    real :: krho1,krho2,krho3,krho4 !Density terms for the Runge-Kutta
    real :: drho                    !Derivative of rho with respect to radius
    real :: m                       !Mass of the star
    real :: km1,km2,km3,km4         !Mass terms for the Runge-Kutta
    real :: dm                      !Derivative of mass with respect to radius
    real :: dr                      !radial step
    real :: kinetic                 !Kinetic and rest energy of electrons
    real :: grav                    !Gravitational energy of star
    real :: epsrho                  !Epsilon from equation II.10a
    real :: Ye                      !Number of electrons per nucleon
    integer :: i                    !counter
    real :: rhocent
    real :: rhoexact

    dr = .0001/rho**(1.0/3.0)
    rhocent = rho
    i = 0
    m = 0.0
    kinetic = 0.0
    grav = 0.0
    do while (rho > .0001)

        i = i+1

        call intdr(i*dr,m,rho,drho,dm)
        km1 = dr*dm
        krho1 = dr*drho

        call intdr((i+.5)*dr,m+km1/2.0,rho+krho1/2.0,drho,dm)
        km2 = dr*dm
        krho2 = dr*drho

        call intdr((i+.5)*dr,m+km2/2.0,rho+krho2/2.0,drho,dm)
        km3 = dr*dm
        krho3 = dr*drho

        call intdr((i+1)*dr,m+km3,rho+krho3,drho,dm)
        km4 = dr*dm
        krho4 = dr*drho

        m = m+(km1+2.0*km2+2.0*km3+km4)/6.0
        rho = rho+(krho1+2.0*krho2+2.0*krho3+krho4)/6.0
        r = i*dr

!        call eps(rho,epsrho)
!        kinetic = kinetic+r*r*epsrho
!        grav = grav+m*rho*r
    end do
!    rhoexact = rhocent/(1.0+r*r*rhocent**(4./5.)/15.0)**(5.0/2.0)
!Exact answer for Cap Gamma = 6/5 in subroutine intdr
    rhoexact = rhocent*sin(r)/r
!Exact answer for Cap Gamma = 2 in subroutine intdr
    print *,'r',r
    print *,'rho',rho,'rhoexact',rhoexact
    print *,'error',rho-rhoexact
!    kinetic = kinetic*dr
!    grav = grav*dr
!    call scale(Ye,m,r,grav,kinetic)
    return
end subroutine

subroutine intdr(r,m,rho,drho,dm)
!equations used when integrating with respect to the radius
implicit none
    real :: m           !Mass at given radius (in)
    real :: rho         !Density !        rho = 1200.0at given radius (in)
    real :: r           !Given radius
    real :: drho        !Density derivative at r (out)
    real :: dm          !Mass derivative at r (out)
    real :: gamma       !Local variable for gamma
    real :: capgam      !Capital gamma
    capgam = 2.0/1.0
    gamma = rho**(capgam-1.)
    drho = -m*rho/gamma/r/r
    dm = rho*r*r
    return
end subroutine

subroutine eps(rho,epsrho)
!Equation for epsilon in equation II.10a. Converts rho -> x
implicit none
    real :: rho             !Rho (in)
    real :: epsrho          !Epsilon (out)
    real :: x
    x = rho**(2.0/1.0)
    epsrho = 3.0*(x*(1+2.0*x*x)*sqrt(1+x*x)-log(x+sqrt(1+x*x)))/8.0
    return
end subroutine

subroutine scale(Ye,m,r,grav,kinetic)
!Inputs the final, unscaled values, scales and prints
implicit none
    integer, parameter :: ikind=selected_real_kind(p=15)
    real :: m           !Final mass
    real :: r           !Final radius
    real :: grav        !Final gravitational energy
    real :: kinetic     !Final kinetic energy
    real :: Ye          !Number of electrons per nucleon
    real :: r0          !Scaling factor for radius
    real :: m0          !Scaling factor for mass
    real (kind=ikind) :: e0 !Scaling factor for energy

    r0 = 7.72E6*Ye
    m0 = 5.67E30*Ye*Ye
    e0 = 2.7746E44_ikind*Ye**3

    print *,'Total mass          =',m*m0*1.0_ikind,'kg'
    print *,'Total radius        =',r*r0*1.0_ikind,'m'
    print *,'Graviational energy =',(-grav*e0),'joules'
    print *,'Kinetic energy      =',(kinetic*e0),'joules'
    print *,'Total energy        =',((kinetic-grav)*e0),'joules'
    print *,
    return
end subroutine
