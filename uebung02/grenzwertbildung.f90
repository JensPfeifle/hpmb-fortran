program grenzwertbildung
implicit none
integer, parameter      :: dp = kind(1.d0)
real(dp)                :: a,b,x,step
integer                 :: n,i
character(len=32)       :: filename

integer                 :: nargs,arglen, stat
character(len=256)      :: args

! analytisch
! lim x->0 f(x) = -1/8
! numerisch:
! input: intervall [a,b], anzahl stuetzpunkte n, outputfile f
! output: x, f(x) auf intervall in datei

!get arguments
nargs = command_argument_count()

arguments: do i=0,nargs
    call get_command_argument(i, args, arglen,stat)
    select case (i)
        case(1)
            read(args(1:arglen),*)a
        case(2)
            read(args(1:arglen),*)b
        case(3)
            read(args(1:arglen),*)n
        case default
    end select
end do arguments


write(*,*) 'calculating f(x) on [',a,',',b,'] in ',n,'steps.'

step = (b-a)/n
    intervall: do i=0,n
    x = a + i*step
    write(*,*) 'f(',x,') =', f(x)
end do intervall


contains
! unterprogramme
! haben zugriff auf variablen von oben
! argumente mit gleichem namen ueberschreiben globale
pure function f(x) result(retval)
    real(dp), intent(in) :: x
    real(dp) :: retval
    retval = (sqrt(16.d0+x)-4.d0)/x
end function f


end program grenzwertbildung