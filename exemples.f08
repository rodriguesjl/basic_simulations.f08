Module Parametros
Implicit None
Integer,Parameter  :: NMCSteps=1000000
Real*8,Parameter   :: x0=0.0D0,xf=5.0D0,y0=0.0D0,     &
                      yf=10.0D0,z0=0.0D0,zf=10.0D0,    &
                      w0=0.0D0,wf=10.0D0
End Module Parametros
!==============================================
Real*8 function func(x,y,z,w)
Use Parametros
IMplicit None
Real*8,Parameter  :: ax=1.0D0,axy=0.0D0,ay=0.0D0,ayz=0.0D0, &
                     az=0.0D0,azw=0.0D0,aw=0.0D0
Real*8   :: x,y,z,w
func = Dexp(-(ax*x**2 + axy*x*y + ay*y**2 + ayz*y*z   +             &
            az*z**2+ azw*z*w + aw*w**2))
End function func
!==============================================
Subroutine gera(frandom,x,y,z,w)
Use Parametros
Implicit None
Real*8   :: frandom,x,y,z,w
Real*8,Dimension(5)   :: r
Call Random_Number(r)
x=xf*r(1)
y=yf*r(2)
z=zf*r(3)
w=wf*r(4)
frandom=r(5)
End Subroutine gera

!==============================================
Program Integral_MC
Use Parametros
Implicit None
Real*8   :: f,func
REal*8   :: x,y,z,w,frandom,Integral
Integer  :: i
Integral = 0.0D0
 Loop_Monte_Carlo : Do i = 1 , NMCSteps
 Call gera(frandom,x,y,z,w)
 f = func(x,y,z,w)
 If(frandom .le. f)Then
 Integral=Integral+1
 Endif
End Do Loop_Monte_Carlo 
Write(*,*)Real(Integral/NMCSteps)
END
