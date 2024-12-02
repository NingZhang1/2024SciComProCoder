! Subroutines and modules in calcuating F_j(T). Must be compiled firstly
module erimod
  implicit none

  ! --- Internal Parameters ---      
  real*8 :: ftstar(0:22,120)
  real*8, parameter :: two=2.d0,tstarstep=0.1d0
  integer,parameter :: ntstep=10,jmax=16
   
end module erimod

subroutine set_ftstar()
!******************************************************************************
! Description:
!   SET UP A TABLE OF F(T*) EXPANSION COEFFICIENTS FOR A
!   7 TERM TAYLOR SERIES IN T OVER THE RANGE 0<T<12. AN
!   INTERVAL OF 0.1 IS USED IN T.
!   Notice: J <=16, so up to [gg|gg] function
! Input:    
!
! Output:
!
! Formular:
!
!******************************************************************************
  use erimod, only : ftstar
  implicit none
  !----dummy variables --------------------------------------------------------
  real*8,parameter :: zero=0.0d+00,tenth=0.1d+00,one=1.0d+00,two=2.0d+00, &
                      fivhnd=0.05d+00,error=1.0d-13,xx43=43.0d+00,        &
                      xx45=45.0d+00,xx47=47.0d+00,xx2115=2115.0d+00
  !----local paramters --------------------------------------------------------
  !----local variables --------------------------------------------------------
  integer :: ii,k
  real*8  :: xxii,t,zm,fact,ym,yy,w

  ! Set table F(T*), T=[0,12) with interval 0.1
  do ii=1,120
     xxii = dble(ii)
     t    = xxii*tenth - fivhnd
     zm   = exp(-t)
     fact = xx47
     ym   = two*t
     yy   = ym/xx2115
     w    = one/xx45
     w=w+yy
     do while (yy/w.ge.error)
       fact = fact+two
       yy   = yy*ym/fact
       w=w+yy 
     enddo
     w     = w*zm
     ftstar(22,ii)=w
     !m     = (ii-1)*23+1
     !cc(m) = w
     fact  = xx43
     do k=21,0,-1
        !cc(m+k) = (ym*cc(m+k-1)+zm)/fact
        ftstar(k,ii) = (ym*ftstar(k+1,ii)+zm)/fact
        fact    = fact-two
     enddo
     !write(6,"(5f20.8)") ftstar(0:4,1)
     !stop 888
  enddo
  ! end calcualte F(T*)
  return
end subroutine set_ftstar

subroutine eri_r000j(j,t,ft,lambda,alpha,icase,fac)
  ! calculate r000j
  ! icase=1, M-D R000j
  ! Notice: we use recursion relation proposed by Pople and Head Gordon to 
  !         calcualte Rnml0 from R000j, so there has a factor -1 which is
  !         diffirent from original paper of Davidson
  !    R000j=(2a)^j Fj(T)
  ! Original R000j=(-2a)^j Fj(T)
  use erimod, only : ftstar,tstarstep,ntstep,jmax,two
  implicit none
  integer,intent(in) :: j,icase
  real*8, intent(in) :: t,lambda,alpha,fac
  real*8, intent(out):: ft(0:j)
  !--------------------------------------------
  real*8,parameter :: dzero=0.d0, pt75=0.75d0, half=0.5d0
  real*8,parameter :: one=1.d0
  real*8,parameter :: pi=3.14159265358979323846264338327950d0
  real*8,parameter :: sixth =0.166666666666667d0
  real*8,parameter :: fifth =0.2d0
  real*8,parameter :: fourth=0.25d0
  real*8,parameter :: third =0.333333333333333d0
 
  real*8,parameter :: d12=12.d0,d15=15.d0,d18=18.d0,d24=24.d0,d30=30.d0, &
                      d64=64.d0
  real*8,parameter :: gc11=0.4999489092d0,gc12=-0.2473631686d0, &
                      gc13=0.321180909d0, gc14=-0.3811559346d0
  real*8,parameter :: gc21=0.4998436875d0, gc22=-0.24249438d0,  &
                      gc23=0.2462845d0
  real*8,parameter :: gc31=0.499093162d0, gc32=-0.2152832d0
  real*8,parameter :: gc41=0.490d0
  !--------------------------------------------
  real*8 :: ftfac(7)
  data ftfac/ 1.d0,1.d0,2.d0,6.d0,24.d0,120.d0,720.d0/
  real*8  :: delta,fjtmp,vv,gg,fjt
  real*8,external :: upward_fjadd1_rr1,upward_fjadd1_rr2,downward_fj_rr
  integer :: i,k,tgrid

  fjt=dzero
  ft=dzero
  if(t.lt.d12) then  ! t <12
    ! calculate T* and T*-T
    tgrid=int(t*ntstep+one)
    delta=tgrid*tstarstep-0.05d0-t
    fjt=(((((ftstar(6+j,tgrid) *delta*sixth    & 
            +ftstar(5+j,tgrid))*delta*fifth    &
            +ftstar(4+j,tgrid))*delta*fourth   &
            +ftstar(3+j,tgrid))*delta*third    &
            +ftstar(2+j,tgrid))*delta*half     &
            +ftstar(1+j,tgrid))*delta          &
            +ftstar(0+j,tgrid)

    ! Downward recuresion relation
    ft(j)=fjt
    do i=j-1,0,-1
      fjtmp=ft(i+1)
      ft(i)=downward_fj_rr(i,t,fjtmp)
    enddo
    write(6,*) "T*",tgrid*tstarstep-0.05,T,fjt
    !write(6,"(5f20.14)") ft(0:j)
    !stop 888
  else if(t.ge.d12.and.t.le.d30) then
     ! calculate F0(T)
     fjt=half*dsqrt(pi/t)
     if(t.le.d15) then  ! 12 < t < 15
       vv=one/t
       gg=gc11+gc12*vv
       vv=vv/t
       gg=gg+gc13*vv
       vv=vv/t
       gg=gg+gc14*vv
     else if(t.gt.d15.and.t.le.d18) then
       ! 15 < t < 18
       vv=one/t
       gg=gc21+gc22*vv
       vv=vv/t
       gg=gg+gc23*vv
     else if(t.gt.d18.and.t.le.d24) then
       ! 18 < t < 24
       vv=one/t
       gg=gc31+gc32*vv 
     else  ! 24 < t < 30
       gg=gc41
     endif
     fjt=fjt-dexp(-t)*gg/t

     ! upward recursion relation to calculate Fj(T)
     ft(0)=fjt
     do i=0,j-1
       ft(i+1)=upward_fjadd1_rr1(i,t,ft(i))
     enddo
  else if(t.gt.d30) then
     ft(0)=half*dsqrt(pi/t)
     if(t.gt.d64) then
       ! T > 2J+36
       do i=0,j-1
         ft(i+1)=upward_fjadd1_rr2(i,t,ft(i))
       enddo
     else
       do i=0,j-1
         ft(i+1)=upward_fjadd1_rr1(i,t,ft(i))
       enddo
     endif
  endif
  !write(6,*) "T=",t,lambda,alpha
  !write(6,"(5(f20.14,1x))") ft(0:j)
  !stop 888
  !write(6,*) "T=",t
  !write(6,"(5(f20.14,1x))") ft(0:j)
 

  if(icase.eq.1) then
    ! this bratch is used in J-Engine
    ! multiply by Lambda and (-2\alpha)^j
    vv=fac
    do i=0,j
      ft(i) = lambda*vv*ft(i)
      vv    = vv*two*alpha
    enddo
  else
    ! this brach is for testing
    ! multiply by Lambda !and (-2\alpha)^j
    !vv=one
    !do i=0,j
    !  ft(i)=lambda*vv*ft(i)
    !  vv=-vv*two*alpha
    !enddo
    do i=0,j
      ft(i)=lambda*ft(i)
    enddo
  endif
  !write(6,*) "Scaled ft",fac
  !write(6,"(5(f20.14,1x))") ft(0:j)
 
 !stop 888

  return
end subroutine eri_r000j

real*8 function downward_fj_rr(j,t,fjadd1)
  implicit none
  integer,intent(in) :: j
  real*8, intent(in) :: t,fjadd1
  real*8, parameter :: one=1.d0,two=2.d0
  real*8 :: v0,v1

  v0=two*T*fjadd1+dexp(-T)
  v1=two*dble(j)+one
  !write(6,"(4f20.14)") T,dexp(-T),V0,V1
  downward_fj_rr=v0/v1

end function downward_fj_rr

real*8 function upward_fjadd1_rr1(j,t,fj)
  implicit none
  integer,intent(in) :: j
  real*8, intent(in) :: t,fj
  real*8, parameter :: one=1.d0,two=2.d0
  real*8 :: v0,v1
  v0=dble(2*j+1)*fj-dexp(-t)
  v1=two*t
  v1=one/v1

  upward_fjadd1_rr1=v0*v1

  return
end function upward_fjadd1_rr1

real*8 function upward_fjadd1_rr2(j,t,fj)
  implicit none
  integer,intent(in) :: j
  real*8, intent(in) :: t,fj
  real*8, parameter :: one=1.d0,two=2.d0
  real*8 :: v0,v1
  v0=dble(2*j+1)*fj
  v1=two*t
  v1=one/v1

  upward_fjadd1_rr2=v0*v1

  return
end function upward_fjadd1_rr2


program checkcode
  implicit none
  real*8 :: T,Fn,lambda,alpha,fac
  integer :: n
  real*8,allocatable :: ft(:)

  call set_ftstar()

  n = 0
  allocate(ft(0:n))
  ft = 0.d0
  lambda = 1.d0 
  alpha  = 1.d0
  fac    = 1.d0
  T      = 0.0  
  call eri_r000j(n,t,ft,lambda,alpha,1,fac)
  write(6,"(a,2F14.8)") "FT",T,ft(0)
  T = 12.724842788
  call eri_r000j(n,t,ft,lambda,alpha,1,fac)
  write(6,"(a,2F14.8)") "FT",T,ft(0)
 
  deallocate(ft)
end 

