      subroutine CSR_Dim(Mat,n,m,n0,thre)
      implicit none
      integer::n,m,n0
      real(kind=8) Mat(n,m),thre
      integer::i,j
      
      n0=0
      do i=1,n
        do j=1,m
          if(dabs(Mat(i,j))>=thre) then
            n0=n0+1
          end if
        end do
      end do
      
      return
      end 
      
      subroutine Mat_to_CSR(Mat,n,m,Va,Ja,Ia,thre,io)
      implicit none 
      integer::n,m,io,Ja(*),Ia(n+1)
      real(kind=8) Mat(n,m),Va(*),thre
      integer::i,j,KK


      KK=0;Ia(:)=0
      Ia(1)=1
      do i=1,n
        do j=1,m
          if(dabs(Mat(i,j))>=thre) then
            KK=KK+1
            Va(KK)=Mat(i,j)
            Ja(KK)=j
          end if
        end do
        Ia(i+1)=KK+1    
      end do

      return
      end
            
      subroutine CSR_to_Mat(Va,Ja,Ia,Mat,n,m,io)
      implicit none
      
      integer::n,m,Ja(n*m),Ia(n+1)
      real(kind=8) Mat(n,m),Va(n*m)
      integer::i,j,k1,k2,io
      
      Mat=0d0
      do i=1,n
         k1=Ia(i);k2=Ia(i+1)-1
         do j=k1,k2
             Mat(i,Ja(j))=Va(j)
         end do
      end do
      
      return
      end 


      subroutine amub(nrow,ncol,a,ja,ia,b,jb,ib,c,jc,ic)
      real*8 a(*), b(*), c(*) 
      integer::nrow,ncol,len 
      integer::ja(*),jb(*),jc(*),ia(nrow+1),ib(*),ic(*)
      integer::i,j,k,ii,jj
      integer::jcol,jpos,kb,ka
      integer,allocatable::iw(:)
      real*8 scal 

      allocate(iw(ncol))
      len = 0
      ic(1) = 1 
      iw(:) = 0

      do 500 ii=1,nrow 

         do 200 ka=ia(ii),ia(ii+1)-1 
	      scal=a(ka)
	      jj=ja(ka)
	    do 100 kb=ib(jj),ib(jj+1)-1
               jcol=jb(kb)
               jpos=iw(jcol)
               if (jpos.eq.0) then

                  len=len+1
                  jc(len)=jcol
                  iw(jcol)=len
                  c(len)=scal*b(kb)

               else
                  c(jpos)=c(jpos)+scal*b(kb)
               endif
 100	    continue
 200     continue
         do 201 k=ic(ii),len
	    iw(jc(k))=0
 201     continue
         ic(ii+1)=len+1
 500  continue

      deallocate(iw)
      return
      end

