      Program main
      implicit none
      Integer,Parameter :: NATOM=2
      Integer,Parameter :: NORB=4
      Integer,Parameter :: Num_ele=2
      integer i,j,k,l
      Real*8  GEOM(NATOM,3)
      Integer ATOMCHG(Natom),iERRO
      Real*8 Hcore(NORB,NORB)       !One-electron integral 
      Real*8 S(NORB,NORB)           !Overlap matrix
      Real*8 S_DIAG(NORB,NORB)      !S^(-0.5)
      real*8 U_matrix(NORB,NORB)    !Equal to Overlap matrix
      Real*8 U(NORB,NORB,NORB,NORB) !Two-electron integral
      character     :: baselable*30
      !*****************add**********************
      character*25 file_name
      real*8 X_transfer(NORB,NORB),X_adjoint(NORB,NORB)
      real*8 P_old(NORB,NORB) ,P_new(NORB,NORB),G_ij(NORB,NORB)
      real*8 Fock(NORB,NORB),Fock_x(NORB,NORB)
      real*8 C_prime(NORB,NORB),C_new(NORB,NORB)
      real*8 E_nu,E_ele,E2,delta
      real*8 P_den(NORB,NORB),orb_energy(NORB)
      real*8 P_new1(NORB,NORB)
      real*8 Fock_xx(NORB,NORB),ING(NORB,NORB)
      real*8 ink
      integer Zn(NATOM)             !the atom charges
      logical logical_e
      integer count_iter
      real*8 doublej,doublek,twoout
      !*********************MP2******************
      real*8 Hcore_mo(NORB,NORB),U_mo(NORB,NORB,NORB,NORB)
      integer m,n,o,p
      real*8  E_mp2
      integer Num
      !******************************************
      file_name='GEOM1.xyz'
      baselable='6-31G'
      CALL RD_GEOM(NATOM,GEOM,ATOMCHG,iERRO,file_name)
      CALL INTXC_ALLOCATE(NATOM)
      CALL BASIS_INT(NATOM,NORB,GEOM,ATOMCHG,S,Hcore,U,baselable)
      CALL INTXC_DEALLOCATE()
      Zn=ATOMCHG
      open(21,file='output.log')
      
      write(21,*)'Basic set:',baselable
      write(21,*)'overlap matrix'
      do i=1,NORB
                write(21,*)S(i,:)
      enddo
      write(21,*)'***************'
      close(22)
        
      U_matrix=S

      CALL DIAG(U_matrix,S_DIAG,NORB) 
       
      do i=1,NORB
               S_DIAG(i,i)=S_DIAG(i,i)**(-0.5d0)
      enddo
      !orthogonalization transformation matrix(eigenvectors*eigenvalues)
      X_transfer = matmul(U_matrix,S_DIAG)
      
      !write(*,*)X_transfer
      !X_transfer transpose get X^+
      X_adjoint =transpose(X_transfer)
      call E_nuclear(NATOM,GEOM,Zn,E_nu)  
      count_iter=0
      P_den = Hcore
      P_old=0.0d0 
      logical_e=.true.
      E2=0.0d0
      Fock_xx(:,:)=0.0d0 
!!!===================================
      !  SCF CYCLE
!!!===================================
      !do i = 1,1
      do while(logical_e)
        count_iter=count_iter+1
        !print*,count_iter
        
       !call G_matrix(P_old,U,NORB,G_ij)
        G_ij=0.0d0  
        do i=1,NORB
           do j=1,NORB
              do k=1,NORB
                 do l=1,NORB
                        CALL INTXC_ALLOCATE1(NATOM)
                        CALL BASIS_INT1(i,j,l,k,NATOM,NORB,GEOM,ATOMCHG,S,Hcore,twoout,baselable)
                        doublej=twoout
                        CALL INTXC_DEALLOCATE1()
                        CALL INTXC_ALLOCATE1(NATOM)
                        CALL BASIS_INT1(i,k,l,j,NATOM,NORB,GEOM,ATOMCHG,S,Hcore,twoout,baselable)
                        doublek=twoout*0.5d0
                        CALL INTXC_DEALLOCATE1()
                        !doublej=U(i,j,k,l)
                        !doublek=0.5d0*U(i,k,j,l)
                        G_ij(i,j)=G_ij(i,j) + P_old(k,l)*(doublej-doublek)
                 enddo
              enddo
           enddo
        enddo
        Fock = Hcore + G_ij
        Fock_x = matmul(X_adjoint,matmul(Fock,X_transfer))
        Fock_xx=Fock_x

        !now,Fock_prime output is an eigenvector fock_prime=C'
        call DIAG(Fock_xx,C_prime,NORB)
        !write(*,*)matmul(Fock_xx,transpose(Fock_xx))
        !C=X*C' here,X:X_transfer C':eigenvector
        C_new=matmul(X_transfer,Fock_xx)
        !Update the P_density matrix by new coefficient(C_new)
        call P_n_den(C_new,P_new,NORB,Num_ele)
        !P_new1=matmul(matmul(X_adjoint,P_new),X_transfer)
        !Calculated energy
        call calc_E0(Hcore,Fock,P_new,NORB,E_ele)
        !P_old = P_new
        
        call E_nuclear(NATOM,GEOM,Zn,E_nu)
     
        call delta_P(NORB,P_old,P_new,delta)
        if((delta < 1.0E-4).and.(abs(E_ele-E2) < 1.0E-6))then
        !if((abs(E_ele-E2) < 10.0E-6))then
               logical_e=.false.
               write(21,*)'Density matrix'
               do i=1,NORB
                                write(21,*)P_new(i,:)
               enddo
               write(21,*)'****************************'
               write(21,*)'congratulation! SCF cycle converged'
               write(21,*)'Number of iterations:',count_iter
               write(21,*)'Electronic energy:   ',E_ele
               write(21,*)'Nuclear energy:      ',E_nu
               write(21,*)'Total energy(HF):    ',E_ele+E_nu
               call DIAG_1(Fock_x,orb_energy,NORB)
               !write(*,*)'111' 
               !write(*,*)E_mp2
        endif
        P_old = P_new
        E2 = E_ele
        !write(*,*)'Number of iterations:',count_iter
        !write(*,*)E0_rhf
      enddo
      stop
      !======================BEGIN OF MP2===========================      
       
      Hcore_mo = 0.0d0
      !Calculate the one-electron integral of the molecular orbita
      do i=1,NORB  !a
        do j=1,NORB  !b
          do k=1,NORB  !u
            do l=1,NORB  !v
              Hcore_mo(i,j) = Hcore_mo(i,j)+Hcore(k,l)*C_new(k,i)*C_new(l,j)
            end do
          end do
        end do
      end do
      !Accuracy of one-electron integration of molecular orbitals
      !compared with BDF
      
      do i=1,NORB
         write(23,*)Hcore_mo(i,:)
      enddo
      write(23,*)'*************'
      !========================================================

      !========================================================
      !Calculate the one-electron integral of the molecular orbita
      U_mo = 0.0d0
      
      do i=1,NORB
        do j=1,NORB
          do k=1,NORB
            do l=1,NORB
      !-----m,n,o,p equal to the atomic orbital integral-----------
              do m=1,NORB
                do n=1,NORB
                  do o=1,NORB
                    do p=1,NORB
                    U_mo(i,j,k,l)= U_mo(i,j,k,l)+U(m,n,o,p)*C_new(m,i)*C_new(n,j)*C_new(o,k)*C_new(p,l)
                    end do
                  end do
                end do
              end do
            end do
          end do
       end do
      end do
      
      !=====================================================
      !Accuracy of TWO-electron integration of molecular orbitals
      !compared with BDF
      !=====================================================
      do i=1,NORB
         do j=1,NORB
                do k=1,NORB
                        do l=1,NORB
                         write(23,*)U_mo(i,j,k,l),'   ',i,j,k,l
                        enddo
                enddo
        enddo
      enddo
     !=========================================================
      E_mp2 = 0.0d0
      Num = Num_ele/2
      !denominator = 0.0d0
      
      do i=1,Num    !a
        do j=1+Num,NORB !r
          do k=1,Num       !b
            do l=1+Num,NORB   !s
            !if(i==j.or.k==l) then
            !    cycle
            !else  
            !denominator = orb_energy(i)+orb_energy(j)-orb_energy(k)-orb_energy(l)
            E_mp2 =E_mp2+U_mo(i,j,k,l)*(2*U_mo(i,j,k,l)-U_mo(i,l,k,j))/(orb_energy(i)+orb_energy(k)-orb_energy(j)-orb_energy(l))
            !E_mp2=E_mp2+(U_mo(i,j,k,l))**2/(orb_energy(i)+orb_energy(k)-orb_energy(j)-orb_energy(l))
       
            enddo
          enddo
        enddo
      enddo
     ! E_mp2 =E_mp2 /4
     ! do i=1,Num
     !   do j=1+Num,NORB
     !     do k=1,NORB
     !       do l=1+Num,NORB
     !          E_mp2=E_mp2+(2*U_mo(i,j,k,l)-U_mo(i,l,k,j))**2/(orb_energy(i)+orb_energy(k)-orb_energy(j)-orb_energy(l))
     !       enddo
     !    enddo
     !  enddo
     !enddo
      write(21,*)'********MP2********'
      write(21,*)'E_mp2:',E_mp2
      write(21,*)'total energy(mp2)',E_nu+E_ele+E_mp2
      
      ENDPROGRAM

      SUBROUTINE RD_GEOM(NATOM,GEOM,ATOMCHG,iERRO,file_name)
      IMPLICIT NONE
      INTEGER NATOM,iERRO,I
      INTEGER ATOMCHG(NATOM)
      character*25 file_name
      REAL*8  GEOM(NATOM,3)
      CHARACTER*10 CTYPE(NATOM)
      GEOM=0.0D0
      ATOMCHG=0
      OPEN(23,FILE=file_name)
      !write(*,*)file_name
      DO I=1,NATOM
        READ(23,*)CTYPE(I),GEOM(I,1),GEOM(I,2),GEOM(I,3)
      ENDDO
      CLOSE(23)
      DO I=1,NATOM
        IF(CTYPE(I)=='H'  .or. CTYPE(I)=='h')  ATOMCHG(I)=1
        IF(CTYPE(I)=='He' .or. CTYPE(I)=='he') ATOMCHG(I)=2
        IF(CTYPE(I)=='Li' .or. CTYPE(I)=='li') ATOMCHG(I)=3
        IF(CTYPE(I)=='Be' .or. CTYPE(I)=='be') ATOMCHG(I)=4
        IF(CTYPE(I)=='B'  .or. CTYPE(I)=='b')  ATOMCHG(I)=5
        IF(CTYPE(I)=='C'  .or. CTYPE(I)=='c')  ATOMCHG(I)=6
        IF(CTYPE(I)=='N'  .or. CTYPE(I)=='n')  ATOMCHG(I)=7
        IF(CTYPE(I)=='O'  .or. CTYPE(I)=='o')  ATOMCHG(I)=8
        IF(CTYPE(I)=='F'  .or. CTYPE(I)=='f')  ATOMCHG(I)=9
        IF(CTYPE(I)=='Ne' .or. CTYPE(I)=='ne') ATOMCHG(I)=10
      ENDDO
      END SUBROUTINE RD_GEOM

      SUBROUTINE DIAG(A,S_DIAG,NORB) 
       real*8  A(NORB,NORB),S_DIAG(NORB,NORB)
       real*8  W(NORB),V_m(NORB,NORB)
       integer, parameter :: lwmax=10000
       real*8, dimension(lwmax):: work
       integer i,j,N,LDA,lwork,info

       
       LDA = NORB
       N = NORB
       lwork = -1
       !allocate (work(lwork))
       call dsyev('V', 'U', N, A, LDA, W, work, lwork,info)
       lwork =min(lwmax,int(work(1)))*2*N
       !allocate (work(lwork))
       !call dgeev('V','V',N,A,LDA,wr,wi,vl,ldvl,vr,ldvr,work,lwork,info)
       call dsyev('V', 'U', N, A, LDA, W, work, lwork,info)
      
       if (INFO .NE. 0) THEN
       write(*,*) "ERROR: IMPOSSIBLE TO SOLVE THE EIGENVALUE PROBLEM!" 
       endif

       S_DIAG = 0
       do i=1,NORB
                S_DIAG(i,i)=W(i)
       enddo
       ENDSUBROUTINE
        
       SUBROUTINE DIAG_1(A,W,NORB)
       real*8  A(NORB,NORB)
       real*8  W(NORB),V_m(NORB,NORB)
       integer, parameter :: lwmax=10000
       real*8, dimension(lwmax):: work
       integer i,j,N,LDA,lwork,info
       LDA = NORB
       N = NORB
       lwork = -1
       !allocate (work(lwork))
       call dsyev('V', 'U', N, A, LDA, W, work, lwork,info)
       lwork =min(lwmax,int(work(1)))*2*N
       !allocate (work(lwork))
       !call
       !dgeev('V','V',N,A,LDA,wr,wi,vl,ldvl,vr,ldvr,work,lwork,info)
       call dsyev('V', 'U', N, A, LDA, W, work, lwork,info)

       if (INFO .NE. 0) THEN
       write(*,*) "ERROR: IMPOSSIBLE TO SOLVE THE EIGENVALUE PROBLEM!"
       endif
       
      !open(23,file='S_diag')
      ! write(23,*)'Diagonalize the over matrix'
      !do i=1,NORB
      !         write(23,*)S_DIAG(i,:)
      !         write(24,*)A(i,:)
      !enddo
       ENDSUBROUTINE

      
      !Inverse matrix  
      subroutine INV_MATRIX(m,inva,Nr)
        implicit none
        integer :: i,j
        integer :: info,Nr
        integer :: ipiv(Nr)
        real*8  :: work(Nr)
        real*8 :: m(Nr,Nr),inva(Nr,Nr),n(Nr,Nr)
        !write(*,*)Nr
        !do i =1,Nr           
        !        write(13,*)m(i,:)
        !enddo
        call dgetrf(Nr,Nr,inva,Nr,ipiv,info)
        call dgetri(Nr,inva,Nr,ipiv,work,Nr,info)
        n=matmul(m,inva)
        !do i =1,Nr
        !       write(13,*)n(i,:)
        !enddo
        end subroutine
        
        
        subroutine G_matrix(P_density,U,NORB,G_ij)
        integer i,j,k,l,NORB
        real*8 U(NORB,NORB,NORB,NORB),G_ij(NORB,NORB)
        real*8 P_density(NORB,NORB)
        real*8 temp,twoout
        real*8 doublej,doublek
        character*30 baselable
        G_ij(:,:)=0.0d0
        
        do i=1,NORB
           do j=1,NORB
              do k=1,NORB
                 do l=1,NORB
                        doublej=U(i,j,l,k)
                        doublek=0.5d0*U(i,k,l,j)            
                        G_ij(i,j)=G_ij(i,j) + P_density(k,l)*(doublej-doublek)
                        
                 enddo
              enddo
     
           enddo
        enddo
        endsubroutine   
        

        subroutine P_n_den(C_new,P_new,NORB,Num_ele)
        !input
        integer NORB
        real*8 C_new(NORB,NORB)       !New coefficient matrix
        integer i,j,k,x
        integer Num_ele
        
        !output
        real*8 P_new(NORB,NORB)       !New density matrix
        
        x=Num_ele/2
        !write(*,*)x
        P_new=0.0d0
        
        do i=1,NORB
                do j=1,NORB
                   do k=1,x
                      P_new(i,j)=P_new(i,j)+2.0d0*C_new(i,k)*C_new(j,k)
                   enddo
                enddo
        enddo
        endsubroutine P_n_den
        
        subroutine calc_E0(Hcore,Fock,P_new,NORB,E_ele)
        !input
        real*8 Hcore(NORB,NORB),Fock(NORB,NORB)
        real*8 P_new(NORB,NORB)
        integer NORB
        
        !output
        real*8 E_ele
        integer i,j
        
        E_ele=0.0d0
        do i=1,NORB
                do j=1,NORB
                    E_ele =E_ele + 0.5d0*P_new(j,i)*(Hcore(i,j)+Fock(i,j))
                enddo
        enddo
        !write(*,*)E0_rhf
        endsubroutine calc_E0
        
        subroutine E_nuclear(Nn,Rn,Zn,E_nu) 
        implicit none

        ! input
        integer Nn            ! Totalnumber of nuclei
        integer Zn(Nn)        ! Nuclear charges
        real*8  Rn(Nn,3)      ! Nuclear positions
        real*8  a             ! bohr     
        integer :: i,j        ! Loop variables
        ! output
        real*8  E_nu          ! Nuclear energy

        E_nu = 0.0D0
        a = 0.5291772083  
        do i = 1, Nn
            do j = i + 1, Nn
                E_nu = E_nu + Zn(i)*Zn(j)/NORM2(Rn(i,1:3)/a-Rn(j,1:3)/a)
            enddo
        enddo
            
        endsubroutine E_nuclear      


        subroutine delta_P(NORB,P_old,P_new,delta) 
        implicit none

        ! input
        integer  NORB          
        real*8  P_old(NORB,NORB)    
        real*8  P_new(NORB,NORB)    
        integer :: i,j

       ! output
       ! Sum ofmatrix elements square differences
        real*8 :: delta      !Second convergence criterion 
        

        delta = 0.0d0
        do i = 1, NORB
            do j = 1, NORB
                delta = delta + (P_old(i,j)-P_new(i,j))**2
            enddo
        enddo
     !dsqrt:Square root
        delta = DSQRT(delta / NORB**2)

        endsubroutine delta_P
