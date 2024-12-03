      Program main
      implicit none
      !!!H2O=====
      !Integer,Parameter :: NATOM=3    !atom number
      !Integer,Parameter :: NORB=13    !orbital number(AO)
      !Integer,Parameter :: Num_ele=10 !ele number
      !!!H2======
      !Integer,Parameter :: NATOM=2   !atom number
      !Integer,Parameter :: NORB=4    !orbital number(AO)
      !Integer,Parameter :: Num_ele=2 !ele number
      !!!LiH=====
      Integer,Parameter :: NATOM=2   !atom number
      Integer,Parameter :: NORB=11   !orbital number(AO)
      Integer,Parameter :: Num_ele=4 !ele number
      !!!
      Real*8  GEOM(NATOM,3) 
      integer i,j,k,l,ii,jj,guess
      integer inv_U
      Integer ATOMCHG(Natom),iERRO
      Real*8 Hcore(NORB,NORB)       !One-electron integral 
      Real*8 S(NORB,NORB)           !Overlap matrix
      Real*8 S_DIAG(NORB,NORB)      !S^(-0.5)
      real*8 U_matrix(NORB,NORB)    !Equal to Overlap matrix
      Real*8 U(NORB,NORB,NORB,NORB) ! atom Two-electron integral
      character     :: baselable*30
      !*****************add-HF**********************
      integer :: kbasst(NATOM),kbased(NATOM),kcenter(NATOM)
      character*25 file_name
      real*8 X_transfer(NORB,NORB),X_adjoint(NORB,NORB)
      real*8 P_old(NORB,NORB) ,P_new(NORB,NORB),G_ij(NORB,NORB)
      real*8 Fock(NORB,NORB),Fock_x(NORB,NORB)
      real*8 C_prime(NORB,NORB),C_new(NORB,NORB),Cof(NORB,NORB)
      real*8 E_nu,E_ele,E2,delta
      real*8 P_den(NORB,NORB),orb_energy(NORB)
      real*8 Fock_xx(NORB,NORB),ING(NORB,NORB)
      real*8 J_martix(NORB,NORB),K_martix(NORB,NORB)
      real*8 t2,t1,HF_t
      integer Zn(NATOM)        !the atom charges
      logical logical_e
      integer count_iter
      !***************DIIS********************
      Integer,Parameter :: ndiis=7
      integer inv_DIIS,kq,kp,infodiis
      integer ipivdiis(ndiis+1)
      real*8 sumdiis
      real*8 fockdiis(NORB,NORB,ndiis),errmat(NORB,NORB,ndiis)
      real*8 cofdiis(ndiis+1),Bdiis(ndiis+1,ndiis+1)
      integer ierr,lwork
      real*8,allocatable :: work(:)
      real*8,allocatable :: ipiv(:)
      real*8 FPS(NORB,NORB),SPF(NORB,NORB)
      !***************End***********************
      inv_DIIS=1  !!=1: call DIIS
      open(21,file='output.log')      !!! output file
      !!!H2.txt; LiH.txt; H2O.txt
      !!!!!!=============================================================
      open(26,file='integral/LiH.txt') !!! Read the two-electron integral
      open(27,file='integral/S_LiH.txt')
      open(28,file='integral/Hcore_LiH.txt')
      file_name='xyz/LiH.xyz'         !!! Geometric 
      !!!!!!!=============================================================
      baselable='6-31G'               !!! basis set 
      write(21,*)"Molecule and Coordinates"
      CALL RD_GEOM(NATOM,GEOM,ATOMCHG,iERRO,file_name)
      CALL INTXC_ALLOCATE(NATOM)
      !CALL BASIS_INT(kbasst,kbased,kcenter,NATOM,NORB,GEOM,ATOMCHG,S,Hcore,U,baselable)
      !CALL BASIS_INT(kbasst,kbased,kcenter,NATOM,NORB,GEOM,ATOMCHG,S,Hcore,baselable)
      CALL INTXC_DEALLOCATE()
      Zn=ATOMCHG
      write(21,*)'Basic set:',baselable
      write(21,*)'==========================================='
      write(21,*)'overlap matrix'
      write(21,*)
      do i=1,NORB
          do j=1,NORB
          read(27,*)S(i,j)
          read(28,*)Hcore(i,j)
          enddo
      enddo
      do i=1,NORB
         do j=1,NORB
            do k=1,NORB
               do l=1,NORB
                read(26,*)U(i,j,k,l)
               enddo
            enddo
         enddo
      enddo  
      rewind(26)
      close(26)
      close(27)
      close(28)
      do i=1,NORB
        write(21,*)S(i,:)
      enddo 
      U_matrix=S

      CALL DIAG(U_matrix,S_DIAG,NORB) 

      do i=1,NORB
               S_DIAG(i,i)=S_DIAG(i,i)**(-0.5d0)
      enddo
       
      !orthogonalization transformation matrix(eigenvectors*eigenvalues)
      X_transfer = matmul(U_matrix,S_DIAG)
      
      !X_transfer transpose get X^+
      X_adjoint =transpose(X_transfer)
        

     !check: ING=x^+*S*x=I
     !ING=matmul(matmul(X_adjoint,S),X_transfer)
     !open(15,file='XSX.txt')
     !write(15,*)'this is transformed basis function orthonormal'
     !do i=1,NORB
     !   write(15,*)ING(i,:)
     !enddo
      guess=2 !!!Set Fock martix initial guess
      call  initial_guess(guess,NORB,X_adjoint,X_transfer,Hcore,S,P_old,Num_ele,E2)
      count_iter=0
      logical_e=.true.
      !E2=0.0d0
      Fock_xx(:,:)=0.0d0 
      !========================
      !  SCF CYCLE
      !========================
      !do i = 1,1
      call cpu_time(t1)
      do while(logical_e)
        count_iter=count_iter+1

        call G_matrix(P_old,U,NORB,G_ij)  
        !call J_and_K(P_old,U,NORB,J_martix,K_martix,G_ij)
        !write(*,*)G_ij
        !stop
        Fock = Hcore + G_ij
        !!!Direct Inversion in the Iterative Subspace(DIIS)
        if(inv_DIIS==1)then !invoke DIIS
                !write(21,*)'call DIIS'
                fockdiis(:,:,mod(count_iter,ndiis)+1)=Fock
                !error matrix:ei=FPS-SPF
                !FPS
                FPS=matmul(matmul(Fock,P_old),S)
                !SPF
                SPF=matmul(matmul(S,P_old),Fock)
                !error_matrix=FPS-SPF
                errmat(:,:,mod(count_iter,ndiis)+1)=FPS-SPF
                !construct B matrix
                if(count_iter>=ndiis)then
                  Bdiis=0.0d0
                  do kp=1,ndiis
                     do kq=1,kp
                        Bdiis(kp,kq)=sum(errmat(:,:,kp)*errmat(:,:,kq))
                     enddo
                  enddo
                Bdiis(ndiis+1,:)=-1.0d0
                Bdiis(ndiis+1,ndiis+1)=0.0d0
                cofdiis=0.0d0
                cofdiis(ndiis+1)=-1.0d0
                !solve New matrix of coefficients
                lwork=-1
                allocate(ipiv(size(cofdiis)))
                do ii = 1,2
                        !lwork=-1
                        allocate(work(abs(lwork)))
                        call dsysv('L', ndiis+1, 1, Bdiis, ndiis+1, ipiv, cofdiis,ndiis+1, work, lwork, ierr)
                        lwork = nint(work(1))
                        deallocate(work)
                end do
                if(ierr/=0)stop 'Solve cofdiis failed!!!'
                deallocate(ipiv)
                !set new fock matrix
                Fock=0.0d0
                do i=1,ndiis
                   Fock=Fock+cofdiis(i)*fockdiis(:,:,i)
                enddo
                endif
        endif
        Fock_x = matmul(X_adjoint,matmul(Fock,X_transfer))
        
        Fock_xx=Fock_x
        !now,Fock_prime output is an eigenvector fock_prime=C'
        call DIAG(Fock_xx,C_prime,NORB)

        !C=X*C' here,X:X_transfer C':eigenvector
        C_new=matmul(X_transfer,Fock_xx)

        !Update the P_density matrix by new coefficient(C_new)
        call P_n_den(C_new,P_new,NORB,Num_ele)
        
        !Calculated energy
        call calc_E0(Hcore,Fock,P_new,NORB,E_ele)
        
        !P_old = P_new
        call E_nuclear(NATOM,GEOM,Zn,E_nu)
        !write(*,*)'hello ' 
        call delta_P(NORB,P_old,P_new,delta)
        if((delta < 1.0E-4).and.(abs(E_ele-E2) < 1.0E-6))then
        !if((abs(E_ele-E2) < 10.0E-6))then
               logical_e=.false.
               write(21,*)"==========================================="
               write(21,*)'Density matrix'
               do i=1,NORB
                  write(21,*)P_new(i,:)
               enddo
               write(21,*)
               write(21,*)'==========================================='
               write(21,*)'Congratulations, SCF has converged!'
               if (inv_DIIS==1)then
               write(21,*)'Call DIIS'
               endif
               write(21,*)'Number of iterations:',count_iter
               write(21,*)'Electronic energy:   ',E_ele
               write(21,*)'Nuclear energy:      ',E_nu
               write(21,*)'Total energy(HF):    ',E_ele+E_nu
               call DIAG_1(Fock_x,orb_energy,NORB)
              ! write(22,*)'G_ij======='
             !do i=1,NORB
             ! write(22,*)P_new(i,:)
             !enddo 
               write(21,*)"Orbital Energy:      "
              do i=1,NORB
               write(21,'(2X,A3,I1,A1,3X,F16.12)')"orb",i,":",orb_energy(i)
              enddo
        endif
        P_old = P_new
        E2 = E_ele
        !write(*,*)'Number of iterations:',count_iter
        !write(*,*)E0_rhf
      enddo
      call cpu_time(t2)
      HF_t = t2-t1
      write(21,*)'HF_TIME:',HF_t
      write(21,*)'==========================================='
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
        write(21,*)CTYPE(I),GEOM(I,1),GEOM(I,2),GEOM(I,3)
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
      !open(23,file='S_diag')
      ! write(23,*)'Diagonalize the over matrix'
      !do i=1,NORB
      !         write(23,*)S_DIAG(i,:)
      !         write(24,*)A(i,:)
      !enddo
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
        
        subroutine J_and_K(P_density,U,NORB,J_martix,K_martix,G_ij)
        implicit none
        !============in===========
        integer i,j,k,l
        integer NORB
        real*8 P_density(NORB,NORB)
        real*8 U(NORB,NORB,NORB,NORB)
        !=============out==========
        real*8 J_martix(NORB,NORB),K_martix(NORB,NORB)
        real*8 G_ij(NORB,NORB)

        J_martix(:,:) = 0.0d0
        K_martix(:,:) = 0.0d0
        do i =1,NORB
           do j=1,NORB
              do k=1,NORB
                 do l=1,NORB
                     !CALL BASIS_INT1(i,j,l,k,NATOM,NORB,GEOM,ATOMCHG,S,Hcore,twoout,baselable) !!subroutine Direct.f90
                     !U(i,j,k,l)=twoout
                     J_martix(i,j) = J_martix(i,j) + P_density(k,l)*U(i,j,l,k)
                     K_martix(i,j) = K_martix(i,j) + 0.5d0*P_density(k,l)*U(i,k,l,j) 
                 enddo
              enddo
           enddo
        enddo
        do i=1,NORB
           do j=1,NORB
           G_ij(i,j) = J_martix(i,j) - K_martix(i,j)
           enddo
        enddo
       endsubroutine 
        
        subroutine G_matrix(P_density,U,NORB,G_ij)
        integer i,j,k,l,NORB
        real*8 U(NORB,NORB,NORB,NORB),G_ij(NORB,NORB)
        real*8 P_density(NORB,NORB)
        real*8 temp
        real*8 doublej,doublek
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
        a = 0.5291772083 !bohr= ai*a
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

