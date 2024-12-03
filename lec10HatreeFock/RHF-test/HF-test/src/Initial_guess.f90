        subroutine initial_guess(guess,NORB,X,X_T,Hcore,S,Den,Nele,E_ele)
        implicit none  
        integer :: i,j
        integer :: guess    !!!Which first guess to use
        integer :: NORB,Nele  
        real*8  :: X(NORB,NORB),X_T(NORB,NORB)
        real*8  :: F_guess(NORB,NORB),S(NORB,NORB)
        real*8  :: Hcore(NORB,NORB),eigv(NORB,NORB)
        real*8  :: Den(NORB,NORB),Fock(NORB,NORB)    
        real*8  :: tem(NORB,NORB),Fock_x(NORB,NORB)
        real*8  :: Fock_xx(NORB,NORB),C_new(NORB,NORB)
        real*8  :: C_prime(NORB,NORB)
        real*8  :: E_ele

!!!!  Build the Initial Guess Density using Hcore.
        if(guess==1)then
!!! core Hamiltonian  Fock=Hcore
            F_guess= Hcore
            write(21,*)'Guess: core Hamiltonian'
        else if(guess==2)then
!!!ref:J. Chem. Theory Comput. 2019, 15, 1593−1604
!!! generalized Wolfsberg−Helmholz (GWH) 
!!! guess(i,j)=0.5*K*(Hcore(i,i)+Hcore(j,j))*S(i,j),K=1.75 typically
            write(21,*)'Guess: Wolfsberg−Helmholz'
            do i=1,NORB
                do j=1,i
                    if(i==j)then
                        F_guess(i,i)=Hcore(i,i)
                    else
                       F_guess(i,j)=0.5d0*1.75d0*S(i,j)*(Hcore(i,i)+Hcore(j,j))
                       F_guess(j,i)=F_guess(i,j)
                    end if
                end do
            end do
!!!!            
        else if(guess==3)then
        write(21,*)'Guess: Huckel'
            tem=Hcore
             call DIAG(tem,eigv,NORB)
            do i=1,NORB
                do j=1,i
                    if(i==j)then
                        F_guess(i,i)=-eigv(i,i)
                    else
                       F_guess(i,j)=0.5d0*1.75d0*S(i,j)*(Hcore(i,i)+Hcore(j,j))
                       F_guess(j,i)=F_guess(i,j)
                    end if
                end do
            end do
        end if
        Fock=F_guess
        tem=0.0d0
        tem=Fock
        Fock_x = matmul(X,matmul(tem,X_T))
        Fock_xx=Fock_x
        !now,Fock_prime output is an eigenvector fock_prime=C'
        call DIAG(Fock_xx,C_prime,NORB)

        !C=X*C' here,X:X_transfer C':eigenvector
        C_new=matmul(X,Fock_xx)

        !Update the P_density matrix by new coefficient(C_new)
        call P_n_den(C_new,Den,NORB,Nele)

        !Calculated energy
        call calc_E0(Hcore,Fock,Den,NORB,E_ele)
        endsubroutine initial_guess 
