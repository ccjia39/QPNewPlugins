use bitmasks ! you need to include the bitmasks_module.f90 features
use general

 BEGIN_PROVIDER [integer, n_sta_coll_max]
  implicit none
!!!  doc:  maximum number of CSFs for a given CI run (given by the python script generate_csfs.py via the parser.txt file)
!!!  here probably wrong. because we use the total (TT + TP) states, then should be nt+np states. 
     if(n_csf_max<5000) then
        n_sta_coll_max = n_csf_max
     else
        n_sta_coll_max = 5000
     endif
 END_PROVIDER 

 BEGIN_PROVIDER [integer, ib_coll]
  implicit none
   ib_coll = 1
write(*,*) "ccjia:lib,ib_coll",ib_coll
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, b_coll]
  implicit none
!!!! here we give the impact parameter b=0.0
!!!! then we will use: the 1st b equals the 1st b 
!!!! in your input file.
!!!! remember to make the orignal .xml consistent with
!!!! Junwen's integral input .xml file.
   b_coll = 0d0
write(*,*) "ccjia:lib,b_coll",b_coll
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, v_coll]
  implicit none
     call ezfio_get_cippres_v_coll(v_coll)
write(*,*) "ccjia:lib,v_coll",v_coll
 END_PROVIDER 

 BEGIN_PROVIDER [integer, i_state_coll]
  implicit none
     call ezfio_get_cippres_i_state_coll(i_state_coll)
write(*,*) "ccjia:lib,i_state_coll",i_state_coll
 END_PROVIDER 

 BEGIN_PROVIDER [integer, stamin_bound]
  implicit none
     call ezfio_get_cippres_stamin_bound(stamin_bound)
write(*,*) "ccjia:lib,stamin_bound",stamin_bound
 END_PROVIDER 

 BEGIN_PROVIDER [integer, stamax_bound]
  implicit none
     call ezfio_get_cippres_stamax_bound(stamax_bound)
write(*,*) "ccjia:lib,stamax_bound",stamax_bound
 END_PROVIDER 

 BEGIN_PROVIDER [integer, stamin_si]
  implicit none
     call ezfio_get_cippres_stamin_si(stamin_si)
write(*,*) "ccjia:lib,stamin_si",stamin_si
 END_PROVIDER 

 BEGIN_PROVIDER [integer, stamax_si]
  implicit none
     call ezfio_get_cippres_stamax_si(stamax_si)
write(*,*) "ccjia:lib,stamax_si",stamax_si
 END_PROVIDER 

 BEGIN_PROVIDER [integer, stamin_di]
  implicit none
     call ezfio_get_cippres_stamin_di(stamin_di)
write(*,*) "ccjia:lib,stamin_di",stamin_di
 END_PROVIDER 

 BEGIN_PROVIDER [integer, stamax_di]
  implicit none
     call ezfio_get_cippres_stamax_di(stamax_di)
write(*,*) "ccjia:lib,stamax_di",stamax_di
 END_PROVIDER 

 BEGIN_PROVIDER [integer, n_time]
  implicit none
     call ezfio_get_cippres_n_time(n_time)
write(*,*) "ccjia:lib,n_time",n_time
 END_PROVIDER 

 BEGIN_PROVIDER [integer, n_bimp]
  implicit none
     call ezfio_get_cippres_n_bimp(n_bimp)
write(*,*) "ccjia:lib,n_bimp",n_bimp
 END_PROVIDER 

 BEGIN_PROVIDER [integer, n_pcenter]
  implicit none
     call ezfio_get_cippres_n_pcenter(n_pcenter)
write(*,*) "ccjia:lib,n_pcenter",n_pcenter
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, charge_pcenter, (n_pcenter)]
  implicit none
     call ezfio_get_cippres_charge_pcenter(charge_pcenter)
write(*,*) "ccjia:lib,charge_pcenter",n_pcenter
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, bgrid, (n_bimp)]
  implicit none
     call ezfio_get_cippres_bgrid(bgrid)
write(*,*) "ccjia:lib,bgrid",n_bimp
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, zgrid, (n_time)]
  implicit none
     call ezfio_get_cippres_zgrid(zgrid)
write(*,*) "ccjia:lib,zgrid",n_time
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, tgrid, (n_time)]
  implicit none
     call ezfio_get_cippres_tgrid(tgrid)
write(*,*) "ccjia:lib,tgrid",n_time
 END_PROVIDER 

 BEGIN_PROVIDER [integer, ntdet]
&BEGIN_PROVIDER [integer, ntsta]
&BEGIN_PROVIDER [integer, npdet]
&BEGIN_PROVIDER [integer, npsta]
&BEGIN_PROVIDER [integer, mo_num_t]
&BEGIN_PROVIDER [integer, mo_num_p]
&BEGIN_PROVIDER [integer, Ndet_total]
&BEGIN_PROVIDER [integer, elec_alpha_num_t]
&BEGIN_PROVIDER [integer, elec_beta_num_t]
&BEGIN_PROVIDER [integer, elec_alpha_num_p]
&BEGIN_PROVIDER [integer, elec_beta_num_p]
&BEGIN_PROVIDER [integer, Ndet_Bound]
&BEGIN_PROVIDER [integer, Ndet_SE]
&BEGIN_PROVIDER [integer, Ndet_DE]
&BEGIN_PROVIDER [integer, Ndet_SC]
&BEGIN_PROVIDER [integer, Ndet_DC]

!!! here only for the number of np nt .....
  integer :: i, j
  ntdet = 0
  npdet = 0
  write(*,*) "ccjia: ntdet,npdet",ntdet,npdet
  open(unit=10,file='ints/tcistates_det.txt')
   read(10,*) mo_num_t,ntdet,ntsta
   read(10,*) elec_alpha_num_t, elec_beta_num_t
   write(*,*) "ccjia: mo_num_t,ntdet,ntsta",mo_num_t,ntdet,ntsta
   write(*,*) "elec_alpha_num_t, elec_beta_num_t",elec_alpha_num_t, elec_beta_num_t
  close(10)
  open(unit=10,file='ints/pcistates_det.txt')
   read(10,*) mo_num_p,npdet,npsta
   read(10,*) elec_alpha_num_p, elec_beta_num_p
   write(*,*) "ccjia: mo_num_p,npdet,npsta",mo_num_p,npdet,npsta
   write(*,*) "elec_alpha_num_p, elec_beta_num_p",elec_alpha_num_p, elec_beta_num_p
  close(10)
  !write(*,*) "ccjia:ntdet,npdet,00"
  
  open(unit=10,file='ints/det_total.dat')
  read(10,*) Ndet_total,i,j
  read(10,*) Ndet_Bound, Ndet_SE, Ndet_DE, Ndet_SC, Ndet_DC
  !read(10,*) Ndet_Bound, Ndet_SE, Ndet_SC
  close(10)
  write(*,*) "ccjia:Ndet_total=",Ndet_total
  !write(*,*) "ccjia:Ndet_Bound, SE, SC=",Ndet_Bound, Ndet_SE, Ndet_SC
  write(*,*) "ccjia:Ndet_Bound, SE, DE, SC, DC=",Ndet_Bound, Ndet_SE, Ndet_DE, Ndet_SC, Ndet_DC
  !Ndet_total= 1+ (mo_num_t + mo_num_p)*2 - elec_alpha_num_t-elec_beta_num_t
  !nsta=ntsta+npsta
  !write(*,*) "ccjia:Ndet_total= 1+ (mo_num_t + mo_num_p)*2 - elec_alpha_num_t-elec_beta_num_t",Ndet_total

  !touch ntdet ntsta npdet npsta
  !touch mo_num_t mo_num_p Ndet_total
  !touch elec_alpha_num_t elec_beta_num_t elec_alpha_num_p elec_beta_num_p

 END_PROVIDER


! BEGIN_PROVIDER [double precision, coll_w1e_mo, (mo_num,mo_num,n_time,n_bimp)] !nico
 BEGIN_PROVIDER [double complex, coll_w1e_mo, (mo_num_t+mo_num_p,mo_num_t+mo_num_p,n_time,n_bimp)] !ccjia
&BEGIN_PROVIDER [double complex, coll_ov1e_mo, (mo_num_t+mo_num_p,mo_num_t+mo_num_p,n_time,n_bimp)] !ccjia
&BEGIN_PROVIDER [double complex, coll_r12_mo, (mo_num_t+mo_num_p,mo_num_t+mo_num_p,mo_num_t+mo_num_p,mo_num_t+mo_num_p,n_time,n_bimp)
  implicit none

  double precision :: zg, bg
  double precision :: w1ea,w1eb,ov1ea,ov1eb
  double precision :: r12a,r12b
  integer :: i, j, k, l, ib, iz
  integer :: mm,nn,oo,pp
!  integer :: testfile
  write(*,*) "ccjia:","read coll_w1e_mo, coll_ov1e_mo"
! for pp, how to test the lowdin-file.
! mo_num should be careful that  mo_num for Proj, mo_num for Target


open(unit=11,file='ints/onee_int_tt.txt')
open(unit=12,file='ints/onee_int_tp.txt')
open(unit=13,file='ints/onee_int_pt.txt')
open(unit=14,file='ints/onee_int_pp.txt')

  do ib = 1, n_bimp
    read(11,*) bg
    read(12,*) bg
    read(13,*) bg
    read(14,*) bg

    do iz = 1, n_time
      read(11,*) zg
      read(12,*) zg
      read(13,*) zg
      read(14,*) zg

      write(1113,*) "b_coll= ",bg," ib=",ib,","," it/iz=",iz,",","zgrid=",zg," output for coll_w1e_mo(j,i,iz,ib)"
      do i = 1, mo_num_t
        do j = 1, mo_num_t
          read(11,*) k,l,w1ea,w1eb,ov1ea,ov1eb
          coll_w1e_mo(j,i,iz,ib)= dcmplx(w1ea,w1eb)
          coll_ov1e_mo(j,i,iz,ib)= dcmplx(ov1ea,ov1eb)  !!mcoup(izgrid,j,i),movl(izgrid,j,i)
          !write(1113,"(A,2I6,4F20.16)") "j=mo_num, i=mo_num, coll_w1e_and_ov1e__mo(j,i,iz,ib)= ",j,i,coll_w1e_mo(j,i,iz,ib),coll_ov1e_mo(j,i,iz,ib)
        enddo
      enddo

       do i = 1, mo_num_t
         do j = 1, mo_num_p
             read(12,*) k,l,w1ea,w1eb,ov1ea,ov1eb
             coll_w1e_mo(j+mo_num_t,i,iz,ib)= dcmplx(w1ea,w1eb)
             coll_ov1e_mo(j+mo_num_t,i,iz,ib)= dcmplx(ov1ea,ov1eb)
             read(13,*) k,l,w1ea,w1eb,ov1ea,ov1eb
             coll_w1e_mo(i,j+mo_num_t,iz,ib)= dcmplx(w1ea,w1eb)
             coll_ov1e_mo(i,j+mo_num_t,iz,ib)= dcmplx(ov1ea,ov1eb)
         enddo
       enddo

       do i = 1, mo_num_p
         do j = 1, mo_num_p
             read(14,*) k,l,w1ea,w1eb,ov1ea,ov1eb
             coll_w1e_mo(j+mo_num_t,i+mo_num_t,iz,ib)= dcmplx(w1ea,w1eb)
             coll_ov1e_mo(j+mo_num_t,i+mo_num_t,iz,ib)= dcmplx(ov1ea,ov1eb)
         enddo
       enddo

    end do  
  end do

  print*,'read ok'
  close(11)
  close(12)
  close(13)
  close(14)

!!!
! test r12mo rep.
! 
write(*,*) "ccjia: read r12mo, two e int"
open(unit=11,file='ints/twoe_int_tttt.txt')

open(unit=12,file='ints/twoe_int_ptpt.txt')
open(unit=13,file='ints/twoe_int_tppt.txt')
open(unit=14,file='ints/twoe_int_pttp.txt')
open(unit=15,file='ints/twoe_int_tptp.txt')

open(unit=16,file='ints/twoe_int_pttt.txt')
open(unit=17,file='ints/twoe_int_tptt.txt')
open(unit=18,file='ints/twoe_int_ttpt.txt')
open(unit=19,file='ints/twoe_int_tttp.txt')

open(unit=20,file='ints/twoe_int_pptt.txt')
open(unit=21,file='ints/twoe_int_ttpp.txt')

open(unit=22,file='ints/twoe_int_ptpp.txt')
open(unit=23,file='ints/twoe_int_tppp.txt')
open(unit=24,file='ints/twoe_int_pppt.txt')
open(unit=25,file='ints/twoe_int_pptp.txt')

open(unit=26,file='ints/twoe_int_pppp.txt')

  do ib = 1, n_bimp

    read(11,*)bg
    do iz = 1, n_time
      read(11,*)zg
      do i=1, mo_num_t
        do j=1, mo_num_t
          do k=1, mo_num_t
            do l=1, mo_num_t
              read(11,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k,j,i,iz,ib)= dcmplx(r12a,r12b)
            enddo
          enddo
        enddo
      enddo
    end do ! for iz=zgrid(iz)

    read(12,*)bg
    read(13,*)bg
    read(14,*)bg
    read(15,*)bg
    do iz = 1, n_time
      read(12,*)zg
      read(13,*)zg
      read(14,*)zg
      read(15,*)zg
      do i=1, mo_num_p
        do j=1, mo_num_t
          do k=1, mo_num_p
            do l=1, mo_num_t
              read(12,*) mm,nn,oo,pp, r12a,r12b !dcmplx(0.0d0,0.0d0)
              coll_r12_mo(k+mo_num_t,l,i+mo_num_t,j,iz,ib)= dcmplx(r12a,r12b)
              read(13,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k+mo_num_t,i+mo_num_t,j,iz,ib)= dcmplx(r12a,r12b)
              read(14,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(k+mo_num_t,l,j,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
              read(15,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k+mo_num_t,j,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
            enddo
          enddo
        enddo
      enddo
    end do ! for iz=zgrid(iz)

    read(16,*)bg
    read(17,*)bg
    do iz = 1, n_time
      read(16,*)zg
      read(17,*)zg
      do i=1, mo_num_t
        do j=1, mo_num_t
          do k=1, mo_num_p
            do l=1, mo_num_t
              read(16,*) mm,nn,oo,pp, r12a,r12b!dcmplx(0.0d0,0.0d0)
              coll_r12_mo(k+mo_num_t,l,i,j,iz,ib)= dcmplx(r12a,r12b)
              read(17,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k+mo_num_t,i,j,iz,ib)= dcmplx(r12a,r12b)
            enddo 
          enddo
        enddo
      enddo
    end do ! for iz=zgrid(iz)

    read(18,*)bg
    read(19,*)bg
    do iz = 1, n_time
      read(18,*)zg
      read(19,*)zg
      do i=1, mo_num_t
        do j=1, mo_num_p
          do k=1, mo_num_t
            do l=1, mo_num_t
              read(18,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k,j+mo_num_t,i,iz,ib)= dcmplx(r12a,r12b)
              read(19,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k,i,j+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
            enddo 
          enddo
        enddo
      enddo
    end do ! for iz=zgrid(iz)

    read(20,*)bg
    read(21,*)bg
    do iz = 1, n_time
      read(20,*)zg
      read(21,*)zg
      do i=1, mo_num_p
        do j=1, mo_num_p
          do k=1, mo_num_t
            do l=1, mo_num_t
              read(20,*) mm,nn,oo,pp, r12a,r12b !dcmplx(0.0d0,0.0d0)
              coll_r12_mo(j+mo_num_t,i+mo_num_t,l,k,iz,ib)= dcmplx(r12a,r12b)
              read(21,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k,j+mo_num_t,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
            enddo 
          enddo
        enddo
      enddo !!??? 
    end do ! for iz=zgrid(iz)

    read(22,*)bg
    read(23,*)bg
    do iz = 1, n_time
      read(22,*)zg
      read(23,*)zg
      do i=1, mo_num_p
        do j=1, mo_num_p
          do k=1, mo_num_p
            do l=1, mo_num_t
              read(22,*) mm,nn,oo,pp, r12a,r12b!dcmplx(0.0d0,0.0d0)
              coll_r12_mo(k+mo_num_t,l,i+mo_num_t,j+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
              read(23,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l,k+mo_num_t,j+mo_num_t,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
            enddo 
          enddo
        enddo
      enddo!! ???
    end do ! for iz=zgrid(iz)

    read(24,*)bg
    read(25,*)bg
    do iz = 1, n_time
      read(24,*)zg
      read(25,*)zg
      do i=1, mo_num_p
        do j=1, mo_num_t
          do k=1, mo_num_p
            do l=1, mo_num_p
              read(24,*) mm,nn,oo,pp, r12a,r12b !dcmplx(0.0d0,0.0d0)
              coll_r12_mo(l+mo_num_t,k+mo_num_t,i+mo_num_t,j,iz,ib)= dcmplx(r12a,r12b)
              read(25,*) mm,nn,oo,pp, r12a,r12b
              coll_r12_mo(l+mo_num_t,k+mo_num_t,j,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
            enddo
          enddo
        enddo
      enddo
    end do ! for iz=zgrid(iz)

    read(26,*)bg
    do iz = 1, n_time
      read(26,*)zg
      do i=1, mo_num_p
        do j=1, mo_num_p
          do k=1, mo_num_p
            do l=1, mo_num_p
              read(26,*) mm,nn,oo,pp, r12a,r12b! dcmplx(0.0d0,0.0d0)
              coll_r12_mo(l+mo_num_t,k+mo_num_t,j+mo_num_t,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
              !coll_r12_mo(k+mo_num_t,l+mo_num_t,j+mo_num_t,i+mo_num_t,iz,ib)= dcmplx(r12a,r12b)
            enddo
          enddo
        enddo
      enddo
    end do ! for iz=zgrid(iz)

  end do  ! for para b

  print*,'read ok'

  close(11)
  close(12)
  close(13)
  close(14)
  close(15)
  close(16)
  close(17)
  close(18)
  close(19)
  close(20)
  close(21)
  close(22)
  close(23)
  close(24)
  close(25)
  close(26)

 END_PROVIDER 


 BEGIN_PROVIDER [integer, detalpha, (elec_alpha_num_t+elec_alpha_num_p+1,Ndet_total)]
&BEGIN_PROVIDER [integer, detbeta, (elec_beta_num_t+elec_beta_num_p+1,Ndet_total)]
&BEGIN_PROVIDER [integer, tdeta, (elec_alpha_num+1,ntdet)]
&BEGIN_PROVIDER [integer, tdetb, (elec_beta_num+1,ntdet)]
&BEGIN_PROVIDER [double precision, ctdet, (ntdet)]
&BEGIN_PROVIDER [double precision, tci_e, (ntsta)]
&BEGIN_PROVIDER [double precision, tci_sta, (ntdet,ntsta)]
&BEGIN_PROVIDER [integer, pdeta, (elec_alpha_num+1,npdet)]
&BEGIN_PROVIDER [integer, pdetb, (elec_beta_num+1,npdet)]
&BEGIN_PROVIDER [double precision, cpdet, (npdet)]
&BEGIN_PROVIDER [double precision, pci_e, (npsta)]
&BEGIN_PROVIDER [double precision, pci_sta, (npdet,npsta)]
 integer :: i, j ,k, l
 integer :: alp, bet


 detalpha(:,:) = 0
 detbeta(:,:) = 0
 tdeta(:,:) = 0
 tdetb(:,:) = 0
 ctdet(:) = 0.0d0
 pdeta(:,:) = 0
 pdetb(:,:) = 0
 cpdet(:) = 0.0d0


 open(unit=10,file='ints/tcistates_det.txt')
    read(10,*)i,j,k
    read(10,*)i,j
  do i = 1, ntdet
    write(*,*) "ntdet=",ntdet
    !write(*,*) "ccjia:1",ctdet(i), (tdeta(j,i),j=1,elec_alpha_num_t), (tdetb(j,i),j=1,elec_beta_num_t)
    read(10,*)ctdet(i), (tdeta(j,i),j=1,elec_alpha_num_t), (tdetb(j,i),j=1,elec_beta_num_t)
    !write(*,*) "ccjia:2",ctdet(i), (tdeta(j,i),j=1,elec_alpha_num_t), (tdetb(j,i),j=1,elec_beta_num_t)
  enddo 
  do i = 1, ntsta
    read(10,*)tci_e(i)
    read(10,*)(tci_sta(j,i),j=1,ntdet)
  enddo 
 close(10)

 open(unit=10,file='ints/pcistates_det.txt')
    read(10,*)i,j,k
    read(10,*)i,j
  do i = 1, npdet
    read(10,*) cpdet(i), (pdeta(j,i),j=1,elec_alpha_num_p), (pdetb(j,i),j=1,elec_beta_num_p)
  enddo
  read(10,*) 
  do i = 1, npsta
    read(10,*)pci_e(i)
    read(10,*)(pci_sta(j,i),j=1,npdet)
  enddo 
 close(10)

!!!! to define the total determinants based on the mo_t+mo_p
  open(unit=10,file='ints/det_total.dat')
  read(10,*) i,alp,bet
  write(*,*) "Ndet_total,alp_ele,bet_ele",i,alp,bet
  read(10,*) 
  do i = 1, Ndet_total
    read(10,*) (detalpha(j,i),j=1,alp), (detbeta(j,i),j=1,bet)
    write(*,*) (detalpha(j,i),j=1,alp), (detbeta(j,i),j=1,bet)
  enddo
  close(10)

!  do j=1, Ndet_total
!    do i=1, elec_alpha_num_t-1
!      detalpha(i,j)= i
!    end do
!    do k=1, elec_beta_num_t-1
!      detbeta(k,j)= k
!    end do
!  end do
!  detalpha(elec_alpha_num_t,1)= elec_alpha_num_t
!  detbeta(elec_beta_num_t,1)= elec_beta_num_t
!  write(111,"(A5,I5,5X,5000I3)")  "i= ",1,(detalpha(l,1),l=1,elec_alpha_num_t),(detbeta(l,1),l=1,elec_beta_num_t)
!  write(*,"(A5,I5,5X,5000I3)")  "i= ",1,(detalpha(l,1),l=1,elec_alpha_num_t),(detbeta(l,1),l=1,elec_beta_num_t)
!  k=elec_alpha_num_t
!  do i=2, Ndet_total-1, 2
!    detalpha(elec_alpha_num_t,i)= elec_alpha_num_t
!    detbeta(elec_beta_num_t,i)= k + 1
!    write(111,"(A5,I5,5X,5000I3)")  "i= ",i,(detalpha(l,i),l=1,elec_alpha_num_t),(detbeta(l,i),l=1,elec_beta_num_t)
!    write(*,"(A5,I5,5X,5000I3)")  "i= ",i,(detalpha(l,i),l=1,elec_alpha_num_t),(detbeta(l,i),l=1,elec_beta_num_t)
!    detalpha(elec_alpha_num_t,i+1)= k + 1
!    detbeta(elec_beta_num_t,i+1)= elec_alpha_num_t
!    write(111,"(A5,I5,5X,5000I3)")  "i= ",i+1,(detalpha(l,i+1),l=1,elec_alpha_num_t),(detbeta(l,i+1),l=1,elec_beta_num_t)
!    write(*,"(A5,I5,5X,5000I3)")  "i= ",i+1,(detalpha(l,i+1),l=1,elec_alpha_num_t),(detbeta(l,i+1),l=1,elec_beta_num_t)
!    k=k+1
!  end do



 END_PROVIDER 

 BEGIN_PROVIDER [double complex, coll_couplings, (Ndet_total,Ndet_total,n_time)]
&BEGIN_PROVIDER [double precision, coll_sta, (Ndet_total,n_time)]
 !!  to use the Ndet_total as the all states. remove the n_csf_max. 
 use general
 use SlaterDeterminant
 implicit none
 integer :: i, j, k, l, imo
 integer :: ib, ic, it
 
 double complex, allocatable :: coll_csf_mat_M(:,:), coll_csf_mat_S(:,:), csf_mat(:)
 double complex, allocatable :: coll_csf_mat(:,:)

 double precision, allocatable :: eigval1(:),eigvec1(:,:),eigval2(:),eigvec2(:,:)!,coll_csf_mat(:,:)!,coll_mat(:,:), mattmp(:,:)

 double complex, dimension(mo_num_t+mo_num_p,mo_num_t+mo_num_p) :: w1e !ccjia
 double complex, dimension(mo_num_t+mo_num_p,mo_num_t+mo_num_p) :: ovmo !ccjia
 double complex, dimension(mo_num_t+mo_num_p,mo_num_t+mo_num_p,mo_num_t+mo_num_p,mo_num_t+mo_num_p) :: r12mo !ccjia
 double complex :: ov, h1e, r12

 integer :: ne, nea, neb, n_mo
 integer :: nsta, ncsf

 double precision :: t1, t2
 logical :: exists

 integer :: LWORK, INFO
 integer, dimension(:), allocatable :: IPIV
 double complex, dimension(:), allocatable ::  WORK
 double complex, allocatable :: matS1(:,:)
 double complex, allocatable :: MMM1(:,:), MMM2(:,:)
 double precision, allocatable :: Rwork(:)

 PROVIDE ezfio_filename !HF_bitmask mo_coef

!! TO BE DONE:  read all integrals and store them in single matrices 1 -> (ntdet+npdet)
!!              read eigevec from cistates_det.txt which should be written manually to incorporate both target and proj. MOs
!!              initiate psi with target CI coeff.
!!              test lowdin rules code (ok for tt-1e ints.)

   print*,'Computing coll_couplings', b_coll 
   call cpu_time(t1)
   coll_sta(:,:)=0.0d0
   coll_couplings(:,:,:) = dcmplx(0.0D0,0.0D0)

   allocate(coll_csf_mat_M(1:Ndet_total,1:Ndet_total))
   allocate(coll_csf_mat_S(1:Ndet_total,1:Ndet_total))
   allocate(coll_csf_mat(1:Ndet_total,1:Ndet_total))
 !  allocate(csf_mat(1:Ndet_total))


   !allocate(eigval1(1:ntsta+npsta),eigval2(1:ntsta+npsta))
   !eigval1(1:ntsta) = 0.0d0
   !eigval2(1:ntsta) = 0.0d0
   !eigval1(1:ntsta) = tci_e(1:ntsta) ! tci_e(:)
   !eigval2(1:ntsta) = tci_e(1:ntsta) ! tci_e(:)
   !eigval1(ntsta+1:ntsta+npsta) = pci_e(1:npsta) ! pci_e(:) 
   !eigval2(ntsta+1:ntsta+npsta) = pci_e(1:npsta) ! pci_e(:)

   !allocate(eigvec1(1:Ndet_total,1:ntsta))
   !allocate(eigvec2(1:Ndet_total,1:ntsta))
   
   !eigvec1(:,:) = 0.0d0
   !eigvec2(:,:) = 0.0d0

   !eigvec1(1:ntdet,1:ntsta) = tci_sta(1:ntdet,1:ntsta) ! tci_sta(:,:)
   !eigvec2(1:ntdet,1:ntsta) = tci_sta(1:ntdet,1:ntsta) ! tci_sta(:,:)

   !nsta = ntsta + npsta
   nsta = Ndet_total
   if(nsta>8000) then
    write(*,*) "n total state or det. =",nsta, "nsta > n_sta_coll_max=8000, I stop"
    print*, "use less states......"
    stop
   endif
   write(*,*) "nsta, ntsta, npsta(wrong)",nsta, ntsta, npsta
   ncsf = Ndet_total


! here, 'it' means the zGrid.
  do it = 1, n_time

   w1e(:,:) = coll_w1e_mo(:,:,it,ib_coll)
   ovmo(:,:)=coll_ov1e_mo(:,:,it,ib_coll)
   r12mo(:,:,:,:)=coll_r12_mo(:,:,:,:,it,ib_coll)
   !r12mo(1:mo_num_t,1:mo_num_t,1:mo_num_t,1:mo_num_t)=coll_r12_mo(1:mo_num_t,1:mo_num_t,1:mo_num_t,1:mo_num_t,it,ib_coll)



   coll_csf_mat_M(:,:) = dcmplx(0.0D0,0.0D0)
   coll_csf_mat_S(:,:) = dcmplx(0.0D0,0.0D0)
   coll_csf_mat(:,:) = dcmplx(0.0D0,0.0D0)
   !csf_mat(:) = 0d0

  ! touch ntdet

   nea = elec_alpha_num_t+elec_alpha_num_p !!! consider the T and P=0 ????
   neb = elec_beta_num_t+elec_beta_num_p !!! consider the T and P=0 ????
   ne = nea + neb
   n_mo = mo_num_t+mo_num_p !+mo_num  !! consider the T and P
   h1e = dcmplx(0.0D0,0.0D0)!0.0d0
   ov  = dcmplx(0.0D0,0.0D0)!0.0d0
   r12 = dcmplx(0.0D0,0.0D0)!0.0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !r12mo(:,:,:,:) = dcmplx(0.0D0,0.0D0)!0.0d0
!!! for r12mo, its definition
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !write(1114,*) "b_coll=,",b_coll," it,  zgrid(it)= ",it,zgrid(it)
   !write(11140,*) "b_coll=,",b_coll," it,  zgrid(it)= ",it,zgrid(it)
    do i=1,Ndet_total  ! 
      do j=1,Ndet_total   !
        call lowdin(ne,nea,neb,n_mo,ovmo,w1e,r12mo,detalpha(:,i),detalpha(:,j),detbeta(:,i),detbeta(:,j),ov,h1e,r12)
        coll_csf_mat_M(j,i) = h1e + r12 !real(h1e)!*DBLE(i+j)*0.01D0
        coll_csf_mat_S(j,i) = ov !real(ov)!*DBLE(i+j)*0.02D0
        !coll_csf_mat(j,i) = DBLE(i+j)*0.01D0
        !write(1114,"(A,2I6,100F25.16)") "Ndet_j=, Ndet_i=, coll_csf_mat(j,i)= ",j,i,coll_csf_mat_M(j,i), coll_csf_mat_S(j,i)
        !write(11140,"(A,2I6,100F25.16)") "Ndet_j=, Ndet_i=, h1e,ov,r12= ",j,i, h1e,ov,r12
      enddo
    enddo
!!! compute the S^(-1) and also the S^(-1) * H

   allocate(matS1(1:Ndet_total,1:Ndet_total))
   allocate(IPIV(1:Ndet_total))
   IPIV=0
   
   matS1(:,:) = coll_csf_mat_S(:,:)
   LWORK = 3*Ndet_total
   allocate(WORK(LWORK))
   WORK=dcmplx(0.0D0,0.0D0)
   CALL ZGETRF( Ndet_total, Ndet_total, matS1(:,:), Ndet_total, IPIV, INFO )
   write(*,*) "CALL ZGETRF, INFO=",INFO
   CALL ZGETRI( Ndet_total, matS1(:,:), Ndet_total, IPIV, WORK, LWORK, INFO )
   write(*,*) "CALL ZGETRI, INFO=",INFO
   !coll_csf_mat(:,:)= matmul(matS1,coll_csf_mat_S)
   !do i=1, Ndet_total
     !write(9991,*) "i=,coll_csf_mat(i,all)=", i, coll_csf_mat(i,1:Ndet_total)
   !end do
   coll_csf_mat(:,:)= matmul(matS1(:,:),coll_csf_mat_M(:,:))
   !CALL ZGEEV( 'N', 'V', Ndet_total, M0, Ndet_total, M1, M2, M3, Ndet_total, M4, Ndet_total, M5, 8*Ndet_total, INFO )
   !do i=1,Ndet_total  ! 
    ! do j=1,Ndet_total   !
       !write(11142,"(A,2I6,100F25.16)") "Ndet_j=, Ndet_i=, matS1(j,i)= ",j,i, matS1(j,i)
     !  write(11141,"(A,2I6,100F25.16)") "Ndet_j=, Ndet_i=, coll_csf_mat_S(j,i),coll_csf_mat(j,i)= ",j,i, coll_csf_mat_S(j,i), coll_csf_mat(j,i)
     !enddo
   !enddo
   deallocate(matS1,WORK,IPIV)

     coll_couplings(1:Ndet_total,1:Ndet_total,it) = coll_csf_mat(1:Ndet_total,1:Ndet_total)

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!
     !LWORK = 4*Ndet_total
     !allocate(Rwork(LWORK),WORK(LWORK))
     !allocate(csf_mat(1:Ndet_total))
     !allocate(MMM1(1:Ndet_total,1:Ndet_total),MMM2(1:Ndet_total,1:Ndet_total))
     !csf_mat = dcmplx(0.0D0,0.0D0)
     !CALL ZGEEV( 'N', 'N', Ndet_total, coll_csf_mat, Ndet_total, csf_mat, MMM1, Ndet_total, MMM2, Ndet_total, WORK, 4*Ndet_total, Rwork, INFO )
     !!write(*,*) "CALL ZGEEV, INFO=",INFO
     !coll_sta(1:Ndet_total,it) = csf_mat(1:Ndet_total)
     !deallocate(Rwork,WORK,csf_mat)
     !deallocate(MMM1,MMM2)
!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!

   enddo

   call cpu_time(t2)
   print*,t2-t1
   print*,' '
 deallocate(coll_csf_mat,coll_csf_mat_S,coll_csf_mat_M)
 !deallocate(eigval1,eigval2,eigvec1,eigvec2)

 END_PROVIDER

