program cippres_prop_collision
  use general
  use propdyn
 ! create a routine for one-e general matrix with determinants

  implicit none
  BEGIN_DOC
! CIPPRES stands for Configuration Interaction Plugin for Photoionized and Resonant Electronic States.
! cippres_prop_collision solves the TDSE using the matrix elements from cippres_collision
  END_DOC

  integer :: nsta_bound
  double precision :: t1, t2, tdyn
  logical :: exists
  integer :: zeroVp, oneVp, twoVp
  integer :: i, j, ib, k, l, it, ni, nf

  PROVIDE ezfio_filename !HF_bitmask mo_coef

write(*,*) "ccjia:prop 0"

  if (mpi_master) then
   call ezfio_has_cippres_n_time(exists)
    if (exists) then
      call ezfio_has_cippres_n_bimp(exists)
      if (exists) then
       call ezfio_has_cippres_tgrid(exists)
       if (exists) then
         call ezfio_has_cippres_zgrid(exists)
         if (exists) then
           call ezfio_has_cippres_bgrid(exists)
            if (exists) then
             call ezfio_has_cippres_v_coll(exists)
             if (exists) then
               call ezfio_has_cippres_i_state_coll(exists)
               if (exists) then
                 call ezfio_has_cippres_stamin_bound(exists)
                 if (exists) then
                   call ezfio_has_cippres_stamax_bound(exists)
                 endif
               endif
             endif
           endif
         endif
       endif
     endif
   endif
 endif

write(*,*) "ccjia:prop 1"

  if (exists) then
write(*,*) "ccjia:prop 111",exists
   call ezfio_get_cippres_n_time(n_time)
write(*,*) "ccjia:prop 111,n_time",exists,n_time
   call ezfio_get_cippres_n_bimp(n_bimp)
write(*,*) "ccjia:prop 111,n_bimp",exists,n_bimp
   call ezfio_get_cippres_zgrid(zgrid)
write(*,*) "ccjia:prop 111,zgrid",exists,zgrid
   call ezfio_get_cippres_tgrid(tgrid)
write(*,*) "ccjia:prop 111,tgrid",exists,tgrid
   call ezfio_get_cippres_bgrid(bgrid)
write(*,*) "ccjia:prop 111,bgrid",exists,bgrid
   call ezfio_get_cippres_stamin_bound(stamin_bound)
write(*,*) "ccjia:prop 111,stamin_bound",exists,stamin_bound
   call ezfio_get_cippres_stamax_bound(stamax_bound)
write(*,*) "ccjia:prop 111,stamax_bound",exists,stamax_bound
   call ezfio_get_cippres_stamin_si(stamin_si)
write(*,*) "ccjia:prop 111,stamin_si",exists,stamin_si
   call ezfio_get_cippres_stamax_si(stamax_si)
write(*,*) "ccjia:prop 111,stamax_si",exists,stamax_si
   call ezfio_get_cippres_stamin_di(stamin_di)
write(*,*) "ccjia:prop 111,stamin_di",exists,stamin_di
   call ezfio_get_cippres_stamax_di(stamax_di)
write(*,*) "ccjia:prop 111,stamax_di",exists,stamax_di
   call ezfio_get_cippres_i_state_coll(i_state_coll)
write(*,*) "ccjia:prop 111,i_state_coll",exists,i_state_coll
   call ezfio_get_cippres_v_coll(v_coll)
write(*,*) "ccjia:prop 111,v_coll",exists,v_coll
   print*, "" 
   print*, "Collision info"
   print*, "" 
   print*,"n_bimp,n_time,i_state_coll,v_coll =", n_bimp, n_time, i_state_coll, v_coll
   print*,"stamin, stamax, nsta_bound =", stamin_bound, stamax_bound, stamax_bound-stamin_bound+1
   print*,"impact. param. =", bgrid
!   print*,"tgrid = ", tgrid
!   print*,"zgrid = ", zgrid
   print*, "" 


   call ezfio_get_cippres_ici1(ici1)
   call ezfio_get_cippres_n_csf_cippres(n_csf_cippres)

   ntime= n_time
   nsave_time= n_time

   nsta_bound = stamax_bound-stamin_bound+1
   nsi  = stamax_si-stamin_si+1
   ndi  = stamax_di-stamin_di+1
   ni = stamin_bound
   nf = stamax_di
   if(stamax_di == 0) then  
      nsi = 0
      ndi = 0 
      ni = stamin_bound
      nf = stamax_bound
   endif
!!!
  nsta = Ndet_total
  !Det_Tar = ntdet
  vproj = v_coll
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

write(1234,*) "nsta,nsta_bound,ni,nf==",nsta,nsta_bound,ni,nf

   allocate(mcoup(ntime,nsta,nsta),timegrid(ntime),esta(nsta),mat(ntime,nsta,nsta))
   allocate(psi(nsta))
   allocate(rmat2intrp(ntime,nsta,nsta),cmat2intrp(ntime,nsta,nsta))
   allocate(matintrp(nsta,nsta))

   !allocate(g_si(1:ntime,1:nsta,1:nsta),g_ddi(1:ntime,1:nsta,1:nsta),g_sdi(1:ntime,1:nsta))
   !allocate(g_si_intrp(1:ntime,1:nsta,1:nsta),g_ddi_intrp(1:ntime,1:nsta,1:nsta),g_sdi_intrp(1:ntime,1:nsta))

   timegrid(1:ntime) = tgrid(1:n_time)
   esta=0.0d0

   open(unit=20,file='Prop_collision.out')
   write(20,*) n_bimp, Ndet_total
!!!
   zeroVp = Ndet_Bound+Ndet_SE+Ndet_DE
   oneVp  = Ndet_Bound+Ndet_SE+Ndet_DE + Ndet_SC
   twoVp  = Ndet_Bound+Ndet_SE+Ndet_DE + Ndet_SC + Ndet_DC
!!!
   !write(21,*) "n_tsta, n_psta, n_tpsta",n_tsta, n_psta, n_tpsta
   write(21,*) "zeroVp, oneVp, twoVp", zeroVp, oneVp, twoVp

   print*,'start dyn'
   do ib = 1, n_bimp

    b_coll = bgrid(ib)
    ib_coll = ib
    touch b_coll

    !write(1234,*) "mat(it,1,1:2),mat(it,2,1:2)"
    mat= dcmplx(0.0D0,0.0D0)
    do it = 1, ntime
      mat(it,1:nsta,1:nsta) = coll_couplings(1:nsta,1:nsta,it)
      !write(1234,*) mat(it,1,1:2),mat(it,3,3:4)
    enddo

!!!!!!!!!!!!!!!!!
    write(21,*) n_bimp, Ndet_total
    write(21,*)"ib,b_coll=",ib,b_coll
    write(21,*)"nsta=",nsta
    !write(21,*) Ndet_Bound, Ndet_SE, Ndet_SC
    write(21,*) Ndet_Bound, Ndet_SE, Ndet_DE, Ndet_SC, Ndet_DC
    n_tsta = Ndet_Bound+Ndet_SE+Ndet_DE
    n_tpsta = Ndet_SC
    n_psta = Ndet_DC
    write(21,*) "n_tsta, n_psta, n_tpsta",n_tsta, n_psta, n_tpsta
    do i=1, nsta !nsta=Ndet_total
      !esta(i)= ABS(coll_sta(i,1))
      !if(i<=ntdet) esta(i)= ABS(coll_couplings(i,i,1))
      !if(i>ntdet) esta(i)= ABS(coll_couplings(i,i,1)) + 0.5D0*v_coll**2
      !esta(i)= real(coll_couplings(i,i,1))
      esta(i) = real(mat(1,i,i))
      !if(i>zeroVp .and. i<=oneVp)  esta(i)= esta(i) + 0.5D0*v_coll**2
      !if(i>oneVp)                  esta(i)= esta(i) + 1.0D0*v_coll**2
      write(21,*) "i=, esta(i)=",i,esta(i)
    end do
!!!!!!!!!!!!!!!!

!    g_si(:,:,:) = 0d0; p_si = 0d0
!    g_ddi(:,:,:) = 0d0; p_sdi = 0d0
!    g_sdi(:,:) = 0d0; p_ddi = 0d0

    mcoup(:,:,:) = dcmplx(0.0D0,0.0D0)
    mcoup(1:ntime,1:nsta,1:nsta) = mat(1:ntime,1:nsta,1:nsta)

    !write(110,'(4(f20.16,1X))') mcoup(:,1,1), mat(:,1,1)
!    write(110,'(2(f20.16,1X))')mcoup(:,1,1)
    !write(110,'(4(f20.16,1X))')mcoup(:,1,1), mat(:,1,1)

    psi(:) = 0.0d0
    write(1234,*) "i_state_coll=",i_state_coll
    psi(i_state_coll) = dcmplx(1.0D0,0.0D0)! 1.0D0
    !psi(1:ntdet)=!ctdet(1:ntdet)
    !psi(1)=0.70711D0
    !psi(2)=0.70711D0

! propagation 
    call cpu_time(t1)
    call dyn
    call cpu_time(t2)
    tdyn = t2-t1
    write(*,*)'DYN takes',tdyn

    write(*,*)bgrid(ib),(cdabs(psi(i))**2,i=1,nsta), sum(cdabs(psi(:))**2),v_coll
    write(20,'(5000(f12.6,1X))')bgrid(ib),(cdabs(psi(i))**2,i=1,nsta), sum(cdabs(psi(:))**2),v_coll

   enddo 

   close(20)

   deallocate(mcoup,timegrid,esta,mat)
   deallocate(matintrp,psi,rmat2intrp,cmat2intrp)
!   deallocate(g_si,g_sdi,g_ddi)
!   deallocate(g_si_intrp,g_sdi_intrp,g_ddi_intrp)

  else

    print*, "Z and/or b grids are not setup correctly"
    stop

  endif

end program cippres_prop_collision


