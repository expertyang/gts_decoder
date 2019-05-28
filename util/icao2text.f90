      program icao2text
      character(len=100) :: stninfo
      character(len=4) :: icao
      integer :: ierr
      open (2, file='gts_sttnid_final.icao',access='direct',recl=100)
         
! from gts_sttnid_final 
      DO I=1, 943499
         read(2, rec=I, iostat=ierr) stninfo
         IF(ierr/=0) EXIT
         icao=stninfo(1:4)
         IF( VERIFY(icao, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789') == 0 ) write(*, '(a)') TRIM(stninfo)
      ENDDO
9999  continue
      end program
