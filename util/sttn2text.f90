      program sttn2text
      character(len=100) :: stninfo
      INTEGER :: gid, ierr
      open (2, file='gts_sttnid_final.wmo',access='direct',recl=100)
         
! from gts_sttnid_final 
      DO I=1, 99999
         read(2, rec=I, iostat=ierr) stninfo
         IF(ierr/=0) EXIT
         read(stninfo(1:5), *) gid
         IF( gid /= 99999 ) write(*, '(a)') TRIM(stninfo)
      ENDDO
9999  continue
      end program
