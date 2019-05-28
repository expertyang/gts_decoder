      program text2sttn
      character(len=100) :: stninfo
      INTEGER :: gid
      open (1, file='gts_sttnid_final.txt')
      open (2, file='gts_sttnid_final.wmo',access='direct',recl=100, status='unknown')
         
! from gts_sttnid_final
      stninfo=repeat('9',100) 
      DO I=1,99999
      	write(2,rec=I) stninfo 
      ENDDO
100   read(1, '(a)', end=9999, err=9999) stninfo

      read(stninfo(1:5), *) gid
      write(2, rec=gid) stninfo
      goto 100
9999  continue
      end program
