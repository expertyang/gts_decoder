      program packicao
      character(len=100) :: stninfo
      INTEGER :: gid
      INTEGER :: index

      open (2, file='gts_sttnid_final.icao',access='direct',recl=100)
      open (3, file='gts_sttnid_final.icao.idx',access='direct',recl=4,  status='unknown')
      open (4, file='gts_sttnid_final.icao.dat',access='direct',recl=100,status='unknown')
         
      index=-1
      DO I=1, 943499
         WRITE(3,rec=I) index
      ENDDO
! from gts_sttnid_final 
      index=1
      DO I=1, 943499
         read(2, rec=I) stninfo
         IF(  VERIFY(stninfo(1:4), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789') == 0 ) THEN
            WRITE(3, rec=I) index
            write(4, rec=index) stninfo
            index=index+1
         ENDIF
      ENDDO
      end program
