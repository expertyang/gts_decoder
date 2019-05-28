      program packsttn
      character(len=100) :: stninfo
      INTEGER :: gid
      INTEGER :: index

      open (2, file='gts_sttnid_final.wmo',access='direct',recl=100)
      open (3, file='gts_sttnid_final.wmo.idx',access='direct',recl=4,  status='unknown')
      open (4, file='gts_sttnid_final.wmo.dat',access='direct',recl=100,status='unknown')
         
      index=-1
      DO I=1, 99999
         WRITE(3,rec=I) index
      ENDDO
! from gts_sttnid_final 
      index=1
      DO I=1, 99999
         read(2, rec=I) stninfo
         read(stninfo(1:5), *) gid
         IF( gid /= 99999 ) THEN
            WRITE(3, rec=I) index
            write(4, rec=index) stninfo
            index=index+1
         ENDIF
      ENDDO
      end program
