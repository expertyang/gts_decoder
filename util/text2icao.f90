      program text2icao
      character(len=100) :: stninfo
      INTEGER :: key, icao_key
      
      open (1, file='gts_sttnid_final.icao.txt')
      open (2, file='gts_sttnid_final.icao',access='direct',recl=100, status='unknown')
         
! from gts_sttnid_final
      stninfo=repeat(CHAR(0),100) 
      DO I=1,943499
      	write(2,rec=I) stninfo 
      ENDDO
      
100   read(1, '(a)', end=9999, err=9999) stninfo
      key=icao_key(stninfo(1:4))
      write(2, rec=key) stninfo
      goto 100
9999  continue
      end program

FUNCTION icao_key ( id )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( IN ) :: id
   INTEGER                              :: icao_key, ia
!  INTEGER, PARAMETER     :: imax = 26*36*36*36

   INTEGER, PARAMETER     :: imax = 943499       ! last station 10AK
   ! imax also corresponds to number of 100-byte record in gts_sttnid_final.icao
 
   CHARACTER ( LEN = 36 ) :: cset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

!  print *,' id = ',id,' imax = ', imax

   ia = VERIFY ( id, cset )
   if ( ia .NE. 0 ) THEN
      icao_key = 0
   else
      icao_key = (SCAN(cset,id(1:1))-1)*26*36*36 + &
                 (SCAN(cset,id(2:2))-1)   *36*36 + &
                 (SCAN(cset,id(3:3))-1)      *36 + &
                 (SCAN(cset,id(4:4))-1)          + 1
   ENDIF
   
   if ( icao_key .GT. imax ) icao_key = 0

!  print *,' icao_key = ',icao_key

ENDFUNCTION icao_key
