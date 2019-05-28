
    CHARACTER(LEN=180) :: line
    INTEGER :: n
    CHARACTER(LEN=180) :: infile
    CHARACTER(LEN=4) :: ID
   CHARACTER :: icao_id*4
   CHARACTER :: wmo_id*5

   CHARACTER :: cntry_id*2
   CHARACTER :: city_id*3
   CHARACTER :: descrptn*60
   CHARACTER :: cntry_nam*80
   CHARACTER :: region_nam*80
   CHARACTER :: stn_lat*10
   CHARACTER :: stn_lon*10
   CHARACTER :: stn_hgt*4
! icao_id, cntry_id, city_id, lat, lng, height, descrptn, cntry_nam

!   CALL GETARG(1,infile)
    infile='stations.txt'
    OPEN(11,FILE=infile,form='formatted',status='old')
    n=0
  
10  READ(11,'(A)',err=999,end=999) line

    IF(LEN_TRIM(line(35:45)) == 0) THEN
       n=1
    ENDIF

    IF(line(1:1)/='!' .AND. LEN_TRIM(line) >1)THEN

    IF(n ==1 ) THEN
       region_nam=line(1:19)
    ELSE
       icao_id=line(21:24)

       wmo_id =line(33:37)
       IF (LEN_TRIM(wmo_id)==0) wmo_id='-----'
       cntry_id=wmo_id(1:2)
       city_id=wmo_id(3:5)
       descrptn=line(4:17)
       stn_lat=line(40:45)
       stn_lon=line(48:54)
       stn_lat(3:3)='-'
       stn_lon(4:4)='-'
       stn_hgt=line(56:59)
       cntry_nam=line(82:83)
       IF(LEN_TRIM(line(1:2)) /= 0 ) THEN
          cntry_nam=TRIM(region_nam)//' / '//cntry_nam
       ELSE
          cntry_nam=TRIM(region_nam)
       ENDIF
        
       IF(icao_id /= 'ICAO' .AND. stn_hgt /= 'ELEV' ) THEN
          IF(LEN_TRIM(icao_id)==4)write(*,'(A,13(";",A))') icao_id, cntry_id, city_id, &
TRIM(descrptn),'',TRIM(cntry_nam),'',TRIM(stn_lat), TRIM(stn_lon), '','', TRIM(ADJUSTL(stn_hgt)), '',''
       ENDIF
    ENDIF

       n=n+1
    ENDIF

    GOTO 10
999 CONTINUE
    END
