SUBROUTINE wr_txt_fm12
   USE record_def
   IMPLICIT NONE
   
   TYPE ( stack ) , POINTER  :: rtmp

   REAL              :: lat, lon, elv, slp, ps, t, td, r0, r1, r3, r6, r12, r24, vis, lch, dir, spd, p3t, p24t, t24t, tmax, tmin, tminn, sst
   INTEGER           :: fm, iwrite, obs_yymmdd, obs_hhmmss, rev_yymmdd, rev_hhmmss, nvalid, ww, w1, w2, cl, cm, ch, tcc
   LOGICAL           :: used, w1in
   CHARACTER(LEN=80) :: name
   CHARACTER(LEN=5)  :: id
   
   REAL, PARAMETER   :: miss = -9999.0
   
   CALL assign_outfile ( 5 )
   rtmp => record

   iwrite = outfiles ( record_fm+500 )
   
   CALL clean_txt()
   DO WHILE ( ASSOCIATED ( rtmp ) )

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+500 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        CALL write_txt()
                        CALL clean_txt()
                         
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     CASE('Rev. Obse'); used = .TRUE.
                        nvalid = nvalid + 1
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Platform '); used = .TRUE.
                        nvalid = nvalid + 1
!                        hdr%location%id = '-6666'
!                        hdr%location%name = rtmp%field_cval(12:)
 
     CASE('SHIP or T'); used = .TRUE.
                        nvalid = nvalid + 1
!                        hdr%location%id = '-7777'
!                        hdr%location%name = rtmp%field_cval(12:)
 
     CASE('Latitude '); used = .TRUE.
                        lat = rtmp%field_rval/100.
 
     CASE('Longitude'); used = .TRUE.
                        lon = rtmp%field_rval/100.
                        IF ( fm .EQ. 12 ) THEN
                           id = rtmp%field_cval(6:10)
                           name = rtmp%field_cval(35:rlen)
                        ENDIF

     !---------------------------------------------------------------
     ! Source information

     CASE('Elevation'); used = .TRUE.
                        elv = rtmp%field_rval
 
     !---------------------------------------------------------------
     ! Terrestrial information
     !     slp*f, ref_pres*f, ground_t*f, sst*f, psfc*f,
     !     precip*f, t_max*f, t_min*f, p_tend03*f,
     !     cloud_low*f, cloud_med*f, cloud_hi*f, ceiling*f

     CASE('Mean SLP '); used = .TRUE.
                        nvalid = nvalid + 1
                        slp = rtmp%field_rval
 
     ! Decoded Ground Temperature look very strange, skipping it
   ! CASE('Ground / '); used = .TRUE.
   !                    nvalid = nvalid + 1
   !                    hdr%ground%ground_t%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Sea Surfa'); used = .TRUE.
                        nvalid = nvalid + 1
                        sst = rtmp%field_rval 

     CASE('Station p'); used = .TRUE.
                        nvalid = nvalid + 1
                        ps = rtmp%field_rval

     CASE('QNH value'); used = .TRUE.
                        nvalid = nvalid + 1
                        ps = rtmp%field_rval

     CASE('Precip. a'); used = .TRUE.
                        nvalid = nvalid + 1
                        SELECT CASE( rtmp%field_ival)
                        CASE(1)
                           r1  = rtmp%field_rval
                        CASE(3)
                           r3  = rtmp%field_rval
                        CASE(6)
                           r6  = rtmp%field_rval
                        CASE(12)
                           r12 = rtmp%field_rval
                        CASE(24)
                           r24 = rtmp%field_rval
                        CASE DEFAULT
!                           r1=0.0
                        END SELECT

     CASE('Precip. ='); used = .TRUE.
                        nvalid = nvalid + 1
                        r0 = 0.0

     CASE('Max Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        tmax = rtmp%field_rval
                         
     CASE('Min Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        tmin = rtmp%field_rval
 
     CASE('Nght Tmin'); used = .TRUE.
                        nvalid = nvalid + 1
                        tminn = rtmp%field_rval

     CASE('Pre. Tend'); used = .TRUE.
                        nvalid = nvalid + 1
                        p3t = rtmp%field_rval
 
     CASE('24-h Pre.'); used = .TRUE.
                        nvalid = nvalid + 1
                        p24t = rtmp%field_rval

     CASE('24-h Temp'); used = .TRUE.
                        nvalid = nvalid + 1
                        t24t = rtmp%field_rval

 
     CASE('Total Clo'); used = .TRUE.
                        nvalid = nvalid + 1
                        tcc = rtmp%field_rval
 
     CASE('Lowest cl'); used = .TRUE.
                        nvalid = nvalid + 1
                        lch = rtmp%field_rval

     CASE('Present W'); used = .TRUE.
                        nvalid = nvalid + 1
                        ww = rtmp%field_ival
                        
     CASE('Past Wea.'); used = .TRUE.
                        nvalid = nvalid + 1
                        IF(w1in)THEN
                           w2 = rtmp%field_ival
                           w1in = .FALSE.
                        ELSE
                           w1 = rtmp%field_ival
                           w1in = .TRUE.
                        ENDIF

     CASE('Visibilit'); used = .TRUE.
                        nvalid = nvalid + 1
                        vis = rtmp%field_rval
                        
                        
!---------------------------------------------------------------
! Meteorological information
     CASE('Temperatu'); used = .TRUE.
                        t = rtmp%field_rval
                        nvalid = nvalid + 1
                        
     CASE('Air Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        t = rtmp%field_rval
 
     CASE('Dew Point'); used = .TRUE.
                        td = rtmp%field_rval
 
     CASE('Wind dire'); used = .TRUE.
                        nvalid = nvalid + 1
                        dir = rtmp%field_rval
 
     CASE('Wind spee'); used = .TRUE.
                        nvalid = nvalid + 1
                        spd = rtmp%field_rval

     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT
    rtmp => rtmp%next_record

   ENDDO
   CALL write_txt()

CONTAINS
   SUBROUTINE write_txt()
   IMPLICIT NONE
   REAL, PARAMETER :: missing_r=missing,   undefine_r=undefined
   REAL, PARAMETER :: missing_i=missing_r, undefine_i=undefine_r
   
   IF(elv   == missing_r .OR. elv   == undefine_r)elv   =miss
   IF(slp   == missing_r .OR. slp   == undefine_r)slp   =miss
   IF(ps    == missing_r .OR. ps    == undefine_r)ps    =miss
   IF(t     == missing_r .OR. t     == undefine_r)t     =miss
   IF(td    == missing_r .OR. td    == undefine_r)td    =miss
   IF(spd   == missing_r .OR. spd   == undefine_r)spd   =miss
   IF(dir   == missing_r .OR. dir   == undefine_r)dir   =miss
   IF(tmax  == missing_r .OR. tmax  == undefine_r)tmax  =miss
   IF(tmin  == missing_r .OR. tmin  == undefine_r)tmin  =miss
   IF(tminn == missing_r .OR. tminn == undefine_r)tminn =miss
   IF(sst   == missing_r .OR. sst   == undefine_r)sst   =miss
   IF(vis   == missing_r .OR. vis   == undefine_r)vis   =miss
   IF(tcc   == missing_r .OR. tcc   == undefine_r)tcc   =miss
   IF(lch   == missing_r .OR. lch   == undefine_r)lch   =miss
   IF(r0    == missing_r .OR. r0    == undefine_r)r0    =miss
   IF(r1    == missing_r .OR. r1    == undefine_r)r1    =miss
   IF(r3    == missing_r .OR. r3    == undefine_r)r3    =miss
   IF(r6    == missing_r .OR. r6    == undefine_r)r6    =miss
   IF(r12   == missing_r .OR. r12   == undefine_r)r12   =miss
   IF(r24   == missing_r .OR. r24   == undefine_r)r24   =miss
   IF(p3t   == missing_r .OR. p3t   == undefine_r)p3t   =miss
   IF(p24t  == missing_r .OR. p24t  == undefine_r)p24t  =miss
   IF(t24t  == missing_r .OR. t24t  == undefine_r)t24t  =miss
   IF(ww    == missing_i .OR. ww    == undefine_r)ww    =miss
   IF(w1    == missing_i .OR. w1    == undefine_r)w1    =miss
   IF(w2    == missing_i .OR. w2    == undefine_r)w2    =miss
   
   IF(lat/=miss)THEN
      WRITE(iwrite,"(I8,I6,A6,2F8.2,F8.1,A24,20F10.3,4I6)") rev_yymmdd, rev_hhmmss, id, lat, lon, elv, name(1:24), &
         slp, ps, t, td, spd, dir, tmax, tmin, sst, vis, lch, r0, r1, r3, r6, r12, r24, p3t, p24t, t24t, ww, w1, w2, tcc
   ENDIF
   END SUBROUTINE
   
   SUBROUTINE clean_txt()
   IMPLICIT NONE
   id    ="-9999"
   lat   =miss
   lon   =miss
   elv   =miss
   name  =""
   slp   =miss
   ps    =miss
   t     =miss
   td    =miss
   spd   =miss
   dir   =miss
   tmax  =miss
   tmin  =miss
   tminn =miss
   sst   =miss
   vis   =miss
   tcc   =miss
   lch   =miss
   r0    =miss
   r1    =miss
   r3    =miss
   r6    =miss
   r12   =miss
   r24   =miss
   p3t   =miss
   p24t  =miss
   t24t  =miss
   ww    =miss
   w1    =miss
   w2    =miss
   END SUBROUTINE
   
END SUBROUTINE wr_txt_fm12
