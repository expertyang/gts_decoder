
SUBROUTINE wr_rap_fm35

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used
   
   CHARACTER :: part

   IF ( .NOT. FOR_RAP ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rap_fm35 : in '

   IF ( FOR_DEBUG ) THEN
      CALL assign_outfile ( 789 )
   ELSE
      CALL assign_outfile ( 7 )
   ENDIF

   iwrite = outfiles ( record_fm+700 )

   rtmp => record

   obs_yymmdd = missing
   obs_hhmmss = missing

   hdr    = empty_hdr
   upa    = empty_upa
   ieor   = 0
   nvalid = 1
   hdr%info%source = 'GTS (ROHK) ' // msg_header
   hdr%info%is_sound = .TRUE.
   wrote_hdr = .FALSE.
   vert_id = MISSING
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+700 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        IF ( wrote_hdr ) THEN
                           CALL write_rpt_upa ( iwrite , upa , ieor ) 
                           CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
                           upa = empty_upa
                        ENDIF

                        part = rtmp%field_cval(26:26)
                        
                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 1
                        ieor   = 0
                        hdr%info%source = 'GTS (ROHK) ' // msg_header
                        hdr%info%is_sound = .TRUE.
                        wrote_hdr = .FALSE.
                        vert_id = MISSING

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
                        hdr%location%id = '-6666'
                        hdr%location%name = rtmp%field_cval(12:)
                        ! hdr%info%elevation = 0.0    ! assume 'SHIP' at elevation = 0 
                        ! upa%height%data =  0.0      ! assume 'SHIP' at elevation = 0 
 
     CASE('SHIP or T'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-7777'
                        hdr%location%name = rtmp%field_cval(12:)
                        ! hdr%info%elevation = 0.0    ! assume 'SHIP' at elevation = 0 
                        ! upa%height%data =  0.0      ! assume 'SHIP' at elevation = 0 

     CASE('Latitude '); used = .TRUE.
                        hdr%location%latitude = rtmp%field_rval/100
 
     CASE('Longitude'); used = .TRUE.
                        hdr%location%longitude = rtmp%field_rval/100
                        IF ( fm .EQ. 35 .OR. fm .EQ. 32 ) THEN
                           hdr%location%id = rtmp%field_cval(6:10)
                           hdr%location%name = rtmp%field_cval(35:rlen)
                        ELSE
                           CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
                        ENDIF

     !---------------------------------------------------------------
     ! Source information

     CASE('Elevation'); used = .TRUE.
                        hdr%info%elevation = rtmp%field_rval
                        ! upa%height%data = rtmp%field_rval 
                        ! The above is true for synop data, think about it here
                        CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
 
     !---------------------------------------------------------------
     ! Terrestrial and Upper air information

     CASE('Station P'); used = .TRUE.
                        hdr%ground%psfc%data = multiply ( rtmp%field_rval , TOPa )
                        upa%pressure%data = multiply ( rtmp%field_rval , TOPa )
                        !upa%height%data = hdr%info%elevation
                        nvalid = nvalid + 1

     CASE('P @ max V'); 
                           CALL set_upa_val ( upa%pressure%data , .FALSE. )
                           upa%pressure%data = multiply ( TOPa , upa%pressure%data )
                           
                        IF ( rtmp%field_ival .NE. MISSING ) THEN
                           nvalid = nvalid + 1
                        ELSE
                           used = .FALSE.
                        ENDIF

     CASE('Pressure '); CALL set_upa_val ( upa%pressure%data , .FALSE. )
                        upa%pressure%data = multiply ( TOPa , upa%pressure%data )
                        !IF ( part == 'B' .AND. rtmp%field_cval(1:4) == '00..') THEN ! fm == 35 .AND.
                        !    upa%height%data = hdr%info%elevation
                        !    nvalid = nvalid + 1
                        !ENDIF
                        nvalid = nvalid + 1

     CASE('Geopotent'); IF (rtmp%field_ival .NE. UNDEFINED .AND. rtmp%field_ival .NE. MISSING ) THEN
                            CALL set_upa_val ( upa%height%data , .TRUE. )
                            nvalid = nvalid + 1
                        ENDIF
     CASE('Z @ max V'); 
                           CALL set_upa_val ( upa%height%data , .FALSE. )
                        IF ( rtmp%field_ival .NE. MISSING ) THEN
                           nvalid = nvalid + 1
                        ELSE
                           used = .FALSE.
                        ENDIF

     CASE('Height (g'); CALL set_upa_val ( upa%height%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Temperatu'); CALL set_upa_val ( upa%temperature%data , .TRUE. )
                        upa%temperature%data = add ( TOKELVIN, upa%temperature%data )
                        nvalid = nvalid + 1

     CASE('Dew Point'); CALL set_upa_val ( upa%dew_point%data , .TRUE. )
                        upa%dew_point%data = add ( TOKELVIN, upa%dew_point%data )
                        nvalid = nvalid + 1

     CASE('Wind dire'); CALL set_upa_val ( upa%direction%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Wind spee'); CALL set_upa_val ( upa%speed%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Sea Surfa'); used = .TRUE.
                        hdr%ground%sst%data = add ( TOKELVIN , rtmp%field_rval )
                        nvalid = nvalid + 1

     CASE('Total Clo'); used = .TRUE.
                        hdr%ground%cloud_cvr%data = rtmp%field_rval
                        nvalid = nvalid + 1
 
     CASE('Lowest cl'); used = .TRUE.
                        hdr%ground%ceiling%data = rtmp%field_rval
                        nvalid = nvalid + 1
 
     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT

    !---------------------------------------------------------------

    IF ( FOR_DEBUG ) THEN
       IF ( used ) THEN
          WRITE ( outfiles ( record_fm+800 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
       ELSE
          WRITE ( outfiles ( record_fm+900 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
       ENDIF
    ENDIF

    rtmp => rtmp%next_record

   ENDDO

   IF ( wrote_hdr ) THEN
!      IF( USED ) THEN
         CALL write_rpt_upa ( iwrite , upa , ieor ) 
!      ENDIF
      upa = empty_upa
      CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'wr_rap_fm35 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE set_upa_val ( variable , set_pz )

   IMPLICIT NONE

   REAL , INTENT ( OUT )               :: variable
   LOGICAL , INTENT ( IN )             :: set_pz

   if(TRACE_MOST) WRITE(*,*) 'upa_set_pz in:', variable, set_pz , rtmp%field_rval, rtmp%field_ival
   used     = .TRUE.

   IF ( vert_id .NE. rtmp%field_ival .AND. vert_id .NE. MISSING ) THEN
      if(TRACE_MOST) WRITE(*,*) 'write_rpt_upa vert_id:, ', upa%pressure%data, upa%height%data
      CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
   ENDIF

   variable = rtmp%field_rval
   vert_id  = rtmp%field_ival

   IF ( set_pz .AND. vert_id .NE. MISSING .AND. vert_id .NE. UNDEFINED ) THEN
      IF ( vert_id > 0 ) THEN
         IF ( upa%pressure%data .EQ. MISSING .or. upa%pressure%data .EQ. 0. ) & 
              upa%pressure%data = multiply ( TOPa, vert_id )
      ELSE IF( vert_id < 0 ) THEN
   !      IF ( upa%height%data .EQ. MISSING ) &
              upa%height%data = abs ( vert_id )
      ENDIF
!   
   ENDIF
   if(TRACE_MOST) WRITE(*,*) 'upa_set_pz out:', variable, vert_id, set_pz, upa%pressure%data, upa%height%data

ENDSUBROUTINE

ENDSUBROUTINE wr_rap_fm35

