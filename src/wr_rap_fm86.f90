
SUBROUTINE wr_rap_fm86

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used
   CHARACTER ( len = 40 ) :: satid 
   
   REAL :: ref_pres = missing, ref_height = missing

   IF ( .NOT. FOR_RAP ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rap_fm86 : in '

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
   satid  = 'Unknown satellite'
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
                           CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
                           CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
                        ENDIF

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 1
                        ieor   = 0
                        hdr%info%source   = 'GTS (ROHK) ' // msg_header
                        hdr%info%is_sound = .TRUE.
                        hdr%location%id   = satid
                        wrote_hdr = .FALSE.
                        ref_pres = missing
                        ref_height = missing
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Satellite'); used = .TRUE.
                        satid = rtmp%field_cval(22:)
                        hdr%location%id = satid

     CASE('Latitude '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%latitude = multiply ( 0.01, rtmp%field_rval )
 
     CASE('Longitude'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%longitude = multiply ( 0.01, rtmp%field_rval )

     !---------------------------------------------------------------
     ! Terrestrial and Upper air information


     CASE('Ref. Pres'); used = .TRUE.
                        ref_pres     = multiply ( TOPa , rtmp%field_rval )
                        ref_height   = rtmp%field_ival
                        nvalid = nvalid + 1
                        hdr%ground%ref_pres%data = ref_pres
                        CALL write_rpt_hdr ( iwrite, seqnum, hdr, ieor )

     CASE('Thickness'); used = .TRUE.
                        IF ( upa%pressure%data .NE. MISSING .AND. ref_pres .NE. MISSING ) THEN
                           CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
                        ENDIF
                        upa%pressure%data = multiply ( TOPa , rtmp%field_rval )
!                        upa%height%data = rtmp%field_ival-ref_height
                        upa%thickness%data = rtmp%field_ival
                        nvalid = nvalid + 2
!     CASE('Tropopaus'); used = .TRUE.    
!                        IF ( upa%pressure%data .NE. MISSING ) THEN
!                           CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
!                        ENDIF               
!                        IF (rtmp%field_cval(6:10) == "SFC_T" ) THEN
!                            upa%temperature%data = add ( TOKelvin, rtmp%field_rval)
!                            nvalid = nvalid + 1
!                        ELSE IF (rtmp%field_cval(6:10) == "PTrop") THEN
!                            upa%pressure%data = multiply ( TOPa, rtmp%field_rval)
!                            nvalid = nvalid + 1
!                           CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
!                        ENDIF

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
      CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
      CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'wr_rap_fm86 : out'

!----------------------------------------------------------------------

ENDSUBROUTINE wr_rap_fm86
