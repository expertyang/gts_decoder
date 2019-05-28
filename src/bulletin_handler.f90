!======================================================================

SUBROUTINE read_a_bulletin ( inunit )

   ! Routine to read in a bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE parameters
   USE special_symbols
   IMPLICIT NONE

!   INCLUDE 'inc.traceflow'
!   INCLUDE 'inc.special_symbols'

   INTEGER , INTENT ( IN )  :: inunit
   CHARACTER ( LEN = llen ) :: line , dummy

   IF ( TRACE_MOST ) PRINT  * , 'read_a_bulletin : in'

   CALL bulletin_initialize

   read_loop : DO
      READ ( inunit , '(A)' , END = 90 , ERR = 99 ) line
      CALL parse_line ( adjustl ( line ) , LINE_DELIMITORS , line , dummy )
       
      IF ( INDEX ( line , ETX ) > 0 ) THEN
         CALL bulletin_append ( ETX )
!        WRITE(*, *) 'INDEX ( line , ETX ) > 0', line
         EXIT read_loop
      ELSEIF ( INDEX ( line , SOH ) > 0 ) THEN
!        WRITE(*, *) 'INDEX ( line , SOH ) > 0', INDEX ( line , SOH ), line
         IF ( bulletin_nlines == 0 ) then
            ! Skipping header SOH
         ELSE
            ! Warning : ETX is missing in end of message, add one
            CALL bulletin_append ( ETX )
            EXIT read_loop
         ENDIF
      ELSEIF ( bulletin_nlines .GE. bulletin_maxlines ) THEN
         PRINT *,'Number of lines in current bulletin >= ', bulletin_maxlines 
         PRINT *,'========== Skipping overflow lines ==========='
         skip_overflow : DO
            bulletin_nlines = bulletin_nlines + 1
            READ ( inunit , '(A20)', END = 90 , ERR = 99 ) line
            IF ( INDEX ( line(1:20) , ETX//SOH ) > 0 ) THEN
               CALL bulletin_append (ETX )
               EXIT read_loop
            ENDIF
         ENDDO skip_overflow
      ELSE
!        WRITE(*, *) 'INDEX ( line ) ELSE :', line
         CALL bulletin_append ( line )
      ENDIF
   ENDDO read_loop

   IF ( bulletin_nlines .GE. bulletin_maxlines ) THEN
      PRINT *,'Number of lines in overflow bulletin = ', bulletin_nlines
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'read_a_bulletin : out'
   RETURN

90 CALL bulletin_append ( ETX )
   IF ( bulletin_nlines .EQ. 1 ) bulletin_nlines = -1
   PRINT  * , ' read_a_bulletin : ***  End of   input unit : ' , inunit
   RETURN

99 CALL bulletin_append ( ETX )
   IF ( bulletin_nlines .EQ. 1 ) bulletin_nlines = -1
   ! bulletin_nlines = -1
   PRINT  * , ' read_a_bulletin : ***  Error in input unit : ' , inunit
   RETURN

ENDSUBROUTINE read_a_bulletin

!----------------------------------------------------------------------

SUBROUTINE identify_code_form

   ! SUBROUTINE to identify the code TYPE of the bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE

   
   INTEGER , EXTERNAL       :: gts_fm_num
   CHARACTER ( LEN = llen ) :: header_lines ( 4 ) ,  &
                               rest ,                &
                               arg
   INTEGER                  :: lcount
   TYPE ( lines ) , POINTER :: btmp
   INTEGER :: record_fm_header
   
   IF ( TRACE_ALL ) PRINT  * , 'identify_code_form : in'

   btmp => bulletin 

!  read_4_lines : DO lcount = 1 , 4
   read_4_lines : DO lcount = 1 , 3
!     WRITE(*, *) 'header_lines ',lcount, ':', btmp%text
      IF ( TRIM ( btmp%text ) .EQ. ETX ) THEN
!        WRITE(*, *) 'TRIM ( btmp%text ) .EQ. ETX'
         record_fm = 99
         RETURN
      ELSE
         header_lines ( lcount ) = btmp%text
         btmp => btmp%next_line
      ENDIF
   ENDDO read_4_lines
   
!  CALL parse_line ( header_lines(4) , ' ' , arg , rest )
   CALL parse_line ( header_lines(3) , ' ' , arg , rest )
!  WRITE(*, *) 'gts_fm_num :', header_lines(1)
!  WRITE(*, *) 'gts_fm_num :', header_lines(2)
!  WRITE(*, *) 'gts_fm_num :', header_lines(3),TRIM ( arg )

   record_fm = gts_fm_num ( TRIM ( arg ) , 'CODE_HEADER' )
   record_fm_header = gts_fm_num ( header_lines(2) , 'STATION_ID' )
   IF ( (record_fm_header .NE. 99) .AND. (record_fm .NE. record_fm_header) ) THEN
!     record_fm = gts_fm_num ( TRIM ( header_lines(3) ) , 'STATION_ID' )
!      record_fm = gts_fm_num ( TRIM ( header_lines(2) ) , 'STATION_ID' )
      record_fm = record_fm_header
!      WRITE (*, *) TRIM ( header_lines(2) )
   ELSE
      region_AA=header_lines(2) (3:4)
   ENDIF
   WRITE(*, '(A, ". ", A, I2,". ", A, I)')  'WMO Header: '// TRIM(header_lines(2)), 'FM-',record_fm, 'Bulletin Lines: ', bulletin_nlines

   IF ( TRACE_MOST ) PRINT  * , 'identify_code_form : out ' , record_fm

ENDSUBROUTINE identify_code_form

!----------------------------------------------------------------------

FUNCTION gts_fm_num ( keyword , keytype )

   ! FUNCTION to identify the code form according to a <keyword>
   !    and a <keytype>. Valid Keytype = 'CODE_HEADER' , 'STATION_ID'.
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   CHARACTER ( LEN = *) , INTENT ( IN ) :: keyword , &
                                           keytype
   INTEGER                              :: gts_fm_num

   gts_fm_num = 99

   IF ( keyword(1:3) .EQ. 'NIL' ) THEN
      gts_fm_num = 98
      RETURN
   ENDIF

   SELECT CASE ( keytype )

   CASE ( 'STATION_ID' )
      IF     ( keyword(1:4) .EQ. 'BMAA' ) THEN ! Adminstration Messages
         gts_fm_num = 93
      ELSEIF ( keyword(1:4) .EQ. 'WWJP' ) THEN ! Japanese TS warnings
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'TPPN' ) THEN ! Satellite TS Fixes
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'WTPN' ) THEN ! Guam TS Warning
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'WDPN' ) THEN ! Guam TS Diagnostics
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'WTPQ' ) THEN ! HK0 TS Warning
         gts_fm_num = 94
!     ELSEIF ( keyword(1:4) .EQ. 'WSCI' ) THEN ! Taipei TS Diagnostics
!        gts_fm_num = 94
      ELSEIF ( keyword(1:1) .EQ. 'W'    ) THEN ! Textual TS Warnings
         gts_fm_num = 95
      ELSEIF ( keyword(1:2) .EQ. 'AB'   ) THEN ! General Weather Bulletins
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'UA'   ) THEN ! other ARPs
         gts_fm_num = 96
      ELSEIF ( keyword(1:2) .EQ. 'AA'   ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:3) .EQ. 'RWK'  ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'FP'   ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'SE'   ) THEN ! Seismic activity report
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'SA'   ) THEN ! Aviation routine reports.  FM 15 (METAR), SAO 
         gts_fm_num = 15
      ENDIF

   CASE ( 'CODE_HEADER' )
      SELECT CASE ( keyword )

         CASE ( 'SATELLITE' ) ;                       gts_fm_num = 95
         CASE ( 'RSMC' ) ;                            gts_fm_num = 95
         CASE ( 'GEOALERT' ) ;                        gts_fm_num = 93
         CASE ( 'ARP' ) ;                             gts_fm_num = 96
         CASE ( 'AIREP' ) ;                           gts_fm_num = 97

         CASE ( 'AAXX' ) ;                            gts_fm_num = 12
         CASE ( 'BBXX' ) ;                            gts_fm_num = 13
         CASE ( 'OOXX' ) ;                            gts_fm_num = 14
         CASE ( 'METAR' ) ;                           gts_fm_num = 15
         CASE ( 'SPECI' ) ;                           gts_fm_num = 16
         CASE ( 'ZZYY' ) ;                            gts_fm_num = 18

         CASE ( 'FFAA' , 'FFBB' , 'GGAA' , 'GGBB' ) ; gts_fm_num = 20
         CASE ( 'RADREP' ) ;                          gts_fm_num = 22

         CASE ( 'PPAA' , 'PPBB' , 'PPCC' , 'PPDD' ) ; gts_fm_num = 32
         CASE ( 'QQAA' , 'QQBB' , 'QQCC' , 'QQDD' ) ; gts_fm_num = 33
         CASE ( 'EEAA' , 'EEBB' , 'EECC' , 'EEDD' ) ; gts_fm_num = 34
         CASE ( 'TTAA' , 'TTBB' , 'TTCC' , 'TTDD' ) ; gts_fm_num = 35
         CASE ( 'UUAA' , 'UUBB' , 'UUCC' , 'UUDD' ) ; gts_fm_num = 36
         CASE ( 'XXAA' , 'XXBB' , 'XXCC' , 'XXDD' ) ; gts_fm_num = 37
         CASE ( 'IIAA' , 'IIBB' , 'IICC' , 'IIDD' ) ; gts_fm_num = 38

         CASE ( 'RRXX' ) ;                            gts_fm_num = 39

         CASE ( 'SSXX' ) ;                            gts_fm_num = 40
         CASE ( 'LLXX' ) ;                            gts_fm_num = 41
         CASE ( 'AMDAR' ) ;                           gts_fm_num = 42
         CASE ( 'ICEAN' ) ;                           gts_fm_num = 44
         CASE ( '10001' , '65556' ) ;                 gts_fm_num = 45
         CASE ( 'GRID' ) ;                            gts_fm_num = 47
         CASE ( 'GRAF' ) ;                            gts_fm_num = 49

         CASE ( 'WINTEM' ) ;                          gts_fm_num = 50
         CASE ( 'TAF' ) ;                             gts_fm_num = 51
         CASE ( 'ARFOR' ) ;                           gts_fm_num = 53
         CASE ( 'ROFOR' ) ;                           gts_fm_num = 54
         CASE ( 'RADOF' ) ;                           gts_fm_num = 57

         CASE ( 'MAFOR' ) ;                           gts_fm_num = 61
         CASE ( 'NNXX' ) ;                            gts_fm_num = 62
         CASE ( 'JJXX' , 'JJYY' , 'JJVV' ) ;          gts_fm_num = 63
         CASE ( 'KKXX' , 'KKYY' ) ;                   gts_fm_num = 64
         CASE ( 'MMXX' ) ;                            gts_fm_num = 65
         CASE ( 'HHXX' ) ;                            gts_fm_num = 67
         CASE ( 'HYFOR' ) ;                           gts_fm_num = 68

         CASE ( 'CLIMAT' ) ;                          gts_fm_num = 71
         CASE ( 'CLINP' , 'CLISA' ) ;                 gts_fm_num = 73
         CASE ( 'INCLI' , 'NACLI' , 'SPCLI' ) ;       gts_fm_num = 73

         CASE ( 'SFLOC' ) ;                           gts_fm_num = 82
         CASE ( 'SFAZU' ) ;                           gts_fm_num = 83
         CASE ( 'CCAA' , 'CCBB' , 'DDAA' , 'DDBB' ) ; gts_fm_num = 85
         CASE ( 'VVAA' , 'VVBB' , 'VVCC' , 'VVDD' ) ; gts_fm_num = 86
         CASE ( 'WWXX' ) ;                            gts_fm_num = 87
         CASE ( 'YYXX' ) ;                            gts_fm_num = 88

      ENDSELECT

   ENDSELECT

ENDFUNCTION gts_fm_num

!----------------------------------------------------------------------

SUBROUTINE interpret_bulletin

   ! Driver Routine for interpreting bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   LOGICAL                  :: decoded

   IF ( TRACE_MOST ) PRINT  * , 'interpret_bulletin : in ' , record_fm

   bul_stat ( record_fm, number ) = bul_stat ( record_fm, number ) + 1
   bulletin_error = 0
   bulletin_warn  = 0

   CALL record_initialize ( record_fm )
   CALL assign_outfile ( 0 )
   IF ( outfiles ( record_fm ) .LT. 0 ) THEN
      CALL prog_abort(' ===> Unexpected condition, outfiles(record_fm) < 0 ')
   ENDIF

   CALL bulletin_print ( outfiles ( record_fm ) )

   SELECT CASE ( record_fm )

      CASE ( 12 , 13 ) ; decoded = .TRUE. 
                         CALL decode_fm12
                         CALL wr_rawins_fm12
                         CALL wr_rap_fm12
!                        CALL wr_txt_fm12
                                         
      CASE ( 15 , 16 ) ; decoded = .TRUE. 
                         CALL decode_fm15
                         CALL wr_rap_fm15
!                        CALL wr_txt_fm12
                                        
      CASE ( 32 : 34 ) ; decoded = .TRUE.
                         CALL decode_fm32
                         CALL wr_rawins_fm35
                         CALL wr_rap_fm35
                                         
      CASE ( 35 : 38 ) ; decoded = .TRUE.
                         CALL decode_fm35
                         CALL wr_rawins_fm35
                         CALL wr_rap_fm35
                                         
      CASE ( 42      ) ; decoded = .TRUE.
                         CALL decode_fm42
                         CALL wr_rap_airep

      !CASE ( 86      ) ; decoded = .TRUE.
      !                   CALL decode_fm86
      !                   CALL wr_rap_fm86
      !                   CALL wr_rawins_fm86

      !CASE ( 88      ) ; decoded = .TRUE.
      !                   CALL decode_fm88
      !                   CALL wr_rawins_fm88
      !                   CALL wr_rap_fm88

      CASE ( 97 , 96 ) ; decoded = .TRUE.
                         CALL decode_airep
                         CALL wr_rap_airep

      CASE DEFAULT     ; decoded = .FALSE.

   ENDSELECT

   IF ( decoded ) THEN

      CALL assign_outfile ( 1 )

      ! input bulletin as the decoder sees it
      CALL bulletin_print ( outfiles ( record_fm+100 ) )

      ! decoded text
      CALL record_print ( outfiles ( record_fm+100 ) )

      ! error(s) or warning(s) found, PRINT output to appropriate files
      IF ( TRACE_LEAST ) THEN
         IF ( bulletin_error  > 0 ) THEN
            CALL bulletin_print ( ierr )
            CALL record_print   ( ierr )
            CALL bulletin_print ( ierr0 )
            CALL record_print   ( ierr0 )
         ENDIF
      ENDIF

      IF ( TRACE_MOST ) THEN
         IF ( bulletin_warn > 0 ) THEN
            CALL bulletin_print ( iwarn )
            CALL record_print   ( iwarn )
            CALL bulletin_print ( iwarn0 )
            CALL record_print   ( iwarn0 )
         ENDIF
      ENDIF

   ENDIF

   IF ( FLUSH_FILES ) CALL flush_outfiles

   IF ( TRACE_ALL ) PRINT  * , 'interpret_bulletin : out'

ENDSUBROUTINE interpret_bulletin

!----------------------------------------------------------------------

SUBROUTINE decode_bulletin_header ( ddhhmm )

   ! Routine to decoder message header (this part of the bulletin does
   !    not appear to be part of the WMO code form specification
   !
   ! After passing through identify code form, it is guaranteed that
   !    there is at least 4 lines in bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE record_def
   USE bulletin_def
   USE date_pack
   IMPLICIT NONE
   INTEGER , INTENT ( OUT ) :: ddhhmm
!  INTEGER                  :: seqnum
   CHARACTER ( LEN = llen ) :: msghdr , cval , rest
   
   REAL :: time_diff

   IF ( TRACE_ALL ) PRINT  * , 'decode_bulletin_header : in'

   current_line => bulletin

   ! First line of bulletin ( a three digit sequence number )
! Skip it!!!!!!!!!
!   current_line   => current_line%next_line
!   seqnum         =  str2int ( TRIM ( current_line%text ) )
!   IF ( seqnum < 0 ) THEN
!      CALL code_error ( 'bulletin_header' , 'seqnum' , current_line%text )
!      ddhhmm = UNDEFINED
!      CALL bulletin_skip ( ETX )
!      NULLIFY ( current_line )
!      RETURN
!   ENDIF
    seqnum        = seqnum + 1

   ! Second line of bulletin ( reporting center id, time of report )
   current_line   => current_line%next_line
   msghdr         =  current_line%text
   msg_header     = TRIM ( msghdr )

   CALL parse_line ( msghdr , WORD_DELIMITORS , cval , rest ) ! message ID
   CALL parse_line ( rest   , WORD_DELIMITORS , center_id , rest ) ! center ID
   CALL parse_line ( rest   , WORD_DELIMITORS , cval , rest ) ! Reporting time
   CALL cset_msg_time ( TRIM ( cval ) )

   time_diff = get_diff_date ( date ( msg_year , msg_month , msg_day , msg_hour , msg_minute , msg_second , 0 ) , &
                               date ( utc_year , utc_month , utc_day , utc_hour , utc_minute , utc_second , 0 ) )

   IF ( msg_yymmdd < 0 .OR. msg_hhmmss < 0 .OR. ABS( time_diff ) > time_window .OR. time_diff > 0 ) THEN
      WRITE(*, *) "Skip Time:", msg_yymmdd , msg_hhmmss, 'Time diff: ', time_diff

      ddhhmm = UNDEFINED
      CALL bulletin_skip ( ETX )
      NULLIFY ( current_line )
      RETURN
   ENDIF
   bul_year = msg_year ; bul_month = msg_month ; bul_day = msg_day
   bul_hour = msg_hour ; bul_minute= msg_minute; bul_second = msg_second
   bul_yymmdd = msg_yymmdd ; bul_hhmmss = msg_hhmmss

   IF ( LEN_TRIM ( rest ) .EQ. 0 ) THEN
      cval = '0002:BHDR :Bulletin hdr DDHHMM ' // TRIM ( msghdr )
   ELSE
      cval = '-002:BHDR :Bulletin hdr DDHHMM ' // TRIM ( msghdr )
   ENDIF
   ddhhmm = bul_day*10000 + bul_hour*100 + bul_minute
   CALL record_appendj ( seqnum , ddhhmm , cval )

   current_line   => current_line%next_line
   remaining_text =  current_line%text

   IF ( TRACE_MOST ) PRINT  * , 'decode_bulletin_header : out ', ddhhmm

ENDSUBROUTINE decode_bulletin_header

!----------------------------------------------------------------------
