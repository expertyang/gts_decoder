
SUBROUTINE decode_fm15

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival , ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   IF ( TRACE_MOST ) PRINT  * , 'decode_fm15 : in '
   CALL assign_outfile ( 23 )

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )
   IF ( .NOT. ASSOCIATED ( current_line ) ) RETURN  ! header error

   ! next argument should be METAR or SPECI
 
   CALL get_next_word_in_mesg ( arg )
   IF ( ( arg(1:5) .EQ. 'METAR' ) .OR. ( arg(1:5) .EQ. 'SPECI' ) ) THEN
      cval = '....:MiMj :CODE HEADER ' // arg
   ELSE
      cval = '....:MiMj :CODE HEADER ' // 'METAR'
      CALL push_word_back_to_mesg ( arg )
   ENDIF
   CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )

!
!   IF ( ( arg(1:5) .NE. 'METAR' ) .and. ( arg(1:5) .NE. 'SPECI' ) ) THEN
!      CALL code_error ( 'metar' , 'ERROR in header' , arg )
!      PRINT  * , '***  ERROR in header , skipping entire bulletin ', arg
!      record_error = record_error + 1
!      CALL bulletin_skip ( ETX )
!   ENDIF

   ! repeated groups

   loop_mesgs : DO WHILE ( ASSOCIATED ( current_line ) )

      CALL new_section ( 1 )

      loop_words : DO
      
         CALL get_next_word_in_mesg ( arg , .FALSE.)       ! get a word from the bulletin

         IF ( arg .EQ. ETX ) THEN                 ! check end of bulletin
            EXIT loop_mesgs

         ELSEIF ( LEN_TRIM ( arg ) .EQ. 0 ) THEN  ! check end of message
            EXIT loop_words

         ELSE
            CALL decode_fm15_words ( arg )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_fm15 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_fm15_words ( arg )

   IMPLICIT NONE
!  CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   CHARACTER ( LEN = * ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm15_words: in ', &
      section_id, section_argnum, ' ', TRIM(arg)

   IF ( ( arg(1:5) .EQ. 'METAR' ) .OR. ( arg(1:5) .EQ. 'SPECI' ) ) THEN
!      cval = '....:MiMj :CODE HEADER ' // arg
!      CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )
      RETURN
   ENDIF
   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   CALL decode_fm15_sec1 ( arg )

   IF ( TRACE_MOST ) PRINT *,'decode_fm15_words: out ', &
      TRIM(arg), ' ', section_id, section_argnum

ENDSUBROUTINE decode_fm15_words

!----------------------------------------------------------------------

SUBROUTINE decode_fm15_sec1 ( arg )

   IMPLICIT NONE
!  CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   CHARACTER ( LEN = * ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget
   CHARACTER ( LEN = rlen ) , SAVE       :: arg2
   
   INTEGER :: arg_len

   IF ( TRACE_MOST ) PRINT *,'decode_fm15_sec1: in ', &
      section_argnum, ' ', TRIM(arg)

   arg_len=LEN_TRIM(arg)
   SELECT CASE ( section_argnum )

   CASE ( 1 )

      msg_yymmdd = UNDEFINED
      msg_hhmmss = UNDEFINED

      ! First group should be CCCC (ICAO international 4-letter location id)

      ! *** begin non-standard code ***
      !
      ! However, it was seen in practise that METAR DDGGggZ was coded before
      !    CCCC in many instance.  The following non-standard extension allows
      !    for that possiblility

      iget = LEN_TRIM(arg)
      IF ( iget .NE. 4 ) THEN
         IF ( arg(iget:iget) .EQ. 'Z' ) THEN
            cval = arg
            IF ( iget .EQ. 5 ) THEN
               call code_table_GGgg ( arg(1:4), ival, rval, cval, 'APPEND' )
               msg_yymmdd = ival
               msg_hhmmss = NINT ( rval )
            ELSE IF ( iget .eq. 7 ) THEN
               call cset_msg_time ( arg(1:6) )
            ENDIF 
            IF ( msg_yymmdd < 0 .OR. msg_hhmmss < 0 ) THEN
               cval = arg
               CALL record_appendj ( msg_yymmdd , msg_hhmmss , cval )
               CALL code_error ( 'fm15_sec1', 'invalid Obs. Time', cval )
               section_id = -1
            ELSE
               cval = '....:GGgg :Observation Time HHMMSS'
               IF ( msg_hhmmss .NE. MISSING  .AND. msg_hhmmss .NE. UNDEFINED )  &
                     CALL record_appendj ( msg_yymmdd, msg_hhmmss, cval )
            ENDIF
            call get_next_word_in_mesg ( arg , .FALSE.)
         ENDIF
      ENDIF

      ! ***   end non-standard code ***
   
      IF ( LEN_TRIM(arg) .NE. 4 ) THEN
         IF ( TRIM(arg) == 'METAR' ) THEN
            RETURN
         ELSE
            cval = arg
            CALL code_error ( 'fm15_sec1', 'invalid ICAO 4-letter id', cval )
            section_id = -1
         ENDIF
      ELSE
         if ( msg_yymmdd <= 0 ) msg_yymmdd = bul_yymmdd
         if ( msg_hhmmss <= 0 ) msg_hhmmss = bul_hhmmss
         if ( record_fm .eq. 15 ) then
            cval = '  15:=====:New message ' // TRIM(arg) // BLANK_LINE
         else
            cval = '  16:=====:New message ' // TRIM(arg) // BLANK_LINE
         endif
         CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )
         CALL code_table_icao ( arg , ival , rval , cval , 'APPEND' )
         section_argnum = 2
      ENDIF

   CASE ( 2 )

      ! GGggZ or DDGGggZ is an optional second argument
   
      iget = LEN_TRIM(arg)
      ! print *,' arg = ',TRIM(arg),' iget = ',iget
      cval = arg
      IF ( arg(iget:iget) .EQ. 'Z' ) THEN
         IF ( LEN_TRIM ( arg ) .EQ. 5 ) THEN
            call code_table_GGgg ( arg(1:4), ival, rval, cval, 'APPEND' )
            msg_yymmdd = ival
            msg_hhmmss = NINT ( rval )
         ELSE IF ( LEN_TRIM ( arg ) .eq. 7 ) THEN
            call cset_msg_time ( arg(1:6) )
         ENDIF 
         IF ( msg_yymmdd < 0 .OR. msg_hhmmss < 0 ) THEN
            cval = arg
            CALL record_appendj ( msg_yymmdd , msg_hhmmss , cval )
            CALL code_error ( 'fm15_sec1', 'invalid Obs. Time', cval )
            section_id = -1
         ELSE
            ! print *, ' msg_yymmdd, msg_hhmmss ', msg_yymmdd, msg_hhmmss
            cval = '....:GGgg :Observation Time HHMMSS'
            IF ( msg_hhmmss .NE. MISSING  .AND. msg_hhmmss .NE. UNDEFINED )  &
                 CALL record_appendj ( msg_yymmdd, msg_hhmmss, cval )
         ENDIF
!         call get_next_word_in_mesg ( arg )
!      ENDIF
      ELSEIF ( TRIM ( arg ) .EQ. 'AUTO' ) THEN ! skip non-standard word AUTO that appear in many messages
!         CALL get_next_word_in_mesg ( arg )
!      ENDIF
!         CALL code_ignore ( 'a fully automated report with no human intervention' , 'ignored' , arg )
!         RETURN
      ELSEIF ( LEN_TRIM(arg) .GT. 6 ) THEN
         IF( arg(arg_len-2:arg_len) == 'KMH' .OR. &
             arg(arg_len-2:arg_len) == 'MPS' .OR. &
             arg(arg_len-1:arg_len) == 'KT' ) THEN ! Decode Wind  ( dddff[Gfmfm][unit] )
            CALL code_fm15_dir ( arg(1:3), ival, rval, cval, 'APPEND' )
            CALL code_fm15_ff  ( arg(4:) , ival, rval, cval, 'APPEND' )
!         ELSE
!            CALL code_ignore ( 'fm15_sec1_argnum2' , 'ignored' , arg )
         ENDIF
!      ELSE &
!      IF ( LEN_TRIM(arg) >= 3 ) THEN
!         IF( arg(arg_len-1:arg_len) == 'SM' .OR. &
!             str2int(arg) >0 )THEN 
!            CALL code_fm15_vvvv ( arg , ival , rval , cval , 'APPEND' )
!         ENDIF
         
      ELSE
!         CALL code_ignore ( 'fm15_sec1_argnum2' , 'ignored' , arg )
         section_argnum = 3
     ENDIF

   CASE ( 3 )

      ! skip all the junk until getting to [M]T'T'/[M]T'dT'd

      IF ( LEN_TRIM ( arg ) .GT. 4 ) THEN
         IF ( arg(3:3) .EQ. '/' ) THEN
            CALL code_fm15_tt ( arg(1:2), ival, rval, cval, 'APPEND' )
            CALL code_fm15_td ( arg(4:) , ival, rval, cval, 'APPEND' )
            IF ( ival .NE. MISSING ) THEN
               section_argnum = 4 
            ENDIF
         ELSEIF ( ( arg(1:1) .EQ. 'M' ) .AND. ( arg(4:4) .EQ. '/' ) ) THEN
            CALL code_fm15_tt ( arg(1:3), ival, rval, cval, 'APPEND' )
            CALL code_fm15_td ( arg(5:) , ival, rval, cval, 'APPEND' )
            IF ( ival .NE. MISSING ) THEN
               section_argnum = 4 
            ENDIF
!         ELSEIF ( arg_len >= 6 .AND. &
!                ( arg(1:3) == 'FEW' .OR. &
!                  arg(1:3) == 'SCT' .OR. &
!                  arg(1:3) == 'BKN' .OR. &
!                  arg(1:3) == 'OVC' .OR. &
!                  arg(1:3) == 'SKC' )  ) THEN
!               ival = str2int(arg(4:6))
!               rval = multiply(FEET*100,ival)
!               SELECT CASE(arg(1:3))
!               CASE('SKC') ; ival=0
!               CASE('FEW') ; ival=1
!               CASE('SCT') ; ival=4
!               CASE('BKN') ; ival=6
!               CASE('OVC') ; ival=8
!               END SELECT
!               cval = 'NSHS:METAR:Clouds at layer '// arg
!               CALL record_appendr ( ival,rval,cval)    
!         ELSE
!            CALL code_ignore ( 'fm15_sec1_argnum3' , 'ignored' , arg )
         ENDIF

!      ELSE
!         CALL code_ignore ( 'fm15_sec1_argnum3' , 'ignored' , arg )
      ENDIF

   CASE ( 4 )

      ! QNH value QPhPhPhPh or APhPhPhPh
      IF ( ( arg(1:1) .EQ. 'Q' ) .OR. ( arg(1:1) .EQ. 'A' ) ) THEN
         CALL code_table_PHPHPHPH ( arg(1:5), ival, rval, cval, 'APPEND' )
!      ENDIF

      ELSE          &
      IF ( arg .EQ. 'RMK' ) THEN ! no standard, in the US RMK group defines SLP and T
         section_argnum = 5
!      ELSE
!         CALL code_ignore ( 'fm15_sec1_argnum4' , 'ignored' , arg )
      ENDIF

   CASE ( 5 )
      
      IF ( arg(1:3).EQ.'SLP' .AND. LEN_TRIM(arg).EQ.6 ) THEN
         ival = str2int ( arg(4:6) )
         IF ( ival < 550 ) THEN ! maximum SLP for any location : 1054.9 hPa
            rval = add ( 1000 , multiply ( 0.1 , ival ) )
         ELSE
            rval = add ( 900 , multiply ( 0.1 , ival ) )
         ENDIF
         CALL record_appendr ( ival , rval , '....:SLP  :Mean SLP (hPa)' )
!      ENDIF
      ELSE &
      IF ( arg(1:1).EQ.'T' .AND. LEN_TRIM(arg).EQ.9 ) THEN
         CALL code_fm15_TTTT ( arg(2:5), ival, rval, cval, 'APPEND' )
         CALL code_fm15_TdTd ( arg(6:9), ival, rval, cval, 'APPEND' )
      ELSE
         CALL code_ignore ( 'fm15_sec1 RMK' , 'ignored' , arg )
      ENDIF

   CASE DEFAULT

      ! ONLY ONE WIND / TEMPERATURE GROUP decoded per METAR report
      CALL code_ignore ( 'fm15_sec1' , 'ignored' , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm15_sec1: out ', section_id

ENDSUBROUTINE decode_fm15_sec1

!----------------------------------------------------------------------

ENDSUBROUTINE decode_fm15
