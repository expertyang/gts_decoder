!======================================================================


MODULE bulletin_def

   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE parameters
   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.bulletin'

!   INCLUDE 'inc.traceflow'
!   PRIVATE :: traceflow , TRACE_ALL , TRACE_MOST , TRACE_NEW_ALL , &
!              TRACE_NEW_MOST , TRACE_NEW_LEAST , TRACE_LEAST , TRACE_NONE, &
!              FLUSH_FILES
!   PRIVATE :: FOR_DEBUG, FOR_RAWINS, FOR_RAP

!   INCLUDE 'inc.special_symbols'
!   PRIVATE :: NULL , SOH , ETX , TAB , LF , CR , SPACE , DLLR , EQSGN , &
!              MESG_SEPARATORS , MESG_DELIMITORS , &
!              LINE_DELIMITORS , WORD_DELIMITORS
!
!   PRIVATE :: UNDEFINED, MISSING, ENDOUTPUT
!   PRIVATE :: ABS_UNDEFINED, ABS_MISSING, ABS_ENDOUTPUT
!
!   PRIVATE :: NUMBER_SET, ERROR_INDICATOR, ALPHANUMERIC_SET
!
!   PRIVATE :: BLANK_LINE
!   PRIVATE :: TOKELVIN, KNOT, TOPa, FEET, TOMETER, KMPH, INCHHG

!   !END INCLUDE 'inc.bulletin'


   INTEGER , PARAMETER         :: llen = 80
   TYPE lines
      CHARACTER ( LEN = llen ) :: text
      TYPE ( lines ) , POINTER :: next_line
   ENDTYPE lines

   ! The whole PROGRAM builds on the idea of a 'current' bulletin,
   !   identified by the global TARGET <bulletin>, the end of which is
   !   identified by the POINTER <bulletin_tail>. The number of lines
   !   in the bulletin is saved in <bulletin_nlines>
   !
   TYPE ( lines ) , TARGET       :: bulletin
   TYPE ( lines ) , POINTER      :: bulletin_tail
   INTEGER                       :: bulletin_nlines
   INTEGER                       :: bulletin_allocate = 0
   INTEGER , PARAMETER           :: bulletin_maxlines = 1000000

   INTEGER                       :: bulletin_error , bulletin_warn  
   INTEGER , PARAMETER , PRIVATE :: number = 1 , error = 2 , warning = 3
   INTEGER , DIMENSION (100,3)   :: bul_stat = 0

   ! During decoding, <current_line> points to the line in the bulletin
   !    that is currently being looked at. Normally, the word that is
   !    being examined is saved in a local variable <arg>, while the
   !    text in the <current_line> still remained to be decoded is saved
   !    in the global variable <remaining_text>.
   !
   TYPE ( lines ) , POINTER      :: current_line
   CHARACTER ( LEN = llen )      :: remaining_text
   !
   INTEGER :: bul_year , bul_month , bul_day   , bul_yymmdd
   INTEGER :: bul_hour , bul_minute, bul_second, bul_hhmmss
   
! WMO GTS Header Region Code AA. 'CI' For China
   CHARACTER(len=2) :: region_AA


CONTAINS

!----------------------------------------------------------------------

SUBROUTINE bulletin_initialize

   ! Routine to create a new bulletin. ROHK convention, each new
   !   bulletin is headed by a 'SOH' CHARACTER (and ended with
   !   a 'ETX' character, see also bulletin_append.)
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_initialize : in'

   NULLIFY ( bulletin%next_line )

   bulletin_tail => bulletin
   bulletin_nlines = 0

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_initialize : out'

ENDSUBROUTINE bulletin_initialize

!----------------------------------------------------------------------

SUBROUTINE bulletin_deallocate

   ! Routine to deallocate space used by the current bulletin
   !
   ! Created : Dec 29, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   TYPE ( lines ) , POINTER :: bprev, btmp

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_deallocate : in  ', bulletin_allocate

   IF ( ASSOCIATED ( bulletin%next_line ) ) THEN
      bprev => bulletin%next_line
      DO WHILE ( ASSOCIATED ( bprev%next_line ) )
         btmp  => bprev
         bprev => bprev%next_line
         DEALLOCATE ( btmp ) 
         bulletin_allocate = bulletin_allocate - 1
      END DO
      DEALLOCATE ( bprev )
      bulletin_allocate = bulletin_allocate - 1
   ENDIF

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_deallocate : out ', bulletin_allocate

ENDSUBROUTINE bulletin_deallocate

!----------------------------------------------------------------------

SUBROUTINE bulletin_append ( line )

   ! SUBROUTINE to append a <line> to the current bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: line
   TYPE ( lines ) , POINTER              :: btmp
   CHARACTER ( LEN = llen )              :: firstpart, remaining

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_append : in ', TRIM(line)

   remaining = line  ! make a copy of the input line
                 ! remaining will be broken up into parts subsequently

   DO WHILE ( LEN_TRIM ( remaining ) > 0 )

      ! make sure every bulletin ends with a mesg_separator
      IF ( TRIM(line) .EQ. ETX ) THEN
         IF ( bulletin_nlines .NE. 0 ) THEN
            bulletin_tail%text = TRIM(bulletin_tail%text) // MESG_SEPARATORS(1:1)
         ENDIF
      ENDIF

      ! check for mesg_separator; if found, break line into two parts
      CALL parse_line ( remaining , MESG_SEPARATORS , firstpart , remaining )
      IF ( LEN_TRIM ( remaining ) > 0 ) THEN
         firstpart = TRIM ( firstpart ) // MESG_SEPARATORS(1:1)
         remaining = ADJUSTL ( remaining )
      ENDIF

      ! append the first part to the bulletin
      ALLOCATE ( btmp )
      bulletin_allocate = bulletin_allocate + 1

      btmp%text = firstpart
      NULLIFY ( btmp%next_line )

      bulletin_tail%next_line => btmp
      bulletin_tail           => btmp

      bulletin_nlines   = bulletin_nlines + 1

   ENDDO

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_append : out'

ENDSUBROUTINE bulletin_append

!----------------------------------------------------------------------

SUBROUTINE bulletin_print ( iounit )

   ! routine to PRINT out the current bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: iounit
   TYPE ( lines ) , POINTER :: btmp

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_print :  in ', iounit

   IF ( iounit < 1 ) THEN
      PRINT *, 'BULLETIN PRINT IOUNIT < 0, IGNORE WRITE ', iounit
      RETURN
   ENDIF

   btmp => bulletin

   ! PRINT every line including the end of bulletin ETX symbol
   DO WHILE ( ASSOCIATED ( btmp ) )
      IF ( ASSOCIATED ( btmp%next_line ) ) THEN
         WRITE ( iounit , '(a)' ) TRIM (btmp%text)//CR//CR
      ELSE
         ! alexis gill =============================================
         !   would be nice to have the following, but appears not to
         !   work on some implementations
         ! alexis gill =============================================
         ! WRITE ( iounit , '(a)' , ADVANCE='NO' ) TRIM (btmp%text)
         WRITE ( iounit , '(a)' ) TRIM (btmp%text)
      ENDIF
      btmp => btmp%next_line
   ENDDO

   ! flush output during error diagnostic / testing runs
!  IF ( TRACE_LEAST ) CALL flush ( iounit )

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_print : out'

ENDSUBROUTINE bulletin_print

!----------------------------------------------------------------------

SUBROUTINE bulletin_skip ( delimitors )

   ! Routine to skip a number of line in the current bulletin.
   !
   ! Typical Usage:
   !
   ! (1) When the decoder encounter an error condition
   !    in the current bulletin, it will USE <bulletin_skip> to
   !    skip forward to the beginning of the next message in the
   !    current bulletin. For this, the <delimitors> supplied
   !    should be a message delimitor ('=$').
   !
   ! (2) This routine can also be used to skip the entire bulletin
   !    when the error is so bad that we cannot CONTINUE anymore.
   !    For this, the <delimitors> should be <ETX>.
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * )    :: delimitors

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_skip : in ', current_line%text

   find_delimitors : DO WHILE ( ASSOCIATED ( current_line ) )

      IF ( TRACE_MOST ) THEN
         IF ( delimitors .EQ. ETX ) THEN
            PRINT * , ' SKIPPING ENTIRE BULLETIN : ', TRIM ( current_line%text )
         ELSE
            PRINT * , ' SKIPPING A MESSAGE : ',       TRIM ( current_line%text )
         ENDIF
      ENDIF

      IF ( SCAN ( current_line%text , delimitors // ETX ) > 0 ) THEN
         IF ( TRIM ( current_line%text ) .NE. ETX ) THEN
            current_line => current_line%next_line
         ENDIF
         EXIT find_delimitors
      ELSE
         current_line => current_line%next_line
      ENDIF

   ENDDO find_delimitors

   remaining_text = current_line%text

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_skip : out ', current_line%text

ENDSUBROUTINE bulletin_skip

!----------------------------------------------------------------------

RECURSIVE SUBROUTINE get_next_word_in_mesg ( arg , split)

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( OUT ) :: arg
   LOGICAL , OPTIONAL , INTENT ( IN ) :: split
   
   LOGICAL :: split_it
   
   IF ( TRACE_ALL ) PRINT  *,'get_next_word_in_mesg : in ',TRIM(remaining_text)

   CALL parse_line ( remaining_text , SPACE // MESG_DELIMITORS , &
                     arg , remaining_text )

   split_it = .TRUE.
   IF ( present (split) ) THEN
      split_it = split
   ENDIF
   IF ( LEN_TRIM(arg) .EQ. 0 ) THEN
      ! argument length = 0 , check end of message, if not, advance line
      IF ( SCAN ( current_line%text , MESG_DELIMITORS ) > 0 ) THEN
         current_line   => current_line%next_line
         remaining_text =  current_line%text
         RETURN
      ELSE
         current_line   => current_line%next_line
         remaining_text =  current_line%text
         CALL get_next_word_in_mesg ( arg )
      ENDIF

   ! **** begin non-standard ****
   ELSEIF ( split_it .AND. (LEN_TRIM(arg) .EQ. 10 .OR.  LEN_TRIM(arg) .EQ. 11)) THEN
      ! handle some case where two words got merged into one
      IF ( LEN_TRIM(arg) .EQ. 11 ) THEN
          remaining_text = TRIM ( arg(7:) ) // ' ' // remaining_text
          WRITE(*, *) 'Split merged words ', TRIM(arg)//'=> '//arg(1:5)//' '// arg(7:)
      ELSE IF ( LEN_TRIM(arg) .EQ. 10 ) THEN
          remaining_text = TRIM ( arg(6:) ) // ' ' // remaining_text
          WRITE(*, *) 'Split merged words ', TRIM(arg)//'=> '//arg(1:5)//' '// arg(6:)
      ENDIF
      arg            = arg(1: 5)
   ! ****  end  non-standard ****

   ENDIF

   IF ( TRACE_MOST ) PRINT  *,'get_next_word_in_mesg : out ',TRIM(arg), &
                              ' <<<>>> ',TRIM(remaining_text)

ENDSUBROUTINE get_next_word_in_mesg

!----------------------------------------------------------------------

RECURSIVE SUBROUTINE get_next_ngrp_in_mesg ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( OUT ) :: arg
   INTEGER                               :: narg

   IF ( TRACE_MOST ) PRINT  * , 'get_next_ngrp_in_mesg : in'

   CALL get_next_word_in_mesg ( arg ) 

   IF ( VERIFY ( TRIM(arg) , NUMBER_SET ) .EQ. 0 ) RETURN
   IF ( TRIM(arg) == "/////" ) RETURN
   IF ( TRIM(arg) .EQ. 'NIL' )                     RETURN
   IF ( TRIM(arg) .EQ. "HNL" ) then
        arg="NIL"
        RETURN
   ENDIF
   IF ( LEN_TRIM(arg) .EQ. 4 ) THEN
      IF ( arg(3:4) .EQ. 'AA' ) RETURN
      IF ( arg(3:4) .EQ. 'BB' ) RETURN
      IF ( arg(3:4) .EQ. 'CC' ) RETURN
      IF ( arg(3:4) .EQ. 'DD' ) RETURN
      IF ( arg(1:4) .EQ. 'YYXX' ) RETURN
   ENDIF
   IF ( TRIM(arg) .EQ. ETX )                     RETURN
   
   CALL code_ignore ( 'get_next_ngrp' , 'non-numeric ****' , arg )
   IF ( SCAN ( ERROR_INDICATOR , arg(LEN_TRIM(arg):LEN_TRIM(arg)) ) .EQ. 0 ) THEN
      IF ( VERIFY ( TRIM(arg) , ALPHANUMERIC_SET ) .NE. 0 ) THEN
         PRINT * , '*** : Non-alphanumeric group found >>> ', TRIM(arg), ' <<< kept'
      ENDIF
      PRINT * , 'Warning : Non-numeric group found >>> ', TRIM(arg), ' <<< kept'
   ELSE
      PRINT * , 'Warning : Non-numeric group found >>> ', TRIM(arg), ' <<< skip'
      CALL get_next_ngrp_in_mesg ( arg )
   ENDIF
   
   IF ( TRACE_MOST ) PRINT  * , 'get_next_ngrp_in_mesg : out ' , TRIM(arg)

ENDSUBROUTINE get_next_ngrp_in_mesg

!---------------------------------------------------------------------- 

SUBROUTINE push_word_back_to_mesg ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg

   IF ( TRACE_ALL ) PRINT  * , 'push_word_back_to_mesg : in ' , TRIM(arg)

   remaining_text = TRIM(arg) // ' ' // remaining_text

   IF ( TRACE_MOST ) PRINT  * , 'push_word_back_to_mesg : out ' , TRIM(remaining_text)

ENDSUBROUTINE push_word_back_to_mesg

!----------------------------------------------------------------------

ENDMODULE bulletin_def

