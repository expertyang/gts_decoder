!======================================================================


MODULE record_def

   USE nan_arithmetic
   USE parameters
   USE special_symbols
   IMPLICIT NONE

!   INCLUDE 'inc.traceflow'
!   INCLUDE 'inc.special_symbols'

! ----------------------------------------------------------------------

   INTEGER , PARAMETER         :: rlen = 80

   INTEGER                     :: outfiles(1000) = UNDEFINED
   INTEGER                     :: current_unit   = 12

   TYPE stack
      INTEGER                  :: field_ival
      REAL                     :: field_rval
      CHARACTER ( LEN = rlen ) :: field_cval
      TYPE ( stack ) , POINTER :: next_record
   ENDTYPE stack

   TYPE ( stack ) , TARGET     :: record
   TYPE ( stack ) , POINTER    :: record_tail

!  INTEGER                     :: record_allocate = 1
   INTEGER                     :: record_allocate = 0
   INTEGER                     :: record_error  , ierr  , ierr0  = 3
   INTEGER                     :: record_warn   , iwarn , iwarn0 = 4
   INTEGER                     :: record_fm      ! current code form number
   INTEGER                     :: section_id     ! current section number
   INTEGER                     :: section_argnum ! current argument number
   INTEGER                     :: section_subgrp ! current subgroup number

   INTEGER                     :: cur_sttn_id    ! current station id
   INTEGER                     :: cur_sttn_alt   ! current station height

   INTEGER , PARAMETER         :: number = 1 , error = 2 , warning = 3
   INTEGER , DIMENSION (100,3) :: msg_stat = 0
   INTEGER                     :: seqnum   = 0

   CHARACTER ( LEN = rlen )    :: msg_header
   CHARACTER ( LEN = rlen )    :: center_id
   CHARACTER ( LEN = rlen )    :: prev
   !
   ! For simplicity , all decoded value is pushed onto a stack with
   !   appropriate comment. The first record in the stack will be
   !   the code form identification number ( fm ) .
   !   Routines can subsequently be developed to output the decoded
   !   values in manners suitable for specific applications. This
   !   stack implementation effectively separates the decoding part
   !   of the PROGRAM ( the core ) from its output routines.

   INTEGER , PARAMETER          :: stn_data = 10
   INTEGER , PARAMETER          :: icao_data=  9 
! ADD 
   INTEGER , PARAMETER          :: stn_idx = 12
   INTEGER , PARAMETER          :: icao_idx= 11
!   
   INTEGER , PARAMETER          :: stn_llen = 100
   CHARACTER ( LEN = 6 )        :: stn_fmt = '(A100)'
   CHARACTER ( LEN = stn_llen ) :: stn_dscrptn
   CHARACTER ( LEN = rlen )     :: RECFMT = ' ( i8 , 1x , f12.3 , 1x , a ) '

   ! Current Date and Time ( READ from input )
   !
   INTEGER                      :: utc_year , utc_month , utc_day
   INTEGER                      :: utc_hour , utc_minute, utc_second
   INTEGER                      :: utc_yymmdd , utc_hhmmss
   !
   INTEGER                      :: msg_year , msg_month , msg_day
   INTEGER                      :: msg_hour , msg_minute, msg_second
   INTEGER                      :: msg_yymmdd , msg_hhmmss
   !
   INTEGER                      :: time_window

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE record_initialize ( fm )

   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: fm

   IF ( TRACE_MOST ) PRINT  * , 'record_initialize : in ' , fm

   record%field_cval = '0000:fm   :CODE FORM'
   record%field_ival = fm
   record%field_rval = fm
   NULLIFY  ( record%next_record )

   record_tail    => record

!  msg_stat ( fm , number ) = msg_stat ( fm , number ) + 1
   record_error   =  0
   record_warn    =  0

   record_fm      =  fm
   ierr           =  outfiles ( fm+200 )
   iwarn          =  outfiles ( fm+300 )
   section_id     =  0
   section_argnum =  0
   section_subgrp =  1
   prev           =  NULL

   IF ( TRACE_ALL ) PRINT  * , 'record_initialize : out', fm, ierr, iwarn

ENDSUBROUTINE record_initialize

!----------------------------------------------------------------------

SUBROUTINE record_deallocate

   ! Routine to deallocate space used by the current record
   !
   ! Created : Dec 29, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   TYPE ( stack ) , POINTER :: rprev, rtmp

   IF ( TRACE_MOST ) PRINT  * , 'record_deallocate   : in  ', record_allocate

   IF ( ASSOCIATED ( record%next_record ) ) THEN
      rprev => record%next_record
      DO WHILE ( ASSOCIATED ( rprev%next_record ) )
         rtmp  => rprev
         rprev => rprev%next_record
         DEALLOCATE ( rtmp ) 
         record_allocate = record_allocate - 1
      END DO
      DEALLOCATE ( rprev )
      record_allocate = record_allocate - 1
   ENDIF

   IF ( TRACE_MOST ) PRINT  * , 'record_deallocate   : out ', record_allocate

ENDSUBROUTINE record_deallocate

!----------------------------------------------------------------------

SUBROUTINE record_appendr ( ival , rval , cval )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )               :: ival
   REAL , INTENT ( IN )                  :: rval
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: cval
   TYPE ( stack ) , POINTER              :: rtmp

   IF ( TRACE_MOST ) PRINT  *, 'record_append : in ', ival, rval, TRIM(cval)

   ALLOCATE ( rtmp )
   record_allocate = record_allocate + 1
   rtmp%field_ival = ival
   rtmp%field_rval = rval
   rtmp%field_cval = cval
   NULLIFY ( rtmp%next_record )

   record_tail%next_record => rtmp
   record_tail             => rtmp

   IF ( TRACE_ALL ) PRINT  * , 'record_append : out'

ENDSUBROUTINE record_appendr

! ----------------------------------------------------------------------

SUBROUTINE record_appendi ( ival , cval )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )               :: ival
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: cval

   CALL record_appendr ( ival , REAL ( ival ) , cval )

ENDSUBROUTINE record_appendi

! ----------------------------------------------------------------------

SUBROUTINE record_appendj ( ival , jval , cval )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )               :: ival
   INTEGER , INTENT ( IN )               :: jval
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: cval

   CALL record_appendr ( ival , REAL ( jval ) , cval )

ENDSUBROUTINE record_appendj

!----------------------------------------------------------------------

SUBROUTINE record_print ( iounit )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: iounit
   INTEGER :: ival, rval
   LOGICAL :: large_ival, large_rval
   TYPE ( stack ) , POINTER :: rtmp

   IF ( TRACE_ALL ) PRINT  * , 'record_print : in', iounit
   IF ( iounit < 1 ) THEN
      PRINT *, 'RECORD PRINT IOUNT < 0, IGNORE WRITE ', iounit
      RETURN
   ENDIF

   rtmp => record

   WRITE ( iounit , ' ( a ) ' ) ' ============================================ '

   DO WHILE ( ASSOCIATED ( rtmp ) )
      WRITE ( iounit , RECFMT ) &
            rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )

      ival = rtmp%field_ival 
      rval = nint(rtmp%field_rval)
      large_rval = ( ABS(rval)*10 .GE. abs(MISSING) )
      large_ival = ( ABS(ival)*10 .GE. abs(MISSING) )

      IF ( ( large_ival .AND. ival.NE.MISSING .AND. ival.NE.UNDEFINED ) .OR. &
           ( large_rval .AND. rval.NE.MISSING .AND. rval.NE.UNDEFINED ) ) THEN
      IF ( ival.LE.19950000 .OR. ival.GE.21000000 ) THEN
      IF ( rtmp%field_cval(2:9) .NE. '002:BHDR' ) THEN
      IF ( rtmp%field_cval(2:9) .NE. '...:Abnn' ) THEN
         PRINT *, ival, rtmp%field_rval, ' ===> ', TRIM(rtmp%field_cval)
      ENDIF
      ENDIF
      ENDIF
      ENDIF

      rtmp => rtmp%next_record
   ENDDO

!  IF ( TRACE_LEAST ) CALL flush ( iounit )

   IF ( TRACE_ALL ) PRINT  * , 'record_print : out'

ENDSUBROUTINE record_print

!----------------------------------------------------------------------

SUBROUTINE assign_outfile ( unit_prefix )

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit_prefix
   INTEGER             :: temp_prefix

   IF ( TRACE_ALL ) PRINT  * , 'assign_outfile : in ', unit_prefix, record_fm
   temp_prefix = unit_prefix

   if ( temp_prefix .EQ. 0 ) then
      CALL open_unit ( record_fm )
   ENDIF

   DO WHILE ( temp_prefix .GT. 0 )
      CALL open_unit ( record_fm+100*MOD(temp_prefix,10) )
      temp_prefix = temp_prefix / 10
   ENDDO

ENDSUBROUTINE assign_outfile

!----------------------------------------------------------------------

SUBROUTINE open_unit ( unitnum )

   IMPLICIT NONE
   INTEGER , PARAMETER     :: MAXRECL = 1024
   INTEGER , INTENT ( IN ) :: unitnum
   CHARACTER               :: outfile_name*80
   LOGICAL                 :: existed

   IF ( outfiles ( unitnum ) .GT. 0 ) THEN
      ! unit already opened
      RETURN

   ELSE IF ( outfiles ( unitnum ) .EQ. 0 ) THEN
      ! unit number in error
      CALL prog_abort ( 'Calling OPEN_UNIT with 0 as unitnum' )

   ELSE IF ( outfiles ( unitnum ) .EQ. UNDEFINED ) THEN
      ! unit number is new
      current_unit      = current_unit + 1
      outfiles(unitnum) = current_unit

   ELSE
      ! old unit number that has been closed
      outfiles(unitnum) = -outfiles(unitnum)

   ENDIF

   WRITE ( outfile_name , 10 ) unitnum
   INQUIRE ( FILE=outfile_name, EXIST=existed )

   IF ( existed ) THEN
      OPEN ( UNIT=outfiles(unitnum), FILE=outfile_name, FORM='FORMATTED', &
             RECL=MAXRECL, ACCESS='SEQUENTIAL', STATUS='OLD', POSITION='APPEND')
   ELSE
      OPEN ( UNIT=outfiles(unitnum), FILE=outfile_name, FORM='FORMATTED', &
             RECL=MAXRECL, ACCESS='SEQUENTIAL', STATUS='NEW' )
   ENDIF

   IF     ( unitnum .GE. 200 .AND. unitnum .LE. 299 ) THEN
      ierr  = outfiles(unitnum)
   ELSEIF ( unitnum .GE. 300 .AND. unitnum .LE. 399 ) THEN
      iwarn = outfiles(unitnum)
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'open_unit : out ', &
      unitnum, current_unit, outfiles(unitnum), ' ', TRIM(outfile_name)

10 FORMAT ( 'gts_out.' , I3.3 )

ENDSUBROUTINE open_unit

!----------------------------------------------------------------------

SUBROUTINE flush_outfiles

   IMPLICIT NONE
   INTEGER :: temp_prefix, unitnum

   IF ( TRACE_ALL ) PRINT  * , 'flush_outfiles : in ', record_fm

   DO temp_prefix = 0, 900, 100
      unitnum = outfiles ( record_fm + temp_prefix )
      IF ( unitnum > 0 ) THEN
         CLOSE ( unitnum )
         outfiles ( record_fm + temp_prefix ) = -unitnum
      ENDIF
   ENDDO

ENDSUBROUTINE flush_outfiles

!----------------------------------------------------------------------

SUBROUTINE new_section ( id )

   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: id

   IF ( TRACE_ALL ) PRINT  * , 'new_section : in  ', section_id, id

   section_id     = id
   section_argnum = 1
   section_subgrp = 1
   prev           = NULL

   IF ( TRACE_ALL ) PRINT  * , 'new_section : out ', section_id, id

ENDSUBROUTINE new_section

!----------------------------------------------------------------------

FUNCTION int2str ( val )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: val
   CHARACTER ( LEN = rlen ) :: int2str
   int2str = BLANK_LINE
   WRITE ( int2str , '(I10)' ) val
   int2str = adjustl ( int2str )

ENDFUNCTION int2str

!----------------------------------------------------------------------

FUNCTION real2str ( val )

   IMPLICIT NONE
   REAL , INTENT ( IN )     :: val
   CHARACTER ( LEN = rlen ) :: real2str
   real2str = BLANK_LINE
   WRITE ( real2str , '(g16.6)' ) val
   real2str = adjustl ( real2str )

ENDFUNCTION real2str

! ----------------------------------------------------------------------

FUNCTION str2int ( string )

   ! FUNCTION to RETURN the INTEGER value of a string of numeric
   !    character.  If the string contains non numeric characters, the
   !    value RETURN by the FUNCTION is <UNDEFINED>, as defined in the
   !    include file 'inc.special_symbols'
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: string
   INTEGER                               :: str2int
   CHARACTER ( LEN = 4 )                 :: fmt
   CHARACTER ( LEN = len ( string ) )    :: tmp_string

   tmp_string = ADJUSTL ( string )
   IF ( VERIFY ( TRIM(tmp_string) , NUMBER_SET ) .NE. 0 ) GOTO 10

   fmt = '(I' // ACHAR ( IACHAR ( '0' ) + LEN ( string ) ) // ')'
   tmp_string = ADJUSTR ( string )
   READ ( tmp_string , fmt , ERR = 10 ) str2int
   RETURN

10 CONTINUE
   IF ( string .EQ. repeat ( '/' , len ( string ) ) ) THEN
      str2int = MISSING
   ELSE
      str2int = UNDEFINED
   ENDIF

ENDFUNCTION str2int

!----------------------------------------------------------------------

ENDMODULE record_def

!======================================================================
