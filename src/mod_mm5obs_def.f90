!======================================================================

MODULE mm5obs_def

   IMPLICIT NONE

!   INCLUDE 'inc.mm5obs'     ! Part of Dave Hart's observation module

!--------------------------------------------------------------------------
!                             PARAMETERS
!---------------------------------------------------------------------------

   !  The following parameters are used as flags to indicate either a
   !  MISSING data value (real number) in the "measurement" TYPE or an
   !  "end of data" flag (two fields with end_data  flag mean end of
   !  measurements for one observation).

   INTEGER , PARAMETER , PRIVATE                  ::  missing = -888888
   INTEGER , PARAMETER                            ::  end_data = -777777

   !  define error return codes used by 'read_measurements' routine
   INTEGER , PARAMETER                            ::  ok = 0 , &
                                                      eof_err = 1 , &
                                                      no_data = 2 , &
                                                      read_err = 3

!  FORMAT STRINGS for input/output of data
!  These format strings correspond to the data structures in this file
!  and are used for input and output of values in the 'report' structure
!  (first format string) and the 'measurement' structure (second format).
!  Note that report struct contains the first of a linked list of 
!  measurements; this first meas is read using the 'meas_format';

   CHARACTER ( LEN = 120 ) , PARAMETER :: rpt_format =  &
                ' ( 2f20.5 , 2a40 , ' &             ! format for location_type
             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' & ! format for source_info
             // ' 2i10 , a20 , ' &                  ! fmt for valid_time
             // ' 13( f13.5 , i7 ) ) '              ! fmt for 'terrestrial' 

   CHARACTER ( LEN = 120 ) , PARAMETER :: meas_format = & 
                ' ( 10( f13.5 , i7 ) ) '            ! fmt for measurement rcd

   CHARACTER ( LEN = 120 ) , PARAMETER :: end_format = &
                ' ( 3 ( i7 ) ) '                    ! fmt for end record

!-------------------------------------------------------------------------
!                          DATA STRUCTURES
!--------------------------------------------------------------------------
   !  These data structures are built to hold all of the required information 
   !  from a single level report.  This includes, but is not limited to,
   !  land based surface observations, ocean based surface observations,
   !  and aircraft data.  All data is assumed to be given a horizontal
   !  location (lat/lon) and a timestamp.  For the data to be useful, there
   !  needs to be a vertical location presribed, or derivable.  For data with
   !  multiple vertical observations, the 'meas' structure is also required.
   !  This includes, but is not limited to, sounding data, satellite derived
   !  winds and satellite derived thickness.

   !  The information in the following two records is usually available 
   !  in every report.  

!-------------------------------------------------------------------------
   TYPE location_type
      !  The fields in this record uniquely identify the source of the 
      !  data, so that duplicates of data can be merged or discarded.
      !  The horizontal location of this report (assumed constant, even
      !  for balloon ascent) is geven by the lat/lon of the site.
      
      REAL                   :: latitude  , &   ! latitude (+ degrees east)
                                longitude       ! longitude (+ degrees north)

      CHARACTER ( LEN = 40 ) :: id , &          ! 5 digit identifier, 
                                                ! consisting of a 2 digit block 
                                                ! number and a 3 digit 
                                                ! identifier (for soundings)
                                                ! for WMO sttn; non digit
                                                ! for other sources
                                name            ! The name corresponds to
                                                ! the id (is obtained from id
                                                ! in the program that is 
                                                ! source of data
   END TYPE location_type


!---------------------------------------------------------------------------
   TYPE source_info
      CHARACTER ( LEN = 40 ) :: platform , &    ! description of the 
                                                ! measurement device
                                source          ! GTS data, NCAR ADP files, 
                                                ! bogus information, etc
      REAL                   :: elevation       ! station elevation

      !  During the decoding process, how many errors (non conforming
      !  codes) were encountered, and how many warnings (this is a subjective
      !  call based on repeated incorrect -- but probably not garbled --
      !  GTS codes).  If a bulletin is completely garbled, the logical
      !  flag to not consider this report is set.
      INTEGER              :: num_vld_fld , & ! number of valid fields in the
                                              ! entire report; used as the
                                              ! first tie-breaker in deciding
                                              ! which conflicting data items
                                              ! to keep if have duplicate rpts
                              num_error , &   ! number of errors 
                                              ! encountered during the
                                              ! decoding process
                              num_warning , & ! number of warnings 
                                              ! encountered during the 
                                              ! decoding process
                              seq_num , &     ! sequence numbers that tell
                                              ! which of 2 reports is more
                                              ! recent.
                              num_dups        ! number of duplicates found of
                                              ! this observation 
      LOGICAL              :: is_sound        ! is-a-sounding tells whether
                                              ! the observation possibly has
                                              ! multiple levels vs having 
                                              ! only one level for srfc ob.
      LOGICAL              :: bogus           ! T/F if this is a bogus 
                                              ! observation
      LOGICAL              :: discard         ! Tells whether this observation
                                              ! has been found to be a dup
                                              ! AND has been discarded or
                                              ! merged.
   END TYPE source_info

!--------------------------------------------------------------------------
   TYPE field
      !  Defines a data type consisting of a paired data value (real) with a
      !  quality control flag that holds a binary-coded combination of error
      !  codes; the codes  identify possible problems with the data.
      REAL                   :: data
      INTEGER                :: qc              !  Quality control flags
                                                !  that are 0 if data is
                                                !  good, or different 
                                                !  integers depending upon
                                                !  what error(s) occured
   END TYPE field

!-------------------------------------------------------------------------
   TYPE terrestrial
   !  The data that will occur, at most, once during a report is 
   !  listed here.  These are typically terrestrial measured values.  The
   !  surface met fields are stored in a separate TYPE, to allow a 
   !  POINTER to the next level (if one exists).  This needs to be a 
   !  separate TYPE so as to allow a POINTER to it 
      TYPE ( field )         :: slp       , &   ! sea level pressure
                                ref_pres  , &   ! reference pres level for
                                                ! the thickness
                                ground_t  , &   ! ground temperature
                                sst       , &   ! sea surface temperature
                                psfc      , &   ! surface pressure
                                precip    , &   ! precipitation accumulation
                                t_max     , &   ! daily temperature max
                                t_min     , &   ! daily temperature min
                                t_min_night , & ! min overnight temperature
                                p_tend03  , &   ! pressure tendency in 3hr
                                p_tend24  , &   ! pressure tendency in 24hr
                                cloud_cvr , &   ! total cloud cover (oktas)
                                ceiling         ! height of lowest cloud base
   END TYPE terrestrial 


!-------------------------------------------------------------------------
!  GTS report time: the valid time of the report.  The largest INTEGER values 
!  require only 8 digits, so that this should function properly with 
!  32-bit INTEGERS.  
   TYPE time_info
      INTEGER                :: sut      , &    ! number of seconds since 1 Jan
                                                ! 0000 UTC 1970
                                julian          ! Julian day
      CHARACTER ( LEN = 14 )    date_char       ! CCYYMMDDHHmmss date
   END TYPE time_info

!--------------------------------------------------------------------------
   TYPE meas_data
   !  The met data involved with this program is defined in this TYPE.  The
   !  standard state variables (wind, temperature, moisture, with pressure
   !  and/or height to fix the vertical location) are stored here.  For 
   !  single level observations, only one of these records is used per     
   !  observation.   For multi-level reports, a linked list of these 
   !  measurement TYPEs is generated.
      TYPE ( field )         :: pressure    , & ! pressure of observation
                                height      , & ! height (above sea level) 
                                temperature , & ! 
                                dew_point   , & ! 
                                speed       , & ! 
                                direction   , & ! 
                                u           , & ! u and v components of wind
                                v           , & ! are derived from spd and dir
                                rh          , & !
                                thickness       ! 
   END TYPE meas_data


!--------------------------------------------------------------------------
   TYPE measurement
      TYPE ( meas_data )               :: meas  ! contains data and qc code
      TYPE ( measurement ) ,  POINTER  :: next  ! the met info is handled
                                                ! as a linked list of the  
                                                ! measurement type
   END TYPE measurement

!-------------------------------------------------------------------------
!  Combine above defined structure into one 'report' structure.
   TYPE report                                 ! this is the entire report
      TYPE ( location_type ) :: location       ! for a single time, from a 
      TYPE ( source_info )   :: info           ! single reporting platform,
      TYPE ( time_info )     :: valid_time     ! a sounding, surface, buoy,
      TYPE ( terrestrial )   :: ground         ! aircraft or ship report, it 
      TYPE ( measurement ) , &
               POINTER       :: surface        
   END TYPE report                            
                                             
!======================================================================


   TYPE mm5_hdr
      TYPE ( location_type ) :: location
      TYPE ( source_info   ) :: info
      TYPE ( time_info     ) :: valid_time
      TYPE ( terrestrial   ) :: ground
   END TYPE mm5_hdr

   TYPE ( mm5_hdr   )        :: empty_hdr
   TYPE ( meas_data )        :: empty_upa

!  REAL , PARAMETER :: fake_stnpre = 101301.00
   REAL , PARAMETER :: fake_stnpre = missing 

   INTEGER :: obs_yymmdd , obs_hhmmss , rev_yymmdd , rev_hhmmss
   INTEGER :: fm           ! FM-xx  WMO format number
   INTEGER :: nvalid       ! number of fields decoded
   INTEGER :: nlevel       ! number of vertical level written

   ! mainly used in sounding data
   INTEGER :: vert_id      ! indicate whether data is pressure
                           !    (+ve) or height (-ve) based
   LOGICAL :: wrote_hdr    ! whether header has been written out

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE init_wr_rap_emptyobs

   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'

   TYPE ( location_type ) :: empty_location
   TYPE ( source_info   ) :: empty_info
   TYPE ( time_info     ) :: empty_time
   TYPE ( field         ) :: empty_field
   TYPE ( terrestrial   ) :: empty_ground

   rev_yymmdd               = MISSING
   rev_hhmmss               = MISSING

   empty_location%latitude  = MISSING
   empty_location%longitude = MISSING
   empty_location%id        = BLANK_LINE
   empty_location%name      = BLANK_LINE

   empty_info%platform      = BLANK_LINE
   empty_info%source        = 'GTS (ROHK)'
   empty_info%elevation     = MISSING
   empty_info%num_vld_fld   = MISSING
   empty_info%num_error     = MISSING
   empty_info%num_warning   = MISSING
   empty_info%seq_num       = MISSING
   empty_info%num_dups      = MISSING

   empty_info%is_sound      = .FALSE.    ! need to discuss with Dave
   empty_info%bogus         = .FALSE.    ! need to discuss with Dave
   empty_info%discard       = .FALSE.    ! need to discuss with Dave
  
   empty_time%sut           = MISSING
   empty_time%julian        = MISSING
   empty_time%date_char     = BLANK_LINE

   empty_field%data         = MISSING
   empty_field%qc           = 0          ! need to discuss with Dave

   empty_ground%slp         = empty_field
   empty_ground%ref_pres    = empty_field
   empty_ground%ground_t    = empty_field
   empty_ground%sst         = empty_field
   empty_ground%psfc        = empty_field
   empty_ground%precip      = empty_field
   empty_ground%t_max       = empty_field
   empty_ground%t_min       = empty_field
   empty_ground%t_min_night = empty_field  
   empty_ground%p_tend03    = empty_field  
   empty_ground%p_tend24    = empty_field  
   empty_ground%cloud_cvr   = empty_field
   empty_ground%ceiling     = empty_field

   empty_upa%pressure       = empty_field
   empty_upa%height         = empty_field
   empty_upa%temperature    = empty_field
   empty_upa%dew_point      = empty_field
   empty_upa%speed          = empty_field
   empty_upa%direction      = empty_field
   empty_upa%u              = empty_field
   empty_upa%v              = empty_field
   empty_upa%rh             = empty_field
   empty_upa%thickness      = empty_field

   empty_hdr%location       = empty_location
   empty_hdr%info           = empty_info
   empty_hdr%valid_time     = empty_time
   empty_hdr%ground         = empty_ground

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE write_rpt_hdr ( iwrite , rpt_seq_num, hdr , ieor )

   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER ,          INTENT ( IN )    :: iwrite, rpt_seq_num
   TYPE ( mm5_hdr ) , INTENT ( INOUT ) :: hdr 
   INTEGER ,          INTENT ( OUT )   :: ieor

   CHARACTER                           :: cdate*14

   IF ( iwrite < 1 ) THEN
      ! PRINT *, 'write_rpt_hdr iounit < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( nvalid .LE. 0 ) THEN
      ! print *, ' **** write_rpt_hdr : NO valid field'
      ieor = 1

   ELSE IF ( rev_yymmdd .EQ. UNDEFINED .OR. rev_hhmmss .EQ. UNDEFINED ) THEN
      ! print *, ' **** write_rpt_hdr : UNEXPECTED rev_yymmdd IS UNDEFINED'
      ieor = 2

   ELSE IF ( rev_yymmdd .EQ. MISSING .OR. rev_hhmmss .EQ. MISSING ) THEN
      ! print *, ' **** write_rpt_hdr : UNEXPECTED rev_yymmdd IS MISSING'
      ieor = 2

   ELSE IF ( nint ( hdr%location%latitude  ) .EQ. MISSING   .OR.  &
             nint ( hdr%location%longitude ) .EQ. MISSING ) THEN
        print *, ' **** write_rpt_hdr : latitude or longitude IS MISSING'
      ieor = 3

   ELSE
      SELECT CASE ( fm ) 
         CASE (12) ; hdr%info%platform = 'FM-12 SYNOP'
                     hdr%info%is_sound = .FALSE.
         CASE (13) ; hdr%info%platform = 'FM-13 SHIP '
                     hdr%info%is_sound = .FALSE.
         CASE (15) ; hdr%info%platform = 'FM-15 METAR'
                     hdr%info%is_sound = .FALSE.
         CASE (16) ; hdr%info%platform = 'FM-16 SPECI'
                     hdr%info%is_sound = .FALSE.
         CASE (32) ; hdr%info%platform = 'FM-32 PILOT'
                     hdr%info%is_sound = .TRUE.
         CASE (33) ; hdr%info%platform = 'FM-33 PILOT SHIP'
                     hdr%info%is_sound = .TRUE.
         CASE (34) ; hdr%info%platform = 'FM-34 PILOT MOBIL'
                     hdr%info%is_sound = .TRUE.
         CASE (35) ; hdr%info%platform = 'FM-35 TEMP'
                     hdr%info%is_sound = .TRUE.
         CASE (36) ; hdr%info%platform = 'FM-36 TEMP SHIP'
                     hdr%info%is_sound = .TRUE.
         CASE (37) ; hdr%info%platform = 'FM-37 TEMP DROP'
                     hdr%info%is_sound = .TRUE.
         CASE (38) ; hdr%info%platform = 'FM-38 TEMP MOBIL'
                     hdr%info%is_sound = .TRUE.
         CASE (42) ; hdr%info%platform = 'FM-42 AMDAR'
                     hdr%info%is_sound = .TRUE.
         CASE (86) ; hdr%info%platform = 'FM-86 SATEM'
                     hdr%info%is_sound = .TRUE.
         CASE (88) ; hdr%info%platform = 'FM-88 SATOB'
! switched is_sound to true for fm-88 satob data jfb 11/16/99
                     hdr%info%is_sound = .TRUE.
! added fm-96 data as a valid airep  jfb 2/23/99
         CASE (96) ; hdr%info%platform = 'FM-97 AIREP'
                     hdr%info%is_sound = .TRUE.
         CASE (97) ; hdr%info%platform = 'FM-97 AIREP'
                     hdr%info%is_sound = .TRUE.
      END SELECT
   
      write ( cdate , '(I8.8,I6.6)' ) rev_yymmdd, rev_hhmmss
!     rpt_seq_num = rpt_seq_num + 1
   
      hdr%info%seq_num         = rpt_seq_num
      hdr%info%num_vld_fld     = nvalid
      hdr%valid_time%date_char = cdate
   
      WRITE ( iwrite , fmt = rpt_format )  hdr
      ieor = 0
      nlevel = 0
      wrote_hdr = .TRUE.
      vert_id = MISSING

      write(360,"(2A,2F13.5)") hdr%info%platform, hdr%location%id, hdr%location%latitude, hdr%location%longitude

!     WRITE(*,*) hdr%location%name, hdr%info%platform
!     WRITE(*,*)  'iwrite :', iwrite
!     WRITE(*,*)  'little_r hdr :',hdr

   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE write_rpt_upa ( iwrite , upa_in , ieor )
   USE special_symbols

   IMPLICIT NONE
   INTEGER ,            INTENT ( IN ) :: iwrite , ieor
   TYPE ( meas_data ) , INTENT ( IN ) :: upa_in
   TYPE ( meas_data ) :: upa

   upa = upa_in
   if(upa%temperature%data/=missing.and.upa%dew_point%data/=missing)then
      if(upa%dew_point%data>upa%temperature%data)then
          upa%temperature%data=missing
          upa%dew_point  %data=missing
      endif
   endif

   if(upa%speed    %data==MISSING.or.upa%speed    %data==UNDEFINED) upa%direction%data=upa%speed    %data
   if(upa%direction%data==MISSING.or.upa%direction%data==UNDEFINED) upa%speed    %data=upa%direction%data
   if(upa%direction%data/=MISSING.and.upa%direction%data/=UNDEFINED)then
      if(upa%direction%data<0.or.upa%direction%data>360.)then
         upa%direction%data=MISSING
         upa%speed    %data=MISSING
      endif
   endif

   if(upa%pressure %data==0.) upa%pressure%data=MISSING

   if(upa%speed%data>=100.)then
      write(360,"(A,6F13.5)")  "SPD>100:",upa%pressure%data, upa%height%data, upa%temperature%data, upa%dew_point%data, upa%direction%data, upa%speed%data
   endif
   if(upa%direction%data==360.)then
      write(360,"(A,6F13.5)")  "DIR=360:",upa%pressure%data, upa%height%data, upa%temperature%data, upa%dew_point%data, upa%direction%data, upa%speed%data
   endif
   if(upa%pressure%data<10000.and.upa%pressure%data/=MISSING.and.upa%pressure%data/=UNDEFINED.and.upa%dew_point%data/=MISSING.and.upa%dew_point%data/=UNDEFINED)then
      write(360,"(A,6F13.5)")  "HUM<100hPa:",upa%pressure%data, upa%height%data, upa%temperature%data, upa%dew_point%data, upa%direction%data, upa%speed%data
   endif
   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_rpt_upa iounit < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

!  WRITE(*, *) 'ieor',ieor
   IF ( ieor .EQ. 0 ) THEN

      WRITE ( iwrite , fmt = meas_format ) upa
      nlevel = nlevel + 1
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE write_rpt_end ( iwrite , nerror , nwarn , ieor )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: iwrite, nerror, nwarn, ieor
   TYPE ( meas_data )       :: upa_tmp

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_rpt_end iounit < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( ieor .EQ. 0 ) THEN
      upa_tmp                  = empty_upa
      upa_tmp%pressure%data    = end_data
      upa_tmp%height%data      = end_data
      upa_tmp%temperature%data = nlevel
      WRITE ( iwrite , fmt = meas_format ) upa_tmp
      WRITE ( iwrite , fmt = end_format ) nvalid, nerror, nwarn
      wrote_hdr = .FALSE.
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

ENDMODULE mm5obs_def
