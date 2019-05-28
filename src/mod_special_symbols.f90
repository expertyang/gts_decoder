MODULE special_symbols

   IMPLICIT NONE

   CHARACTER , PARAMETER ::   &
      NULL  = ACHAR ( 000 ) , &
!     SOH   = ACHAR ( 001 ) , &
!     ETX   = ACHAR ( 003 ) , &
      TAB   = ACHAR ( 009 ) , &
      LF    = ACHAR ( 010 ) , &
      CR    = ACHAR ( 013 ) , &
      SPACE = ACHAR ( 032 ) , &
      DLLR  = ACHAR ( 036 ) , &
      EQSGN = ACHAR ( 061 )

   CHARACTER(len=4) , PARAMETER ::   &
                  SOH   = 'ZCZC' ,   & 
                  ETX   = 'NNNN'

   CHARACTER , PARAMETER :: BLANK_LINE * 132 = REPEAT ( SPACE , 132 )

   CHARACTER , PARAMETER ::                                       &
      MESG_SEPARATORS * ( 1 ) = DLLR ,                            &
      MESG_DELIMITORS * ( 2 ) = DLLR // EQSGN ,                   &
      LINE_DELIMITORS * ( 3 ) = CR // LF // NULL ,                &
      WORD_DELIMITORS * ( 5 ) = SPACE // TAB // LINE_DELIMITORS 

   CHARACTER , PARAMETER ::                                       &
      ERROR_INDICATOR * ( 2 ) = 'Ee' ,                            &
      NUMBER_SET * ( 17 )     = '-+0123456789/' // SOH // ETX // MESG_SEPARATORS

   CHARACTER , PARAMETER ::                                       &
      ALPHANUMERIC_SET * ( 69 ) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // './' // &
                                  'abcdefghijklmnopqrstuvwxyz' // NUMBER_SET

   INTEGER , PARAMETER :: UNDEFINED = -999999
   INTEGER , PARAMETER :: MISSING   = -888888
   INTEGER , PARAMETER :: ENDOUTPUT = -777777

   INTEGER , PARAMETER :: ABS_UNDEFINED = 999999
   INTEGER , PARAMETER :: ABS_MISSING   = 888888
   INTEGER , PARAMETER :: ABS_ENDOUTPUT = 777777

   REAL , PARAMETER    :: KNOT      = 1./1.94
   REAL , PARAMETER    :: KMPH      = 1./3.6
   REAL , PARAMETER    :: TOKELVIN  = 273.15
   REAL , PARAMETER    :: FEET      = 0.3048
   REAL , PARAMETER    :: INCH      = 0.0254
   REAL , PARAMETER    :: TOPa      = 100
   REAL , PARAMETER    :: TOMETER   = 0.001
   REAL , PARAMETER    :: INCHHG    = 33.86

ENDMODULE special_symbols
