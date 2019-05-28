!======================================================================


MODULE nan_arithmetic
   
   USE special_symbols
   IMPLICIT NONE
   
   INTERFACE remainder
      MODULE PROCEDURE remainder_ii , remainder_rr
!      FUNCTION remainder_ii ( val1 , val2 ) RESULT ( val )
!         INTEGER , INTENT(in)  :: val1
!         INTEGER , INTENT(in)  :: val2
!         INTEGER               :: val
!      ENDFUNCTION
!      FUNCTION remainder_rr ( val1 , val2 ) RESULT ( val )
!         REAL , INTENT(in)     :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: val
!      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE minus
      MODULE PROCEDURE minus_i , minus_r
!      FUNCTION minus_i ( val1 ) RESULT ( val )
!         INTEGER , INTENT(in)  :: val1
!         INTEGER               :: val
!      ENDFUNCTION
!      FUNCTION minus_r ( val1 ) RESULT ( val )
!         REAL , INTENT(in)     :: val1
!         REAL                  :: val
!      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE add
      MODULE PROCEDURE add_ii , add_ir , add_ri , add_rr  
!      FUNCTION add_ii ( val1 , val2 ) RESULT ( val )
!         INTEGER , INTENT(in)  :: val1
!         INTEGER , INTENT(in)  :: val2
!         INTEGER               :: val
!      ENDFUNCTION
!      FUNCTION add_ir ( val1 , val2 ) RESULT ( val )
!         INTEGER , INTENT(in)  :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: val
!      ENDFUNCTION
!      FUNCTION add_ri ( val1 , val2 ) RESULT ( val )
!         REAL , INTENT(in)     :: val1
!         INTEGER , INTENT(in)  :: val2
!         REAL                  :: val
!      ENDFUNCTION
!      FUNCTION add_rr ( val1 , val2 ) RESULT ( val )
!         REAL , INTENT(in)     :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: val
!      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE subtract
      MODULE PROCEDURE subtract_ii , subtract_ir , subtract_ri , subtract_rr  
!      FUNCTION subtract_ii ( val1 , val2 ) RESULT ( val )
!         INTEGER , INTENT(in)  :: val1
!         INTEGER , INTENT(in)  :: val2
!         INTEGER               :: val
!      ENDFUNCTION
!      FUNCTION subtract_ir ( val1 , val2 ) RESULT ( val )
!         INTEGER , INTENT(in)  :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: val
!      ENDFUNCTION
!      FUNCTION subtract_ri ( val1 , val2 ) RESULT ( val )
!         REAL , INTENT(in)     :: val1
!         INTEGER , INTENT(in)  :: val2
!         REAL                  :: val
!      ENDFUNCTION
!      FUNCTION subtract_rr ( val1 , val2 ) RESULT ( val )
!         REAL , INTENT(in)     :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: val
!      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE multiply
      MODULE PROCEDURE multiply_ii , multiply_ir , multiply_ri , multiply_rr  
!      FUNCTION multiply_ii ( val1 , val2 ) RESULT ( prod )
!         INTEGER , INTENT(in)  :: val1
!         INTEGER , INTENT(in)  :: val2
!         INTEGER               :: prod
!      ENDFUNCTION
!      FUNCTION multiply_ir ( val1 , val2 ) RESULT ( prod )
!         INTEGER , INTENT(in)  :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: prod
!      ENDFUNCTION
!      FUNCTION multiply_ri ( val1 , val2 ) RESULT ( prod )
!         REAL , INTENT(in)     :: val1
!         INTEGER , INTENT(in)  :: val2
!         REAL                  :: prod
!      ENDFUNCTION
!      FUNCTION multiply_rr ( val1 , val2 ) RESULT ( prod )
!         REAL , INTENT(in)     :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: prod
!      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE divide
      MODULE PROCEDURE divide_ii , divide_ir , divide_ri , divide_rr  
!      FUNCTION divide_ii ( val1 , val2 ) RESULT ( prod )
!         INTEGER , INTENT(in)  :: val1
!         INTEGER , INTENT(in)  :: val2
!         INTEGER               :: prod
!      ENDFUNCTION
!      FUNCTION divide_ir ( val1 , val2 ) RESULT ( prod )
!         INTEGER , INTENT(in)  :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: prod
!      ENDFUNCTION
!      FUNCTION divide_ri ( val1 , val2 ) RESULT ( prod )
!         REAL , INTENT(in)     :: val1
!         INTEGER , INTENT(in)  :: val2
!         REAL                  :: prod
!      ENDFUNCTION
!      FUNCTION divide_rr ( val1 , val2 ) RESULT ( prod )
!         REAL , INTENT(in)     :: val1
!         REAL , INTENT(in)     :: val2
!         REAL                  :: prod
!      ENDFUNCTION
   ENDINTERFACE

!----------------------------------------------------------------------

CONTAINS

!======================================================================

FUNCTION add_ii ( val1 , val2 ) RESULT ( val )

!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_ir ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_ri ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_rr ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION subtract_ii ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_ir ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_ri ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_rr ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION multiply_ii ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_ir ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_ri ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_rr ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION divide_ii ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0 ) THEN
      IF ( val1.EQ.0 ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_ir ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0. ) THEN
      IF ( val1.EQ.0 ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_ri ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0 ) THEN
      IF ( val1.EQ.0. ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_rr ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0. ) THEN
      IF ( val1.EQ.0. ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION remainder_ii ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = mod ( val1 , val2 )
   ENDIF
ENDFUNCTION

FUNCTION remainder_rr ( val1 , val2 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = mod ( val1 , val2 )
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION minus_i ( val1 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = -val1
   ENDIF
ENDFUNCTION

FUNCTION minus_r ( val1 ) RESULT ( val )
!   USE special_symbols
   IMPLICIT NONE
!   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = -val1
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------


ENDMODULE nan_arithmetic

