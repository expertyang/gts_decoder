MODULE parameters

   IMPLICIT NONE
   
   INTEGER , PARAMETER :: traceflow = 9

   LOGICAL , PARAMETER :: TRACE_ALL       = ( traceflow <= 1 ) , &
                          TRACE_MOST      = ( traceflow <= 3 ) , &
                          TRACE_NEW_ALL   = ( traceflow <= 4 ) , &
                          TRACE_NEW_MOST  = ( traceflow <= 5 ) , &
                          TRACE_NEW_LEAST = ( traceflow <= 6 ) , &
                          TRACE_LEAST     = ( traceflow <= 7 ) , &
                          TRACE_NONE      = ( traceflow <= 9 )
   !
   ! <traceflow> determines the level of details in diagnostic messages
   !   the lower the number , the more detail the messages would be.
   !   <traceflow> set to 1 or lower will be most detailed.
   !
   ! The following line is a typical one that uses traceflow:
   !
   ! SUBROUTINE routine_name ( ... ... )
   !    IF ( traceflow <= nnn ) PRINT * , '<routine name> : in'
   !      ... ...
   !    IF ( traceflow <= nnn ) PRINT * , '<routine name> : out'
   ! ENDSUBROUTINE

   ! where
   !   nnn = 1-3 for well tested routines
   !   nnn = 7-9 for routines you always want to trace (like error handlers.)

   ! The following parameter determines whether output files will be
   !   closed after decoding each bulletin (effectively, flushing the
   !   output).
   ! LOGICAL , PARAMETER   :: FLUSH_FILES   = .TRUE.
     LOGICAL , PARAMETER   :: FLUSH_FILES   = .FALSE.

   ! The following parameters determine the amount of output generated by
   !   the decoder.
   LOGICAL , PARAMETER :: FOR_DEBUG  = .FALSE.   ! write decoder usage record
   LOGICAL , PARAMETER :: FOR_RAWINS = .FALSE.   ! write files for RAWINS
   LOGICAL , PARAMETER :: FOR_RAP    = .TRUE.   ! write files for RAP

ENDMODULE parameters
