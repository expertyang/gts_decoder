PROGRAM proc_metar
    USE date_pack
    IMPLICIT NONE
    CHARACTER(LEN=180) :: line
    INTEGER :: n, ierr
    CHARACTER(LEN=180) :: infile , outfile
    CHARACTER(LEN=date_str_len) :: date_string
    CHARACTER(LEN=date_char_len) :: datechar
    TYPE(date) :: datein , datew

    date_string='0000-00-00_00:00:00.000'
    CALL GETARG(1,infile)
     
    OPEN(11,FILE=infile,form='formatted',status='old')
  
    READ (11,'(A)') line
    REWIND(11)

    date_string(1:16)=line(1:16)

    datein=init_date(date_string)
    datew=datein
    datew%minute=0

    IF(get_diff_date(datein,datew)>=45.*one_minute) THEN
       datew=get_new_date(datew,one_hour)
    ENDIF

    datechar=date_to_char(datew)
    WRITE(*,*) 'proc metar for time ',datechar
   
    outfile=datechar(3:10)//'Z.TXT'
    open(12,FILE=outfile,form='formatted',status='unknown',iostat=ierr)
    IF(ierr/=0)THEN
       WRITE(*,*) 'Error opening file: ',outfile
       STOP
    ENDIF
    

    n=1
10  READ(11,'(A)',err=999,end=999) line

    IF(n == 1) THEN
       WRITE(12,'(A)') 'ZCZC'
       WRITE(12,'(A)') 'SACI10 BCSY '//datechar(7:12)
    ELSE
       IF (LEN_TRIM(line) > 1 ) THEN
          WRITE(12,'(A)') TRIM(line)
       ELSE
          WRITE(12,'(A)') 'NNNN' 
          n=0
       ENDIF
    ENDIF

    n=n+1
    
    GOTO 10
999 CONTINUE
    WRITE(12,'(A)') 'NNNN' 
    END
