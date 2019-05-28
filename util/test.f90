      CHARACTER(*) , PARAMETER :: date_char_fmt = 'I4.4,2I2.2' , time_char_fmt = '3I2.2,".",I3.3' , &
                                  date_time_char_fmt = date_char_fmt // time_char_fmt

      CHARACTER(*) , PARAMETER :: date_str_fmt = 'I4.4, 2("-",I2.2,)' , &
                                  time_str_fmt = '2(I2.2,":"),I2.2,".",I3.3,"0"' , &
                                  date_time_str_fmt = date_str_fmt // "_" // time_str_fmt

      INTEGER      , PARAMETER :: date_char_len = LEN(date_char_fmt) , time_char_len = LEN(time_char_fmt) , &
                                  date_time_char_len = LEN(date_time_char_fmt)
                                  
      INTEGER      , PARAMETER :: date_str_len = LEN(date_str_fmt) , time_str_len = LEN(time_str_fmt) , &
                                  date_time_str_len = LEN(date_time_str_fmt)

      END
