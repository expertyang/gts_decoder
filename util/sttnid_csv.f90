      program append_sttnid
      !integer :: id(99999), lon(99999), lat(99999), elv(99999)
      !
      integer :: nid, nlon, nlat, nelvi, nlevl
      integer :: gid, region
      real :: relv

      character(len=8) :: clon, clat
      character(len=100) :: field(29), country, sname
      character(len=1024) :: pubstninfo
      character(len=1024) :: pubstnname
      character(len=100) :: stninfo, stninfo_read

      character(len=8000) :: input_file

      character(len=1), parameter :: tab=","  !ACHAR(009)

      integer :: index_from, index_to, index_tab

      if(iargc()<1)then
         write(*,*) "usage: program_name china_file"
         stop 1
      endif
      call getarg(1, input_file)

!     open (1, file='Pub9volA011029.flatfile', status='old')
!     open (1, file='Pub9volA.flatfile', status='old')
      open (1, file=input_file, status='old')
!     open (2, file='gts_sttnid_final.wmo',access='direct',recl=100)
      open (2, file='gts_sttnid_final.wmo',access='direct',recl=100, status="old")
      open (21,file="china_site.txt",status="unknown")

      read(1, *)
!     read(1, *)

100   read(1, '(a)', end=9999, err=9999) pubstninfo

      index_from=0
      index_to=len(trim(pubstninfo))

      ! read into sttn variable 
      do i=1, 9 
      !write(*, *) (ICHAR(pubstninfo(k:k)),k=1,20) 
      !stop

         index_tab=INDEX(pubstninfo(index_from:index_to),tab)
         index_tab=index_tab+index_from
         if(index_from == 0)then
            field(i)=pubstninfo((index_from+1):(index_tab-1))
         else
            field(i)=pubstninfo((index_from):(index_tab-1))
         endif
         index_from=index_tab
!        write(*, *) i, index_from, index_tab, index_to, field(i)
      enddo 

!     write(*, *) (trim(field(i)), i=1, 11)

      country="China"

! station id IIiii
      read(field(3), *,iostat=ierr) nid
      if(ierr/=0.or.nid>60000.or.nid<50000) goto 100

! longitude, latitude   
         clon=field(7)
         clat=field(6)

         read(clat, *) nlat
         read(clon, *) nlon

         nlat= INT(nlat/100)*100 + (MOD(nlat,100) /60.)*100
         nlon= INT(nlon/100)*100 + (MOD(nlon,100) /60.)*100
! station name   
         sname=field(4)
         !write(*, *) field(11)

! elevation
         write(*,*) '"',trim(field(8)),'",', '"',trim(field(9)),'",'
         if(len(trim(field(8))) >= 1 .and. index(field(8), "NA")==0) then
            read(field(8), *) relv
            nelv=relv
         else
            if(len(trim(field(9))) >= 1 .and. index(field(9) ,"NA")==0) then 
               read(field(9), *) relv
               nelv=relv
            else
         !     write(*, *) 'nelv= :',field(11), len(trim(field(11)(1:(len(trim(field(11)))-1))))
         !     write(*, *) 'nelv= :',field(13), len(trim(field(13)(1:(len(trim(field(13)))-1))))
               nelv=9999
            endif
         endif

         WRITE(*,*) nid, nlat, nlon, nelv, TRIM(sname), TRIM(country)
!        STOP
         
! from gts_sttnid_final 
         read(2, rec=nid) stninfo
         write(*, '(a)') stninfo
         stninfo_read=stninfo
         read(stninfo(1:5), *) gid
!        if(nid /= gid ) then
            if(nid >=60000 .and. nid <=69999)then
               region=1
            endif
            if((nid >=20000 .and. nid <=20099 ).or. &
               (nid >=20200 .and. nid <=21999 ).or. &
               (nid >=23000 .and. nid <=25999 ).or. &
               (nid >=28000 .and. nid <=32999 ).or. &
               (nid >=35000 .and. nid <=36999 ).or. &
               (nid >=38000 .and. nid <=39999 ).or. &
               (nid >=40350 .and. nid <=48599 ).or. &
               (nid >=48800 .and. nid <=49999 ).or. &
               (nid >=50000 .and. nid <=59999 )     &
               )then
               region=2
            endif
            if(nid >=80000 .and. nid <=88999)then
               region=3
            endif
            if(nid >=70000 .and. nid <=79999)then
               region=4
            endif
            if((nid >=48600 .and. nid <=48799) .or. &
               (nid >=90000 .and. nid <=98999) )then
               region=5
            endif
            if((nid >=    0 .and. nid <=19999 ).or. &
               (nid >=20100 .and. nid <=20199 ).or. &
               (nid >=22000 .and. nid <=22999 ).or. &
               (nid >=26000 .and. nid <=27999 ).or. &
               (nid >=33000 .and. nid <=34999 ).or. &
               (nid >=37000 .and. nid <=37999 ).or. &
               (nid >=40000 .and. nid <=40349 ) &
               )then
               region=6
            endif
            if(nid >=89000 .and. nid <=89999)then
               region=7
            endif
            write(stninfo(1:30), '(i5.5,i6.4,"N",i7.5,"E",i6.4,"m",i2)') nid , nlat, nlon, nelv, region
!        endif

         if ( nid /= gid )then
         write(pubstnname,'("_WMO_  ",a," / ",a)')trim(sname), trim(country)
         stninfo(31:100)=pubstnname(1:70)
         endif
         if (stninfo(1:30)/=stninfo_read(1:30)) then
            write(2, rec=nid) stninfo
            write(*, '(a)') stninfo
         else
            write(*,*) "stninfos are same!!!", stninfo(1:30)//";"//stninfo_read(1:30)
         endif
       write(21,"(I5,2F12.2,F10.1)") nid , nlat/100., nlon/100., float(nelv)
!stop
!      endif

      goto 100
9999  continue
      end program
