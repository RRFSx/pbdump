program prepbufr_decode_all
!
! read all observations out from prepbufr.
! read bufr table from prepbufr file
!
 USE datetime_module !,ONLY:datetime
 implicit none

 integer, parameter :: mxmn=35, mxlv=250
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ'
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE'
 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb, iMinute, IUPVS01
 CHARACTER(len=256) :: arg, bufrfile,sDate,sDate2,stmp1,stmp2,stmp0, sTime1,sTime2

 character(8)   :: subset, sSubset
 integer        :: unit_in=10,idate,nmsg,ntb
 logical        :: ldump, lallset, lonlymsgtype, lsatellite,ldate
 logical        :: ldumpobs, ldumperr, ldumpqcf, ldumpall

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: i,k,iret,dMinutes
 type(datetime) date0,date2

 lsatellite=.false.
 ldumpobs=.false.
 ldumperr=.false.
 ldumpqcf=.false.
 ldumpall=.false.
 ldate=.false.
 k=iargc()
 if(k<1) then
   print *, "Usage: pbdump filename msgtype dump[obs/err/qcf/all] YYYYMMDDHHmm-YYYYMMDDHHmm"
   print *, "                where dumpopt takes one of dumpobs, dumperr, dumpqcf, dumpall"
   stop
 endif
 CALL getarg(1,arg)
 bufrfile=arg
 ldump=.false.
 lonlymsgtype=.true.
 if (k>1) then
   lonlymsgtype=.false.
   call getarg(2,arg)
   if (trim(arg)=='all') then
     lallset=.true.
     lsatellite=.true.
   else
     lallset=.false.
     sSubset=trim(arg)
     !print *, 'Decode message type: ', trim(sSubset)
   endif
   if(trim(arg)=='SATWND' .or. &
      trim(arg)=='SATEMP' .or. &
      trim(arg)=='SPSSMI' .or. &
      trim(arg)=='ERS1DA' .or. &
      trim(arg)=='GOESND' .or. &
      trim(arg)=='QKSWND' .or. &
      trim(arg)=='QKSWND' .or. &
      trim(arg)=='WDSATR' .or. &
      trim(arg)=='ASCATW' )    &
      lsatellite=.true.
 endif
 if(k>2) then
   call getarg(3,arg)
   stmp0=trim(arg)
   if (stmp0(1:4)=='dump') then
     ldump=.true.
     if (stmp0(5:7)=='obs') then
       ldumpobs=.true.
     else if (stmp0(5:7)=='err') then
       ldumperr=.true.
     else if (stmp0(5:7)=='qcf') then
       ldumpqcf=.true.
     else if (stmp0(5:7)=='all') then
       ldumpall=.true.
     else
       print *, 'only support dumpobs,dumperr,dumpqcf,dumpall'
       stop
     endif
   else if (stmp0(1:1)=='2'.or. stmp0(1:1)=='1') then  !time peroid limit
     !yyyymmddhhmm
     sTime1=stmp0(1:12)
     sTime2=stmp0(14:25)
     print*,'time peroid: ',trim(sTime1), ' to ', trim(sTime2)
     ldate=.true.
   else
     print *, 'only support dumpobs,dumperr,dumpqcf,dumpall,201711011656-201711011700'
     print *, 'ignore the parameter -',trim(arg)
   endif
 endif
 if (k>3) then
      call getarg(3,arg)
   stmp0=trim(arg)
   if (stmp0(1:4)=='dump') then
     ldump=.true.
     if (stmp0(5:7)=='obs') then
       ldumpobs=.true.
     else if (stmp0(5:7)=='err') then
       ldumperr=.true.
     else if (stmp0(5:7)=='qcf') then
       ldumpqcf=.true.
     else if (stmp0(5:7)=='all') then
       ldumpall=.true.
     else
       print *, 'only support dumpobs,dumperr,dumpqcf,dumpall'
       stop
     endif
   else
     print *, 'only support dumpobs,dumperr,dumpqcf,dumpall,201711011656-201711011700'
     print *, 'ignore the parameter -',trim(arg)
   endif
 endif
!
!
 open(24,file='tmp.table')
 open(unit_in,file=trim(bufrfile),form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   if(.not. lonlymsgtype) then
     if (lsatellite) then
       write(*,*) 'Header: #,levels,ID,time,x,y,elv,DHR,type,SAID'
     else
       write(*,*) 'Header: #,levels,ID,time,x,y,elv,DHR,type'
     endif
   endif
   if (ldump) then
     if(ldumpobs .or. ldumpall) then
       write(*,*) 'obs:#,P Q T Z U V PW CAT PRSS' 
     endif
     if(ldumperr .or. ldumpall) then
       write(*,*) 'err:#,P Q T NUL W NUL PW'
     endif
     if(ldumpqcf .or. ldumpall) then
       write(*,*) 'qcf:#,P Q T Z W NUL PW'
     endif
   endif
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     ntb = 0
     write(stmp0,'(i12)') idate
     date0 = strptime(stmp0,"%Y%m%d%H%M")
     iMinute=IUPVS01(unit_in,'MINU')
     call date0%addMinutes(iMinute)

     if(lonlymsgtype &
       .or. lallset  .or. trim(subset)==trim(sSubset) ) then
       sDate=date0%strftime("%Y%m%d_%H%M")
       write(stmp2,'(3a,a15)') 'subset=',subset,' cycle time =',sDate
       if (stmp2 /= stmp1) then
         write(*,'(a)') trim(stmp2)
         stmp1=stmp2
       endif
     endif
     if (.not. lonlymsgtype &
       .and. ( lallset .or. trim(subset)==trim(sSubset)) ) then 
       sb_report: do while (ireadsb(unit_in) == 0)
         ntb = ntb+1
         call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
         rstation_id=hdr(1)
         date2=date0
         dMinutes=NINT(hdr(4)*60.0)
         call date2%addMinutes(dMinutes)
         sDate=date2%strftime("%Y%m%d_%H%M")
         sDate2=date2%strftime("%Y%m%d%H%M")
         if ( (trim(sDate2)>=trim(sTime1) .and. trim(sDate2)<=trim(sTime2)) &
           .or. (.not. ldate) ) then

           call ufbint(unit_in,obs,mxmn,mxlv,iret,obstr)
           call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
           call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)

           do i=2,8
             if(hdr(i)>10e10) hdr(i)=-99999
           enddo
           if (lsatellite) then
            !write(*,'(I5,I4,a10,a15,2f12.6,f8.1,f8.4,I4,I9)') ntb,iret,c_sid,trim(sDate),hdr(2),hdr(3),hdr(6),hdr(4),nint(hdr(5)),nint(hdr(7))
             write(*,'(I5,I4,a10,a15,2f9.3,f8.1,f8.4,I4,I9)') ntb,iret,c_sid,trim(sDate),hdr(2),hdr(3),hdr(6),hdr(4),nint(hdr(5)),nint(hdr(7))
           else
             !write(*,'(I8,I4,a10,a15,2f12.6,f8.1,f8.4,2I4)') ntb,iret,c_sid,trim(sDate),hdr(2),hdr(3),hdr(6),hdr(4),nint(hdr(5)),nint(hdr(8))
            !write(*,'(I5,I4,a10,a15,2f12.6,f8.1,f8.4,I4)') ntb,iret,c_sid,trim(sDate),hdr(2),hdr(3),hdr(6),hdr(4),nint(hdr(5))
             write(*,'(I5,I4,a10,a15,2f9.3,f8.1,f8.4,I4)') ntb,iret,c_sid,trim(sDate),hdr(2),hdr(3),hdr(6),hdr(4),nint(hdr(5))
           endif
           if (ldump) then
             DO k=1,iret
               if (ldumpall .or. ldumpobs) then
                 do i=1,9
                   if (obs(i,k)>10e10) obs(i,k)=-99999.0
                 enddo
                 write(*,'(6x,a4,i3,7f10.2,f4.1,f10.2)') 'obs=',k,(obs(i,k),i=1,9)
               endif
               if (ldumpall .or. ldumperr) then
                 do i=1,9
                   if (oer(i,k)>10e10) oer(i,k)=-99999.0
                 enddo
                 write(*,'(6x,a4,i3,7f10.2,f4.1,f10.2)') 'oer=',k,(oer(i,k),i=1,7)
               endif
               if (ldumpall .or. ldumpqcf) then
                 do i=1,9
                   if (qcf(i,k)>10e10) qcf(i,k)=-99999.0
                 enddo
                 write(*,'(6x,a4,i3,7f10.2,f4.1,f10.2)') 'qcf=',k,(qcf(i,k),i=1,7)
               endif
             ENDDO
           endif
         endif !date peroid check
       enddo sb_report
     endif

   enddo msg_report
 call closbf(unit_in)

end program
