#!/bin/bash
#
# Script to decode a gts data files.
#
# Strip the control characters from the wmo files treating
# the surface and upa files separately, then cat them together.
#

CWD=`pwd`
cd `dirname $0`
PWD=`pwd`

if [ $# -gt 0 ]
then
# $1 : YYYY-MM-dd_hh
  time=$1
  yyyymmddhh=`echo $1|cut -c1-4,6-7,9-10,12-13`
# yymmddhh=$1
else
  echo Usage: $0 Time\(FMT:YYYY-MM-dd_hh\) ObsFileName
  exit 1
fi

if [ $# -gt 1 ]
then
  gts_data_file=$2
  if [ `echo $gts_data_file|cut -c1` != "/" ]
  then
     gts_data_file=$CWD/$gts_data_file
  fi
  ln -sf $gts_data_file gts_data
fi

# Now the data can be decoded, standard input is the time of
# the last input file plus about 55 minutes.
# Assume the gts_sttnid_final and gts_sttnid_final.icao files are in this
# directory (or linked to it).

# Clean the directory
#rm -f gts_obs gts_out.* std_in std_out
rm -f gts_out.*

echo ${yyyymmddhh}55 > std_in
echo 7200           >> std_in
./gts_decoder < std_in > std_out 2> /dev/null
status=$? 

if tail std_out| grep "SUCCESSFUL COMPLETION OF DECODER FOR"
then
  echo "..  GTS_DECODER SUCCESS FOR " $yyyymmddhh
else
  echo "Status Returned:" $status
  echo "............   GTS_DECODER failed ......."
fi

# Cat the decoded files into one obs file in the order we want (upper
# air first).

touch  gts_out.73{5,6,7,8} gts_out.73{2,3,4} gts_out.713 gts_out.712 gts_out.71{5,6} \
       gts_out.79{0,1,2,3,4,5,6,7,8,9} gts_out.78{0,1,2,3,4,5,6,7,8,9}  \
       gts_out.74{0,1,2,3,4,5,6,7,8,9}

cat gts_out.73[5-8] gts_out.73[2-4] gts_out.713 gts_out.712 gts_out.71[5-6] \
    gts_out.79[0-9] gts_out.78[0-9] gts_out.74[0-9] > obs-$1
#cat gts_out.73[5-8] gts_out.73[2-4] gts_out.713 gts_out.71[5-6] \
#    gts_out.79[0-9] gts_out.78[0-9] gts_out.74[0-9] > obs-$1
cat gts_out.712 gts_out.713 gts_out.71[5-6]> sfc_obs-$1

# Clean the directory
#rm -f gts_out.* gts_data
#rm -f std_in 
#rm -f std_out

if [ $CWD != $PWD ] 
then
   mv obs-$time $CWD
   mv sfc_obs-$time $CWD
fi
