#cat ICAO_STATION_LOCATION|tr [:lower:] [:upper:] > ICAO_STATION_LOCATION_UPPER

file1=icao_new.txt
file2=ICAO_STATION_LOCATION_UPPER
# input stations.txt ICAO_STATION_LOCATION
./sttnidicao > $file1

while read line
do 
   icao_id=`echo $line|cut -c1-4`
#  echo $icao_id
   grep ^$icao_id $file1 >/dev/null
   if [ $? -ne 0 ] 
   then 
      echo $line >> $file1 
      echo Add $icao_id to $file1
   fi
done< $file2

mv ICAO_STATION_LOCATION ICAO_STATION_LOCATION.bak
mv $file1 ICAO_STATION_LOCATION

./sid_icao

./packicao
