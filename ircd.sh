#~/bin/bash

cd ebin

rm -f ebin/*.beam
for file in `ls ../src/`; do
	/usr/bin/erlc ../src/$file
done;

erl eircd.beam -s eircd start_link
