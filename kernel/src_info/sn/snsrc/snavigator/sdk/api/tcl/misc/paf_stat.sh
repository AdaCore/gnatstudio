#/bin/sh
# $Id$
#######################################################################
#                                                                     #
#  (c) Copyright 1992-1996 multiX Software GmbH.                      #
#                  All rights reserved                                #
#                                                                     #
#  Tel: +49-711-7287426  Fax: +49-711-7287427                         #
#  E-mail: odd@multix.de  or  paf@multix.de                           #
#  WWW http://www.multix.de or http://www.multix.n-e-t.de             #
#                                                                     #
#######################################################################

PAF=$HOME/pathfinder/bin
LOG=`pwd`/FILES.LST

echo > $LOG

statistic () {
	db=$1
	defs=$2
	rm  -f $db.hs?

	cou=0
	rm -f $defs

	for i in `ls -1 $db.*`
	do
		nm=`basename $i`
		if [ -s $i ]
		then	
			c=`dbdump $i | wc -l`
			echo $c definitions in $i >> $defs
			cou=`expr $cou + $c`
		fi
	done
	echo '------' >> $defs
	echo 'Total: ' $cou >> $defs
}

make_project () {
	save_dir=`pwd`
	out_list=$save_dir/$1.STAT
	cd $1

	find * -name '*.[hc]' -type f -print > src.lst
	echo
	echo
	echo Parsing $i project ...
	echo Parsing $i project ... >> $LOG
	rm -rf .paf
	time $PAF/cbrowser -n .paf/$i -y src.lst -p $PAF/dbimp >> $LOG
	rm src.lst

	statistic .paf/$i $out_list

	cd $save_dir
}
if [ $# -eq 0 ]
then	files=`ls`
else	files=$1
fi

for i in $files
do
	if [ -d $i ]
	then make_project $i
	fi
done
