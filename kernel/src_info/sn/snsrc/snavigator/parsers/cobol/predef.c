/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

   Predef ("ALPHABETIC-LOWER"	, ans85 | vs3 | vs4 | sa | x | mf7);
   Predef ("ALPHABETIC-UPPER"	, ans85 | vs3 | vs4 | sa | x | mf7);
   Predef ("C01"	, os | dvs);
   Predef ("C02"	, os | dvs);
   Predef ("C03"	, os | dvs);
   Predef ("C04"	, os | dvs);
   Predef ("C05"	, os | dvs);
   Predef ("C06"	, os | dvs);
   Predef ("C07"	, os | dvs);
   Predef ("C08"	, os | dvs);
   Predef ("C09"	, os | dvs);
   Predef ("C10"	, os | dvs);
   Predef ("C11"	, os | dvs);
   Predef ("C12"	, os | dvs);
   Predef ("COBOL"	, all);
   Predef ("COM-REG"	, vs | dvs);
   Predef ("CURRENT-DATE"	, os | vs2 | dvs | mf);
   Predef ("CYL-INDEX"	, dvs);
   Predef ("CYL-OVERFLOW"	, dvs);
   Predef ("DEBUG"	, os | vs2 | dvs);
   Predef ("DEBUG-CONTENTS"	, all);
   Predef ("DEBUG-ITEM"	, all);
   Predef ("DEBUG-LINE"	, all);
   Predef ("DEBUG-NAME"	, all);
   Predef ("DEBUG-SUB-1"	, all);
   Predef ("DEBUG-SUB-2"	, all);
   Predef ("DEBUG-SUB-3"	, all);
   Predef ("DISPLAY-ST"	, os | vs2 | dvs);
   Predef ("EGI"	, all - dvs);
   Predef ("EMI"	, all - dvs);
   Predef ("ESI"	, all - dvs);
   Predef ("FILE-LIMIT"	, os | vs2 | dvs);
   Predef ("FILE-LIMITS"	, os | vs2 | dvs);
   Predef ("FORMFEED"	, rm);
   Predef ("KANJI"	, vs | mf8);
   Predef ("LINAGE-COUNTER"	, all - dvs);
   Predef ("LINE-COUNTER"	, all);
   Predef ("MASTER-INDEX"	, dvs);
   Predef ("MORE-LABELS"	, os | vs | dvs);
   Predef ("PAGE-COUNTER"	, all);
   Predef ("RETURN-CODE"	, os | vs | sa2 | x | mf5);
   Predef ("S01"	, os | dvs);
   Predef ("S02"	, os | dvs);
   Predef ("S03"	, dvs);
   Predef ("S04"	, dvs);
   Predef ("S05"	, dvs);
   Predef ("SHIFT-IN"	, vs);
   Predef ("SHIFT-OUT"	, vs);
   Predef ("SORT-CONTROL"	, os | vs);
   Predef ("SORT-CORE-SIZE"	, os | vs | dvs);
   Predef ("SORT-FILE-SIZE"	, os | vs | dvs);
   Predef ("SORT-MESSAGE"	, os | vs);
   Predef ("SORT-MODE-SIZE"	, os | vs | dvs);
   Predef ("SORT-OPTION"	, dvs);
   Predef ("SORT-RETURN"	, os | vs | sa2 | mf7 | dvs);
   Predef ("SYSIN"	, os);
   Predef ("SYSIPT"	, os | dvs);
   Predef ("SYSLST"	, os | dvs);
   Predef ("SYSOUT"	, os);
   Predef ("SYSPCH"	, dvs);
   Predef ("SYSPUNCH"	, os | dvs);
   Predef ("TAB"	, rm);
   Predef ("TALLY"	, os | vs | dvs);
   Predef ("TIME-OF-DAY"	, os | vs2 | dvs);
   Predef ("TRACK-AREA"	, os | vs2 | dvs);
   Predef ("TRACK-LIMIT"	, os | vs2);
   Predef ("TRACKS"	, os | vs2 | dvs);
   Predef ("UPSI-0"	, os | dvs);
   Predef ("UPSI-1"	, os | dvs);
   Predef ("UPSI-2"	, os | dvs);
   Predef ("UPSI-3"	, os | dvs);
   Predef ("UPSI-4"	, os | dvs);
   Predef ("UPSI-5"	, os | dvs);
   Predef ("UPSI-6"	, os | dvs);
   Predef ("UPSI-7"	, os | dvs);
   Predef ("WHEN-COMPILED"	, os | vs | sa | mf7 | dvs);

