-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with GNAT.Regpat; use GNAT.Regpat;

package Search_Callback is

   --  NOTE: This callback is not task safe ! It can't be used to handle two
   --        concurrent searches !

   procedure Reset_Search;
   --  Call it before every search using this callback.

   procedure Abort_Search;
   --  Call it to abort the search. The next time the callback is called, it
   --  will abort the search.

   function Callback
     (Match_Found : Boolean;
      File        : String;
      Line_Nr     : Positive    := 1;
      Line_Text   : String      := "";
      Sub_Matches : Match_Array := (0 => No_Match))
      return Boolean;
   --  Print every match 'file:line:text'.
   --  Print sub-matches '      num> ## <', showing where the num-th regexp
   --  sub-expression matched.
   --  Ignore file calls.
   --  Handle Gtk pending events.

end Search_Callback;
