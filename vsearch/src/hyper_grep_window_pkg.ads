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

--  The hypergrep searches for a pattern in files, handling source files to be
--  able to limit the scope to comments, strings, statements.

with Hyper_Grep_Base_Pkg; use Hyper_Grep_Base_Pkg;

package Hyper_Grep_Window_Pkg is

   type Hyper_Grep_Window_Record is new Hyper_Grep_Base_Record with private;
   type Hyper_Grep_Window_Access is access all Hyper_Grep_Window_Record'Class;

   procedure Gtk_New (Hyper_Grep_Window : out Hyper_Grep_Window_Access);
   --  Create a new window handling the hyper grep.

   procedure Initialize
     (Hyper_Grep_Window : access Hyper_Grep_Window_Record'Class);

private

   type Hyper_Grep_Window_Record is new Hyper_Grep_Base_Record
     with null record;

end Hyper_Grep_Window_Pkg;
