------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;

package Templates_Parser.Input is

   type File_Type is limited private;

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;

   procedure Open
     (File : in out File_Type;
      Name : String;
      Form : String    := "");
   pragma Inline (Open);
   --  Like Text_IO.Open and Mode = In_File

   procedure Close (File : in out File_Type);
   pragma Inline (Close);
   --  Like Text_IO.Close. Raises text_IO.Status_Error is file is not open

   function End_Of_File (File : File_Type) return Boolean;
   pragma Inline (End_Of_File);
   --  Like Text_IO.End_Of_File. Raises Text_IO.Status_Error is file is not
   --  open.

   function LF_Terminated (File : File_Type) return Boolean;
   pragma Inline (LF_Terminated);
   --  Returns True if last line returned by Get_Line was terminated with a LF
   --  or CR+LF on DOS based systems.

   procedure Get_Line
     (File   : File_Type;
      Buffer :    out String;
      Last   :    out Natural);
   --  Like Text_IO.Get_Line. Raises Text_IO.Status_Error is file is not open

private

   type File_Record;
   type File_Type is access File_Record;

end Templates_Parser.Input;
