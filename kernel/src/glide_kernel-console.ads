-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Interactive_Consoles; use Interactive_Consoles;
with Glide_Result_View;

package Glide_Kernel.Console is

   type Message_Type is (Info, Error, Verbose);
   --  We are dealing with 3 types of messages :
   --   - Info for general information
   --   - Error for signaling errors
   --   - Verbose for detailed information

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the console module into the list

   procedure Initialize_Console
     (Kernel         : access Kernel_Handle_Record'Class);
   --  Initializes the Kernel's console. Note that the main window must have
   --  been created first.

   procedure Insert
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info);
   --  Insert Text in the GPS's console.
   --  Highlight parts of Text that match a source location (the color is set
   --  using the preferences) if Highlight_Sloc is True.
   --  If Add_LF is True, automatically add a line separator.

   procedure Parse_File_Locations
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Category       : String);
   --  Perform a basic parsing on Text, and add any found file locations
   --  to the results view in Category.

   procedure Raise_Console (Kernel : access Kernel_Handle_Record'Class);
   --  If the message window is present in the MDI, raise it.

   procedure Insert_Result
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : String;
      Text     : String;
      Line     : Positive;
      Column   : Positive;
      Length   : Natural := 0);
   --  Insert a new location in the result view.

   procedure Remove_Result_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String);
   --  Remove Category from the results view, if it exists.

   procedure Clear (Kernel : access Kernel_Handle_Record'Class);
   --  Clear all the text in the Console.

   function Get_Interactive_Console
     (Kernel : access Kernel_Handle_Record'Class)
      return Interactive_Console;
   --  Return the interactive console associated with the kernel.

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class)
      return Interactive_Console;
   --  Return the interactive console associated with the kernel.

   function Get_Or_Create_Result_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Glide_Result_View.Result_View;
   --  Return the results view widget. Create it if it doesn't exist and
   --  Allow_Creation is true.

end Glide_Kernel.Console;
