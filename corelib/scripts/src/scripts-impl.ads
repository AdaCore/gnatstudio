-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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

--  This package provides subprograms that are used when adding support for
--  new scripting languages. Applications should not typically have a need for
--  these types or subprograms.

package Scripts.Impl is

   function From_Instance
     (Script : access Scripting_Language_Record'Class;
      Inst   : access Class_Instance_Record'Class) return Class_Instance;
   --  Return a class instance wrapping Inst.
   --  For internal use by scripting languages only.

   procedure Insert_Text
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String;
      Hide    : Boolean := False);
   procedure Insert_Log
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String);
   procedure Insert_Error
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String);
   procedure Insert_Prompt
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String);
   --  Display txt either on the specified console or on the scripts' default
   --  console if Console is set to null.
   --  If Hide is set to True, the text is not displayed on the console after
   --  all, although it will be displayed in the log instead.

   procedure Register_Console_Class
     (Repo  : Scripts_Repository;
      Class : Class_Type);
   --  Register the console class, which is used to redirect output of script
   --  languages to a specific Virtual_Console

end Scripts.Impl;
