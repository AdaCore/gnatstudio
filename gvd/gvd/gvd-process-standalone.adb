-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                       use Glib;

with Gdk.Input;
with Gdk.Types;                  use Gdk.Types;

with Gtk;                        use Gtk;
with Gtk.Notebook;               use Gtk.Notebook;

with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Process_Proxies;            use Process_Proxies;
with Pixmaps_IDE;                use Pixmaps_IDE;

with GVD.Main_Window;            use GVD.Main_Window;
with GVD.Types;                  use GVD.Types;
with GVD.Preferences;            use GVD.Preferences;
with GVD.Text_Box.Source_Editor; use GVD.Text_Box.Source_Editor;
with GVD.Text_Box.Source_Editor.Socket;
with GVD.Text_Box.Source_Editor.Builtin;

package body GVD.Process.Standalone is

   ---------------------
   -- Create_Debugger --
   ---------------------

   function Create_Debugger
     (Window          : access GVD_Main_Window_Record'Class;
      Kind            : Debugger_Type;
      Executable      : String;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") return Debugger_Process_Tab
   is
      Process         : Debugger_Process_Tab;
      Builtin_Source  : Builtin.Builtin;
      External_Source : Socket.Socket;
      Source          : Source_Editor;
      Success         : Boolean;

   begin
      Process := new Debugger_Process_Tab_Record;

      if Window.External_XID = 0 then
         Builtin.Gtk_New
           (Builtin_Source, Process, Window.TTY_Mode,
            Get_Pref (GVD_Prefs, Editor_Font),
            dot_xpm, arrow_xpm, stop_xpm,
            Comments_Color => Get_Pref (GVD_Prefs, Comments_Color),
            Strings_Color  => Get_Pref (GVD_Prefs, Strings_Color),
            Keywords_Color => Get_Pref (GVD_Prefs, Keywords_Color));
         Source := Source_Editor (Builtin_Source);

      else
         Socket.Gtk_New
           (External_Source, Window.External_XID, Window.TTY_Mode);
         Source := Source_Editor (External_Source);
      end if;

      Initialize (Process, Window, Source);
      Configure
        (Process, Kind, new Process_Proxy,
         Executable, Debugger_Args, Executable_Args,
         Remote_Host, Remote_Target, Remote_Protocol, Debugger_Name,
         Success => Success);

      if Success then
         return Process;
      else
         Destroy (Process);
         return null;
      end if;
   end Create_Debugger;

   ---------------------
   -- Input_Available --
   ---------------------

   procedure Input_Available
     (Debugger  : Standard_Input_Package.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      pragma Unreferenced (Source, Condition);

      Tab       : Debugger_Process_Tab;
      Buffer    : String (1 .. 8192);
      Len       : Natural;

      use String_History;

   begin
      Tab := Process_User_Data.Get
        (Get_Nth_Page
          (Debugger.Process_Notebook,
           Get_Current_Page (Debugger.Process_Notebook)));

      --  If we are already processing a command, just wait until
      --  the debugger is available

      if not Command_In_Process (Get_Process (Tab.Debugger)) then
         Get_Line (Buffer, Len);

         if Len = 0 then
            Find_Match
              (Tab.Window.Command_History,
               Natural (Get_Num (Tab)),
               Backward);
            Process_User_Command
              (Tab, Get_Current (Tab.Window.Command_History).Command.all,
               Output_Command => True,
               Mode => User);

         else
            Process_User_Command (Tab, Buffer (1 .. Len));
         end if;
      end if;

   exception
      --  The history was empty, so there is no command to execute...
      when No_Such_Item =>
         null;
   end Input_Available;

end GVD.Process.Standalone;
