-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Glib; use Glib;
with Gtk.Main;
with Gtk;           use Gtk;
with Gtk.GEntry;    use Gtk.GEntry;
with Gtk.Box;       use Gtk.Box;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.List;      use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Debugger; use Debugger;
with GVD.Process; use GVD.Process;
with String_Utils; use String_Utils;
with GVD.Types; use GVD.Types;
with GVD.Main_Window; use GVD.Main_Window;

with Ada.Unchecked_Deallocation;

package body GVD.Session_Dialog is

   Buffer_Length : constant := 8192;
   --  Maximum length of a string buffer

   ----------------------
   -- Local procedures --
   ----------------------

   procedure Append_Button
     (Open  : access GVD_Session_Dialog_Record'Class;
      Label : String);
   --  Add a check_button to the layout.

   ------------------
   -- Save_Session --
   ------------------

   procedure Save_Session
     (Window : access Gtk_Widget_Record'Class;
      Open   : in out GVD_Session_Dialog;
      Dir    : in String)
   is
      File          : File_Type;
      Top           : constant GVD_Main_Window := GVD_Main_Window (Window);
      Tab           : Debugger_Process_Tab;
      Directory     : Dir_Type;
      Buffer        : String (1 .. Buffer_Length);
      Last          : Natural;
      Item          : Gtk_List_Item;
      Program       : Basic_Types.String_Access;
      Debugger_List : Debugger_List_Link := Top.First_Debugger;
      Num_Debuggers : Natural := 0;
      use String_History;

   begin
      if Open = null then
         Open := new GVD_Session_Dialog_Record;
         Open_Session_Pkg.Initialize (Open);
         Open.Sessions_Dir := new String' (Dir);
      end if;

      Set_Title (Open, "Save Session");
      Grab_Focus (Open.Entry1);
      Open.Lock_Buttons := True;
      Show_All (Open);
      Remove_Items (Open.List, Get_Children (Open.List));
      GNAT.Directory_Operations.Open (Directory, Dir);

      loop
         Read (Directory, Buffer, Last);

         exit when Last = 0;

         if Buffer (1) /= '.' then
            Gtk_New (Item, Label => Buffer (1 .. Last));
            Show (Item);
            Add (Open.List, Item);
         end if;
      end loop;

      GNAT.Directory_Operations.Close (Directory);
      Remove_All_Buttons (Open);

      while Debugger_List /= null loop
         Program :=
           Debugger_Process_Tab (Debugger_List.Debugger).Descriptor.Program;

         if Program.all = "" then
            Append_Button (Open, "<no executable>");
         else
            Append_Button (Open, Program.all);
         end if;

         Debugger_List := Debugger_List.Next;
         Num_Debuggers := Num_Debuggers + 1;
      end loop;

      Gtk.Main.Main;

      declare
         Conversion_Table : array (1 .. Num_Debuggers) of Integer;
         Active_Debuggers : Natural := 0;
         Current_Button   : Button_Link := Open.First_Button;

      begin
         if Get_Text (Open.Entry1) = "" then
            return;
         end if;

         Create
           (File, Out_File,
            Top.Sessions_Dir.all &
           Directory_Separator & Get_Text (Open.Entry1));

         for J in 1 .. Num_Debuggers loop
            if Get_Active (Current_Button.Button) then
               Active_Debuggers := Active_Debuggers + 1;
            end if;

            Current_Button := Current_Button.Next;
         end loop;

         Put_Line (File, "[Session_File Header]");
         Put_Line (File, Natural'Image (Active_Debuggers));
         Put_Line (File, "---------------------");

         Debugger_List := Top.First_Debugger;
         Current_Button := Open.First_Button;
         Active_Debuggers := 0;

         for J in 1 .. Num_Debuggers loop
            if Get_Active (Current_Button.Button) then
               Active_Debuggers :=
                 Active_Debuggers + 1;
               Conversion_Table (J) := Active_Debuggers;
               Tab := Debugger_Process_Tab (Debugger_List.Debugger);

               Put_Line (File, Tab.Descriptor.Program.all);
               Put_Line (File,
                         Debugger_Type'Image (Tab.Descriptor.Debugger));
               Put_Line (File, Tab.Descriptor.Remote_Host.all);
               Put_Line (File, Tab.Descriptor.Remote_Target.all);
               Put_Line (File, Tab.Descriptor.Protocol.all);
                  Put_Line (File, Tab.Descriptor.Debugger_Name.all);
                  Put_Line (File, "---------------------");

            else
               Conversion_Table (J) := -1;
            end if;

            Current_Button := Current_Button.Next;
            Debugger_List := Debugger_List.Next;
         end loop;

         Put_Line (File, "[History]");
         Wind (Top.Command_History, Backward);

         for J in reverse 1 .. Length (Top.Command_History) loop
            for Count in 1 ..
              Get_Current_Repeat_Num (Top.Command_History)
            loop
               declare Data : History_Data :=
                 Get_Current (Top.Command_History);
               begin
                  if Conversion_Table (Data.Debugger_Num) /= -1 then
                     Put_Line
                       (File,
                        Natural'Image
                          (Conversion_Table (Data.Debugger_Num)) &
                        " " & Command_Type'Image (Data.Mode) (1) &
                        " " & Data.Command.all);
                  end if;
               end;
            end loop;

            if J /= 1 then
               Move_To_Next (Top.Command_History);
            end if;
         end loop;

         Put_Line (File, "---------------------");
         Close (File);
      end;

   exception
      when No_Such_Item =>
         Put_Line (File, "---------------------");
         Close (File);
      when Use_Error =>
         null;
   end Save_Session;

   ------------------
   -- Open_Session --
   ------------------

   procedure Open_Session
     (Window : access Gtk_Widget_Record'Class;
      Open   : in out GVD_Session_Dialog;
      Dir    : String)
   is
      Directory    : Dir_Type;
      Buffer       : String (1 .. Buffer_Length);
      Last         : Natural;
      Item         : Gtk_List_Item;
      File         : File_Type;
      Num_Pages    : Gint;

      use String_History;

   begin
      if Open = null then
         Open := new GVD_Session_Dialog_Record;
         Open_Session_Pkg.Initialize (Open);
         Open.Sessions_Dir := new String' (Dir);
      end if;

      Set_Title (Open, "Open Session");
      Grab_Focus (Open.Entry1);
      Open.Lock_Buttons := False;
      Show_All (Open);

      Remove_Items (Open.List, Get_Children (Open.List));
      GNAT.Directory_Operations.Open (Directory, Dir);

      loop
         Read (Directory, Buffer, Last);
         exit when Last = 0;

         if Buffer (1) /= '.' then
            Gtk_New (Item, Label => Buffer (1 .. Last));
            Show (Item);
            Add (Open.List, Item);
         end if;
      end loop;

      GNAT.Directory_Operations.Close (Directory);
      Gtk.Main.Main;

      if Get_Text (Open.Entry1) /= "" then
         Ada.Text_IO.Open
           (File, In_File, Open.Sessions_Dir.all &
              Directory_Separator & Get_Text (Open.Entry1));
         Get_Line (File, Buffer, Last);

         if Buffer (1 .. Last) /= "[Session_File Header]" then
            Close (File);
            return;
         end if;

         --  Get the number of processes.

         Get_Line (File, Buffer, Last);
         Num_Pages := Gint'Value (Buffer (1 ..  Last));
         Get_Line (File, Buffer, Last);

         declare
            Processes      : array (1 .. Num_Pages) of Debugger_Process_Tab;
            Current_Button : Button_Link := Open.First_Button;
            List           : Argument_List (1 .. 0);
            Tab            : Debugger_Process_Tab;
            Program        : Program_Descriptor;
            Index          : Natural;

         begin
            --  Read the descriptors and create the debuggers.

            for J in 1 .. Num_Pages loop
               Get_Line (File, Buffer, Last);
               --  Ignore program name

               Get_Line (File, Buffer, Last);
               Program.Debugger := Debugger_Type'Value (Buffer (1 .. Last));
               Get_Line (File, Buffer, Last);
               Program.Remote_Host := new String' (Buffer (1 .. Last));
               Get_Line (File, Buffer, Last);
               Program.Remote_Target := new String' (Buffer (1 .. Last));
               Get_Line (File, Buffer, Last);
               Program.Protocol := new String' (Buffer (1 .. Last));
               Get_Line (File, Buffer, Last);
               Program.Debugger_Name := new String' (Buffer (1 .. Last));
               Get_Line (File, Buffer, Last);

               if Get_Active (Current_Button.Button) then
                  Processes (J) :=
                    Create_Debugger
                    (GVD_Main_Window (Window),
                     Program.Debugger, "",
                     List, "",
                     Program.Remote_Host.all,
                     Program.Remote_Target.all,
                     Program.Protocol.all);
               else
                  Processes (J) := null;
               end if;

               Current_Button := Current_Button.Next;
            end loop;

            --  Read and compute the commands history.

            Get_Line (File, Buffer, Last);

            loop
               Get_Line (File, Buffer, Last);

               exit when Last > 4 and then Buffer (1 .. 4) = "----";

               Index := 1;
               Skip_Blanks (Buffer (1 .. Last), Index);
               Skip_To_Blank (Buffer (1 .. Last), Index);

               if Processes (Gint'Value (Buffer (1 .. Index))) /= null then
                  Tab := Processes (Gint'Value (Buffer (1 .. Index)));
                  Skip_To_Blank (Buffer (Index .. Last), Index);

                  if Buffer (Index + 1) = 'H' then
                     Send
                       (Tab.Debugger,
                        Buffer (Index + 3 .. Last),
                        Mode => Hidden);

                  else
                     Process_User_Command
                       (Tab, Buffer (Index + 3 .. Last),
                        Mode => GVD.Types.Visible);
                     Wait_User_Command (Tab.Debugger);
                  end if;
               end if;
            end loop;
         end;

         Close (File);
      end if;

   exception
      when Name_Error =>
         null;
      when Device_Error =>
         null;
   end Open_Session;

   -------------------
   -- Append_Button --
   -------------------

   procedure Append_Button
     (Open  : access GVD_Session_Dialog_Record'Class;
      Label : String)
   is
      Button : Gtk_Check_Button;
      Buffer : Button_Link;

   begin
      Gtk_New (Button, Label);
      Pack_Start (Open.File_Buttons, Button, False, False, 0);
      Set_Active (Button, True);
      Show_All (Button);

      if Open.First_Button = null then
         Open.First_Button :=
           new Button_Node'
             (Next   => null,
              Button => Button,
              Label  => new String' (Label));

      else
         Buffer := Open.First_Button;

         while Buffer.Next /= null loop
            Buffer := Buffer.Next;
         end loop;

         Buffer.Next :=
           new Button_Node'
             (Next   => null,
              Button => Button,
              Label  => new String' (Label));
      end if;
   end Append_Button;

   --------------------
   -- Create_Buttons --
   --------------------

   procedure Create_Buttons
     (Open      : access GVD_Session_Dialog_Record'Class;
      File_Name : String)
   is
      File      : File_Type;
      Num_Pages : Gint;
      Buffer    : String (1 .. Buffer_Length);
      Last      : Natural;

   begin
      Ada.Text_IO.Open
        (File, In_File,
         Open.Sessions_Dir.all & Directory_Separator & File_Name);
      Get_Line (File, Buffer, Last);

      if Buffer (1 .. Last) /= "[Session_File Header]" then
         Close (File);
         return;
      end if;

      Get_Line (File, Buffer, Last);
      Num_Pages := Gint'Value (Buffer (1 ..  Last));
      Get_Line (File, Buffer, Last);

      for J in 1 .. Num_Pages loop
         Get_Line (File, Buffer, Last);
         Append_Button (Open, Buffer (1 .. Last));

         for K in 1 .. 6 loop
            Get_Line (File, Buffer, Last);
         end loop;
      end loop;

      Close (File);
   end Create_Buttons;

   ------------------------
   -- Remove_All_Buttons --
   ------------------------

   procedure Remove_All_Buttons
     (Open : access GVD_Session_Dialog_Record'Class)
   is
      procedure Free is new
        Ada.Unchecked_Deallocation (Button_Node, Button_Link);
      Buffer   : Button_Link := Open.First_Button;
      Previous : Button_Link;

   begin
      while Buffer /= null loop
         Previous := Buffer;
         Buffer   := Buffer.Next;
         Destroy (Previous.Button);
         Free (Previous.Label);
         Free (Previous);
      end loop;

      Open.First_Button := null;
   end Remove_All_Buttons;

end GVD.Session_Dialog;
