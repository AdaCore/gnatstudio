-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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
with Gtk; use Gtk;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Open_Session_Pkg.Callbacks; use Open_Session_Pkg.Callbacks;

with Gtk.Main;
with Gtk.List; use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Debugger; use Debugger;
with Odd.Process; use Odd.Process;
with Odd.Strings; use Odd.Strings;
with Open_Program_Pkg; use Open_Program_Pkg;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;

with Unchecked_Deallocation;

package body Open_Session_Pkg is

----------------------
-- Local procedures --
----------------------

procedure Append_Button
  (Open  : access Open_Session_Record'Class;
   Label : in String);
--  Add a check_button to the layout.

procedure Gtk_New (Open_Session : out Open_Session_Access) is
begin
   Open_Session := new Open_Session_Record;
   Open_Session_Pkg.Initialize (Open_Session);
end Gtk_New;

procedure Initialize (Open_Session : access Open_Session_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Open_Session, Window_Toplevel);
   Set_Title (Open_Session, -"Open Session");
   Set_Policy (Open_Session, False, True, False);
   Set_Position (Open_Session, Win_Pos_Center);
   Set_Modal (Open_Session, True);

   Gtk_New_Vbox (Open_Session.Vbox17, False, 0);
   Add (Open_Session, Open_Session.Vbox17);

   Gtk_New_Hbox (Open_Session.Hbox7, False, 0);
   Pack_Start (Open_Session.Vbox17, Open_Session.Hbox7, True, True, 0);

   Gtk_New_Vbox (Open_Session.Vbox18, False, 0);
   Pack_Start (Open_Session.Hbox7, Open_Session.Vbox18, True, True, 0);

   Gtk_New (Open_Session.Label94, -("Session List"));
   Pack_Start (Open_Session.Vbox18, Open_Session.Label94, False, False, 0);
   Set_Alignment (Open_Session.Label94, 0.5, 0.5);
   Set_Padding (Open_Session.Label94, 0, 0);
   Set_Justify (Open_Session.Label94, Justify_Center);
   Set_Line_Wrap (Open_Session.Label94, False);

   Gtk_New (Open_Session.Scrolledwindow10);
   Pack_Start (Open_Session.Vbox18, Open_Session.Scrolledwindow10, True, True, 0);
   Set_Policy (Open_Session.Scrolledwindow10, Policy_Automatic, Policy_Automatic);

   Gtk_New (Open_Session.Viewport1);
   Add (Open_Session.Scrolledwindow10, Open_Session.Viewport1);
   Set_Shadow_Type (Open_Session.Viewport1, Shadow_In);

   Gtk_New (Open_Session.List);
   List_Callback.Connect
     (Open_Session.List, "select_child", On_List_Select_Child'Access);
   Add (Open_Session.Viewport1, Open_Session.List);
   Set_Selection_Mode (Open_Session.List, Selection_Single);

   Gtk_New_Hbox (Open_Session.Hbox6, False, 0);
   Pack_Start (Open_Session.Vbox18, Open_Session.Hbox6, False, False, 7);

   Gtk_New (Open_Session.Label73, -("Session:"));
   Pack_Start (Open_Session.Hbox6, Open_Session.Label73, False, False, 7);
   Set_Alignment (Open_Session.Label73, 0.5, 0.5);
   Set_Padding (Open_Session.Label73, 0, 0);
   Set_Justify (Open_Session.Label73, Justify_Center);
   Set_Line_Wrap (Open_Session.Label73, False);

   Gtk_New (Open_Session.Entry1);
   Pack_Start (Open_Session.Hbox6, Open_Session.Entry1, True, True, 0);
   Set_Editable (Open_Session.Entry1, True);
   Set_Max_Length (Open_Session.Entry1, 0);
   Set_Text (Open_Session.Entry1, -"");
   Set_Visibility (Open_Session.Entry1, True);

   Gtk_New_Vseparator (Open_Session.Vseparator4);
   Pack_Start (Open_Session.Hbox7, Open_Session.Vseparator4, False, False, 7);

   Gtk_New_Vbox (Open_Session.Vbox19, False, 0);
   Pack_Start (Open_Session.Hbox7, Open_Session.Vbox19, True, True, 0);

   Gtk_New (Open_Session.Scrolledwindow11);
   Pack_Start (Open_Session.Vbox19, Open_Session.Scrolledwindow11, True, True, 0);
   Set_Policy (Open_Session.Scrolledwindow11, Policy_Automatic, Policy_Automatic);

   Gtk_New (Open_Session.Viewport2);
   Add (Open_Session.Scrolledwindow11, Open_Session.Viewport2);
   Set_Shadow_Type (Open_Session.Viewport2, Shadow_In);

   Gtk_New_Vbox (Open_Session.File_Buttons, False, 0);
   Add (Open_Session.Viewport2, Open_Session.File_Buttons);

   Gtk_New (Open_Session.Hbuttonbox10);
   Pack_Start (Open_Session.Vbox19, Open_Session.Hbuttonbox10, False, False, 0);
   Set_Spacing (Open_Session.Hbuttonbox10, 30);
   Set_Layout (Open_Session.Hbuttonbox10, Buttonbox_Spread);
   Set_Child_Size (Open_Session.Hbuttonbox10, 85, 27);
   Set_Child_Ipadding (Open_Session.Hbuttonbox10, 7, 0);

   Gtk_New (Open_Session.Select_All, -"Select all");
   Set_Flags (Open_Session.Select_All, Can_Default);
   Button_Callback.Connect
     (Open_Session.Select_All, "clicked",
      Button_Callback.To_Marshaller (On_Select_All_Clicked'Access));
   Add (Open_Session.Hbuttonbox10, Open_Session.Select_All);

   Gtk_New (Open_Session.Unselect_All, -"Unselect all");
   Set_Flags (Open_Session.Unselect_All, Can_Default);
   Button_Callback.Connect
     (Open_Session.Unselect_All, "clicked",
      Button_Callback.To_Marshaller (On_Unselect_All_Clicked'Access));
   Add (Open_Session.Hbuttonbox10, Open_Session.Unselect_All);

   Gtk_New_Hseparator (Open_Session.Hseparator1);
   Pack_Start (Open_Session.Vbox17, Open_Session.Hseparator1, False, False, 0);

   Gtk_New (Open_Session.Hbuttonbox9);
   Add (Open_Session.Vbox17, Open_Session.Hbuttonbox9);
   Set_Spacing (Open_Session.Hbuttonbox9, 30);
   Set_Layout (Open_Session.Hbuttonbox9, Buttonbox_Spread);
   Set_Child_Size (Open_Session.Hbuttonbox9, 85, 27);
   Set_Child_Ipadding (Open_Session.Hbuttonbox9, 7, 0);

   Gtk_New (Open_Session.Ok_Button, -"OK");
   Set_Flags (Open_Session.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Ok_Button);

   Gtk_New (Open_Session.Cancel_Button, -"Cancel");
   Set_Flags (Open_Session.Cancel_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Cancel_Button);

   Gtk_New (Open_Session.Help_Button, -"Help");
   Set_Flags (Open_Session.Help_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Help_Button, "clicked",
      Button_Callback.To_Marshaller (On_Help_Button_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Help_Button);
end Initialize;

------------------
-- Save_Session --
------------------

procedure Save_Session
  (Window : access Gtk_Widget_Record'Class;
   Open   : in out Open_Session_Access;
   Dir    : in String)
is
   File          : File_Type;
   Top           : constant Main_Debug_Window_Access :=
     Main_Debug_Window_Access (Window);
   Tab           : Debugger_Process_Tab;
   Directory     : Dir_Type;
   Buffer        : String (1 .. 256);
   Last          : Natural;
   Item          : Gtk_List_Item;
   Program       : GNAT.OS_Lib.String_Access;
   Debugger_List : Debugger_List_Link := Top.First_Debugger;
   Num_Debuggers : Natural := 0;
   use String_History;

begin
   if Open = null then
      Gtk_New (Open);
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
      if Get_Text (Open.Entry1) /= "" then
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
      end if;
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
   Open   : in out Open_Session_Access;
   Dir    : in String)
is
   Directory    : Dir_Type;
   Buffer       : String (1 .. 256);
   Last         : Natural;
   Item         : Gtk_List_Item;
   File         : File_Type;
   Num_Pages    : Gint;
   use String_History;

begin
   if Open = null then
      Gtk_New (Open);
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
            Program.Program := new String' (Buffer (1 .. Last));
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
                 (Main_Debug_Window_Access (Window),
                  Program.Debugger,
                  Program.Program.all,
                  List,
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
               Set_Busy_Cursor (Tab, True);

               if Buffer (Index + 1) = 'H' then
                  Send
                    (Tab.Debugger,
                     Buffer (Index + 3 .. Last),
                     Wait_For_Prompt => True,
                     Mode => Hidden);

               else
                  Process_User_Command
                    (Tab, Buffer (Index + 3 .. Last),
                     Mode => Odd.Types.Visible);
               end if;

               Set_Busy_Cursor (Tab, False);
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
  (Open  : access Open_Session_Record'Class;
   Label : in String)
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
  (Open      : access Open_Session_Record'Class;
   File_Name : in String)
is
   File         : File_Type;
   Num_Pages    : Gint;
   Buffer       : String (1 .. 256);
   Last         : Natural;

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
  (Open : access Open_Session_Record'Class)
is
   procedure Free is new Unchecked_Deallocation (Button_Node, Button_Link);
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

end Open_Session_Pkg;
