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

with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Main_Window;    use Glide_Main_Window;
with Glide_Page;           use Glide_Page;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Gtk.Box;              use Gtk.Box;
with Gtk.Main;             use Gtk.Main;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.MDI;           use Gtkada.MDI;
with Src_Editor_Box;       use Src_Editor_Box;
with Src_Info;             use Src_Info;
with Src_Info.ALI;         use Src_Info.ALI;
with GVD.Process;          use GVD.Process;
with String_Utils;         use String_Utils;

package body Glide_Kernel.Editor is

   --  ??? Preferences
   Default_Editor_Width  : constant := 400;
   Default_Editor_Height : constant := 400;

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   procedure Gtk_New
     (Box    : out Source_Box;
      Editor : Source_Editor_Box);
   --  Create a new source box.

   procedure Initialize
     (Box    : access Source_Box_Record'Class;
      Editor : Source_Editor_Box);
   --  Internal initialization function.

   function Open_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String) return Source_Editor_Box;
   --  Open a file and return the handle associated with it.
   --  ??? Need more comments.

   function Get_Current_Editor
     (Top : Glide_Window) return Source_Editor_Box;
   --  Return the source editor that has currently the focus in the MDI
   --  window associated with Top, null the focus child is not an editor.

   function Tmp_Get_ALI_Filename
     (Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String)
      return String;
   --  ??? Return the full path to the ALI file associated to the given source
   --  ??? file. Over simplistic algorithm applied: replaces the extension by
   --  ??? ".ali". That'll do it until the demo.

   type Location_Idle_Data is record
      Edit : Source_Editor_Box;
      Line, Column : Natural;
   end record;

   package Location_Idle is new Gtk.Main.Idle (Location_Idle_Data);

   function Location_Callback (D : Location_Idle_Data) return Boolean;
   --  Idle callback used to scroll the source editors.

   ------------------------
   -- Get_Current_Editor --
   ------------------------

   function Get_Current_Editor (Top : Glide_Window) return Source_Editor_Box is
      MDI         : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Focus_Child : constant MDI_Child := Get_Focus_Child (MDI);
      Source : Gtk_Widget;

   begin
      if Focus_Child = null then
         return null;
      end if;

      Source := Get_Widget (Focus_Child);

      if Source.all in Source_Box_Record'Class then
         return Source_Box (Source).Editor;
      else
         return null;
      end if;
   end Get_Current_Editor;

   --------------------------
   -- Tmp_Get_ALI_Filename --
   --------------------------

   function Tmp_Get_ALI_Filename
     (Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String)
      return String
   is
      Short_Source : constant String := Base_File_Name (Source_Filename);
      Last_Dot     : Integer := Short_Source'Last;
      ALI_Ext      : constant String := ".ali";
   begin
      while Last_Dot >= Short_Source'First loop
         exit when Short_Source (Last_Dot) = '.';
         Last_Dot := Last_Dot - 1;
      end loop;
      if Last_Dot < Short_Source'First then
         Last_Dot := Short_Source'Last + 1;
      end if;

      declare
         Short_Result : constant String :=
           Short_Source (Short_Source'First .. Last_Dot - 1) & ALI_Ext;
         Path : constant String :=
           Find_Object_File (Kernel, Short_Result, Use_Object_Path => True);
      begin
         if Path = "" then
            return Short_Result;
         else
            return Path;
         end if;
      end;
   end Tmp_Get_ALI_Filename;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box    : out Source_Box;
      Editor : Source_Editor_Box) is
   begin
      Box := new Source_Box_Record;
      Initialize (Box, Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Box_Record'Class;
      Editor : Source_Editor_Box) is
   begin
      Gtk.Box.Initialize_Hbox (Box);
      Box.Editor := Editor;
   end Initialize;

   ----------------
   -- New_Editor --
   ----------------

   procedure New_Editor
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Top    : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI    : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Editor : Source_Editor_Box;
      Box    : Source_Box;
      Child  : MDI_Child;

   begin
      Gtk_New (Editor, Top.Kernel);
      Gtk_New (Box, Editor);
      Set_USize (Box, Default_Editor_Width, Default_Editor_Height);
      Attach (Editor, Box);
      Child := Put (MDI, Box);
      Show_All (Box);
      Set_Title (Child, "No Name");
   end New_Editor;

   --------------
   -- New_View --
   --------------

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI     : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Editor  : Source_Editor_Box;
      Box     : Source_Box;
      Focus   : constant MDI_Child := Get_Focus_Child (MDI);
      Current : Source_Box := Source_Box (Get_Widget (Focus));
      Child   : MDI_Child;
      Title   : constant String := Get_Title (Focus);

   begin
      Create_New_View (Editor, Current.Editor);
      Gtk_New (Box, Editor);
      Attach (Editor, Box);
      Child := Put (MDI, Box);
      Set_Title (Child, Title & " <2>");
   end New_View;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String) return Source_Editor_Box
   is
      Top        : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI        : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Short_File : String := Base_File_Name (File);
      Success    : Boolean;
      Editor     : Source_Editor_Box;
      Box        : Source_Box;
      Child      : MDI_Child;

   begin
      if File = "" then
         return null;
      end if;

      --  ??? Should do a search on the full filename instead
      Child := Find_MDI_Child (MDI, Short_File);

      if Child /= null then
         Raise_Child (Child);
         Set_Focus_Child (Child);
         return Source_Box (Get_Widget (Child)).Editor;
      end if;

      Gtk_New (Editor, Top.Kernel);
      Gtk_New (Box, Editor);
      Set_USize (Box, Default_Editor_Width, Default_Editor_Height);
      Attach (Editor, Box);
      Child := Put (MDI, Box);
      Set_Title (Child, Short_File);
      Load_File (Editor, File, Success => Success);

      return Editor;
   end Open_File;

   procedure Open_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String)
   is
      Editor  : Source_Editor_Box;
   begin
      Editor := Open_File (Kernel, File);
   end Open_File;

   -----------------------
   -- Location_Callback --
   -----------------------

   function Location_Callback (D : Location_Idle_Data) return Boolean is
   begin
      Set_Cursor_Location (D.Edit, D.Line, D.Column);
      return False;
   end Location_Callback;

   -----------
   -- Go_To --
   -----------

   procedure Go_To
     (Kernel    : access Kernel_Handle_Record'Class;
      File      : String;
      Line      : Natural := 0;
      Column    : Natural := 0;
      Highlight : Boolean := True)
   is
      Edit : Source_Editor_Box;
      Id : Idle_Handler_Id;
   begin
      Edit := Open_File (Kernel, File);

      --  For some reason, we can not directly call Set_Cursor_Location, since
      --  the source editor won't be scrolled the first time the editor is
      --  displayed. Doing this in an idle callback ensures that all the proper
      --  events and initializations have taken place before we try to scroll
      --  the editor.
      Id := Location_Idle.Add (Location_Callback'Access, (Edit, Line, Column));

      if Highlight then
         Highlight_Line (Edit, Line);
      end if;
   end Go_To;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean)
   is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Source  : constant Source_Editor_Box := Get_Current_Editor (Top);

   begin
      if Source = null then
         return;
      end if;

      Save_To_File (Source, Name, Success);
   end Save_To_File;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Source  : constant Source_Editor_Box := Get_Current_Editor (Top);

   begin
      if Source = null then
         return;
      end if;

      Cut_Clipboard (Source);
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Source  : constant Source_Editor_Box := Get_Current_Editor (Top);

   begin
      if Source = null then
         return;
      end if;

      Copy_Clipboard (Source);
   end Copy_Clipboard;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Source  : constant Source_Editor_Box := Get_Current_Editor (Top);

   begin
      if Source = null then
         return;
      end if;

      Paste_Clipboard (Source);
   end Paste_Clipboard;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Kernel : access Kernel_Handle_Record'Class) is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Source  : constant Source_Editor_Box := Get_Current_Editor (Top);

   begin
      if Source = null then
         return;
      end if;

      Select_All (Source);
   end Select_All;

   ---------------------
   -- Focus_Is_Editor --
   ---------------------

   function Focus_Is_Editor
     (Kernel : access Kernel_Handle_Record'Class) return Boolean is
   begin
      return Get_Current_Editor (Glide_Window (Kernel.Main_Window)) /= null;
   end Focus_Is_Editor;

   ---------------------
   -- Get_Focus_Title --
   ---------------------

   function Get_Focus_Title
     (Kernel : access Kernel_Handle_Record'Class) return String
   is
      Top    : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI    : constant MDI_Window   :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;

   begin
      return Get_Title (Get_Focus_Child (MDI));
   end Get_Focus_Title;

   ------------------------------
   -- Goto_Declaration_Or_Body --
   ------------------------------

   procedure Goto_Declaration_Or_Body
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Top          : constant Glide_Window :=
        Glide_Window (Kernel.Main_Window);
      Source       : Source_Editor_Box := Get_Current_Editor (Top);
      Source_Info  : LI_File_Ptr;
      Filename     : String_Access;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;

   begin
      if Get_Filename (Source) = "" then
         Console.Insert
           (Kernel, "Cross-references not possible on unamed files!",
            Highlight_Sloc => False);
         return;
      end if;

      Source_Info :=
        Locate_From_Source
          (Get_Source_Info_List (Kernel), Get_Filename (Source));

      if Source_Info = No_LI_File or else Is_Incomplete (Source_Info) then
         declare
            ALI_Filename : constant String :=
              Tmp_Get_ALI_Filename (Kernel, Get_Filename (Source));
            Success : Boolean;
         begin
            Parse_ALI_File
              (ALI_Filename, Get_Project_View (Kernel), Kernel.Source_Path.all,
               Kernel.Source_Info_List, Source_Info, Success);
            if not Success then
               Console.Insert
                 (Kernel, "Could not find associated ALI file",
                  Highlight_Sloc => False);
               return;
            end if;
         end;
      end if;

      Find_Declaration_Or_Body
        (Editor => Source, Lib_Info => Source_Info, Filename => Filename,
         Start_Line => Start_Line, Start_Column => Start_Column,
         End_Line => End_Line, End_Column => End_Column);

      if Filename = null then
         --  Means the query failed.
         Console.Insert
           (Kernel, "Cross-reference failed", Highlight_Sloc => False);
         --  ??? A more elaborate error message, with intl support will be
         --  ??? done later.
         return;
      end if;

      --  Open the file, and reset Source to the new editor in order to
      --  highlight the region returned by the Xref query.
      Go_To
        (Kernel, Filename.all, Start_Line, Start_Column, Highlight => False);
      Source := Get_Current_Editor (Top);
      Unhighlight_All (Source);
      Highlight_Region
        (Source, Start_Line, Start_Column, End_Line, End_Column);
   end Goto_Declaration_Or_Body;

end Glide_Kernel.Editor;
