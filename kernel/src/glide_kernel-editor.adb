-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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
with Src_Info.Queries;     use Src_Info.Queries;
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
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Open a file and return the handle associated with it. If the given
   --  file does not exist, create an empty editor only if Create_New is True.
   --  ??? Need more comments.

   function Get_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return Source_Editor_Box;
   --  Return the source editor that has currently the focus in the MDI
   --  window associated with Top, null if the focus child is not an editor.

   type LI_File_Update_Status is (Failure, Success);
   --  The status returned by the Update_LI_File_If_Necessary routine.

   procedure Update_LI_File_If_Necessary
     (Kernel          : access Kernel_Handle_Record'Class;
      Source_Info     : in out LI_File_Ptr;
      Source_Filename : String;
      Status          : out LI_File_Update_Status);
   --  Bring the given Source_Info to an up to date. If Source_Info is null,
   --  then Source_Filename is used to locate the associated LI file and
   --  parse it.

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

   function Get_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return Source_Editor_Box
   is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI     : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Focus   : constant Gtk_Widget := Get_Widget (Get_Focus_Child (MDI));

   begin
      if Focus.all in Source_Box_Record'Class then
         --  Simple case, the current focus window is a source editor
         return Source_Box (Focus).Editor;

      elsif Kernel.Current_Editor = null then
         return null;
      else
         --  Otherwise, return the last source editor registered that had
         --  the focus.
         return Source_Box (Get_Widget (Kernel.Current_Editor)).Editor;
      end if;
   end Get_Current_Editor;

   ---------------------------------
   -- Update_LI_File_If_Necessary --
   ---------------------------------

   procedure Update_LI_File_If_Necessary
     (Kernel          : access Kernel_Handle_Record'Class;
      Source_Info     : in out LI_File_Ptr;
      Source_Filename : String;
      Status          : out LI_File_Update_Status) is
   begin
      Status := Success;

      if Source_Info = No_LI_File or else Is_Incomplete (Source_Info) then
         --  ??? At the moment, this procedure does not handle out of date
         --  ??? LI information. This should be corrected.
         declare
            ALI_Filename : constant String := ALI_Filename_From_Source
              (Source_Filename, Get_Project_View (Kernel),
               Get_Source_Path (Kernel));

            --  ??? We only have the ALI_Filename

            Success : Boolean;
         begin
            Parse_ALI_File
              (ALI_Filename, Get_Project_View (Kernel), Kernel.Source_Path.all,
               Kernel.Source_Info_List, Source_Info, Success);
            --  ??? Define functions to access the source path and use it
            --  ??? to access this function. The idea is that they can hide
            --  ??? some caching mechnism.
            if not Success then
               Console.Insert
                 (Kernel, "Could not find associated ALI file.",
                  Highlight_Sloc => False);
               Status := Failure;
            end if;
         end;
      end if;
   end Update_LI_File_If_Necessary;

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
      Set_Size_Request (Box, Default_Editor_Width, Default_Editor_Height);
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
      Current : constant Source_Editor_Box := Get_Current_Editor (Kernel);
      Title   : constant String := Get_Filename (Current);
      Editor  : Source_Editor_Box;
      Box     : Source_Box;
      Child   : MDI_Child;

   begin
      if Current /= null then
         Create_New_View (Editor, Current);
         Gtk_New (Box, Editor);
         Set_Size_Request (Box, Default_Editor_Width, Default_Editor_Height);
         Attach (Editor, Box);
         Child := Put (MDI, Box);
         --  ??? Should compute the right number.
         Set_Title (Child, Base_File_Name (Title) & " <2>");
      end if;
   end New_View;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box
   is
      Top         : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI         : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Short_File  : String := Base_File_Name (File);
      Success     : Boolean;
      Editor      : Source_Editor_Box;
      Box         : Source_Box;
      Child       : MDI_Child;
      File_Exists : Boolean;
      Iter        : Child_Iterator := First_Child (MDI);

   begin
      if File = "" then
         return null;
      end if;

      --  ??? Should do a search on the full filename instead
      loop
         Child := Get (Iter);
         exit when Child = null
           or else Get_Title (Child) = Short_File;
         Next (Iter);
      end loop;

      if Child /= null then
         Raise_Child (Child);
         Set_Focus_Child (Child);
         return Source_Box (Get_Widget (Child)).Editor;
      end if;

      --  At this point, we know that this file has not been opened yet,
      --  so we need to open it.

      File_Exists := Is_Regular_File (File);

      --  Create a new editor only if the file exists or we are asked to
      --  create a new empty one anyway.
      if File_Exists or else Create_New then
         Gtk_New (Editor, Top.Kernel);
      end if;

      if File_Exists then
         Load_File (Editor, File, Success => Success);
         if not Success then
            Destroy (Editor);
         end if;
      end if;

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Gtk_New (Box, Editor);
         Set_Size_Request (Box, Default_Editor_Width, Default_Editor_Height);
         Attach (Editor, Box);
         Child := Put (MDI, Box);
         Set_Title (Child, Short_File);
      else
         Console.Insert
           (Kernel, "Can not open file '" & File & "'",
            Highlight_Sloc => False);
      end if;

      return Editor;
   end Open_File;

   procedure Open_File
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : String;
      Success : out Boolean)
   is
      Editor  : Source_Editor_Box;
   begin
      Editor := Open_File (Kernel, File, Create_New => False);
      Success := Editor /= null;
   end Open_File;

   --------------------
   -- Open_Or_Create --
   --------------------

   procedure Open_Or_Create
     (Kernel : access Kernel_Handle_Record'Class;
      File   : String)
   is
      Editor  : Source_Editor_Box;
   begin
      Editor := Open_File (Kernel, File);
   end Open_Or_Create;

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
      Highlight : Boolean := True;
      Success   : out Boolean)
   is
      Edit : Source_Editor_Box;
      Id   : Idle_Handler_Id;
   begin
      Edit := Open_File (Kernel, File, Create_New => False);
      Success := Edit /= null;

      --  Abort if we failed to read the file
      if not Success then
         return;
      end if;

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
      Source : constant Source_Editor_Box := Get_Current_Editor (Kernel);
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
      Source : constant Source_Editor_Box := Get_Current_Editor (Kernel);
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
      Source : constant Source_Editor_Box := Get_Current_Editor (Kernel);
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
      Source : constant Source_Editor_Box := Get_Current_Editor (Kernel);
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
      Source : constant Source_Editor_Box := Get_Current_Editor (Kernel);
   begin
      if Source = null then
         return;
      end if;

      Select_All (Source);
   end Select_All;

   ----------------------
   -- Get_Editor_Child --
   ----------------------

   function Get_Editor_Child
     (Kernel : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Child
   is
   begin
      return Kernel.Current_Editor;
   end Get_Editor_Child;

   ----------------------
   -- Set_Editor_Child --
   ----------------------

   procedure Set_Editor_Child (Kernel : access Kernel_Handle_Record'Class) is
      Top   : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI   : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Child : constant MDI_Child := Get_Focus_Child (MDI);

   begin
      if Get_Widget (Child).all in Source_Box_Record'Class then
         Kernel.Current_Editor := Child;
      else
         Kernel.Current_Editor := null;
      end if;
   end Set_Editor_Child;

   -------------------------
   -- Get_Editor_Filename --
   -------------------------

   function Get_Editor_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String
   is
      Source : constant Source_Editor_Box := Get_Current_Editor (Kernel);
   begin
      if Source = null then
         return "";
      else
         return Get_Filename (Source);
      end if;
   end Get_Editor_Filename;

   ------------------------------
   -- Goto_Declaration_Or_Body --
   ------------------------------

   procedure Goto_Declaration_Or_Body
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Source        : constant Source_Editor_Box :=
        Get_Current_Editor (Kernel);
      Source_Info   : LI_File_Ptr;
      Filename      : String_Access;
      Start_Line    : Positive;
      Start_Column  : Positive;
      End_Line      : Positive;
      End_Column    : Positive;
      Status        : Src_Info.Queries.Find_Decl_Or_Body_Query_Status;
      Is_Success    : Boolean;
      Update_Status : LI_File_Update_Status;

   begin
      if Source = null or else Get_Filename (Source) = "" then
         Console.Insert
           (Kernel, "Cross-references not possible on unamed files!",
            Highlight_Sloc => False);
         return;
      end if;

      Source_Info := Locate_From_Source
        (Get_Source_Info_List (Kernel), Get_Filename (Source));
      Update_LI_File_If_Necessary
        (Kernel, Source_Info, Get_Filename (Source), Update_Status);

      Find_Declaration_Or_Body
        (Editor => Source, Lib_Info => Source_Info, Filename => Filename,
         Start_Line => Start_Line, Start_Column => Start_Column,
         End_Line => End_Line, End_Column => End_Column, Status => Status);

      case Status is
         when Entity_Not_Found =>
            Console.Insert
              (Kernel, "Cross-reference failed.", Highlight_Sloc => False);
            return;
         when Internal_Error =>
            Console.Insert
              (Kernel, "Cross-reference internal error detected.",
               Highlight_Sloc => False);
            return;
         when No_Body_Entity_Found =>
            Console.Insert
              (Kernel,
               "This entity does not have an associated declaration or body.",
               Highlight_Sloc => False);
            return;
         when Success =>
            null; --  No error message to print
      end case;

      --  Extra safety check, verify that Filename is not null before starting
      --  dereferencing it. That would be a programing error
      if Filename = null then
         Console.Insert
           (Kernel, "Internal error detected.", Highlight_Sloc => False);
         --  Note that the error message is different from the internal error
         --  message above. This will help us pin-pointing the location of
         --  the error message without any doubt, thus helping us debug the
         --  situation, should this happen. (but it won't, of course :-)
         return;
      end if;

      --  Open the file, and reset Source to the new editor in order to
      --  highlight the region returned by the Xref query.
      Go_To
        (Kernel, Filename.all, Start_Line, Start_Column,
         Highlight => False, Success => Is_Success);

      --  Abort if we failed to go to the xref location. An error message
      --  has already been printed, so just bail-out.
      if not Is_Success then
         return;
      end if;

      Unhighlight_All (Source);
      Highlight_Region
        (Source, Start_Line, Start_Column, End_Line, End_Column);
   end Goto_Declaration_Or_Body;

   -----------------------
   -- Find_Dependencies --
   -----------------------

   procedure Find_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      Source_Name  : String;
      Dependencies : out Src_Info.Queries.Dependency_List;
      Status       : out Src_Info.Queries.Dependencies_Query_Status)
   is
      Source_Info   : LI_File_Ptr;
      Update_Status : LI_File_Update_Status;

      procedure Handle_Internal_Error (Msg : String);
      --  Set the return values to appropriate values when an internal error
      --  is detected.

      procedure Handle_Internal_Error (Msg : String) is
      begin
         Console.Insert
           (Kernel, "Internal Error detected: " & Msg & ".",
            Highlight_Sloc => False);
         Status := Internal_Error;
      end Handle_Internal_Error;

   begin
      Dependencies := null;

      if Source_Name = "" then
         Handle_Internal_Error (Msg => "Empty source file name");
         return;
      end if;

      Source_Info := Locate_From_Source
        (Get_Source_Info_List (Kernel), Source_Name);
      Update_LI_File_If_Necessary
        (Kernel, Source_Info, Source_Name, Update_Status);

      case Update_Status is
         when Failure =>
            Destroy (Dependencies);
            Dependencies := null;
            Status := Failure;
         when Success =>
            Find_Dependencies (Source_Info, Dependencies, Status);
      end case;

   end Find_Dependencies;

end Glide_Kernel.Editor;
