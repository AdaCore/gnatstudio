with Glib; use Glib;
with Glib.Values; use Glib.Values;
with Gtk; use Gtk;

with Gdk.Types;       use Gdk.Types;
with Gdk.Pixbuf;      use Gdk.Pixbuf;

with Gtk.Widget;      use Gtk.Widget;
with Gtk.Main ; use Gtk.Main;
with Gtk.Arguments;          use Gtk.Arguments;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Box;                use Gtk.Box;
with Gtk.Toolbar;            use Gtk.Toolbar;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtkada.Handlers;        use Gtkada.Handlers;

with Vcs_View_Intl;          use Vcs_View_Intl;

with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with String_Utils; use String_Utils;
with GUI_Utils;    use GUI_Utils;

with VCS;
with VCS.CVS;

with Vcs_View_Pixmaps; use Vcs_View_Pixmaps;

package body Vcs_View_Pkg is

   --  use VCS_View_VCS;

   --  package VCS_View_CVS is new VCS.CVS (Vcs_View_Access);

   use VCS.String_List;

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   function Columns_Types return GType_Array is
   begin
      return GType_Array' (GType_Boolean,
                           --  Whether the file is selected or not.

                           GType_String,
                           --  The base file name of the file.

                           GType_String,
                           --  The local revision.

                           GType_String,
                           --  The repository revision.

                           GType_String,
                           --  The status description

                           Gdk.Pixbuf.Get_Type,
                           --  The status pixbuf

                           GType_String,
                           --  The Log for this file

                           Gtk.Widget.Get_Type
                           --  The widget that edits the log.
                           --  This should have a procedure which returns
                           --  the log string given a filename.
                          );
   end Columns_Types;

   Selected_Column           : constant Gint := 0;
   Name_Column               : constant Gint := 1;
   Local_Rev_Column          : constant Gint := 2;
   Rep_Rev_Column            : constant Gint := 3;
   Status_Description_Column : constant Gint := 4;
   Status_Pixbuf_Column      : constant Gint := 5;
   Log_Column                : constant Gint := 6;
   Log_Editor_Column         : constant Gint := 7;

   function Number_Of_Columns return Gint;
   --  Return the number of columns in the model.

   function Number_Of_Columns return Gint is
   begin
      return Columns_Types'Length;
   end Number_Of_Columns;

   -----------------------
   -- Local subprograms --
   -----------------------

   type Message_Type is (Info, Error, Verbose);

   procedure Launch_Viewer
     (Explorer : access Vcs_View_Record'Class;
      Strings  : in out List;
      Title    : String := "");
   --  Display a String_List.
   --  Strings is freed by that procedure.

   procedure Launch_Editor
     (Explorer : access Vcs_View_Record'Class;
      Filename : String);
   --  Launch an editor for the given file.

   procedure Push_Message
     (Explorer : access Vcs_View_Record'Class;
      M_Type   : Message_Type;
      Message  : String);
   --  Display a message in the Message_Text.

   procedure Handle_VCS_Error
     (Message  : String;
      Explorer : Gtk_Widget);
   --  Handle the error message output by VCS operations.

   procedure Display_String_List
     (Explorer : access Vcs_View_Record'Class;
      List     : VCS.String_List.List;
      M_Type   : Message_Type);
   --  Convenience procedure to display a String_List.List.

   procedure Refresh_Files (Explorer : access Vcs_View_Record'Class;
                            Connect  : Boolean := False);
   --  Display the relevant entries in the local directory.

   procedure On_Edit_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_View_Diff_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Annotate_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_View_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Get_Status_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Update_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Commit_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Revert_Button_Add
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Add_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Remove_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure Set_Column_Types (Explorer : access Vcs_View_Record'Class);
   --

   procedure Toggled_Callback
     (Object      : access Gtk_Widget_Record'Class;
      Params      : Glib.Values.GValues);
   --

   procedure Set_Directory (Explorer  : access Vcs_View_Record'Class;
                            Directory : String);
   --  Sets the current directory to Directory.
   --  Directory must be an absolute directory name ending
   --  with Directory_Separator. If Directory is invalid, then
   --  the explorer will be set to the current directory.
   --  This procedure will also look for an acceptable VCS system for this
   --  directory.

   function Get_Selected_Files
     (Explorer : access Vcs_View_Record'Class)
     return VCS.String_List.List;
   --  Return the list of files that are selected.

   function Get_Iter_From_Name
     (Explorer : access Vcs_View_Record'Class;
      Name     : String)
     return Gtk_Tree_Iter;
   --  Return the Iter associated with the given name.
   --  Name is a base file name.
   --  Return Null_Iter if no such iter was found.

   procedure Fill_Info
     (Explorer      : access Vcs_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Status_Record : File_Status_Record;
      Selected      : Boolean := False;
      Success       : out Boolean);
   --  Fills the tree info at the given Iter with values from
   --  Status_Record.
   --  Success tells whether the information has been filled or not.

   -------------------
   -- Launch_Viewer --
   -------------------

   procedure Launch_Viewer
     (Explorer : access Vcs_View_Record'Class;
      Strings  : in out List;
      Title    : String := "")
   is
   begin
      while not Is_Empty (Strings) loop
         Put_Line (Head (Strings));
         Tail (Strings);
      end loop;
   end Launch_Viewer;

   -------------------
   -- Launch_Editor --
   -------------------

   procedure Launch_Editor
     (Explorer : access Vcs_View_Record'Class;
      Filename : String)
   is
   begin
      Put_Line ("glide " & Filename);
   end Launch_Editor;

   -------------------------
   -- Display_String_List --
   -------------------------

   procedure Display_String_List
     (Explorer : access Vcs_View_Record'Class;
      List     : VCS.String_List.List;
      M_Type   : Message_Type)
   is
      Temp_List : VCS.String_List.List := List;
   begin
      while not Is_Empty (Temp_List) loop
         Push_Message (Explorer, M_Type, "   "  & Head (List));
         Temp_List := Next (Temp_List);
      end loop;
   end Display_String_List;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer      : access Vcs_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Status_Record : File_Status_Record;
      Selected      : Boolean := False;
      Success       : out Boolean)
   is
      String_Value : GValue;
      Bool_Value   : GValue;
      use VCS.String_List;
   begin
      Success := True;

      if Is_Empty (Status_Record.File_Name)
        or else Is_Directory (Head (Status_Record.File_Name)) then
         Success := False;
         return;
      end if;

      Init (Bool_Value, Gtype_Boolean);
      Init (String_Value, GType_String);

      Set_Boolean (Bool_Value, Selected);
      Set_Value (Explorer.Model, Iter, Selected_Column, Bool_Value);

      Set_String  (String_Value,
                   Base_File_Name (Head (Status_Record.File_Name)));
      Set_Value (Explorer.Model, Iter, Name_Column, String_Value);

      if not Is_Empty (Status_Record.Working_Revision) then
         Set_String  (String_Value,
                      (Head (Status_Record.Working_Revision)));
      else
         Set_String  (String_Value,"n/a");
      end if;
      Set_Value (Explorer.Model, Iter, Local_Rev_Column, String_Value);

      if not Is_Empty (Status_Record.Repository_Revision) then
         Set_String  (String_Value,
                      (Head (Status_Record.Repository_Revision)));
      else
         Set_String  (String_Value,"n/a");
      end if;
      Set_Value (Explorer.Model, Iter, Rep_Rev_Column, String_Value);

      case Status_Record.Status is
         when Unknown =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Unknown_Pixbuf.all'Address);
         when Not_Registered =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Not_Registered_Pixbuf.all'Address);
         when Up_To_Date =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Up_To_Date_Pixbuf.all'Address);
         when Removed =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Removed_Pixbuf.all'Address);
         when Modified =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Modified_Pixbuf.all'Address);
         when Needs_Merge =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Needs_Merge_Pixbuf.all'Address);
         when Needs_Update =>
            Set_Value (Explorer.Model, Iter, Status_Pixbuf_Column,
                       Status_Needs_Update_Pixbuf.all'Address);
      end case;

      Set_String  (String_Value,
                   File_Status'Image (Status_Record.Status));
      Set_Value (Explorer.Model, Iter, Status_Description_Column, String_Value);

      Set_String (String_Value, "");
      Set_Value (Explorer.Model, Iter, Log_Column, String_Value);
   end Fill_Info;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access Vcs_View_Record'Class;
      Name     : String)
     return Gtk_Tree_Iter
   is
      Iter    : Gtk_Tree_Iter;
      Success : Boolean;

   begin
      Tree_Model_Get_Iter_Root (Explorer.Model, Iter, Success);

      while Success loop
         if Gtk.Tree_Model.Get_String
           (Explorer.Model, Iter, Name_Column) = Name
         then
            return Iter;
         end if;

         Tree_Model_Iter_Next (Explorer.Model, Iter, Success);
      end loop;

      return Null_Iter;
   end Get_Iter_From_Name;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : access Vcs_View_Record'Class)
     return VCS.String_List.List
   is
      Iter : Gtk_Tree_Iter;
      Success : Boolean;
      Toggled : Boolean;

      Result : VCS.String_List.List;
      package Boolean_Data is new Model_Data (Boolean);
   begin
      if Explorer.Current_Directory = null then
         return Result;
      end if;

      Tree_Model_Get_Iter_Root (Explorer.Model, Iter, Success);

      while Success loop
         Toggled := Boolean_Data.Get (Explorer.Model, Iter, Selected_Column);

         if Toggled then
            VCS.String_List.Append
              (Result,
               Explorer.Current_Directory.all
               & Get_String (Explorer.Model, Iter, Name_Column));
         end if;

         Tree_Model_Iter_Next (Explorer.Model, Iter, Success);
      end loop;

      return Result;
   end Get_Selected_Files;

   -------------------
   -- Refresh_Files --
   -------------------

   procedure Refresh_Files
     (Explorer : access Vcs_View_Record'Class;
      Connect  : Boolean := False)
   is
      Iter : Gtk_Tree_Iter;
      L : File_Status_List.List;
      Directory_List : VCS.String_List.List;

      use File_Status_List;

      Success : Boolean := True;
   begin
      if Explorer.Current_Directory = null then
         return;
      end if;

      Clear (Explorer.Model);

      VCS.String_List.Append (Directory_List, Explorer.Current_Directory.all);

      if Connect then
         L := Get_Status (Explorer.VCS_Ref, Directory_List);
      else
         L := Local_Get_Status (Explorer.VCS_Ref, Directory_List);
      end if;

      while not Is_Empty (L) loop
         Append (Explorer.Model, Iter, Null_Iter);
         Fill_Info (Explorer, Iter, Head (L), False, Success);
         if not Success then
            Remove (Explorer.Model, Iter);
         end if;
         Tail (L);
      end loop;

      Columns_Autosize (Explorer.Tree);
   end Refresh_Files;

   ------------------
   -- Push_Message --
   ------------------

   procedure Push_Message
     (Explorer : access Vcs_View_Record'Class;
      M_Type   : Message_Type;
      Message  : String)
   is
   begin
      --  ??? Right now we display any message.
      --  Later, we may want to be able to
      --    - use a different color for error messages
      --    - let the user decide what kind of message he wants
      --    - pass the message to the glide console instead
      --  and so on...

      if M_Type = Error then
         Insert (Explorer.Message_Text,
                 Chars => "    Error : ");
      end if;

      Insert (Explorer.Message_Text,
              Chars => Message & ASCII.LF);
   end Push_Message;

   --------------------------------
   -- On_Edit_Log_Button_Clicked --
   --------------------------------

   procedure On_Edit_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      null;
   end On_Edit_Log_Button_Clicked;

   --------------------------------
   -- On_View_Diff_Button_Clicked --
   --------------------------------

   procedure On_View_Diff_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L : VCS.String_List.List := Get_Selected_Files (Explorer);
      L_Temp : List := L;
      Temp_String_List : List;
   begin
      Push_Message (Explorer,
                    Verbose,
                    "Viewing diffs for files :");

      Display_String_List (Explorer, L, Verbose);

      while not Is_Empty (L_Temp) loop
         Temp_String_List := Diff (Explorer.VCS_Ref, Head (L_Temp));
         Launch_Viewer (Explorer, Temp_String_List,
                        "Diff for current revision of " & Head (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Push_Message (Explorer, Verbose, "... done." & ASCII.LF);
   end On_View_Diff_Button_Clicked;

   --------------------------------
   -- On_Annotate_Button_Clicked --
   --------------------------------

   procedure On_Annotate_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L : VCS.String_List.List := Get_Selected_Files (Explorer);
      L_Temp : List := L;

      Temp_String_List : List;
   begin
      Push_Message (Explorer,
                    Verbose,
                    "Annotating files :");

      Display_String_List (Explorer, L, Verbose);

      while not Is_Empty (L_Temp) loop
         Temp_String_List := Annotate (Explorer.VCS_Ref, Head (L_Temp));
         Launch_Viewer (Explorer, Temp_String_List,
                        "Annotating of " & Head (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Push_Message (Explorer, Verbose, "... done." & ASCII.LF);
   end On_Annotate_Button_Clicked;

   --------------------------------
   -- On_View_Log_Button_Clicked --
   --------------------------------

   procedure On_View_Log_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L : VCS.String_List.List := Get_Selected_Files (Explorer);
      L_Temp : List := L;

      Temp_String_List : List;
   begin
      Push_Message (Explorer,
                    Verbose,
                    "Viewing logs of files :");

      Display_String_List (Explorer, L, Verbose);

      while not Is_Empty (L_Temp) loop
         Temp_String_List := Log (Explorer.VCS_Ref, Head (L_Temp));
         Launch_Viewer (Explorer, Temp_String_List);
         L_Temp := Next (L_Temp);
      end loop;

      Push_Message (Explorer, Verbose, "... done." & ASCII.LF);
   end On_View_Log_Button_Clicked;

   ----------------------------------
   -- On_Get_Status_Button_Clicked --
   ----------------------------------

   procedure On_Get_Status_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      if VCS.String_List.Is_Empty (L) then
         if Explorer.Current_Directory = null then
            Explorer.Current_Directory := new String' (Get_Current_Dir);
         end if;

         Push_Message (Explorer,
                       Verbose,
                       "Querying status for files in directory "
                       & Explorer.Current_Directory.all
                       & " ... ");
         Set_Busy_Cursor (Get_Window (Explorer), True, True);
         Refresh_Files (Explorer, True);
         Set_Busy_Cursor (Get_Window (Explorer), False);
         Push_Message (Explorer, Info, "... done." & ASCII.LF);

      else
         Push_Message (Explorer,
                       Verbose,
                       "Querying status for files :");

         Display_String_List (Explorer, L, Verbose);

         declare
            Iter   : Gtk_Tree_Iter;
            Result : File_Status_List.List
              := Get_Status (Explorer.VCS_Ref, L);
            Dummy  : Boolean;
         begin

            while not File_Status_List.Is_Empty (Result) loop
               Iter := Get_Iter_From_Name
                 (Explorer,
                  Base_File_Name
                    (VCS.String_List.Head
                      (File_Status_List.Head (Result).File_Name)));

               if Iter /= Null_Iter then
                  Fill_Info (Explorer,
                             Iter,
                             File_Status_List.Head (Result),
                             True,
                             Dummy);
               end if;

               File_Status_List.Tail (Result);
            end loop;
            Push_Message (Explorer, Verbose, "... done." & ASCII.LF);
         end;
      end if;

      VCS.String_List.Free (L);
   end On_Get_Status_Button_Clicked;

   ------------------------------
   -- On_Update_Button_Clicked --
   ------------------------------

   procedure On_Update_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L        : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      if not Is_Empty (L) then
         Push_Message (Explorer,
                       Verbose,
                       "Updating files :");

         Display_String_List (Explorer, L, Verbose);

         Update (Explorer.VCS_Ref, L);

         Push_Message (Explorer,
                       Verbose,
                       "... done." & ASCII.LF);
         VCS.String_List.Free (L);


      else
         if Explorer.Current_Directory = null then
            Explorer.Current_Directory := new String' (Get_Current_Dir);
         end if;

         Push_Message (Explorer,
                       Verbose,
                       "Updating files in directory "
                       & Explorer.Current_Directory.all
                       & " ... ");

         Set_Busy_Cursor (Get_Window (Explorer), True, True);

         Append (L, Explorer.Current_Directory.all);
         Update (Explorer.VCS_Ref, L);

         Push_Message (Explorer,
                       Verbose,
                       "... done." & ASCII.LF);

         Set_Busy_Cursor (Get_Window (Explorer), False);
      end if;
      On_Get_Status_Button_Clicked (Object, Params);

      Free (L);
   end On_Update_Button_Clicked;

   ----------------------------
   -- On_Open_Button_Clicked --
   ----------------------------

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L        : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      if not Is_Empty (L) then
         Push_Message (Explorer,
                       Verbose,
                       "Opening files :");

         Display_String_List (Explorer, L, Verbose);

         Open (Explorer.VCS_Ref, L);

         declare
            L_Temp : List := L;
         begin
            while not Is_Empty (L_Temp) loop
               Launch_Editor (Explorer, Head (L_Temp));
               L_Temp := Next (L_Temp);
            end loop;
         end;

         Push_Message (Explorer,
                       Verbose,
                       "... done." & ASCII.LF);
         VCS.String_List.Free (L);

         On_Get_Status_Button_Clicked (Object, Params);
      end if;
   end On_Open_Button_Clicked;

   ------------------------------
   -- On_Commit_Button_Clicked --
   ------------------------------

   procedure On_Commit_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L        : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      if not Is_Empty (L) then
         Push_Message (Explorer,
                       Verbose,
                       "Committing files :");

         Display_String_List (Explorer, L, Verbose);

         --  ??? MUST deal with LOGS !!

         --  Commit (Explorer.VCS_Ref, L);

         Push_Message (Explorer,
                       Verbose,
                       "... done." & ASCII.LF);
         VCS.String_List.Free (L);

         On_Get_Status_Button_Clicked (Object, Params);
      end if;
   end On_Commit_Button_Clicked;

   --------------------------
   -- On_Revert_Button_Add --
   --------------------------

   procedure On_Revert_Button_Add
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Revert_Button_Add;

   ---------------------------
   -- On_Add_Button_Clicked --
   ---------------------------

   procedure On_Add_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L        : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      if not Is_Empty (L) then
         Push_Message (Explorer,
                       Verbose,
                       "Adding files :");

         Display_String_List (Explorer, L, Verbose);

         Add (Explorer.VCS_Ref, L);

         Push_Message (Explorer,
                       Verbose,
                       "... done." & ASCII.LF);
         VCS.String_List.Free (L);

         On_Get_Status_Button_Clicked (Object, Params);
      end if;
   end On_Add_Button_Clicked;

   ------------------------------
   -- On_Remove_Button_Clicked --
   ------------------------------

   procedure On_Remove_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Get_Toplevel (Object));
      L        : VCS.String_List.List := Get_Selected_Files (Explorer);
   begin
      if not Is_Empty (L) then
         Push_Message (Explorer,
                       Verbose,
                       "Removing files :");

         Display_String_List (Explorer, L, Verbose);

         Remove (Explorer.VCS_Ref, L);

         Push_Message (Explorer,
                       Verbose,
                       "... done." & ASCII.LF);
         VCS.String_List.Free (L);

         On_Get_Status_Button_Clicked (Object, Params);
      end if;
   end On_Remove_Button_Clicked;

   ----------
   -- Idle --
   ----------

   procedure Idle is
      No_Main_Loop : Boolean;
   begin
      while Gtk.Main.Events_Pending
      loop
         No_Main_Loop := Gtk.Main.Main_Iteration;
      end loop;
   end Idle;

   ----------------------
   -- Handle_VCS_Error --
   ----------------------

   procedure Handle_VCS_Error
     (Message  : String;
      Explorer : Gtk_Widget)
   is
   begin
      Push_Message (Vcs_View_Access (Explorer),
                    Error,
                    Message);
   end Handle_VCS_Error;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory (Explorer  : access Vcs_View_Record'Class;
                            Directory : String)
   is

   begin
      if Explorer.Current_Directory /= null then
         Free (Explorer.Current_Directory);
      end if;

      if Is_Absolute_Path (Directory)
        and then Is_Directory (Directory)
        and then Directory (Directory'Last) = Directory_Separator
      then
         Explorer.Current_Directory := new String'(Directory);
      else
         Explorer.Current_Directory := new String'(Get_Current_Dir);
      end if;

      --  Find an acceptable VCS for this directory.
      --  ??? right now, we assume only CVS is implemented.
      --  ??? we need functions in VCS.XXX to validate that a
      --  given entry is acceptable for a given directory.

      Explorer.VCS_Ref := new VCS.CVS.CVS_Record;

      Register_Idle_Function (Explorer.VCS_Ref,
                              Idle'Unrestricted_Access,
                              200);

      Register_Error_Function (Explorer.VCS_Ref,
                               Handle_VCS_Error'Unrestricted_Access,
                               Gtk_Widget (Explorer));
   end Set_Directory;

   ----------------
   -- Show_Files --
   ----------------

   procedure Show_Files (Explorer  : Vcs_View_Access;
                         Directory : String)
   is
   begin
      Set_Directory (Explorer, Directory);
      Refresh_Files (Explorer, False);
   end Show_Files;

   ------------------------
   -- Select_All_Toggled --
   ------------------------

   procedure Select_All_Toggled (Explorer : Vcs_View_Access)
   is
      Iter : Gtk_Tree_Iter;
      Success : Boolean;
      Toggled : Boolean;

      package Boolean_Data is new Model_Data (Boolean);

      Selection : Gtk_Tree_Selection := Get_Selection (Explorer.Tree);
   begin
      Explorer.Model_Sync := True;
      Unselect_All (Selection);

      Tree_Model_Get_Iter_Root (Explorer.Model, Iter, Success);

      while Success loop
         Toggled := Boolean_Data.Get (Explorer.Model, Iter, Selected_Column);

         if Toggled then
            Select_Iter (Selection, Iter);
         end if;

         Tree_Model_Iter_Next (Explorer.Model, Iter, Success);
      end loop;

   end Select_All_Toggled;

   ----------------------
   -- Toggled_Callback --
   ----------------------

   procedure Toggled_Callback
     (Object      : access Gtk_Widget_Record'Class;
      Params      : Glib.Values.GValues)
   is
      Explorer : Vcs_View_Access := Vcs_View_Access (Object);

      Iter : Gtk_Tree_Iter;

      Path_String : String := Get_String (Nth (Params, 1));
      Path   : Gtk_Tree_Path;

      Success : Boolean;
      Value   : GValue;

      package Boolean_Data is new Model_Data (Boolean);
   begin
      Gtk_New (Path, Path_String);

      Tree_Model_Get_Iter (Explorer.Model, Iter, Path, Success);

      Success := Boolean_Data.Get (Explorer.Model, Iter, Selected_Column);

      Init (Value, Gtype_Boolean);
      Set_Boolean (Value, not Success);
      Set_Value (Explorer.Model, Iter, Selected_Column, Value);

      --  Select_All_Toggled (Explorer);
   end Toggled_Callback;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access Vcs_View_Record'Class)
   is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Toggle_Rend : Gtk_Cell_Renderer_Toggle;

      Dummy       : Gint;
   begin
      Gtk_New (Text_Rend);
      Gtk_New (Toggle_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Explorer.Tree, True);

      Gtk_New (Col);
      Set_Title (Col, "");
      Pack_Start (Col, Toggle_Rend, True);
      Add_Attribute (Col, Toggle_Rend, "active", Selected_Column);
      Set_Clickable (Col, True);
      Dummy := Append_Column (Explorer.Tree, Col);


      Widget_Callback.Object_Connect
        (Toggle_Rend,
         "toggled",
         Toggled_Callback'Unrestricted_Access,
         Gtk_Widget (Explorer));

      Gtk_New (Col);
      Set_Title (Col, "Local file name");
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Status_Pixbuf_Column);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Name_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      --       Gtk_New (Col);
      --       Set_Title (Col, "Status");
      --       Pack_Start (Col, Text_Rend, True);
      --       Add_Attribute (Col, Text_Rend, "text", Status_Description_Column);
      --       Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, "Local revision");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Local_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, "Repository revision");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Rep_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, "Log");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Log_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Widget_Callback.Object_Connect
        (Text_Rend,
         "edited",
         Toggled_Callback'Unrestricted_Access,
         Gtk_Widget (Explorer));

      --  Set_Expander_Column (Explorer.Tree, Col);
   end Set_Column_Types;

   -------------------
   --  Create_Model --
   -------------------

   procedure Create_Model (Vcs_View : access Vcs_View_Record'Class)
   is
   begin
      Gtk_New (Vcs_View.Model, Number_Of_Columns, Columns_Types);
   end Create_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Vcs_View : out Vcs_View_Access) is
   begin
      Init_Graphics;
      --  ??? should the graphics be put at another location ?

      Vcs_View := new Vcs_View_Record;
      Vcs_View_Pkg.Initialize (Vcs_View);

      Show_Files (Vcs_View, "");
   end Gtk_New;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
     (Object      : access Gtk_Tree_Selection_Record'Class;
      Params      : Glib.Values.GValues)
   is
      Explorer : Vcs_View_Access :=
        Vcs_View_Access (Get_Toplevel (Get_Tree_View (Object)));
      Value    : GValue;

      package Toggle_Selected is
         new Gtk.Tree_Selection.Selection_Foreach (Vcs_View_Access);

      procedure Toggle (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                        Path  : Gtk.Tree_Model.Gtk_Tree_Path;
                        Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
                        Data  : Vcs_View_Access)
      is
      begin
         Set_Value (Explorer.Model, Iter, Selected_Column, Value);
      end Toggle;

      Success : Boolean;
      Iter    : Gtk_Tree_Iter;

      package Boolean_Data is new Model_Data (Boolean);
   begin
      if not Explorer.Model_Sync then
         Init (Value, Gtype_Boolean);
         Set_Boolean (Value, False);

         --  Set all items to not selected.
         Tree_Model_Get_Iter_Root (Explorer.Model, Iter, Success);

         while Success loop
            Set_Value (Explorer.Model, Iter, Selected_Column, Value);
            Tree_Model_Iter_Next (Explorer.Model, Iter, Success);
         end loop;

         Set_Boolean (Value, True);

         Toggle_Selected.Selected_Foreach
           (Object,
            Toggle'Unrestricted_Access,
            Explorer);
      else
         Explorer.Model_Sync := False;
      end if;
   end Selection_Changed;

   package Selection_Callback is new Gtk.Handlers.Callback (Gtk_Tree_Selection_Record);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Vcs_View : access Vcs_View_Record'Class) is
      pragma Suppress (All_Checks);

      Vbox1 : Gtk_Vbox;
      Hbox1 : Gtk_Hbox;
      Hbox2 : Gtk_Hbox;
      Toolbar2 : Gtk_Toolbar;
      Toolbar1 : Gtk_Toolbar;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Scrolledwindow2 : Gtk_Scrolled_Window;

      Selection : Gtk_Tree_Selection;
   begin
      Gtk.Window.Initialize (Vcs_View, Window_Toplevel);
      Set_Default_Size (Vcs_View, 600, 600);

      Set_Title (Vcs_View, -"");
      Set_Policy (Vcs_View, False, True, False);
      Set_Position (Vcs_View, Win_Pos_None);
      Set_Modal (Vcs_View, False);

      Gtk_New_Vbox (Vbox1, False, 0);
      Add (Vcs_View, Vbox1);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Vbox1, Hbox1, True, True, 3);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1, Policy_Automatic, Policy_Automatic);
      Pack_Start (Hbox1, Scrolledwindow1, True, True, 3);

      Create_Model (Vcs_View);

      Gtk_New (Vcs_View.Tree, Vcs_View.Model);

      Selection := Get_Selection (Vcs_View.Tree);

      --       Selection_Callback.Connect
      --         (Selection,
      --          "changed",
      --          Selection_Changed'Unrestricted_Access);

      Set_Mode (Selection, Selection_Multiple);

      Add (Scrolledwindow1, Vcs_View.Tree);

      --  Set columns types for the Tree.

      Set_Column_Types (Vcs_View);

      Gtk_New (Toolbar2, Orientation_Vertical, Toolbar_Both);
      --  Set_Space_Size (Toolbar2, 5);
      --  Set_Space_Style (Toolbar2, Toolbar_Space_Line);
      Set_Tooltips (Toolbar2, True);
      --  Set_Button_Relief (Toolbar2, Relief_Normal);
      Vcs_View.Edit_Log_Button := Append_Element
        (Toolbar => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text => -"Edit Log");
      Widget_Callback.Connect
        (Vcs_View.Edit_Log_Button, "clicked",
         On_Edit_Log_Button_Clicked'Access);

      Vcs_View.View_Diff_Button := Append_Element
        (Toolbar => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text => -"View Diff");
      Widget_Callback.Connect
        (Vcs_View.View_Diff_Button, "clicked",
         On_View_Diff_Button_Clicked'Access);

      Vcs_View.Annotate_Button := Append_Element
        (Toolbar => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text => -"Annotate");
      Widget_Callback.Connect
        (Vcs_View.Annotate_Button, "clicked",
         On_Annotate_Button_Clicked'Access);

      Vcs_View.View_Log_Button := Append_Element
        (Toolbar => Toolbar2,
         The_Type => Toolbar_Child_Button,
         Text => -"View Log");
      Widget_Callback.Connect
        (Vcs_View.View_Log_Button, "clicked",
         On_View_Log_Button_Clicked'Access);
      Pack_Start (Hbox1, Toolbar2, False, False, 3);

      Gtk_New (Toolbar1, Orientation_Vertical, Toolbar_Both);
      --  Set_Space_Size (Toolbar1, 5);
      --  Set_Space_Style (Toolbar1, Toolbar_Space_Empty);
      Set_Tooltips (Toolbar1, True);
      --  Set_Button_Relief (Toolbar1, Relief_Normal);
      Vcs_View.Get_Status_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Get status");
      Widget_Callback.Connect
        (Vcs_View.Get_Status_Button, "clicked",
         On_Get_Status_Button_Clicked'Access);
      Vcs_View.Update_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Update");
      Widget_Callback.Connect
        (Vcs_View.Update_Button, "clicked",
         On_Update_Button_Clicked'Access);
      Vcs_View.Open_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Open");
      Widget_Callback.Connect
        (Vcs_View.Open_Button, "clicked", On_Open_Button_Clicked'Access);
      Vcs_View.Commit_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Commit");
      Widget_Callback.Connect
        (Vcs_View.Commit_Button, "clicked", On_Commit_Button_Clicked'Access);
      Vcs_View.Revert_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Revert");
      Widget_Callback.Connect
        (Vcs_View.Revert_Button, "add", On_Revert_Button_Add'Access);
      Vcs_View.Add_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Add");
      Widget_Callback.Connect
        (Vcs_View.Add_Button, "clicked", On_Add_Button_Clicked'Access);
      Vcs_View.Remove_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Text => -"Remove");
      Widget_Callback.Connect
        (Vcs_View.Remove_Button, "clicked", On_Remove_Button_Clicked'Access);
      Pack_Start (Hbox1, Toolbar1, False, False, 3);

      Gtk_New_Hbox (Hbox2, False, 0);
      Pack_Start (Vbox1, Hbox2, True, True, 3);

      Gtk_New (Scrolledwindow2);
      Set_Policy (Scrolledwindow2, Policy_Never, Policy_Always);
      Pack_Start (Hbox2, Scrolledwindow2, True, True, 3);

      Gtk_New (Vcs_View.Message_Text);
      Set_Editable (Vcs_View.Message_Text, False);
      Add (Scrolledwindow2, Vcs_View.Message_Text);

   end Initialize;

end Vcs_View_Pkg;

--  ??? known problems, missing features, etc
--  when there is nothing in the tree, clicking raises storage_error
