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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Language_Handlers;         use Language_Handlers;
with Glide_Main_Window;         use Glide_Main_Window;
with Basic_Types;               use Basic_Types;
with GVD.Status_Bar;            use GVD.Status_Bar;
with GVD.Dialogs;               use GVD.Dialogs;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Entry_Completion;   use Gtkada.Entry_Completion;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Src_Editor_Box;            use Src_Editor_Box;
with String_List_Utils;         use String_List_Utils;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with Ada.Text_IO;               use Ada.Text_IO;
with Prj_API;                   use Prj_API;

package body Src_Editor_Module is

   Me : constant Debug_Handle := Create ("Src_Editor_Module");

   No_Handler : constant Handler_Id := (Null_Signal_Id, null);

   type Source_Editor_Module_Record is new Module_ID_Record with record
      Reopen_Menu_Item         : Gtk_Menu_Item;
      List                     : String_List_Utils.String_List.List;
      Source_Lines_Revealed_Id : Handler_Id := No_Handler;
      File_Edited_Id           : Handler_Id := No_Handler;
      Location_Open_Id         : Idle_Handler_Id := 0;
      Display_Line_Numbers     : Boolean    := False;
   end record;
   type Source_Editor_Module is access all Source_Editor_Module_Record'Class;

   procedure Destroy (Id : in out Source_Editor_Module_Record);
   --  Free the memory used by the module.

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   Max_Number_Of_Reopens : constant Integer := 10;
   --  ??? should we have a preference for that ?

   procedure Generate_Body_Cb (Data : Process_Data);
   --  Callback called when gnatstub has completed.

   procedure Pretty_Print_Cb (Data : Process_Data);
   --  Callback called when gnatpp has completed.

   procedure Gtk_New
     (Box : out Source_Box; Editor : Source_Editor_Box);
   --  Create a new source box.

   procedure Initialize
     (Box : access Source_Box_Record'Class; Editor : Source_Editor_Box);
   --  Internal initialization function.

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure Save_To_File
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String := "";
      Create_New : Boolean := True;
      Add_To_MDI : Boolean := True) return Source_Box;
   --  Open a file and return the handle associated with it.
   --  If Add_To_MDI is set to True, the box will be added to the MDI window.
   --  See Create_File_Exitor.

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box;
   --  Create a new text editor that edits File.
   --  If File is the empty string, or the file doesn't exist and Create_New is
   --  True, then an empty editor is created.
   --  No check is done to make sure that File is not already edited
   --  elsewhere. The resulting editor is not put in the MDI window.

   procedure Refresh_Reopen_Menu (Kernel : access Kernel_Handle_Record'Class);
   --  Fill the reopen menu.

   function Save_Function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Force  : Boolean := False) return Save_Return_Value;
   --  Save the text editor.
   --  If Force is False, then offer a choice to the user before doing so.

   type Location_Idle_Data is record
      Edit : Source_Editor_Box;
      Line, Column, Column_End : Natural;
   end record;

   package Location_Idle is new Gtk.Main.Idle (Location_Idle_Data);

   function Location_Callback (D : Location_Idle_Data) return Boolean;
   --  Idle callback used to scroll the source editors.

   procedure Location_Destroy (D : in out Location_Idle_Data);
   --  Called when the Idle callback to scroll the source editors is
   --  destroyed.

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean;
   --  Emit the File_Edited signal.

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open menu

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open From Path menu

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New View menu

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->New menu

   procedure On_Reopen
     (Widget : access GObject_Record'Class;
      Params : GValues;
      Kernel : Kernel_Handle);
   --  File->Reopen menu

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save menu

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save As... menu

   procedure On_Save_All_Editors
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save All Editors menu

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save All menu

   procedure On_Cut
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Cut menu

   procedure On_Copy
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Copy menu

   procedure On_Paste
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Paste menu

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Select All menu

   procedure On_Goto_Line
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Line... menu

   procedure On_Goto_Declaration
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Declaration menu
   --  Goto the declaration of the entity under the cursor in the current
   --  editor.

   procedure On_Goto_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Navigate->Goto Body menu
   --  Goto the next body of the entity under the cursor in the current
   --  editor.

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Generate Body menu

   procedure On_Pretty_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Edit->Pretty Print menu

   procedure On_Edit_File
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Edit a file (from a contextual menu)

   procedure On_Lines_Revealed
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Display the line numbers.

   procedure Source_Editor_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Generate the contextual menu entries for contextual menus in other
   --  modules than the source editor.

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create the current context for Glide_Kernel.Get_Current_Context

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access;
   --  Same as above.

   procedure New_View
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" signal.

   procedure File_Edited_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_edited" signal.

   procedure Preferences_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

   function Edit_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String;
   --  Interactive command handler for the source editor module.

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   function Edit_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;

      Node : List_Node;
      Filename : Basic_Types.String_Access;
      Line   : Natural := 0;
      Column : Natural := 0;
   begin
      if Command = "edit" then
         Node := First (Args);

         while Node /= Null_Node loop
            if Data (Node) = "-c" then
               Node := Next (Node);

               if Node = Null_Node then
                  Free (Filename);
                  return "edit: option -c requires a value.";
               end if;

               declare
               begin
                  Column := Natural'Value (Data (Node));
               exception
                  when others =>
                     Free (Filename);
                     return "edit: option -c requires a numerical value";
               end;

            elsif Data (Node) = "-l" then
               Node := Next (Node);

               if Node = Null_Node then
                  Free (Filename);
                  return "edit: option -l requires a value.";
               end if;

               declare
               begin
                  Column := Natural'Value (Data (Node));
               exception
                  when others =>
                     Free (Filename);
                     return "edit: option -l requires a numerical value";
               end;
            end if;

            if Filename = null then
               Filename := new String'(Data (Node));
            else
               return "edit: too many parameters.";
            end if;

            Node := Next (Node);
         end loop;

         if Filename /= null then
            Open_File_Editor
              (Kernel,
               Filename.all,
               Line,
               Column);

            Free (Filename);

            return "";
         else
            return "edit: missing parameter file_name.";
         end if;

      else
         return -"Command not recognized: " & Command;
      end if;
   end Edit_Command_Handler;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Id    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Infos : Line_Information_Data;
      File  : constant String := Get_String (Nth (Args, 1));
   begin
      if Id.Display_Line_Numbers then
         Create_Line_Information_Column
           (Kernel,
            File,
            Src_Editor_Module_Name,
            Stick_To_Data => False,
            Every_Line    => True);

         Infos := new Line_Information_Array (1 .. 1);
         Infos (1).Text := new String'("   1");

         Add_Line_Information
           (Kernel,
            File,
            Src_Editor_Module_Name,
            Infos);

         Unchecked_Free (Infos);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);
   begin
      return Save_Function
        (Get_Kernel (Source_Box (Widget).Editor), Gtk_Widget (Widget), False)
        = Cancel;
   end Delete_Callback;

   ------------------------
   -- File_Edit_Callback --
   ------------------------

   function File_Edit_Callback (D : Location_Idle_Data) return Boolean is
   begin
      File_Edited (Get_Kernel (D.Edit), Get_Filename (D.Edit));
      return False;
   end File_Edit_Callback;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget
   is
      Src  : Source_Box := null;
      File : Glib.String_Ptr;
      Data : Location_Idle_Data;
      Id   : Idle_Handler_Id;
   begin
      if Node.Tag.all = "Source_Editor" then
         File := Get_Field (Node, "File");

         if File /= null then
            Src := Open_File (User, File.all, False, False);

            if Src /= null then
               Data.Edit := Src.Editor;
               Id := Location_Idle.Add
                 (File_Edit_Callback'Access, (Src.Editor, 1, 1, 0));

               return Gtk_Widget (Src);
            end if;
         end if;
      end if;

      return null;
   end Load_Desktop;

   -----------------------
   -- On_Lines_Revealed --
   -----------------------

   procedure On_Lines_Revealed
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context      : constant Selection_Context_Access :=
        To_Selection_Context_Access (Get_Address (Nth (Args, 1)));
      Area_Context : File_Area_Context_Access;
      Infos        : Line_Information_Data;
      Line1, Line2 : Integer;

   begin
      if Context.all in File_Area_Context'Class then
         Area_Context := File_Area_Context_Access (Context);

         Get_Area (Area_Context, Line1, Line2);

         Infos := new Line_Information_Array (Line1 .. Line2);

         for J in Infos'Range loop
            Infos (J).Text := new String'(Image (J));
         end loop;

         if Has_File_Information (Area_Context) then
            Add_Line_Information
              (Kernel,
               Directory_Information (Area_Context) &
               File_Information (Area_Context),
               Src_Editor_Module_Name,
               Infos);
         else
            Add_Line_Information
              (Kernel,
               "",
               Src_Editor_Module_Name,
               Infos);
         end if;

         Unchecked_Free (Infos);
      end if;
   end On_Lines_Revealed;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N, Child : Node_Ptr;
   begin
      if Widget.all in Source_Box_Record'Class then
         N := new Node;
         N.Tag := new String'("Source_Editor");

         Child := new Node;
         Child.Tag := new String'("File");
         Child.Value := new String'
           (Get_Filename (Source_Box (Widget).Editor));
         Add_Child (N, Child);

         return N;
      end if;

      return null;
   end Save_Desktop;

   -----------------------------
   -- Get_Source_Box_From_MDI --
   -----------------------------

   function Get_Source_Box_From_MDI
     (Child : Gtkada.MDI.MDI_Child) return Source_Editor_Box is
   begin
      if Child = null then
         return null;
      else
         return Source_Box (Get_Widget (Child)).Editor;
      end if;
   end Get_Source_Box_From_MDI;

   -------------------------
   -- Find_Current_Editor --
   -------------------------

   function Find_Current_Editor
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child is
   begin
      return Find_MDI_Child_By_Tag (Get_MDI (Kernel), Source_Box_Record'Tag);
   end Find_Current_Editor;

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

   --------------
   -- New_View --
   --------------

   procedure New_View
     (Kernel : access Kernel_Handle_Record'Class)
   is
      MDI     : constant MDI_Window := Get_MDI (Kernel);
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Title   : constant String := Get_Filename (Current);
      Editor  : Source_Editor_Box;
      Box     : Source_Box;
      Child   : MDI_Child;

   begin
      if Current /= null then
         Create_New_View (Editor, Kernel, Current);
         Gtk_New (Box, Editor);
         Set_Size_Request
           (Box,
            Get_Pref (Kernel, Default_Widget_Width),
            Get_Pref (Kernel, Default_Widget_Height));
         Attach (Editor, Box);
         Child := Put (MDI, Box);

         Gtkada.Handlers.Return_Callback.Object_Connect
           (Box,
            "delete_event",
            Delete_Callback'Access,
            Gtk_Widget (Box),
            After => False);

         --  ??? Should compute the right number.
         Set_Title (Child, Title & " <2>", Base_Name (Title) & " <2>");
      end if;
   end New_View;

   -------------------
   -- Save_Function --
   -------------------

   function Save_Function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Force  : Boolean := False) return Save_Return_Value
   is
      Success        : Boolean;
      Containing_Box : constant Source_Box := Source_Box (Child);
      Box            : constant Source_Editor_Box := Containing_Box.Editor;
      Button         : Message_Dialog_Buttons;
   begin
      if Force then
         if Modified (Box) then
            Save_To_File (Box, Success => Success);
         end if;

      elsif Modified (Box) then
         Button := Message_Dialog
           (Msg            =>
              (-"Do you want to save file ") & Get_Filename (Box) & " ?",
            Dialog_Type    => Confirmation,
            Buttons        =>
              Button_Yes or Button_All or Button_No or Button_Cancel,
            Default_Button => Button_Cancel,
            Parent         => Get_Main_Window (Kernel));

         case Button is
            when Button_Yes =>
               Save_To_File (Box, Success => Success);
               return Saved;

            when Button_No =>
               return Not_Saved;

            when Button_All =>
               Save_To_File (Box, Success => Success);
               return Save_All;

            when others =>
               return Cancel;

         end case;
      end if;
      return Saved;
   end Save_Function;

   ------------------------
   -- Create_File_Editor --
   ------------------------

   function Create_File_Editor
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String;
      Create_New : Boolean := True) return Source_Editor_Box
   is
      Success     : Boolean;
      Editor      : Source_Editor_Box;
      File_Exists : Boolean := False;

   begin
      if File /= "" then
         File_Exists := Is_Regular_File (File);
      end if;

      --  Create a new editor only if the file exists or we are asked to
      --  create a new empty one anyway.

      if File_Exists or else Create_New then
         Gtk_New (Editor, Kernel_Handle (Kernel));
      else
         return null;
      end if;

      if File_Exists then
         Load_File (Editor, File, Success => Success);

         if not Success then
            Destroy (Editor);
            Editor := null;
         end if;

      else
         Load_Empty_File (Editor, File, Get_Language_Handler (Kernel));
      end if;

      return Editor;
   end Create_File_Editor;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : String := "";
      Create_New : Boolean := True;
      Add_To_MDI : Boolean := True) return Source_Box
   is
      MDI        : constant MDI_Window := Get_MDI (Kernel);
      Short_File : constant String := Base_Name (File);
      Editor     : Source_Editor_Box;
      Box        : Source_Box;
      Child      : MDI_Child;
      Iter       : Child_Iterator := First_Child (MDI);

   begin
      if File /= "" then
         loop
            Child := Get (Iter);
            exit when Child = null
              or else Get_Title (Child) = File;
            Next (Iter);
         end loop;

         if Child /= null then
            Raise_Child (Child);
            Set_Focus_Child (Child);
            return Source_Box (Get_Widget (Child));
         end if;
      end if;

      Editor := Create_File_Editor (Kernel, File, Create_New);

      --  If we have created an editor, put it into a box, and give it
      --  to the MDI to handle

      if Editor /= null then
         Gtk_New (Box, Editor);
         Set_Size_Request
           (Box,
            Get_Pref (Kernel, Default_Widget_Width),
            Get_Pref (Kernel, Default_Widget_Height));
         Attach (Editor, Box);

         if Add_To_MDI then
            Child := Put (MDI, Box);

            if File /= "" then
               Set_Title (Child, File, Short_File);
            else
               --  Determine the number of "Untitled" files open.

               declare
                  Iterator    : Child_Iterator := First_Child (MDI);
                  The_Child   : MDI_Child;
                  Nb_Untitled : Natural := 0;
                  No_Name     : constant String := -"Untitled";
               begin
                  The_Child := Get (Iterator);

                  while The_Child /= null loop
                     declare
                        Title : constant String := Get_Title (The_Child);
                     begin
                        if Title'Length >= No_Name'Length
                          and then Title
                            (Title'First
                               .. Title'First + No_Name'Length - 1) = No_Name
                        then
                           Nb_Untitled := Nb_Untitled + 1;
                        end if;
                     end;

                     Next (Iterator);
                     The_Child := Get (Iterator);
                  end loop;

                  if Nb_Untitled = 0 then
                     Set_Title (Child, -"Untitled");
                  else
                     declare
                        I : constant String := Natural'Image (Nb_Untitled + 1);
                     begin
                        Set_Title
                          (Child,
                           -"Untitled"
                             & " (" & I (I'First + 1 .. I'Last) & ")");
                     end;
                  end if;

                  Set_Filename (Editor, Get_Title (Child));
               end;
            end if;

            --  We have created a new file editor : emit the
            --  corresponding signal.
            --  ??? what do we do when opening an editor with no name ?

            File_Edited (Kernel, Get_Title (Child));
         end if;

         Gtkada.Handlers.Return_Callback.Object_Connect
           (Box,
            "delete_event",
            Delete_Callback'Access,
            Gtk_Widget (Box),
            After => False);

         --  Update and save the "Reopen" menu state.
         declare
            use String_List_Utils.String_List;

            Reopen_File_Name : constant String :=
              Format_Pathname (Get_Home_Dir (Kernel) & "/recent_files");
            Reopen_File      : File_Type;
            Counter          : Integer := 0;
            Node             : List_Node;
            The_Data         : Source_Editor_Module :=
              Source_Editor_Module (Src_Editor_Module_Id);

         begin
            Node := First (The_Data.List);

            while Node /= Null_Node loop
               Counter := Counter + 1;

               if Data (Node) = File then
                  Counter := -1;
                  exit;
               end if;

               Node := Next (Node);
            end loop;

            if Counter >= 0
              and then Node = Null_Node
            then
               Prepend (The_Data.List, File);

               if Counter >= Max_Number_Of_Reopens then
                  Node := Last (The_Data.List);
                  Node := Prev (The_Data.List, Node);
                  Remove_Nodes (The_Data.List, Node);
               end if;

               Open (Reopen_File, Out_File, Reopen_File_Name);
               Node := First (The_Data.List);

               while Node /= Null_Node loop
                  if Data (Node) /= "" then
                     Put_Line (Reopen_File, Data (Node));
                  end if;

                  Node := Next (Node);
               end loop;

               Close (Reopen_File);
               Refresh_Reopen_Menu (Kernel);
            end if;
         end;

      else
         Console.Insert
           (Kernel, (-"Cannot open file ") & "'" & File & "'",
            Highlight_Sloc => False,
            Mode           => Error);
      end if;

      return Box;
   end Open_File;

   -----------------------
   -- Location_Callback --
   -----------------------

   function Location_Callback (D : Location_Idle_Data) return Boolean is
   begin
      Grab_Focus (D.Edit);

      if Is_Valid_Location (D.Edit, D.Line, D.Column) then
         Set_Cursor_Location (D.Edit, D.Line, D.Column);

         if D.Column_End /= 0
           and then Is_Valid_Location (D.Edit, D.Line, D.Column_End)
         then
            Select_Region (D.Edit, D.Line, D.Column, D.Line, D.Column_End);
         end if;

      elsif Is_Valid_Location (D.Edit, D.Line) then
         Set_Cursor_Location (D.Edit, D.Line);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Location_Callback;

   ----------------------
   -- Location_Destroy --
   ----------------------

   procedure Location_Destroy (D : in out Location_Idle_Data) is
      pragma Unreferenced (D);
   begin
      Source_Editor_Module (Src_Editor_Module_Id).Location_Open_Id := 0;
   end Location_Destroy;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String := "";
      Success : out Boolean)
   is
      Child    : constant MDI_Child := Find_Current_Editor (Kernel);
      Source   : Source_Editor_Box;

   begin
      if Child = null then
         return;
      end if;

      Source := Source_Box (Get_Widget (Child)).Editor;

      declare
         Old_Name : constant String := Get_Filename (Source);
      begin
         Save_To_File (Source, Name, Success);

         --  Update the title, in case "save as..." was used.
         if Old_Name /= Get_Filename (Source) then
            Set_Title
              (Child, Get_Filename (Source),
               Base_Name (Get_Filename (Source)));
         end if;
      end;
   end Save_To_File;

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant String := Select_File (Title => -"Open File");
      begin
         if Filename = "" then
            return;
         end if;

         Open_File_Editor (Kernel, Filename);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_File;

   -----------------------
   -- On_Open_From_Path --
   -----------------------

   procedure On_Open_From_Path
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Dialog : Gtk_Dialog;
      Ent    : Gtkada_Entry;
      Label  : Gtk_Label;
      Button : Gtk_Widget;

   begin
      Gtk_New (Dialog,
               Title  => -"Open file from project",
               Parent => Get_Main_Window (Kernel),
               Flags  => Modal or Destroy_With_Parent);
      Set_Default_Size (Dialog, 300, 100);
      Set_Position (Dialog, Win_Pos_Mouse);

      Gtk_New (Label, -"Enter file name (use <tab> for completion):");
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      Gtk_New (Ent);
      Set_Width_Chars (Ent, 20);
      Set_Completions
        (Ent, Get_Source_Files
           (Get_Project_View (Kernel), Recursive => True, Full_Path => False));

      Pack_Start (Get_Vbox (Dialog), Ent, Fill => True, Expand => True);
      Grab_Focus (Ent);
      Set_Activates_Default (Ent, True);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Set_Default_Response (Dialog, Gtk_Response_OK);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Open_File_Editor (Kernel, Get_Text (Ent), From_Path => True);
      end if;

      Destroy (Dialog);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_From_Path;

   ---------------
   -- On_Reopen --
   ---------------

   procedure On_Reopen
     (Widget : access GObject_Record'Class;
      Params : GValues;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Params);
      Item  : constant Gtk_Menu_Item := Gtk_Menu_Item (Widget);
      Label : constant Gtk_Label := Gtk_Label (Get_Child (Item));
   begin
      Open_File_Editor (Kernel, Get_Text (Label));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Reopen;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : Source_Box;
   begin
      Editor := Open_File (Kernel, File => "");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_File;

   -------------
   -- On_Save --
   -------------

   procedure On_Save
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Success : Boolean;
   begin
      Save_To_File (Kernel, Success => Success);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save;

   ----------------
   -- On_Save_As --
   ----------------

   procedure On_Save_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Success : Boolean;
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      if Source /= null then
         declare
            New_Name : constant String := Select_File (-"Save File As");
         begin
            if New_Name = "" then
               return;
            else
               Save_To_File (Kernel, New_Name, Success);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_As;

   -----------------
   -- On_Save_All --
   -----------------

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Ignore : Boolean;

   begin
      Ignore := Save_All_MDI_Children (Kernel, Force => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_All;

   -------------------------
   -- On_Save_All_Editors --
   -------------------------

   procedure On_Save_All_Editors
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Ignore : Boolean;

   begin
      Ignore := Save_All_Editors (Kernel, Force => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_All_Editors;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         Cut_Clipboard (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Cut;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         Copy_Clipboard (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Copy;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         Paste_Clipboard (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Paste;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Source : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         Select_All (Source);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Select_All;

   -----------------
   -- On_New_View --
   -----------------

   procedure On_New_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      New_View (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_View;

   ------------------
   -- On_Goto_Line --
   ------------------

   procedure On_Goto_Line
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      if Editor = null then
         return;
      end if;

      declare
         Str : constant String := Simple_Entry_Dialog
           (Get_Main_Window (Kernel), -"Goto Line...", -"Enter line number:",
            Win_Pos_Mouse, "Goto_Line");

      begin
         if Str = "" or else Str (Str'First) = ASCII.NUL then
            return;
         end if;

         Set_Cursor_Location (Editor, Positive'Value (Str));

      exception
         when Constraint_Error =>
            Console.Insert (Kernel, -"Invalid line number: " & Str);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Line;

   -------------------------
   -- On_Goto_Declaration --
   -------------------------

   procedure On_Goto_Declaration
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return;
      end if;

      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Editor,
         Context => Entity_Selection_Context_Access
           (Default_Factory (Kernel, Editor)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Declaration;

   ------------------
   -- On_Goto_Body --
   ------------------

   procedure On_Goto_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return;
      end if;

      Goto_Declaration_Or_Body
        (Kernel, To_Body => True,
         Editor => Editor,
         Context => Entity_Selection_Context_Access
           (Default_Factory (Kernel, Editor)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Body;

   ----------------------
   -- Generate_Body_Cb --
   ----------------------

   procedure Generate_Body_Cb (Data : Process_Data) is
      function Body_Name (Name : String) return String;
      --  Return the name of the body corresponding to a spec file.

      function Body_Name (Name : String) return String is
      begin
         --  ??? Should ask the project module instead
         return Name (Name'First .. Name'Last - 1) & 'b';
      end Body_Name;

   begin
      Open_File_Editor (Data.Kernel, Body_Name (Data.Name.all));
   end Generate_Body_Cb;

   ----------------------
   -- On_Generate_Body --
   ----------------------

   procedure On_Generate_Body
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context = null
        or else not (Context.all in File_Selection_Context'Class)
      then
         Console.Insert
           (Kernel, -"No file selected, cannot generate body", Mode => Error);
         return;
      end if;

      declare
         File_Context : constant File_Selection_Context_Access :=
           File_Selection_Context_Access (Context);
         Filename     : constant String := File_Information (File_Context);
         File         : constant String :=
           Directory_Information (File_Context) & Filename;
         Success      : Boolean;
         Args         : Argument_List (1 .. 2);
         Lang         : String := Get_Language_From_File
           (Get_Language_Handler (Kernel), File);

      begin
         if File = "" then
            Console.Insert
              (Kernel, -"No file name, cannot generate body", Mode => Error);
            return;
         end if;

         Lower_Case (Lang);

         if Lang /= "ada" then
            Console.Insert
              (Kernel, -"Body generation of non Ada file not yet supported",
               Mode => Error);
            return;
         end if;

         if Save_All_MDI_Children (Kernel, Force => False) = False then
            return;
         end if;

         Args (1) := new String'(File);
         Args (2) := new String'(Dir_Name (File));
         Launch_Process
           (Kernel, "gnatstub", Args, Generate_Body_Cb'Access, File, Success);
         Free (Args);

         if Success then
            Print_Message
              (Glide_Window (Get_Main_Window (Kernel)).Statusbar,
               Help, -"Generating body...");
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Generate_Body;

   ----------------------
   -- Pretty_Print_Cb --
   ----------------------

   procedure Pretty_Print_Cb (Data : Process_Data) is
      function Pretty_Name (Name : String) return String;
      --  Return the name of the pretty printed file.

      function Pretty_Name (Name : String) return String is
      begin
         return Name & ".pp";
      end Pretty_Name;

   begin
      Open_File_Editor (Data.Kernel, Pretty_Name (Data.Name.all));
   end Pretty_Print_Cb;

   ---------------------
   -- On_Pretty_Print --
   ---------------------

   procedure On_Pretty_Print
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context = null
        or else not (Context.all in File_Selection_Context'Class)
      then
         Console.Insert
           (Kernel, -"No file selected, cannot pretty print", Mode => Error);
         return;
      end if;

      declare
         File_Context : constant File_Selection_Context_Access :=
           File_Selection_Context_Access (Context);
         Filename     : constant String := File_Information (File_Context);
         File         : constant String :=
           Directory_Information (File_Context) & Filename;
         Project      : constant String :=
           Get_Subproject_Name (Kernel, Filename);
         Success      : Boolean;
         Args, Vars   : Argument_List_Access;
         Lang         : String := Get_Language_From_File
           (Get_Language_Handler (Kernel), File);

      begin
         if File = "" then
            Console.Insert
              (Kernel, -"No file name, cannot pretty print", Mode => Error);
            return;
         end if;

         Lower_Case (Lang);

         if Lang /= "ada" then
            Console.Insert
              (Kernel, -"Pretty printing of non Ada file not yet supported",
               Mode => Error);
            return;
         end if;

         if Save_All_MDI_Children (Kernel, Force => False) = False then
            return;
         end if;

         if Project = "" then
            Args := new Argument_List'
              (new String'("pretty"), new String'(File));

         else
            Vars := Argument_String_To_List
              (Scenario_Variables_Cmd_Line (Kernel, GNAT_Syntax));
            Args := new Argument_List'
              ((1 => new String'("pretty"),
                2 => new String'("-P" & Project),
                3 => new String'(File)) & Vars.all);
            Unchecked_Free (Vars);
         end if;

         Launch_Process
           (Kernel, "gnat", Args.all, Pretty_Print_Cb'Access, File, Success);
         Free (Args);

         if Success then
            Print_Message
              (Glide_Window (Get_Main_Window (Kernel)).Statusbar,
               Help, -"Pretty printing...");
         end if;
      end;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Pretty_Print;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      pragma Unreferenced (Mode);

      Source : Source_Box;
      Edit   : Source_Editor_Box;
      MDI    : constant MDI_Window := Get_MDI (Kernel);
   begin
      if Mime_Type = Mime_Source_File then
         declare
            File       : constant String  := Get_String (Data (Data'First));
            Line       : constant Gint    := Get_Int (Data (Data'First + 1));
            Column     : constant Gint    := Get_Int (Data (Data'First + 2));
            Column_End : constant Gint    := Get_Int (Data (Data'First + 3));
            Highlight  : constant Boolean :=
              Get_Boolean (Data (Data'First + 4));
            New_File   : constant Boolean :=
              Get_Boolean (Data (Data'First + 6));
            The_Data   : Source_Editor_Module :=
              Source_Editor_Module (Src_Editor_Module_Id);
            Iter       : Child_Iterator := First_Child (MDI);
            Child      : MDI_Child;

         begin
            if Line = -1 then
               loop
                  Child := Get (Iter);

                  exit when Child = null;

                  if Get_Title (Child) = File then
                     Destroy (Source_Box (Get_Widget (Child)));
                  end if;

                  Next (Iter);
               end loop;

               return True;

            else
               if The_Data.Location_Open_Id /= 0 then
                  Idle_Remove (The_Data.Location_Open_Id);
               end if;

               Source := Open_File (Kernel, File, Create_New => New_File);

               if Source /= null then
                  Edit := Source.Editor;
               end if;

               if Edit /= null
                 and then (Line /= 0 or else Column /= 0)
               then
                  --  For some reason, we can not directly call
                  --  Set_Cursor_Location, since the source editor won't be
                  --  scrolled the first time the editor is displayed. Doing
                  --  this in an idle callback ensures that all the proper
                  --  events and initializations have taken place before we
                  --  try to scroll the editor.

                  Grab_Focus (Edit);

                  The_Data.Location_Open_Id := Location_Idle.Add
                    (Location_Callback'Access,
                       (Edit,
                        Natural (Line),
                        Natural (Column),
                        Natural (Column_End)),
                     Destroy => Location_Destroy'Access);

                  if Highlight then
                     Highlight_Line (Edit, Natural (Line));
                  end if;
               end if;

               return Edit /= null;
            end if;
         end;

      elsif Mime_Type = Mime_File_Line_Info then
         declare
            File  : constant String  := Get_String (Data (Data'First));
            Id    : constant String  := Get_String (Data (Data'First + 1));
            Info  : constant Line_Information_Data :=
              To_Line_Information (Get_Address (Data (Data'First + 2)));
            Stick_To_Data : constant Boolean :=
              Get_Boolean (Data (Data'First + 3));
            Every_Line : constant Boolean :=
              Get_Boolean (Data (Data'First + 4));
            Child : MDI_Child;
            Iter  : Child_Iterator := First_Child (MDI);

            procedure Apply_Mime_On_Child (Child : MDI_Child);
            --  Apply the mime information on Child.

            procedure Apply_Mime_On_Child (Child : MDI_Child) is
            begin
               if Info'First = 0 then
                  Create_Line_Information_Column
                    (Source_Box (Get_Widget (Child)).Editor,
                     Id,
                     Stick_To_Data,
                     Every_Line);

               elsif Info'Length = 0 then
                  Remove_Line_Information_Column
                    (Source_Box (Get_Widget (Child)).Editor, Id);

               else
                  Add_File_Information
                    (Source_Box (Get_Widget (Child)).Editor, Id, Info);
               end if;
            end Apply_Mime_On_Child;

         begin
            --  Look for the corresponding file editor.

            if File = "" then
               loop
                  Child := Get (Iter);
                  exit when Child = null;

                  declare
                     Title   : constant String := Get_Title (Child);
                     No_Name : constant String := -"Untitled";
                  begin
                     --  ??? Right now, we detect that a certain MDI child
                     --  is an editor by checking that the first character is
                     --  "/" or that the title is "Untitled[...]"

                     if Title (Title'First) = '/'
                       or else (Title'Length >= No_Name'Length
                                and then Title
                                  (Title'First
                                     .. Title'First + No_Name'Length - 1)
                                  = No_Name)
                     then
                        Apply_Mime_On_Child (Child);
                     end if;
                  end;

                  Next (Iter);
               end loop;

               return True;

            else
               declare
                  Base : constant String := Base_Name (File);
               begin
                  loop
                     Child := Get (Iter);

                     exit when Child = null
                       or else Get_Title (Child) = File
                       or else Get_Title (Child) = Base;
                     Next (Iter);
                  end loop;

                  if Child /= null then
                     --  The editor was found.
                     Apply_Mime_On_Child (Child);

                     return True;
                  end if;
               end;
            end if;
         end;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Mime_Action;

   ------------------
   -- On_Edit_File --
   ------------------

   procedure On_Edit_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      Trace (Me, "On_Edit_File: " & File_Information (File));
      Open_File_Editor
        (Get_Kernel (Context),
         Directory_Information (File) & File_Information (File));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Edit_File;

   ------------------------------
   -- Source_Editor_Contextual --
   ------------------------------

   procedure Source_Editor_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      File  : File_Selection_Context_Access;
      Mitem : Gtk_Menu_Item;

   begin
      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File)
           and then Has_File_Information (File)
         then
            Gtk_New (Mitem, -"Edit " & Base_Name (File_Information (File)));
            Append (Menu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (On_Edit_File'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Source_Editor_Contextual;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Source_Editor_Box_Record'Class)
      return Selection_Context_Access is
   begin
      return Get_Contextual_Menu (Kernel, Editor, null, null);
   end Default_Factory;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      C : constant Source_Box := Source_Box (Child);
   begin
      return Default_Factory (Kernel, C.Editor);
   end Default_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File             : constant String := '/' & (-"File") & '/';
      Save             : constant String := File & (-"Save...") & '/';
      Edit             : constant String := '/' & (-"Edit") & '/';
      Navigate         : constant String := '/' & (-"Navigate") & '/';
      Mitem            : Gtk_Menu_Item;
      Button           : Gtk_Button;
      Toolbar          : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Undo_Redo        : Undo_Redo_Information;
      Reopen_File_Name : constant String :=
        Format_Pathname (Get_Home_Dir (Kernel) & "/recent_files");
      Reopen_File      : File_Type;
      Buffer           : String (1 .. 1024);
      Last             : Integer := 1;

   begin
      Src_Editor_Module_Id := new Source_Editor_Module_Record;
      Register_Module
        (Module                  => Src_Editor_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => Src_Editor_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Source_Editor_Contextual'Access,
         Mime_Handler            => Mime_Action'Access,
         MDI_Child_Tag           => Source_Box_Record'Tag,
         Default_Context_Factory => Default_Factory'Access,
         Save_Function           => Save_Function'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      --  Menus

      Register_Menu (Kernel, File, -"Open...",  Stock_Open,
                     On_Open_File'Access, null,
                     GDK_F3, Ref_Item => -"Save...");
      Register_Menu (Kernel, File, -"Open From Project...",  Stock_Open,
                     On_Open_From_Path'Access, null,
                     GDK_F3, Shift_Mask, Ref_Item => -"Save...");

      Source_Editor_Module (Src_Editor_Module_Id).Reopen_Menu_Item :=
        Register_Menu (Kernel, File, -"Reopen", "", null,
                       Ref_Item   => -"Open From Project...",
                       Add_Before => False);

      if not Is_Regular_File (Reopen_File_Name) then
         declare
            File : File_Descriptor;
         begin
            File := Create_New_File (Reopen_File_Name, Text);
            Close (File);
         end;
      end if;

      Open (Reopen_File, In_File, Reopen_File_Name);

      while Last >= 0
        and then not End_Of_File (Reopen_File)
      loop
         Get_Line (Reopen_File, Buffer, Last);

         String_List_Utils.String_List.Append
           (Source_Editor_Module (Src_Editor_Module_Id).List,
            Buffer (1 .. Last));
      end loop;

      Close (Reopen_File);
      Refresh_Reopen_Menu (Kernel);

      Register_Menu (Kernel, File, -"New", Stock_New, On_New_File'Access,
                     Ref_Item => -"Open...");
      Register_Menu (Kernel, File, -"New View", "", On_New_View'Access,
                     Ref_Item => -"Open...");

      Register_Menu (Kernel, File, -"Save", Stock_Save,
                     On_Save'Access, null,
                     GDK_S, Control_Mask, Ref_Item => -"Save...");
      Register_Menu (Kernel, Save, -"File As...", Stock_Save_As,
                     On_Save_As'Access, Ref_Item => -"Desktop");
      Register_Menu (Kernel, Save, -"All Editors", "",
                     On_Save_All_Editors'Access, Sensitive => False,
                     Ref_Item => -"Desktop");
      Register_Menu (Kernel, Save, -"All", "",
                     On_Save_All'Access, Ref_Item => -"Desktop");

      --  Note: callbacks for the Undo/Redo menu items will be added later
      --  by each source editor.

      Undo_Redo.Undo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"Undo", Stock_Undo,
                       null, null,
                       GDK_Z, Control_Mask, Ref_Item => -"Preferences",
                       Sensitive => False);
      Undo_Redo.Redo_Menu_Item :=
        Register_Menu (Kernel, Edit, -"Redo", Stock_Redo,
                       null, null,
                       GDK_R, Control_Mask, Ref_Item => -"Preferences",
                       Sensitive => False);

      Gtk_New (Mitem);
      Register_Menu
        (Kernel, Edit, Mitem, Ref_Item => "Redo", Add_Before => False);

      Register_Menu (Kernel, Edit, -"Cut",  Stock_Cut,
                     On_Cut'Access, null,
                     GDK_Delete, Shift_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Copy",  Stock_Copy,
                     On_Copy'Access, null,
                     GDK_Insert, Control_Mask,
                     Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Paste",  Stock_Paste,
                     On_Paste'Access, null,
                     GDK_Insert, Shift_Mask,
                     Ref_Item => -"Preferences");

      --  ??? This should be bound to Ctrl-A, except this would interfer with
      --  Emacs keybindings for people who want to use them.
      Register_Menu (Kernel, Edit, -"Select All",  "",
                     On_Select_All'Access, Ref_Item => -"Preferences");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Edit, Mitem, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Edit, -"Generate Body", "",
                     On_Generate_Body'Access, Ref_Item => -"Preferences");
      Register_Menu (Kernel, Edit, -"Pretty Print", "",
                     On_Pretty_Print'Access, Ref_Item => -"Preferences");

      Register_Menu (Kernel, Navigate, -"Goto Line...", Stock_Jump_To,
                     On_Goto_Line'Access, null,
                     GDK_G, Control_Mask,
                     Ref_Item => -"Goto File Spec<->Body");
      Register_Menu (Kernel, Navigate, -"Goto Declaration", Stock_Home,
                     On_Goto_Declaration'Access, Ref_Item => -"Goto Line...");
      Register_Menu (Kernel, Navigate, -"Goto Body", "",
                     On_Goto_Body'Access, Ref_Item => -"Goto Line...");

      --  Toolbar buttons

      Button := Insert_Stock
        (Toolbar, Stock_New, -"Create a New File", Position => 0);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_New_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Open, -"Open a File", Position => 1);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Open_File'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Save, -"Save Current File", Position => 2);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Save'Access),
         Kernel_Handle (Kernel));

      Insert_Space (Toolbar, Position => 3);
      Undo_Redo.Undo_Button := Insert_Stock
        (Toolbar, Stock_Undo, -"Undo Previous Action", Position => 4);
      Set_Sensitive (Undo_Redo.Undo_Button, False);
      Undo_Redo.Redo_Button := Insert_Stock
        (Toolbar, Stock_Redo, -"Redo Previous Action", Position => 5);
      Set_Sensitive (Undo_Redo.Redo_Button, False);

      Insert_Space (Toolbar, Position => 6);
      Button := Insert_Stock
        (Toolbar, Stock_Cut, -"Cut to Clipboard", Position => 7);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Cut'Access),
         Kernel_Handle (Kernel));
      Button := Insert_Stock
        (Toolbar, Stock_Copy, -"Copy to Clipboard", Position => 8);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Copy'Access),
         Kernel_Handle (Kernel));
      Button := Insert_Stock
        (Toolbar, Stock_Paste, -"Paste from Clipboard", Position => 9);
      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Paste'Access),
         Kernel_Handle (Kernel));

      Undo_Redo_Data.Set (Kernel, Undo_Redo, Undo_Redo_Id);

      Preferences_Changed (Kernel, Kernel_Handle (Kernel));

      Kernel_Callback.Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         User_Data   => Kernel_Handle (Kernel));

      Register_Command
        (Kernel,
         Command => "edit",
         Help    => -"Usage:" & ASCII.LF
         & "  edit [-l line] [-c column] file_name" & ASCII.LF
         & (-"Open a file editor for file_name."),
         Handler => Edit_Command_Handler'Access);

   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);
      Id                        : Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Pref_Display_Line_Numbers : constant Boolean :=
        Get_Pref (Kernel, Display_Line_Numbers);

   begin
      if Pref_Display_Line_Numbers = Id.Display_Line_Numbers then
         return;
      end if;

      Id.Display_Line_Numbers := Pref_Display_Line_Numbers;

      --  Connect necessary signal to display line numbers.
      if Pref_Display_Line_Numbers then
         if Id.Source_Lines_Revealed_Id = No_Handler then
            Id.Source_Lines_Revealed_Id :=
              Kernel_Callback.Connect
                (Kernel,
                 Source_Lines_Revealed_Signal,
                 On_Lines_Revealed'Access,
                 Kernel);
            Id.File_Edited_Id :=
              Kernel_Callback.Connect
                (Kernel,
                 File_Edited_Signal,
                 File_Edited_Cb'Access,
                 Kernel);

            Create_Line_Information_Column
              (Kernel,
               "",
               Src_Editor_Module_Name,
               Stick_To_Data => False,
               Every_Line    => True);
         end if;

      elsif Id.Source_Lines_Revealed_Id /= No_Handler then
         Gtk.Handlers.Disconnect
           (Kernel, Id.Source_Lines_Revealed_Id);
         Gtk.Handlers.Disconnect
           (Kernel, Id.File_Edited_Id);
         Id.Source_Lines_Revealed_Id := No_Handler;
         Id.File_Edited_Id := No_Handler;

         Remove_Line_Information_Column (Kernel, "", Src_Editor_Module_Name);
      end if;
   end Preferences_Changed;

   -------------------------
   -- Refresh_Reopen_Menu --
   -------------------------

   procedure Refresh_Reopen_Menu
     (Kernel : access Kernel_Handle_Record'Class)
   is
      use String_List_Utils.String_List;

      The_Data    : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Node        : List_Node;
      Reopen_Menu : Gtk_Menu;
      Mitem       : Gtk_Menu_Item;
   begin
      if Get_Submenu (The_Data.Reopen_Menu_Item) /= null then
         Remove_Submenu (The_Data.Reopen_Menu_Item);
      end if;

      Gtk_New (Reopen_Menu);
      Set_Submenu (The_Data.Reopen_Menu_Item, Gtk_Widget (Reopen_Menu));

      Node := First (The_Data.List);

      while Node /= Null_Node loop
         Gtk_New (Mitem, Data (Node));
         Append (Reopen_Menu, Mitem);

         Kernel_Callback.Connect
           (Mitem,
            "activate",
            On_Reopen'Access,
            Kernel_Handle (Kernel));

         Node := Next (Node);
      end loop;

      Show_All (Reopen_Menu);
   end Refresh_Reopen_Menu;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Source_Editor_Module_Record) is
   begin
      String_List_Utils.String_List.Free (Id.List);
   end Destroy;

end Src_Editor_Module;
