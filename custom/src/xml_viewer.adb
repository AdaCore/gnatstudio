-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gdk.Event;                 use Gdk.Event;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Tree_View;          use Gtkada.Tree_View;

with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;

with Custom_Module;             use Custom_Module;
with GUI_Utils;                 use GUI_Utils;
with Traces;                    use Traces;
with XML_Parsers;               use XML_Parsers;
with VFS;                       use VFS;
with GNAT.Strings;

package body XML_Viewer is

   Filename_Cst  : aliased constant String := "filename";
   XML_Constructor_Params : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access);

   type XML_Viewer_Record is new Gtk_Vbox_Record with record
      Name  : GNAT.Strings.String_Access;
      Child : GPS_MDI_Child;
      Tree  : Tree_View;
   end record;
   type XML_Viewer is access all XML_Viewer_Record'Class;

   ---------------
   -- Constants --
   ---------------

   Name_Column    : constant := 0;
   Value_Column   : constant := 1;
   Command_Column : constant := 2;
   Sort_Column    : constant := 3;

   -----------------------
   -- Local subprograms --
   -----------------------

   function On_Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called on a click on the tree view

   procedure XML_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands for custom XML viewers.

   function Create_XML_Viewer
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return XML_Viewer;
   --  Create a new XML Viewer.

   function Parse_Metrix (View : XML_Viewer; File : String) return String;
   --  Parse a metrix file and add the contents to View.
   --  Return an error message from the XML parser, if any.

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View : constant XML_Viewer := XML_Viewer (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Iter := Find_Iter_For_Event (View.Tree, View.Tree.Model, Event);

         if Iter /= Null_Iter then
            declare
               Cmd : constant String := Get_String
                 (View.Tree.Model, Iter, Command_Column);
            begin
               if Cmd /= "" then
                  Execute_GPS_Shell_Command
                    (Get_Kernel (Custom_Module_ID.all), Cmd);
                  return True;
               end if;
            end;
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Button_Press;

   ------------------
   -- Parse_Metrix --
   ------------------

   function Parse_Metrix
     (View : XML_Viewer;
      File : String) return String
   is
      Error        : GNAT.Strings.String_Access;
      Root         : Node_Ptr;
      Path         : Gtk_Tree_Path;
      Dummy        : Boolean;
      Col          : Gint;
      Metric_Count : Natural := 1;
      pragma Unreferenced (Dummy);

      function Right_Align return String;
      pragma Inline (Right_Align);
      --  Returns the right-aligned Metric_Count for sorting purposes

      procedure Parse_Node
        (N      : Node_Ptr;
         Parent : Gtk_Tree_Iter;
         File   : String);
      --  Add a metrix node to the tree

      -----------------
      -- Right_Align --
      -----------------

      function Right_Align return String is
         Img : constant String := Metric_Count'Img;
      begin
         return (1 .. 7 - Img'Length => ' ') & Img;
      end Right_Align;

      ----------------
      -- Parse_Node --
      ----------------

      procedure Parse_Node
        (N      : Node_Ptr;
         Parent : Gtk_Tree_Iter;
         File   : String)
      is
         Name : constant UTF8_String := Get_Attribute (N, "name");
         C    : Node_Ptr;
         I    : Gtk_Tree_Iter;
      begin
         if N.Tag.all = "file" then
            Append (View.Tree.Model, I, Parent);
            Set (View.Tree.Model, I, Name_Column,
                 "<b>" & Base_Name (Name) & "</b>");
            Set (View.Tree.Model, I, Sort_Column, '~' & Base_Name (Name));
            Set (View.Tree.Model, I, Command_Column,
                 "Editor.edit """"""" & Name & """""""");
            Metric_Count := 1; --  Reset the count on new file

         elsif N.Tag.all = "unit" then
            Append (View.Tree.Model, I, Parent);

            declare
               Kind : constant UTF8_String := Get_Attribute (N, "kind");
            begin
               if Kind = "" then
                  Set (View.Tree.Model, I, Name_Column,
                       "<b>" & Name & "</b>");
               else
                  Set (View.Tree.Model, I, Name_Column,
                       "<b>" & Name & "</b> (" & Kind & ")");
               end if;
            end;

            Set (View.Tree.Model, I, Sort_Column, '~' & Name);
            Set (View.Tree.Model, I, Command_Column,
                 "Editor.edit """""""
                 & File & """"""" "
                 & Get_Attribute (N, "line") & " "
                 & Get_Attribute (N, "col"));

         elsif  N.Tag.all = "metric" then
            Append (View.Tree.Model, I, Parent);
            Set (View.Tree.Model, I, Name_Column, Name);
            Set (View.Tree.Model, I, Sort_Column, Right_Align);
            Set (View.Tree.Model, I, Value_Column, N.Value.all);
            Metric_Count := Metric_Count + 1;
         else
            I := Parent;
         end if;

         C := N.Child;

         while C /= null loop
            if N.Tag.all = "file" then
               Parse_Node (C, I, Name);
            else
               Parse_Node (C, I, File);
            end if;

            C := C.Next;
         end loop;
      end Parse_Node;

      use type GNAT.Strings.String_Access;
   begin
      --  ??? Add a dirty kludge here to support versions of gnatmetrix prior
      --  to 5.03a1, which didn't output correct XML.

      declare
         V : constant Virtual_File := Create (File);
         S : GNAT.Strings.String_Access := Read_File (V);
         W : Writable_File;

         use type GNAT.Strings.String_Access;
      begin
         if S /= null
           and then S.all'Length > 2
           and then S (S'First .. S'First + 1) /= "<?"
         then
            --  The beginning tag is missing, add it.

            W := Write_File (V);
            Write (W, "<?xml version=""1.0""?>" & ASCII.LF & S.all);
            Close (W);
         end if;

         GNAT.Strings.Free (S);
      end;

      Parse (File, Root, Error);

      if Error /= null then
         declare
            Message : constant String := Error.all;
         begin
            Free (Root);
            GNAT.Strings.Free (Error);
            return Message;
         end;
      end if;

      Col := Freeze_Sort (View.Tree.Model);

      while Root /= null loop
         Parse_Node (Root, Null_Iter, "");
         Root := Root.Next;
      end loop;

      Thaw_Sort (View.Tree.Model, Col);

      Columns_Autosize (View.Tree);

      --  Expand the first iter

      Path := Get_Path (View.Tree.Model, Get_Iter_First (View.Tree.Model));
      Dummy := Expand_Row (View.Tree, Path, True);
      Path_Free (Path);

      return "";
   end Parse_Metrix;

   -----------------------
   -- Create_XML_Viewer --
   -----------------------

   function Create_XML_Viewer
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return XML_Viewer
   is
      View   : XML_Viewer;
      Col    : Gtk_Tree_View_Column;
      Rend   : Gtk_Cell_Renderer_Text;
      Dumm   : Gint;
      Scroll : Gtk_Scrolled_Window;
      pragma Unreferenced (Dumm);
   begin
      View := new XML_Viewer_Record;

      Initialize_Hbox (View);

      View.Name := new String'(Name);

      Gtk_New (Scroll);

      Gtk_New
        (View.Tree,
         (Name_Column    => GType_String,
          Value_Column   => GType_String,
          Command_Column => GType_String,
          Sort_Column    => GType_String));

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree, "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (On_Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Add (Scroll, View.Tree);

      Set_Headers_Visible (View.Tree, False);
      Set_Rules_Hint (View.Tree, True);

      Pack_Start (View, Scroll);

      Gtk_New (View.Child, View, Group => Group_View,
               Module => Custom_Module_ID);
      Put (Get_MDI (Kernel), View.Child,
           Initial_Position => Position_Left);

      --  Create the columns

      Gtk_New (Col);
      Gtk_New (Rend);
      Pack_Start (Col, Rend, False);
      Add_Attribute (Col, Rend, "markup", Name_Column);
      Set_Sort_Column_Id (Col, Sort_Column);
      Dumm := Append_Column (View.Tree, Col);
      Clicked (Col);

      Gtk_New (Col);
      Gtk_New (Rend);
      Pack_Start (Col, Rend, False);
      Add_Attribute (Col, Rend, "text", Value_Column);
      Dumm := Append_Column (View.Tree, Col);

      Set_Title (View.Child, Name);

      Raise_Child (View.Child);

      return View;
   end Create_XML_Viewer;

   --------------------------
   -- XML_Commands_Handler --
   --------------------------

   procedure XML_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel           : constant Kernel_Handle := Get_Kernel (Data);
      XML_Viewer_Class : constant Class_Type :=
                           New_Class (Kernel, "XMLViewer");
      Inst             : Class_Instance;
      View             : XML_Viewer;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, XML_Constructor_Params);
         Inst := Nth_Arg (Data, 1, XML_Viewer_Class);
         View := Create_XML_Viewer (Kernel, Nth_Arg (Data, 2));
         Set_Data (Inst, Widget => GObject (View));

      elsif Command = "parse" then
         Inst := Nth_Arg (Data, 1, XML_Viewer_Class);
         View := XML_Viewer (GObject'(Get_Data (Inst)));
         Set_Error_Msg (Data, Parse_Metrix (View, Nth_Arg (Data, 2)));

      end if;
   end XML_Commands_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      XML_Viewer_Class : constant Class_Type := New_Class
        (Kernel, "XMLViewer");

   begin
      Register_Command
        (Kernel, Constructor_Method,
         Class        => XML_Viewer_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => XML_Commands_Handler'Access);

      Register_Command
        (Kernel, "parse",
         Class        => XML_Viewer_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => XML_Commands_Handler'Access);
   end Register_Commands;

end XML_Viewer;
