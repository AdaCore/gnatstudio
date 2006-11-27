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
with Ada.Unchecked_Conversion;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Strings;
with System;

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

with Basic_Types;
with Custom_Module;             use Custom_Module;
with GUI_Utils;                 use GUI_Utils;
with Traces;                    use Traces;
with XML_Parsers;               use XML_Parsers;
with VFS;                       use VFS;

package body XML_Viewer is

   Name_Cst      : aliased constant String := "name";
   Filename_Cst  : aliased constant String := "filename";
   Str_Cst       : aliased constant String := "str";
   Columns_Cst   : aliased constant String := "columns";
   Parser_Cst    : aliased constant String := "parser";
   On_Click_Cst  : aliased constant String := "on_click";
   On_Select_Cst : aliased constant String := "on_select";
   Sorted_Cst    : aliased constant String := "sorted";

   XML_Constructor_Params : constant Cst_Argument_List  :=
                              (1 => Name_Cst'Access,
                               2 => Columns_Cst'Access,
                               3 => Parser_Cst'Access,
                               4 => On_Click_Cst'Access,
                               5 => On_Select_Cst'Access,
                               6 => Sorted_Cst'Access);
   XML_Metrics_Params : constant Cst_Argument_List :=
                          (1 => Name_Cst'Access);
   Parse_Params : constant Cst_Argument_List :=
                     (1 => Filename_Cst'Access);
   Parse_String_Params : constant Cst_Argument_List :=
                           (1 => Str_Cst'Access);

   type XML_Viewer_Record is abstract new Abstract_XML_Viewer_Record with
      record
         Child          : GPS_MDI_Child;
         Tree           : Tree_View;
         Sorted         : Boolean := False;
         Sort_Column    : Gint;
         Command_Column : Gint;
         XML_Column     : Gint;
         XML            : Node_Ptr;
      end record;
   type XML_Viewer is access all XML_Viewer_Record'Class;

   type Metrix_XML_Viewer_Record is new XML_Viewer_Record with record
      File : GNAT.Strings.String_Access;
   end record;
   function Node_Parser
     (View        : access Metrix_XML_Viewer_Record;
      Parent      : Gtk_Tree_Iter;
      Node        : Node_Ptr;
      Child_Index : Positive) return Gtk_Tree_Iter;
   procedure Free (View : access Metrix_XML_Viewer_Record);
   function On_Click
     (View         : access Metrix_XML_Viewer_Record;
      Double_Click : Boolean;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node         : Glib.Xml_Int.Node_Ptr) return Boolean;
   --  See inherited documentation

   type Custom_XML_Viewer_Record is new XML_Viewer_Record with record
      Parser    : Subprogram_Type;
      On_Click  : Subprogram_Type;
      On_Select : Subprogram_Type;
      Columns   : Integer;
   end record;
   function Node_Parser
     (View        : access Custom_XML_Viewer_Record;
      Parent      : Gtk_Tree_Iter;
      Node        : Node_Ptr;
      Child_Index : Positive) return Gtk_Tree_Iter;
   procedure Free (View : access Custom_XML_Viewer_Record);
   function On_Click
     (View         : access Custom_XML_Viewer_Record;
      Double_Click : Boolean;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node         : Glib.Xml_Int.Node_Ptr) return Boolean;
   --  See inherited documentation

   function Create_Callback_Data
     (Sub  : Subprogram_Type;
      Node : Node_Ptr) return Callback_Data'Class;
   --  Create the callback data for subprograms called on this row.
   --  Returned value must be freed by the caller

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

   procedure Initialize_XML_Viewer
     (View    : access XML_Viewer_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      Name    : String;
      Columns : Natural);
   --  Create a new XML Viewer.

   function Parse_XML
     (View         : access XML_Viewer_Record'Class;
      Buffer       : String;
      Is_File_Name : Boolean := True) return String;
   --  Parse an XML file (if Is_File_Name is true), or an XML string, and for
   --  each calls the appropriate node parser to create the rows of the XML
   --  view.
   --  Return an error message from the XML parser, if any.

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Called when View is being destroyed

   function Set_Row_Content
     (View     : access XML_Viewer_Record'Class;
      Parent   : Gtk_Tree_Iter;
      Col0     : String;
      Sort_On  : String  := "";
      Col1     : String  := "";
      On_Click : String  := "") return Gtk_Tree_Iter;
   --  Add a new row to the model.
   --  Name and Value are displayed in the visible columns.
   --  Sort_On, if specified, is the sorting string used to sort columns. If
   --  unspecified, it defaults to Name.
   --  On_Click is a GPS shell command to execute when the line is clicked on.

   ----------
   -- Free --
   ----------

   procedure Free (View : access Custom_XML_Viewer_Record) is
   begin
      Free (View.Parser);
      Free (View.On_Click);
      Free (View.On_Select);
   end Free;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   function Create_Callback_Data
     (Sub  : Subprogram_Type;
      Node : Node_Ptr) return Callback_Data'Class
   is
      Script : constant Scripting_Language := Get_Script (Sub.all);
      C      : Callback_Data'Class := Create (Script, 3);
   begin
      Set_Nth_Arg (C, 1, Node.Tag.all);

      if Node.Attributes = null then
         Set_Nth_Arg (C, 2, "");
      else
         Set_Nth_Arg (C, 2, Node.Attributes.all);
      end if;

      if Node.Value = null then
         Set_Nth_Arg (C, 3, "");
      else
         Set_Nth_Arg (C, 3, Node.Value.all);
      end if;
      return C;
   end Create_Callback_Data;

   -----------------
   -- Node_Parser --
   -----------------

   function Node_Parser
     (View        : access Custom_XML_Viewer_Record;
      Parent      : Gtk_Tree_Iter;
      Node        : Node_Ptr;
      Child_Index : Positive) return Gtk_Tree_Iter
   is
      pragma Unreferenced (Child_Index);
      Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      if View.Parser /= null then
         declare
            C : Callback_Data'Class :=
              Create_Callback_Data (View.Parser, Node);
            Tmp : GNAT.Strings.String_List := Execute (View.Parser, C);
         begin
            if Tmp'Length /= 0 then
               Append (View.Tree.Model, Iter, Parent);

               for S in Tmp'Range loop
                  Set (View.Tree.Model, Iter, Gint (S - Tmp'First),
                       Tmp (S).all);
               end loop;

               Set (View.Tree.Model, Iter, View.Sort_Column,
                    Tmp (Tmp'First).all);
            end if;

            Basic_Types.Free (Tmp);
            Free (C);
         end;

      else
         Append (View.Tree.Model, Iter, Parent);
         Set (View.Tree.Model, Iter, 0, Node.Tag.all);
         if View.Columns >= 2 and then Node.Attributes /= null then
            Set (View.Tree.Model, Iter, 1, Node.Attributes.all);
         end if;
         if View.Columns >= 3 and then Node.Value /= null then
            Set (View.Tree.Model, Iter, 2, Node.Value.all);
         end if;
      end if;

      return Iter;

   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Information (E));
         return Null_Iter;
   end Node_Parser;

   --------------
   -- On_Click --
   --------------

   function On_Click
     (View         : access Custom_XML_Viewer_Record;
      Double_Click : Boolean;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node         : Glib.Xml_Int.Node_Ptr) return Boolean
   is
      Tmp : Boolean;
      pragma Unreferenced (Iter, Tmp);
   begin
      if Double_Click then
         if View.On_Click /= null then
            declare
               C : Callback_Data'Class :=
                 Create_Callback_Data (View.On_Click, Node);
            begin
               Tmp := Execute (View.On_Click, C);
               Free (C);
               return True;
            end;
         end if;

      else
         if View.On_Select /= null then
            declare
               C : Callback_Data'Class :=
                 Create_Callback_Data (View.On_Select, Node);
            begin
               Tmp := Execute (View.On_Select, C);
               Free (C);
               return False;
            end;
         end if;
      end if;
      return False;
   end On_Click;

   --------------
   -- On_Click --
   --------------

   function On_Click
     (View         : access Metrix_XML_Viewer_Record;
      Double_Click : Boolean;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node         : Glib.Xml_Int.Node_Ptr) return Boolean
   is
      pragma Unreferenced (Node);
      Cmd : constant String :=
              Get_String (View.Tree.Model, Iter, View.Command_Column);
   begin
      if Double_Click then
         if Cmd /= "" then
            Execute_GPS_Shell_Command (Get_Kernel (Custom_Module_ID.all), Cmd);
            return True;
         end if;
      end if;
      return False;
   end On_Click;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Node_Ptr);
      View : constant XML_Viewer := XML_Viewer (Widget);
      Iter : Gtk_Tree_Iter;
      N    : Node_Ptr;
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press
        or else Get_Event_Type (Event) = Button_Press
      then
         Iter := Find_Iter_For_Event (View.Tree, View.Tree.Model, Event);
         if Iter /= Null_Iter then
            N :=
              Convert (Get_Address (View.Tree.Model, Iter, View.XML_Column));
            return On_Click
              (View         => View,
               Double_Click => Get_Event_Type (Event) = Gdk_2button_Press,
               Iter         => Iter,
               Node         => N);
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Button_Press;

   ---------------------
   -- Set_Row_Content --
   ---------------------

   function Set_Row_Content
     (View      : access XML_Viewer_Record'Class;
      Parent    : Gtk_Tree_Iter;
      Col0      : String;
      Sort_On   : String  := "";
      Col1      : String  := "";
      On_Click  : String  := "") return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter;
   begin
      Append (View.Tree.Model, Iter, Parent);
      Set (View.Tree.Model, Iter, 0, Col0);

      if Col1 /= "" then
         Set (View.Tree.Model, Iter, 1, Col1);
      end if;

      if Sort_On /= "" then
         Set (View.Tree.Model, Iter, View.Sort_Column, Sort_On);
      else
         Set (View.Tree.Model, Iter, View.Sort_Column, Col0);
      end if;

      if On_Click /= "" then
         Set (View.Tree.Model, Iter, View.Command_Column, On_Click);
      end if;

      return Iter;
   end Set_Row_Content;

   ----------
   -- Free --
   ----------

   procedure Free (View : access Metrix_XML_Viewer_Record) is
   begin
      GNAT.Strings.Free (View.File);
   end Free;

   -----------------
   -- Node_Parser --
   -----------------

   function Node_Parser
     (View        : access Metrix_XML_Viewer_Record;
      Parent      : Gtk_Tree_Iter;
      Node        : Node_Ptr;
      Child_Index : Positive) return Gtk_Tree_Iter
   is
      function Get_Kind return String;
      --  Return the kind of the unit

      function Right_Align return String;
      pragma Inline (Right_Align);
      --  Returns the right-aligned Metric_Count for sorting purposes

      --------------
      -- Get_Kind --
      --------------

      function Get_Kind return String is
         Kind : constant UTF8_String := Get_Attribute (Node, "kind");
      begin
         if Kind = "" then
            return "";
         else
            return " (" & Kind & ")";
         end if;
      end Get_Kind;

      -----------------
      -- Right_Align --
      -----------------

      function Right_Align return String is
         Img : constant String := Child_Index'Img;
      begin
         return (1 .. 7 - Img'Length => ' ') & Img;
      end Right_Align;

      Name : constant UTF8_String := Get_Attribute (Node, "name");
   begin
      if Node.Tag.all = "file" then
         GNAT.Strings.Free (View.File);
         View.File := new String'(Get_Attribute (Node, "name"));
         return Set_Row_Content
           (View, Parent,
            Col0      => "<b>" & Base_Name (Name) & "</b>",
            Sort_On   => Base_Name (Name),
            On_Click  => "Editor.edit """"""" & Name & """""""");

      elsif Node.Tag.all = "unit" then
         return Set_Row_Content
           (View, Parent,
            Col0      => "<b>" & Name & "</b>" & Get_Kind,
            On_Click  =>
              "Editor.edit """""""
            & View.File.all & """"""" "
            & Get_Attribute (Node, "line") & " "
            & Get_Attribute (Node, "col"));

      elsif Node.Tag.all = "metric" then
         return Set_Row_Content
           (View, Parent,
            Col0    => Name,
            Sort_On => Right_Align,
            Col1    => Node.Value.all);
      else
         return Null_Iter;
      end if;
   end Node_Parser;

   ---------------
   -- Parse_XML --
   ---------------

   function Parse_XML
     (View         : access XML_Viewer_Record'Class;
      Buffer       : String;
      Is_File_Name : Boolean := True) return String
   is
      Error : GNAT.Strings.String_Access;
      Root  : Node_Ptr;
      Path  : Gtk_Tree_Path;
      Col   : Gint;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

      procedure Parse_Node
        (N           : Node_Ptr;
         Parent      : Gtk_Tree_Iter;
         Child_Index : Natural);
      --  Add a metrix node to the tree

      ----------------
      -- Parse_Node --
      ----------------

      procedure Parse_Node
        (N           : Node_Ptr;
         Parent      : Gtk_Tree_Iter;
         Child_Index : Natural)
      is
         C     : Node_Ptr;
         Iter  : Gtk_Tree_Iter;
         Index : Natural := 1;
      begin
         Iter := Node_Parser
           (View        => View,
            Parent      => Parent,
            Node        => N,
            Child_Index => Child_Index);

         if Iter /= Null_Iter then
            --  This is valid because N and its tree belongs to View, and are
            --  not freed before View is freed
            Set (View.Tree.Model, Iter, View.XML_Column, N.all'Address);

            C := N.Child;
            while C /= null loop
               Parse_Node (C, Iter, Index);
               Index := Index + 1;
               C     := C.Next;
            end loop;
         end if;
      end Parse_Node;

      use type GNAT.Strings.String_Access;
   begin
      --  ??? Add a dirty kludge here to support versions of gnatmetrix prior
      --  to 5.03a1, which didn't output correct XML.

      if Is_File_Name then
         declare
            V : constant Virtual_File := Create (Buffer);
            S : GNAT.Strings.String_Access := Read_File (V);
            W : Writable_File;

            use type GNAT.Strings.String_Access;
         begin
            if S /= null
              and then S.all'Length > 2
              and then S (S'First .. S'First + 1) /= "<?"
            then
               --  The beginning tag is missing, add it

               W := Write_File (V);
               Write (W, "<?xml version=""1.0""?>" & ASCII.LF & S.all);
               Close (W);
            end if;

            GNAT.Strings.Free (S);
         end;

         Parse (Buffer, Root, Error);
      else
         Parse_Buffer (Buffer, Tree => Root, Error => Error);
      end if;

      if Error /= null then
         declare
            Message : constant String := Error.all;
         begin
            Free (Root);
            GNAT.Strings.Free (Error);
            return Message;
         end;
      end if;

      Clear (View.Tree.Model);
      Col := Freeze_Sort (View.Tree.Model);

      if Root /= null then
         Root := Root.Child;
      end if;

      while Root /= null loop
         Parse_Node (Root, Null_Iter, 1);
         Root := Root.Next;
      end loop;
      Thaw_Sort (View.Tree.Model, Col);

      Columns_Autosize (View.Tree);
      View.XML := Root;

      --  Expand the first iter

      Path := Get_Path (View.Tree.Model, Get_Iter_First (View.Tree.Model));
      Dummy := Expand_Row (View.Tree, Path, True);
      Path_Free (Path);

      return "";
   end Parse_XML;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (View : access Gtk_Widget_Record'Class) is
   begin
      Free (XML_Viewer (View));
      Free (XML_Viewer (View).XML);
   end On_Destroy;

   ---------------------------
   -- Initialize_XML_Viewer --
   ---------------------------

   procedure Initialize_XML_Viewer
     (View    : access XML_Viewer_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      Name    : String;
      Columns : Natural)
   is
      Col          : Gtk_Tree_View_Column;
      Rend         : Gtk_Cell_Renderer_Text;
      Dumm         : Gint;
      Scroll       : Gtk_Scrolled_Window;
      pragma Unreferenced (Dumm);
      Column_Types : Glib.GType_Array (1 .. Guint (Columns) + 3) :=
                       (others => GType_String);
   begin
      Column_Types (Guint (Columns) + 3) := GType_Pointer;

      Initialize_Hbox (View);

      Gtk_New (Scroll);

      Gtk_New (View.Tree, Column_Types);
      View.Sort_Column    := Gint (Columns);
      View.Command_Column := Gint (Columns) + 1;
      View.XML_Column     := Gint (Columns) + 2;

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

      Gtk_New (Rend);

      for C in 1 .. Columns loop
         Gtk_New (Col);
         Pack_Start (Col, Rend, False);
         Add_Attribute (Col, Rend, "markup", Gint (C - 1));
         Set_Sort_Column_Id (Col, View.Sort_Column);
         Dumm := Append_Column (View.Tree, Col);
         if C = 1 and then View.Sorted then
            Clicked (Col);
         end if;
      end loop;

      Widget_Callback.Connect (View, "destroy", On_Destroy'Access);

      Set_Title (View.Child, Name);
      Raise_Child (View.Child);
   end Initialize_XML_Viewer;

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
         View := new Custom_XML_Viewer_Record;
         Custom_XML_Viewer_Record (View.all).Parser := Nth_Arg (Data, 4, null);
         Custom_XML_Viewer_Record (View.all).On_Click :=
           Nth_Arg (Data, 5, null);
         Custom_XML_Viewer_Record (View.all).On_Select :=
           Nth_Arg (Data, 6, null);
         View.Sorted := Nth_Arg (Data, 7, False);
         Custom_XML_Viewer_Record (View.all).Columns := Nth_Arg (Data, 3, 3);
         Initialize_XML_Viewer
           (View, Kernel,
            Name    => Nth_Arg (Data, 2),
            Columns => Custom_XML_Viewer_Record (View.all).Columns);
         Set_Data (Inst, Widget => GObject (View));

      elsif Command = "create_metric" then
         Name_Parameters (Data, XML_Metrics_Params);
         View := new Metrix_XML_Viewer_Record;
         View.Sorted := True;
         Initialize_XML_Viewer (View, Kernel, Nth_Arg (Data, 1), 2);
         Inst := Get_Instance (Get_Script (Data), Widget => View);
         if Inst = No_Class_Instance then
            Inst := New_Instance (Get_Script (Data), XML_Viewer_Class);
            Set_Data (Inst, Widget => GObject (View));
         end if;
         Set_Return_Value (Data, Inst);

      elsif Command = "parse" then
         Name_Parameters (Data, Parse_Params);
         Inst := Nth_Arg (Data, 1, XML_Viewer_Class);
         View := XML_Viewer (GObject'(Get_Data (Inst)));
         declare
            Error : constant String :=
              Parse_XML (View, Nth_Arg (Data, 2), Is_File_Name => True);
         begin
            if Error /= "" then
               Set_Error_Msg (Data, Error);
            end if;
         end;

      elsif Command = "parse_string" then
         Name_Parameters (Data, Parse_String_Params);
         Inst := Nth_Arg (Data, 1, XML_Viewer_Class);
         View := XML_Viewer (GObject'(Get_Data (Inst)));
         declare
            Error : constant String :=
              Parse_XML (View, Nth_Arg (Data, 2), Is_File_Name => False);
         begin
            if Error /= "" then
               Set_Error_Msg (Data, Error);
            end if;
         end;
      end if;
   end XML_Commands_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      XML_Viewer_Class : constant Class_Type :=
                           New_Class (Kernel, "XMLViewer");
   begin
      Register_Command
        (Kernel, Constructor_Method,
         Class        => XML_Viewer_Class,
         Minimum_Args => 1,
         Maximum_Args => 6,
         Handler      => XML_Commands_Handler'Access);

      Register_Command
        (Kernel, "create_metric",
         Class         => XML_Viewer_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => XML_Commands_Handler'Access);

      Register_Command
        (Kernel, "parse",
         Class        => XML_Viewer_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => XML_Commands_Handler'Access);

      Register_Command
        (Kernel, "parse_string",
         Class        => XML_Viewer_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => XML_Commands_Handler'Access);
   end Register_Commands;

end XML_Viewer;
