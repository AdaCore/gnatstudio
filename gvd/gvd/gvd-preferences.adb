-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                    use Glib;
with General_Preferences_Pkg; use General_Preferences_Pkg;
with Glib.XML;
with Gtk.Widget;
with Gdk.Color;               use Gdk.Color;
with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Deallocation;

package body GVD.Preferences is

   package Gint_Xml is new Glib.XML (Gint);
   use Gint_Xml;

   procedure XML_Add_Child (N : Node_Ptr; Child : Node_Ptr);
   procedure XML_Free (N : in out Node_Ptr);
   --  These two functions are there only so that GVD doesn't depend on the
   --  very latest version of GtkAda and can be released independently.
   --  These should be coordinated with GtkAda when a new release of the latter
   --  is done. ???

   Current_Preferences : Node_Ptr;
   --  The XML tree that contains the current preferences.

   Tab_Size_Cached : Gint;
   --  Cached value for tab size, for fast access

   procedure Set
     (Var : String_Gint; Value : Gint; Override : Boolean := False);
   procedure Set
     (Var : String_Guint; Value : Guint; Override : Boolean := False);
   procedure Set
     (Var : String_Boolean; Value : Boolean; Override : Boolean := False);
   procedure Set
     (Var : String_Tooltips_In_Source;
      Value : Tooltips_In_Source_Type;
      Override : Boolean := False);
   pragma Inline (Set);

   procedure Set (Var : String; Value : String; Override : Boolean := False);
   --  Create a new entry in the current preferences, or modify the value
   --  of the existing one (only if Override is True)

   -------------------
   -- XML_Add_Child --
   -------------------

   procedure XML_Add_Child (N : Node_Ptr; Child : Node_Ptr) is
   begin
      Child.Next := N.Child;
      Child.Parent := N;
      N.Child := Child;
   end XML_Add_Child;

   --------------
   -- XML_Free --
   --------------

   procedure XML_Free (N : in out Node_Ptr)
   is
      procedure Free_Node (N : in out Node_Ptr);
      --  Free the memory for a node, but doesn't remove it from its parent

      procedure Unchecked_Free is new Unchecked_Deallocation (Node, Node_Ptr);

      ---------------
      -- Free_Node --
      ---------------

      procedure Free_Node (N : in out Node_Ptr) is
         Child : Node_Ptr := N.Child;
         Previous : Node_Ptr;
      begin
         Gint_Xml.Free (N.Tag);
         Gint_Xml.Free (N.Attributes);
         Gint_Xml.Free (N.Value);

         --  Free all the children
         while Child /= null loop
            Previous := Child.Next;
            Free_Node (Child);
            Child := Previous;
         end loop;

         Unchecked_Free (N);
      end Free_Node;

      Child : Node_Ptr;
      Previous : Node_Ptr;
   begin
      if N = null then
         return;
      end if;

      if N.Parent /= null then
         Child := N.Parent.Child;
      end if;
      --  Remove the node from its parent
      while Child /= null and then Child /= N loop
         if Previous = null then
            N.Parent.Child := Child.Next;
         else
            Previous.Next := Child.Next;
         end if;
         Previous := Child;
         Child := Child.Next;
      end loop;

      --  Free the memory occupied by the node
      Free_Node (N);
   end XML_Free;

   -----------------
   -- Fill_Dialog --
   -----------------

   procedure Fill_Dialog (Dialog : General_Preferences_Access) is
   begin
      null;
   end Fill_Dialog;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences (File_Name : String) is
   begin
      if Current_Preferences /= null then
         XML_Free (Current_Preferences);
      end if;
      Current_Preferences := Parse (File_Name);
      Set_Default_Preferences;
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences (File_Name : String) is
   begin
      null;
   end Save_Preferences;

   ------------------
   -- Get_Tab_Size --
   ------------------

   function Get_Tab_Size return Gint is
   begin
      return Tab_Size_Cached;
   end Get_Tab_Size;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Guint) return Guint is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Guint'Value (Node.Value.all);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_String) return String is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Node.Value.all;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Boolean) return Boolean is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Boolean'Value (Node.Value.all);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Gint) return Gint is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Gint'Value (Node.Value.all);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Color) return Gdk.Color.Gdk_Color is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
      Color : Gdk_Color;
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      Color := Parse (Node.Value.all);
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      --  Alloc_Color (Get_System, Color, True, True, Success);
      return Color;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Font) return String is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Node.Value.all;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Tooltips_In_Source)
      return Tooltips_In_Source_Type
   is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Tooltips_In_Source_Type'Value (Node.Value.all);
   end Get_Pref;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String; Value : String; Override : Boolean := False)
   is
      N : Node_Ptr := Find_Tag (Current_Preferences.Child, Var);
   begin
      if N = null then
         N := new Node;
         N.Tag := new String' (Var);
         N.Value := new String' (Value);
         XML_Add_Child (Current_Preferences, N);
      elsif Override then
         Gint_Xml.Free (N.Value);
         N.Value := new String' (Value);
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Gint; Value : Gint; Override : Boolean := False)
   is
   begin
      Set (String (Var), Gint'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Guint; Value : Guint; Override : Boolean := False) is
   begin
      Set (String (Var), Guint'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Boolean; Value : Boolean; Override : Boolean := False) is
   begin
      Set (String (Var), Boolean'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Tooltips_In_Source;
      Value : Tooltips_In_Source_Type;
      Override : Boolean := False) is
   begin
      Set (String (Var), Tooltips_In_Source_Type'Image (Value), Override);
   end Set;

   -----------------------------
   -- Set_Default_Preferences --
   -----------------------------

   procedure Set_Default_Preferences is
   begin
      if Current_Preferences = null then
         Current_Preferences := new Node;
         Current_Preferences.Tag := new String' ("GVD_Preferences");
      end if;
      Set (Hide_Delay, Guint' (5000));
      Set (String (Remote_Protocol), "rsh");
      Set (String (Remote_Copy), "rcp");
      Set (Display_Explorer, True);
      Set (String (File_Name_Bg_Color), "#BEBEBE");
      Set (String (Editor_Font), "Courier");
      Set (Editor_Font_Size, Gint' (12));
      Set (Editor_Show_Line_Nums, True);
      Set (Editor_Show_Line_With_Code, True);
      Set (Do_Color_Highlighting, True);
      Set (String (Comments_Color), "#FF0000");
      Set (String (Strings_Color), "#A52A2A");
      Set (String (Keywords_Color), "#0000FF");
      Set (Editor_Highlight_Current_Line, True);
      Set (String (Editor_Highlight_Color), "#00CC00");
      Set (Tab_Size, Gint' (8));
      Set (Tooltips_In_Source, Simple);
      Set (String (Asm_Highlight_Color), "#FF0000");
      Set (String (Assembly_Range_Size), "50");
      Set (String (Xref_Color), "#0000FF");
      Set (String (Title_Color), "#BEBEBE");
      Set (String (Change_Color), "#FF0000");
      Set (String (Thaw_Bg_Color), "#FFFFFF");
      Set (String (Freeze_Bg_Color), "#AAAAAA");
      Set (Look_3d, True);
      Set (String (Title_Font), "Helvetica-Bold");
      Set (Title_Font_Size, Default_Font_Size);
      Set (String (Value_Font), "Helvetica");
      Set (String (Command_Font), "Courier");
      Set (String (Type_Font), "Helvetica-Oblique");
      Set (Value_Font_Size, Default_Font_Size);
      Set (Type_Font_Size, Default_Font_Size);
      Set (Annotation_Font_Size, Default_Link_Font_Size);
      Set (Hide_Big_Items, True);
      Set (Big_Item_Height, Gint' (150));
      Set (Default_Detect_Aliases, True);
      Set (Display_Grid, True);
      Set (Align_Items_On_Grid, True);
      Set (String (Debugger_Highlight_Color), "#0000FF");
      Set (String (Debugger_Font), "Courier");
      Set (Debugger_Font_Size, Gint' (12));
      Set (String (Memory_View_Font), "Courier");
      Set (Memory_View_Font_Size, Gint (12));
      Set (String (Memory_View_Color), "#333399");
      Set (String (Memory_Highlighted_Color), "#DDDDDD");
      Set (String (Memory_Selected_Color), "#00009c");
      Set (String (Memory_Modified_Color), "#FF0000");
      Set (String (List_Processes),
           "ps x 2> /dev/null || ps -ef 2> /dev/null || ps");
      Set (String (Default_External_Editor), "glide %f -emacs +%l");

      Tab_Size_Cached := Get_Pref (Tab_Size);
   end Set_Default_Preferences;

begin
   --  Initialize the default values
   Set_Default_Preferences;
end GVD.Preferences;
