-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Enums;

with Gtkada.Handlers;          use Gtkada.Handlers;
with Glide_Intl;               use Glide_Intl;

with Commands;                 use Commands;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

package body Task_Manager.GUI is

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column             : constant := 0;
   Command_Name_Column     : constant := 1;
   Command_Status_Column   : constant := 2;
   Command_Progress_Column : constant := 3;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types
     (View : access Task_Manager_Interface_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure On_View_Destroy
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a "destroy" signal.

   ---------------------
   -- On_View_Destroy --
   ---------------------

   procedure On_View_Destroy
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
   begin
      Task_Manager_Interface (Object).Manager.GUI := null;
   end On_View_Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Task_Manager_Interface_Record'Class) is
      Global : constant Boolean := View.Manager.Need_Global_Refresh;
   begin
      if Global then
         Clear (View.Tree.Model);

         Unchecked_Free (View.Lines);

         if View.Manager.Queues = null then
            return;
         end if;

         View.Lines := new Iter_Array (View.Manager.Queues'Range);
      end if;

      for J in View.Manager.Queues'Range loop
         View.Manager.Queues (J).Need_Refresh := True;

         if Global then
            Append (View.Tree.Model, View.Lines (J), Null_Iter);
         end if;

         Refresh_Command (View, J);
      end loop;

      View.Manager.Need_Global_Refresh := False;
   end Refresh;

   ---------------------
   -- Refresh_Command --
   ---------------------

   procedure Refresh_Command
     (View  : access Task_Manager_Interface_Record'Class;
      Index : Integer)
   is
      Command  : Command_Access;
      Progress : Progress_Record;
   begin
      if not (Index in View.Manager.Queues'Range)
        or else not View.Manager.Queues (Index).Need_Refresh
        or else not (Index in View.Lines'Range)
      then
         return;
      end if;

      if not Command_Queues.Is_Empty (View.Manager.Queues (Index).Queue) then
         Command := Command_Queues.Head (View.Manager.Queues (Index).Queue);

         Set
           (View.Tree.Model, View.Lines (Index), Command_Name_Column,
            Name (Command));

         Progress := Commands.Progress (Command);

         Set
           (View.Tree.Model, View.Lines (Index), Command_Status_Column,
              -(To_Lower (Progress.Activity'Img)));

         Set
           (View.Tree.Model, View.Lines (Index), Command_Progress_Column,
            Progress.Current'Img & "/" & Progress.Total'Img);
      end if;

      View.Manager.Queues (Index).Need_Refresh := False;
   end Refresh_Command;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (View : access Task_Manager_Interface_Record'Class)
   is
      Tree          : constant Tree_View := View.Tree;
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Dummy         : Gint;
      pragma Unreferenced (Dummy);

   begin
      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Set_Title (Col, -"Task");
      Gtk_New (Pixbuf_Rend);
      Gtk_New (Text_Rend);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Command_Name_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Status");
      Gtk_New (Text_Rend);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Command_Status_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Progress");
      Gtk_New (Text_Rend);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Command_Progress_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column             => Gdk.Pixbuf.Get_Type,
         Command_Name_Column     => GType_String,
         Command_Status_Column   => GType_String,
         Command_Progress_Column => GType_String);
   end Columns_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View    : out Task_Manager_Interface;
      Manager : Task_Manager_Access) is
   begin
      View := new Task_Manager_Interface_Record;
      Initialize (View, Manager);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Manager : Task_Manager_Access)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Hbox (View);

      View.Manager := Manager;

      --  Initialize the tree.

      Gtk_New (View.Tree, Columns_Types);
      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, True);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Always);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      View.Manager.GUI := Gtk_Widget (View);

      Refresh (View);

      Object_Callback.Object_Connect
        (View,
         "destroy",
         On_View_Destroy'Access,
         GObject (View),
         After => False);
   end Initialize;

end Task_Manager.GUI;
