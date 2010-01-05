-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009-2010, AdaCore                  --
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

with Ada.Unchecked_Conversion;

with Glib.Convert;

with GPS.Kernel.Messages.Classic_Models;
with Traces;

package body GPS.Kernel.Messages is

   use Ada.Containers;
   use Ada.Strings.Unbounded;
   use Category_Maps;
   use File_Maps;
   use GPS.Editors;
   use Listener_Vectors;
   use Model_Vectors;
   use Node_Vectors;
   use Traces;

   procedure Notify_Listeners_About_Message_Added
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class);
   --  Call listeners to notify about add of message.

   procedure Notify_Models_About_Message_Added
     (Self    : not null access constant Messages_Container'Class;
      Message : not null Message_Access);
   --  Call models to notify about add of the message.

   procedure Remove_Category
     (Self              : not null access Messages_Container'Class;
      Category_Position : in out Category_Maps.Cursor;
      Category_Index    : Positive;
      Category_Node     : in out Node_Access);
   --  Removes specified category and all underling entities.

   procedure Remove_File
     (Self          : not null access Messages_Container'Class;
      File_Position : in out File_Maps.Cursor;
      File_Index    : Positive;
      File_Node     : in out Node_Access);
   --  Removes specified file and all underling entities.

   function Get_Container
     (Self : not null access constant Abstract_Message'Class)
      return not null Messages_Container_Access;

   function To_Address is
     new Ada.Unchecked_Conversion (Messages_Container_Access, System.Address);
   function To_Messages_Container_Access is
     new Ada.Unchecked_Conversion (System.Address, Messages_Container_Access);

   ------------------------------
   -- Create_Message_Container --
   ------------------------------

   function Create_Message_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return System.Address
   is
      use GPS.Kernel.Messages.Classic_Models;

      Result : constant Messages_Container_Access :=
        new Messages_Container (Kernel);
      Model  : Classic_Tree_Model;

   begin
      Gtk_New (Model, Result);

      Result.Models.Append (Messages_Model_Access (Model));

      return To_Address (Result);
   end Create_Message_Container;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : not null access Abstract_Message) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Editor_Mark'Class, Editor_Mark_Access);

   begin
      Free (Self.Mark);
   end Finalize;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Self : not null access constant Abstract_Message'Class) return String is
   begin
      return To_String (Self.Get_Category);
   end Get_Category;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Self : not null access constant Abstract_Message'Class)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      case Self.Level is
         when Primary =>
            return Self.Parent.Parent.Name;

         when Secondary =>
            return Abstract_Message'Class (Self.Parent.all).Get_Category;
      end case;
   end Get_Category;

   ----------------------------
   -- Get_Classic_Tree_Model --
   ----------------------------

   function Get_Classic_Tree_Model
     (Self : not null access constant Messages_Container'Class)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
   begin
      return Gtk.Tree_Model.Gtk_Tree_Model (Self.Models.First_Element);
   end Get_Classic_Tree_Model;

   -------------------
   -- Get_Container --
   -------------------

   function Get_Container
     (Self : not null access constant Abstract_Message'Class)
      return not null Messages_Container_Access is
   begin
      case Self.Level is
         when Primary =>
            return Self.Parent.Parent.Container;

         when Secondary =>
            return Abstract_Message'Class (Self.Parent.all).Get_Container;
      end case;
   end Get_Container;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Self : not null access constant Abstract_Message'Class)
      return GNATCOLL.VFS.Virtual_File is
   begin
      case Self.Level is
         when Primary =>
            return Self.Parent.File;

         when Secondary =>
            return Self.Corresponding_File;
      end case;
   end Get_File;

   ----------------
   -- Get_Markup --
   ----------------

   function Get_Markup
     (Self : not null access constant Abstract_Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return
        To_Unbounded_String
          (Glib.Convert.Escape_Text
               (To_String (Abstract_Message'Class (Self.all).Get_Text)));
   end Get_Markup;

   --------------------
   -- Get_Message_At --
   --------------------

   function Get_Message_At
     (Self     : not null access constant Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type)
      return Message_Access
   is
      use Basic_Types;

      Category_Position : constant Category_Maps.Cursor :=
        Self.Category_Map.Find (To_Unbounded_String (Category));
      File_Position     : File_Maps.Cursor;
      Message_Position  : Node_Vectors.Cursor;
      Message           : Node_Access;

   begin
      if not Has_Element (Category_Position) then
         return null;
      end if;

      File_Position := Element (Category_Position).File_Map.Find (File);

      if not Has_Element (File_Position) then
         return null;
      end if;

      Message_Position := Element (File_Position).Children.Last;
      --  Go from the last message to first one to satisfy subprogram's
      --  semantic.

      while Has_Element (Message_Position) loop
         Message := Element (Message_Position);

         if Message.Line = Line
           and then Message.Column = Column
         then
            return Message_Access (Message);
         end if;

         Previous (Message_Position);
      end loop;

      return null;
   end Get_Message_At;

   ----------------------------
   -- Get_Messages_Container --
   ----------------------------

   function Get_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access is
   begin
      return To_Messages_Container_Access (Kernel.Messages_Container);
   end Get_Messages_Container;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (Self : not null access constant Abstract_Message'Class)
      return Message_Access is
   begin
      return Message_Access (Self.Parent);
   end Get_Parent;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : not null access Abstract_Message'Class;
      Container : not null Messages_Container_Access;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type)
   is
      pragma Assert (Category /= "");
      pragma Assert (File /= No_File);

      Category_Name     : constant Unbounded_String :=
        To_Unbounded_String (Category);
      Category_Position : constant Category_Maps.Cursor :=
        Container.Category_Map.Find (Category_Name);
      Category_Node     : Node_Access;
      File_Position     : File_Maps.Cursor;
      File_Node         : Node_Access;

   begin
      Self.Line := Line;
      Self.Column := Column;
      Self.Mark :=
        new Editor_Mark'Class'
          (Container.Kernel.Get_Buffer_Factory.New_Mark
               (File, Line, Integer (Column)));

      --  Resolve category node, create new one when there is no existent node

      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);

      else
         Category_Node :=
           new Node_Record'
             (Kind      => Node_Category,
              Parent    => null,
              Children  => Node_Vectors.Empty_Vector,
              Container => Container,
              Name      => Category_Name,
              File_Map  => File_Maps.Empty_Map);
         Container.Categories.Append (Category_Node);
         Container.Category_Map.Insert (Category_Name, Category_Node);

         --  Notify listeners

         declare
            Listener_Position : Listener_Vectors.Cursor :=
              Container.Listeners.First;

         begin
            while Has_Element (Listener_Position) loop
               begin
                  Element (Listener_Position).Category_Added (Category_Name);

               exception
                  when E : others =>
                     Trace (Exception_Handle, E);
               end;

               Next (Listener_Position);
            end loop;
         end;

         --  Notify models

         declare
            Model_Position : Model_Vectors.Cursor := Container.Models.First;

         begin
            while Has_Element (Model_Position) loop
               Element (Model_Position).Category_Added (Category_Node);
               Next (Model_Position);
            end loop;
         end;
      end if;

      --  Resolve file node, create new one when there is no existent node

      File_Position := Category_Node.File_Map.Find (File);

      if Has_Element (File_Position) then
         File_Node := Element (File_Position);

      else
         File_Node :=
           new Node_Record'
             (Kind     => Node_File,
              Parent   => Category_Node,
              Children => Node_Vectors.Empty_Vector,
              File     => File);
         Category_Node.Children.Append (File_Node);
         Category_Node.File_Map.Insert (File, File_Node);

         --  Notify listeners

         declare
            Listener_Position : Listener_Vectors.Cursor :=
              Container.Listeners.First;

         begin
            while Has_Element (Listener_Position) loop
               begin
                  Element (Listener_Position).File_Added (Category_Name, File);

               exception
                  when E : others =>
                     Trace (Exception_Handle, E);
               end;

               Next (Listener_Position);
            end loop;
         end;

         --  Notify models

         declare
            Model_Position : Model_Vectors.Cursor := Container.Models.First;

         begin
            while Has_Element (Model_Position) loop
               Element (Model_Position).File_Added (File_Node);
               Next (Model_Position);
            end loop;
         end;
      end if;

      --  Connect message with file node

      Self.Parent := File_Node;
      File_Node.Children.Append (Node_Access (Self));

      --  Notify listeners

      Container.Notify_Listeners_About_Message_Added (Self);

      --  Notify models

      Notify_Models_About_Message_Added (Container, Message_Access (Self));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Abstract_Message'Class;
      Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type)
   is
   begin
      Self.Corresponding_File := File;
      Self.Line := Line;
      Self.Column := Column;
      Self.Mark :=
        new Editor_Mark'Class'
          (Parent.Get_Container.Kernel.Get_Buffer_Factory.New_Mark
               (File, Line, Integer (Column)));

      Self.Parent := Node_Access (Parent);
      Parent.Children.Append (Node_Access (Self));

      --  Notify listeners and models

      Parent.Get_Container.Notify_Listeners_About_Message_Added (Self);
      Parent.Get_Container.Notify_Models_About_Message_Added
        (Message_Access (Self));
   end Initialize;

   ------------------------------------------
   -- Notify_Listeners_About_Message_Added --
   ------------------------------------------

   procedure Notify_Listeners_About_Message_Added
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class)
   is
      Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

   begin
      while Has_Element (Listener_Position) loop
         begin
            Element (Listener_Position).Message_Added (Message);

         exception
            when E : others =>
               Trace (Exception_Handle, E);
         end;

         Next (Listener_Position);
      end loop;
   end Notify_Listeners_About_Message_Added;

   ---------------------------------------
   -- Notify_Models_About_Message_Added --
   ---------------------------------------

   procedure Notify_Models_About_Message_Added
     (Self    : not null access constant Messages_Container'Class;
      Message : not null Message_Access)
   is
      Model_Position : Model_Vectors.Cursor := Self.Models.First;

   begin
      while Has_Element (Model_Position) loop
         Element (Model_Position).Message_Added (Message);
         Next (Model_Position);
      end loop;
   end Notify_Models_About_Message_Added;

   -----------------------
   -- Register_Listener --
   -----------------------

   procedure Register_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access)
   is
      Listener_Position : constant Listener_Vectors.Cursor :=
        Self.Listeners.Find (Listener);

   begin
      if not Has_Element (Listener_Position) then
         Self.Listeners.Append (Listener);
      end if;
   end Register_Listener;

   -------------------------
   -- Remove_All_Messages --
   -------------------------

   procedure Remove_All_Messages
     (Self : not null access Messages_Container'Class)
   is
      Category_Position : Category_Maps.Cursor;
      Category_Node     : Node_Access;

   begin
      while not Self.Categories.Is_Empty loop
         Category_Node := Self.Categories.Last_Element;
         Category_Position := Self.Category_Map.Find (Category_Node.Name);

         Self.Remove_Category
           (Category_Position,
            Self.Categories.Last_Index,
            Category_Node);
      end loop;
   end Remove_All_Messages;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (Self              : not null access Messages_Container'Class;
      Category_Position : in out Category_Maps.Cursor;
      Category_Index    : Positive;
      Category_Node     : in out Node_Access)
   is
      pragma Assert (Has_Element (Category_Position));
      pragma Assert (Category_Node /= null);

      File_Position : File_Maps.Cursor;
      File_Index    : Positive;
      File_Node     : Node_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation (Node_Record'Class, Node_Access);

   begin
      --  Remove files

      while not Category_Node.Children.Is_Empty loop
         File_Node  := Category_Node.Children.Last_Element;
         File_Index := Category_Node.Children.Last_Index;
         File_Position := Category_Node.File_Map.Find (File_Node.File);

         Self.Remove_File (File_Position, File_Index, File_Node);
      end loop;

      Self.Category_Map.Delete (Category_Position);
      Self.Categories.Delete (Category_Index);

      declare
         Model_Position : Model_Vectors.Cursor := Self.Models.First;

      begin
         while Has_Element (Model_Position) loop
            Element (Model_Position).Category_Removed (Category_Index);
            Next (Model_Position);
         end loop;
      end;

      Free (Category_Node);
   end Remove_Category;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (Self     : not null access Messages_Container'Class;
      Category : String)
   is
      Category_Position : Category_Maps.Cursor :=
        Self.Category_Map.Find (To_Unbounded_String (Category));
      Category_Index    : Positive;
      Category_Node     : Node_Access;

   begin
      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);
         Category_Index := Self.Categories.Find_Index (Category_Node);

         Self.Remove_Category
           (Category_Position, Category_Index, Category_Node);
      end if;
   end Remove_Category;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Self          : not null access Messages_Container'Class;
      File_Position : in out File_Maps.Cursor;
      File_Index    : Positive;
      File_Node     : in out Node_Access)
   is
      Category_Node : constant Node_Access := File_Node.Parent;

      procedure Free is
        new Ada.Unchecked_Deallocation (Node_Record'Class, Node_Access);

      procedure Delete_Messages (Parent_Node : Node_Access);

      ---------------------
      -- Delete_Messages --
      ---------------------

      procedure Delete_Messages (Parent_Node : Node_Access) is
         Message_Node : Node_Access;

      begin
         while not Parent_Node.Children.Is_Empty loop
            Message_Node := Parent_Node.Children.Last_Element;

            Delete_Messages (Message_Node);

            Parent_Node.Children.Delete_Last;

            declare
               Model_Position : Model_Vectors.Cursor := Self.Models.First;

            begin
               while Has_Element (Model_Position) loop
                  Element (Model_Position).Message_Removed
                    (Parent_Node,
                     Natural (Parent_Node.Children.Length) + 1);
                  Next (Model_Position);
               end loop;
            end;

            Finalize (Message_Access (Message_Node));
            Free (Message_Node);
         end loop;
      end Delete_Messages;

   begin
      --  Remove messages

      Delete_Messages (File_Node);

      Category_Node.File_Map.Delete (File_Position);
      Category_Node.Children.Delete (File_Index);

      declare
         Model_Position : Model_Vectors.Cursor := Self.Models.First;

      begin
         while Has_Element (Model_Position) loop
            Element (Model_Position).File_Removed (Category_Node, File_Index);
            Next (Model_Position);
         end loop;
      end;

      Free (File_Node);
   end Remove_File;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Self     : not null access Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Category_Position : constant Category_Maps.Cursor :=
        Self.Category_Map.Find (To_Unbounded_String (Category));
      Category_Node     : Node_Access;
      File_Position     : File_Maps.Cursor;
      File_Index        : Positive;
      File_Node         : Node_Access;

   begin
      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);

         File_Position := Category_Node.File_Map.Find (File);

         if Has_Element (File_Position) then
            File_Node := Element (File_Position);
            File_Index := Category_Node.Children.Find_Index (File_Node);

            Self.Remove_File (File_Position, File_Index, File_Node);
         end if;
      end if;
   end Remove_File;

   -------------------------
   -- Unregister_Listener --
   -------------------------

   procedure Unregister_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access)
   is
      Listener_Position : Listener_Vectors.Cursor :=
        Self.Listeners.Find (Listener);

   begin
      if Has_Element (Listener_Position) then
         Self.Listeners.Delete (Listener_Position);
      end if;
   end Unregister_Listener;

end GPS.Kernel.Messages;
