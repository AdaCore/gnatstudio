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
   --  Calls listeners to notify about add of message.

   procedure Notify_Listeners_About_Message_Property_Changed
     (Self     : not null access constant Messages_Container'Class;
      Message  : not null access Abstract_Message'Class;
      Property : String);
   --  Calls listeners to notify about change of message's property.

   procedure Notify_Listeners_About_Message_Removed
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class);
   --  Calls listeners to notify about remove of message.

   procedure Notify_Models_About_Message_Added
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class);
   --  Calls models to notify about add of the message.

   procedure Notify_Models_About_Message_Property_Changed
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class);
   --  Calls models to notify about change of message's property.

   procedure Notify_Models_About_Message_Removed
     (Self   : not null access constant Messages_Container'Class;
      Parent : not null access Node_Record'Class;
      Index  : Positive);
   --  Calls models to notify about remove of the message.

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

   procedure Increment_Message_Counters
     (Self : not null access Abstract_Message'Class);
   --  Increments messages counters on parent nodes.

   procedure Decrement_Message_Counters
     (Self : not null access Abstract_Message'Class);
   --  Decrements messages counters on parent nodes.

   function Get_Container
     (Self : not null access constant Abstract_Message'Class)
      return not null Messages_Container_Access;

   function To_Address is
     new Ada.Unchecked_Conversion (Messages_Container_Access, System.Address);
   function To_Messages_Container_Access is
     new Ada.Unchecked_Conversion (System.Address, Messages_Container_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (GPS.Editors.Line_Information_Record, Action_Item);

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

   --------------------------------
   -- Decrement_Message_Counters --
   --------------------------------

   procedure Decrement_Message_Counters
     (Self : not null access Abstract_Message'Class)
   is
      Node : Node_Access := Self.Parent;

   begin
      while Node /= null loop
         Node.Message_Count := Node.Message_Count - 1;
         Node := Node.Parent;
      end loop;
   end Decrement_Message_Counters;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : not null access Abstract_Message) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Editor_Mark'Class, Editor_Mark_Access);

   begin
      Self.Mark.Delete;
      Free (Self.Mark);
      Free (Self.Action);
   end Finalize;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (Self : not null access constant Abstract_Message'Class)
      return Action_Item is
   begin
      return Self.Action;
   end Get_Action;

   --------------------
   -- Get_Categories --
   --------------------

   function Get_Categories
     (Self : not null access constant Messages_Container'Class)
      return Unbounded_String_Array
   is
      Result : Unbounded_String_Array (1 .. Natural (Self.Categories.Length));

   begin
      for J in Result'Range loop
         Result (J) := Self.Categories.Element (J).Name;
      end loop;

      return Result;
   end Get_Categories;

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

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Self : not null access constant Abstract_Message'Class)
      return Basic_Types.Visible_Column_Type is
   begin
      return Self.Column;
   end Get_Column;

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

   ---------------------
   -- Get_Editor_Mark --
   ---------------------

   function Get_Editor_Mark
     (Self : not null access constant Abstract_Message'Class)
      return GPS.Editors.Editor_Mark'Class is
   begin
      return Self.Mark.all;
   end Get_Editor_Mark;

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

   ---------------
   -- Get_Files --
   ---------------

   function Get_Files
     (Self     : not null access constant Messages_Container'Class;
      Category : Ada.Strings.Unbounded.Unbounded_String)
      return Virtual_File_Array
   is
      Category_Position : constant Category_Maps.Cursor :=
        Self.Category_Map.Find (Category);
      Category_Node     : Node_Access;

   begin
      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);

         declare
            Result : Virtual_File_Array
              (1 .. Natural (Category_Node.Children.Length));

         begin
            for J in Result'Range loop
               Result (J) := Category_Node.Children.Element (J).File;
            end loop;

            return Result;
         end;

      else
         return Virtual_File_Array'(1 .. 0 => No_File);
      end if;
   end Get_Files;

   -----------------------------
   -- Get_Highlighting_Length --
   -----------------------------

   function Get_Highlighting_Length
     (Self : not null access constant Abstract_Message'Class) return Natural is
   begin
      return Self.Length;
   end Get_Highlighting_Length;

   ----------------------------
   -- Get_Highlighting_Style --
   ----------------------------

   function Get_Highlighting_Style
     (Self : not null access constant Abstract_Message'Class)
      return GPS.Kernel.Styles.Style_Access is
   begin
      return Self.Style;
   end Get_Highlighting_Style;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Self : not null access constant Abstract_Message'Class)
      return Positive is
   begin
      return Self.Line;
   end Get_Line;

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

   ------------------
   -- Get_Messages --
   ------------------

   function Get_Messages
     (Self     : not null access constant Messages_Container'Class;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
      return Message_Array
   is
      Category_Position : constant Category_Maps.Cursor :=
        Self.Category_Map.Find (Category);
      Category_Node     : Node_Access;
      File_Position     : File_Maps.Cursor;
      File_Node         : Node_Access;

   begin
      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);
         File_Position := Category_Node.File_Map.Find (File);

         if Has_Element (File_Position) then
            File_Node := Element (File_Position);

            declare
               Result : Message_Array
                 (1 .. Natural (File_Node.Children.Length));

            begin
               for J in Result'Range loop
                  Result (J) :=
                    Message_Access (File_Node.Children.Element (J));
               end loop;

               return Result;
            end;
         end if;
      end if;

      return Message_Array'(1 .. 0 => null);
   end Get_Messages;

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
      case Self.Level is
         when Primary =>
            return null;

         when Secondary =>
            return Message_Access (Self.Parent);
      end case;
   end Get_Parent;

   --------------------------------
   -- Increment_Message_Counters --
   --------------------------------

   procedure Increment_Message_Counters
     (Self : not null access Abstract_Message'Class)
   is
      Node : Node_Access := Self.Parent;

   begin
      while Node /= null loop
         Node.Message_Count := Node.Message_Count + 1;
         Node := Node.Parent;
      end loop;
   end Increment_Message_Counters;

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
      Self.Message_Count := 0;
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
             (Kind          => Node_Category,
              Parent        => null,
              Children      => Node_Vectors.Empty_Vector,
              Message_Count => 0,
              Container     => Container,
              Name          => Category_Name,
              File_Map      => File_Maps.Empty_Map);
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
             (Kind          => Node_File,
              Parent        => Category_Node,
              Children      => Node_Vectors.Empty_Vector,
              Message_Count => 0,
              File          => File);
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

      --  Update message counters

      Self.Increment_Message_Counters;

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
      Self.Message_Count := 0;
      Self.Mark :=
        new Editor_Mark'Class'
          (Parent.Get_Container.Kernel.Get_Buffer_Factory.New_Mark
               (File, Line, Integer (Column)));

      Self.Parent := Node_Access (Parent);
      Parent.Children.Append (Node_Access (Self));

      --  Update messages counters

      Self.Increment_Message_Counters;

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

   -----------------------------------------------------
   -- Notify_Listeners_About_Message_Property_Changed --
   -----------------------------------------------------

   procedure Notify_Listeners_About_Message_Property_Changed
     (Self     : not null access constant Messages_Container'Class;
      Message  : not null access Abstract_Message'Class;
      Property : String)
   is
      Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

   begin
      while Has_Element (Listener_Position) loop
         begin
            Element (Listener_Position).Message_Property_Changed
              (Message, Property);

         exception
            when E : others =>
               Trace (Exception_Handle, E);
         end;

         Next (Listener_Position);
      end loop;
   end Notify_Listeners_About_Message_Property_Changed;

   --------------------------------------------
   -- Notify_Listeners_About_Message_Removed --
   --------------------------------------------

   procedure Notify_Listeners_About_Message_Removed
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class)
   is
      Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

   begin
      while Has_Element (Listener_Position) loop
         begin
            Element (Listener_Position).Message_Removed (Message);

         exception
            when E : others =>
               Trace (Exception_Handle, E);
         end;

         Next (Listener_Position);
      end loop;
   end Notify_Listeners_About_Message_Removed;

   ---------------------------------------
   -- Notify_Models_About_Message_Added --
   ---------------------------------------

   procedure Notify_Models_About_Message_Added
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class)
   is
      Model_Position : Model_Vectors.Cursor := Self.Models.First;

   begin
      while Has_Element (Model_Position) loop
         Element (Model_Position).Message_Added (Message_Access (Message));
         Next (Model_Position);
      end loop;
   end Notify_Models_About_Message_Added;

   --------------------------------------------------
   -- Notify_Models_About_Message_Property_Changed --
   --------------------------------------------------

   procedure Notify_Models_About_Message_Property_Changed
     (Self    : not null access constant Messages_Container'Class;
      Message : not null access Abstract_Message'Class)
   is
      Model_Position : Model_Vectors.Cursor := Self.Models.First;

   begin
      while Has_Element (Model_Position) loop
         Element (Model_Position).Message_Property_Changed
           (Message_Access (Message));
         Next (Model_Position);
      end loop;
   end Notify_Models_About_Message_Property_Changed;

   -----------------------------------------
   -- Notify_Models_About_Message_Removed --
   -----------------------------------------

   procedure Notify_Models_About_Message_Removed
     (Self   : not null access constant Messages_Container'Class;
      Parent : not null access Node_Record'Class;
      Index  : Positive)
   is
      Model_Position : Model_Vectors.Cursor := Self.Models.First;

   begin
      while Has_Element (Model_Position) loop
         Element (Model_Position).Message_Removed
           (Node_Access (Parent), Index);
         Next (Model_Position);
      end loop;
   end Notify_Models_About_Message_Removed;

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

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : not null access Abstract_Message'Class) is

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Abstract_Message'Class, Message_Access);

      Container : constant Messages_Container_Access := Self.Get_Container;
      Parent    : constant Node_Access := Self.Parent;
      Index     : constant Positive :=
        Parent.Children.Find_Index (Node_Access (Self));
      Aux       : Message_Access := Message_Access (Self);

   begin
      while not Self.Children.Is_Empty loop
         Message_Access (Self.Children.Last_Element).Remove;
      end loop;

      Container.Notify_Listeners_About_Message_Removed (Self);
      Self.Decrement_Message_Counters;
      Parent.Children.Delete (Index);
      Container.Notify_Models_About_Message_Removed (Parent, Index);

      Self.Finalize;
      Free (Aux);
   end Remove;

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

   begin
      --  Remove messages

      while not File_Node.Children.Is_Empty loop
         Message_Access (File_Node.Children.Last_Element).Remove;
      end loop;

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

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action
     (Self   : not null access Abstract_Message'Class;
      Action : Action_Item)
   is
      Container : constant Messages_Container_Access := Self.Get_Container;

   begin
      Free (Self.Action);
      Self.Action := Action;

      Container.Notify_Listeners_About_Message_Property_Changed
        (Self, "action");
      Container.Notify_Models_About_Message_Property_Changed (Self);
   end Set_Action;

   ----------------------
   -- Set_Highlighting --
   ----------------------

   procedure Set_Highlighting
     (Self   : not null access Abstract_Message'Class;
      Style  : GPS.Kernel.Styles.Style_Access;
      Length : Positive)
   is
   begin
      Self.Style := Style;
      Self.Length := Length;

      Self.Get_Container.Notify_Listeners_About_Message_Property_Changed
        (Self, "highlighting");
   end Set_Highlighting;

   ----------------------
   -- Set_Highlighting --
   ----------------------

   procedure Set_Highlighting
     (Self  : not null access Abstract_Message'Class;
      Style : GPS.Kernel.Styles.Style_Access)
   is
   begin
      Self.Style := Style;
      Self.Length := 0;

      Self.Get_Container.Notify_Listeners_About_Message_Property_Changed
        (Self, "highlighting");
   end Set_Highlighting;

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
