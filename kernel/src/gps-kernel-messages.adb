------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed.Hash;
with Ada.Unchecked_Conversion;

with Glib.Convert;

with GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.Markup;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages.View;
with GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles; use GPS.Kernel.Styles;
with Traces;
with XML_Parsers;
with XML_Utils;

package body GPS.Kernel.Messages is

   use Ada;
   use Ada.Containers;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ada.Tags;
   use Basic_Types;
   use Category_Maps;
   use File_Maps;
   use GPS.Editors;
   use GPS.Kernel.Hooks;
   use GPS.Kernel.Project;
   use GPS.Styles;
   use GPS.Styles.UI;
   use Listener_Vectors;
   use Node_Vectors;
   use Note_Maps;
   use Projects;
   use Sort_Order_Hint_Maps;
   use Traces;
   use XML_Parsers;
   use XML_Utils;

   Messages_File_Name : constant Filesystem_String := "messages.xml";

   procedure On_Project_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class);
   --  Loads data for opened project.

   procedure On_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Save messages and clears messages container.

   procedure On_File_Renamed_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  React to a file renaming

   package Notifiers is

      procedure Notify_Listeners_About_Category_Added
        (Self     : not null access constant Messages_Container'Class;
         Category : Ada.Strings.Unbounded.Unbounded_String;
         Flags    : Message_Flags);
      --  Calls listeners to notify about add of the category

      procedure Notify_Listeners_About_File_Added
        (Self     : not null access constant Messages_Container'Class;
         Category : Ada.Strings.Unbounded.Unbounded_String;
         File     : GNATCOLL.VFS.Virtual_File;
         Flags    : Message_Flags);
      --  Calls listeners to notify about add of the file

      procedure Notify_Listeners_About_File_Removed
        (Self     : not null access constant Messages_Container'Class;
         Category : Ada.Strings.Unbounded.Unbounded_String;
         File     : GNATCOLL.VFS.Virtual_File;
         Flags    : Message_Flags);
      --  Calls listeners to notify about remove of the file

      procedure Notify_Listeners_About_Message_Added
        (Self    : not null access constant Messages_Container'Class;
         Message : not null access Abstract_Message'Class;
         Flags   : Message_Flags);
      --  Calls listeners to notify about add of message. Flags specify subset
      --  of listeners to be notified.

      procedure Notify_Listeners_About_Message_Property_Changed
        (Self     : not null access constant Messages_Container'Class;
         Message  : not null access Abstract_Message'Class;
         Property : String);
      --  Calls listeners to notify about change of message's property

      procedure Notify_Listeners_About_Message_Removed
        (Self    : not null access constant Messages_Container'Class;
         Message : not null access Abstract_Message'Class;
         Flags   : Message_Flags);
      --  Calls listeners to notify about remove of message. Flags specify
      --  subset of listeners to be notified.

      function Ask_About_Message_Destroy
        (Self    : not null access constant Messages_Container'Class;
         Message : not null access Abstract_Message'Class) return Boolean;
      --  Calls listeners to ask aboud destruction of the message.

   end Notifiers;

   procedure Remove_Category
     (Self              : not null access Messages_Container'Class;
      Category_Position : in out Category_Maps.Cursor;
      Category_Index    : Positive;
      Category_Node     : in out Node_Access;
      Flags             : Message_Flags);
   --  Removes specified category and all underling entities

   procedure Remove_File
     (Self          : not null access Messages_Container'Class;
      File_Position : in out File_Maps.Cursor;
      File_Index    : Positive;
      File_Node     : in out Node_Access;
      Flags         : Message_Flags;
      Recursive     : Boolean);
   --  Removes specified file and all underling entities. Removes category
   --  when it doesn't have items and resursive destruction is allowed.

   procedure Remove_Message
     (Self      : not null access Messages_Container'Class;
      Message   : in out Message_Access;
      Flags     : Message_Flags;
      Recursive : Boolean);
   --  Removes specified message, all secondary messages. Removes enclosing
   --  file and category when they don't have other items and recursive
   --  destruction is allowed.

   function Get_Container
     (Self : not null access constant Abstract_Message'Class)
      return not null Messages_Container_Access;

   procedure Load (Self : not null access Messages_Container'Class);
   --  Loads all messages for the current project

   function To_Address is
     new Unchecked_Conversion (Messages_Container_Access, System.Address);
   function To_Messages_Container_Access is
     new Unchecked_Conversion (System.Address, Messages_Container_Access);

   procedure Free is new Ada.Unchecked_Deallocation
       (GPS.Editors.Line_Information.Line_Information_Record, Action_Item);

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Record'Class, Node_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Abstract_Note'Class, Note_Access);

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access Messages_Container'Class) is
   begin
      Self.Cleanup_Mode := True;
      Self.Remove_All_Messages (Empty_Message_Flags);
      Self.Sort_Order_Hints.Clear;
      Self.Cleanup_Mode := False;
   end Clear;

   -------------------------------
   -- Create_Messages_Container --
   -------------------------------

   function Create_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return System.Address
   is
      Result : constant Messages_Container_Access :=
                 new Messages_Container (Kernel);

   begin
      --  Register simple message load/save procedures

      GPS.Kernel.Messages.Simple.Register (Result);
      GPS.Kernel.Messages.Hyperlink.Register (Result);
      GPS.Kernel.Messages.Markup.Register (Result);

      --  Setup "project_changing" and "project_changed" hook

      Add_Hook
        (Kernel,
         Project_Changing_Hook,
         Wrapper (On_Project_Changing_Hook'Access),
         "messages_container.project_changing");
      Add_Hook
        (Kernel,
         Project_Changed_Hook,
         Wrapper (On_Project_Changed_Hook'Access),
         "messages_container.project_changed");

      Add_Hook
        (Kernel,
         File_Renamed_Hook,
         Wrapper (On_File_Renamed_Hook'Access),
         "messages_container.file_renamed");

      return To_Address (Result);
   end Create_Messages_Container;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : not null access Abstract_Message) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Editor_Mark'Class, Editor_Mark_Access);

      Position : Note_Maps.Cursor;
      Aux      : Note_Access;

   begin
      --  Destroy notes.

      while not Self.Notes.Is_Empty loop
         Position := Self.Notes.First;
         Aux := Element (Position);
         Self.Notes.Delete (Position);
         Finalize (Aux);
         Free (Aux);
      end loop;

      --  Delete editor's mark.

      if Self.Mark /= null then
         Self.Mark.Delete;
         Free (Self.Mark);
      end if;

      --  Destroy action item.

      if Self.Action /= null then
         GPS.Editors.Line_Information.Free (Self.Action.all);
      end if;

      Free (Self.Action);
   end Finalize;

   -----------------------------
   -- Free_Messages_Container --
   -----------------------------

   procedure Free_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
   is

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Messages_Container'Class, Messages_Container_Access);

      Container : Messages_Container_Access := Get_Messages_Container (Kernel);

   begin
      Container.Clear;

      Free (Container);
   end Free_Messages_Container;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (Self : not null access constant Abstract_Message'Class)
      return Action_Item is
   begin
      return Self.Action;
   end Get_Action;

   ------------------
   -- Has_Category --
   ------------------

   function Has_Category
     (Self     : not null access constant Messages_Container'Class;
      Category : String) return Boolean is
   begin
      return Self.Category_Map.Contains (To_Unbounded_String (Category));
   end Has_Category;

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

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Self : not null access constant Abstract_Message'Class)
      return Message_Flags is
   begin
      return Self.Flags;
   end Get_Flags;

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

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Self : not null access constant Abstract_Message'Class)
      return Basic_Types.Visible_Column_Type is
   begin
      return Self.Column;
   end Get_Column;

   ----------------
   -- Get_Weight --
   ----------------

   function Get_Weight
     (Self : not null access constant Abstract_Message'Class)
      return Natural is
   begin
      case Self.Level is
         when Primary =>
            return Self.Weight;
         when Secondary =>
            return 0;
      end case;
   end Get_Weight;

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
      if Self.Mark /= null then
         return Self.Mark.all;
      else
         return Nil_Editor_Mark;
      end if;
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
      return GPS.Styles.UI.Style_Access is
   begin
      return Self.Style;
   end Get_Highlighting_Style;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Self : not null access constant Abstract_Message'Class)
      return Natural is
   begin
      return Self.Line;
   end Get_Line;

   ----------------
   -- Get_Markup --
   ----------------

   function Get_Markup
     (Self : not null access constant Abstract_Message)
      return Ada.Strings.Unbounded.Unbounded_String is
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
      File     : GNATCOLL.VFS.Virtual_File) return Message_Array
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

   --------------
   -- Get_Note --
   --------------

   function Get_Note
     (Self : not null access constant Abstract_Message'Class;
      Tag  : Ada.Tags.Tag) return not null Note_Access
   is
      Position : constant Note_Maps.Cursor := Self.Notes.Find (Tag);

   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         raise Constraint_Error
           with "There is no note associated with the message";
      end if;
   end Get_Note;

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

   --------------
   -- Has_Note --
   --------------

   function Has_Note
     (Self : not null access constant Abstract_Message'Class;
      Tag  : Ada.Tags.Tag) return Boolean is
   begin
      return Self.Notes.Contains (Tag);
   end Has_Note;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Ada.Tags.Tag) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Fixed.Hash (External_Tag (Item));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self          : not null access Abstract_Message'Class;
      Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Weight        : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags)
   is
      pragma Assert (Category /= "");

      Category_Name     : constant Unbounded_String :=
                            To_Unbounded_String (Category);
      Category_Position : constant Category_Maps.Cursor :=
                            Container.Category_Map.Find (Category_Name);
      Category_Node     : Node_Access;
      File_Position     : File_Maps.Cursor;
      File_Node         : Node_Access;
      Sort_Position     : Sort_Order_Hint_Maps.Cursor;
      Sort_Hint         : Sort_Order_Hint;

   begin
      Self.Line := Line;
      Self.Column := Column;
      Self.Weight := Weight;
      Self.Flags  := Flags;

      if File /= No_File then
         Self.Mark :=
           new Editor_Mark'Class'
             (Container.Kernel.Get_Buffer_Factory.New_Mark
                  (File, Actual_Line, Actual_Column));
      end if;

      --  Resolve category node, create new one when there is no existent node

      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);

      else
         Sort_Position := Container.Sort_Order_Hints.Find (Category_Name);

         if Has_Element (Sort_Position) then
            Sort_Hint := Element (Sort_Position);

         else
            Sort_Hint := Chronological;
         end if;

         Category_Node :=
           new Node_Record'
             (Kind          => Node_Category,
              Parent        => null,
              Flags         => Flags,
              Children      => Node_Vectors.Empty_Vector,
              Container     => Container,
              Name          => Category_Name,
              File_Map      => File_Maps.Empty_Map,
              Sort_Hint     => Sort_Hint);
         Container.Categories.Append (Category_Node);
         Container.Category_Map.Insert (Category_Name, Category_Node);

         --  Notify listeners

         Notifiers.Notify_Listeners_About_Category_Added
           (Container, Category_Name, Flags);
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
              Flags         => Flags,
              Children      => Node_Vectors.Empty_Vector,
              File          => File);
         Category_Node.Children.Append (File_Node);
         Category_Node.File_Map.Insert (File, File_Node);

         --  Notify listeners

         Notifiers.Notify_Listeners_About_File_Added
           (Container, Category_Name, File, Flags);
      end if;

      --  Connect message with file node

      Self.Parent := File_Node;
      File_Node.Children.Append (Node_Access (Self));

      --  Update flags of category and file nodes

      for K in Message_Visibility_Kind loop
         File_Node.Flags (K) := File_Node.Flags (K) or Self.Flags (K);
         Category_Node.Flags (K) := Category_Node.Flags (K) or Self.Flags (K);
      end loop;

      --  Notify listeners

      Notifiers.Notify_Listeners_About_Message_Added
        (Container, Self, Self.Flags);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self          : not null access Abstract_Message'Class;
      Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags) is
   begin
      Self.Corresponding_File := File;
      Self.Line := Line;
      Self.Column := Column;
      Self.Flags := Flags;

      if File /= No_File then
         Self.Mark :=
           new Editor_Mark'Class'
             (Parent.Get_Container.Kernel.Get_Buffer_Factory.New_Mark
                  (File, Actual_Line, Actual_Column));
      end if;

      Self.Parent := Node_Access (Parent);
      Parent.Children.Append (Node_Access (Self));

      --  Notify listeners

      Notifiers.Notify_Listeners_About_Message_Added
        (Parent.Get_Container, Self, Self.Flags);
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load (Self : not null access Messages_Container'Class) is

      procedure Load_Message
        (XML_Node : Node_Ptr;
         Category : String;
         File     : Virtual_File);
      --  Loads primary message and its secondary messages

      procedure Load_Message
        (XML_Node : Node_Ptr;
         Parent   : not null Message_Access);
      --  Loads secondary message

      ------------------
      -- Load_Message --
      ------------------

      procedure Load_Message
        (XML_Node : Node_Ptr;
         Category : String;
         File     : Virtual_File)
      is
         Class         : constant Tag :=
                           Internal_Tag
                             (Get_Attribute (XML_Node, "class", ""));
         Line          : constant Natural :=
                           Natural'Value
                             (Get_Attribute (XML_Node, "line", ""));
         Column        : constant Visible_Column_Type :=
                           Visible_Column_Type'Value
                             (Get_Attribute (XML_Node, "column", ""));
         Weight        : constant Natural :=
                           Natural'Value
                             (Get_Attribute (XML_Node, "weight", "0"));

         Flags : constant Message_Flags :=
           From_Int (Integer'Value (Get_Attribute (XML_Node, "flags", "0")));

         Actual_Line   : constant Integer :=
                           Integer'Value
                             (Get_Attribute
                                (XML_Node,
                                 "actual_line",
                                 Natural'Image (Line)));
         Actual_Column : constant Integer :=
                           Integer'Value
                             (Get_Attribute
                                (XML_Node,
                                 "actual_column",
                                 Visible_Column_Type'Image (Column)));
         Style_Name    : constant String :=
                           Get_Attribute (XML_Node, "highlighting_style", "");
         Length        : constant Natural :=
                           Natural'Value
                             (Get_Attribute
                                (XML_Node, "highlighting_length", "0"));
         Message       : Message_Access;
         XML_Child     : Node_Ptr := XML_Node.Child;
         Style         : Style_Access;

      begin
         Message :=
           Self.Primary_Loaders.Element (Class)
           (XML_Node,
            Messages_Container_Access (Self),
            Category,
            File,
            Line,
            Column,
            Weight,
            Actual_Line,
            Actual_Column,
            Flags);

         if Style_Name /= "" then
            Style := Get_Or_Create_Style (Self.Kernel, Style_Name, True);

            if Length = 0 then
               Set_Highlighting (Message, Style);

            else
               Set_Highlighting (Message, Style, Length);
            end if;
         end if;

         while XML_Child /= null loop
            if XML_Child.Tag.all = "message" then
               Load_Message (XML_Child, Message);
            end if;

            XML_Child := XML_Child.Next;
         end loop;
      end Load_Message;

      ------------------
      -- Load_Message --
      ------------------

      procedure Load_Message
        (XML_Node : Node_Ptr;
         Parent   : not null Message_Access)
      is
         Class         : constant Tag :=
                           Internal_Tag
                             (Get_Attribute (XML_Node, "class", ""));
         File          : constant Virtual_File :=
                           Get_File_Child (XML_Node, "file");
         Line          : constant Natural :=
                           Natural'Value
                             (Get_Attribute (XML_Node, "line", ""));
         Column        : constant Visible_Column_Type :=
                           Visible_Column_Type'Value
             (Get_Attribute (XML_Node, "column", ""));

         Flags : constant Message_Flags :=
           From_Int (Integer'Value (Get_Attribute (XML_Node, "flags", "0")));

         Actual_Line   : constant Integer :=
                           Integer'Value
                             (Get_Attribute
                                (XML_Node,
                                 "actual_line",
                                 Natural'Image (Line)));
         Actual_Column : constant Integer :=
                           Integer'Value
                             (Get_Attribute
                                (XML_Node,
                                 "actual_column",
                                 Visible_Column_Type'Image (Column)));

      begin
         Self.Secondary_Loaders.Element (Class)
           (XML_Node, Parent, File, Line, Column, Actual_Line, Actual_Column,
            Flags);
      end Load_Message;

      Messages_File     : constant Virtual_File :=
                            Create_From_Dir
                              (Self.Kernel.Home_Dir, Messages_File_Name);
      Project_File      : constant Virtual_File :=
                            Get_Project (Self.Kernel).Project_Path;
      Root_XML_Node     : Node_Ptr;
      Project_XML_Node  : Node_Ptr;
      Category_XML_Node : Node_Ptr;
      File_XML_Node     : Node_Ptr;
      Message_XML_Node  : Node_Ptr;
      Error             : GNAT.Strings.String_Access;
      Category          : Unbounded_String;
      File              : Virtual_File;

   begin
      if Messages_File.Is_Regular_File then
         Parse (Messages_File, Root_XML_Node, Error);
      end if;

      if Root_XML_Node /= null then
         Project_XML_Node := Root_XML_Node.Child;

         while Project_XML_Node /= null loop
            exit when Get_File_Child (Project_XML_Node, "file") = Project_File;

            Project_XML_Node := Project_XML_Node.Next;
         end loop;
      end if;

      if Project_XML_Node /= null then
         Category_XML_Node := Project_XML_Node.Child;

         while Category_XML_Node /= null loop
            if Category_XML_Node.Tag.all = "sort_order_hint" then
               Self.Sort_Order_Hints.Insert
                 (To_Unbounded_String
                    (Get_Attribute (Category_XML_Node, "category", "")),
                  Sort_Order_Hint'Value
                    (Get_Attribute
                       (Category_XML_Node,
                        "hint",
                        Sort_Order_Hint'Image (Chronological))));

            elsif Category_XML_Node.Tag.all = "category" then
               Category :=
                 To_Unbounded_String
                   (Get_Attribute (Category_XML_Node, "name", "ERROR"));

               File_XML_Node := Category_XML_Node.Child;

               while File_XML_Node /= null loop
                  File := Get_File_Child (File_XML_Node, "name");

                  Message_XML_Node := File_XML_Node.Child;

                  while Message_XML_Node /= null loop
                     if Message_XML_Node.Tag.all = "message" then
                        Load_Message
                          (Message_XML_Node, To_String (Category), File);
                     end if;

                     Message_XML_Node := Message_XML_Node.Next;
                  end loop;

                  File_XML_Node := File_XML_Node.Next;
               end loop;
            end if;

            Category_XML_Node := Category_XML_Node.Next;
         end loop;
      end if;

      Free (Root_XML_Node);
   end Load;

   -----------
   -- Match --
   -----------

   function Match (A, B : Message_Flags) return Boolean is
   begin
      for K in Message_Visibility_Kind loop
         if A (K) and then B (K) then
            return True;
         end if;
      end loop;

      return False;
   end Match;

   ------------------------------
   -- Message_Can_Be_Destroyed --
   ------------------------------

   function Message_Can_Be_Destroyed
     (Self    : not null access Abstract_Listener;
      Message : not null access Abstract_Message'Class) return Boolean
   is
      pragma Unreferenced (Self, Message);

   begin
      return True;
   end Message_Can_Be_Destroyed;

   ---------------
   -- Notifiers --
   ---------------

   package body Notifiers is

      -------------------------------
      -- Ask_About_Message_Destroy --
      -------------------------------

      function Ask_About_Message_Destroy
        (Self    : not null access constant Messages_Container'Class;
         Message : not null access Abstract_Message'Class) return Boolean
      is
         Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;
         Result            : Boolean := True;

      begin
         while Has_Element (Listener_Position) loop
            begin
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match
                           (Element (Listener_Position).Flags, Message.Flags)
               then
                  Result :=
                    Result
                      and Element (Listener_Position).Message_Can_Be_Destroyed
                            (Message);
               end if;

            exception
               when E : others =>
                  Trace (Exception_Handle, E);
            end;

            Next (Listener_Position);
         end loop;

         return Result;
      end Ask_About_Message_Destroy;

      -------------------------------------------
      -- Notify_Listeners_About_Category_Added --
      -------------------------------------------

      procedure Notify_Listeners_About_Category_Added
        (Self     : not null access constant Messages_Container'Class;
         Category : Ada.Strings.Unbounded.Unbounded_String;
         Flags    : Message_Flags)
      is
         Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

      begin
         while Has_Element (Listener_Position) loop
            begin
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match (Element (Listener_Position).Flags, Flags)
               then
                  Element (Listener_Position).Category_Added (Category);
               end if;

            exception
               when E : others =>
                  Trace (Exception_Handle, E);
            end;

            Next (Listener_Position);
         end loop;
      end Notify_Listeners_About_Category_Added;

      ---------------------------------------
      -- Notify_Listeners_About_File_Added --
      ---------------------------------------

      procedure Notify_Listeners_About_File_Added
        (Self     : not null access constant Messages_Container'Class;
         Category : Ada.Strings.Unbounded.Unbounded_String;
         File     : GNATCOLL.VFS.Virtual_File;
         Flags    : Message_Flags)
      is
         Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

      begin
         while Has_Element (Listener_Position) loop
            begin
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match (Element (Listener_Position).Flags, Flags)
               then
                  Element (Listener_Position).File_Added (Category, File);
               end if;

            exception
               when E : others =>
                  Trace (Exception_Handle, E);
            end;

            Next (Listener_Position);
         end loop;
      end Notify_Listeners_About_File_Added;

      ---------------------------------------
      -- Notify_Listeners_About_File_Removed --
      ---------------------------------------

      procedure Notify_Listeners_About_File_Removed
        (Self     : not null access constant Messages_Container'Class;
         Category : Ada.Strings.Unbounded.Unbounded_String;
         File     : GNATCOLL.VFS.Virtual_File;
         Flags    : Message_Flags)
      is
         Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

      begin
         while Has_Element (Listener_Position) loop
            begin
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match (Element (Listener_Position).Flags, Flags)
               then
                  Element (Listener_Position).File_Removed (Category, File);
               end if;

            exception
               when E : others =>
                  Trace (Exception_Handle, E);
            end;

            Next (Listener_Position);
         end loop;
      end Notify_Listeners_About_File_Removed;

      ------------------------------------------
      -- Notify_Listeners_About_Message_Added --
      ------------------------------------------

      procedure Notify_Listeners_About_Message_Added
        (Self    : not null access constant Messages_Container'Class;
         Message : not null access Abstract_Message'Class;
         Flags   : Message_Flags)
      is
         Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

      begin
         while Has_Element (Listener_Position) loop
            begin
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match (Element (Listener_Position).Flags, Flags)
               then
                  Element (Listener_Position).Message_Added (Message);
               end if;

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
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match
                   (Element (Listener_Position).Flags, Message.Flags)
               then
                  Element (Listener_Position).Message_Property_Changed
                    (Message, Property);
               end if;

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
         Message : not null access Abstract_Message'Class;
         Flags   : Message_Flags)
      is
         Listener_Position : Listener_Vectors.Cursor := Self.Listeners.First;

      begin
         while Has_Element (Listener_Position) loop
            begin
               if Element (Listener_Position).Flags = Empty_Message_Flags
                 or else Match (Element (Listener_Position).Flags, Flags)
               then
                  Element (Listener_Position).Message_Removed (Message);
               end if;

            exception
               when E : others =>
                  Trace (Exception_Handle, E);
            end;

            Next (Listener_Position);
         end loop;
      end Notify_Listeners_About_Message_Removed;

   end Notifiers;

   -----------------------------
   -- On_Project_Changed_Hook --
   -----------------------------

   procedure On_Project_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Container : constant Messages_Container_Access :=
                    Get_Messages_Container (Kernel);

   begin
      --  Load messages for opened project

      GPS.Kernel.Messages.View.Do_Not_Goto_First_Location (Kernel);
      Container.Project_File := Get_Project (Kernel).Project_Path;
      Container.Load;
   end On_Project_Changed_Hook;

   ------------------------------
   -- On_Project_Changing_Hook --
   ------------------------------

   procedure On_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);

      Container : constant Messages_Container_Access :=
                    Get_Messages_Container (Kernel);

   begin
      --  Save messages for current project

      Container.Save;

      --  Clear container.

      Container.Clear;
   end On_Project_Changing_Hook;

   --------------------------
   -- On_File_Renamed_Hook --
   --------------------------

   procedure On_File_Renamed_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File : constant Virtual_File := Files_2_Hooks_Args (Data.all).File;

      Container : constant Messages_Container_Access :=
        Get_Messages_Container (Kernel);

      Categories : constant Unbounded_String_Array :=
        Get_Categories (Container);
      Category_Position : Category_Maps.Cursor;
      Category_Node     : Node_Access;
      File_Position     : File_Maps.Cursor;
      File_Index        : Positive;
      File_Node         : Node_Access;

   begin
      --  ??? In the future it would be nice to move all messages from the
      --  original file to the new file. For now we just remove messages
      --  from the original file, for safety.

      for J in Categories'Range loop
         Category_Position := Container.Category_Map.Find (Categories (J));
         if Has_Element (Category_Position) then
            Category_Node := Element (Category_Position);

            File_Position := Category_Node.File_Map.Find (File);

            if Has_Element (File_Position) then
               File_Node := Element (File_Position);
               File_Index := Category_Node.Children.Find_Index (File_Node);
               Container.Remove_File
                 (File_Position,
                  File_Index, File_Node, (others => True), True);
            end if;
         end if;
      end loop;
   end On_File_Renamed_Hook;

   -----------------------
   -- Register_Listener --
   -----------------------

   procedure Register_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access;
      Flags    : Message_Flags)
   is
      Listener_Position : constant Listener_Vectors.Cursor :=
                            Self.Listeners.Find (Listener);

   begin
      Listener.Flags := Flags;
      if not Has_Element (Listener_Position) then
         Self.Listeners.Append (Listener);
      end if;
   end Register_Listener;

   ----------------------------
   -- Register_Message_Class --
   ----------------------------

   procedure Register_Message_Class
     (Self           : not null access Messages_Container'Class;
      Tag            : Ada.Tags.Tag;
      Save           : not null Message_Save_Procedure;
      Primary_Load   : Primary_Message_Load_Procedure;
      Secondary_Load : Secondary_Message_Load_Procedure) is
   begin
      Self.Savers.Insert (Tag, Save);

      if Primary_Load /= null then
         Self.Primary_Loaders.Insert (Tag, Primary_Load);
      end if;

      if Secondary_Load /= null then
         Self.Secondary_Loaders.Insert (Tag, Secondary_Load);
      end if;
   end Register_Message_Class;

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : not null access Abstract_Message'Class) is
      Message : Message_Access := Message_Access (Self);

   begin
      Self.Get_Container.Remove_Message (Message, Message.Flags, True);
   end Remove;

   -------------------------
   -- Remove_All_Messages --
   -------------------------

   procedure Remove_All_Messages
     (Self  : not null access Messages_Container'Class;
      Flags : Message_Flags)
   is
      Category_Position : Category_Maps.Cursor;
      Category_Node     : Node_Access;

   begin
      for J in reverse 1 .. Self.Categories.Last_Index loop
         Category_Node := Self.Categories.Element (J);
         Category_Position := Self.Category_Map.Find (Category_Node.Name);
         Self.Remove_Category (Category_Position, J, Category_Node, Flags);
      end loop;
   end Remove_All_Messages;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (Self              : not null access Messages_Container'Class;
      Category_Position : in out Category_Maps.Cursor;
      Category_Index    : Positive;
      Category_Node     : in out Node_Access;
      Flags             : Message_Flags)
   is
      pragma Assert (Has_Element (Category_Position));
      pragma Assert (Category_Node /= null);

   begin
      --  Remove files

      for J in reverse 1 .. Category_Node.Children.Last_Index loop
         declare
            File_Node     : Node_Access := Category_Node.Children.Element (J);
            File_Position : File_Maps.Cursor :=
                              Category_Node.File_Map.Find (File_Node.File);

         begin
            Self.Remove_File (File_Position, J, File_Node, Flags, False);
         end;
      end loop;

      if Category_Node.Children.Is_Empty then
         Self.Category_Map.Delete (Category_Position);
         Self.Categories.Delete (Category_Index);

         Free (Category_Node);
      end if;
   end Remove_Category;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (Self     : not null access Messages_Container'Class;
      Category : String;
      Flags    : Message_Flags)
   is
      Category_Position : Category_Maps.Cursor :=
                            Self.Category_Map.Find
                              (To_Unbounded_String (Category));
      Category_Index    : Positive;
      Category_Node     : Node_Access;

   begin
      if Has_Element (Category_Position) then
         Category_Node := Element (Category_Position);
         Category_Index := Self.Categories.Find_Index (Category_Node);

         Self.Remove_Category
           (Category_Position, Category_Index, Category_Node, Flags);
      end if;
   end Remove_Category;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Self          : not null access Messages_Container'Class;
      File_Position : in out File_Maps.Cursor;
      File_Index    : Positive;
      File_Node     : in out Node_Access;
      Flags         : Message_Flags;
      Recursive     : Boolean)
   is
      Category_Node : Node_Access := File_Node.Parent;

   begin
      --  Remove messages

      for J in reverse 1 .. File_Node.Children.Last_Index loop
         declare
            Message : Message_Access :=
              Message_Access (File_Node.Children.Element (J));

         begin
            Self.Remove_Message (Message, Flags, False);
         end;
      end loop;

      if File_Node.Children.Is_Empty then
         --  Notify listeners

         Notifiers.Notify_Listeners_About_File_Removed
           (Self, Category_Node.Name, File_Node.File, File_Node.Flags);

         --  Delete file's node

         Category_Node.File_Map.Delete (File_Position);
         Category_Node.Children.Delete (File_Index);
         Free (File_Node);

         --  Remove category when there are no files for it

         if Recursive
           and then Category_Node.Children.Is_Empty
         then
            declare
               Category_Position : Category_Maps.Cursor :=
                 Self.Category_Map.Find (Category_Node.Name);
               Category_Index    : constant Positive :=
                 Self.Categories.Find_Index (Category_Node);

            begin
               Self.Remove_Category
                 (Category_Position, Category_Index, Category_Node, Flags);
            end;
         end if;
      end if;
   end Remove_File;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Self     : not null access Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Flags    : Message_Flags)
   is
      Category_Position : constant Category_Maps.Cursor :=
                            Self.Category_Map.Find
                              (To_Unbounded_String (Category));
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
            Self.Remove_File
              (File_Position, File_Index, File_Node, Flags, True);
         end if;
      end if;
   end Remove_File;

   --------------------
   -- Remove_Message --
   --------------------

   procedure Remove_Message
     (Self      : not null access Messages_Container'Class;
      Message   : in out Message_Access;
      Flags     : Message_Flags;
      Recursive : Boolean)
   is

      procedure Free is
        new Unchecked_Deallocation (Abstract_Message'Class, Message_Access);

      Parent  : Node_Access := Message.Parent;
      Index   : constant Positive :=
        Parent.Children.Find_Index (Node_Access (Message));
      Destroy : constant Boolean :=
        Self.Cleanup_Mode
          or else Notifiers.Ask_About_Message_Destroy (Self, Message);

   begin
      if Flags = Empty_Message_Flags or else Match (Message.Flags, Flags) then
         for J in reverse 1 .. Message.Children.Last_Index loop
            declare
               Secondary : Message_Access :=
                 Message_Access (Message.Children.Element (J));

            begin
               Self.Remove_Message (Secondary, Empty_Message_Flags, False);
            end;
         end loop;

         Notifiers.Notify_Listeners_About_Message_Removed
           (Self, Message, Message.Flags);

         if Destroy then
            Parent.Children.Delete (Index);
            Message.Finalize;
            Free (Message);

         else
            Message.Flags := (others => False);
         end if;

         --  Remove file node when there are no messages for the file and
         --  recursive destruction is enabled.

         if Recursive
           and then Parent.Kind = Node_File
           and then Parent.Children.Is_Empty
         then
            declare
               Category_Node : constant Node_Access := Parent.Parent;
               File_Position : File_Maps.Cursor :=
                 Category_Node.File_Map.Find (Parent.File);
               File_Index    : constant Positive :=
                 Category_Node.Children.Find_Index (Parent);

            begin
               Self.Remove_File
                 (File_Position, File_Index, Parent, Flags, True);
            end;
         end if;
      end if;
   end Remove_Message;

   ----------
   -- Save --
   ----------

   procedure Save (Self : not null access Messages_Container'Class) is
      use type GNATCOLL.Projects.Project_Status;

   begin
      --  Save messages only for ordinary projects (not empty nor default)

      if GPS.Kernel.Project.Get_Registry (Self.Kernel).Tree.Status
        = GNATCOLL.Projects.From_File
      then
         Self.Save
           (Create_From_Dir (Self.Kernel.Home_Dir, Messages_File_Name),
            (True, True),
            False);
      end if;
   end Save;

   ----------
   -- Save --
   ----------

   procedure Save
     (Self  : not null access Messages_Container'Class;
      File  : GNATCOLL.VFS.Virtual_File;
      Flags : Message_Flags;
      Debug : Boolean)
   is

      procedure Save_Node
        (Current_Node    : not null Node_Access;
         Parent_XML_Node : not null Node_Ptr);
      --  Saves specified node as child of specified XML node

      ---------------
      -- Save_Node --
      ---------------

      procedure Save_Node
        (Current_Node    : not null Node_Access;
         Parent_XML_Node : not null Node_Ptr)
      is
         XML_Node : Node_Ptr;

      begin
         --  Create XML node for the current node with corresponding tag and
         --  add corresponding attributes to it.

         case Current_Node.Kind is
            when Node_Category =>
               XML_Node :=
                 new Node'(Tag => new String'("category"), others => <>);

            when Node_File =>
               XML_Node :=
                 new Node'(Tag => new String'("file"), others => <>);

            when Node_Message =>
               XML_Node :=
                 new Node'(Tag => new String'("message"), others => <>);
         end case;

         Add_Child (Parent_XML_Node, XML_Node, True);

         case Current_Node.Kind is
            when Node_Category =>
               Set_Attribute (XML_Node, "name", To_String (Current_Node.Name));

            when Node_File =>
               Add_File_Child (XML_Node, "name", Current_Node.File);

            when Node_Message =>
               Set_Attribute
                 (XML_Node, "class", External_Tag (Current_Node'Tag));
               Set_Attribute
                 (XML_Node,
                  "line",
                  Trim (Positive'Image (Current_Node.Line), Both));
               Set_Attribute
                 (XML_Node,
                  "column",
                  Trim
                    (Visible_Column_Type'Image (Current_Node.Column), Both));

               declare
                  Flags_Int : constant Integer := To_Int (Current_Node.Flags);
               begin
                  --  No need to emit a flag node if it is going to be 0, since
                  --  this is the default. Saves some bytes in the XML files.
                  if Flags_Int /= 0 then
                     Set_Attribute
                       (XML_Node,
                        "flags", Trim (Integer'Image (Flags_Int), Both));
                  end if;
               end;

               if Current_Node.Mark /= null
                 and then Current_Node.Mark.Line /= Current_Node.Line
               then
                  Set_Attribute
                    (XML_Node,
                     "actual_line",
                     Trim (Integer'Image (Current_Node.Mark.Line), Both));
               end if;

               if Current_Node.Mark /= null
                 and then Current_Node.Mark.Column
                 /= Current_Node.Column
               then
                  Set_Attribute
                    (XML_Node,
                     "actual_column",
                     Trim (Visible_Column_Type'Image
                       (Current_Node.Mark.Column), Both));
               end if;

               if Current_Node.Style /= null then
                  Set_Attribute
                    (XML_Node,
                     "highlighting_style",
                     Get_Name (Current_Node.Style));

                  if Current_Node.Length /= 0 then
                     Set_Attribute
                       (XML_Node,
                        "highlighting_length",
                        Trim (Integer'Image (Current_Node.Length), Both));
                  end if;
               end if;

               case Message_Access (Current_Node).Level is
                  when Primary =>
                     if Message_Access (Current_Node).Weight /= 0 then
                        Set_Attribute
                          (XML_Node,
                           "weight",
                           Trim
                             (Natural'Image
                                (Message_Access (Current_Node).Weight),
                              Both));
                     end if;

                  when Secondary =>
                     Add_File_Child
                       (XML_Node,
                        "file",
                        Message_Access (Current_Node).Corresponding_File);
               end case;

               Self.Savers.Element
                 (Current_Node'Tag) (Message_Access (Current_Node), XML_Node);

               if Debug then
                  --  In debug mode save flag to mark messages with associated
                  --  action

                  if Current_Node.Action /= null then
                     Set_Attribute (XML_Node, "has_action", "true");
                  end if;
               end if;
         end case;

         --  Save child nodes also

         for J in 1 .. Natural (Current_Node.Children.Length) loop
            Save_Node (Current_Node.Children.Element (J), XML_Node);
         end loop;
      end Save_Node;

      Project_File     : constant Virtual_File := Self.Project_File;
      Sort_Position    : Sort_Order_Hint_Maps.Cursor :=
                           Self.Sort_Order_Hints.First;
      Root_XML_Node    : Node_Ptr;
      Project_XML_Node : Node_Ptr;
      Sort_XML_Node    : Node_Ptr;
      Error            : GNAT.Strings.String_Access;

   begin
      if Project_File /= No_File then
         if File.Is_Regular_File then
            Parse (File, Root_XML_Node, Error);
         end if;

         --  Create root node when it is absent

         if Root_XML_Node = null then
            Root_XML_Node :=
              new Node'(Tag => new String'("messages"), others => <>);
         end if;

         --  Remove project specific node if necessary

         declare
            Current : Node_Ptr := Root_XML_Node.Child;

         begin
            while Current /= null loop
               exit when Get_File_Child (Current, "file") = Project_File;

               Current := Current.Next;
            end loop;

            Free (Current);
         end;

         --  Create project node

         Project_XML_Node :=
           new Node'(Tag => new String'("project"), others => <>);
         Add_Child (Root_XML_Node, Project_XML_Node);
         Add_File_Child (Project_XML_Node, "file", Project_File);

         --  Save sort order hints

         while Has_Element (Sort_Position) loop
            if Element (Sort_Position) /= Chronological then
               Sort_XML_Node :=
                 new Node'
                   (Tag => new String'("sort_order_hint"), others => <>);
               Set_Attribute
                 (Sort_XML_Node,
                  "category",
                  To_String (Key (Sort_Position)));
               Set_Attribute
                 (Sort_XML_Node,
                  "hint",
                  Sort_Order_Hint'Image (Element (Sort_Position)));
               Add_Child (Project_XML_Node, Sort_XML_Node, True);
            end if;

            Next (Sort_Position);
         end loop;

         --  Save categories

         for J in 1 .. Natural (Self.Categories.Length) loop
            if Match (Flags, Self.Categories.Element (J).Flags) then
               Save_Node (Self.Categories.Element (J), Project_XML_Node);
            end if;
         end loop;

         Print (Root_XML_Node, File);
         Free (Root_XML_Node);
      end if;
   end Save;

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

      Notifiers.Notify_Listeners_About_Message_Property_Changed
        (Container, Self, "action");
   end Set_Action;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
     (Self  : not null access Abstract_Message'Class;
      Flags : Message_Flags)
   is
      Container : constant Messages_Container_Access := Self.Get_Container;
      Changed   : constant Message_Flags := Self.Flags xor Flags;
      Removed   : constant Message_Flags := Self.Flags and Changed;
      Added     : constant Message_Flags := Flags and Changed;

   begin
      Notifiers.Notify_Listeners_About_Message_Removed
        (Container, Self, Removed);
      Self.Flags := Flags;
      Notifiers.Notify_Listeners_About_Message_Added
        (Container, Self, Added);
   end Set_Flags;

   ----------------------
   -- Set_Highlighting --
   ----------------------

   procedure Set_Highlighting
     (Self   : not null access Abstract_Message'Class;
      Style  : GPS.Styles.UI.Style_Access;
      Length : Positive) is
   begin
      Self.Style := Style;
      Self.Length := Length;

      Notifiers.Notify_Listeners_About_Message_Property_Changed
        (Self.Get_Container, Self, "highlighting");
   end Set_Highlighting;

   ----------------------
   -- Set_Highlighting --
   ----------------------

   procedure Set_Highlighting
     (Self  : not null access Abstract_Message'Class;
      Style : GPS.Styles.UI.Style_Access) is
   begin
      Self.Style := Style;
      Self.Length := 0;

      Notifiers.Notify_Listeners_About_Message_Property_Changed
        (Self.Get_Container, Self, "highlighting");
   end Set_Highlighting;

   --------------
   -- Set_Note --
   --------------

   procedure Set_Note
     (Self : not null access Abstract_Message'Class;
      Note : not null Note_Access)
   is
      Position : Note_Maps.Cursor := Self.Notes.Find (Note'Tag);
      Aux      : Note_Access;

   begin
      if Has_Element (Position) then
         Aux := Element (Position);
         Self.Notes.Delete (Position);
         Finalize (Aux);
         Free (Aux);
      end if;

      Self.Notes.Insert (Note'Tag, Note);
   end Set_Note;

   -----------------
   -- Remove_Note --
   -----------------

   procedure Remove_Note
     (Self : not null access Abstract_Message'Class;
      Tag  : Ada.Tags.Tag)
   is
      Position : Note_Maps.Cursor := Self.Notes.Find (Tag);
      Aux      : Note_Access;
   begin
      if Has_Element (Position) then
         Aux := Element (Position);
         Self.Notes.Delete (Position);
         Finalize (Aux);
         Free (Aux);
      end if;
   end Remove_Note;

   -------------------------
   -- Set_Sort_Order_Hint --
   -------------------------

   procedure Set_Sort_Order_Hint
     (Self     : not null access Messages_Container'Class;
      Category : String;
      Hint     : Sort_Order_Hint)
   is
      Category_Name : constant Unbounded_String :=
                        To_Unbounded_String (Category);
      Position      : constant Sort_Order_Hint_Maps.Cursor :=
                        Self.Sort_Order_Hints.Find (Category_Name);

   begin
      if Has_Element (Position) then
         Self.Sort_Order_Hints.Replace_Element (Position, Hint);

      else
         Self.Sort_Order_Hints.Insert (Category_Name, Hint);
      end if;
   end Set_Sort_Order_Hint;

   -------------------------
   -- Get_Sort_Order_Hint --
   -------------------------

   function Get_Sort_Order_Hint
     (Self     : not null access Messages_Container'Class;
      Category : String) return Sort_Order_Hint
   is
      Category_Name : constant Unbounded_String :=
        To_Unbounded_String (Category);
      Position      : constant Sort_Order_Hint_Maps.Cursor :=
        Self.Sort_Order_Hints.Find (Category_Name);

   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         return Chronological;
      end if;
   end Get_Sort_Order_Hint;

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

   ------------
   -- To_Int --
   ------------

   function To_Int (Flags : Message_Flags) return Integer is
      Int : Integer := 0;

   begin
      for K in Message_Visibility_Kind loop
         if not Flags (K) then
            Int := Int + 2**(Message_Visibility_Kind'Pos (K));
         end if;
      end loop;

      return Int;
   end To_Int;

   --------------
   -- From_Int --
   --------------

   function From_Int (Int : Integer) return Message_Flags is
      Flags : Message_Flags := (Editor_Side => True, Locations => True);
      type T is mod 2**32;
      B : constant T := T (Int);
   begin
      for K in Message_Visibility_Kind loop
         if (B and 2**Message_Visibility_Kind'Pos (K)) /= 0 then
            Flags (K) := False;
         end if;
      end loop;

      return Flags;
   end From_Int;

end GPS.Kernel.Messages;
