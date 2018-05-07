------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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
with GNAT.Strings;

with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Utils;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

with CodePeer.Module;
with GPS.Kernel.Project; use GPS.Kernel.Project;

with CodePeer.Bridge.Reader_Utilities;

package body CodePeer.Bridge.Inspection_Readers.Base is

   use type Code_Analysis.Subprogram_Access;

   Annotation_Category_Tag : constant String := "annotation_category";
   CWE_Category_Tag        : constant String := "cwe_category";
   Entry_Point_Tag         : constant String := "entry_point";
   Entry_Point_Access_Tag  : constant String := "entry_point_access";
   File_Tag                : constant String := "file";
   Message_Tag             : constant String := "message";
   Message_Category_Tag    : constant String := "message_category";
   Object_Race_Tag         : constant String := "object_race";
   Object_Access_Tag       : constant String := "object_access";
   Subprogram_Tag          : constant String := "subprogram";

   Annotations_Attribute        : constant String := "annotations";
   Category_Attribute           : constant String := "category";
   Checks_Attribute             : constant String := "checks";
   Column_Attribute             : constant String := "column";
   CWE_Attribute                : constant String := "cwe";
   Entry_Point_Attribute        : constant String := "entry_point";
   File_Attribute               : constant String := "file";
   Identifier_Attribute         : constant String := "identifier";
   Is_Check_Attribute           : constant String := "is_check";
   Kind_Attribute               : constant String := "kind";
   Line_Attribute               : constant String := "line";
   Name_Attribute               : constant String := "name";
   Primary_Checks_Attribute     : constant String := "primary_checks";
   Rank_Attribute               : constant String := "rank";
   Vn_Id_Attribute              : constant String := "vn-id";
   Vn_Ids_Attribute             : constant String := "vn-ids";

   function Subprogram_Name
     (Self : Base_Inspection_Reader'Class)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns name of the currently processed subprogram when known or
   --  Null_Unbounded_String otherwise.

   procedure Start_Annotation_Category
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'annotation_category' element

   procedure Start_CWE_Category
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'cwe_category' element

   procedure Start_Entry_Point
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'entry_point' element

   procedure Start_Entry_Point_Access
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'entry_point_access' element

   procedure End_Entry_Point_Access
     (Self : in out Base_Inspection_Reader'Class);
   --  Process ending tag of 'entry_point_access' element

   procedure Start_File
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'file' element

   procedure Start_Message_Category
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'message_category' element

   procedure Start_Object_Access
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'object_access' element

   procedure Start_Object_Race
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process starting tag of 'object_race' element

   procedure End_Object_Race (Self : in out Base_Inspection_Reader'Class);
   --  Process ending tag of 'object_race' element

   procedure Include_CWE_Category
     (Self : in out Base_Inspection_Reader'Class;
      Id   : CWE_Identifier;
      Name : Ada.Strings.Unbounded.Unbounded_String);
   --  Includes CWE category into internal data structues when it is not
   --  included yet. Note, in version 3 of interchange format, cwe_category
   --  elements are optional; set of CWE categories is populated from lists
   --  of CWEs from message_category elements.

   procedure Update_CWE
     (Self : in out Base_Inspection_Reader'Class;
      Set  : in out CWE_Category_Sets.Set;
      CWEs : String);
   --  Update sets of CWEs for given set and project node.

   function Get_Optional_Column
     (Attrs : Sax.Attributes.Attributes'Class) return Positive;
   --  Returns value of "column" attribute is specified and 1 instead.

   -------------------------
   -- Annotation_Category --
   -------------------------

   function Annotation_Category
     (Self : Base_Inspection_Reader'Class;
      Id   : Natural) return Annotation_Category_Access is
   begin
      return Self.Annotation_Categories.Element (Id);
   end Annotation_Category;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self  : in out Base_Inspection_Reader;
      Name  : String) is
   begin
      if Self.Ignore_Depth /= 0 then
         --  Decrase depth of ignored XML element.

         Self.Ignore_Depth := Self.Ignore_Depth - 1;

      elsif Name = Message_Tag then
         Base_Inspection_Reader'Class (Self).End_Message;

      elsif Name = Object_Race_Tag then
         Self.End_Object_Race;

      elsif Name = Entry_Point_Access_Tag then
         Self.End_Entry_Point_Access;
      end if;
   end End_Element;

   ----------------------------
   -- End_Entry_Point_Access --
   ----------------------------

   procedure End_Entry_Point_Access
     (Self : in out Base_Inspection_Reader'Class) is
   begin
      Self.Object_Race.Entry_Points.Append (Self.Object_Accesses);
      Self.Object_Accesses :=
        (null, CodePeer.Object_Access_Vectors.Empty_Vector);
   end End_Entry_Point_Access;

   -----------------
   -- End_Message --
   -----------------

   procedure End_Message (Self : in out Base_Inspection_Reader) is
   begin
      Self.Current_Message := null;
   end End_Message;

   ---------------------
   -- End_Object_Race --
   ---------------------

   procedure End_Object_Race (Self : in out Base_Inspection_Reader'Class) is
   begin
      CodePeer.Project_Data'Class
        (Self.Root_Inspection.all).Object_Races.Append (Self.Object_Race);
      Self.Object_Race :=
        (Name         => <>,
         Entry_Points => Entry_Point_Object_Access_Vectors.Empty_Vector,
         File         => GNATCOLL.VFS.No_File,
         Line         => 0,
         Column       => 0,
         Message      => null);
   end End_Object_Race;

   ---------------
   -- File_Node --
   ---------------

   function File_Node
     (Self : Base_Inspection_Reader'Class) return Code_Analysis.File_Access is
   begin
      return Self.File_Node;
   end File_Node;

   -------------------------------
   -- Get_Annotation_Categories --
   -------------------------------

   overriding function Get_Annotation_Categories
     (Self : Base_Inspection_Reader) return Annotation_Category_Maps.Map is
   begin
      return Self.Annotation_Categories;
   end Get_Annotation_Categories;

   ----------------------------
   -- Get_Code_Analysis_Tree --
   ----------------------------

   overriding function Get_Code_Analysis_Tree
     (Self : Base_Inspection_Reader) return Code_Analysis.Code_Analysis_Tree is
   begin
      return Self.Projects;
   end Get_Code_Analysis_Tree;

   -------------------------
   -- Get_Optional_Column --
   -------------------------

   function Get_Optional_Column
     (Attrs : Sax.Attributes.Attributes'Class) return Positive
   is
      Index : constant Integer := Attrs.Get_Index (Column_Attribute);

   begin
      if Index /= -1 then
         return Integer'Value (Attrs.Get_Value (Index));

      else
         return 1;
      end if;
   end Get_Optional_Column;

   -----------------------
   -- Get_Race_Category --
   -----------------------

   overriding function Get_Race_Category
     (Self : Base_Inspection_Reader) return CodePeer.Message_Category_Access is
   begin
      return Self.Race_Category;
   end Get_Race_Category;

   --------------------------
   -- Include_CWE_Category --
   --------------------------

   procedure Include_CWE_Category
     (Self : in out Base_Inspection_Reader'Class;
      Id   : CWE_Identifier;
      Name : Ada.Strings.Unbounded.Unbounded_String)
   is
      Aux : CWE_Category_Access;

   begin
      if not Self.CWE_Categories.Contains (Id) then
         Aux := new CWE_Category'(Identifier => Id, Name => Name);
         Self.CWE_Categories.Include (Id, Aux);
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).CWE_Categories.Include (Aux);
      end if;
   end Include_CWE_Category;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Base_Inspection_Reader'Class;
      Base_Directory  : GNATCOLL.VFS.Virtual_File;
      Root_Inspection : Code_Analysis.CodePeer_Data_Access;
      Messages        : access CodePeer.Message_Maps.Map)
   is
      Root_Project : Code_Analysis.Project_Access;

   begin
      Self.Base_Directory  := Base_Directory;
      Self.Root_Inspection := Root_Inspection;

      Self.Projects        := new Code_Analysis.Project_Maps.Map;
      Self.Messages        := Messages;
      Self.Message_Categories.Clear;
      Self.Messages.Clear;
      Root_Project :=
        Code_Analysis.Get_Or_Create
          (Self.Projects,
           GPS.Kernel.Project.Get_Project (Self.Kernel));
      Root_Project.Analysis_Data.CodePeer_Data := Self.Root_Inspection;

      Self.Object_Race :=
        (Name         => <>,
         Entry_Points => Entry_Point_Object_Access_Vectors.Empty_Vector,
         File         => GNATCOLL.VFS.No_File,
         Line         => 0,
         Column       => 0,
         Message      => null);
   end Initialize;

   -------------
   -- Message --
   -------------

   function Message
     (Self : Base_Inspection_Reader'Class) return CodePeer.Message_Access is
   begin
      return Self.Current_Message;
   end Message;

   -------------------------------
   -- Start_Annotation_Category --
   -------------------------------

   procedure Start_Annotation_Category
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      function Get_Vn return Natural;
      --  Returns value of "vn_id" attribute if specified, overwise returns 0.

      ------------
      -- Get_Vn --
      ------------

      function Get_Vn return Natural is
         Index : constant Integer := Attrs.Get_Index (Vn_Id_Attribute);

      begin
         if Index /= -1 then
            return Integer'Value (Attrs.Get_Value (Index));

         else
            return 0;
         end if;
      end Get_Vn;

      Annotation_Category : CodePeer.Annotation_Category_Access;

   begin
      Annotation_Category :=
        new CodePeer.Annotation_Category'
          (Order => Natural'Value (Attrs.Get_Value ("identifier")),
           Text  => Reader_Utilities.Get_Value (Attrs, "name"),
           Vn    => Get_Vn);
      CodePeer.Project_Data'Class
        (Self.Root_Inspection.all).Annotation_Categories.Insert
        (Annotation_Category);
      Self.Annotation_Categories.Insert
        (Natural'Value (Attrs.Get_Value ("identifier")),
         Annotation_Category);
   end Start_Annotation_Category;

   ------------------------
   -- Start_CWE_Category --
   ------------------------

   procedure Start_CWE_Category
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class) is
   begin
      Self.Include_CWE_Category
        (Id   =>
           CWE_Identifier'Value (Attrs.Get_Value (Identifier_Attribute)),
         Name =>
           Ada.Strings.Unbounded.To_Unbounded_String
             (Attrs.Get_Value (Name_Attribute)));
   end Start_CWE_Category;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self  : in out Base_Inspection_Reader;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class) is
   begin
      if Self.Ignore_Depth /= 0 then
         Self.Ignore_Depth := Self.Ignore_Depth + 1;

      elsif Name = CWE_Category_Tag then
         Self.Start_CWE_Category (Attrs);

      elsif Name = Message_Category_Tag then
         Self.Start_Message_Category (Attrs);

      elsif Name = Annotation_Category_Tag then
         Self.Start_Annotation_Category (Attrs);

      elsif Name = File_Tag then
         Self.Start_File (Attrs);

      elsif Name = Subprogram_Tag then
         Base_Inspection_Reader'Class (Self).Start_Subprogram (Attrs);

      elsif Name = Message_Tag then
         Base_Inspection_Reader'Class (Self).Start_Message (Attrs);

      elsif Name = Entry_Point_Tag then
         Self.Start_Entry_Point (Attrs);

      elsif Name = Object_Race_Tag then
         Self.Start_Object_Race (Attrs);

      elsif Name = Entry_Point_Access_Tag then
         Self.Start_Entry_Point_Access (Attrs);

      elsif Name = Object_Access_Tag then
         Self.Start_Object_Access (Attrs);

      else
         --  Activate ignore of nested XML elements to be able to load data
         --  files of newer version then supported by GPS.

         Self.Ignore_Depth := 1;
      end if;
   end Start_Element;

   -----------------------
   -- Start_Entry_Point --
   -----------------------

   procedure Start_Entry_Point
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Entry_Point : CodePeer.Entry_Point_Information_Access;

   begin
      Entry_Point :=
        new Entry_Point_Information'
          (Name   => Reader_Utilities.Get_Value (Attrs, Name_Attribute),
           File   =>
             GPS.Kernel.Create
               (+Attrs.Get_Value (File_Attribute), Self.Kernel),
           Line   => Integer'Value (Attrs.Get_Value (Line_Attribute)),
           Column => Get_Optional_Column (Attrs));
      CodePeer.Project_Data'Class
        (Self.Root_Inspection.all).Entry_Points.Insert (Entry_Point);
      Self.Entry_Point_Map.Insert
        (Integer'Value (Attrs.Get_Value ("identifier")), Entry_Point);
   end Start_Entry_Point;

   ------------------------------
   -- Start_Entry_Point_Access --
   ------------------------------

   procedure Start_Entry_Point_Access
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Entry_Point : CodePeer.Entry_Point_Information_Access;

   begin
      Entry_Point :=
        Self.Entry_Point_Map.Element
          (Integer'Value (Attrs.Get_Value (Entry_Point_Attribute)));
      Self.Object_Accesses :=
        (Entry_Point, CodePeer.Object_Access_Vectors.Empty_Vector);
      --  Entry point's object access information is added in the
      --  End_Element callback.
   end Start_Entry_Point_Access;

   ----------------
   -- Start_File --
   ----------------

   procedure Start_File
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is

      function Get_Checks return Natural;
      --  Returns value of "checks" attribiute if any, otherwise returns zero.

      function Get_Optional_Annotations return GNATCOLL.VFS.Virtual_File;
      --  Returns full name of the file spewcified by "annotations"  attribute,
      --  or No_File when it is not specified. Relative names are resolved to
      --  full names using directory name of the inspection data file.

      ----------------
      -- Get_Checks --
      ----------------

      function Get_Checks return Natural is
         Index : constant Integer := Attrs.Get_Index (Checks_Attribute);

      begin
         if Index = -1 then
            return 0;

         else
            return Natural'Value (Attrs.Get_Value (Index));
         end if;
      end Get_Checks;

      ------------------------------
      -- Get_Optional_Annotations --
      ------------------------------

      function Get_Optional_Annotations return GNATCOLL.VFS.Virtual_File is
         Index : constant Integer := Attrs.Get_Index (Annotations_Attribute);

      begin
         if Index /= -1 then
            return
              GNATCOLL.VFS.Create_From_Base
                (Filesystem_String (Attrs.Get_Value (Index)),
                 Self.Base_Directory.Full_Name.all);

         else
            return GNATCOLL.VFS.No_File;
         end if;
      end Get_Optional_Annotations;

      File_Name           : GNATCOLL.VFS.Virtual_File;
      Relocated_Name      : GNATCOLL.VFS.Virtual_File;
      Project_Node        : Code_Analysis.Project_Access;

   begin
      File_Name :=
        GPS.Kernel.Create (+Attrs.Get_Value ("name"), Self.Kernel);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.

      --  Try to find file with the same base name in the current project.
      --  Use this file instead of original name which comes from the
      --  database to be able to reuse database between several users.

      Relocated_Name := Self.Kernel.Create_From_Base (File_Name.Base_Name);

      if Relocated_Name.Is_Regular_File then
         File_Name := Relocated_Name;
      end if;

      declare
         F_Info : constant GNATCOLL.Projects.File_Info'Class :=
           GNATCOLL.Projects.File_Info'Class
             (Get_Registry (Self.Kernel).Tree.Info_Set (File_Name)
              .First_Element);
      begin
         Project_Node :=
           Code_Analysis.Get_Or_Create
             (Self.Projects,
              F_Info.Project);
      end;

      Self.File_Node :=
        Code_Analysis.Get_Or_Create (Project_Node, File_Name);
      Self.File_Node.Analysis_Data.CodePeer_Data :=
        new CodePeer.File_Data'
          (Lifeage            => Reader_Utilities.Get_Lifeage (Attrs),
           Total_Checks       => Get_Checks,
           Annotations_File   => Get_Optional_Annotations,
           Annotations_Loaded => False);
   end Start_File;

   -------------------
   -- Start_Message --
   -------------------

   procedure Start_Message
     (Self  : in out Base_Inspection_Reader;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      function Get_Rank return Message_Ranking_Level;
      --  Returns value of "rank" attribute. Handle old representation of Info
      --  literal ("INFORMATIONAL").

      function Get_Vns return Natural_Sets.Set;
      --  Returns value of "vn_ids" attribute if specified, overwise returns
      --  empty set.

      function Is_Check return Boolean;
      --  Returns value of "is_check" attribute is any, otherwise returns
      --  False.

      function Merged return Natural_Sets.Set;
      --  Returns set of merged messages as specified in 'merged_messages'
      --  attribute. Returns empty set when attribute is not specified.

      --------------
      -- Get_Rank --
      --------------

      function Get_Rank return Message_Ranking_Level is
         Image : constant String := Attrs.Get_Value (Rank_Attribute);

      begin
         if Image = "INFORMATIONAL" then
            return Info;

         else
            return Message_Ranking_Level'Value (Image);
         end if;
      end Get_Rank;

      -------------
      -- Get_Vns --
      -------------

      function Get_Vns return Natural_Sets.Set is
         Index : constant Integer := Attrs.Get_Index (Vn_Ids_Attribute);

      begin
         if Index /= -1 then
            declare
               List   : constant GNATCOLL.Utils.Unbounded_String_Array :=
                 GNATCOLL.Utils.Split (Attrs.Get_Value (Index), ' ');
               Result : Natural_Sets.Set;

            begin
               for Item of List loop
                  Result.Insert
                    (Integer'Value (Ada.Strings.Unbounded.To_String (Item)));
               end loop;

               return Result;
            end;

         else
            return Natural_Sets.Empty_Set;
         end if;
      end Get_Vns;

      --------------
      -- Is_Check --
      --------------

      function Is_Check return Boolean is
         Index : constant Integer := Attrs.Get_Index (Is_Check_Attribute);

      begin
         if Index = -1 then
            return False;

         else
            return Boolean'Value (Attrs.Get_Value (Index));
         end if;
      end Is_Check;

      ------------
      -- Merged --
      ------------

      function Merged return Natural_Sets.Set is
         Index  : constant Integer := Attrs.Get_Index ("merged_messages");
         Result : Natural_Sets.Set;

      begin
         if Index /= -1 then
            declare
               Value : constant String := Attrs.Get_Value (Index);
               First : Positive := Value'First;
               Last  : Positive := Value'First;

            begin
               loop
                  --  Skip spaces

                  while Last <= Value'Last loop
                     First := Last;

                     exit when Value (Last) /= ' ';

                     Last := Last + 1;
                  end loop;

                  --  Looking for identifier

                  while Last <= Value'Last loop
                     exit when Value (Last) = ' ';

                     Last := Last + 1;
                  end loop;

                  if First <= Value'Last and First < Last then
                     Result.Insert (Natural'Value (Value (First .. Last - 1)));
                  end if;

                  exit when Last > Value'Last;
               end loop;
            end;
         end if;

         return Result;
      end Merged;

      Checks      : Message_Category_Sets.Set;
      From_File   : GNATCOLL.VFS.Virtual_File;
      From_Line   : Positive := 1;
      From_Column : Positive := 1;
      CWEs        : CWE_Category_Sets.Set;

   begin
      --  Only primary checks need to be displayed.

      if Attrs.Get_Index (Primary_Checks_Attribute) /= -1 then
         declare
            Check_Ids : GNAT.Strings.String_List_Access :=
              GNATCOLL.Utils.Split
                (Attrs.Get_Value (Primary_Checks_Attribute),
                                   ' ',
                 True);

         begin
            for Index in Check_Ids'Range loop
               Checks.Insert
                 (Self.Message_Categories
                    (Natural'Value (Check_Ids (Index).all)));
            end loop;

            GNAT.Strings.Free (Check_Ids);
         end;
      end if;

      if Attrs.Get_Index ("from_file") /= -1 then
         From_File :=
           GPS.Kernel.Create
             (+Attrs.Get_Value ("from_file"), Self.Kernel);
         From_Line :=
           Positive'Value (Attrs.Get_Value ("from_line"));
         From_Column :=
           Positive'Value (Attrs.Get_Value ("from_column"));
      end if;

      Self.Update_CWE
        (CWEs,
         (if Attrs.Get_Index (CWE_Attribute) /= -1
          then Attrs.Get_Value (CWE_Attribute)
          else ""));

      Self.Current_Message :=
        CodePeer.Module.Create_CodePeer_Message
          (Id          =>
             Positive'Value (Attrs.Get_Value ("identifier")),
           File        => Self.File_Node,
           Subprogram  => Self.Subprogram_Name,
           Merged      => Merged,
           Lifeage     => Reader_Utilities.Get_Lifeage (Attrs),
           Line        => Positive'Value (Attrs.Get_Value ("line")),
           Column      => Positive'Value (Attrs.Get_Value ("column")),
           Category    =>
             Self.Message_Categories.Element
               (Positive'Value (Attrs.Get_Value (Category_Attribute))),
           Is_Check    => Is_Check,
           Ranking     => Get_Rank,
           Text        => Attrs.Get_Value ("text"),
           From_File   => From_File,
           From_Line   => From_Line,
           From_Column => From_Column,
           Checks      => Checks,
           Vns         => Get_Vns,
           CWEs        => CWEs);

      if Self.Messages.Contains (Self.Current_Message.Id) then
         Self.Kernel.Insert
           (Text   =>
              "CodePeer: duplicate message"
            & Natural'Image (Self.Current_Message.Id),
            Add_LF => False,
            Mode   => GPS.Kernel.Error);

      else
         Self.Messages.Insert
           (Self.Current_Message.Id, Self.Current_Message);
      end if;

      --  Append message's category to the list of corresponding
      --  categories.

      if Self.Current_Message.Is_Check then
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Check_Subcategories.Include
           (Self.Current_Message.Category);

      else
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Warning_Subcategories.Include
           (Self.Current_Message.Category);
      end if;
   end Start_Message;

   ----------------------------
   -- Start_Message_Category --
   ----------------------------

   procedure Start_Message_Category
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Message_Category : CodePeer.Message_Category_Access;

   begin
      Message_Category :=
        new CodePeer.Message_Category'
          (Name => Reader_Utilities.Get_Value (Attrs, "name"), CWEs => <>);

      if Attrs.Get_Index (Is_Check_Attribute) /= -1
        and then Boolean'Value (Attrs.Get_Value (Is_Check_Attribute))
      then
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Check_Subcategories.Include
           (Message_Category);
      end if;

      CodePeer.Project_Data'Class
        (Self.Root_Inspection.all).Message_Categories.Insert
        (Message_Category);
      Self.Message_Categories.Insert
        (Natural'Value (Attrs.Get_Value ("identifier")), Message_Category);

      Self.Update_CWE
        (Message_Category.CWEs,
         (if Attrs.Get_Index (CWE_Attribute) /= -1
          then Attrs.Get_Value (CWE_Attribute)
          else ""));
   end Start_Message_Category;

   -------------------------
   -- Start_Object_Access --
   -------------------------

   procedure Start_Object_Access
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Object_Access : CodePeer.Object_Access_Information;

   begin
      Object_Access :=
        (Kind    =>
           Object_Access_Kinds'Value (Attrs.Get_Value (Kind_Attribute)),
         File    =>
           GPS.Kernel.Create (+Attrs.Get_Value (File_Attribute), Self.Kernel),
         Line    => Natural'Value (Attrs.Get_Value (Line_Attribute)),
         Column  => Get_Optional_Column (Attrs),
         Message => null);
      Self.Object_Accesses.Object_Accesses.Append (Object_Access);
   end Start_Object_Access;

   -----------------------
   -- Start_Object_Race --
   -----------------------

   procedure Start_Object_Race
     (Self  : in out Base_Inspection_Reader'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      function Get_Optional_File return GNATCOLL.VFS.Virtual_File;
      --  Returns value of "file" attribute if specified; overwise returns
      --  No_File.

      function Get_Optional_Line return Natural;
      --  Returns value of "line" attribute if specified, overwise returns 0.

      -----------------------
      -- Get_Optional_File --
      -----------------------

      function Get_Optional_File return GNATCOLL.VFS.Virtual_File is
         Index : constant Integer := Attrs.Get_Index (File_Attribute);

      begin
         if Index /= -1 then
            return
              GPS.Kernel.Create
                (+Attrs.Get_Value (File_Attribute), Self.Kernel);

         else
            return GNATCOLL.VFS.No_File;
         end if;
      end Get_Optional_File;

      -----------------------
      -- Get_Optional_Line --
      -----------------------

      function Get_Optional_Line return Natural is
         Index : constant Integer := Attrs.Get_Index (Line_Attribute);

      begin
         if Index /= -1 then
            return Natural'Value (Attrs.Get_Value (Index));

         else
            return 0;
         end if;
      end Get_Optional_Line;

   begin
      if Self.Race_Category = null then
         Self.Race_Category :=
           new CodePeer.Message_Category'
             (Name =>
                Ada.Strings.Unbounded.To_Unbounded_String
                  (CodePeer.Module.Race_Condition_Category),
              CWEs => CodePeer.CWE_Category_Sets.Empty_Set);
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Warning_Subcategories.Include
           (Self.Race_Category);
      end if;

      Self.Object_Race.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Attrs.Get_Value (Name_Attribute));
      Self.Object_Race.File := Get_Optional_File;
      Self.Object_Race.Line := Get_Optional_Line;
      Self.Object_Race.Column := Get_Optional_Column (Attrs);
      --  Object race information is added to data in the End_Element
      --  callback.
   end Start_Object_Race;

   ---------------------
   -- Subprogram_Data --
   ---------------------

   function Subprogram_Data
     (Self : Base_Inspection_Reader'Class)
      return CodePeer.Subprogram_Data_Access is
   begin
      return
        (if Self.Subprogram_Node /= null
         then CodePeer.Subprogram_Data_Access
           (Self.Subprogram_Node.Analysis_Data.CodePeer_Data)
         else null);
   end Subprogram_Data;

   ---------------------
   -- Subprogram_Name --
   ---------------------

   function Subprogram_Name
     (Self : Base_Inspection_Reader'Class)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return
        (if Self.Subprogram_Node /= null
         then Ada.Strings.Unbounded.To_Unbounded_String
           (Self.Subprogram_Node.Name.all)
         else Ada.Strings.Unbounded.Null_Unbounded_String);
   end Subprogram_Name;

   ----------------
   -- Update_CWE --
   ----------------

   procedure Update_CWE
     (Self : in out Base_Inspection_Reader'Class;
      Set  : in out CWE_Category_Sets.Set;
      CWEs : String)
   is

      procedure Insert (Id : CWE_Identifier);
      --  Inserts CWE into set of CWEs

      ------------
      -- Insert --
      ------------

      procedure Insert (Id : CWE_Identifier) is
      begin
         Self.Include_CWE_Category
           (Id, Ada.Strings.Unbounded.Null_Unbounded_String);
         Set.Include (Self.CWE_Categories (Id));
      end Insert;

      CWE_Id : CWE_Identifier;
      First  : Positive := CWEs'First;

   begin
      for Current in CWEs'Range loop
         case CWEs (Current) is
            when '0' .. '9' =>
               null;

            when ' ' =>
               CWE_Id := CWE_Identifier'Value (CWEs (First .. Current - 1));
               First  := Current + 1;
               Insert (CWE_Id);

            when others =>
               raise Program_Error;
         end case;
      end loop;

      if First < CWEs'Last then
         CWE_Id := CWE_Identifier'Value (CWEs (First .. CWEs'Last));
         Insert (CWE_Id);
      end if;
   end Update_CWE;

end CodePeer.Bridge.Inspection_Readers.Base;
