-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2007-2008, AdaCore                  --
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

with Ada.Characters.Handling;               use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Glib.Convert; use Glib.Convert;

with GNAT.HTable;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Basic_Types;
with Commands;                  use Commands;
with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with File_Utils;
with Find_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Language;                  use Language;
with Language.Ada;
with Language.Tree;             use Language.Tree;
with Language_Handlers;         use Language_Handlers;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with Templates_Parser;          use Templates_Parser;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Docgen2_Backend;           use Docgen2_Backend;

package body Docgen2 is

   Me : constant Debug_Handle := Create ("Docgen");

   type Entity_Info_Category is
     (Cat_File,
      Cat_Package,
      Cat_Class,
      Cat_Task,
      Cat_Protected,
      Cat_Type,
      Cat_Variable,
      Cat_Parameter,
      Cat_Subprogram,
      Cat_Entry,
      Cat_Unknown);

   function Image (Cat : Entity_Info_Category) return String;
   function Image (Cat : Language_Category) return String;
   --  Returns a printable image of the category

   type Entity_Info_Record (Category : Entity_Info_Category := Cat_Unknown);
   type Entity_Info is access all Entity_Info_Record;

   type Location_Type is record
      File_Loc : File_Location;
      Pkg_Nb   : Natural;
   end record;

   Null_Location : constant Location_Type :=
                     (File_Loc => No_File_Location,
                      Pkg_Nb   => 0);

   type Cross_Ref_Record;
   type Cross_Ref is access all Cross_Ref_Record;

   type Cross_Ref_Record is record
      Location      : File_Location;
      Name          : GNAT.Strings.String_Access;
      Xref          : Entity_Info := null;
      Inherited     : Boolean := False; -- Primitive operation cross-ref
      Overriding_Op : Cross_Ref;        -- Primitive operation cross-ref
   end record;

   procedure Xref_Free (Xref : in out Cross_Ref);
   --  Free memory used by Xref

   package Cross_Ref_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Cross_Ref);

   package Entity_Info_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Entity_Info);

   package Entity_Ref_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Entity_Reference);

   package Files_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => GNATCOLL.VFS.Virtual_File);

   function Less_Than (Left, Right : Cross_Ref) return Boolean;
   function Less_Than (Left, Right : Entity_Info) return Boolean;
   function Less_Than (Left, Right : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Used to sort the children lists

   package Vector_Sort is new Cross_Ref_List.Generic_Sorting
     ("<" => Less_Than);

   package EInfo_Vector_Sort is new Entity_Info_List.Generic_Sorting
     ("<" => Less_Than);

   package Files_Vector_Sort is new Files_List.Generic_Sorting
     ("<" => Less_Than);

   procedure Free (List : in out Cross_Ref_List.Vector);
   procedure Free (List : in out Entity_Info_List.Vector);
   --  Free memory used by List

   type Entity_Info_Record (Category : Entity_Info_Category := Cat_Unknown)
      is record
         Lang_Category        : Language_Category;
         Name                 : GNAT.Strings.String_Access;
         Short_Name           : GNAT.Strings.String_Access;
         Description          : GNAT.Strings.String_Access;
         Printout             : GNAT.Strings.String_Access;
         Entity_Loc           : Source_Location;
         Printout_Loc         : Source_Location;
         Location             : Location_Type;
         Body_Location        : File_Location;
         Is_Abstract          : Boolean := False;
         Is_Private           : Boolean := False;
         Is_Generic           : Boolean := False;
         Generic_Params       : Entity_Info_List.Vector;
         Is_Renaming          : Boolean := False;
         Renamed_Entity       : Cross_Ref := null;
         Is_Instantiation     : Boolean := False;
         Instantiated_Entity  : Cross_Ref := null;
         Is_Partial           : Boolean := False;
         Full_Declaration     : Cross_Ref := null;
         Displayed            : Boolean := False;
         Children             : Entity_Info_List.Vector;
         References           : Entity_Ref_List.Vector;
         Calls                : Cross_Ref_List.Vector;
         Called               : Cross_Ref_List.Vector;

         case Category is
            when Cat_Package | Cat_File =>
               Language       : Language_Access;
               File           : Source_File;
               Pkg_Nb         : Natural;
            when Cat_Task | Cat_Protected =>
               Is_Type        : Boolean;
            when Cat_Class =>
               Parents        : Cross_Ref_List.Vector;
               Class_Children : Cross_Ref_List.Vector;
               Primitive_Ops  : Cross_Ref_List.Vector;
            when Cat_Variable =>
               Variable_Type  : Cross_Ref := null;
            when Cat_Parameter =>
               Parameter_Type : Cross_Ref := null;
            when Cat_Subprogram | Cat_Entry =>
               Return_Type    : Cross_Ref := null;
            when others =>
               null;
         end case;
      end record;

   procedure EInfo_Free (EInfo : in out Entity_Info);

   function Hash (Key : File_Location) return Ada.Containers.Hash_Type;
   function Equivalent_Keys (Left, Right : File_Location)
                             return Boolean;
   package Entity_Info_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (File_Location, Entity_Info, Hash, Equivalent_Keys);
   --  A hashed set of nodes, identified by their 'loc' attribute

   function To_Category (Category : Language_Category)
                         return Entity_Info_Category;
   --  Translate language category into entity_info_category

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Whether File is a spec file

   function Get_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language_Access) return Entity_Information;
   --  Retrieve the entity corresponding to construct, or create a new one.

   function Get_Declaration_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language_Access) return Entity_Information;
   --  Retrieve the entity declaration corresponding to construct.

   function Filter_Documentation
     (Doc     : String;
      Options : Docgen_Options) return String;
   --  Filters the doc according to the Options.

   procedure Ensure_Loc_Index
     (File_Buffer : GNAT.Strings.String_Access;
      Loc         : in out Source_Location);
   --  Ensures that loc.Index is properly set.

   procedure Set_Printout
     (Construct   : Simple_Construct_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info);
   --  Retrieve the Source extract representing the construct, and
   --  set the printout field of E_Info

   procedure Set_Pkg_Printout
     (Construct   : Simple_Construct_Information;
      Entity      : Entity_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info);
   --  Retrieve the Source extract representing the header of the package, or
   --  the full construct if the package is an instantiation or a renaming.

   type Context_Stack_Element is record
      Parent_Entity : Entity_Info;
      Pkg_Entity    : Entity_Info;
      Parent_Iter   : Construct_Tree_Iterator;
   end record;

   package Context_Stack is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Context_Stack_Element);

   type Comment is record
      Start_Loc : Source_Location;
      End_Loc   : Source_Location;
   end record;

   package Comments_List is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Comment);

   type Analysis_Context is record
      Stack       : Context_Stack.Vector;
      Iter        : Construct_Tree_Iterator;
      Tree        : Construct_Tree;
      File_Buffer : GNAT.Strings.String_Access;
      File        : Source_File;
      Language    : Language_Handler;
      Pkg_Nb      : Natural;
      Comments    : Comments_List.Vector;
   end record;

   procedure Push
     (Context : in out Analysis_Context;
      Elem    : Context_Stack_Element);
   procedure Pop (Context : in out Analysis_Context);
   function Current (Context : Analysis_Context) return Context_Stack_Element;
   --  Context stack manipulation

   --  Command --
   type Docgen_Command is new Commands.Root_Command with record
      Kernel          : Kernel_Handle;
      Backend         : Docgen2_Backend.Backend_Handle;
      Project         : Projects.Project_Type;
      Source_Files    : File_Array_Access;
      Annotated_Files : File_Array_Access := null;
      Xref_List       : Cross_Ref_List.Vector;
      EInfo_Map       : Entity_Info_Map.Map;
      Class_List      : Cross_Ref_List.Vector;
      Documentation   : Entity_Info_List.Vector;
      File_Index      : Natural;
      Src_File_Index  : Natural;
      Src_A_Files_Idx : Natural;
      Buffer          : GNAT.Strings.String_Access;
      Src_Files       : Files_List.Vector;
      Files           : Cross_Ref_List.Vector;
      Analysis_Ctxt   : Analysis_Context;
      Options         : Docgen_Options;
      Cursor          : Entity_Info_List.Cursor;
      Doc_Gen         : Boolean := False;
   end record;
   --  Command used for generating the documentation

   type Docgen_Command_Access is access all Docgen_Command'Class;

   function Name (Command : access Docgen_Command) return String;
   function Progress (Command : access Docgen_Command) return Progress_Record;
   function Execute (Command : access Docgen_Command)
                     return Command_Return_Type;
   --  See inherited for documentation

   procedure Analyse_Construct
     (Context     : in out Analysis_Context;
      Options     : Docgen_Options;
      Db          : Entities_Database;
      EInfo_Map   : in out Entity_Info_Map.Map;
      Xref_List   : in out Cross_Ref_List.Vector;
      Class_List  : in out Cross_Ref_List.Vector);

   procedure Generate_Support_Files
     (Kernel : access Kernel_Handle_Record'Class;
      Backend : Backend_Handle);
   --  Generate support files in destination directory

   procedure Generate_Xrefs
     (XRef_List : in out Cross_Ref_List.Vector;
      EInfo_Map : Entity_Info_Map.Map);
   --  Generate all missing links in Xref_List

   procedure Generate_TOC
     (Kernel     : access Kernel_Handle_Record'Class;
      Backend    : Backend_Handle;
      Files      : in out Cross_Ref_List.Vector);
   --  Generate the Table Of Contents

   procedure Generate_Files_Index
     (Kernel     : access Kernel_Handle_Record'Class;
      Backend    : Backend_Handle;
      Src_Files  : Files_List.Vector);
   --  Generate the global src files index

   procedure Generate_Trees
     (Kernel     : access Kernel_Handle_Record'Class;
      Backend    : Backend_Handle;
      Class_List : in out Cross_Ref_List.Vector;
      EInfo_Map  : Entity_Info_Map.Map);
   --  Generate the global inheritance trees

   procedure Generate_Global_Index
     (Kernel     : access Kernel_Handle_Record'Class;
      Backend    : Backend_Handle;
      EInfo_Map  : Entity_Info_Map.Map);
   --  Generate the global index

   procedure Generate_Annotated_Source
     (Kernel  : access Kernel_Handle_Record'Class;
      Backend : Backend_Handle;
      File    : Source_File;
      Buffer  : GNAT.Strings.String_Access;
      Lang    : Language_Access;
      Db      : Entities_Database;
      Xrefs   : Entity_Info_Map.Map);
   --  Generate hrefs and pretty print on a source file.

   procedure Generate_Doc
     (Kernel    : access Kernel_Handle_Record'Class;
      Backend   : Backend_Handle;
      Xrefs     : Entity_Info_Map.Map;
      Lang      : Language_Access;
      Db        : Entities_Database;
      File      : Source_File;
      Files     : in out Cross_Ref_List.Vector;
      Prj_Files : File_Array;
      E_Info    : Entity_Info);
   --  Generate the final documentation for the specified Entity_Info.
   --  E_Info's category must be Cat_File

   function Gen_Href
     (Backend : Backend_Handle;
      EInfo   : Entity_Info;
      Name    : String := "") return String;
   --  Generates a '<a href' tag, using Name to display or the entity's
   --  name if name is empty

   function Gen_Href
     (Backend : Backend_Handle;
      Xref    : Cross_Ref;
      Name    : String := "") return String;
   --  Same as above for Cross-Refs. If the cross-ref could not be found
   --  then only the name is displayed with no hyper-link

   function Location_Image (Loc : File_Location) return String;
   --  Return the location formated the gnat way: "file:line:col"

   function Get_Doc_Directory
     (Kernel : not null access Kernel_Handle_Record'Class) return String;
   --  Return the directory in which the documentation will be generated

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Cross_Ref_List.Vector) is
   begin
      for J in List.First_Index .. List.Last_Index loop
         List.Update_Element (J, Xref_Free'Access);
      end loop;

      Cross_Ref_List.Clear (List);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Entity_Info_List.Vector) is
   begin
      for J in List.First_Index .. List.Last_Index loop
         List.Update_Element (J, EInfo_Free'Access);
      end loop;

      Entity_Info_List.Clear (List);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Xref_Free (Xref : in out Cross_Ref) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Cross_Ref_Record, Cross_Ref);
   begin
      if Xref /= null then
         Free (Xref.Name);
         Internal (Xref);
      end if;
   end Xref_Free;

   ----------
   -- Free --
   ----------

   procedure EInfo_Free (EInfo : in out Entity_Info) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Entity_Info_Record, Entity_Info);
   begin
      if EInfo = null then
         return;
      end if;

      if EInfo.Short_Name /= EInfo.Name then
         Free (EInfo.Short_Name);
      end if;

      Free (EInfo.Name);
      Free (EInfo.Description);
      Free (EInfo.Printout);
      Free (EInfo.Generic_Params);
      Xref_Free (EInfo.Renamed_Entity);
      Xref_Free (EInfo.Instantiated_Entity);
      Xref_Free (EInfo.Full_Declaration);
      Free (EInfo.Children);
      Entity_Ref_List.Clear (EInfo.References);
      Cross_Ref_List.Clear (EInfo.Calls);
      Cross_Ref_List.Clear (EInfo.Called);

      case EInfo.Category is
         when Cat_Class =>
            Free (EInfo.Parents);
            Free (EInfo.Class_Children);
            Free (EInfo.Primitive_Ops);

         when Cat_Variable =>
            Xref_Free (EInfo.Variable_Type);

         when Cat_Parameter =>
            Xref_Free (EInfo.Parameter_Type);

         when Cat_Subprogram | Cat_Entry =>
            Xref_Free (EInfo.Return_Type);

         when others =>
            null;
      end case;

      Internal (EInfo);
   end EInfo_Free;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Cross_Ref) return Boolean is
   begin
      return To_Lower (Left.Name.all) < To_Lower (Right.Name.all);
   end Less_Than;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Entity_Info) return Boolean is
   begin
      return To_Lower (Left.Short_Name.all) < To_Lower (Right.Short_Name.all);
   end Less_Than;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Virtual_File) return Boolean is
   begin
      return To_Lower (Base_Name (Left)) < To_Lower (Base_Name (Right));
   end Less_Than;

   -----------
   -- Image --
   -----------

   function Image (Cat : Entity_Info_Category) return String is
   begin
      case Cat is
         when Cat_File =>
            return "file";
         when Cat_Package =>
            return "package";
         when Cat_Class =>
            return "class";
         when Cat_Task =>
            return "task";
         when Cat_Protected =>
            return "protected";
         when Cat_Type =>
            return "type";
         when Cat_Variable =>
            return "constant or variable";
         when Cat_Parameter =>
            return "parameter";
         when Cat_Subprogram =>
            return "subprogram";
         when Cat_Entry =>
            return "entry";
         when Cat_Unknown =>
            return "";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Cat : Language_Category) return String is
   begin
      case Cat is
         when Cat_Function =>
            return "function";
         when Cat_Procedure =>
            return "procedure";
         when others =>
            return Category_Name (Cat);
      end case;
   end Image;

   -----------------
   -- To_Category --
   -----------------

   function To_Category
     (Category : Language_Category) return Entity_Info_Category is
   begin
      case Category is
         when Cat_Package =>
            return Cat_Package;
         when Cat_Class =>
            return Cat_Class;
         when Cat_Task =>
            return Cat_Task;
         when Cat_Protected =>
            return Cat_Protected;
         when Cat_Entry =>
            return Cat_Entry;
         when Cat_Structure | Cat_Union | Cat_Type | Cat_Subtype =>
            return Cat_Type;
         when Cat_Variable =>
            return Cat_Variable;
         when Cat_Parameter =>
            return Cat_Parameter;
         when Cat_Procedure | Cat_Function | Cat_Method =>
            return Cat_Subprogram;
         when others =>
            return Cat_Unknown;
      end case;
   end To_Category;

   --------------
   -- Gen_Href --
   --------------

   function Gen_Href
     (Backend : Backend_Handle;
      EInfo   : Entity_Info;
      Name    : String := "") return String
   is
      Ref : constant String :=
              Backend.To_Href
                (Location_Image (EInfo.Location.File_Loc),
                 GNATCOLL.VFS.Base_Name
                   (Get_Filename (EInfo.Location.File_Loc.File)),
                 EInfo.Location.Pkg_Nb);
   begin
      if Name = "" then
         return Backend.Gen_Href
           (EInfo.Name.all, Ref,
            "defined at " & Location_Image (EInfo.Location.File_Loc));
      else
         return Backend.Gen_Href
           (Name, Ref,
            "defined at " & Location_Image (EInfo.Location.File_Loc));
      end if;
   end Gen_Href;

   --------------
   -- Gen_Href --
   --------------

   function Gen_Href
     (Backend : Backend_Handle;
      Xref    : Cross_Ref;
      Name    : String := "") return String
   is
   begin
      if Xref = null then
         return "";
      end if;

      if Xref.Xref /= null then
         if Name = "" then
            return Gen_Href (Backend, Xref.Xref, Xref.Name.all);
         else
            return Gen_Href (Backend, Xref.Xref, Name);
         end if;
      else
         if Name = "" then
            return Xref.Name.all;
         else
            return Name;
         end if;
      end if;
   end Gen_Href;

   --------------------
   -- Location_Image --
   --------------------

   function Location_Image (Loc : File_Location) return String is
      function Int_Img (I : Integer) return String;

      -------------
      -- Int_Img --
      -------------

      function Int_Img (I : Integer) return String is
         Str : constant String := Integer'Image (I);
      begin
         if Str (Str'First) = ' ' then
            return Str (Str'First + 1 .. Str'Last);
         else
            return Str;
         end if;
      end Int_Img;

   begin
      if Loc = No_File_Location then
         return "";
      end if;

      declare
         F_Name : constant String := Base_Name (Get_Filename (Get_File (Loc)));
      begin
         if F_Name = "<case_insensitive_predefined>"
           or else F_Name = "<case_sensitive_predefined>"
         then
            return "standard";
         end if;

         return F_Name & ":" &
           Int_Img (Get_Line (Loc)) & ":" &
           Int_Img (Integer (Get_Column (Loc)));
      end;
   end Location_Image;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Get_Unit_Part_From_Filename
        (Get_Project_From_File (Get_Registry (Kernel).all, File), File) =
        Unit_Spec;
   end Is_Spec_File;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language_Access) return Entity_Information
   is
      Entity        : Entity_Information;
      Current_Loc   : File_Location;
      pragma Unreferenced (Db);

   begin
      Current_Loc :=
        (File   => File,
         Line   => Loc.Line,
         Column => Basic_Types.Visible_Column_Type (Loc.Column));

      Entity := Get_Or_Create
        (Construct,
         File,
         Current_Loc.Line,
         Current_Loc.Column,
         Allow_Create => False);

      if Entity = null and then Get_Name (Lang) = "Ada" then
         for J in Construct'Range loop
            --  ??? Ada Specific ... should use language service
            --  Need to define it !
            if Construct (J) = '.' then
               Current_Loc.Column :=
                 Basic_Types.Visible_Column_Type
                   (Loc.Column + J + 1 - Construct'First);

               Entity := Get_Or_Create
                 (Construct (J + 1 .. Construct'Last),
                  File,
                  Current_Loc.Line,
                  Current_Loc.Column,
                  Allow_Create => False);

               exit when Entity /= null;
            end if;
         end loop;
      end if;

      --  Last chance, force creation of entity
      if Entity = null then
         Current_Loc :=
           (File   => File,
            Line   => Loc.Line,
            Column => Basic_Types.Visible_Column_Type (Loc.Column));

         Entity := Get_Or_Create
           (Construct,
            File,
            Current_Loc.Line,
            Current_Loc.Column);
      end if;

      return Entity;
   end Get_Entity;

   ----------------------------
   -- Get_Declaration_Entity --
   ----------------------------

   function Get_Declaration_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language_Access) return Entity_Information
   is
      Entity        : Entity_Information;
      Entity_Status : Find_Decl_Or_Body_Query_Status;
      Current_Loc   : File_Location;
   begin
      Current_Loc :=
        (File   => File,
         Line   => Loc.Line,
         Column => Basic_Types.Visible_Column_Type (Loc.Column));

      Find_Declaration
        (Db              => Db,
         File_Name       => Get_Filename (File),
         Entity_Name     => Construct,
         Line            => Current_Loc.Line,
         Column          => Current_Loc.Column,
         Entity          => Entity,
         Status          => Entity_Status,
         Check_Decl_Only => False);

      if Entity = null and then Get_Name (Lang) = "Ada" then
         for J in Construct'Range loop
            --  ??? Ada Specific ... should use language service
            --  Need to define it !
            if Construct (J) = '.' then
               Current_Loc.Column :=
                 Basic_Types.Visible_Column_Type
                   (Loc.Column + J + 1 - Construct'First);

               Find_Declaration
                 (Db,
                  File_Name       => Get_Filename (File),
                  Entity_Name     => Construct (J + 1 .. Construct'Last),
                  Line            => Current_Loc.Line,
                  Column          => Current_Loc.Column,
                  Entity          => Entity,
                  Status          => Entity_Status,
                  Check_Decl_Only => False);

               exit when Entity /= null;
            end if;
         end loop;
      end if;

      return Entity;
   end Get_Declaration_Entity;

   --------------------------
   -- Filter_Documentation --
   --------------------------

   function Filter_Documentation
     (Doc     : String;
      Options : Docgen_Options) return String
   is
      Matches : Match_Array (0 .. 0);
      use type GNAT.Expect.Pattern_Matcher_Access;
   begin
      if Options.Comments_Filter = null then
         return Doc;
      end if;

      Match (Options.Comments_Filter.all, Doc, Matches);

      if Matches (0) = No_Match then
         return Doc;
      end if;

      return Filter_Documentation
        (Doc (Doc'First .. Matches (0).First - 1) &
           Doc (Matches (0).Last + 1 .. Doc'Last),
         Options);
   end Filter_Documentation;

   ----------------------
   -- Ensure_Loc_Index --
   ----------------------

   procedure Ensure_Loc_Index
     (File_Buffer : GNAT.Strings.String_Access;
      Loc         : in out Source_Location)
   is
      Col, Line : Natural;
   begin
      if File_Buffer = null then
         return;
      end if;

      if Loc.Index >= File_Buffer'First then
         --  Nothing to do: location index is initialized
         return;
      end if;

      --  Case where index is not initialized in the construct
      --  This happens with the C++ parser, for example.
      if Loc.Column /= 0 then
         Col := 1;
         Line := 1;

         for J in File_Buffer'Range loop

            if File_Buffer (J) = ASCII.LF then
               Line := Line + 1;
               Col := 0;
            elsif File_Buffer (J) /= ASCII.CR then
               --  ??? what about utf-8 characters ?
               Col := Col + 1;
            end if;

            if Line = Loc.Line and then Col = Loc.Column then
               Loc.Index := J;
               exit;
            end if;

         end loop;
      end if;
   end Ensure_Loc_Index;

   ------------------
   -- Set_Printout --
   ------------------

   procedure Set_Printout
     (Construct   : Simple_Construct_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info)
   is
      Sloc_Start, Sloc_End : Source_Location;
   begin
      Sloc_Start := Construct.Sloc_Start;
      Sloc_End   := Construct.Sloc_End;

      Ensure_Loc_Index (File_Buffer, Sloc_Start);
      Ensure_Loc_Index (File_Buffer, Sloc_End);

      E_Info.Printout_Loc := Construct.Sloc_Start;
      E_Info.Printout := new String'
        (File_Buffer
           (Sloc_Start.Index .. Sloc_End.Index));
   end Set_Printout;

   ----------------------
   -- Set_Pkg_Printout --
   ----------------------

   procedure Set_Pkg_Printout
     (Construct   : Simple_Construct_Information;
      Entity      : Entity_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info)
   is
      Start_Index : Natural;
      End_Index   : Natural;
      Pkg_Found   : Boolean;
      Entity_Kind : constant E_Kind := Get_Kind (Entity);

      function Is_Token
        (Token : String; Start_Index : Natural) return Boolean;
      --  Test if Token is found at index Start_Index

      --------------
      -- Is_Token --
      --------------

      function Is_Token
        (Token : String; Start_Index : Natural) return Boolean is
      begin
         return Start_Index >= File_Buffer'First
           and then Start_Index + Token'Length <= File_Buffer'Last
           and then Is_Blank (File_Buffer (Start_Index + Token'Length))
           and then (Start_Index = File_Buffer'First
                     or else Is_Blank (File_Buffer (Start_Index - 1)))
           and then Equal
             (File_Buffer (Start_Index .. Start_Index + Token'Length - 1),
              Token,
              Case_Sensitive => False);
      end Is_Token;

   begin
      --  We assume Index is initialized, as the Ada parser does so.
      Start_Index := Construct.Sloc_Start.Index;
      End_Index := Construct.Sloc_Start.Index - 1;

      if Entity_Kind.Is_Generic then
         --  Look for 'generic' beforee Sloc_Start.Index
         for J in reverse File_Buffer'First ..
           Construct.Sloc_Start.Index - 6
         loop
            if Is_Token ("generic", J) then
               Start_Index := J;
               exit;
            end if;
         end loop;
      end if;

      --  If we have an instantiation or a renaming, then output the full
      --  printout
      if Is_Instantiation_Of (Entity) /= null
        or else Renaming_Of (Entity) /= null
      then
         End_Index := Construct.Sloc_End.Index;

      else
         --  We will stop after 'package XXX is'
         Pkg_Found := False;

         for J in Construct.Sloc_Start.Index .. Construct.Sloc_End.Index loop
            if not Pkg_Found and then Is_Token ("package", J) then
               Pkg_Found := True;
            end if;

            if Pkg_Found then
               --  After package keywork, expect a ' is '
               --  or a '; '
               if File_Buffer (J) = ';' then
                  End_Index := J;
                  exit;

               elsif Is_Token ("is", J) then
                  End_Index := J + 1;
                  exit;

               end if;
            end if;
         end loop;
      end if;

      E_Info.Printout := new String'(File_Buffer (Start_Index .. End_Index));

      if Start_Index /= Construct.Sloc_Start.Index then
         declare
            Line    : Natural := 1;
            Col     : Basic_Types.Character_Offset_Type := 1;
            V_Col   : Basic_Types.Visible_Column_Type := 1;
            L_Start : Natural := 1;
         begin
            Find_Utils.To_Line_Column
              (Buffer         => File_Buffer.all,
               Pos            => Start_Index,
               Line           => Line,
               Column         => Col,
               Visible_Column => V_Col,
               Line_Start     => L_Start);
            E_Info.Printout_Loc :=
              (Line   => Line,
               Column => Natural (Col),
               Index  => Start_Index);
         end;
      else
         E_Info.Printout_Loc := Construct.Sloc_Start;
      end if;
   end Set_Pkg_Printout;

   ----------
   -- Push --
   ----------

   procedure Push
     (Context : in out Analysis_Context;
      Elem    : Context_Stack_Element) is
   begin
      Context.Stack.Append (Elem);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Context : in out Analysis_Context) is
      Elem : Context_Stack_Element;
   begin
      if not Context.Stack.Is_Empty then
         Elem := Current (Context);
         Context.Iter := Next
           (Context.Tree, Elem.Parent_Iter, Jump_Over);
         Context.Stack.Delete_Last (Count => 1);
      end if;
   end Pop;

   -------------
   -- Current --
   -------------

   function Current (Context : Analysis_Context) return Context_Stack_Element
   is
   begin
      return Context.Stack.Last_Element;
   end Current;

   -----------------------
   -- Analyse_Construct --
   -----------------------

   procedure Analyse_Construct
     (Context     : in out Analysis_Context;
      Options     : Docgen_Options;
      Db          : Entities_Database;
      EInfo_Map   : in out Entity_Info_Map.Map;
      Xref_List   : in out Cross_Ref_List.Vector;
      Class_List  : in out Cross_Ref_List.Vector)
   is
      Construct   : access Simple_Construct_Information;
      Entity      : Entity_Information := null;
      E_Info      : Entity_Info;
      Entity_Kind : E_Kind;
      Lang        : constant Language_Access :=
                      Get_Language_From_File
                        (Context.Language, Get_Filename (Context.File));
      Body_Location : File_Location;
      Context_Elem  : constant Context_Stack_Element := Current (Context);

      function Create_Xref
        (E             : Entity_Information;
         Use_Full_Name : Boolean := False)
         return Cross_Ref;
      function Create_Xref
        (Name : String; Loc : File_Location) return Cross_Ref;
      --  Create a new Cross-Ref and update the Cross-Refs list

      function Create_EInfo
        (Cat : Language_Category;
         Loc : File_Location) return Entity_Info;
      --  Create a new Entity Info and update the Entity info list

      function Find_Doc (Sloc_Start, Sloc_End : Source_Location) return String;
      --  Retrieve documentation comments just before Sloc_Start or just
      --  after Sloc_End

      -----------------
      -- Create_Xref --
      -----------------

      function Create_Xref
        (Name : String; Loc : File_Location) return Cross_Ref
      is
         Xref : Cross_Ref;
      begin
         if Loc = No_File_Location then
            return null;
         end if;

         Xref := new Cross_Ref_Record'
           (Name         => new String'(Name),
            Location     => Loc,
            Xref         => null,
            Inherited    => False,
            Overriding_Op => null);

         Xref_List.Append (Xref);

         return Xref;
      end Create_Xref;

      -----------------
      -- Create_Xref --
      -----------------

      function Create_Xref
        (E             : Entity_Information;
         Use_Full_Name : Boolean := False)
         return Cross_Ref is
      begin
         if E = null then
            return null;
         end if;

         --  If the cross ref is in the same file, use a simple name
         --  If in another file, then use the fully qualified name
         if Get_Declaration_Of (E).File = Get_Declaration_Of (Entity).File
           and then not Use_Full_Name
         then
            return Create_Xref (Get_Name (E).all, Get_Declaration_Of (E));
         else
            return Create_Xref (Get_Full_Name (E), Get_Declaration_Of (E));
         end if;
      end Create_Xref;

      ------------------
      -- Create_EInfo --
      ------------------

      function Create_EInfo
        (Cat : Language_Category;
         Loc : File_Location) return Entity_Info
      is
         E_Info : Entity_Info;

      begin
         E_Info := new Entity_Info_Record (Category => To_Category (Cat));
         E_Info.Lang_Category := Cat;

         if Loc /= No_File_Location then
            if Context_Elem.Pkg_Entity /= null then
               E_Info.Location := (File_Loc => Loc,
                                   Pkg_Nb   => Context_Elem.Pkg_Entity.Pkg_Nb);
            else
               E_Info.Location := (File_Loc => Loc, Pkg_Nb => 1);
            end if;

            EInfo_Map.Include (Loc, E_Info);
         else
            E_Info.Location := Null_Location;
         end if;

         if E_Info.Category = Cat_Class then
            --  Class list is global for all tagged types. Use the full name.
            Class_List.Append
              (Create_Xref
                 (Get_Full_Name (Entity), Get_Declaration_Of (Entity)));
         end if;

         return E_Info;
      end Create_EInfo;

      --------------
      -- Find_Doc --
      --------------

      function Find_Doc (Sloc_Start, Sloc_End : Source_Location) return String
      is
         Index : Natural;
      begin
         Index := Context.Comments.First_Index;

         while Index <= Context.Comments.Last_Index loop
            if Sloc_Start.Line - 1 =
              Context.Comments.Element (Index).End_Loc.Line
            then
               return Filter_Documentation
                 (Comment_Block
                    (Lang,
                     Context.File_Buffer
                       (Context.Comments.Element (Index).Start_Loc.Index ..
                          Context.Comments.Element (Index).End_Loc.Index),
                     Comment => False,
                     Clean   => True),
                  Options);

            elsif Sloc_End.Line + 1 =
              Context.Comments.Element (Index).Start_Loc.Line
            then
               return Filter_Documentation
                 (Comment_Block
                    (Lang,
                     Context.File_Buffer
                       (Context.Comments.Element (Index).Start_Loc.Index ..
                          Context.Comments.Element (Index).End_Loc.Index),
                     Comment => False,
                     Clean   => True),
                  Options);
            end if;

            exit when Context.Comments.Element (Index).Start_Loc > Sloc_End;

            Index := Index + 1;
         end loop;

         return "";
      end Find_Doc;

   begin
      --  Exit when no more construct is available
      if Context.Iter = Null_Construct_Tree_Iterator then
         return;
      end if;

      --  If scope has changed, pop the context and return
      if Context_Elem.Parent_Iter /= Null_Construct_Tree_Iterator
        and then Get_Parent_Scope (Context.Tree, Context.Iter) /=
          Context_Elem.Parent_Iter
      then
         Pop (Context);
         return;
      end if;

      Construct := Get_Construct (Context.Iter);

      --  Ignore the private part
      if Construct.Visibility = Visibility_Private
        and then not Options.Show_Private
      then
         Pop (Context);
         return;
      end if;

      --  Ignoring with, use clauses
      --  Ignoring parameters, directly handled in subprogram nodes
      --  Ignoring represenation clauses.
      --  Ignoring constructs whose category is Unknown
      --  Ignoring unnamed entities.
      if Construct.Category not in Dependency_Category
        and then Construct.Category /= Cat_Representation_Clause
        and then Construct.Category /= Cat_Namespace
        and then Category_Name (Construct.Category) /= ""
        and then Construct.Name /= null
      then
         Entity := Get_Entity
           (Construct.Name.all, Construct.Sloc_Entity, Context.File, Db, Lang);

         if Entity /= null then
            Entity_Kind := Get_Kind (Entity);

            E_Info := Create_EInfo
              (Construct.Category, Get_Declaration_Of (Entity));
         else
            E_Info := Create_EInfo
              (Construct.Category,
               (File   => Context.File,
                Line   => Construct.Sloc_Entity.Line,
                Column => Basic_Types.Visible_Column_Type
                            (Construct.Sloc_Entity.Column)));
         end if;

         Context_Elem.Parent_Entity.Children.Append (E_Info);
         E_Info.Entity_Loc := Construct.Sloc_Entity;
         E_Info.Body_Location := No_File_Location;

         --  First set values common to all categories

         --  Set Name
         if Entity /= null then
            if Construct.Category = Cat_Package then
               --  Packages receive fully qualified names
               E_Info.Name := new String'(Get_Full_Name (Entity));
            else
               E_Info.Name := new String'(Get_Name (Entity).all);
            end if;

            E_Info.Short_Name := new String'(Get_Name (Entity).all);

            if Is_Container (Get_Kind (Entity).Kind) then
               Find_Next_Body
                 (Entity   => Entity,
                  Location => E_Info.Body_Location);
            end if;

         else
            E_Info.Name := new String'(Construct.Name.all);
            E_Info.Short_Name := new String'(Construct.Name.all);
         end if;

         --  Retrieve documentation comments
         declare
            Doc : constant String :=
                    Find_Doc (Construct.Sloc_Start, Construct.Sloc_End);
         begin
            if Doc /= "" then
               E_Info.Description := new String'(Doc);
            end if;
         end;

         --  Retrieve printout

         if Construct.Category not in Namespace_Category then
            Set_Printout (Construct.all, Context.File_Buffer, E_Info);

         elsif Construct.Category = Cat_Package and then Entity /= null then
            Set_Pkg_Printout
              (Construct.all, Entity, Context.File_Buffer, E_Info);
         end if;

         E_Info.Is_Private := Construct.Visibility = Visibility_Private;

      end if;

      if Entity /= null then
         --  Retrieve references

         declare
            Iter       : Entity_Reference_Iterator;
            Ref        : Entity_Reference;
            Calls_Iter : Calls_Iterator;
            Called_E   : Entity_Information;
            Caller_E   : Entity_Information;
         begin
            if Options.References
              and then Construct.Category /= Cat_Package
            then
               Find_All_References (Iter, Entity);

               while not At_End (Iter) loop
                  Ref := Get (Iter);

                  if Ref /= No_Entity_Reference then
                     E_Info.References.Append (Ref);
                     Caller_E := Get_Caller (Ref);

                     if Caller_E /= null
                       and then Is_Subprogram (Caller_E)
                     then
                        E_Info.Called.Append (Create_Xref (Caller_E));
                     end if;
                  end if;

                  Next (Iter);
               end loop;

               Calls_Iter := Get_All_Called_Entities (Entity);

               while not At_End (Calls_Iter) loop
                  Called_E := Get (Calls_Iter);

                  if Called_E /= null
                    and then Is_Subprogram (Called_E)
                  then
                     E_Info.Calls.Append (Create_Xref (Called_E));
                  end if;

                  Next (Calls_Iter);
               end loop;
            end if;
         end;

         E_Info.Is_Abstract := Entity_Kind.Is_Abstract;
         E_Info.Is_Generic := Entity_Kind.Is_Generic;
         E_Info.Is_Renaming := Renaming_Of (Entity) /= null;
         E_Info.Is_Instantiation := Is_Instantiation_Of (Entity) /= null;

         --  Init common children

         --  generic parameters:
         if E_Info.Is_Generic then
            declare
               Iter         : Generic_Iterator;
               Param_Entity : Entity_Information;
               Param_EInfo  : Entity_Info;
            begin
               Iter := Get_Generic_Parameters (Entity);

               loop
                  Get (Iter, Param_Entity);

                  exit when Param_Entity = null;

                  Param_EInfo := Create_EInfo
                    (Cat_Parameter,
                     Get_Declaration_Of (Param_Entity));
                  Param_EInfo.Name := new String'(Get_Name (Param_Entity).all);

                  declare
                     Doc : constant String :=
                             Find_Doc
                               (Param_EInfo.Entity_Loc,
                                Param_EInfo.Entity_Loc);
                  begin
                     if Doc /= "" then
                        Param_EInfo.Description := new String'(Doc);
                     end if;
                  end;

                  --  ??? need a better handling of formal parameters types
                  Param_EInfo.Parameter_Type := new Cross_Ref_Record'
                    (Name         => new String'
                                    (Kind_To_String (Get_Kind (Param_Entity))),
                     Location     => No_File_Location,
                     Xref         => null,
                     Inherited    => False,
                     Overriding_Op => null);

                  E_Info.Generic_Params.Append (Param_EInfo);

                  Next (Iter);
               end loop;
            end;
         end if;

         --  Renamed entity
         if E_Info.Is_Renaming then
            declare
               Renamed_Entity : constant Entity_Information :=
                                  Renaming_Of (Entity);
            begin
               E_Info.Renamed_Entity := Create_Xref (Renamed_Entity);
            end;
         end if;

         --  Entity is an instantiation
         if E_Info.Is_Instantiation then
            declare
               Instantiated_Entity : constant Entity_Information :=
                                       Is_Instantiation_Of (Entity);
            begin
               E_Info.Instantiated_Entity := Create_Xref (Instantiated_Entity);
            end;
         end if;

         --  Is the entity a partial declaration ?
         Body_Location := No_File_Location;

         Find_Next_Body
           (Entity   => Entity,
            Location => Body_Location);

         if Body_Location /= No_File_Location
           and then Body_Location /= E_Info.Location.File_Loc
           and then Body_Location.File = Context.File
         then
            E_Info.Is_Partial := True;
            E_Info.Full_Declaration :=
              Create_Xref (E_Info.Name.all, Body_Location);
         end if;

         --  Initialize now category specific parameters
         case E_Info.Category is
            when Cat_Package =>
               if not E_Info.Is_Instantiation
                 and then not E_Info.Is_Renaming
               then
                  --  Bump pkg nb
                  Context.Pkg_Nb := Context.Pkg_Nb + 1;
                  E_Info.Pkg_Nb := Context.Pkg_Nb;
                  E_Info.Location.Pkg_Nb := Context.Pkg_Nb;

                  if E_Info.Is_Generic then
                     declare
                        Cursor : Entity_Info_List.Cursor;
                     begin
                        Cursor := Entity_Info_List.First
                          (E_Info.Generic_Params);

                        while Entity_Info_List.Has_Element (Cursor) loop
                           Entity_Info_List.Element (Cursor).Location.Pkg_Nb :=
                             E_Info.Pkg_Nb;
                           Entity_Info_List.Next (Cursor);
                        end loop;
                     end;
                  end if;

                  --  Get children
                  declare
                     New_Context : constant Context_Stack_Element :=
                                     (Parent_Entity => E_Info,
                                      Pkg_Entity    => E_Info,
                                      Parent_Iter   => Context.Iter);
                  begin
                     Push (Context, New_Context);
                     Context.Iter := Next
                       (Context.Tree, Context.Iter, Jump_Into);
                     return;
                  end;
               end if;

            when Cat_Task | Cat_Protected =>
               E_Info.Is_Type := Entity_Kind.Is_Type;

               --  Get children (task entries)

               declare
                  New_Context : constant Context_Stack_Element :=
                                  (Parent_Entity => E_Info,
                                   Pkg_Entity    => Context_Elem.Pkg_Entity,
                                   Parent_Iter   => Context.Iter);
               begin
                  Push (Context, New_Context);
                  Context.Iter := Next
                    (Context.Tree, Context.Iter, Jump_Into);
                  return;
               end;

            when Cat_Class =>
               --  Get parents
               declare
                  Parents : constant Entity_Information_Array :=
                              Get_Parent_Types (Entity, Recursive => True);
               begin
                  for J in Parents'Range loop
                     E_Info.Parents.Append (Create_Xref (Parents (J)));
                  end loop;
               end;

               --  Get known children
               declare
                  Children : Children_Iterator := Get_Child_Types
                    (Entity, Recursive => False, Update_Xref => False);
               begin
                  while not At_End (Children) loop
                     if Get (Children) /= null then
                        E_Info.Class_Children.Append
                          (Create_Xref (Get (Children), True));
                     end if;

                     Next (Children);
                  end loop;

                  Destroy (Children);
               end;

               --  Get primitive operations
               declare
                  Iter, Iter_First : Primitive_Operations_Iterator;
                  E_Overriden      : Entity_Information;
                  Op_Xref          : Cross_Ref;
                  Nb_Primop        : Natural;

               begin
                  Find_All_Primitive_Operations (Iter, Entity, True);

                  Iter_First := Iter;

                  Nb_Primop := 0;

                  while not At_End (Iter) loop
                     Nb_Primop := Nb_Primop + 1;
                     Next (Iter);
                  end loop;

                  declare
                     type Primop_Info is record
                        Entity    : Entity_Information;
                        Overriden : Boolean;
                     end record;

                     List : array (1 .. Nb_Primop) of Primop_Info;

                  begin
                     Iter := Iter_First;

                     --  Init the list of primitive operations
                     for J in List'Range loop
                        List (J) := (Get (Iter), False);
                        Next (Iter);
                     end loop;

                     --  Search for overriden operations
                     for J in List'Range loop
                        E_Overriden := Overriden_Entity (List (J).Entity);

                        for K in J + 1 .. List'Last loop
                           if E_Overriden = List (K).Entity then
                              List (K).Overriden := True;
                           end if;
                        end loop;
                     end loop;

                     --  Insert non overriden operations in the xref list.
                     for J in List'Range loop
                        if not List (J).Overriden then
                           Op_Xref := Create_Xref  (List (J).Entity);

                           Op_Xref.Inherited :=
                             Is_Primitive_Operation_Of (List (J).Entity) /=
                             Entity;

                           E_Overriden := Overriden_Entity (List (J).Entity);

                           if E_Overriden /= null then
                              Op_Xref.Overriding_Op :=
                                Create_Xref (E_Overriden);
                           end if;

                           E_Info.Primitive_Ops.Append (Op_Xref);
                        end if;
                     end loop;
                  end;
               end;

            when Cat_Variable =>
               --  Get its type
               declare
                  Type_Entity : Entity_Information;
               begin
                  Type_Entity := Get_Type_Of (Entity);

                  if Type_Entity = null then
                     Type_Entity := Pointed_Type (Entity);
                  end if;

                  if Type_Entity = null then
                     Type_Entity := Get_Variable_Type (Entity);
                  end if;

                  if Type_Entity = null then
                     Type_Entity := Get_Returned_Type (Entity);
                  end if;

                  if Type_Entity /= null then
                     E_Info.Variable_Type := Create_Xref (Type_Entity);
                  end if;
               end;

            when Cat_Parameter =>
               declare
                  Type_Entity : Entity_Information;
               begin
                  Type_Entity := Get_Type_Of (Entity);

                  if Type_Entity = null then
                     Type_Entity := Pointed_Type (Entity);
                  end if;

                  if Type_Entity /= null then
                     E_Info.Parameter_Type := Create_Xref (Type_Entity);
                  else
                     --  don't know the type, let's use the printout as name
                     E_Info.Parameter_Type :=
                       new Cross_Ref_Record'
                         (Name          => new String'
                              (Context.File_Buffer
                                   (Construct.Sloc_Start.Index +
                                        Construct.Name.all'Length ..
                                          Construct.Sloc_End.Index)),
                          Location      => No_File_Location,
                          Xref          => null,
                          Inherited     => False,
                          Overriding_Op => null);
                  end if;
               end;

            when Cat_Subprogram | Cat_Entry =>
               --  Get the parameters and return type

               --  Parameters
               declare
                  New_Context : constant Context_Stack_Element :=
                                  (Parent_Entity => E_Info,
                                   Pkg_Entity    => Context_Elem.Pkg_Entity,
                                   Parent_Iter   => Context.Iter);
                  Type_Entity : Entity_Information;
               begin
                  --  Return type
                  Type_Entity := Get_Returned_Type (Entity);

                  if Type_Entity /= null then
                     E_Info.Return_Type := Create_Xref (Type_Entity);
                  end if;

                  Push (Context, New_Context);
                  Context.Iter := Next
                    (Context.Tree, Context.Iter, Jump_Into);
                  return;
               end;

            when others =>
               --  ??? Todo:
               --  Cat_Constructor (idem)
               --  Cat_Destructor (idem)
               --  Cat_Local_Variable (should not require anything, since we
               --   are evaluating specs)
               --  Cat_Field (do we want to detail a record's field ?)
               --  Cat_Literal (do we want to output anything for a literal ?)
               null;
         end case;
      end if;

      Context.Iter := Next
        (Context.Tree, Context.Iter, Jump_Over);
   end Analyse_Construct;

   ----------
   -- Hash --
   ----------

   function Hash (Key : File_Location) return Ada.Containers.Hash_Type is
      type Internal_Hash_Type is range 0 .. 2 ** 31 - 1;
      function Internal is new GNAT.HTable.Hash
        (Header_Num => Internal_Hash_Type);
   begin
      return Ada.Containers.Hash_Type
        (Internal
           (GNATCOLL.VFS.Full_Name (Get_Filename (Key.File)).all &
            Natural'Image (Key.Line) &
            Basic_Types.Visible_Column_Type'Image (Key.Column)));
   end Hash;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Keys (Left, Right : File_Location)
                             return Boolean is
      use Basic_Types;
   begin
      return Left.File = Right.File
        and then Left.Line = Right.Line
        and then Left.Column = Right.Column;
   end Equivalent_Keys;

   ----------
   -- Name --
   ----------

   function Name (Command : access Docgen_Command) return String is
      pragma Unreferenced (Command);
   begin
      return "Documentation generator";
   end Name;

   --------------
   -- Progress --
   --------------

   function Progress (Command : access Docgen_Command) return Progress_Record
   is
   begin
      if Command.File_Index < Command.Source_Files'Last then
         return (Activity => Running,
                 Current  => Command.File_Index,
                 Total    => Command.Source_Files'Length * 3);
      elsif Command.Src_File_Index < Command.Source_Files'Last then
         return (Activity => Running,
                 Current  => Command.Source_Files'Length +
                               Command.Src_File_Index,
                 Total    => Command.Source_Files'Length * 3);
      else
         return (Activity => Running,
                 Current  => Command.Source_Files'Length * 2 +
                              Entity_Info_List.To_Index (Command.Cursor),
                 Total    => Command.Source_Files'Length * 2 +
                              Natural (Command.Documentation.Length));
      end if;
   end Progress;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Docgen_Command) return Command_Return_Type
   is
      function Get_All_Comments
        (Lang   : Language_Access;
         Buffer : String) return Comments_List.Vector;
      --  Retrieve all comment blocks from a file

      ----------------------
      -- Get_All_Comments --
      ----------------------

      function Get_All_Comments
        (Lang   : Language_Access;
         Buffer : String) return Comments_List.Vector
      is
         Comments : Comments_List.Vector;
         Last_Entity : Language_Entity := Normal_Text;

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  Callback used when parsing the file.

         --------
         -- CB --
         --------

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);
            Elem : Comment;
         begin
            --  If last entity was a comment, then set its end to new
            --  non-comment entity location - 1
            if Entity = Comment_Text then
               if Last_Entity = Comment_Text then
                  Elem := Comments.Last_Element;
                  Elem.End_Loc := Sloc_End;
                  Comments.Replace_Element (Comments.Last_Index, Elem);

               else
                  Comments.Append
                    ((Start_Loc => Sloc_Start,
                      End_Loc   => Sloc_End));
               end if;
            end if;

            Last_Entity := Entity;

            return False;

         exception
            when E : others =>
               Trace (Exception_Handle, E);
               return True;
         end CB;

      begin
         Parse_Entities (Lang, Buffer, CB'Unrestricted_Access);
         return Comments;
      end Get_All_Comments;

      File_EInfo    : Entity_Info;
      File_Buffer   : GNAT.Strings.String_Access;
      Comment_End   : Integer;
      Database      : constant Entities_Database :=
                        Get_Database (Command.Kernel);
      Lang_Handler  : constant Language_Handler :=
                        Get_Language_Handler (Command.Kernel);
      Lang          : Language_Access;
      File          : Source_File;
      use type Ada.Containers.Count_Type;
   begin
      --  Freeze the database to speed-up the processing of the cross
      --  references. The database has just been updated before the command
      --  has been launched.
      Freeze (Database);

      if Command.Analysis_Ctxt.Iter /= Null_Construct_Tree_Iterator then
         --  Analysis of a new construct of the current file.
         Analyse_Construct
           (Command.Analysis_Ctxt, Command.Options,
            Db          => Get_Database (Command.Kernel),
            EInfo_Map   => Command.EInfo_Map,
            Xref_List   => Command.Xref_List,
            Class_List  => Command.Class_List);

      elsif Command.File_Index < Command.Source_Files'Last then
         --  Current file has been analysed, Let's start the next one

         if Command.File_Index < Command.Source_Files'First then
            --  Very first file analysed. Let's first create the destination
            --  directory with support files
            Generate_Support_Files (Command.Kernel, Command.Backend);
         end if;

         --  Clean-up previous context if needed
         Free (Command.Analysis_Ctxt.Tree);
         Free (Command.Analysis_Ctxt.File_Buffer);

         Command.File_Index := Command.File_Index + 1;

         --  Only analyze specs
         if Is_Spec_File (Command.Kernel,
                          Command.Source_Files (Command.File_Index))
         then
            if not Command.Source_Files
                     (Command.File_Index).Is_Regular_File
            then
               Insert
                 (Command.Kernel,
                  (-"warning: the file ") &
                  Command.Source_Files (Command.File_Index).Full_Name.all &
                  (-" cannot be found. It will be skipped."),
                  Mode => Info);
               return Execute_Again;
            end if;

            File := Get_Or_Create
              (Database,
               Command.Source_Files (Command.File_Index));

            Lang := Get_Language_From_File
              (Lang_Handler,
               Command.Source_Files (Command.File_Index));

            --  ??? We won't support other parsers than Ada here, as cross
            --  references are for now only reliable with Ada. Change this
            --  as soon as we have correct support for cross-refs in the
            --  other languages.
            if (not Command.Options.Process_Up_To_Date_Only
                or else Is_Up_To_Date (File))
              and then Lang.all in Language.Ada.Ada_Language'Class
            then
               --  Create the new entity info structure
               File_EInfo := new Entity_Info_Record
                 (Category => Cat_File);
               File_EInfo.Name := new String'
                 (Base_Name (Command.Source_Files (Command.File_Index)));
               File_EInfo.Short_Name := File_EInfo.Name;

               Trace (Me, "Analysis of file " & File_EInfo.Name.all);

               declare
                  Construct_T   : Construct_Tree;
                  Constructs    : aliased Construct_List;
                  Ctxt_Elem     : Context_Stack_Element;
                  Comments      : Comments_List.Vector;
                  Last          : Natural;
                  CR_Found      : Boolean;

               begin
                  File_EInfo.Language := Lang;
                  File_EInfo.File := File;
                  Ref (File_EInfo.File);
                  File_Buffer := Read_File
                    (Command.Source_Files (Command.File_Index));

                  --  Strip CRs from file.
                  Strip_CR (File_Buffer.all, Last, CR_Found);

                  if CR_Found then
                     declare
                        Old_Buff : GNAT.Strings.String_Access := File_Buffer;
                     begin
                        File_Buffer :=
                          new String'(Old_Buff (Old_Buff'First .. Last));
                        Free (Old_Buff);
                     end;
                  end if;

                  --  ??? Commented out code, because for now only Ada will be
                  --  supported

                  --  In case of C/C++, the LI_Handler's Parse_File_Construct
                  --  work much better than Language's Parse_Construct.
--                    if Lang.all in Cpp.Cpp_Language'Class
--                      or else Lang.all in C.C_Language'Class
--                    then
--                       declare
--                          Handler : LI_Handler;
--                       begin
--                          Handler := Get_LI_Handler_From_File
--                            (Lang_Handler,
--                             Command.Source_Files (Command.File_Index));
--
--                          if Handler /= null then
--                             Parse_File_Constructs
--                               (Handler,
--                                Lang_Handler,
--                                Command.Source_Files (Command.File_Index),
--                                Constructs);
--                          else
--                             Parse_Constructs
--                               (Language, File_Buffer.all, Constructs);
--                          end if;
--                       end;
--                    else

                  Parse_Constructs
                    (Lang, Locale_To_UTF8 (File_Buffer.all), Constructs);
--                    end if;

                  --  And add it to the global documentation list
                  Command.Documentation.Append (File_EInfo);

                  Construct_T := To_Construct_Tree (Constructs'Access, True);

                  Comments := Get_All_Comments (Lang, File_Buffer.all);

                  --  Retrieve the file's main unit comments, if any

                  --  If the main unit is documented, then there are
                  --  great chances that the comment is at the beginning
                  --  of the file. Let's look for it

                  --  Try to retrieve the documentation before the first
                  --  construct.

                  if First (Construct_T) /= Null_Construct_Tree_Iterator then
                     Comment_End :=
                       Get_Construct (First (Construct_T)).Sloc_Start.Line - 1;
                  else
                     Comment_End := 0;
                  end if;

                  for J in Comments.First_Index .. Comments.Last_Index loop
                     if Comments.Element (J).Start_Loc.Line > Comment_End then
                        --  Concatenate all comments before this
                        for K in Comments.First_Index .. J - 1 loop
                           --  Add this description to the unit node
                           if File_EInfo.Description /= null then
                              File_EInfo.Description := new String'
                                (File_EInfo.Description.all & ASCII.LF &
                                 Filter_Documentation
                                   (Comment_Block
                                      (Lang,
                                       File_Buffer
                                         (Comments.Element
                                            (K).Start_Loc.Index ..
                                            Comments.Element
                                              (K).End_Loc.Index),
                                       Comment => False,
                                       Clean   => True),
                                    Command.Options));
                           else
                              File_EInfo.Description := new String'
                                (Filter_Documentation
                                   (Comment_Block
                                      (Lang,
                                       File_Buffer
                                         (Comments.Element
                                            (K).Start_Loc.Index ..
                                            Comments.Element
                                              (K).End_Loc.Index),
                                       Comment => False,
                                       Clean   => True),
                                    Command.Options));
                           end if;
                        end loop;

                        exit;
                     end if;
                  end loop;

                  --  We now create the command's analysis_ctxt structure
                  Command.Analysis_Ctxt
                    := (Stack       => Context_Stack.Empty_Vector,
                        Iter        => First (Construct_T),
                        Tree        => Construct_T,
                        File_Buffer => File_Buffer,
                        Language    => Lang_Handler,
                        File        => File_EInfo.File,
                        Comments    => Comments,
                        Pkg_Nb      => 0);

                  Ctxt_Elem := (Parent_Entity => File_EInfo,
                                Pkg_Entity    => null,
                                Parent_Iter   => Null_Construct_Tree_Iterator);
                  Push (Command.Analysis_Ctxt, Ctxt_Elem);
                  --  Push it twice, so that one remains when the file analysis
                  --  is (finished
                  Push (Command.Analysis_Ctxt, Ctxt_Elem);
               end;
            else
               Insert
                 (Command.Kernel,
                  -("warning: cross references for file ") &
                  Base_Name (Command.Source_Files (Command.File_Index))
                  & (-" are not up-to-date. Documentation not generated."),
                  Mode => Info);
            end if;
         end if;

      elsif Command.Src_File_Index < Command.Source_Files'Last then
         --  Generate annotated source files

         Command.Src_File_Index := Command.Src_File_Index + 1;

         if Is_Spec_File
             (Command.Kernel,
              Command.Source_Files (Command.Src_File_Index))
           or else Command.Options.Process_Body_Files
         then
            if not Command.Source_Files
                     (Command.Src_File_Index).Is_Regular_File
            then
               Insert
                 (Command.Kernel,
                  (-"warning: the file ") &
                  Command.Source_Files (Command.Src_File_Index).Full_Name.all &
                  (-" cannot be found. It will be skipped."),
                  Mode => Info);
               return Execute_Again;
            end if;

            File := Get_Or_Create
              (Database,
               Command.Source_Files (Command.Src_File_Index));

            --  File might be null here if no LI_Handler corresponds to it.
            if File /= null
              and then
                (not Command.Options.Process_Up_To_Date_Only
                 or else Is_Up_To_Date (File))
            then
               if Command.Annotated_Files = null then
                  Command.Annotated_Files :=
                    new File_Array (1 .. Command.Source_Files'Length);
                  Command.Src_A_Files_Idx := 0;
               end if;

               --  Add file in the list of annotated source files
               Command.Src_A_Files_Idx := Command.Src_A_Files_Idx + 1;
               Command.Annotated_Files (Command.Src_A_Files_Idx) :=
                 Command.Source_Files (Command.Src_File_Index);

               Command.Src_Files.Append
                 (Command.Source_Files (Command.Src_File_Index));

               if Active (Me) then
                  Trace
                    (Me, "Generate annotated source for " &
                     Base_Name
                       (Command.Source_Files (Command.Src_File_Index)));
               end if;

               Command.Buffer :=
                 Read_File (Command.Source_Files (Command.Src_File_Index));

               declare
                  Lang_Handler : constant Language_Handler :=
                                   Get_Language_Handler (Command.Kernel);
                  Language     : constant Language_Access :=
                                   Get_Language_From_File
                                     (Lang_Handler,
                                      Command.Source_Files
                                        (Command.Src_File_Index));
               begin
                  Generate_Annotated_Source
                    (Kernel  => Command.Kernel,
                     Backend => Command.Backend,
                     File    => File,
                     Buffer  => Command.Buffer,
                     Lang    => Language,
                     Db      => Database,
                     Xrefs   => Command.EInfo_Map);
               end;

               Free (Command.Buffer);

            else
               Insert
                 (Command.Kernel,
                  -("warning: cross references for file ") &
                  Base_Name (Command.Source_Files (Command.Src_File_Index))
                  & (-" are not up-to-date. Annotated file not generated"),
                  Mode => Info);
            end if;
         end if;

      elsif not Command.Doc_Gen then
         --  All files analysed. Let's generate the documentation

         --  Clean-up previous context if needed
         Free (Command.Analysis_Ctxt.Tree);
         Free (Command.Analysis_Ctxt.File_Buffer);

         --  Generate all cross-refs
         Trace (Me, "Generate all Cross-Refs");
         Generate_Xrefs (Command.Xref_List, Command.EInfo_Map);

         Command.Doc_Gen := True;

         if not Command.Documentation.Is_Empty then
            Command.Cursor := Command.Documentation.First;
         end if;

      elsif Entity_Info_List.Has_Element (Command.Cursor) then
         --  Documentation generation has already started.

         --  For every file, generate the documentation.
         declare
            EInfo   : constant Entity_Info :=
                        Entity_Info_List.Element (Command.Cursor);
         begin
            Trace (Me, "Generate doc for " & EInfo.Name.all);

            Generate_Doc
              (Kernel    => Command.Kernel,
               Backend   => Command.Backend,
               Xrefs     => Command.EInfo_Map,
               Lang      => EInfo.Language,
               Db        => Database,
               File      => EInfo.File,
               Files     => Command.Files,
               Prj_Files => Command.Annotated_Files
                 (1 .. Command.Src_A_Files_Idx),
               E_Info    => EInfo);
            Entity_Info_List.Next (Command.Cursor);
         end;

      else
         --  No more file processing. Let's print indexes.

         Files_Vector_Sort.Sort (Command.Src_Files);
         Generate_Files_Index
           (Command.Kernel, Command.Backend, Command.Src_Files);

         Generate_Trees
           (Command.Kernel, Command.Backend,
            Command.Class_List, Command.EInfo_Map);

         Generate_Global_Index
           (Command.Kernel, Command.Backend, Command.EInfo_Map);

         Generate_TOC
           (Command.Kernel, Command.Backend, Command.Files);

         Templates_Parser.Release_Cache;
         Free (Command.Buffer);
         Free (Command.Documentation);
         Unchecked_Free (Command.Source_Files);
         --  Don't free the internal data of those lists, as they are just
         --  pointers to the Documentation structure, which has already been
         --  freed
         Command.Xref_List.Clear;
         Command.EInfo_Map.Clear;
         Command.Class_List.Clear;
         Command.Src_Files.Clear;
         Command.Files.Clear;

         Thaw (Database);
         return Success;
      end if;

      Thaw (Database);

      return Execute_Again;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Thaw (Database);
         return Failure;
   end Execute;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend : Docgen2_Backend.Backend_Handle;
      File    : GNATCOLL.VFS.Virtual_File;
      Options : Docgen_Options)
   is
      P          : constant Project_Type :=
                     Get_Project_From_File
                       (Registry          => Get_Registry (Kernel).all,
                        Source_Filename   => File,
                        Root_If_Not_Found => True);
      Other_File : constant Virtual_File :=
                     Create
                       (Other_File_Base_Name (P, File), P);

      C          : Docgen_Command_Access;
   begin
      Parse_All_LI_Information (Kernel, P, False);

      C := new Docgen_Command;

      if Is_Regular_File (Other_File) then
         C.Source_Files := new File_Array'(1 => File, 2 => Other_File);
      else
         C.Source_Files := new File_Array'(1 => File);
      end if;

      C.Kernel             := Kernel_Handle (Kernel);
      C.Backend            := Backend;
      C.Project            := P;
      C.File_Index         := C.Source_Files'First - 1;
      C.Src_File_Index     := C.Source_Files'First - 1;
      C.Options            := Options;
      C.Analysis_Ctxt.Iter := Null_Construct_Tree_Iterator;

      Launch_Background_Command
        (Kernel,
         Command         => C,
         Active          => True,
         Show_Bar        => True,
         Destroy_On_Exit => True,
         Block_Exit      => True);
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend   : Docgen2_Backend.Backend_Handle;
      Project   : Projects.Project_Type;
      Options   : Docgen_Options;
      Recursive : Boolean := False)
   is
      C       : Docgen_Command_Access;
      P       : Projects.Project_Type := Project;
      Context : Selection_Context;
   begin
      if P = No_Project then
         Context := Get_Current_Context (Kernel);

         if Has_Project_Information (Context) then
            P := Project_Information (Context);
         else
            P := Get_Project (Kernel);
         end if;
      end if;

      Parse_All_LI_Information (Kernel, P, Recursive);

      declare
         Source_Files  : constant File_Array_Access :=
                           Get_Source_Files (P, Recursive);
      begin
         C := new Docgen_Command;
         C.Kernel         := Kernel_Handle (Kernel);
         C.Backend        := Backend;
         C.Project        := P;
         C.Source_Files   := Source_Files;
         C.Src_File_Index := Source_Files'First - 1;
         C.File_Index     := Source_Files'First - 1;
         C.Options        := Options;
         C.Analysis_Ctxt.Iter := Null_Construct_Tree_Iterator;
      end;

      Launch_Background_Command
        (Kernel,
         Command         => C,
         Active          => True,
         Show_Bar        => True,
         Destroy_On_Exit => True,
         Block_Exit      => True);
   end Generate;

   -------------------------------
   -- Generate_Annotated_Source --
   -------------------------------

   procedure Generate_Annotated_Source
     (Kernel  : access Kernel_Handle_Record'Class;
      Backend : Backend_Handle;
      File    : Source_File;
      Buffer  : GNAT.Strings.String_Access;
      Lang    : Language_Access;
      Db      : Entities_Database;
      Xrefs   : Entity_Info_Map.Map)
   is
      Last_Idx     : Natural := 0;
      Printout     : Unbounded_String;
      File_Handle  : File_Type;
      Translation  : Translate_Set;
      Line_Nb      : Natural := 0;
      Tmpl         : constant String :=
                       Backend.Get_Template
                         (Get_System_Dir (Kernel), Tmpl_Src);

      function CB
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      --------
      -- CB --
      --------

      function CB
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
         Decl_Entity  : Entity_Information;
         EInfo_Cursor : Entity_Info_Map.Cursor;
         EInfo        : Entity_Info;
         Loc          : File_Location;

         use Basic_Types;
      begin
         --  Print all text between previous call and current one
         if Last_Idx /= 0 then
            Ada.Strings.Unbounded.Append
              (Printout,
               Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
         end if;

         Last_Idx := Sloc_End.Index;

         if Entity /= Identifier_Text
           and then Entity /= Partial_Identifier_Text
         then
            --  For all entities that are not identifiers, print them
            --  directly

            Ada.Strings.Unbounded.Append
              (Printout,
               Backend.Gen_Tag
                 (Entity, Buffer (Sloc_Start.Index .. Sloc_End.Index)));

         else
            --  Print line number for all identifiers (one per line only).
            if Sloc_Start.Line > Line_Nb then
               Line_Nb := Sloc_Start.Line;

               declare
                  Line : constant String := Natural'Image (Line_Nb);
               begin
                  Ada.Strings.Unbounded.Append
                    (Printout,
                     Backend.Gen_Ref (Line (Line'First + 1 .. Line'Last)));
               end;
            end if;

            --  If entity is an identifier or a partial identifier, then try
            --  to find its corresponding Entity_Info

            --  Find the entity in E_Info and its children
            Decl_Entity := Get_Declaration_Entity
              (Buffer (Sloc_Start.Index .. Sloc_End.Index),
               Sloc_Start, File, Db, Lang);

            EInfo := null;

            if Decl_Entity /= null then
               Loc := Get_Declaration_Of (Decl_Entity);
               EInfo_Cursor := Xrefs.Find (Loc);

               if Entity_Info_Map.Has_Element (EInfo_Cursor) then
                  EInfo := Entity_Info_Map.Element (EInfo_Cursor);
               end if;
            end if;

            --  EInfo should now contain the Entity_Info corresponding to
            --  the entity declaration

            if EInfo /= null then
               --  The entity references an entity that we've analysed
               --  We generate a href to this entity declaration.

               --  Print href to entity declaration
               Ada.Strings.Unbounded.Append
                 (Printout,
                  Gen_Href
                    (Backend, EInfo,
                     Buffer (Sloc_Start.Index .. Sloc_End.Index)));

            elsif Decl_Entity /= null
              and then Loc.File = File
              and then Loc.Line = Sloc_Start.Line
              and then Loc.Column =
                Basic_Types.Visible_Column_Type (Sloc_Start.Column)
            then
               --  the examined entity is a declaration entity
               Ada.Strings.Unbounded.Append
                 (Printout,
                  Backend.Gen_Tag
                    (Identifier_Text,
                     Buffer (Sloc_Start.Index .. Sloc_End.Index)));

            else
               --  No declaration associated with examined entity
               --  just generate simple text
               Ada.Strings.Unbounded.Append
                 (Printout,
                  Backend.Gen_Tag
                    (Normal_Text,
                     Buffer (Sloc_Start.Index .. Sloc_End.Index)));

            end if;
         end if;

         return False;
      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return True;
      end CB;

   begin
      Trace (Me, "Parse entities");
      Parse_Entities (Lang, Buffer.all, CB'Unrestricted_Access);

      if Last_Idx /= 0 then
         Ada.Strings.Unbounded.Append
           (Printout,
            Buffer (Last_Idx + 1 .. Buffer'Last));
      end if;

      Insert
        (Translation, Assoc ("SOURCE_FILE", Get_Filename (File).Base_Name));
      Insert
        (Translation, Assoc ("PRINTOUT", Printout));

      declare
         Name : constant String :=
                  Get_Doc_Directory (Kernel) & "src_"
                  & Backend.To_Destination_Name
                      (GNATCOLL.VFS.Base_Name (Get_Filename (File)));
      begin
         Ada.Text_IO.Create (File_Handle, Name => Name);
      exception
         when Name_Error =>
            Insert (Kernel, "Could not create " & Name, Mode => Error);
            return;
      end;

      Ada.Text_IO.Put
        (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Annotated_Source;

   ------------------
   -- Generate_Doc --
   ------------------

   procedure Generate_Doc
     (Kernel    : access Kernel_Handle_Record'Class;
      Backend   : Backend_Handle;
      Xrefs     : Entity_Info_Map.Map;
      Lang      : Language_Access;
      Db        : Entities_Database;
      File      : Source_File;
      Files     : in out Cross_Ref_List.Vector;
      Prj_Files : File_Array;
      E_Info    : Entity_Info)
   is
      Tmpl        : constant String :=
                      Backend.Get_Template
                        (Get_System_Dir (Kernel), Tmpl_Spec);
      Translation : Translate_Set;
      Pkg_Found   : Boolean;
      File_Handle : File_Type;

      type Common_Info_Tags is record
         Name_Tag           : Vector_Tag;
         Src_Tag            : Vector_Tag;
         Body_Src_Tag       : Vector_Tag;
         Printout_Tag       : Vector_Tag;
         Description_Tag    : Vector_Tag;
         References_Tag     : Vector_Tag;
         Called_Tag         : Vector_Tag;
         Calls_Tag          : Vector_Tag;
         Loc_Tag            : Vector_Tag;
         Instantiation_Tag  : Vector_Tag;
         Renames_Tag        : Vector_Tag;
         Cat_Tag            : Vector_Tag;
      end record;

      function Get_Name (E : Entity_Info) return String;
      --  Get name from E, and formats it to reflect its attribute (abstract,
      --  generic)

      procedure Init_Common_Informations
        (E_Info   : Entity_Info;
         Tags     : in out Common_Info_Tags);

      procedure Insert_Common_Informations
        (Tag_Name : String;
         CI       : Common_Info_Tags);
      --  Insert in Translation the CI structure, with Tag_Name prefix

      procedure Format_Printout (E_Info : Entity_Info);
      --  Formats the printout to reflect language_entity types and
      --  cross-refs.

      --------------
      -- Get_Name --
      --------------

      function Get_Name (E : Entity_Info) return String is
         Str : Unbounded_String;
      begin
         Str := To_Unbounded_String (E.Name.all);

         if E.Is_Abstract then
            Str := Str & " (abstract)";
         end if;

         if E.Is_Private then
            Str := Str & " (private)";
         end if;

         if E.Is_Generic then
            Str := Str & " (generic)";
         end if;

         return To_String (Str);
      end Get_Name;

      ------------------------------
      -- Init_Common_Informations --
      ------------------------------

      procedure Init_Common_Informations
        (E_Info : Entity_Info;
         Tags   : in out Common_Info_Tags)
      is
         Ref_Tag    : Tag;
         Calls_Tag  : Tag;
         Called_Tag : Tag;
         Found      : Boolean;

      begin
         Append
           (Tags.Name_Tag, Get_Name (E_Info));

         Append
           (Tags.Src_Tag,
            Backend.To_Href
              (Location => Image (E_Info.Location.File_Loc.Line),
               Src_File => "src_" &
               GNATCOLL.VFS.Base_Name
                 (Get_Filename (E_Info.Location.File_Loc.File)),
               Pkg_Nb   => 1));

         Found := False;

         if E_Info.Body_Location /= No_File_Location then
            declare
               File : constant GNATCOLL.VFS.Virtual_File :=
                        Get_Filename (E_Info.Body_Location.File);
            begin
               for J in Prj_Files'Range loop
                  if Prj_Files (J) = File then
                     Found := True;
                     exit;
                  end if;
               end loop;
            end;
         end if;

         if Found then
            Append
              (Tags.Body_Src_Tag,
               Backend.To_Href
                 (Location => Image (E_Info.Body_Location.Line),
                  Src_File => "src_" &
                  GNATCOLL.VFS.Base_Name
                    (Get_Filename (E_Info.Body_Location.File)),
                  Pkg_Nb   => 1));
         else
            Append
              (Tags.Body_Src_Tag, "");
         end if;

         Format_Printout (E_Info);

         if E_Info.Printout /= null then
            Append (Tags.Printout_Tag, E_Info.Printout.all);
         else
            Append (Tags.Printout_Tag, "");
         end if;

         if E_Info.Description /= null then
            Append (Tags.Description_Tag, E_Info.Description.all);
         else
            Append (Tags.Description_Tag, "");
         end if;

         for J in E_Info.References.First_Index .. E_Info.References.Last_Index
         loop
            declare
               Loc      : constant File_Location :=
                            Get_Location (E_Info.References.Element (J));
               Src_File : constant GNATCOLL.VFS.Virtual_File :=
                            Get_Filename (Loc.File);
            begin
               Found := False;

               for J in Prj_Files'Range loop
                  if Prj_Files (J) = Src_File then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if Found then
                  Append
                    (Ref_Tag,
                     Gen_Href
                       (Backend,
                        Location_Image (Loc),
                        Backend.To_Href
                          (Image (Loc.Line),
                           "src_" & Base_Name (Src_File),
                           1),
                        Location_Image (Loc)) &
                     " (" &
                     Kind_To_String
                       (Get_Kind (E_Info.References.Element (J))) &
                     ")");

               else
                  Append
                    (Ref_Tag,
                     Location_Image (Loc) &
                     " (" &
                     Kind_To_String
                       (Get_Kind (E_Info.References.Element (J))) &
                     ")");
               end if;
            end;
         end loop;

         Vector_Sort.Sort (E_Info.Called);

         for J in E_Info.Called.First_Index .. E_Info.Called.Last_Index loop
            declare
               Str      : Unbounded_String;
               Loc      : constant File_Location :=
                            E_Info.Called.Element (J).Location;
               Src_File : constant GNATCOLL.VFS.Virtual_File :=
                            Get_Filename (Loc.File);
               Found    : Boolean := False;

            begin
               if E_Info.Called.Element (J).Xref /= null
                 and then
                   (E_Info.Called.Element (J).Xref.Category = Cat_Package
                    or else E_Info.Called.Element (J).Xref.Category = Cat_File)
               then
                  --  Do not provide a href to a non subprogram element
                  Str := To_Unbounded_String
                    (E_Info.Called.Element (J).Xref.Name.all);
               else
                  Str := To_Unbounded_String
                    (Gen_Href (Backend, E_Info.Called.Element (J)));
               end if;

               for J in Prj_Files'Range loop
                  if Prj_Files (J) = Src_File then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if Found then
                  Str := Str & " defined at " & Gen_Href
                    (Backend,
                     Location_Image (Loc),
                     Backend.To_Href
                       (Image (Loc.Line),
                        "src_" & Base_Name (Src_File),
                        1),
                     Location_Image (Loc));
               else
                  Str := Str & " defined at " & Location_Image (Loc);
               end if;

               Append (Called_Tag, To_String (Str));
            end;
         end loop;

         Vector_Sort.Sort (E_Info.Calls);

         for J in E_Info.Calls.First_Index .. E_Info.Calls.Last_Index loop
            declare
               Str      : Unbounded_String;
               Loc      : constant File_Location :=
                            E_Info.Calls.Element (J).Location;
               Src_File : constant GNATCOLL.VFS.Virtual_File :=
                            Get_Filename (Loc.File);
               Found    : Boolean := False;

            begin
               Str := To_Unbounded_String
                 (Gen_Href (Backend, E_Info.Calls.Element (J)));

               for J in Prj_Files'Range loop
                  if Prj_Files (J) = Src_File then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if Found then
                  Str := Str & " defined at " & Gen_Href
                    (Backend,
                     Location_Image (Loc),
                     Backend.To_Href
                       (Image (Loc.Line),
                        "src_" & Base_Name (Src_File),
                        1),
                     Location_Image (Loc));
               else
                  Str := Str & " defined at " & Location_Image (Loc);
               end if;

               Append (Calls_Tag, To_String (Str));
            end;
         end loop;

         Append (Tags.References_Tag, Ref_Tag);
         Append (Tags.Called_Tag, Called_Tag);
         Append (Tags.Calls_Tag, Calls_Tag);

         Append (Tags.Loc_Tag,
                 Location_Image (E_Info.Location.File_Loc));
         Append (Tags.Instantiation_Tag,
                 Gen_Href (Backend, E_Info.Instantiated_Entity));
         Append (Tags.Renames_Tag,
                 Gen_Href (Backend, E_Info.Renamed_Entity));
         Append (Tags.Cat_Tag, Image (E_Info.Lang_Category));
      end Init_Common_Informations;

      --------------------------------
      -- Insert_Common_Informations --
      --------------------------------

      procedure Insert_Common_Informations
        (Tag_Name : String;
         CI       : Common_Info_Tags) is
      begin
         Insert (Translation, Assoc (Tag_Name, CI.Name_Tag));
         Insert (Translation, Assoc (Tag_Name & "_SRC", CI.Src_Tag));
         Insert (Translation, Assoc (Tag_Name & "_BODY_SRC", CI.Body_Src_Tag));
         Insert (Translation, Assoc (Tag_Name & "_PRINTOUT", CI.Printout_Tag));
         Insert (Translation,
                 Assoc (Tag_Name & "_DESCRIPTION", CI.Description_Tag));
         Insert (Translation,
                 Assoc (Tag_Name & "_REFERENCES", CI.References_Tag));
         Insert (Translation,
                 Assoc (Tag_Name & "_CALLED", CI.Called_Tag));
         Insert (Translation,
                 Assoc (Tag_Name & "_CALLS", CI.Calls_Tag));
         Insert (Translation, Assoc (Tag_Name & "_LOC", CI.Loc_Tag));
         Insert (Translation,
                 Assoc (Tag_Name & "_INSTANTIATION", CI.Instantiation_Tag));
         Insert (Translation, Assoc (Tag_Name & "_RENAMES", CI.Renames_Tag));
         Insert (Translation, Assoc (Tag_Name & "_CAT", CI.Cat_Tag));
      end Insert_Common_Informations;

      ---------------------
      -- Format_Printout --
      ---------------------

      procedure Format_Printout (E_Info : Entity_Info) is
         Printout : Unbounded_String;
         Last_Idx : Natural := 0;

         function To_Global_Loc (Src : Source_Location) return Source_Location;
         function Extract (Idx_Start, Idx_End : Natural) return String;
         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;

         -------------------
         -- To_Global_Loc --
         -------------------

         function To_Global_Loc (Src : Source_Location) return Source_Location
         is
            Ret : Source_Location;
         begin
            Ret.Line := E_Info.Printout_Loc.Line + Src.Line - 1;

            if Src.Line = 1 then
               Ret.Column := E_Info.Printout_Loc.Column + Src.Column - 1;
            else
               Ret.Column := Src.Column;
            end if;

            Ret.Index := E_Info.Printout_Loc.Index + Src.Index -
              E_Info.Printout'First;

            return Ret;
         end To_Global_Loc;

         -------------
         -- Extract --
         -------------

         function Extract (Idx_Start, Idx_End : Natural) return String is
         begin
            return E_Info.Printout (Idx_Start .. Idx_End);
         end Extract;

         --------
         -- CB --
         --------

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);
            Decl_Entity        : Entity_Information;
            EInfo_Cursor       : Entity_Info_Map.Cursor;
            EInfo              : Entity_Info;
            Start_Loc          : Source_Location;
            Loc                : File_Location;
            use Basic_Types;
         begin
            --  Print all text between previous call and current one
            if Last_Idx < Sloc_Start.Index then
               Append (Printout, Extract (Last_Idx, Sloc_Start.Index - 1));
            end if;

            Last_Idx := Sloc_End.Index + 1;

            if Entity /= Identifier_Text
              and then Entity /= Partial_Identifier_Text
            then
               --  For all entities that are not identifiers, print them
               --  directly
               Append
                 (Printout,
                  Backend.Gen_Tag
                    (Entity, Extract (Sloc_Start.Index, Sloc_End.Index)));

            else
               --  If entity is an identifier or a partial identifier, then try
               --  to find its corresponding Entity_Info

               --  Find the entity in E_Info and its children
               Start_Loc := To_Global_Loc (Sloc_Start);

               if E_Info.Entity_Loc.Index = Start_Loc.Index
                 or else
                   (E_Info.Full_Declaration /= null
                    and then E_Info.Full_Declaration.Xref /= null
                    and then E_Info.Full_Declaration.Xref.Entity_Loc.Index =
                      Start_Loc.Index)
               then
                  --  Identifier of current entity.
                  Append
                    (Printout,
                     Backend.Gen_Tag
                       (Entity,
                        Extract (Sloc_Start.Index, Sloc_End.Index), True));

               else
                  Decl_Entity := Get_Declaration_Entity
                    (Extract (Sloc_Start.Index, Sloc_End.Index),
                     Start_Loc, File, Db, Lang);

                  EInfo := null;

                  if Decl_Entity /= null then
                     Loc := Get_Declaration_Of (Decl_Entity);
                     EInfo_Cursor := Xrefs.Find (Loc);

                     if Entity_Info_Map.Has_Element (EInfo_Cursor) then
                        EInfo := Entity_Info_Map.Element (EInfo_Cursor);
                     end if;
                  end if;

                  --  EInfo should now contain the Entity_Info corresponding to
                  --  the entity declaration

                  if EInfo /= null
                    and then EInfo.Location.File_Loc.File = File
                    and then EInfo.Location.File_Loc.Line = Start_Loc.Line
                    and then EInfo.Location.File_Loc.Column =
                      Basic_Types.Visible_Column_Type (Start_Loc.Column)
                  then
                     --  the examined entity is a declaration entity
                     Append
                       (Printout,
                        Backend.Gen_Tag
                          (Identifier_Text,
                           Backend.Gen_Ref
                             (Location_Image (EInfo.Location.File_Loc)) &
                           Extract (Sloc_Start.Index, Sloc_End.Index)));

                  elsif EInfo = null then
                     --  No declaration associated with examined entity
                     --  just generate simple text
                     Append
                       (Printout,
                        Backend.Gen_Tag
                          (Normal_Text,
                           Extract (Sloc_Start.Index, Sloc_End.Index)));

                  else
                     --  The entity references an entity that we've analysed
                     --  We generate a href to this entity declaration.
                     Append
                       (Printout,
                        Gen_Href
                          (Backend, EInfo,
                           Extract (Sloc_Start.Index, Sloc_End.Index)));
                  end if;
               end if;
            end if;

            return False;
         exception
            when E : others =>
               Trace (Exception_Handle, E);
               return True;
         end CB;

      begin
         if E_Info.Printout /= null then
            Last_Idx := E_Info.Printout'First;
            Parse_Entities (Lang, E_Info.Printout.all, CB'Unrestricted_Access);
            Free (E_Info.Printout);
            E_Info.Printout := new String'(To_String (Printout));
         end if;
      end Format_Printout;

      Cursor           : Entity_Info_List.Cursor;
      Child_EInfo      : Entity_Info;
      Displayed        : Boolean;

      Pkg_CI                  : Common_Info_Tags;
      Pkg_Full_Link           : Vector_Tag;
      Pkg_Gen_Params          : Vector_Tag;
      Pkg_Gen_Params_Loc      : Vector_Tag;
      Class_CI                : Common_Info_Tags;
      Class_Parents           : Vector_Tag;
      Class_Children          : Vector_Tag;
      Class_Primitives        : Vector_Tag;
      Task_CI                 : Common_Info_Tags;
      Task_Type               : Vector_Tag;
      Task_Is_Type            : Vector_Tag;
      Task_Entry              : Vector_Tag;
      Task_Entry_Cat          : Vector_Tag;
      Task_Entry_Parent       : Vector_Tag;
      Task_Entry_Parent_Loc   : Vector_Tag;
      Task_Entry_Loc          : Vector_Tag;
      Task_Entry_Description  : Vector_Tag;
      Task_Entry_Printout     : Vector_Tag;
      Type_CI                 : Common_Info_Tags;
      Cst_CI                  : Common_Info_Tags;
      Cst_Type                : Vector_Tag;
      Subp_CI                 : Common_Info_Tags;
      Subp_Generic_Params     : Vector_Tag;
      Subp_Generic_Params_Loc : Vector_Tag;

   begin
      Pkg_Found := False;

      Cursor := E_Info.Children.First;

      while Entity_Info_List.Has_Element (Cursor) loop
         Child_EInfo := Entity_Info_List.Element (Cursor);

         if Child_EInfo.Category = Cat_Package then
            --  Move file's description to main package's description
            if E_Info.Category = Cat_File
              and then E_Info.Description /= null
              and then Child_EInfo.Description = null
            then
               Child_EInfo.Description := E_Info.Description;
               E_Info.Description := null;
            end if;

            Init_Common_Informations (Child_EInfo, Pkg_CI);
            Append (Pkg_Full_Link,
                    Gen_Href (Backend, Child_EInfo, "Full description"));

            if not Child_EInfo.Is_Renaming
              and then not Child_EInfo.Is_Instantiation
            then
               Pkg_Found := True;
               Generate_Doc
                 (Kernel, Backend, Xrefs, Lang, Db,
                  File, Files, Prj_Files, Child_EInfo);
            end if;
         end if;

         Entity_Info_List.Next (Cursor);
      end loop;

      if E_Info.Category = Cat_File then

         --  If one or more packages are defined there, then this means that
         --  the file's content has already been alalysed, just exit here.

         if Pkg_Found then
            --  If at least one package is found, this means that nothing
            --  more needs to be analysed on this file. Let's exit now then
            return;
         end if;

         --  Set main unit name
         Insert (Translation, Assoc ("SOURCE_FILE", Get_Name (E_Info)));

      elsif E_Info.Category = Cat_Package then

         Insert (Translation, Assoc ("PACKAGE_NAME", Get_Name (E_Info)));
      end if;

      --  UNIT HANDLING --

      --  Get current unit description
      if E_Info.Description /= null then
         Insert (Translation, Assoc ("DESCRIPTION", E_Info.Description.all));
      end if;

      --  Get current unit printout
      if E_Info.Printout /= null then
         Insert (Translation, Assoc ("PRINTOUT", E_Info.Printout.all));
      end if;

      --  Get the current unit generic parameters
      if E_Info.Is_Generic then
         declare
            Cursor            : Entity_Info_List.Cursor;
            Param             : Entity_Info;
            Param_Type        : Cross_Ref;
            Name_Str          : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Cursor := Entity_Info_List.First (E_Info.Generic_Params);

            while Entity_Info_List.Has_Element (Cursor) loop
               Param := Entity_Info_List.Element (Cursor);
               Param_Type := Param.Parameter_Type;

               Name_Str := To_Unbounded_String
                 (Backend.Gen_Tag (Identifier_Text, Get_Name (Param)));

               if Param_Type /= null then
                  Name_Str := Name_Str & " (" &
                    Gen_Href (Backend, Param_Type) & ")";
               end if;

               Append (Pkg_Gen_Params, Name_Str);
               Append (Pkg_Gen_Params_Loc,
                       Location_Image (Param.Location.File_Loc));

               Entity_Info_List.Next (Cursor);
            end loop;
         end;
      end if;

      Cursor := E_Info.Children.First;

      while Entity_Info_List.Has_Element (Cursor) loop
         Child_EInfo := Entity_Info_List.Element (Cursor);

         Displayed := False;

         if Child_EInfo.Is_Partial
           and then Child_EInfo.Full_Declaration.Xref /= null
           and then not Child_EInfo.Full_Declaration.Xref.Displayed
         then
            --  Replace current printout by full declaration's printout
            Free (Child_EInfo.Printout);
            Child_EInfo.Printout :=
              Child_EInfo.Full_Declaration.Xref.Printout;
            Child_EInfo.Full_Declaration.Xref.Printout := null;

            Child_EInfo.Printout_Loc :=
              Child_EInfo.Full_Declaration.Xref.Printout_Loc;
            Child_EInfo.Full_Declaration.Xref.Displayed := True;

         elsif Child_EInfo.Displayed then
            Displayed := True;
         end if;

         if Displayed then
            null;

            --  CLASSES HANDLING --

         elsif Child_EInfo.Category = Cat_Class then

            declare
               Prim_Tag, Parent_Tag, Children_Tag : Vector_Tag;
               Xref        : Cross_Ref;
               Prev_Xref   : Cross_Ref := null;
               Prim_Op_Str : Ada.Strings.Unbounded.Unbounded_String;
            begin
               --  Init entities common information
               Init_Common_Informations (Child_EInfo, Class_CI);

               --  Init parents
               for J in Child_EInfo.Parents.First_Index ..
                 Child_EInfo.Parents.Last_Index
               loop
                  Xref := Child_EInfo.Parents.Element (J);

                  --  avoid duplicated entries
                  if Prev_Xref = null
                    or else Xref.Location /= Prev_Xref.Location
                  then
                     Append (Parent_Tag, Gen_Href (Backend, Xref));
                  end if;
                  Prev_Xref := Xref;
               end loop;

               Append (Class_Parents, Parent_Tag);

               Vector_Sort.Sort (Child_EInfo.Class_Children);
               Prev_Xref := null;

               for J in Child_EInfo.Class_Children.First_Index ..
                 Child_EInfo.Class_Children.Last_Index
               loop
                  Xref := Child_EInfo.Class_Children.Element (J);

                  --  avoid duplicated entries
                  if Prev_Xref = null
                    or else Xref.Location /= Prev_Xref.Location
                  then
                     Append (Children_Tag, Gen_Href (Backend, Xref));
                  end if;

                  Prev_Xref := Xref;
               end loop;

               Append (Class_Children, Children_Tag);

               --  Init primitive operations
               Vector_Sort.Sort (Child_EInfo.Primitive_Ops);

               for J in Child_EInfo.Primitive_Ops.First_Index ..
                 Child_EInfo.Primitive_Ops.Last_Index
               loop
                  Xref := Child_EInfo.Primitive_Ops.Element (J);

                  Prim_Op_Str := To_Unbounded_String
                    (Gen_Href (Backend, Xref));

                  if Xref.Overriding_Op /= null
                    and then not Xref.Inherited
                  then
                     Append
                       (Prim_Op_Str,
                        " (overriding " &
                        Gen_Href
                          (Backend,
                           Xref.Overriding_Op) &
                        ")");
                  end if;

                  if Xref.Inherited then
                     Append
                       (Prim_Op_Str, " (Inherited)");
                  end if;

                  Append (Prim_Tag, Prim_Op_Str);
               end loop;

               Append (Class_Primitives, Prim_Tag);
               Clear (Prim_Tag);
            end;

            --  TASKS HANDLING --

         elsif Child_EInfo.Category = Cat_Task
           or else Child_EInfo.Category = Cat_Protected
         then

            --  Init entities common information
            declare
               Entry_Tag        : Vector_Tag;
               Entry_Cat_Tag    : Vector_Tag;
               Entry_Parent_Tag : Vector_Tag;
               Entry_P_Loc_Tag  : Vector_Tag;
               Entry_Loc_Tag    : Vector_Tag;
               Entry_Print_Tag  : Vector_Tag;
               Entry_Descr_Tag  : Vector_Tag;
               Entry_Cursor     : Entity_Info_List.Cursor;
               E_Entry          : Entity_Info;

            begin
               Init_Common_Informations (Child_EInfo, Task_CI);
               Append (Task_Type, Image (Child_EInfo.Category));
               Append (Task_Is_Type, Child_EInfo.Is_Type);

               Entry_Cursor := Entity_Info_List.First (Child_EInfo.Children);

               while Entity_Info_List.Has_Element (Entry_Cursor) loop
                  E_Entry := Entity_Info_List.Element (Entry_Cursor);

                  Append (Entry_Tag, E_Entry.Name.all);
                  Append (Entry_Cat_Tag, Image (E_Entry.Lang_Category));
                  Append (Entry_Parent_Tag, Child_EInfo.Name.all);
                  Append (Entry_P_Loc_Tag,
                          Location_Image (Child_EInfo.Location.File_Loc));
                  Append (Entry_Loc_Tag,
                          Location_Image (E_Entry.Location.File_Loc));
                  Format_Printout (E_Entry);
                  Append (Entry_Print_Tag, E_Entry.Printout.all);

                  if E_Entry.Description /= null then
                     Append (Entry_Descr_Tag, E_Entry.Description.all);
                  else
                     Append (Entry_Descr_Tag, "");
                  end if;

                  Entity_Info_List.Next (Entry_Cursor);
               end loop;

               Append (Task_Entry, Entry_Tag);
               Append (Task_Entry_Cat, Entry_Cat_Tag);
               Append (Task_Entry_Parent, Entry_Parent_Tag);
               Append (Task_Entry_Parent_Loc, Entry_P_Loc_Tag);
               Append (Task_Entry_Loc, Entry_Loc_Tag);
               Append (Task_Entry_Description, Entry_Descr_Tag);
               Append (Task_Entry_Printout, Entry_Print_Tag);
            end;

            --  TYPES HANDLING --
         elsif Child_EInfo.Category = Cat_Type then

            --  Init entities common information
            Init_Common_Informations (Child_EInfo, Type_CI);

            --  CONSTANTS AND GLOBAL VARIABLES HANDLING --
         elsif Child_EInfo.Category = Cat_Variable then

            --  Init entities common information
            Init_Common_Informations (Child_EInfo, Cst_CI);
            Append
              (Cst_Type,
               Gen_Href (Backend, Child_EInfo.Variable_Type));

            --  SUBPROGRAMS
         elsif Child_EInfo.Category = Cat_Subprogram then

            --  Init entities common information
            Init_Common_Informations (Child_EInfo, Subp_CI);

            declare
               Param_Tag     : Tag;
               Param_Loc_Tag : Tag;
               Param_Cursor  : Entity_Info_List.Cursor;
               Param         : Entity_Info;
               Param_Type    : Cross_Ref;
               Name_Str      : Ada.Strings.Unbounded.Unbounded_String;
            begin
               Param_Cursor := Entity_Info_List.First
                 (Child_EInfo.Generic_Params);

               while Entity_Info_List.Has_Element (Param_Cursor) loop
                  Param := Entity_Info_List.Element (Param_Cursor);
                  Param_Type := Param.Parameter_Type;

                  Name_Str := To_Unbounded_String
                    (Backend.Gen_Tag (Identifier_Text, Param.Name.all));

                  if Param_Type /= null then
                     Append
                       (Name_Str,
                        " (" & Gen_Href (Backend, Param_Type) & ")");
                  end if;

                  Append (Param_Tag, Name_Str);
                  Append (Param_Loc_Tag, Location_Image (Param_Type.Location));

                  Entity_Info_List.Next (Param_Cursor);
               end loop;

               Append (Subp_Generic_Params, Param_Tag);
               Append (Subp_Generic_Params_Loc, Param_Loc_Tag);
            end;
         end if;

         Entity_Info_List.Next (Cursor);
      end loop;

      if E_Info.Category = Cat_File then
         Insert
           (Translation,
            Assoc
              ("SOURCE",
               "src_" & Backend.To_Destination_Name (E_Info.Name.all)));
      else
         Insert
           (Translation,
            Assoc
              ("SOURCE",
               "src_" & Backend.To_Destination_Name
                 (Get_Filename (E_Info.Location.File_Loc.File).Base_Name)));
      end if;

      if E_Info.Body_Location /= No_File_Location
        and then E_Info.Body_Location.File /= null
      then
         declare
            File : constant GNATCOLL.VFS.Virtual_File :=
                     Get_Filename (E_Info.Body_Location.File);
            Found : Boolean := False;
         begin
            for J in Prj_Files'Range loop
               if Prj_Files (J) = File then
                  Found := True;
                  exit;
               end if;
            end loop;

            if Found then
               Insert
                 (Translation,
                  Assoc
                    ("BODY_SOURCE",
                     "src_" & Backend.To_Destination_Name (File.Base_Name)));
            end if;
         end;
      end if;

      if E_Info.Category = Cat_File then
         Files.Append
           (new Cross_Ref_Record'
              (Location      => No_File_Location,
               Name          => new String'
                 (Backend.To_Destination_Name (E_Info.Name.all)),
               Xref          => E_Info,
               Inherited     => False,
               Overriding_Op => null));
         Ada.Text_IO.Create
           (File_Handle,
            Name => Get_Doc_Directory (Kernel) &
              Backend.To_Destination_Name (E_Info.Name.all));
      else
         --  Don't display inner packages in TOC.
         if E_Info.Location.Pkg_Nb = 1 then
            Files.Append
              (new Cross_Ref_Record'
                 (Location      => E_Info.Location.File_Loc,
                  Name          => new String'
                    (Backend.To_Destination_Name
                       (Get_Filename (E_Info.Location.File_Loc.File).Base_Name,
                        1)),
                  Xref          => E_Info,
                  Inherited     => False,
                  Overriding_Op => null));
         end if;

         Ada.Text_IO.Create
           (File_Handle,
            Name => Get_Doc_Directory (Kernel) &
              Backend.To_Destination_Name
                (Base_Name (Get_Filename (E_Info.Location.File_Loc.File)),
                 E_Info.Location.Pkg_Nb));
      end if;

      Insert_Common_Informations ("PKG", Pkg_CI);
      Insert (Translation, Assoc ("PKG_FULL_LINK", Pkg_Full_Link));
      Insert (Translation, Assoc ("PKG_GENERIC_PARAMETERS", Pkg_Gen_Params));
      Insert (Translation,
              Assoc ("PKG_GENERIC_PARAMETERS_LOC", Pkg_Gen_Params_Loc));
      Insert_Common_Informations ("CLASS", Class_CI);
      Insert (Translation, Assoc ("CLASS_PARENTS", Class_Parents));
      Insert (Translation, Assoc ("CLASS_CHILDREN", Class_Children));
      Insert (Translation, Assoc ("CLASS_PRIMITIVES", Class_Primitives));
      Insert_Common_Informations ("TASK", Task_CI);
      Insert (Translation, Assoc ("TASK_TYPE", Task_Type));
      Insert (Translation, Assoc ("TASK_IS_TYPE", Task_Is_Type));
      Insert (Translation, Assoc ("TASK_ENTRY", Task_Entry));
      Insert (Translation, Assoc ("TASK_ENTRY_CAT", Task_Entry_Cat));
      Insert (Translation, Assoc ("TASK_ENTRY_PARENT", Task_Entry_Parent));
      Insert (Translation,
              Assoc ("TASK_ENTRY_PARENT_LOC", Task_Entry_Parent_Loc));
      Insert (Translation, Assoc ("TASK_ENTRY_LOC", Task_Entry_Loc));
      Insert (Translation,
              Assoc ("TASK_ENTRY_DESCRIPTION", Task_Entry_Description));
      Insert (Translation, Assoc ("TASK_ENTRY_PRINTOUT", Task_Entry_Printout));
      Insert_Common_Informations ("TYPE", Type_CI);
      Insert_Common_Informations ("CST", Cst_CI);
      Insert (Translation, Assoc ("CST_TYPE", Cst_Type));
      Insert_Common_Informations ("SUBP", Subp_CI);
      Insert (Translation,
              Assoc ("SUBP_GENERIC_PARAMETERS", Subp_Generic_Params));
      Insert (Translation,
              Assoc ("SUBP_GENERIC_PARAMETERS_LOC", Subp_Generic_Params_Loc));

      Ada.Text_IO.Put (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Doc;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs
     (XRef_List : in out Cross_Ref_List.Vector;
      EInfo_Map : Entity_Info_Map.Map)
   is
      Xref_Cursor  : Cross_Ref_List.Cursor;
      Xref_Node    : Cross_Ref;
      EInfo_Cursor : Entity_Info_Map.Cursor;

   begin
      Xref_Cursor := XRef_List.First;

      while Cross_Ref_List.Has_Element (Xref_Cursor) loop
         Xref_Node := Cross_Ref_List.Element (Xref_Cursor);
         EInfo_Cursor := EInfo_Map.Find (Xref_Node.Location);

         if Entity_Info_Map.Has_Element (EInfo_Cursor) then
            --  We found the referenced node
            Xref_Node.Xref := Entity_Info_Map.Element (EInfo_Cursor);
         end if;

         Cross_Ref_List.Next (Xref_Cursor);
      end loop;
   end Generate_Xrefs;

   ------------------
   -- Generate_TOC --
   ------------------

   procedure Generate_TOC
     (Kernel  : access Kernel_Handle_Record'Class;
      Backend : Backend_Handle;
      Files   : in out Cross_Ref_List.Vector)
   is
      Tmpl               : constant String :=
                             Backend.Get_Template
                               (Get_System_Dir (Kernel), Tmpl_TOC);
      Translation        : Translate_Set;
      File_Handle        : File_Type;
      Xref               : Cross_Ref;
      First_Letter       : Character;
      Prev_Letter        : Character := ASCII.NUL;
      Letter_Tag         : Vector_Tag;
      Letter_Changed_Tag : Vector_Tag;
      Files_Tag          : Vector_Tag;
   begin
      Vector_Sort.Sort (Files);

      for J in Files.First_Index .. Files.Last_Index loop
         Xref := Files.Element (J);
         First_Letter := To_Upper (Xref.Xref.Name (Xref.Xref.Name'First));

         if not Is_Letter (First_Letter) then
            First_Letter := '*';
         end if;

         Append (Letter_Tag, First_Letter);
         Append (Letter_Changed_Tag, First_Letter /= Prev_Letter);
         Append (Files_Tag,
                 Backend.Gen_Href
                   (Name  => Xref.Xref.Name.all,
                    Href  => Xref.Name.all,
                    Title => Location_Image (Xref.Location)));
         Prev_Letter := First_Letter;
      end loop;

      Insert (Translation, Assoc ("LETTER", Letter_Tag));
      Insert (Translation, Assoc ("LETTER_CHANGED", Letter_Changed_Tag));
      Insert (Translation, Assoc ("FILES", Files_Tag));

      Ada.Text_IO.Create
        (File_Handle,
         Name =>  Get_Doc_Directory (Kernel) &
           Backend.To_Destination_Name ("index"));
      Ada.Text_IO.Put (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);

      Open_Html
        (Kernel      => Kernel,
         URL_Or_File => Get_Doc_Directory (Kernel) &
           Backend.To_Destination_Name ("index"));
   end Generate_TOC;

   --------------------------
   -- Generate_Files_Index --
   --------------------------

   procedure Generate_Files_Index
     (Kernel     : access Kernel_Handle_Record'Class;
      Backend    : Backend_Handle;
      Src_Files  : Files_List.Vector)
   is
      Tmpl        : constant String :=
                      Backend.Get_Template
                        (Get_System_Dir (Kernel), Tmpl_Src_Index);
      File_Handle : File_Type;
      Translation : Translate_Set;
      Src_File    : Vector_Tag;
   begin
      for J in Src_Files.First_Index .. Src_Files.Last_Index loop
         Append
           (Src_File, Base_Name (Src_Files.Element (J)));
      end loop;

      Insert (Translation, Assoc ("SRC_FILE", Src_File));

      Ada.Text_IO.Create
        (File_Handle,
         Name =>  Get_Doc_Directory (Kernel) &
           Backend.To_Destination_Name ("src_index"));
      Ada.Text_IO.Put (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Files_Index;

   --------------------
   -- Generate_Trees --
   --------------------

   procedure Generate_Trees
     (Kernel     : access Kernel_Handle_Record'Class;
      Backend    : Backend_Handle;
      Class_List : in out Cross_Ref_List.Vector;
      EInfo_Map  : Entity_Info_Map.Map)
   is
      Tmpl        : constant String :=
                      Backend.Get_Template
                        (Get_System_Dir (Kernel), Tmpl_Class_Tree);
      Tmpl_Elem   : constant String :=
                      Backend.Get_Template
                        (Get_System_Dir (Kernel), Tmpl_Class_Tree_Elem);
      File_Handle : File_Type;
      Xref_Cursor : Cross_Ref_List.Cursor;
      EInfo       : Entity_Info;
      Map_Cursor  : Entity_Info_Map.Cursor;
      Xref        : Cross_Ref;
      Translation : Translate_Set;
      Tree_Tag    : Vector_Tag;

      function Print_Tree
        (Name  : String;
         Class : Entity_Info;
         Depth : Natural) return String;
      --  Recursively print the class tree

      ----------------
      -- Print_Tree --
      ----------------

      function Print_Tree
        (Name  : String;
         Class : Entity_Info;
         Depth : Natural) return String
      is
         Cursor            : Cross_Ref_List.Cursor;
         Translation       : Translate_Set;
         Tree_Tag          : Vector_Tag;
         Tree_Loc_Tag      : Vector_Tag;
         Root_Tree_Tag     : Vector_Tag;
         Tree_Children_Tag : Vector_Tag;
         Xref, Prev_Xref   : Cross_Ref;
      begin
         if Class = null then
            return "";
         elsif Class.Category /= Cat_Class then
            return "";
         end if;

         Append (Tree_Tag, Gen_Href (Backend, Class, Name));
         Append (Tree_Loc_Tag, Location_Image (Class.Location.File_Loc));
         Append (Root_Tree_Tag, Depth = 0);

         Vector_Sort.Sort (Class.Class_Children);
         Prev_Xref := null;
         Cursor := Class.Class_Children.First;

         while Cross_Ref_List.Has_Element (Cursor) loop
            Xref := Cross_Ref_List.Element (Cursor);

            if Xref.Xref /= null
              and then not Xref.Xref.Displayed
            then
               if Prev_Xref = null
                 or else Xref.Location /= Prev_Xref.Location
               then
                  declare
                     Child : constant String :=
                       Print_Tree
                         (Xref.Name.all,
                          Xref.Xref,
                          Depth + 1);
                  begin
                     if Child /= "" then
                        Append (Tree_Children_Tag, Child);
                     end if;
                  end;
               end if;
            end if;

            Prev_Xref := Xref;

            Cross_Ref_List.Next (Cursor);
         end loop;

         Insert (Translation, Assoc ("TREE", Tree_Tag));
         Insert (Translation, Assoc ("TREE_LOC", Tree_Loc_Tag));
         Insert (Translation, Assoc ("ROOT_TREE", Root_Tree_Tag));
         Insert (Translation, Assoc ("TREE_CHILDREN", Tree_Children_Tag));

         return Parse (Tmpl_Elem, Translation, Cached => True);
      end Print_Tree;

   begin
      Vector_Sort.Sort (Class_List);
      Xref_Cursor := Class_List.First;

      while Cross_Ref_List.Has_Element (Xref_Cursor) loop
         Xref := Cross_Ref_List.Element (Xref_Cursor);
         Map_Cursor := EInfo_Map.Find (Xref.Location);

         if Entity_Info_Map.Has_Element (Map_Cursor) then
            --  We found the referenced node
            EInfo := Entity_Info_Map.Element (Map_Cursor);

            --  Let's see if it has a parent
            if (EInfo.Parents.Is_Empty
                or else EInfo.Parents.First_Element.Xref = null)
              and then not EInfo.Displayed
            then
               Append (Tree_Tag, Print_Tree (Xref.Name.all, EInfo, 0));
            end if;
         end if;

         Cross_Ref_List.Next (Xref_Cursor);
      end loop;

      Insert (Translation, Assoc ("TREE", Tree_Tag));
      Ada.Text_IO.Create
        (File_Handle,
         Name => Get_Doc_Directory (Kernel) &
           Backend.To_Destination_Name ("tree"));
      Ada.Text_IO.Put (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Trees;

   ---------------------------
   -- Generate_Global_Index --
   ---------------------------

   procedure Generate_Global_Index
     (Kernel    : access Kernel_Handle_Record'Class;
      Backend   : Backend_Handle;
      EInfo_Map : Entity_Info_Map.Map)
   is
      Tmpl         : constant String :=
                       Backend.Get_Template
                         (Get_System_Dir (Kernel), Tmpl_Index);
      Letter       : Character;
      Map_Cursor   : Entity_Info_Map.Cursor;
      EInfo        : Entity_Info;
      List_Index   : Natural;
      Translation  : Translate_Set;
      Href_Tag     : Tag;
      Loc_Tag      : Tag;
      Kind_Tag     : Tag;

      File_Handle  : File_Type;
      Local_List   : array (1 .. 27) of Entity_Info_List.Vector;
      First_List   : Boolean;

   begin
      Map_Cursor := EInfo_Map.First;

      while Entity_Info_Map.Has_Element (Map_Cursor) loop
         EInfo := Entity_Info_Map.Element (Map_Cursor);

         if EInfo.Category /= Cat_Parameter then
            Letter := To_Upper (EInfo.Short_Name (EInfo.Short_Name'First));

            if not Is_Letter (Letter) then
               List_Index := 1;
            else
               List_Index := 2 +
                 Character'Pos (Letter) - Character'Pos ('A');
            end if;

            Local_List (List_Index).Append (EInfo);
         end if;

         Entity_Info_Map.Next (Map_Cursor);
      end loop;

      for J in Local_List'Range loop
         if J = 1 then
            Letter := '*';
         else
            Letter := Character'Val (J - 2 + Character'Pos ('A'));
         end if;

         Insert
           (Translation,
            Assoc
              ("ENTITY_EXISTS_" & Letter,
               +(not Local_List (J).Is_Empty)));
      end loop;

      for J in Local_List'Range loop
         EInfo_Vector_Sort.Sort (Local_List (J));
         Clear (Href_Tag);
         Clear (Loc_Tag);
         Clear (Kind_Tag);

         for K in Local_List (J).First_Index .. Local_List (J).Last_Index loop
            EInfo := Local_List (J).Element (K);
            Append (Href_Tag, Gen_Href (Backend, EInfo, EInfo.Short_Name.all));
            Append (Loc_Tag, Location_Image (EInfo.Location.File_Loc));
            Append (Kind_Tag, Image (EInfo.Category));
         end loop;

         Insert (Translation, Assoc ("ENTITY_HREF", Href_Tag));
         Insert (Translation, Assoc ("ENTITY_LOC", Loc_Tag));
         Insert (Translation, Assoc ("ENTITY_KIND", Kind_Tag));

         if J = 1 then
            Letter := '*';
         else
            Letter := Character'Val (J - 2 + Character'Pos ('A'));
         end if;

         Insert (Translation, Assoc ("LETTER", +Letter));

         if Letter = '*' then
            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Kernel) &
                 Backend.To_Destination_Name ("entitiesother"));
         else
            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Kernel) &
                 Backend.To_Destination_Name ("entities" & Letter));
         end if;

         Ada.Text_IO.Put
           (File_Handle, Parse (Tmpl, Translation, Cached => True));
         Ada.Text_IO.Close (File_Handle);

         --  Special handling for the first non empty lists: we need
         --  this list to be named entities.html, so that other files
         --  can easily reference it.
         First_List := True;
         for K in 1 .. J - 1 loop
            if not Local_List (K).Is_Empty then
               First_List := False;
               exit;
            end if;
         end loop;

         if First_List then
            --  First non empty list... create entities.html
            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Kernel) &
                 Backend.To_Destination_Name ("entities"));
            Ada.Text_IO.Put
              (File_Handle, Parse (Tmpl, Translation, Cached => True));
            Ada.Text_IO.Close (File_Handle);
         end if;
      end loop;
   end Generate_Global_Index;

   ----------------------------
   -- Generate_Support_Files --
   ----------------------------

   procedure Generate_Support_Files
     (Kernel  : access Kernel_Handle_Record'Class;
      Backend : Backend_Handle)
   is
      Src_Dir : constant GNATCOLL.VFS.Virtual_File :=
                  Create (Backend.Get_Support_Dir (Get_System_Dir (Kernel)));
      Dst_Dir : constant String :=
                  Get_Doc_Directory (Kernel);
      Success : Boolean;
   begin
      Src_Dir.Copy (Dst_Dir & Base_Dir_Name (Src_Dir), Success);
      pragma Assert (Success);
   end Generate_Support_Files;

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory
     (Kernel : not null access Kernel_Handle_Record'Class) return String is
   begin
      return File_Utils.Name_As_Directory
        (Object_Path
           (Get_Root_Project (Get_Registry (Kernel).all),
            False, False)) & "doc/";
   end Get_Doc_Directory;

end Docgen2;
