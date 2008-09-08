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
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Text_IO;                           use Ada.Text_IO;

with Glib.Convert; use Glib.Convert;

with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;

with Basic_Types;
with Commands;                  use Commands;
with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with File_Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
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

with Docgen2_Backend;        use Docgen2_Backend;
with Docgen2.Entities;       use Docgen2.Entities, Docgen2.Entities.Files_List;
with Docgen2.Scripts;        use Docgen2.Scripts;
with Docgen2.Tags;           use Docgen2.Tags;
with Docgen2.Utils;          use Docgen2.Utils;

package body Docgen2 is

   Me : constant Debug_Handle := Create ("Docgen");

   function Filter_Documentation
     (Doc     : String;
      Options : Docgen_Options) return String;
   --  Filters the doc according to the Options.

   type Context_Stack_Element is record
      Parent_Entity : Entity_Info;
      Pkg_Entity    : Entity_Info;
      Parent_Iter   : Construct_Tree_Iterator;
   end record;

   package Context_Stack is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Context_Stack_Element);

   type Analysis_Context is record
      Stack          : Context_Stack.Vector;
      Iter           : Construct_Tree_Iterator;
      Tree           : Construct_Tree;
      File_Buffer    : GNAT.Strings.String_Access;
      File           : Source_File;
      Language       : Language_Handler;
      Pkg_Nb         : Natural;
      Comments       : Comments_List.Vector;
   end record;

   procedure Push
     (Context : in out Analysis_Context;
      Elem    : Context_Stack_Element);
   procedure Pop (Context : in out Analysis_Context);
   function Current (Context : Analysis_Context) return Context_Stack_Element;
   --  Context stack manipulation

   procedure Remove_Element
     (List   : in out Files_List.Vector;
      Cursor : in out Files_List.Cursor);
   --  Removes an element, and place the cursor just after the current
   --  postition

   type Custom_File_Record is record
      Name     : Unbounded_String;
      Filename : Unbounded_String;
      Content  : Unbounded_String;
   end record;

   package Custom_Files_List is new Ada.Containers.Vectors
     (Natural, Custom_File_Record);

   procedure Add_Custom_Index
     (Cmd   : Docgen_Object;
      Trans : in out Translate_Set);
   --  Add in the translate set links to the user defined files

   type Command_State is
     (Analysis_Setup,
      Analyse_Files,
      Analyse_File_Constructs,
      Analysis_Tear_Down,
      Gen_Doc_Setup,
      Gen_Doc_Annotated_Src,
      Gen_Doc_Spec,
      Gen_Doc_Tear_Down);
   --  The state of the docgen command processing.

   type Docgen_Command is new Commands.Root_Command with record
      State            : Command_State := Analysis_Setup;
      --  The current state of the analysis

      Kernel           : Kernel_Handle;

      Backend          : Docgen2_Backend.Backend_Handle;
      --  The backend used to generate the documentation

      Analysis_Ctxt    : Analysis_Context;
      --  The current analysis context

      Project          : Projects.Project_Type;
      --  The project

      Src_Files        : Files_List.Vector;
      --  List of all files in the project

      File_Index       : Files_List.Cursor;
      --  Index on Src_Files

      Package_List     : Entity_Info_List.Vector;
      --  List of all files actually used

      Cursor           : Entity_Info_List.Cursor;
      --  Index on Package_List

      EInfos           : Entity_Info_Map.Map;
      --  List of all entities described in this documentation

      Xref_List        : Cross_Ref_List.Vector;
      --  All cross-ref used in this documentation.

      Class_List       : Cross_Ref_List.Vector;
      --  All tagged types found

      Custom_Files     : Custom_Files_List.Vector;
      --  Files created by the user

      Options          : Docgen_Options;
      --  Docgen options from the preferences menu

      Current_File     : GNATCOLL.VFS.Virtual_File;
      --  The file currently analysed. Used for user-defined callbacks.
   end record;
   --  Command used for generating the documentation

   overriding function Name (Command : access Docgen_Command) return String;
   overriding function Progress
     (Command : access Docgen_Command) return Progress_Record;
   overriding function Execute
     (Command : access Docgen_Command) return Command_Return_Type;
   --  See inherited for documentation

   procedure Analyse_Construct (Cmd : Docgen_Object);
   --  Analyse the construct pointed to by the current Context

   procedure Generate_Support_Files (Cmd : Docgen_Object);
   --  Generate support files in destination directory

   procedure Generate_Comments (Cmd : Docgen_Object);
   --  Generate structured comments from the raw data

   procedure Generate_Xrefs (Cmd : Docgen_Object);
   --  Generate all missing links in Xref_List

   procedure Generate_Trees (Cmd : Docgen_Object);
   --  Generate the global inheritance trees

   procedure Generate_Global_Index (Cmd : Docgen_Object);
   --  Generate the global index

   procedure Generate_User_Indexes (Command : Docgen_Object);
   --  Generate the user-defined indexes

   procedure Generate_Annotated_Source
     (Command     : Docgen_Object;
      File        : Source_File;
      Buffer      : GNAT.Strings.String_Access;
      Buffer_Last : Natural;
      Lang        : Language_Access;
      Db          : Entities_Database;
      Xrefs       : Entity_Info_Map.Map);
   --  Generate hrefs and pretty print on a source file.

   procedure Generate_Doc
     (Command   : Docgen_Object;
      Xrefs     : Entity_Info_Map.Map;
      Lang      : Language_Access;
      Db        : Entities_Database;
      File      : Source_File;
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

   function Location_Image (E_Info : Entity_Info) return String;
   function Location_Image (Loc : File_Location) return String;
   --  Return the location formated the gnat way: "file:line:col"

   procedure Get_All_Comments
     (Lang     : Language_Access;
      Buffer   : String;
      Comments : out Comments_List.Vector);
   --  Retrieve all comment blocks from a file

   --------------
   -- Gen_Href --
   --------------

   function Gen_Href
     (Backend : Backend_Handle;
      EInfo   : Entity_Info;
      Name    : String := "") return String
   is
      File : GNATCOLL.VFS.Virtual_File;
      Pkg  : Natural;
   begin
      if EInfo.Location.File_Loc.File /= null then
         File := Get_Filename (EInfo.Location.File_Loc.File);
         Pkg  := EInfo.Location.Pkg_Nb;

      elsif EInfo.Category = Cat_Package
        or else EInfo.Category = Cat_File
      then
         File := Get_Filename (EInfo.File);
         Pkg  := 1;

      else
         File := GNATCOLL.VFS.No_File;
      end if;

      pragma Assert (File /= GNATCOLL.VFS.No_File);

      declare
         Ref : constant String :=
                 Backend.To_Href
                   (Location_Image (EInfo),
                    File.Base_Name,
                    Pkg);
      begin
         if Name = "" then
            return Backend.Gen_Href
              (EInfo.Name.all, Ref,
               "defined at " & Location_Image (EInfo));
         else
            return Backend.Gen_Href
              (Name, Ref,
               "defined at " & Location_Image (EInfo));
         end if;
      end;
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

   function Location_Image (E_Info : Entity_Info) return String is
   begin
      if Get_File (E_Info.Location.File_Loc) /= null then
         return Location_Image (E_Info.Location.File_Loc);
      else
         declare
            F_Name : constant String := Base_Name (Get_Filename (E_Info.File));
         begin
            return F_Name;
         end;
      end if;
   end Location_Image;

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
      if Loc = No_File_Location
        or else Get_File (Loc) = null
      then
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

   procedure Analyse_Construct (Cmd : Docgen_Object)
   is
      Db            : constant Entities_Database := Get_Database (Cmd.Kernel);
      Context       : Analysis_Context renames Cmd.Analysis_Ctxt;
      Construct     : access Simple_Construct_Information;
      Entity        : Entity_Information := null;
      E_Info        : Entity_Info;
      Entity_Kind   : E_Kind;
      Lang          : constant Language_Access :=
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
           (Name          => new String'(Name),
            Location      => Loc,
            Xref          => null,
            Inherited     => False,
            Overriding_Op => null);

         Cmd.Xref_List.Append (Xref);

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
         E_Info.Body_Location := No_File_Location;

         if Loc /= No_File_Location then
            if Context_Elem.Pkg_Entity /= null then
               E_Info.Location := (File_Loc => Loc,
                                   Pkg_Nb   => Context_Elem.Pkg_Entity.Pkg_Nb);
            else
               E_Info.Location := (File_Loc => Loc, Pkg_Nb => 1);
            end if;

            Cmd.EInfos.Include (Loc, E_Info);
         else
            E_Info.Location := Null_Location;
         end if;

         if E_Info.Category = Cat_Class then
            --  Class list is global for all tagged types. Use the full name.
            Cmd.Class_List.Append
              (Create_Xref
                 (Get_Full_Name (Entity), Get_Declaration_Of (Entity)));
         end if;

         return E_Info;
      end Create_EInfo;

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
        and then not Cmd.Options.Show_Private
      then
         Pop (Context);
         return;
      end if;

      --  Ignoring constructs within <doc_ignore> </doc_ignore>
      --  Ignoring with, use clauses
      --  Ignoring parameters, directly handled in subprogram nodes
      --  Ignoring represenation clauses.
      --  Ignoring constructs whose category is Unknown
      --  Ignoring unnamed entities.
      if not Ignore (Construct.Sloc_Start, Context.Comments)
        and then Construct.Category not in Dependency_Category
        and then Construct.Category /= Cat_Representation_Clause
        and then Construct.Category /= Cat_Namespace
        and then Category_Name (Construct.Category) /= ""
        and then Construct.Name /= null
      then
         Entity := Docgen2.Utils.Get_Entity
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
               E_Info.Name := new String'(Construct.Name.all);
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
         E_Info.Description :=
           Find_Doc (Construct.Sloc_Start,
                     Construct.Sloc_End,
                     Context.Comments);
         --  ??? Remove commented code
         --           declare
         --              Doc : constant String :=
         --                      Filter_Documentation
         --                        (Find_Doc
         --                           (Command.Backend,
         --                            Construct.Sloc_Start,
         --                            Construct.Sloc_End,
         --                            Context.Comments),
         --                         Options);
         --           begin
         --              if Doc /= "" then
         --                 E_Info.Description := new String'(Doc);
         --              end if;
         --           end;

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
            if Cmd.Options.References
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

                  Param_EInfo.Description :=
                    Find_Doc
                      (Param_EInfo.Entity_Loc,
                       Param_EInfo.Entity_Loc,
                       Context.Comments);

                  --  ??? need a better handling of formal parameters types
                  Param_EInfo.Parameter_Type := Create_Xref
                    (Name => Kind_To_String (Get_Kind (Param_Entity)),
                     Loc  => No_File_Location);

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

                  Destroy (Iter);
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
   -- Name --
   ----------

   overriding function Name (Command : access Docgen_Command) return String is
      pragma Unreferenced (Command);
   begin
      return "Documentation generator";
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access Docgen_Command) return Progress_Record
   is
      Current : Natural;
      Total1  : constant Natural := (Natural (Command.Src_Files.Length));
      Total2  : constant Natural := (Natural (Command.Package_List.Length));
   begin
      case Command.State is
         when Analysis_Setup =>
            Current := 0;
         when Analyse_Files | Analyse_File_Constructs =>
            Current := Files_List.To_Index (Command.File_Index);
         when Analysis_Tear_Down | Gen_Doc_Setup =>
            Current := Total1;
         when Gen_Doc_Annotated_Src =>
            Current := Total1 + Files_List.To_Index (Command.File_Index);
         when Gen_Doc_Spec =>
            Current := 2 * Total1 +
              Entity_Info_List.To_Index (Command.Cursor);
         when Gen_Doc_Tear_Down =>
            Current := 2 * Total1 + Total2;
      end case;

      return (Activity => Running,
              Current  => Current,
              Total    => 2 * Total1 + Total2);
   end Progress;

   ----------------------
   -- Get_All_Comments --
   ----------------------

   procedure Get_All_Comments
     (Lang     : Language_Access;
      Buffer   : String;
      Comments : out Comments_List.Vector)
   is
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

      begin
         if Entity = Comment_Text or else Entity = Annotated_Comment_Text then
            --  ??? would be nice to handle annotated comments specially
            --  in particular SPARK annotations
            Add_Comment_Line
              (Sloc_Start, Sloc_End,
               Comment_Block
                 (Lang, Buffer (Sloc_Start.Index .. Sloc_End.Index),
                  Comment => False,
                  Clean   => False),
               Force_New => Last_Entity /= Comment_Text,
               List      => Comments);
            Last_Entity := Comment_Text;
         else
            Last_Entity := Entity;
         end if;

         return False;

      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return True;
      end CB;

   begin
      Parse_Entities (Lang, Buffer, CB'Unrestricted_Access);
   end Get_All_Comments;

   --------------------
   -- Remove_Element --
   --------------------

   procedure Remove_Element
     (List   : in out Files_List.Vector;
      Cursor : in out Files_List.Cursor)
   is
      Prev : constant Files_List.Extended_Index := To_Index (Cursor);
   begin
      List.Delete (Cursor);
      Cursor := List.To_Cursor (Prev);
   end Remove_Element;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Docgen_Command) return Command_Return_Type
   is
      File_EInfo    : Entity_Info;
      File_Buffer   : GNAT.Strings.String_Access;
      Database      : constant Entities_Database :=
                        Get_Database (Command.Kernel);
      Lang_Handler  : constant Language_Handler :=
                        Get_Language_Handler (Command.Kernel);
      Lang          : Language_Access;
      File          : Source_File;
      Striped       : Boolean;
      pragma Unreferenced (Striped);
      use type Ada.Containers.Count_Type;
   begin
      --  Freeze the database to speed-up the processing of the cross
      --  references. The database has just been updated before the command
      --  has been launched.
      Freeze (Database);

      case Command.State is
         when Analysis_Setup =>
            --  Let's first create the destination directory with support files
            Generate_Support_Files (Docgen_Object (Command));
            Command.State := Analyse_Files;
            Command.File_Index := Command.Src_Files.First;
            On_Documentation_Start (Docgen_Object (Command));

         when Analyse_Files =>
            --  Test if there are still files to analyse
            if not Has_Element (Command.File_Index) then
               Command.State := Gen_Doc_Setup;
               Thaw (Database);
               return Execute_Again;
            end if;

            if not Element (Command.File_Index).Is_Regular_File then
               Insert
                 (Command.Kernel,
                  (-"warning: the file ") &
                  Element (Command.File_Index).Full_Name.all &
                  (-" cannot be found. It will be skipped."),
                  Mode => Info);

               Remove_Element (Command.Src_Files, Command.File_Index);
               Thaw (Database);
               return Execute_Again;
            end if;

            File := Get_Or_Create (Database, Element (Command.File_Index));

            --  File might be null here if no LI_Handler corresponds to it.
            if File = null then
               Insert
                 (Command.Kernel,
                  (-"warning: the file ") &
                  Element (Command.File_Index).Full_Name.all &
                  (-" has no cross reference. It will be skipped."),
                  Mode => Info);
               Remove_Element (Command.Src_Files, Command.File_Index);
               Thaw (Database);
               return Execute_Again;
            end if;

            Lang := Get_Language_From_File
              (Lang_Handler, Element (Command.File_Index));

            --  ??? We won't support other parsers than Ada here, as cross
            --  references are for now only reliable with Ada. Change this
            --  as soon as we have correct support for cross-refs in the
            --  other languages.
            if Lang.all not in Language.Ada.Ada_Language'Class then
               Insert
                 (Command.Kernel,
                  -("info: Documentation not generated for ") &
                  Base_Name (Element (Command.File_Index)) &
                  (-" as this is a non Ada file."),
                  Mode => Info);

               Remove_Element (Command.Src_Files, Command.File_Index);
               Thaw (Database);
               return Execute_Again;
            end if;

            --  Verify that we have the .ali file for this source file.
            if Command.Options.Process_Up_To_Date_Only
              and then not Is_Up_To_Date (File)
            then
               Insert
                 (Command.Kernel,
                  -("warning: cross references for file ") &
                  Base_Name (Element (Command.File_Index)) &
                  (-" are not up-to-date. Documentation not generated."),
                  Mode => Error);

               Remove_Element (Command.Src_Files, Command.File_Index);
               Thaw (Database);
               return Execute_Again;
            end if;

            --  Only analyze specs
            if not Is_Spec_File
              (Command.Kernel, Element (Command.File_Index))
            then
               if Command.Options.Process_Body_Files then
                  --  We'll need this file later for generating annotated
                  --  sources
                  Next (Command.File_Index);
               else
                  --  We won't need this file.
                  Remove_Element (Command.Src_Files, Command.File_Index);
               end if;

               Thaw (Database);
               return Execute_Again;
            end if;

            --  Create the new entity info structure
            File_EInfo := new Entity_Info_Record (Category => Cat_File);
            File_EInfo.Name := new String'
              (Base_Name (Element (Command.File_Index)));
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
               File_Buffer := Read_File (Element (Command.File_Index));

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
                  --  if Lang.all in Cpp.Cpp_Language'Class
                  --    or else Lang.all in C.C_Language'Class
                  --  then
                  --     declare
                  --        Handler : LI_Handler;
                  --     begin
                  --        Handler := Get_LI_Handler_From_File
                  --          (Lang_Handler,
                  --           Command.Source_Files (Command.File_Index));
                  --
                  --        if Handler /= null then
                  --           Parse_File_Constructs
                  --             (Handler,
                  --              Lang_Handler,
                  --              Command.Source_Files (Command.File_Index),
                  --              Constructs);
                  --        else
                  --           Parse_Constructs
                  --             (Language, File_Buffer.all, Constructs);
                  --        end if;
                  --     end;
                  --  else

               Parse_Constructs
                 (Lang, Locale_To_UTF8 (File_Buffer.all), Constructs);
               --  end if;

               --  And add it to the global documentation list
               Command.Package_List.Append (File_EInfo);

               Construct_T := To_Construct_Tree (Constructs'Access, True);

               Get_All_Comments (Lang, File_Buffer.all, Comments);

               --  Retrieve the file's main unit comments, if any
               if First (Construct_T) /= Null_Construct_Tree_Iterator then
                  File_EInfo.Description :=
                    Find_Doc
                      (Get_Construct (First (Construct_T)).Sloc_Start,
                       Get_Construct (First (Construct_T)).Sloc_End,
                       Comments => Comments,
                       File_Doc => True);
               else
                  File_EInfo.Description := No_Comment;
               end if;

               --  We now create the command's analysis_ctxt structure
               Command.Analysis_Ctxt
                 := (Stack          => Context_Stack.Empty_Vector,
                     Iter           => First (Construct_T),
                     Tree           => Construct_T,
                     File_Buffer    => File_Buffer,
                     Language       => Lang_Handler,
                     File           => File_EInfo.File,
                     Comments       => Comments,
                     Pkg_Nb         => 0);

               Ctxt_Elem := (Parent_Entity => File_EInfo,
                             Pkg_Entity    => null,
                             Parent_Iter   => Null_Construct_Tree_Iterator);
               Push (Command.Analysis_Ctxt, Ctxt_Elem);
               Command.State := Analyse_File_Constructs;
            end;

         when Analyse_File_Constructs =>
            if Command.Analysis_Ctxt.Iter = Null_Construct_Tree_Iterator then
               --  Current file is analysed. Let's see if it has a single child
               --  e_info to move comment when appropriate

               --  For this, we pop all values until getting the root context
               while Current (Command.Analysis_Ctxt).Parent_Iter /=
                 Null_Construct_Tree_Iterator
               loop
                  Pop (Command.Analysis_Ctxt);
               end loop;

               declare
                  Ctxt_Elem  : constant Context_Stack_Element :=
                                 Current (Command.Analysis_Ctxt);
                  EInfo      : Entity_Info renames
                                 Ctxt_Elem.Parent_Entity;
               begin
                  if EInfo.Children.Length = 1
                    and then EInfo.Description /= No_Comment
                    and then EInfo.Children.First_Element.Description =
                      No_Comment
                  then
                     EInfo.Children.First_Element.all.Description :=
                       EInfo.Description;
                     EInfo.Description := No_Comment;
                  end if;
               end;

               Command.State := Analysis_Tear_Down;
            else
               --  Analysis of a new construct of the current file.
               Analyse_Construct (Docgen_Object (Command));
            end if;

         when Analysis_Tear_Down =>
            --  Clean-up analysis context
            Free (Command.Analysis_Ctxt.Tree);
            Free (Command.Analysis_Ctxt.File_Buffer);
            Free (Command.Analysis_Ctxt.Comments);

            --  And setup for next file analysis
            Next (Command.File_Index);

            if Has_Element (Command.File_Index) then
               Command.State := Analyse_Files;
            else
               Command.State := Gen_Doc_Setup;
            end if;

         when Gen_Doc_Setup =>
            --  Generate all annotated comments
            Generate_Comments (Docgen_Object (Command));
            --  Generate all cross-refs
            Trace (Me, "Generate all Cross-Refs");
            Generate_Xrefs (Docgen_Object (Command));

            --  We tell the user that the analysis is done
            On_Documentation_Finished (Docgen_Object (Command));

            Command.State := Gen_Doc_Setup;

            if not Command.Package_List.Is_Empty then
               Command.Cursor := Command.Package_List.First;
            end if;
            if not Command.Src_Files.Is_Empty then
               Command.File_Index := Command.Src_Files.First;
            end if;

            --  We need to first generate doc for annotated sources. Once we
            --  have them all, then we can generate the spec docs that
            --  reference those.
            Command.State := Gen_Doc_Annotated_Src;

         when Gen_Doc_Annotated_Src =>
            --  Generate annotated source files
            if not Has_Element (Command.File_Index) then
               Command.State := Gen_Doc_Spec;
               Thaw (Database);
               return Execute_Again;
            end if;

            File := Get_Or_Create (Database, Element (Command.File_Index));

            if Active (Me) then
               Trace
                 (Me, "Generate annotated source for " &
                  Base_Name (Element (Command.File_Index)));
            end if;

            declare
               Buffer       : GNAT.Strings.String_Access;
               Last         : Natural;
               Lang_Handler : constant Language_Handler :=
                                Get_Language_Handler (Command.Kernel);
               Language     : constant Language_Access :=
                                Get_Language_From_File
                                  (Lang_Handler,
                                   Element (Command.File_Index));
            begin
               Buffer := Read_File (Element (Command.File_Index));
               Strip_CR (Buffer.all, Last, Striped);

               Generate_Annotated_Source
                 (Command     => Docgen_Object (Command),
                  File        => File,
                  Buffer      => Buffer,
                  Buffer_Last => Last,
                  Lang        => Language,
                  Db          => Database,
                  Xrefs       => Command.EInfos);

               Free (Buffer);
            end;

            Next (Command.File_Index);

         when Gen_Doc_Spec =>
            if Entity_Info_List.Has_Element (Command.Cursor) then
               --  For every file, generate the documentation.
               declare
                  EInfo   : constant Entity_Info :=
                              Entity_Info_List.Element (Command.Cursor);
               begin
                  Trace (Me, "Generate doc for " & EInfo.Name.all);

                  Generate_Doc
                    (Command   => Docgen_Object (Command),
                     Xrefs     => Command.EInfos,
                     Lang      => EInfo.Language,
                     Db        => Database,
                     File      => EInfo.File,
                     E_Info    => EInfo);
                  Entity_Info_List.Next (Command.Cursor);
               end;
            end if;

            if not Entity_Info_List.Has_Element (Command.Cursor) then
               Command.State := Gen_Doc_Tear_Down;
            end if;

         when Gen_Doc_Tear_Down =>
            --  No more file processing. Let's print indexes.

            Files_Vector_Sort.Sort (Command.Src_Files);

            Generate_User_Indexes (Docgen_Object (Command));
            Generate_Trees (Docgen_Object (Command));
            Generate_Global_Index (Docgen_Object (Command));

            Templates_Parser.Release_Cache;
            Free (Command.EInfos);
            Free (Command.Package_List);
            Free (Command.Xref_List);
            --  Just clear the Class list as all xref are already freed by
            --  the previous global xref list cleanup.
            Command.Class_List.Clear;
            --  Files are controlled objects that do not need explicit cleanup
            Command.Src_Files.Clear;

            Thaw (Database);
            return Success;
      end case;

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

      C          : Docgen_Object;
   begin
      Parse_All_LI_Information (Kernel, P, False);

      C := new Docgen_Command;

      C.Src_Files.Append (File);
      if Is_Regular_File (Other_File) then
         C.Src_Files.Append (Other_File);
      end if;

      C.State              := Analysis_Setup;
      C.Kernel             := Kernel_Handle (Kernel);
      C.Backend            := Backend;
      C.Project            := P;
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
      C       : Docgen_Object;
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
         Source_Files  : File_Array_Access := Get_Source_Files (P, Recursive);
      begin
         C := new Docgen_Command;

         C.State          := Analysis_Setup;
         C.Kernel         := Kernel_Handle (Kernel);
         C.Backend        := Backend;
         C.Project        := P;
         for J in Source_Files'Range loop
            C.Src_Files.Append (Source_Files (J));
         end loop;
         C.Options        := Options;
         C.Analysis_Ctxt.Iter := Null_Construct_Tree_Iterator;
         Unchecked_Free (Source_Files);
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
     (Command     : Docgen_Object;
      File        : Source_File;
      Buffer      : GNAT.Strings.String_Access;
      Buffer_Last : Natural;
      Lang        : Language_Access;
      Db          : Entities_Database;
      Xrefs       : Entity_Info_Map.Map)
   is
      Last_Idx     : Natural := 0;
      Printout     : Unbounded_String;
      File_Handle  : File_Type;
      Translation  : Translate_Set;
      Line_Nb      : Natural := 0;
      Tmpl         : constant String :=
                       Command.Backend.Get_Template
                         (Get_System_Dir (Command.Kernel), Tmpl_Src);

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
               Command.Backend.Filter
                 (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1)));
         end if;

         Last_Idx := Sloc_End.Index;

         if Entity /= Identifier_Text
           and then Entity /= Partial_Identifier_Text
         then
            --  For all entities that are not identifiers, print them
            --  directly

            Ada.Strings.Unbounded.Append
              (Printout,
               Command.Backend.Gen_Tag
                 (Entity,
                  Command.Backend.Filter
                    (Buffer (Sloc_Start.Index .. Sloc_End.Index))));

         else
            --  Print line number for all identifiers (one per line only).
            if Sloc_Start.Line > Line_Nb then
               Line_Nb := Sloc_Start.Line;

               declare
                  Line : constant String := Natural'Image (Line_Nb);
               begin
                  Ada.Strings.Unbounded.Append
                    (Printout,
                     Command.Backend.Gen_Ref
                       (Line (Line'First + 1 .. Line'Last)));
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
                    (Command.Backend, EInfo,
                     Command.Backend.Filter
                       (Buffer (Sloc_Start.Index .. Sloc_End.Index))));

            elsif Decl_Entity /= null
              and then Loc.File = File
              and then Loc.Line = Sloc_Start.Line
              and then Loc.Column =
                Basic_Types.Visible_Column_Type (Sloc_Start.Column)
            then
               --  the examined entity is a declaration entity
               Ada.Strings.Unbounded.Append
                 (Printout,
                  Command.Backend.Gen_Tag
                    (Identifier_Text,
                     Command.Backend.Filter
                       (Buffer (Sloc_Start.Index .. Sloc_End.Index))));

            else
               --  No declaration associated with examined entity
               --  just generate simple text
               Ada.Strings.Unbounded.Append
                 (Printout,
                  Command.Backend.Gen_Tag
                    (Normal_Text,
                     Command.Backend.Filter
                       (Buffer (Sloc_Start.Index .. Sloc_End.Index))));

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
      Parse_Entities
        (Lang, Buffer (Buffer'First .. Buffer_Last), CB'Unrestricted_Access);

      if Last_Idx /= 0 then
         Ada.Strings.Unbounded.Append
           (Printout,
            Command.Backend.Filter (Buffer (Last_Idx + 1 .. Buffer_Last)));
      end if;

      Insert
        (Translation, Assoc ("SOURCE_FILE", Get_Filename (File).Base_Name));
      Insert
        (Translation, Assoc ("PRINTOUT", Printout));

      declare
         Name : constant String :=
                  Get_Doc_Directory (Command) & "src_"
                  & Command.Backend.To_Destination_Name
           (GNATCOLL.VFS.Base_Name (Get_Filename (File)));
      begin
         Ada.Text_IO.Create (File_Handle, Name => Name);
      exception
         when Name_Error =>
            Insert (Command.Kernel, "Could not create " & Name, Mode => Error);
            return;
      end;

      Add_Custom_Index (Command, Translation);
      Ada.Text_IO.Put
        (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Annotated_Source;

   ------------------
   -- Generate_Doc --
   ------------------

   procedure Generate_Doc
     (Command   : Docgen_Object;
      Xrefs     : Entity_Info_Map.Map;
      Lang      : Language_Access;
      Db        : Entities_Database;
      File      : Source_File;
      E_Info    : Entity_Info)
   is
      Tmpl        : constant String :=
                      Command.Backend.Get_Template
                        (Get_System_Dir (Command.Kernel), Tmpl_Spec);
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

      function Is_In_Prj (File : GNATCOLL.VFS.Virtual_File) return Boolean;
      --  Tell if File is an analysed file of the project

      procedure Init_Common_Informations
        (E_Info        : Entity_Info;
         Tags          : in out Common_Info_Tags);

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

      ---------------
      -- Is_In_Prj --
      ---------------

      function Is_In_Prj (File : GNATCOLL.VFS.Virtual_File) return Boolean is
      begin
         for J in Command.Src_Files.First_Index .. Command.Src_Files.Last_Index
         loop
            if Command.Src_Files.Element (J) = File then
               return True;
            end if;
         end loop;

         return False;
      end Is_In_Prj;

      ------------------------------
      -- Init_Common_Informations --
      ------------------------------

      procedure Init_Common_Informations
        (E_Info        : Entity_Info;
         Tags          : in out Common_Info_Tags)
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
            Command.Backend.To_Href
              (Location => Image (E_Info.Location.File_Loc.Line),
               Src_File => "src_" &
               GNATCOLL.VFS.Base_Name
                 (Get_Filename (E_Info.Location.File_Loc.File)),
               Pkg_Nb   => 1));

         Found := False;

         if E_Info.Body_Location /= No_File_Location then
            Found := Is_In_Prj (Get_Filename (E_Info.Body_Location.File));
         end if;

         if Found then
            Append
              (Tags.Body_Src_Tag,
               Command.Backend.To_Href
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

         Append
           (Tags.Description_Tag,
            Filter_Documentation
              (To_String (E_Info.Description),
               Command.Options));

         for J in E_Info.References.First_Index .. E_Info.References.Last_Index
         loop
            declare
               Loc      : constant File_Location :=
                            Get_Location (E_Info.References.Element (J));
               Src_File : constant GNATCOLL.VFS.Virtual_File :=
                            Get_Filename (Loc.File);
            begin
               if Is_In_Prj (Src_File) then
                  Append
                    (Ref_Tag,
                     Gen_Href
                       (Command.Backend,
                        Location_Image (Loc),
                        Command.Backend.To_Href
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
                    (Gen_Href (Command.Backend, E_Info.Called.Element (J)));
               end if;

               if Is_In_Prj (Src_File) then
                  Str := Str & " defined at " & Gen_Href
                    (Command.Backend,
                     Location_Image (Loc),
                     Command.Backend.To_Href
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

            begin
               Str := To_Unbounded_String
                 (Gen_Href (Command.Backend, E_Info.Calls.Element (J)));

               if Is_In_Prj (Src_File) then
                  Str := Str & " defined at " & Gen_Href
                    (Command.Backend,
                     Location_Image (Loc),
                     Command.Backend.To_Href
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
                 Location_Image (E_Info));
         Append (Tags.Instantiation_Tag,
                 Gen_Href (Command.Backend, E_Info.Instantiated_Entity));
         Append (Tags.Renames_Tag,
                 Gen_Href (Command.Backend, E_Info.Renamed_Entity));
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
                  Command.Backend.Gen_Tag
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
                     Command.Backend.Gen_Tag
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
                        Command.Backend.Gen_Tag
                          (Identifier_Text,
                           Command.Backend.Gen_Ref
                             (Location_Image (EInfo)) &
                           Extract (Sloc_Start.Index, Sloc_End.Index)));

                  elsif EInfo = null then
                     --  No declaration associated with examined entity
                     --  just generate simple text
                     Append
                       (Printout,
                        Command.Backend.Gen_Tag
                          (Normal_Text,
                           Extract (Sloc_Start.Index, Sloc_End.Index)));

                  else
                     --  The entity references an entity that we've analysed
                     --  We generate a href to this entity declaration.
                     Append
                       (Printout,
                        Gen_Href
                          (Command.Backend, EInfo,
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
            Init_Common_Informations (Child_EInfo, Pkg_CI);
            Append (Pkg_Full_Link,
                    Gen_Href
                      (Command.Backend, Child_EInfo, "Full description"));

            if not Child_EInfo.Is_Renaming
              and then not Child_EInfo.Is_Instantiation
            then
               Pkg_Found := True;
               Generate_Doc
                 (Command, Xrefs, Lang, Db, File, Child_EInfo);
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
      declare
         Unit_Comment : constant String :=
                          Filter_Documentation
                            (To_String (E_Info.Description), Command.Options);
      begin
         if Unit_Comment /= "" then
            Insert (Translation, Assoc ("DESCRIPTION", Unit_Comment));
         end if;
      end;

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
                 (Command.Backend.Gen_Tag (Identifier_Text, Get_Name (Param)));

               if Param_Type /= null then
                  Name_Str := Name_Str & " (" &
                  Gen_Href (Command.Backend, Param_Type) & ")";
               end if;

               Append (Pkg_Gen_Params, Name_Str);
               Append (Pkg_Gen_Params_Loc,
                       Location_Image (Param));

               Entity_Info_List.Next (Cursor);
            end loop;
         end;
      end if;

      Cursor := E_Info.Children.First;

      while Entity_Info_List.Has_Element (Cursor) loop
         Child_EInfo := Entity_Info_List.Element (Cursor);

         if Child_EInfo.Is_Partial
           and then Child_EInfo.Full_Declaration.Xref /= null
           and then not Child_EInfo.Full_Declaration.Xref.Hidden
         then
            --  Replace current printout by full declaration's printout
            Free (Child_EInfo.Printout);
            Child_EInfo.Printout :=
              Child_EInfo.Full_Declaration.Xref.Printout;
            Child_EInfo.Full_Declaration.Xref.Printout := null;

            Child_EInfo.Printout_Loc :=
              Child_EInfo.Full_Declaration.Xref.Printout_Loc;
            Child_EInfo.Full_Declaration.Xref.Hidden := True;
         end if;

         if Child_EInfo.Hidden then
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
                     Append (Parent_Tag, Gen_Href (Command.Backend, Xref));
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
                     Append (Children_Tag, Gen_Href (Command.Backend, Xref));
                  end if;

                  Prev_Xref := Xref;
               end loop;

               Append (Class_Children, Children_Tag);

               --  Init primitive operations
               Vector_Sort.Sort (Child_EInfo.Primitive_Ops);
               Prev_Xref := null;

               for J in Child_EInfo.Primitive_Ops.First_Index ..
                 Child_EInfo.Primitive_Ops.Last_Index
               loop
                  Xref := Child_EInfo.Primitive_Ops.Element (J);

                  if Prev_Xref = null
                    or else Prev_Xref.Location /= Xref.Location
                  then
                     Prev_Xref := Xref;
                     Prim_Op_Str := To_Unbounded_String
                       (Gen_Href (Command.Backend, Xref));

                     if Xref.Overriding_Op /= null
                       and then not Xref.Inherited
                     then
                        Append
                          (Prim_Op_Str,
                           " (overriding " &
                           Gen_Href
                             (Command.Backend,
                              Xref.Overriding_Op) &
                           ")");
                     end if;

                     if Xref.Inherited then
                        Append
                          (Prim_Op_Str, " (Inherited)");
                     end if;

                     Append (Prim_Tag, Prim_Op_Str);
                  end if;
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
                          Location_Image (Child_EInfo));
                  Append (Entry_Loc_Tag,
                          Location_Image (E_Entry));
                  Format_Printout (E_Entry);
                  Append (Entry_Print_Tag, E_Entry.Printout.all);

                  Append
                    (Entry_Descr_Tag,
                     Filter_Documentation
                       (To_String (E_Entry.Description), Command.Options));

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
               Gen_Href (Command.Backend, Child_EInfo.Variable_Type));

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
                    (Command.Backend.Gen_Tag
                       (Identifier_Text, Param.Name.all));

                  if Param_Type /= null then
                     Append
                       (Name_Str,
                        " (" & Gen_Href (Command.Backend, Param_Type) & ")");
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
               "src_" &
               Command.Backend.To_Destination_Name (E_Info.Name.all)));
      else
         Insert
           (Translation,
            Assoc
              ("SOURCE",
               "src_" & Command.Backend.To_Destination_Name
                 (Get_Filename (E_Info.Location.File_Loc.File).Base_Name)));
      end if;

      if E_Info.Body_Location /= No_File_Location
        and then E_Info.Body_Location.File /= null
      then
         declare
            File  : constant GNATCOLL.VFS.Virtual_File :=
                      Get_Filename (E_Info.Body_Location.File);
         begin
            if Is_In_Prj (File) then
               Insert
                 (Translation,
                  Assoc
                    ("BODY_SOURCE",
                     "src_" &
                     Command.Backend.To_Destination_Name (File.Base_Name)));
            end if;
         end;
      end if;

      if E_Info.Category = Cat_File then
         Ada.Text_IO.Create
           (File_Handle,
            Name => Get_Doc_Directory (Command) &
            Command.Backend.To_Destination_Name (E_Info.Name.all));
      else
         Ada.Text_IO.Create
           (File_Handle,
            Name => Get_Doc_Directory (Command) &
            Command.Backend.To_Destination_Name
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
      Add_Custom_Index (Command, Translation);

      Ada.Text_IO.Put (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Doc;

   ----------------------
   -- Add_Custom_Index --
   ----------------------

   procedure Add_Custom_Index
     (Cmd   : Docgen_Object;
      Trans : in out Translate_Set)
   is
      Names_Tag : Tag;
      Files_Tag : Tag;
   begin
      for J in Cmd.Custom_Files.First_Index .. Cmd.Custom_Files.Last_Index loop
         Append (Names_Tag, To_String (Cmd.Custom_Files.Element (J).Name));
         Append (Files_Tag, To_String (Cmd.Custom_Files.Element (J).Filename));
      end loop;
      Insert (Trans, Assoc ("USER_DEFINED_NAMES", Names_Tag));
      Insert (Trans, Assoc ("USER_DEFINED_FILES", Files_Tag));
   end Add_Custom_Index;

   ---------------------------
   -- Generate_User_Indexes --
   ---------------------------

   procedure Generate_User_Indexes (Command : Docgen_Object) is
      Tmpl         : constant String :=
                       Command.Backend.Get_Template
                         (Get_System_Dir (Command.Kernel),
                          Tmpl_User_Defined_File);
      File_Handle  : File_Type;
      Translation  : Translate_Set;
   begin
      Add_Custom_Index (Command, Translation);

      for J in Command.Custom_Files.First_Index ..
        Command.Custom_Files.Last_Index
      loop
         declare
            Obj : Custom_File_Record renames Command.Custom_Files.Element (J);
         begin
            Insert
              (Translation, Assoc ("NAME", Obj.Name));
            Insert
              (Translation, Assoc ("FILE_NAME", Obj.Filename));
            Insert
              (Translation, Assoc ("PRINTOUT", Obj.Content));

            declare
               Name : constant String :=
                        Get_Doc_Directory (Command) & To_String (Obj.Filename);
            begin
               Ada.Text_IO.Create (File_Handle, Name => Name);
            exception
               when Name_Error =>
                  Insert
                    (Command.Kernel,
                     "Could not create " & To_String (Obj.Name),
                     Mode => Error);
                  return;
            end;

            Ada.Text_IO.Put
              (File_Handle, Parse (Tmpl, Translation, Cached => True));
            Ada.Text_IO.Close (File_Handle);
         end;
      end loop;
   end Generate_User_Indexes;

   ---------------------------------
   -- Generate_Custom_Docgen_File --
   ---------------------------------

   procedure Generate_Custom_Docgen_File
     (Command  : Docgen_Object;
      Name     : String;
      Filename : String;
      Content  : String)
   is
   begin
      Command.Custom_Files.Append
        ((Name     => To_Unbounded_String (Name),
          Filename => To_Unbounded_String (Filename),
          Content  => To_Unbounded_String (Content)));
   end Generate_Custom_Docgen_File;

   -----------------------
   -- Generate_Comments --
   -----------------------

   procedure Generate_Comments (Cmd : Docgen_Object) is
      Cursor : Entity_Info_Map.Cursor;
      procedure Internal (Loc : File_Location; Elem : in out Entity_Info);
      --  Update the comment for Elem

      procedure Internal (Loc : File_Location; Elem : in out Entity_Info) is
         pragma Unreferenced (Loc);
      begin
         Cmd.Current_File := Get_Filename (Elem.Location.File_Loc.File);
         Analyse_Comment
           (Elem.Description,
            Cmd,
            Elem.Location.File_Loc.File,
            Elem.Name.all,
            Cmd.Backend.To_Href
              (Location_Image (Elem),
               Base_Name (Get_Filename (Elem.Location.File_Loc.File)),
               Elem.Location.Pkg_Nb));
      end Internal;

   begin
      Cursor := Cmd.EInfos.First;
      while Entity_Info_Map.Has_Element (Cursor) loop
         Entity_Info_Map.Update_Element (Cmd.EInfos, Cursor, Internal'Access);
         Entity_Info_Map.Next (Cursor);
      end loop;
   end Generate_Comments;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs (Cmd : Docgen_Object)
   is
      Xref_Cursor  : Cross_Ref_List.Cursor;
      Xref_Node    : Cross_Ref;
      EInfo_Cursor : Entity_Info_Map.Cursor;

   begin
      Xref_Cursor := Cmd.Xref_List.First;

      while Cross_Ref_List.Has_Element (Xref_Cursor) loop
         Xref_Node := Cross_Ref_List.Element (Xref_Cursor);
         EInfo_Cursor := Cmd.EInfos.Find (Xref_Node.Location);

         if Entity_Info_Map.Has_Element (EInfo_Cursor) then
            --  We found the referenced node
            Xref_Node.Xref := Entity_Info_Map.Element (EInfo_Cursor);
         end if;

         Cross_Ref_List.Next (Xref_Cursor);
      end loop;
   end Generate_Xrefs;

   --------------------
   -- Generate_Trees --
   --------------------

   procedure Generate_Trees (Cmd : Docgen_Object)
   is
      Tmpl        : constant String :=
                      Cmd.Backend.Get_Template
                        (Get_System_Dir (Cmd.Kernel), Tmpl_Class_Tree);
      Tmpl_Elem   : constant String :=
                      Cmd.Backend.Get_Template
                        (Get_System_Dir (Cmd.Kernel), Tmpl_Class_Tree_Elem);
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

         Append (Tree_Tag, Gen_Href (Cmd.Backend, Class, Name));
         Append (Tree_Loc_Tag, Location_Image (Class));
         Append (Root_Tree_Tag, Depth = 0);

         Vector_Sort.Sort (Class.Class_Children);
         Prev_Xref := null;
         Cursor := Class.Class_Children.First;

         while Cross_Ref_List.Has_Element (Cursor) loop
            Xref := Cross_Ref_List.Element (Cursor);

            if Xref.Xref /= null and then not Xref.Xref.Hidden then
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
      Vector_Sort.Sort (Cmd.Class_List);
      Xref_Cursor := Cmd.Class_List.First;

      while Cross_Ref_List.Has_Element (Xref_Cursor) loop
         Xref := Cross_Ref_List.Element (Xref_Cursor);
         Map_Cursor := Cmd.EInfos.Find (Xref.Location);

         if Entity_Info_Map.Has_Element (Map_Cursor) then
            --  We found the referenced node
            EInfo := Entity_Info_Map.Element (Map_Cursor);

            --  Let's see if it has a parent
            if (EInfo.Parents.Is_Empty
                or else EInfo.Parents.First_Element.Xref = null)
              and then not EInfo.Hidden
            then
               Append (Tree_Tag, Print_Tree (Xref.Name.all, EInfo, 0));
            end if;
         end if;

         Cross_Ref_List.Next (Xref_Cursor);
      end loop;

      Insert (Translation, Assoc ("TREE", Tree_Tag));
      Add_Custom_Index (Cmd, Translation);
      Ada.Text_IO.Create
        (File_Handle,
         Name => Get_Doc_Directory (Cmd) &
           Cmd.Backend.To_Destination_Name ("tree"));
      Ada.Text_IO.Put (File_Handle, Parse (Tmpl, Translation, Cached => True));
      Ada.Text_IO.Close (File_Handle);
   end Generate_Trees;

   ---------------------------
   -- Generate_Global_Index --
   ---------------------------

   procedure Generate_Global_Index (Cmd : Docgen_Object)
   is
      Tmpl         : constant String :=
                       Cmd.Backend.Get_Template
                         (Get_System_Dir (Cmd.Kernel), Tmpl_Index);
      Letter       : Character;
      Last_Letter  : Character := ' ';
      Letter_Kind  : Character;
      Map_Cursor   : Entity_Info_Map.Cursor;
      EInfo        : Entity_Info;
      Translation  : Translate_Set;
      Href_Tag     : Tag;
      Loc_Tag      : Tag;
      Kind_Tag     : Tag;

      File_Handle  : File_Type;
      type Index_Type is new Natural range 1 .. 2*27;
      Local_List   : array (Index_Type) of Entity_Info_List.Vector;
      List_Index   : Index_Type;
      First_List   : Boolean;

      type Index_Kind is (API_Index, Entity_Index);

      function Index
        (Kind : Index_Kind; Letter : Character) return Index_Type;
      --  Return index from kind and letter

      function Get_Index_Kind (I : Index_Type) return Index_Kind;
      --  Get the index kind from the current list index

      function Get_Letter (I : Index_Type) return Character;
      --  Get the first letter corresponding to Index.

      -----------
      -- Index --
      -----------

      function Index
        (Kind : Index_Kind; Letter : Character) return Index_Type
      is
         I : Index_Type;
      begin
         if Kind = API_Index then
            I := 1;
         else
            I := 28;
         end if;

         if not Is_Letter (Letter) then
            return I;
         else
            return I + 1 + Character'Pos (Letter) - Character'Pos ('A');
         end if;
      end Index;

      --------------------
      -- Get_Index_Kind --
      --------------------

      function Get_Index_Kind (I : Index_Type) return Index_Kind is
      begin
         if I <= 27 then
            return API_Index;
         else
            return Entity_Index;
         end if;
      end Get_Index_Kind;

      ----------------
      -- Get_Letter --
      ----------------

      function Get_Letter (I : Index_Type) return Character is
         Idx : Natural;
      begin
         Idx := Natural (I);

         if Idx = 1 or else I = 28 then
            return '*';
         elsif Idx <= 27 then
            return Character'Val (Idx - 2 + Character'Pos ('A'));
         else
            return Character'Val (Idx - 29 + Character'Pos ('A'));
         end if;
      end Get_Letter;

      use type Ada.Containers.Count_Type;

   begin
      for J in Cmd.Package_List.First_Index .. Cmd.Package_List.Last_Index loop
         EInfo := Cmd.Package_List.Element (J);

         if EInfo.Children.Length = 1 then
            EInfo := EInfo.Children.First_Element;
         end if;

         Letter := To_Upper (EInfo.Name (EInfo.Name'First));
         List_Index := Index (API_Index, Letter);
         Local_List (List_Index).Append (EInfo);
      end loop;

      Map_Cursor := Cmd.EInfos.First;

      while Entity_Info_Map.Has_Element (Map_Cursor) loop
         EInfo := Entity_Info_Map.Element (Map_Cursor);

         if EInfo.Category /= Cat_Parameter
           and then not EInfo.Hidden
         then
            Letter := To_Upper (EInfo.Short_Name (EInfo.Short_Name'First));
            List_Index := Index (Entity_Index, Letter);
            Local_List (List_Index).Append (EInfo);
         end if;

         Entity_Info_Map.Next (Map_Cursor);
      end loop;

      for J in Local_List'Range loop
         Letter := Get_Letter (J);

         case Get_Index_Kind (J) is
            when API_Index =>
               Insert
                 (Translation,
                  Assoc
                    ("FILE_EXISTS_" & Letter,
                     +(not Local_List (J).Is_Empty)));
            when Entity_Index =>
               Insert
                 (Translation,
                  Assoc
                    ("ENTITY_EXISTS_" & Letter,
                     +(not Local_List (J).Is_Empty)));
         end case;
      end loop;

      for J in Cmd.Src_Files.First_Index .. Cmd.Src_Files.Last_Index loop
         declare
            Name : constant String := Cmd.Src_Files.Element (J).Base_Name;
         begin
            Letter := To_Upper (Name (Name'First));
         end;

         if Letter /= Last_Letter then
            Insert (Translation, Assoc ("SRC_EXISTS_" & Letter, True));
            Last_Letter := Letter;
         end if;
      end loop;

      for J in Local_List'Range loop

         case Get_Index_Kind (J) is
            when API_Index =>
               --  Sort using the full name
               EInfo_Vector_Sort_Full.Sort (Local_List (J));
            when Entity_Index =>
               --  Sort using the short name
               EInfo_Vector_Sort_Short.Sort (Local_List (J));
         end case;

         Clear (Href_Tag);
         Clear (Loc_Tag);
         Clear (Kind_Tag);

         for K in Local_List (J).First_Index .. Local_List (J).Last_Index loop
            EInfo := Local_List (J).Element (K);
            --  For packages index, we want the full name. Else, we want the
            --  short name.
            if J < 28 then
               Append
                 (Href_Tag, Gen_Href (Cmd.Backend, EInfo, EInfo.Name.all));
            else
               Append
                 (Href_Tag,
                  Gen_Href (Cmd.Backend, EInfo, EInfo.Short_Name.all));
            end if;

            Append (Loc_Tag, Location_Image (EInfo));
            Append (Kind_Tag, Image (EInfo.Category));
         end loop;

         Insert (Translation, Assoc ("HREF", Href_Tag));
         Insert (Translation, Assoc ("LOC", Loc_Tag));
         Insert (Translation, Assoc ("KIND", Kind_Tag));

         Letter := Get_Letter (J);

         case Get_Index_Kind (J) is
            when API_Index =>
               Insert (Translation, Assoc ("E_LETTER", ""));
               Insert (Translation, Assoc ("F_LETTER", +Letter));
               Letter_Kind := 'f';
            when Entity_Index =>
               Insert (Translation, Assoc ("E_LETTER", +Letter));
               Insert (Translation, Assoc ("F_LETTER", ""));
               Letter_Kind := 'e';
         end case;

         Add_Custom_Index (Cmd, Translation);

         if Letter = '*' then
            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Cmd) &
               Cmd.Backend.To_Destination_Name
                 ("index" & Letter_Kind & "other"));
         else
            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Cmd) &
               Cmd.Backend.To_Destination_Name
                 ("index" & Letter_Kind & Letter));
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
            --  First non empty list... create index.html
            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Cmd) &
                 Cmd.Backend.To_Destination_Name ("index"));
            Ada.Text_IO.Put
              (File_Handle, Parse (Tmpl, Translation, Cached => True));
            Ada.Text_IO.Close (File_Handle);
         end if;
      end loop;

      Clear (Href_Tag);
      Clear (Loc_Tag);
      Clear (Kind_Tag);

      Last_Letter := ' ';
      for J in Cmd.Src_Files.First_Index .. Cmd.Src_Files.Last_Index + 1 loop
         if J <= Cmd.Src_Files.Last_Index then
            declare
               Name : constant String := Cmd.Src_Files.Element (J).Base_Name;
            begin
               Letter := To_Upper (Name (Name'First));
            end;
         else
            Letter := ASCII.NUL;
         end if;

         if Letter /= Last_Letter
           and then Last_Letter /= ' '
         then
            Insert (Translation, Assoc ("E_LETTER", ""));
            Insert (Translation, Assoc ("F_LETTER", ""));
            Insert (Translation, Assoc ("S_LETTER", +Last_Letter));
            Insert (Translation, Assoc ("HREF", Href_Tag));
            Insert (Translation, Assoc ("LOC", Loc_Tag));
            Insert (Translation, Assoc ("KIND", Kind_Tag));

            Ada.Text_IO.Create
              (File_Handle,
               Name => Get_Doc_Directory (Cmd) &
               Cmd.Backend.To_Destination_Name ("indexs" & Last_Letter));
            Ada.Text_IO.Put
              (File_Handle, Parse (Tmpl, Translation, Cached => True));
            Ada.Text_IO.Close (File_Handle);

            Clear (Href_Tag);
            Clear (Loc_Tag);
            Clear (Kind_Tag);
         end if;

         exit when J > Cmd.Src_Files.Last_Index;

         Last_Letter := Letter;

         Append
           (Href_Tag,
            Cmd.Backend.Gen_Href
              (Cmd.Src_Files.Element (J).Base_Name,
               Cmd.Backend.To_Destination_Name
                 ("src_" & Cmd.Src_Files.Element (J).Base_Name),
               Cmd.Src_Files.Element (J).Base_Name));
      end loop;

      Open_Html
        (Kernel      => Cmd.Kernel,
         URL_Or_File => Get_Doc_Directory (Cmd) &
         Cmd.Backend.To_Destination_Name ("index"));
   end Generate_Global_Index;

   ----------------------------
   -- Generate_Support_Files --
   ----------------------------

   procedure Generate_Support_Files (Cmd : Docgen_Object)
   is
      Src_Dir : constant GNATCOLL.VFS.Virtual_File :=
                  Create
                    (Cmd.Backend.Get_Support_Dir
                       (Get_System_Dir (Cmd.Kernel)));
      Dst_Dir : constant String :=
                  Get_Doc_Directory (Cmd);
      Success : Boolean;
   begin
      Src_Dir.Copy (Dst_Dir & Base_Dir_Name (Src_Dir), Success);
      pragma Assert (Success);
   end Generate_Support_Files;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (D : Docgen_Object) return GPS.Kernel.Kernel_Handle is
   begin
      return D.Kernel;
   end Get_Kernel;

   -----------------
   -- Get_Backend --
   -----------------

   function Get_Backend
     (D : Docgen_Object) return Docgen2_Backend.Backend_Handle is
   begin
      return D.Backend;
   end Get_Backend;

   -----------------
   -- Get_Options --
   -----------------

   function Get_Options (D : Docgen_Object) return Docgen_Options is
   begin
      return D.Options;
   end Get_Options;

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory (Object : Docgen_Object) return String is
   begin
      return File_Utils.Name_As_Directory
        (Object_Path
           (Get_Root_Project (Get_Registry (Object.Kernel).all),
            False, False)) & "doc/";
   end Get_Doc_Directory;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Object : Docgen_Object) return GNATCOLL.VFS.Virtual_File is
   begin
      return Object.Current_File;
   end Get_Current_File;

end Docgen2;
