with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Strings.Unbounded;

with Prj;                  use Prj;
with Prj.Tree;             use Prj.Tree;
with Src_Info;             use Src_Info;
with Src_Info.Queries;     use Src_Info.Queries;

with Language_Handlers;       use Language_Handlers;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with Test_Utils;              use Test_Utils;

with Work_On_Source;	use Work_On_Source;


package body Work_on_File is

   Exception_List   : Type_Exception_List.List;
   Exception_Node   : Exception_Information;
   Subprogram_List  : Type_Subprogram_List.List;
   Subprogram_Node  : Subprogram_Information;
   Type_List        : Type_Type_List.List;
   Type_Node        : Type_Information;


   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access)

   is
      Handler                   : Language_Handler;
      Project_Tree              : Project_Node_Id;
      Project_View              : Project_Id;
      LI_Unit                   : LI_File_Ptr;
      Source_Info_List          : Src_Info.LI_File_List;
      Entity_Iter               : Entity_Declaration_Iterator;
      Reference_Iter            : Entity_Reference_Iterator;
      Info                      : Entity_Information;
      LI_Handle                 : LI_Handler;
      Dependencies,Dep          : Dependency_List;
      Dep1                      : Dependency;
      Status                    : Dependencies_Query_Status;
      Iter                      : Dependency_Iterator;
      Doc_File                  : File_Type;
      Package_Name              : String_Access;
      Package_Label             : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if not Type_Exception_List.Is_Empty (Exception_List) then
         Put_Line ("Merde, Exception_List is not empty!");
      end if;

      Handler := Create_Lang_Handler;

      -- get Project_Tree and Project_View
      Reset (Source_Info_List);
      Load_Project (Prj_Filename.all, Handler, Project_Tree, Project_View);


      --if Parse_All then
         for L in 1 .. Languages_Count (Glide_Language_Handler (Handler)) loop
            LI_Handle := Get_Nth_Handler (Glide_Language_Handler (Handler), L);
            if LI_Handle /= null then
               Parse_All_LI_Information
                 (LI_Handle,
                  Source_Info_List,
                  Get_Current_Dir,
                  Project_View,
                  Predefined_Source_Path,
                  Predefined_Object_Path);
            end if;
         end loop;
      --end if;

      Load_LI_File
         (Source_Info_List, Handler, Project_View, Source_Filename.all, LI_Unit);

      --get all  entities of the file
      if LI_Unit /= No_LI_File then
         Entity_Iter := Find_All_Possible_Declarations (LI_Unit,"");

         Find_Dependencies
           (LI_Unit, Source_Filename.all, Dependencies, Status);

         Dep := Dependencies;
         Put_Line ("************************************************************ ");
         Put_Line ("This package depands on: ");
         while Dep /= null loop
            Put_Line
              ("Dep= "
               & Get_Source_Filename (File_Information (Dep.value))
               & " from_spec=" & Get_Depends_From_Spec (Dependency_Information (Dep.value))'Img & " from_body=" & Get_Depends_From_Body (Dependency_Information (Dep.value))'Img);

            Dep := Dep.Next;
         end loop;
         Destroy (Dependencies);

         Put_Line ("On this package depand: ");
         Find_Ancestor_Dependencies
           (Root_Project    => Project_Tree,
            Lang_Handler    => Handler,
            Source_Filename => Source_Filename.all,
            List            => Source_Info_List,
            Iterator        => Iter);
         while Get (Iter) /= No_LI_File loop
            Dep1 := Get (Iter);
            Put_Line
              ("Dep= "
               & Get_Source_Filename (File_Information (Dep1))
               & " from_spec=" & Get_Depends_From_Spec (Dependency_Information (Dep1))'Img & " from_body=" & Get_Depends_From_Body (Dependency_Information (Dep1))'Img);

            Destroy (Dep1);
            Next (Handler, Iter, Source_Info_List);
         end loop;

         Destroy (Iter);



      else
         Put_Line ("LI file not found");  --later Exception?
      end if;

      while not At_End(Entity_Iter) loop

        Info := Get (Entity_Iter);
         -- Put_Line (Get_Full_Name(Info, LI_Unit, ".") & " : " & Kind_To_String(Get_Kind(Info)));

         case Get_Kind (Info) is
            when Non_Generic_Function_Or_Operator =>
               Process_Function (Prj_Filename, Source_Filename, LI_Unit, Entity_Iter, Info);
            when Generic_Function_Or_Operator =>
               Process_Function (Prj_Filename, Source_Filename, LI_Unit, Entity_Iter, Info);
            when Non_Generic_Procedure =>
               Process_Procedure (Prj_Filename, Source_Filename, LI_Unit, Entity_Iter, Info);
            when Generic_Procedure =>
               Process_Procedure (Prj_Filename, Source_Filename, LI_Unit, Entity_Iter, Info);
            when Record_Type | Enumeration_Type =>
               Process_Type (Prj_Filename, Source_Filename, LI_Unit, Entity_Iter, Info);
            when Exception_Entity =>
               Process_Exception (Prj_Filename, Source_Filename, LI_Unit,
                                  Entity_Iter, Info);
            when Label_On_Block =>
               Package_Label :=  Ada.Strings.Unbounded.To_Unbounded_String(Get_Full_Name(Info, LI_Unit, "."));

               if Ada.Strings.Unbounded.Index (Package_Label, ".") = 0 then   --sure?

                  Package_Name := new String'(Get_Full_Name(Info, LI_Unit, "."));
               end if;
            when others => Put_Line (""); --Put_Line ("Entity unknown!");
         end case;
         Next(Entity_Iter);
         --Put_Line(Kind_To_String (Get_Kind(Info)) & ": " & Get_Full_Name(Info, LI_Unit, "."));
      end loop;

      Destroy (Entity_Iter);
      Destroy (Info);


      --now give all the information the formatting part
      Create (Doc_File, Out_File, Get_Doc_File(Source_Filename));
                                    Put_Line (Package_Name.all);
      Process_Source (Doc_File, "html", Package_Name,
                      Exception_List, Subprogram_List, Type_List);
      Close (Doc_File);

   end Process_File;



   -----------------------
   -- Process_Procedure --
   -----------------------

   procedure Process_Procedure
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access;
      LI_Unit         : LI_File_Ptr;
      Entity_Iter     : Entity_Declaration_Iterator;
      Info            : Entity_Information) is

      Dummy           : Entity_Information;
      Status          : Find_Decl_Or_Body_Query_Status;

   begin

      Subprogram_Node.Name       := new String'(Get_Full_Name(Info, LI_Unit, "."));
      Subprogram_Node.Short_Name := new String'(Get_Name(Info));
      Subprogram_Node.Type_Name  := new String' ("Procedure");
      Subprogram_Node.File_Name  := Source_Filename;
      Subprogram_Node.Column     := Get_Declaration_Column_Of (Info);
      Subprogram_Node.Line       := Get_Declaration_Line_Of (Info);

      Type_Subprogram_List.Append (Subprogram_List, Subprogram_Node);

   end Process_Procedure;



   ----------------------
   -- Process_Function --
   ----------------------

   procedure Process_Function
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access;
      LI_Unit         : LI_File_Ptr;
      Entity_Iter     : Entity_Declaration_Iterator;
      Info            : Entity_Information) is

   begin

      Subprogram_Node.Name       := new String'(Get_Full_Name(Info, LI_Unit, "."));
      Subprogram_Node.Short_Name := new String'(Get_Name(Info));
      Subprogram_Node.Type_Name  := new String' ("Function");
      Subprogram_Node.File_Name  := Source_Filename;
      Subprogram_Node.Column     := Get_Declaration_Column_Of (Info);
      Subprogram_Node.Line       := Get_Declaration_Line_Of (Info);

      Type_Subprogram_List.Append (Subprogram_List, Subprogram_Node);


   end Process_Function;

   -----------------------
   -- Process_Exception --
   -----------------------

   procedure Process_Exception
      (Prj_Filename    : String_Access;
       Source_Filename : String_Access;
       LI_Unit         : LI_File_Ptr;
       Entity_Iter     : Entity_Declaration_Iterator;
       Info            : Entity_Information) is

       Dummy           : Entity_Information;
       Status          : Find_Decl_Or_Body_Query_Status;

   begin

       Exception_Node.Name       := new String'(Get_Full_Name(Info, LI_Unit, "."));
       Exception_Node.Short_Name := new String'(Get_Name(Info));
       Exception_Node.File_Name  := Source_Filename;
       Exception_Node.Column     := Get_Declaration_Column_Of (Info);
       Exception_Node.Line       := Get_Declaration_Line_Of (Info);

       Type_Exception_List.Append (Exception_List, Exception_Node);
   end Process_Exception;


    -----------------
   -- Process_Type --
   ------------------

   procedure Process_Type
      (Prj_Filename    : String_Access;
       Source_Filename : String_Access;
       LI_Unit         : LI_File_Ptr;
       Entity_Iter     : Entity_Declaration_Iterator;
       Info            : Entity_Information) is

       Dummy           : Entity_Information;
       Status          : Find_Decl_Or_Body_Query_Status;

   begin

      Type_Node.Name       := new String'(Get_Full_Name(Info, LI_Unit, "."));
      Type_Node.Short_Name := new String'(Get_Name(Info));
      Type_Node.File_Name  := Source_Filename;
      Type_Node.Column     := Get_Declaration_Column_Of (Info);
      Type_Node.Line       := Get_Declaration_Line_Of (Info);

      case Get_Kind (Info) is
         when Record_Type =>
            Type_Node.Type_Name := new String'("Record");
         when Enumeration_Type =>
            Type_Node.Type_Name := new String'("Enumeration");
         when others => Put_Line ("Entity unknown!");
      end case;


      Type_Type_List.Append (Type_List, Type_Node);
   end Process_Type;



   ------------------
   -- Get_Doc_File --
   ------------------

   function Get_Doc_File
     (Source_Filename : String_Access) return String
   is
   begin
      return "docgen.htm";
   end Get_Doc_File;


   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (Kind : Src_Info.E_Kind) return String is
   begin
      --  ??? Would be nice to do it as a primitive subprogram of the
      --  LI_Handlers, unfortunately they currently don't have access to
      --  Glide_Intl for proper translations.

      case Kind is
         when Overloaded_Entity            => return "???";
         when Unresolved_Entity            => return "unknown";
         when Access_Object                =>
            return "access variable / pointer";
         when Access_Type                  => return "access type / pointer";
         when Array_Object                 => return "array";
         when Array_Type                   => return "array type";
         when Boolean_Object               => return "boolean";
         when Boolean_Type                 => return "boolean type";
         when Class_Wide_Object            => return "class wide";
         when Class_Wide_Type              => return "class wide type";
         when Decimal_Fixed_Point_Object   => return "decimal fixed point";
         when Decimal_Fixed_Point_Type     =>
            return "decimal fixed point type";
         when Entry_Or_Entry_Family        => return "entry or entry family";
         when Enumeration_Literal          => return "enumeration literal";
         when Enumeration_Object           => return "enumeration";
         when Enumeration_Type             => return "enumeration type";
         when Exception_Entity             => return "exception";
         when Floating_Point_Object        => return "floating point";
         when Floating_Point_Type          => return "floating point type";
         when Generic_Class                => return "generic class";
         when Generic_Function_Or_Operator => return "generic function";
         when Generic_Package              => return "generic package";
         when Generic_Procedure            => return "generic procedure";
         when Label_On_Block               => return "label on block";
         when Label_On_Loop                => return "label on loop";
         when Label_On_Statement           => return "label on statement";
         when Modular_Integer_Object       => return "modular integer";
         when Modular_Integer_Type         => return "modular integer type";
         when Named_Number                 => return "named number";
         when Non_Generic_Function_Or_Operator => return "function";
         when Non_Generic_Package          => return "package";
         when Non_Generic_Procedure        => return "procedure";
         when Ordinary_Fixed_Point_Object  => return "fixed point";
         when Ordinary_Fixed_Point_Type    => return "fixed point type";
         when Private_Type                 => return "private type";
         when Protected_Object             => return "protected object";
         when Protected_Type               => return "protected type";
         when Record_Object                => return "record / struct";
         when Record_Type                  => return "record type / struct";
         when Signed_Integer_Object        => return "signed integer";
         when Signed_Integer_Type          => return "signed integer type";
         when String_Object                => return "string";
         when String_Type                  => return "string type";
         when Task_Object                  => return "task";
         when Task_Type                    => return "task type";
      end case;
   end Kind_To_String;


end Work_on_File;
