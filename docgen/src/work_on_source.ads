with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Prj;                  use Prj;
with Prj.Tree;             use Prj.Tree;
with Src_Info;             use Src_Info;
with Src_Info.Queries;     use Src_Info.Queries;

with Language_Handlers;       use Language_Handlers;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with Test_Utils;              use Test_Utils;
with Generic_List;


package Work_on_Source is


   type Exception_Information is record
      Name       : GNAT.OS_Lib.String_Access;
      Short_Name : GNAT.OS_Lib.String_Access;
      File_Name  : GNAT.OS_Lib.String_Access;
      Column     : Natural;
      Line       : Positive;
   end record;
   procedure Free (X : in out Exception_Information);
   package Type_Exception_List is new Generic_List (Exception_Information);


   type Type_Information is record
      Name      : GNAT.OS_Lib.String_Access;
      Short_Name : GNAT.OS_Lib.String_Access;
      File_Name : GNAT.OS_Lib.String_Access;
      Type_Name : GNAT.OS_Lib.String_Access;
      Column    : Natural;
      Line      : Positive;
   end record;
   procedure Free (X : in out Type_Information);
   package Type_Type_List is new Generic_List (Type_Information);


  type Subprogram_Information is record
      Name       : GNAT.OS_Lib.String_Access;
      Short_Name : GNAT.OS_Lib.String_Access;
      Type_Name  : GNAT.OS_Lib.String_Access;
      File_Name  : GNAT.OS_Lib.String_Access;
      Column     : Natural;
      Line       : Positive;
      Para_List  : Type_Type_List.List;
   end record;
   procedure Free (X : in out Subprogram_Information);
   package Type_Subprogram_List is new Generic_List (Subprogram_Information);


   procedure Process_Source
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Package_Name    : GNAT.OS_Lib.String_Access;
      Exception_List  : Type_Exception_List.List;
      Subprogram_List : Type_Subprogram_List.List;
      Type_List       : Type_Type_List.List);
   --with the data from the lists, the soucre file and the config file,
   --create the Strings for the output

private

   procedure Process_Exceptions
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Exception_List : Type_Exception_List.List);

   procedure Process_Subprograms
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Subprogram_List : Type_Subprogram_List.List);

   procedure Process_Types
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Type_List : Type_Type_List.List);


   function Get_Next_Comment_Line
     (File : File_Type) return Unbounded_String;

   function Extract_Comment
     (File_Name : String;
      Line      : Natural) return Unbounded_String;

   function Exception_Renames
     (File_Name : String;
      Line      : Natural) return Unbounded_String;


end Work_on_Source;
