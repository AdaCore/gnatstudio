with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Prj;                  use Prj;
with Prj.Tree;             use Prj.Tree;
with Src_Info;             use Src_Info;
with Src_Info.Queries;     use Src_Info.Queries;

with Language_Handlers;       use Language_Handlers;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with Test_Utils;              use Test_Utils;
with Generic_List;

with work_on_source;	use work_on_source;

package Work_on_File is

   procedure Process_File
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access);
   --Process the file: Find all declarations in the file, sort them and
   --call the subprograms for the creation of the documentation output.
   --Each file listed in the command line or in the
   --project file must be used with this procedure.


private

   procedure Process_Procedure
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access;
      LI_Unit         : LI_File_Ptr;
      Entity_Iter     : Entity_Declaration_Iterator;
      Info            : Entity_Information);
   --prepares the documantation for the different procedures in the source file.

   procedure Process_Function
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access;
      LI_Unit         : LI_File_Ptr;
      Entity_Iter     : Entity_Declaration_Iterator;
      Info            : Entity_Information);
   --prepares the documantation for the different procedures in the source file.

   procedure Process_Exception
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access;
      LI_Unit         : LI_File_Ptr;
      Entity_Iter     : Entity_Declaration_Iterator;
      Info            : Entity_Information);
   --prepares the documantation for the exceptions in the source file.


   procedure Process_Type
     (Prj_Filename    : String_Access;
      Source_Filename : String_Access;
      LI_Unit         : LI_File_Ptr;
      Entity_Iter     : Entity_Declaration_Iterator;
      Info            : Entity_Information);
   --prepares the documantation for the types in the source file.


   function Get_Doc_File
      (Source_Filename : String_Access) return String;
   --returns the pointer to the new created doc file -> first only HTML

   function Kind_To_String
     (Kind : Src_Info.E_Kind) return String;


end Work_on_File;
