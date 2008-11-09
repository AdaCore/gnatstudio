with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Adp_Converter;             use Adp_Converter;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Convert.Adp is

   -----------------------------
   -- Convert_From_Adp_To_Gpr --
   -----------------------------

   procedure Convert_From_Adp_To_Gpr
     (Adp_Filename : String;
      Spec_Extension, Body_Extension : GNAT.OS_Lib.String_Access)
   is
      Project  : Project_Type;
      Registry : Project_Registry;
      Tmp      : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Projects.Registry.Initialize;
      Load_Empty_Project (Registry);

      Project := Create_Project
        (Registry,
         Name => Base_Name (Adp_Filename, ".adp"),
         Path => Create (Dir_Name (Adp_Filename)));
      Set_Paths_Type (Project, Absolute);
      Convert_Adp_File (Adp_Filename,
                        Registry       => Registry,
                        Project        => Project,
                        Spec_Extension => Spec_Extension.all,
                        Body_Extension => Body_Extension.all);
      Tmp := Save_Project (Project);
   end Convert_From_Adp_To_Gpr;
end Convert.Adp;
