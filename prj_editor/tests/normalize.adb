
with Prj_Normalize; use Prj_Normalize;
with Ada.Command_Line; use Ada.Command_Line;

with Prj.Tree;      use Prj.Tree;
with Prj.Part;      use Prj.Part;
with Prj.PP;        use Prj.PP;
with Prj_API;       use Prj_API;
with Prj.Ext;       use Prj.Ext;
with Text_IO;       use Text_IO;
with Errout;        use Errout;
with Types;         use Types;

procedure Normalize is
   Name : String := Argument (1);
   Project : Project_Node_Id;

begin
   Parse (Project, Name, Always_Errout_Finalize => True);
   Errout.Finalize;
   pragma Assert (Project /= Empty_Node);
   Pretty_Print (Project);

   Put_Line ("Project is normalized: "
             & Is_Normalized (Project)'Img);

   Put_Line ("----- Normalized -----");
   Normalize (Project);
   Pretty_Print (Project);

   Put_Line ("Project is now normalized: "
             & Is_Normalized (Project)'Img);

   Put_Line ("----- Adding new variable -----");

   Prj.Ext.Add ("OS", "Linux");
   Prj.Ext.Add ("TARGET", "Devel");
   Prj.Ext.Add ("OS2", "Windows");
   --  Update_Attribute_Value_In_Scenario
   --    (Project         => Project,
   --     Pkg_Name        => "gnatmake",
   --     Attribute_Name  => "switches2",
   --     Value           => "-foo",
   --     Attribute_Index => Types.No_String);
   Update_Attribute_Value_In_Scenario
     (Project         => Project,
      Pkg_Name        => "compiler",
      Attribute_Name  => "Source_Files",
      Value           => "foo.adb",
      Attribute_Index => Types.No_String);
   Pretty_Print (Project);

end Normalize;

