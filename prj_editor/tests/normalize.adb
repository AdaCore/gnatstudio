
with Prj_Normalize; use Prj_Normalize;
with Prj.Tree;      use Prj.Tree;
with Prj.Part;      use Prj.Part;
with Prj.PP;        use Prj.PP;
with Text_IO;       use Text_IO;

procedure Normalize is
   Name : String := "child1.gpr";
   Project : Project_Node_Id;

begin
   Parse (Project, Name, Always_Errout_Finalize => True);
   Pretty_Print (Project);

   Put_Line ("----- Normalized -----");
   Normalize_Project (Project);
   Pretty_Print (Project);
end Normalize;

