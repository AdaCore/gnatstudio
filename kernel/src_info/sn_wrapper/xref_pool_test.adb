with SN;            use SN;
with SN.Xref_Pools; use SN.Xref_Pools;
with GNAT.IO;       use GNAT.IO;

procedure Xref_Pool_Test is

   Xrefs : Xref_Pool;

   D  : String := ".";

   F1 : String := "test.c";
   F2 : String := "dir/nested.c";
   F3 : String := "a/twins.c";
   F4 : String := "b/twins.c";

begin
   Init (Xrefs);

   Put_Line (Xref_Filename_For (F1, D, Xrefs).all);
   Put_Line (Xref_Filename_For (F2, D, Xrefs).all);
   Put_Line (Xref_Filename_For (F3, D, Xrefs).all);
   Put_Line (Xref_Filename_For (F4, D, Xrefs).all);

   Save (Xrefs, "XXX"); -- save
   Save (Xrefs, "XXX"); -- overwrite XXX
   Free (Xrefs);
   Load (Xrefs, "XXX");

   Free_Filename_For (F1, D, Xrefs);
   Free_Filename_For (F2, D, Xrefs);
   Free_Filename_For (F3, D, Xrefs);
   Free_Filename_For (F4, D, Xrefs);

   Free (Xrefs);
end Xref_Pool_Test;
