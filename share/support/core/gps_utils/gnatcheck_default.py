gnatcheck_default = """<?xml version="1.0"?>
<gnatcheck>
  <category name ="ada 95 quality and style">
    <category name ="concurrency">
      <check switch="+RMultiple_Entries_In_Protected_Definitions" label="protected definitions with more than one entry"/>
      <check switch="+RVolatile_Objects_Without_Address_Clauses" label="volatile objects with no address clause"/>
    </category>
    <category name ="object oriented features">
      <spin switch="+RDeep_Inheritance_Hierarchies" label="derivation tree is too deep" min="0" max="99999" default="0" separator=":"/>
      <check switch="+RDirect_Calls_To_Primitives" label="non-dispatching calls to primitives"/>
      <spin switch="+RToo_Many_Parents" label="type/object has too many parents" min="0" max="99999" default="0" separator=":"/>
      <check switch="+RVisible_Components" label="Types with publically accessible components"/>
    </category>
    <category name ="portability">
      <check  switch="+RForbidden_Attributes:ALL" label="detect all attributes except explicitly disabled"/>
      <check  switch="+RForbidden_Attributes:GNAT" label="detect all GNAT attributes except explicitly disabled"/>
      <field switch="+RForbidden_Attributes:" label="detect specified attributes (use ',' as separator)"/>
      <field switch="-RForbidden_Attributes:" label="do not detect specified attributes (use ',' as separator)"/>
      <check  switch="+RForbidden_Pragmas:ALL" label="detect all pragmas except explicitly disabled"/>
      <check  switch="+RForbidden_Pragmas:GNAT" label="detect all GNAT pragmas except explicitly disabled"/>
      <field switch="+RForbidden_Pragmas:" label="detect specified pragmas (use ',' as separator)"/>
      <field switch="-RForbidden_Pragmas:" label="do not detect specified pragmas (use ',' as separator)"/>
      <check switch="+RImplicit_SMALL_For_Fixed_Point_Types" label="fixed point type declarations with no 'Small clause"/>
      <check switch="+RPredefined_Numeric_Types" label="explicit references to predefined numeric subtypes"/>
      <check switch="+RSeparate_Numeric_Error_Handlers" label="Numeric_Error and Constraint error are not handled together"/>
    </category>
    <category name ="program structure">
      <spin switch="+RDeeply_Nested_Generics" label="deeply nested generic declarations" min="0" max="99999" default="0" separator=":"/>
      <check switch="+RLocal_Packages" label="local packages"/>
      <check switch="+RNon_Visible_Exceptions" label="potential propagations of non-visible exceptions">
        <tip>Flag constructs leading to the possibility of propagating an
exception out of the scope in which the exception is declared.
Two cases are detected:
* An exception declaration in a subprogram body, task body
or block statement is flagged if the body or statement does not
contain a handler for that exception or a handler with an 
others choice.
* A raise statement in an exception handler of a subprogram
body, task body or block statement is flagged if it (re)raises
a locally declared exception. This may occur under the
following circumstances:
 - it explicitly raises a locally declared exception, or
 - it does not specify an exception name (i.e., it is simply
raise;) and the enclosing handler contains a locally declared
exception in its exception choices.
Renamings of local exceptions are not flagged.</tip>
      </check>
      <check switch="+RRaising_External_Exceptions" label="visibility of exceptions raised by routines declared in library package"/>
    </category>
    <category name ="programming practices">
      <check switch="+RAnonymous_Arrays" label="anonymous array types"/>
      <check switch="+REnumeration_Ranges_In_CASE_Statements" label="enumeration ranges as choices in case statements"/>
      <check switch="+RExceptions_As_Control_Flow" label="exceptions for control flow"/>
      <check switch="+REXIT_Statements_With_No_Loop_Name" label="exit statements with no loop name"/>
      <check switch="+RGOTO_Statements" label="goto statements"/>
      <check switch="+RImproper_Returns" label="improper use of return statements"/>
      <check switch="+RNon_Short_Circuit_Operators" label="use of predefined AND and OR for boolean types"/>
      <check switch="+ROTHERS_In_Aggregates" label="OTHERS choices in aggregates"/>
      <check switch="+ROTHERS_In_CASE_Statements" label="OTHERS choices in case statements"/>
      <check switch="+ROTHERS_In_Exception_Handlers" label="OTHERS choices in exception handlers"/>
      <spin switch="+ROverly_Nested_Control_Structures" label="deep nesting level of control structures" min="0" max="99999" default="0" separator=":"/>
      <check switch="+RPositional_Actuals_For_Defaulted_Generic_Parameters" label="positional generic actuals for defaulted generic parameters"/>
      <check switch="+RPositional_Actuals_For_Defaulted_Parameters" label="positional actuals for defaulted parameters"/>
      <check switch="+RPositional_Components" label="positional components associations in aggregates"/>
      <check switch="+RPositional_Generic_Parameters" label="positional generic associations"/>
      <check switch="+RPositional_Parameters" label="positional associations in subprogram and entry calls"/>
      <check switch="+RUnnamed_Blocks_And_Loops" label="compound statements naming"/>
      <check switch="+RUSE_PACKAGE_Clauses" label="use clause for packages"/>
    </category>
    <category name ="readability">
      <check switch="+RMisnamed_Identifiers:Default" label="identifiers use standard suffixes"/>
      <field switch="+RMisnamed_Identifiers:Type_Suffix" label="suffix for type names (empty string disables check)" separator="=" switch-off="-RMisnamed_Identifiers:Type_Suffix"/>
      <field switch="+RMisnamed_Identifiers:Access_Suffix" label="suffix for access type names  (empty string disables check)" separator="=" switch-off="-RMisnamed_Identifiers:Access_Suffix"/>
      <field switch="+RMisnamed_Identifiers:Constant_Suffix" label="suffix for constant names (empty string disables check)" separator="=" switch-off="-RMisnamed_Identifiers:Constant_Suffix"/>
      <field switch="+RMisnamed_Identifiers:Renaming_Suffix" label="suffix for package renaming names (empty string disables check)" separator="=" switch-off="-RMisnamed_Identifiers:Renaming_Suffix"/>
      <default-value-dependency master-switch="+RMisnamed_Identifiers:Default" slave-switch="+RMisnamed_Identifiers:Type_Suffix=_T"/>
      <default-value-dependency master-switch="+RMisnamed_Identifiers:Default" slave-switch="+RMisnamed_Identifiers:Access_Suffix=_A"/>
      <default-value-dependency master-switch="+RMisnamed_Identifiers:Default" slave-switch="+RMisnamed_Identifiers:Constant_Suffix=_C"/>
      <default-value-dependency master-switch="+RMisnamed_Identifiers:Default" slave-switch="+RMisnamed_Identifiers:Renaming_Suffix=_R"/>
      <field switch="+RName_Clashes" label="restrictions on name space (specify dictionary of forbidden names) " as-file="true"/>
      <check switch="+RUncommented_BEGIN_In_Package_Bodies" label="BEGIN keywords in package bodies non-marked with comment with package name"/>
    </category>
    <category name ="source code presentation">
    </category>
  </category>
  <category name ="feature use detectors">
    <check switch="+RAbstract_Type_Declarations" label="abstract types"/>
    <check switch="+RAnonymous_Subtypes" label="anonymous subtypes"/>
    <check switch="+RBlocks" label="block statements"/>
    <spin switch="+RComplex_Inlined_Subprograms" label="complex inlined subprograms" min="0" max="99999" default="0" separator=":"/>
    <check switch="+RControlled_Type_Declarations" label="controlled types"/>
    <check switch="+RDeclarations_In_Blocks" label="block statements with local declarations"/>
    <check switch="+RDefault_Parameters" label="declarations of default subprogram parameters"/>
    <check switch="+RDiscriminated_Records" label="discriminanted records"/>
    <check switch="+RExplicit_Full_Discrete_Ranges" label="explicit discrete ranges"/>
    <check switch="+RFloat_Equality_Checks" label="equality for float values"/>
    <check switch="+RFunction_Style_Procedures" label="procedures looking like functions"/>
    <check switch="+RGenerics_In_Subprograms" label="definitions of generic units in  subprogram bodies"/>
    <check switch="+RImplicit_IN_Mode_Parameters" label="implicit IN mode in parameter specifications"/>
    <check switch="+RImproperly_Located_Instantiations" label="instantiations that can cause problems"/>
    <check switch="+RLibrary_Level_Subprograms" label="library level subprograms"/>
    <check switch="+RNon_Qualified_Aggregates" label="non-qualified aggregates"/>
    <check switch="+RNumeric_Literals" label="integer literals greater than" hasextraparam="True" separator=":">
      <field label="min nonchecked literal"  default="1"/>
    </check>
    <check switch="+RParameters_Out_Of_Order" label="formal parameters ordering"/>
    <check switch="+RRaising_Predefined_Exceptions" label="explicit raise of predefined exceptions"/>
    <check switch="+RUnassigned_OUT_Parameters" label="OUT parameters do not get values in procedure bodies"/>
    <check switch="+RUnconstrained_Array_Returns" label="functions returning unconstrained arrays"/>
  </category>
  <category name ="metric violations">
    <spin switch="+RMetrics_Cyclomatic_Complexity" label="(metrics) high cyclomatic complexity" min="0" max="99999" default="0" separator=":"/>
    <spin switch="+RMetrics_Essential_Complexity" label="(metrics) high essential complexity" min="0" max="99999" default="0" separator=":"/>
    <spin switch="+RMetrics_LSLOC" label="(metrics) high LSLOC value" min="0" max="99999" default="0" separator=":"/>
  </category>
  <category name ="spark ada subset">
    <check switch="+RBoolean_Relational_Operators" label="comparisons of Boolean values"/>
    <check switch="+RExpanded_Loop_Exit_Names" label="expanded loop names in exit statements"/>
    <check switch="+RNon_SPARK_Attributes" label="attributes that are not from the SPARK subset"/>
    <check switch="+RNon_Tagged_Derived_Types" label="derived types that are not type extensions"/>
    <check switch="+ROuter_Loop_Exits" label="exiting more than one loop at once"/>
    <check switch="+ROverloaded_Operators" label="operator overloading"/>
    <check switch="+RSlices" label="slices"/>
    <check switch="+RUniversal_Ranges" label="ranges with universal integer bounds"/>
  </category>
</gnatcheck>"""
