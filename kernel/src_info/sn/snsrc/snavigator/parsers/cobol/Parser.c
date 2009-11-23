/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

#include "Parser.h"

#ifdef __cplusplus
extern "C" {
#include "Reuse.h"
#include "rMemory.h"
#include "General.h"
#include "DynArray.h"
#ifndef NO_RECOVER
#include "Sets.h"
#endif
#if ! defined NO_RECOVER | defined YYReParse
#include "Errors.h"
#endif
#if defined YYDEBUG | defined lex_interface
#include "Position.h"
#endif
#include <stdio.h>
#include <string.h>
}
#else
#include "Reuse.h"
#include "rMemory.h"
#include "General.h"
#include "DynArray.h"
#ifndef NO_RECOVER
#include "Sets.h"
#endif
#if ! defined NO_RECOVER | defined YYReParse
#include "Errors.h"
#endif
#if defined YYDEBUG | defined lex_interface
#include "Position.h"
#endif
#include <stdio.h>
#endif

#ifdef lex_interface
#define GetToken	yylex
     extern int yylex ARGS ((void));
#ifndef AttributeDef
#include "Position.h"
	   typedef struct { tPosition Position; } tScanAttribute;
	   tScanAttribute	Attribute = {{ 0, 0 }};
#endif
#ifndef ErrorAttributeDef
#define ErrorAttribute(Token, RepairAttribute)
#endif
#ifndef yyGetAttribute
#define yyGetAttribute(yyAttrStackPtr, a) * yyAttrStackPtr = yylval
#endif
#else
#include "Scanner.h"
#ifndef yyGetAttribute
#define yyGetAttribute(yyAttrStackPtr, a) (yyAttrStackPtr)->Scan = a
#endif
#endif

typedef unsigned short	yyStateRange	;
typedef unsigned short	yySymbolRange	;
typedef struct { yyStateRange Check, Next; } yytComb;
typedef enum {
yyNT0_intern	= 463,
yyNTprograms	= 464,
yyNTprogram_end_o	= 465,
yyNTprogram_end	= 466,
yyNTprogram_l	= 467,
yyNTprogram	= 468,
yyNTsce	= 469,
yyNTidentification_division	= 470,
yyNTenvironment_division_o	= 471,
yyNTdata_division_o	= 472,
yyNTprocedure_division_o	= 473,
yyNTidentification_o	= 474,
yyNTprogram_id_o	= 475,
yyNTprogram_o	= 476,
yyNTidentification_l	= 477,
yyNTenvironment_division	= 478,
yyNTenvironment_o	= 479,
yyNTconfiguration_section_o	= 480,
yyNTinput_output_section_o	= 481,
yyNTconfiguration_o	= 482,
yyNTconfiguration_l	= 483,
yyNTsource_computer_o	= 484,
yyNTobject_computer_o	= 485,
yyNTmemory_o	= 486,
yyNTmemory_unit	= 487,
yyNTprogram_sequence_o	= 488,
yyNTsegment_limit_o	= 489,
yyNTspecial_names_o	= 490,
yyNTspecial_names_l	= 491,
yyNTimplementor_name_l	= 492,
yyNTimplementor_name_e	= 493,
yyNTimplementor_name	= 494,
yyNTbuiltin_name	= 495,
yyNTon_off	= 496,
yyNTon_status	= 497,
yyNToff_status	= 498,
yyNTalphabet_names	= 499,
yyNTalphabet_entry	= 500,
yyNTalphabet_name	= 501,
yyNTalphabet_l	= 502,
yyNTalphabet_e	= 503,
yyNTalso_l	= 504,
yyNTalphabet_or_class_literal	= 505,
yyNTsymbolic_characters	= 506,
yyNTsymbolic_entry	= 507,
yyNTsymbolic_l	= 508,
yyNTsymbolic_e	= 509,
yyNTsymbolic_character_l	= 510,
yyNTinteger_l	= 511,
yyNTclass_names	= 512,
yyNTclass	= 513,
yyNTclass_l	= 514,
yyNTclass_e	= 515,
yyNTcurrency_o	= 516,
yyNTdecimal_point_o	= 517,
yyNTnumeric_sign_o	= 518,
yyNTcall_convention_o	= 519,
yyNTconsole_o	= 520,
yyNTcursor_o	= 521,
yyNTcrt_status_o	= 522,
yyNTinput_output_o	= 523,
yyNTfile_control	= 524,
yyNTfile_control_o	= 525,
yyNTfile_control_entry_l	= 526,
yyNTfile_control_entry	= 527,
yyNTselect_clause_l	= 528,
yyNTselect_clause	= 529,
yyNTassign_o	= 530,
yyNTassign_l	= 531,
yyNTassign_e	= 532,
yyNTexternal_o	= 533,
yyNTinteger_or_no	= 534,
yyNTorganization_is_o	= 535,
yyNTalternate_l	= 536,
yyNTalternate_e	= 537,
yyNTduplicates_o_2	= 538,
yyNTpassword_o	= 539,
yyNTsuppress_o	= 540,
yyNTlock_o	= 541,
yyNTi_o_control_o	= 542,
yyNTi_o_control_l	= 543,
yyNTi_o_control_e	= 544,
yyNTrerun_e	= 545,
yyNTsame	= 546,
yyNTmultiple_l	= 547,
yyNTmultiple_e	= 548,
yyNTdata_division	= 549,
yyNTdata_o	= 550,
yyNTfile_section_o	= 551,
yyNTfile_o	= 552,
yyNTfile_description_l	= 553,
yyNTfile_description_e	= 554,
yyNTworking_storage_section_o	= 555,
yyNTxx_working_storage_section_o_1_4	= 556,
yyNTlocal_storage_section_o	= 557,
yyNTxx_local_storage_section_o_1_4	= 558,
yyNTlinkage_section_o	= 559,
yyNTxx_linkage_section_o_1_4	= 560,
yyNTcommunication_section_o	= 561,
yyNTxx_communication_section_o_1_4	= 562,
yyNTcommunication_description_l	= 563,
yyNTreport_section_o	= 564,
yyNTxx_report_section_o_1_4	= 565,
yyNTscreen_section_o	= 566,
yyNTxx_screen_section_o_1_4	= 567,
yyNTreport_description_entry_l	= 568,
yyNTreport_group_description_entry_l	= 569,
yyNTfile_description_entry	= 570,
yyNTsort_merge_file_description_entry	= 571,
yyNTfile_clause_l	= 572,
yyNTfile_clause	= 573,
yyNTsort_merge_clause_l	= 574,
yyNTsort_merge_clause	= 575,
yyNTvalue_l	= 576,
yyNTvalue_e	= 577,
yyNTfrom_o	= 578,
yyNTto_o	= 579,
yyNTdepending_o	= 580,
yyNTbottom_o	= 581,
yyNTfooting	= 582,
yyNTtop	= 583,
yyNTbottom	= 584,
yyNTlinage	= 585,
yyNTdata_description_entry_l	= 586,
yyNTdata_description_entry	= 587,
yyNTperiod_o	= 588,
yyNTrenames_tail	= 589,
yyNTredefines_o	= 590,
yyNTname_or_Filler	= 591,
yyNTdata_clause_l	= 592,
yyNTdata_clause	= 593,
yyNTusage	= 594,
yyNTsign_1	= 595,
yyNTseparate_o	= 596,
yyNTa_de_scending_l	= 597,
yyNTindexed_o	= 598,
yyNTindexed_l	= 599,
yyNTcondition_l	= 600,
yyNTdd_condition_e	= 601,
yyNTfalse_o	= 602,
yyNTconst_expression	= 603,
yyNTconst_primary	= 604,
yyNToperator	= 605,
yyNTcommunication_description_entry	= 606,
yyNTcd_input_o	= 607,
yyNTcd_input_l	= 608,
yyNTcd_input_e	= 609,
yyNTcd_output_l	= 610,
yyNTcd_output_e	= 611,
yyNTcd_i_o_o	= 612,
yyNTcd_i_o_l	= 613,
yyNTcd_i_o_e	= 614,
yyNTname_or_filler	= 615,
yyNTreport_description_entry	= 616,
yyNTreport_clause_l	= 617,
yyNTreport_clause	= 618,
yyNTlimit_o	= 619,
yyNTline_o	= 620,
yyNTpage_l	= 621,
yyNTpage_e	= 622,
yyNTreport_group_description_entry	= 623,
yyNTreport_group_clause_l	= 624,
yyNTreport_group_clause	= 625,
yyNTtype	= 626,
yyNTname_or_final	= 627,
yyNTsign_2	= 628,
yyNTreset_o	= 629,
yyNTscreen_description_entry_l	= 630,
yyNTscreen_description_entry	= 631,
yyNTscreen_clause_l	= 632,
yyNTscreen_clause	= 633,
yyNTsign_line_o	= 634,
yyNTprocedure_division	= 635,
yyNTprocedure_head	= 636,
yyNTmnemonic_name_o	= 637,
yyNTusing_o	= 638,
yyNTusing_l	= 639,
yyNTusing_1_e	= 640,
yyNTusing_2_e	= 641,
yyNTdeclaratives_o	= 642,
yyNTd_section_l	= 643,
yyNTsection_l	= 644,
yyNTsection_e	= 645,
yyNTsection_head	= 646,
yyNTsegment_number_o	= 647,
yyNTuse_o	= 648,
yyNTparagraph_l	= 649,
yyNTparagraph_e	= 650,
yyNTparagraph_head	= 651,
yyNTsentence_l	= 652,
yyNTstatement_l	= 653,
yyNTstatement	= 654,
yyNTimperative_statement	= 655,
yyNTstatement_i	= 656,
yyNTaccept	= 657,
yyNTaccept_i	= 658,
yyNTexception_2	= 659,
yyNTaccept_name	= 660,
yyNTaccept_from	= 661,
yyNTescape	= 662,
yyNTexception_or_escape	= 663,
yyNTline_column	= 664,
yyNTfrom_crt_o	= 665,
yyNTfrom_crt	= 666,
yyNTmode_block_o	= 667,
yyNTmode_block	= 668,
yyNTwith_o	= 669,
yyNTwith	= 670,
yyNTwith_l	= 671,
yyNTadd	= 672,
yyNTadd_i	= 673,
yyNTsize_error	= 674,
yyNTgiving	= 675,
yyNTalter	= 676,
yyNTalter_l	= 677,
yyNTalter_e	= 678,
yyNTcall	= 679,
yyNTcall_i	= 680,
yyNTcall_name	= 681,
yyNTcall_using_o	= 682,
yyNTcall_using_l	= 683,
yyNTcall_using_1_e	= 684,
yyNTcall_using_2_e	= 685,
yyNTcall_chain_l	= 686,
yyNTcall_chain_e	= 687,
yyNTgiving_o	= 688,
yyNToverflow	= 689,
yyNTon_overflow	= 690,
yyNTexception	= 691,
yyNTcancel	= 692,
yyNTchain	= 693,
yyNTchain_l	= 694,
yyNTchain_1_e	= 695,
yyNTchain_2_e	= 696,
yyNTclose	= 697,
yyNTclose_l	= 698,
yyNTclose_e	= 699,
yyNTcommit	= 700,
yyNTcompute	= 701,
yyNTcompute_i	= 702,
yyNTequal_2	= 703,
yyNTcontinue	= 704,
yyNTdelete	= 705,
yyNTdelete_i	= 706,
yyNTinvalid	= 707,
yyNTdisable	= 708,
yyNTdevice	= 709,
yyNTdisplay	= 710,
yyNTdisplay_i	= 711,
yyNTdisplay_l	= 712,
yyNTdisplay_2_l	= 713,
yyNTdisplay_2_e	= 714,
yyNTupon_o	= 715,
yyNTdisplay_advancing_o	= 716,
yyNTdisplay_3_l	= 717,
yyNTdisplay_3_e	= 718,
yyNTwith_display_l	= 719,
yyNTdivide	= 720,
yyNTdivide_i	= 721,
yyNTenable	= 722,
yyNTenter	= 723,
yyNTentry	= 724,
yyNTentry_l	= 725,
yyNTentry_1_e	= 726,
yyNTentry_2_e	= 727,
yyNTevaluate	= 728,
yyNTevaluate_i	= 729,
yyNTevaluate_expression_l	= 730,
yyNTevaluate_expression	= 731,
yyNTcase_l	= 732,
yyNTwhen_other_o	= 733,
yyNTwhen_l	= 734,
yyNTwhen_label_l	= 735,
yyNTwhen_label	= 736,
yyNTexpression_or_literal	= 737,
yyNTexamine	= 738,
yyNTexamine_repl	= 739,
yyNTexamine_repl_o	= 740,
yyNTexamine_tally_o	= 741,
yyNTexecute	= 742,
yyNTexhibit	= 743,
yyNTexhibit_o	= 744,
yyNTexit	= 745,
yyNTgiving_2_o	= 746,
yyNTgiving_or_returning	= 747,
yyNTgenerate	= 748,
yyNTgoback	= 749,
yyNTgoto	= 750,
yyNTprocedure_name_l	= 751,
yyNTif	= 752,
yyNTif_i	= 753,
yyNTthen	= 754,
yyNTelse	= 755,
yyNTelse_i	= 756,
yyNTinitialize	= 757,
yyNTreplacing_o	= 758,
yyNTinitialize_replacing_l	= 759,
yyNTinitialize_replacing_e	= 760,
yyNTreplacing_mode	= 761,
yyNTinitiate	= 762,
yyNTinspect	= 763,
yyNTinspect_repl	= 764,
yyNTtallying_l	= 765,
yyNTtallying_e	= 766,
yyNTfor_l	= 767,
yyNTfor_e	= 768,
yyNTall_leading_l	= 769,
yyNTall_leading_e	= 770,
yyNTreplacing_l	= 771,
yyNTreplacing_e	= 772,
yyNTall_leading_first_l	= 773,
yyNTall_leading_first_e	= 774,
yyNTbefore_after_o	= 775,
yyNTmerge	= 776,
yyNTsort_merge_l	= 777,
yyNTsort_merge_e	= 778,
yyNTsequence_o	= 779,
yyNToutput	= 780,
yyNTmove	= 781,
yyNTmultiply	= 782,
yyNTmultiply_i	= 783,
yyNTnext_sentence	= 784,
yyNTon	= 785,
yyNTon_every_o	= 786,
yyNTon_until_o	= 787,
yyNTon_else_o	= 788,
yyNTopen	= 789,
yyNTopen_l	= 790,
yyNTopen_e	= 791,
yyNTfile_name_1_l	= 792,
yyNTfile_name_2_l	= 793,
yyNTfile_name_3_l	= 794,
yyNTfile_name_1	= 795,
yyNTfile_name_2	= 796,
yyNTfile_name_3	= 797,
yyNTfile_name	= 798,
yyNTfile_name_l	= 799,
yyNTperform	= 800,
yyNTperform_body	= 801,
yyNTprocedure	= 802,
yyNTtest_o	= 803,
yyNTvarying_l	= 804,
yyNTvarying_e	= 805,
yyNTpurge	= 806,
yyNTread	= 807,
yyNTread_i	= 808,
yyNTinto_o	= 809,
yyNTwith_read_o	= 810,
yyNTend	= 811,
yyNTend_o	= 812,
yyNTnot_end_o	= 813,
yyNTready_trace	= 814,
yyNTreceive	= 815,
yyNTreceive_i	= 816,
yyNTmessage_segment	= 817,
yyNTdata	= 818,
yyNTrelease	= 819,
yyNTreset_trace	= 820,
yyNTreturn	= 821,
yyNTreturn_i	= 822,
yyNTreturn_end	= 823,
yyNTrewrite	= 824,
yyNTrewrite_i	= 825,
yyNTfrom_2_o	= 826,
yyNTrollback	= 827,
yyNTsearch	= 828,
yyNTsearch_i	= 829,
yyNTvarying_o	= 830,
yyNTsearch_when_l	= 831,
yyNTwhen_e	= 832,
yyNTsearch_l	= 833,
yyNTsearch_e	= 834,
yyNTsend	= 835,
yyNTadvancing_o	= 836,
yyNTadvancing	= 837,
yyNTadvance	= 838,
yyNTsend_replacing_o	= 839,
yyNTservice	= 840,
yyNTset	= 841,
yyNTset_l	= 842,
yyNTset_e	= 843,
yyNTon_off_l	= 844,
yyNTon_off_e	= 845,
yyNTtrue_false_l	= 846,
yyNTtrue_false_e	= 847,
yyNTsort	= 848,
yyNTduplicates_o	= 849,
yyNTinput	= 850,
yyNTstart	= 851,
yyNTstart_i	= 852,
yyNTkey_o	= 853,
yyNTstart_operator	= 854,
yyNTstop	= 855,
yyNTstring_v	= 856,
yyNTstring_i	= 857,
yyNTstring_l	= 858,
yyNTstring_e	= 859,
yyNTdelimiter	= 860,
yyNTpointer_o	= 861,
yyNTsubtract	= 862,
yyNTsubtract_i	= 863,
yyNTsuppress	= 864,
yyNTterminate	= 865,
yyNTtransform	= 866,
yyNTunlock	= 867,
yyNTunstring	= 868,
yyNTunstring_i	= 869,
yyNTdelimited_o	= 870,
yyNTdelimited_l	= 871,
yyNTdelimited_e	= 872,
yyNTtallying_o	= 873,
yyNTunstring_l	= 874,
yyNTunstring_e	= 875,
yyNTuse	= 876,
yyNTexception_or_error	= 877,
yyNTbegin_or_end	= 878,
yyNTuse_files	= 879,
yyNTgiving_use_o	= 880,
yyNTfile_or_reel	= 881,
yyNTuse_l	= 882,
yyNTuse_e	= 883,
yyNTwrite	= 884,
yyNTwrite_i	= 885,
yyNTend_of_page	= 886,
yyNTcopy_o	= 887,
yyNTcopy_or_replace	= 888,
yyNTcopy	= 889,
yyNTcopy_name	= 890,
yyNTcopy_suppress_o	= 891,
yyNTcopy_replacing_o	= 892,
yyNTxx_copy_replacing_o_1_1	= 893,
yyNTcopy_replacing_l	= 894,
yyNTcopy_replacing_e	= 895,
yyNTreplace	= 896,
yyNTxx_replace_1_2	= 897,
yyNTreplace_l	= 898,
yyNTreplace_e	= 899,
yyNTreplacing_item_1	= 900,
yyNTpseudo_text_1	= 901,
yyNTreplacing_item_2	= 902,
yyNTxx_replacing_item_2_2_1	= 903,
yyNTpseudo_text_2	= 904,
yyNTreplacing_item	= 905,
yyNTtoken_l	= 906,
yyNTtoken_e	= 907,
yyNTtoken	= 908,
yyNTcondition	= 909,
yyNTand_condition	= 910,
yyNTnot_condition	= 911,
yyNTprimary_condition	= 912,
yyNTclassification	= 913,
yyNTsign_3	= 914,
yyNTpointer_operand	= 915,
yyNTrelational_operator	= 916,
yyNTequal	= 917,
yyNTgreater	= 918,
yyNTgreater_equal	= 919,
yyNTless	= 920,
yyNTless_equal	= 921,
yyNTis_relational_operator	= 922,
yyNTno_is_relational_operator	= 923,
yyNTrelational_operator_2	= 924,
yyNTexpression	= 925,
yyNTmultiplicative_expression	= 926,
yyNTpower_expression	= 927,
yyNTunary_expression	= 928,
yyNTprimary_expression	= 929,
yyNTidentifier	= 930,
yyNTsubscription	= 931,
yyNTqualification	= 932,
yyNTqualification_f	= 933,
yyNTidentifier_w	= 934,
yyNTsubscription_w	= 935,
yyNTqualification_w	= 936,
yyNTxx_qualification_w_1_1	= 937,
yyNTidentifier_c	= 938,
yyNTqualification_c	= 939,
yyNTidentifier_n	= 940,
yyNTqualification_n	= 941,
yyNTin_of	= 942,
yyNTindex_l	= 943,
yyNTindex	= 944,
yyNTreference_modifier	= 945,
yyNTidentifier_l	= 946,
yyNTidentifier_l_w	= 947,
yyNTidentifier_l_c	= 948,
yyNTqualification_l	= 949,
yyNTqualification_l_f	= 950,
yyNTname_l	= 951,
yyNTname_l_f	= 952,
yyNTidentifier_or_numeric_literal_l	= 953,
yyNTidentifier_or_non_numeric_literal_l	= 954,
yyNTidentifier_rounded_l_w	= 955,
yyNTfunction_call	= 956,
yyNTfunction_name_1	= 957,
yyNTfunction_name_2	= 958,
yyNTargument_l	= 959,
yyNTsymbolic_character	= 960,
yyNTcondition_name	= 961,
yyNTidentifier_rounded_w	= 962,
yyNTidentifier_rounded_c	= 963,
yyNTidentifier_or_literal	= 964,
yyNTidentifier_or_numeric_literal	= 965,
yyNTidentifier_or_non_numeric_literal	= 966,
yyNTidentifier_or_non_all_literal	= 967,
yyNTname_or_literal	= 968,
yyNTidentifier_or_integer	= 969,
yyNTname_	= 970,
yyNTname_f	= 971,
yyNTprocedure_name	= 972,
yyNTchapter_name	= 973,
yyNTinteger	= 974,
yyNTu_integer	= 975,
yyNTliteral	= 976,
yyNTnon_figurative_literal	= 977,
yyNTnon_numeric_literal	= 978,
yyNTconcat_expression	= 979,
yyNTconcat_operand	= 980,
yyNTnumeric_literal	= 981,
yyNTfigurative_non_numeric_literal	= 982,
yyNTfigurative_numeric_literal	= 983,
yyNTnon_figurative_non_numeric_literal	= 984,
yyNTnon_figurative_numeric_literal	= 985,
yyNTnon_all_figurative_literal	= 986,
yyNTnon_all_figurative_non_numeric_literal	= 987,
yyNTnon_all_figurative_numeric_literal	= 988,
yyNTall_figurative_non_numeric_literal	= 989,
yyNTall_figurative_numeric_literal	= 990,
yyNTAdvancing	= 991,
yyNTAre	= 992,
yyNTAreIs	= 993,
yyNTArea	= 994,
yyNTAt	= 995,
yyNTBy	= 996,
yyNTCharacter	= 997,
yyNTCharacters	= 998,
yyNTCollating	= 999,
yyNTContains	= 1000,
yyNTData	= 1001,
yyNTEvery	= 1002,
yyNTFile	= 1003,
yyNTFiller	= 1004,
yyNTFor	= 1005,
yyNTFrom	= 1006,
yyNTIndicate	= 1007,
yyNTIn	= 1008,
yyNTInto	= 1009,
yyNTIs	= 1010,
yyNTKey	= 1011,
yyNTLine	= 1012,
yyNTLines	= 1013,
yyNTMessage	= 1014,
yyNTMode	= 1015,
yyNTNumber	= 1016,
yyNTOf	= 1017,
yyNTOn	= 1018,
yyNTOrder	= 1019,
yyNTPrinting	= 1020,
yyNTProgram	= 1021,
yyNTReferences	= 1022,
yyNTRight	= 1023,
yyNTSign	= 1024,
yyNTSize	= 1025,
yyNTStandard	= 1026,
yyNTStatus	= 1027,
yyNTSymbolic	= 1028,
yyNTTape	= 1029,
yyNTThan	= 1030,
yyNTThen	= 1031,
yyNTTimes	= 1032,
yyNTTo	= 1033,
yyNTWhen	= 1034,
yyNTWith	= 1035,
yyNTTrailing	= 1036,
yyNTSet	= 1037,
yyNTAlternate	= 1038,
yyNTMultiple	= 1039,
yyNTGlobal	= 1040,
yyNTInitial	= 1041,
yyNTOptional	= 1042,
yyNTRecord	= 1043,
yyNTRounded	= 1044,
yyNTEnd_accept	= 1045,
yyNTEnd_add	= 1046,
yyNTEnd_call	= 1047,
yyNTEnd_chain	= 1048,
yyNTEnd_compute	= 1049,
yyNTEnd_delete	= 1050,
yyNTEnd_display	= 1051,
yyNTEnd_divide	= 1052,
yyNTEnd_evaluate	= 1053,
yyNTEnd_if	= 1054,
yyNTEnd_multiply	= 1055,
yyNTEnd_perform	= 1056,
yyNTEnd_read	= 1057,
yyNTEnd_receive	= 1058,
yyNTEnd_return	= 1059,
yyNTEnd_rewrite	= 1060,
yyNTEnd_search	= 1061,
yyNTEnd_start	= 1062,
yyNTEnd_string	= 1063,
yyNTEnd_subtract	= 1064,
yyNTEnd_unstring	= 1065,
yyNTEnd_write	= 1066,
yyNTRELATIVE_Key_Is_name	= 1067,
yyNTON_head	= 1068,
yyNTnot	= 1069,
yyNTALTERNATE_AREA	= 1070,
yyNTON_head_or_WITH_DATA	= 1071,
yyNTname_Message_COUNT	= 1072,
yyNTidentifier_or_numeric_literal_GIVING	= 1073,
yyNTexamine_replacing	= 1074,
yyNTqualification_FROM	= 1075,
yyNTidentifier_replacing	= 1076,
yyNTidentifier_FOR	= 1077,
yyNTCONSOLE_Is_CRT	= 1078,
yyNTimplementor_name_e_head	= 1079,
yyNTsymbolic_e_head	= 1080,
yyNTnon_screen_display	= 1081,
yyNTparentheses_relational_operator	= 1082,
yyNTrelational_operator_3	= 1083,
yyNTin_of_qualification_TIMES	= 1084,
yyNTon_size_error_head	= 1085,
yyNTon_exception_head	= 1086,
yyNTon_overflow_head	= 1087,
yyNTinvalid_head	= 1088,
yyNTat_end_head	= 1089,
yyNTat_end_of_page_head	= 1090,
yyNTon_error_head	= 1091,
yyNTnot_size_error_head	= 1092,
yyNTnot_exception_head	= 1093,
yyNTnot_overflow_head	= 1094,
yyNTnot_invalid_head	= 1095,
yyNTnot_end_head	= 1096,
yyNTnot_end_of_page_head	= 1097,
yyNTsize_error_head	= 1098,
yyNTaccept_tail	= 1099,
yyNTadd_tail	= 1100,
yyNTcall_tail	= 1101,
yyNTcompute_tail	= 1102,
yyNTdelete_tail	= 1103,
yyNTdisplay_tail	= 1104,
yyNTdivide_tail	= 1105,
yyNTmultiply_tail	= 1106,
yyNTread_tail	= 1107,
yyNTreturn_tail	= 1108,
yyNTrewrite_tail	= 1109,
yyNTstart_tail	= 1110,
yyNTstring_tail	= 1111,
yyNTsubtract_tail	= 1112,
yyNTunstring_tail	= 1113,
yyNTwrite_tail	= 1114,
yyNTdescriptions	= 1115,
yyNTdescription_l	= 1116,
yyNT2_release_Trial_2	= 1117,
yyNT5_accept_i_Trial_2	= 1118,
yyNT5_accept_Trial_2	= 1119
} yytNonterminal;
typedef struct { short yyMode; rbool yyActions, yyMessages; } yytControl;

static	yytControl	yyControl	= { 0, rtrue, rtrue };
	rbool		Parser_Debug	= rfalse;

#define yyFirstTerminal	0
#define yyLastTerminal	462
#define yySetSize	463
#define yyFirstSymbol	0
#define yyLastSymbol	1119
#define yyTTableMax	20749
#define yyNTableMax	3595
#define yyStartState	1
#define yyFirstReadState	1
#define yyLastReadState	2217
#define yyFirstReadReduceState	2218
#define yyLastReadReduceState	2887
#define yyFirstReduceState	2888
#define yyLastReduceState	5053
#define yyLastState	5296
#define yyLastStopState	2889
#define YYTDefault
#define YYNDefault
#define YYTrialParse
#define YYGetLook
#define YYDEC_TABLE
#define YYaccDefault

#define yyFirstFinalState	yyFirstReadReduceState

#define ErrorMessages(Messages) yyControl.yyMessages = Messages
#define SemActions(Actions)	 yyControl.yyActions = Actions

#ifdef YYGetLook

#define GetLookahead(k)	yyGetLookahead ((k) - 1, yyTerminal)
#define GetAttribute(k, a)	xxGetAttribute ((k) - 1, yyTerminal, a)

static int yyGetLookahead	ARGS ((int yyk, yySymbolRange yyToken));
static void xxGetAttribute	ARGS ((int yyk, yySymbolRange yyToken,
				tScanAttribute * yyAttribute));

#endif

/* line 5 "Parser.lrk" */


#include <ctype.h>
#include "Position.h"
#include "StringM.h"
#include "Idents.h"
#include "keywdef.h"
#include "keywords.h"
#include "def.h"
#include "deftab.h"
#include "sn.h"

#define yyInitStackSize	200
#define yyInitBufferSize	32

#define TOKENOP	PrevEPos = CurrentEPos; CurrentEPos = Attribute.name.EPos;
#define BEFORE_TRIAL	tPosition SavePEPos, SaveCEPos; SavePEPos = PrevEPos; SaveCEPos = CurrentEPos;
#define AFTER_TRIAL	PrevEPos = SavePEPos; CurrentEPos = SaveCEPos;

extern	rbool		Copy ARGS ((tIdent ident, tPosition pos));

static	tPosition	PrevEPos, CurrentEPos;
static	tIdent		iCURRENT_DATE	;
static	tIdent		iWHEN_COMPILED	;

typedef struct { tScanAttribute Scan; } zzname_or_Filler;
typedef struct { tIdent Ident; tPosition Pos; } zzcopy_name;
typedef struct { tpdecl decl; tScanAttribute Scan; } zzqualification;
typedef struct { tScanAttribute Scan; } zzqualification_n;
typedef struct { tScanAttribute Scan; } zzchapter_name;
typedef struct { long Value; } zzinteger;
typedef struct { long Value; tScanAttribute Scan; } zzu_integer;

typedef union {
tScanAttribute Scan;
zzname_or_Filler name_or_Filler;
zzcopy_name copy_name;
zzqualification qualification;
zzqualification_n qualification_n;
zzchapter_name chapter_name;
zzinteger integer;
zzu_integer u_integer;
} tParsAttribute;


#if defined lex_interface & ! defined yylvalDef
     tParsAttribute yylval;
#endif

#ifndef yyInitStackSize
#define yyInitStackSize	100
#endif
#ifndef ERROR
#define ERROR
#endif
#define yyNoState		0
#define yystandard		1
#define yytrial		2
#define yybuffer		4
#define yyreparse		8
#define yyS			yySynAttribute
#define yyA			yyAttrStackPtr
#define YYACCEPT		goto yyAccept
#define YYABORT		goto yyAbort

#ifdef YYDEC_TABLE
#define yyDecrement(x)
#define yySetNT(x)
#else
#define yyDecrement(x)		yyStateStackPtr -= x; yyAttrStackPtr -= x;
#define yySetNT(x)		yyNonterminal = x;
#endif

#ifdef YYNDefault
#define yytNComb yytComb
#else
#define yytNComb yyStateRange
#endif

#if defined YYDEBUG | defined YYDCRP
static	long		yyCount		= 0;
static	FILE *		yyTrace		;

static	void		yyPrintState	ARGS ((yyStateRange yyState));
static	void		yyNl		ARGS ((void));

static	char *		yyRule		[] = { 0,
""
};
#endif
	char *		Parser_TokenName	[yyLastTerminal + 2] = {
"_EOF_",
"name",
"paragraph_name",
"unsigned_integer",
"plus_integer",
"minus_integer",
"level_number",
"real",
"string",
"pseudo_text",
"picture_string",
"illegal_character",
".",
"ACCEPT",
"ACCESS",
"ADD",
"ADDRESS",
"ADVANCING",
"AFTER",
"ALL",
"ALPHABET",
"ALPHABETIC",
"ALPHANUMERIC",
"ALPHANUMERIC-EDITED",
"ALSO",
"ALTER",
"ALTERNATE",
"AND",
"ANY",
"APPLY",
"ARE",
"AREA",
"AREAS",
"ASCENDING",
"ASSIGN",
"AT",
"AUTHOR",
"AUTO",
"AUTO-SKIP",
"AUTOMATIC",
"BACKGROUND-COLOR",
"BEEP",
"BEFORE",
"BEGINNING",
"BELL",
"BINARY",
"BLANK",
"BLINK",
"BLOCK",
"BOTTOM",
"BY",
"CALL",
"CALL-CONVENTION",
"CANCEL",
"CD",
"CF",
"CH",
"CHAIN",
"CHAINING",
"CHANGED",
"CHARACTER",
"CHARACTERS",
"CLASS",
"CLOCK-UNITS",
"CLOSE",
"CODE",
"CODE-SET",
"COL",
"COLLATING",
"COLUMN",
"COMMA",
"COMMIT",
"COMMON",
"COMMUNICATION",
"COMPUTATIONAL",
"COMPUTATIONAL-0",
"COMPUTATIONAL-1",
"COMPUTATIONAL-2",
"COMPUTATIONAL-3",
"COMPUTATIONAL-4",
"COMPUTATIONAL-5",
"COMPUTATIONAL-6",
"COMPUTATIONAL-X",
"COMPUTE",
"CONFIGURATION",
"CONSOLE",
"CONTAINS",
"CONTENT",
"CONTINUE",
"CONTROL",
"CONTROLS",
"CONVERTING",
"COPY",
"CORE-INDEX",
"CORRESPONDING",
"COUNT",
"CRT",
"CRT-UNDER",
"CURRENCY",
"CURSOR",
"CYCLE",
"DATA",
"DATE",
"DATE-COMPILED",
"DATE-WRITTEN",
"DAY",
"DAY-OF-WEEK",
"DBCS",
"DEBUGGING",
"DECIMAL-POINT",
"DECLARATIVES",
"DELETE",
"DELIMITED",
"DELIMITER",
"DEPENDING",
"DESCENDING",
"DESTINATION",
"DETAIL",
"DISABLE",
"DISK",
"DISP",
"DISPLAY",
"DISPLAY-1",
"DIVIDE",
"DIVISION",
"DOWN",
"DUPLICATES",
"DYNAMIC",
"ELSE",
"EMPTY-CHECK",
"ENABLE",
"END",
"END-ACCEPT",
"END-ADD",
"END-CALL",
"END-CHAIN",
"END-COMPUTE",
"END-DELETE",
"END-DISPLAY",
"END-DIVIDE",
"END-EVALUATE",
"END-IF",
"END-MULTIPLY",
"END-OF-PAGE",
"END-PERFORM",
"END-READ",
"END-RECEIVE",
"END-RETURN",
"END-REWRITE",
"END-SEARCH",
"END-START",
"END-STRING",
"END-SUBTRACT",
"END-UNSTRING",
"END-WRITE",
"ENDING",
"ENTER",
"ENTRY",
"ENVIRONMENT",
"EOL",
"EOS",
"EQUAL",
"EQUALS",
"ERASE",
"ERROR",
"ESCAPE",
"EVALUATE",
"EVERY",
"EXAMINE",
"EXCEEDS",
"EXCEPTION",
"EXCLUSIVE",
"EXECUTE",
"EXHIBIT",
"EXIT",
"EXTEND",
"EXTERNAL",
"FALSE",
"FD",
"FILE",
"FILE-CONTROL",
"FILE-ID",
"FILLER",
"FINAL",
"FIRST",
"FIXED",
"FOOTING",
"FOR",
"FOREGROUND-COLOR",
"FROM",
"FULL",
"FUNCTION",
"GENERATE",
"GIVING",
"GLOBAL",
"GO",
"GOBACK",
"GREATER",
"GRID",
"GROUP",
"HEADING",
"HIGH-VALUE",
"HIGHLIGHT",
"I-O",
"I-O-CONTROL",
"IDENTIFICATION",
"IF",
"IGNORE",
"IN",
"INDEX",
"INDEXED",
"INDICATE",
"INITIAL",
"INITIALIZE",
"INITIATE",
"INPUT",
"INPUT-OUTPUT",
"INSPECT",
"INSTALLATION",
"INTO",
"INVALID",
"IS",
"JUSTIFIED",
"KEPT",
"KEY",
"KEYBOARD",
"LABEL",
"LAST",
"LEADING",
"LEFT",
"LEFT-JUSTIFY",
"LEFTLINE",
"LENGTH",
"LENGTH-CHECK",
"LESS",
"LIMIT",
"LIMITS",
"LINAGE",
"LINE",
"LINES",
"LINKAGE",
"LOCAL-STORAGE",
"LOCK",
"LOW-VALUE",
"LOWER",
"LOWLIGHT",
"MANUAL",
"MEMORY",
"MERGE",
"MESSAGE",
"MODE",
"MODULES",
"MOVE",
"MULTIPLE",
"MULTIPLY",
"NAME",
"NAMED",
"NATIONAL",
"NATIONAL-EDITED",
"NEGATIVE",
"NEXT",
"NO",
"NO-ECHO",
"NOT",
"NULL",
"NULLS",
"NUMBER",
"NUMERIC",
"NUMERIC-EDITED",
"OBJECT-COMPUTER",
"OCCURS",
"OF",
"OFF",
"OMITTED",
"ON",
"OPEN",
"OPTIONAL",
"OR",
"ORDER",
"ORGANIZATION",
"OTHER",
"OUTPUT",
"OVERFLOW",
"OVERLINE",
"PACKED-DECIMAL",
"PADDING",
"PAGE",
"PARAGRAPH",
"PASSWORD",
"PERFORM",
"PF",
"PH",
"PICTURE",
"PLUS",
"POINTER",
"POSITION",
"POSITIONING",
"POSITIVE",
"PREVIOUS",
"PRINTER",
"PRINTER-1",
"PRINTING",
"PROCEDURE",
"PROCEDURE-POINTER",
"PROCEDURES",
"PROCEED",
"PROGRAM",
"PROGRAM-ID",
"PROMPT",
"PURGE",
"QUEUE",
"QUOTE",
"RANDOM",
"RD",
"READ",
"READY",
"RECEIVE",
"RECORD",
"RECORD-OVERFLOW",
"RECORDING",
"RECORDS",
"REDEFINES",
"REEL",
"REFERENCE",
"REFERENCES",
"RELATIVE",
"RELEASE",
"RELOAD",
"REMAINDER",
"REMARKS",
"REMOVAL",
"RENAMES",
"REORG-CRITERIA",
"REPLACE",
"REPLACING",
"REPORT",
"REPORTING",
"REPORTS",
"REQUIRED",
"RERUN",
"RESERVE",
"RESET",
"RETURN",
"RETURNING",
"REVERSE-VIDEO",
"REVERSED",
"REWIND",
"REWRITE",
"RF",
"RH",
"RIGHT",
"RIGHT-JUSTIFY",
"ROLLBACK",
"ROUNDED",
"RUN",
"SAME",
"SCREEN",
"SD",
"SEARCH",
"SECTION",
"SECURE",
"SECURITY",
"SEGMENT",
"SEGMENT-LIMIT",
"SELECT",
"SEND",
"SENTENCE",
"SEPARATE",
"SEQUENCE",
"SEQUENTIAL",
"SERVICE",
"SET",
"SIGN",
"SIZE",
"SORT",
"SORT-MERGE",
"SOURCE",
"SOURCE-COMPUTER",
"SPACE",
"SPACE-FILL",
"SPECIAL-NAMES",
"STANDARD",
"STANDARD-1",
"START",
"STATUS",
"STOP",
"STRING",
"SUB-QUEUE-1",
"SUB-QUEUE-2",
"SUB-QUEUE-3",
"SUBTRACT",
"SUM",
"SUPPRESS",
"SYMBOLIC",
"SYNCHRONIZED",
"TABLE",
"TALLYING",
"TAPE",
"TERMINAL",
"TERMINATE",
"TEST",
"TEXT",
"THAN",
"THEN",
"THROUGH",
"TIME",
"TIME-OUT",
"TIMES",
"TO",
"TOP",
"TRACE",
"TRAILING",
"TRAILING-SIGN",
"TRANSFORM",
"TRUE",
"TYPE",
"UNDERLINE",
"UNEQUAL",
"UNLOCK",
"UNSTRING",
"UNTIL",
"UP",
"UPDATE",
"UPON",
"UPPER",
"USAGE",
"USE",
"USER",
"USING",
"VALUE",
"VALUES",
"VARIABLE",
"VARYING",
"WAIT",
"WHEN",
"WITH",
"WORDS",
"WORKING-STORAGE",
"WRITE",
"WRITE-ONLY",
"ZERO",
"ZERO-FILL",
"ZEROS",
"=",
"<",
">",
"(",
")",
"+",
"<>",
">=",
"<=",
"66",
"77",
"78",
"88",
"-",
":",
"&",
"**",
"*",
"/",
"==",
""
};
static	yytComb		yyTComb		[yyTTableMax + 1] = {
{  48, 2889}, { 675,  852}, { 675, 2696}, { 675, 2699}, { 675, 2697}, 
{ 675, 2698}, { 675, 2700}, {1411, 4760}, { 462, 2935}, {1899,   57}, 
{1899, 1897}, {1899, 2699}, {2078, 2263}, { 675,  853}, {1899, 2700}, 
{ 675,  854}, {1657, 4772}, {1251, 4790}, { 104, 2934}, {2078, 2705}, 
{1899, 4380}, {2178,   57}, {  46, 2888}, {1853, 2693}, { 155, 3315}, 
{ 675,  645}, {1103, 2562}, {1899, 1898}, {2178, 2406}, { 354, 2693}, 
{ 998, 2693}, { 155, 3315}, {1251, 1496}, {1975, 4627}, { 992, 1212}, 
{ 155, 2309}, {  35, 2725}, {1411, 2724}, { 820,  817}, { 597, 2694}, 
{ 820, 2699}, { 820, 2697}, { 820, 2698}, { 820, 2700}, { 820, 2706}, 
{ 820, 2705}, {1121, 4808}, {1054, 2540}, {1054, 2541}, {1054, 2543}, 
{1103, 2561}, { 675,  855}, { 295, 2721}, { 675,  647}, {1866, 4792}, 
{1854, 2693}, { 820,  183}, { 675,  648}, { 992, 4852}, { 348, 2693}, 
{ 353, 2693}, { 490, 2321}, {  42, 2725}, {1434,  928}, { 675,  649}, 
{1292, 2540}, {1292, 2541}, {1292, 2543}, { 930, 4764}, {1253, 1245}, 
{1980, 2725}, { 675, 2485}, {1374,   57}, { 244,  314}, {1374, 2699}, 
{1374, 2697}, {1374, 2698}, {1374, 2700}, { 354,  464}, { 348, 2658}, 
{2178, 2201}, {2001, 2250}, {2001, 2251}, { 675,  856}, {1048, 4066}, 
{1048, 4066}, {1048, 4066}, { 502, 4758}, { 675, 2489}, {1048, 4066}, 
{ 490, 2322}, { 490, 2326}, { 490, 2327}, { 490, 2328}, { 490, 2329}, 
{ 490, 2330}, { 490, 2331}, { 490, 2332}, { 490, 2333}, { 228, 2350}, 
{1395, 2725}, {1657, 2730}, {  75, 2797}, {1253, 2726}, {  75, 2699}, 
{  75, 2697}, {  75, 2698}, {  75, 2700}, {  75, 2706}, { 353,  464}, 
{ 365, 4792}, { 675,  857}, { 912,  107}, {1856, 2693}, { 998, 2430}, 
{ 244,  315}, { 502, 2722}, { 930, 2726}, { 675,  652}, { 519, 4758}, 
{  75,   60}, { 675,  858}, {1165,  928}, { 675,  859}, { 992, 4782}, 
{ 998, 2442}, {1740, 2693}, { 717, 2980}, {1121, 4808}, { 867,   57}, 
{ 675,  655}, { 998, 2431}, {  35, 4762}, {1054, 2547}, { 998, 2432}, 
{ 998, 2433}, {1873, 2768}, { 490, 2323}, { 490, 2335}, {1121, 4842}, 
{1253, 4764}, {1873, 4848}, {1873, 4848}, {1149, 3005}, { 675, 2560}, 
{1434,  929}, {  35, 2880}, { 867, 1089}, { 519, 2722}, {2207, 1166}, 
{1623, 4832}, {1292, 2547}, { 228, 4842}, { 576, 2627}, { 576, 2629}, 
{ 576, 2699}, { 576, 2697}, { 576, 2698}, { 576, 2700}, { 576, 2630}, 
{ 576, 2631}, {1921,  437}, { 576, 2632}, { 576, 2633}, { 576, 2628}, 
{ 348, 2659}, { 675,  860}, { 891, 4627}, { 675,  659}, {1781, 2725}, 
{  42, 4762}, { 576, 2634}, { 675, 2530}, { 675,  660}, { 675,  661}, 
{ 820, 2780}, { 228,  293}, { 228,  294}, {1980, 4762}, { 228,  224}, 
{  81,   57}, {  42, 2887}, { 891,  890}, { 717, 2980}, {1975, 2793}, 
{1921,  438}, {1866, 4792}, { 853, 5099}, {1253, 2777}, {1980, 2795}, 
{2078, 5288}, { 675,   61}, { 675,  662}, {1411, 4760}, { 998, 1219}, 
{ 675,  663}, { 675,  664}, {1048, 4066}, { 912,  224}, { 998, 1220}, 
{2078, 2129}, {1452, 2603}, { 576, 2635}, { 992, 2735}, {1165,  929}, 
{ 294, 4758}, { 675,  861}, {1251, 4790}, {1395, 4762}, { 412, 3375}, 
{ 992, 1213}, {2178,   61}, { 973, 2725}, { 675,  666}, { 675,  667}, 
{ 545,  627}, { 545,  628}, { 675,  668}, {1904, 2440}, { 348,  455}, 
{1132, 1392}, {  35, 2863}, {  94, 2725}, {1904, 2439}, { 412, 2344}, 
{ 490, 2324}, { 973, 4762}, { 206,   57}, { 820,   61}, { 206, 2699}, 
{ 206, 2697}, { 206, 2698}, { 206, 2700}, { 206, 2706}, { 294, 2722}, 
{1251, 2739}, {1866, 2740}, {1132, 2725}, { 820, 2708}, {1346, 1588}, 
{1434,  930}, {2178, 2202}, {2050, 2725}, { 295, 2353}, {1132, 1393}, 
{ 206,   60}, { 283,  365}, {  42, 2863}, { 675,  669}, { 545,  629}, 
{ 365, 4792}, { 532, 4846}, { 675,  670}, { 283,  366}, { 675,  862}, 
{ 348,  456}, {1346, 2725}, { 295,  397}, { 295,  398}, { 365, 2740}, 
{ 675,  672}, {1866, 4792}, {1374,   61}, {1346, 1393}, {  35,   94}, 
{1781, 4762}, { 901, 2728}, { 998, 1221}, { 239,   57}, { 820,  818}, 
{ 239, 2699}, { 239, 2697}, { 239, 2698}, { 239, 2700}, { 675,  673}, 
{ 675,  674}, {2170, 4792}, { 545, 2372}, { 912, 2530}, {1781, 5242}, 
{ 820, 2709}, { 228, 4792}, { 244,  316}, {1054, 2545}, {1054, 2546}, 
{1395, 1502}, { 239, 2689}, {1121, 5134}, { 918, 2273}, { 675,  675}, 
{  42,   99}, { 891, 1118}, { 348,  457}, {1054, 2542}, {1054, 2544}, 
{1975, 1481}, { 187,  185}, { 187, 2678}, {1980, 1647}, {1165,  930}, 
{ 490, 2325}, {1292, 2545}, {1292, 2546}, {1374, 1618}, { 187, 2680}, 
{ 918, 1139}, {1975, 5266}, { 502, 2723}, { 228, 2740}, { 675,  676}, 
{ 490, 2334}, {1292, 2542}, {1292, 2544}, {1452, 2604}, { 675,  863}, 
{ 675,  678}, { 675,  864}, { 348, 2661}, {  94, 4762}, { 490, 2336}, 
{1472, 2445}, {1472, 2446}, {1452, 2602}, {1472, 1463}, {1472, 2448}, 
{ 348, 2657}, { 675,  680}, {1472, 2447}, {1452, 2601}, {1867, 3121}, 
{1472, 2449}, { 260,  181}, { 187,  186}, { 260, 2699}, { 260, 2697}, 
{ 260, 2698}, { 260, 2700}, { 260, 2706}, { 260, 2705}, { 519, 2723}, 
{1995, 2086}, { 675,  681}, { 675,  865}, { 576, 2636}, {1134,  107}, 
{1132, 4762}, { 260,  338}, { 675,  866}, { 820, 2710}, { 260,  183}, 
{2050, 4762}, { 359,  224}, { 675, 2581}, { 972, 4798}, {1295, 1534}, 
{ 348, 2660}, {1132, 2795}, {1867, 3121}, { 675,  867}, { 502, 4758}, 
{ 576, 2637}, {2050, 5286}, { 717,  921}, {1149, 3005}, {1346, 4762}, 
{ 675,  685}, {1716, 1907}, { 972, 4798}, {1716, 1908}, { 187,  188}, 
{ 675,  686}, { 675,  687}, {1472, 1464}, { 228,  295}, { 675,  688}, 
{1346, 5178}, {1253, 4764}, {1211, 5139}, {1211, 5140}, {1211, 5141}, 
{ 431, 4792}, { 431, 4792}, {1211, 5142}, { 675,  868}, { 576, 2638}, 
{ 675,  690}, { 675,  869}, { 294, 4758}, {1211, 2418}, {1211,  643}, 
{ 675,  870}, {1211,  644}, { 675,  693}, {1891,  928}, { 901, 4768}, 
{2156, 2263}, {1295, 1535}, {1781, 5243}, { 187,  247}, { 675,  694}, 
{ 675, 4842}, {1211,  645}, {2156, 2705}, {  55, 2805}, {  55, 2806}, 
{1975, 4808}, { 440,   57}, {  94, 2870}, { 440, 2699}, { 440, 2697}, 
{ 440, 2698}, { 440, 2700}, {1472, 2458}, { 675,  695}, {1149, 1419}, 
{ 820, 2707}, {1149, 1420}, { 206,   61}, { 675,  696}, { 675,  871}, 
{ 675, 4190}, { 717, 2980}, {1132, 1260}, { 576, 2639}, { 440, 2689}, 
{ 294, 2723}, {1623, 4832}, {1211,  646}, { 687, 4627}, {1211,  647}, 
{1134,  224}, { 359, 2530}, { 675, 4190}, {1211,  648}, { 998, 2434}, 
{ 675, 2765}, {1867, 1996}, { 359,  109}, { 675,  872}, { 912,  284}, 
{1211,  649}, {1346, 1260}, { 809, 4842}, { 687,  890}, {1867, 2256}, 
{ 532, 2767}, { 545,  630}, {1211, 2485}, {1121, 5135}, { 190,  256}, 
{ 545, 2374}, { 545, 2371}, { 675, 2701}, { 675, 2702}, { 675, 2703}, 
{ 675, 2704}, { 998, 1222}, {1900, 2687}, { 206,  133}, {1211,  650}, 
{1899, 2701}, {1899, 2702}, {1899, 2703}, {1899, 2704}, {1211, 2489}, 
{1132, 1395}, {2170, 4792}, { 502, 4758}, {1211,  224}, {1092,   57}, 
{2050, 5287}, {1472, 1465}, {1995, 2262}, {1472, 2450}, { 412, 2345}, 
{1891,  929}, {1092, 2705}, { 820, 2711}, { 717, 2980}, { 820, 2712}, 
{ 431, 2740}, {1472, 2452}, { 532, 4846}, { 717, 2980}, {1346, 5179}, 
{1472, 2453}, { 427, 4792}, {1211,  651}, {1867, 3121}, { 820, 2701}, 
{ 820, 2702}, { 820, 2703}, { 820, 2704}, {  31, 2876}, {1211,  652}, 
{ 545,  631}, {2170, 2740}, {1211,  653}, { 348, 2662}, {1211,  654}, 
{ 348, 2663}, { 348,  458}, { 348,  459}, { 348,  460}, { 348,  461}, 
{ 397, 2722}, {1211,  655}, {1211, 3585}, { 545, 2376}, { 545, 2369}, 
{1134, 2530}, { 293, 4758}, {1874, 5251}, {1472, 2463}, {1472, 2455}, 
{  99, 2725}, {1472, 2451}, {1874, 3119}, {1874, 3119}, {1295, 1536}, 
{1900, 2688}, { 260,   61}, { 228, 2765}, {1374, 2701}, {1374, 2702}, 
{1374, 2703}, {1374, 2704}, {1472, 2469}, {1472, 2454}, { 918, 1140}, 
{1578, 2725}, { 260, 2708}, {1211,  656}, {1211,  657}, {1312, 2693}, 
{1048, 4066}, {1048, 4066}, {1048, 4066}, {1048, 4066}, { 988,   57}, 
{ 293, 2722}, {  75, 2711}, {1211,  658}, {  75, 2712}, {1211,  659}, 
{1472, 2461}, { 988, 2705}, {1995, 2087}, {1211, 2530}, {1211,  660}, 
{1211,  661}, {1623, 2760}, { 187,  191}, {  75, 2701}, {  75, 2702}, 
{  75, 2703}, {  75, 2704}, { 972, 2743}, { 912,  112}, { 912,  113}, 
{ 912,  114}, { 912,  115}, { 260,  133}, {1295, 1537}, { 809, 4842}, 
{ 589, 3414}, {1472, 2456}, {1851,  224}, {1211,  662}, {1590, 2725}, 
{1891,  930}, {1211,  663}, {1211,  664}, { 260, 2709}, { 891, 1119}, 
{ 616, 2693}, {  31, 2863}, { 922, 2985}, { 187,  192}, { 187,  193}, 
{ 187,  194}, { 190, 2665}, {1211,  665}, { 809, 4842}, { 187, 2677}, 
{ 187, 2681}, { 187, 2679}, { 891, 1120}, { 190, 2667}, {1211,  666}, 
{1211,  667}, {1472, 1466}, { 359,  284}, {1211,  668}, { 260, 2664}, 
{ 576, 2652}, { 576, 2649}, { 576, 2653}, { 576, 2641}, { 576, 2642}, 
{ 576, 2645}, { 576, 2651}, { 576, 2654}, { 576, 2650}, { 576, 2701}, 
{ 576, 2702}, { 576, 2703}, { 576, 2704}, { 576, 2646}, { 576, 2648}, 
{ 576, 2640}, { 576, 2644}, { 576, 2643}, { 576, 2647}, { 576, 2624}, 
{ 918, 1141}, { 359,  111}, { 465, 3572}, {1312, 2430}, {  31,   92}, 
{2126, 1166}, {1472, 2457}, {2139, 2557}, {  99, 4762}, {1211,  669}, 
{ 364,  478}, {1578, 4762}, {1472, 2459}, {1211,  670}, {1312, 2442}, 
{1211,  671}, { 918, 1142}, { 853, 1070}, { 922, 4814}, {1472, 2464}, 
{1312, 2431}, {1211,  672}, { 291, 4854}, {1312, 2432}, {1312, 2433}, 
{ 190, 2740}, { 260, 2710}, {1672, 4794}, {1472, 2460}, { 809, 1019}, 
{ 412, 2340}, { 291, 2770}, {1851, 2530}, {  83, 2725}, { 291, 4854}, 
{1211,  673}, {1211,  674}, {1985, 2072}, { 412, 2341}, { 566,  107}, 
{ 465,  464}, {1472, 1467}, { 412, 2342}, { 412, 2343}, {1092,   61}, 
{1228, 2445}, {1228, 2446}, {1472, 2465}, {1228, 1463}, {1228, 2448}, 
{1211,  675}, { 206, 2711}, {1228, 2447}, { 206, 2712}, {1092, 2708}, 
{1228, 2449}, {1134,  284}, { 206,  134}, { 206, 2690}, { 206,  135}, 
{1552, 2774}, { 427, 4792}, {1590, 4762}, { 206, 2701}, { 206, 2702}, 
{ 206, 2703}, { 206, 2704}, { 206,  136}, {  34, 2879}, { 589,  741}, 
{1211,  676}, { 352, 2693}, {1092, 1342}, {1590, 5225}, {1472, 1468}, 
{1211,  677}, {1211,  678}, {1211,  679}, { 293, 4758}, { 804,  727}, 
{1472, 2466}, { 397, 2723}, { 352, 3570}, {1312, 1219}, {1472, 2462}, 
{  99, 2870}, { 804, 2478}, {1211,  680}, {1312, 1220}, {  90, 2866}, 
{1472, 2467}, { 427, 2740}, {1472, 2468}, { 260, 2707}, {1211,  284}, 
{  90, 4807}, {1092, 2709}, {1228, 1464}, {1552, 4808}, { 239, 2683}, 
{ 291,  382}, {1057, 4808}, {1211,  681}, {1211,  682}, { 239, 2701}, 
{ 239, 2702}, { 239, 2703}, { 239, 2704}, {1211,  683}, {1472, 2470}, 
{1994, 4778}, {1272, 4627}, { 342,  455}, {1211, 2581}, { 988,   61}, 
{1997, 2088}, { 293, 2723}, {1994, 4778}, { 364,  479}, {1211,  684}, 
{1338, 5173}, { 364,  480}, {1997, 2705}, {1985,  224}, { 988, 2708}, 
{ 566,  224}, {1211,  685}, { 381, 2619}, { 352, 3570}, {  83, 4762}, 
{2126, 4842}, {1211,  686}, {1211,  687}, {1997,  516}, { 381, 2620}, 
{1211,  688}, {1057, 2729}, {1228, 2458}, { 993, 5110}, { 993, 5111}, 
{ 993, 5112}, {  83, 4762}, { 809, 5094}, { 993, 5113}, {1211,  689}, 
{ 752,  957}, {1211,  690}, {1211,  691}, { 342,  456}, { 993, 2418}, 
{ 993,  643}, {1211,  692}, { 993,  644}, {1211,  693}, { 852, 5097}, 
{ 260, 2711}, {1312, 1221}, { 260, 2712}, { 548,   57}, {1052, 1291}, 
{1211,  694}, { 260,  134}, { 993,  645}, { 260,  135}, {1092, 2710}, 
{ 548, 2705}, { 988, 2709}, { 260, 2701}, { 260, 2702}, { 260, 2703}, 
{ 260, 2704}, { 260,  136}, {1052, 2783}, {1851,  110}, {1211,  695}, 
{1134,  112}, {1134,  113}, {1134,  114}, {1134,  115}, {1211,  696}, 
{1211,  697}, {2139, 2608}, {1590, 5226}, {1057, 4808}, {  82, 2693}, 
{ 342,  457}, {1736, 4627}, { 589,  742}, { 993,  646}, {  34,   86}, 
{ 993,  647}, {1552, 5193}, {1338, 1260}, {1851,  284}, { 993,  648}, 
{1671, 4794}, {1228, 1465}, {1763, 5237}, {1228, 2450}, {1211,  698}, 
{  34, 2748}, { 993,  649}, {1552, 5194}, {1985, 2530}, {  82, 2658}, 
{ 566, 2530}, {1228, 2452}, {  90, 4807}, { 993, 2485}, {2139, 2607}, 
{1228, 2453}, { 752,  958}, {1198, 4798}, {1211, 5143}, {1211, 5144}, 
{1211, 5145}, {1211, 5146}, { 852, 5098}, {  83, 2870}, { 467, 3571}, 
{ 993,  650}, {2139, 2605}, { 752,  959}, { 752,  960}, { 752,  961}, 
{ 993, 2489}, {1198, 4798}, { 441,  338}, {1994, 2078}, { 993,  224}, 
{1994, 2079}, {1092, 2707}, { 616, 2301}, {2128, 2263}, { 988, 2710}, 
{1338, 5174}, {1844, 4788}, { 440,  550}, {1228, 2463}, {1228, 2455}, 
{2128, 2705}, {1228, 2451}, { 440, 2701}, { 440, 2702}, { 440, 2703}, 
{ 440, 2704}, {1576, 4842}, {1672, 4794}, { 993,  651}, {1272, 2781}, 
{1672, 2741}, { 417, 2356}, {1228, 2469}, {1228, 2454}, {2130, 2263}, 
{ 993,  652}, { 922, 2751}, { 467,  464}, { 993,  653}, {1576, 1260}, 
{ 993,  654}, {2130, 2705}, {1576, 4842}, {1576, 1779}, {1014, 2695}, 
{1014, 2696}, {1014, 2699}, { 993,  655}, { 993, 3583}, {1014, 2700}, 
{1228, 2461}, { 416, 2693}, {  83, 2748}, { 416, 2699}, { 416, 2697}, 
{ 416, 2698}, { 416, 2700}, { 416, 2706}, { 416, 2705}, { 604, 2693}, 
{  83, 4808}, {1576, 4842}, { 416, 3374}, { 605, 2693}, {1326, 5168}, 
{ 417,  418}, { 804, 2687}, {2139, 2606}, {1994, 2733}, { 416,  183}, 
{  82, 2659}, {1228, 2456}, {1109, 4214}, { 993,  656}, { 993,  657}, 
{ 624, 2699}, { 624, 2697}, { 624, 2698}, { 624, 2700}, {  34, 4808}, 
{1576, 4842}, { 988, 2707}, {1576, 5219}, { 993,  658}, { 725,  928}, 
{ 993,  659}, { 638, 2693}, {1671, 1870}, { 417,  419}, { 993, 2530}, 
{ 993,  660}, { 993,  661}, { 400, 2693}, { 922, 2985}, {2095, 2557}, 
{1997, 2708}, {1228, 1466}, { 638, 3578}, { 922, 4814}, { 400, 2705}, 
{ 184,  185}, { 184, 2678}, { 589,  743}, {1312, 2434}, {1736, 2781}, 
{2095, 4385}, { 596, 2694}, {1763, 5238}, { 184, 2680}, { 993,  662}, 
{ 400,  516}, { 589, 2758}, { 993,  663}, { 993,  664}, {1994, 2080}, 
{1400,  107}, { 589, 4828}, {1397, 2725}, {1763, 5239}, { 589,  744}, 
{ 616, 2300}, { 521, 4758}, {1057, 2748}, { 993,  665}, {  82, 2798}, 
{1312, 1222}, {1228, 2457}, {1994, 2081}, { 548,   61}, { 804, 2688}, 
{ 993,  666}, { 993,  667}, {1228, 2459}, { 852, 4616}, { 993,  668}, 
{ 638, 3578}, { 184,  186}, {1997, 2709}, { 548, 2708}, {1228, 2464}, 
{ 417, 2361}, { 417, 2363}, {1994, 2769}, { 988, 2711}, {1985,  284}, 
{ 988, 2712}, { 566,  284}, {1326, 1260}, {1228, 2460}, { 292, 2283}, 
{ 521, 2722}, {  83, 4808}, { 413, 2693}, {1272, 1481}, { 413, 2699}, 
{ 413, 2697}, { 413, 2698}, { 413, 2700}, { 413, 2706}, { 413, 2705}, 
{  82, 2800}, {1228, 1467}, {1953, 2786}, { 413, 3374}, {1272, 5162}, 
{ 993,  669}, { 725,  929}, {1228, 2465}, {1109, 4214}, { 993,  670}, 
{ 413,  183}, { 993,  671}, { 852, 4616}, { 184,  188}, { 342,  458}, 
{ 342,  459}, { 342,  460}, { 993,  672}, {1337, 4214}, { 195, 4792}, 
{ 548, 2709}, { 623, 2699}, { 623, 2697}, { 623, 2698}, { 623, 2700}, 
{ 292,  384}, {2157, 2263}, {1795, 2725}, {1994, 2082}, {1994, 2083}, 
{1326, 5169}, { 993,  673}, { 993,  674}, {2157, 2705}, {1228, 1468}, 
{1671, 4794}, {1198, 2743}, {  82, 2799}, {1671, 2741}, { 195, 4792}, 
{1228, 2466}, {1400,  224}, { 184,  247}, { 292,  385}, {1228, 2462}, 
{1997, 2710}, { 993,  675}, {  52, 4792}, { 195,  256}, {1453, 4824}, 
{1228, 2467}, {1994, 4850}, {1228, 2468}, { 183, 2693}, { 646,  804}, 
{1453, 4824}, {1057, 4770}, {1763, 4808}, { 417,  420}, {2126, 2765}, 
{ 183, 2717}, { 646, 2477}, {  82, 2661}, {1844, 2738}, { 639, 2693}, 
{1397, 4762}, { 993,  676}, { 416, 4840}, {1736, 1481}, {1228, 2470}, 
{  82, 2657}, { 993,  677}, { 993,  678}, { 993,  679}, {  77,  131}, 
{ 639, 3579}, {1397, 2795}, {1953, 1260}, { 292,  386}, {1736, 5233}, 
{1564, 5207}, { 417,  421}, { 317,  314}, { 993,  680}, {1702,   57}, 
{1702, 1897}, {1702, 2699}, { 417,  422}, { 548, 2710}, {1702, 2700}, 
{ 993,  284}, { 416, 2708}, {1576, 4842}, { 684,   57}, {1576, 5220}, 
{ 441, 2664}, {1109, 1355}, { 417,  423}, { 993,  681}, { 993,  682}, 
{  82, 2660}, { 725,  930}, {1702, 1898}, {1272, 4808}, { 993,  683}, 
{ 566,  112}, { 566,  113}, { 566,  114}, { 566,  115}, { 993, 2581}, 
{ 806,   57}, { 684,  886}, {1997, 2707}, { 639, 3579}, {1337, 4214}, 
{ 993,  684}, {1057, 4129}, { 806, 2705}, { 352, 3570}, { 417,  424}, 
{1953, 1261}, {1400, 2530}, { 993,  685}, {1795, 4762}, { 317,  315}, 
{ 195, 4792}, {2095, 4385}, { 993,  686}, { 993,  687}, {1813, 4603}, 
{1813, 4603}, { 993,  688}, { 400, 2708}, { 416, 2709}, { 707, 2695}, 
{ 707, 2696}, { 707, 2699}, {1795, 2786}, {1067, 4842}, { 707, 2700}, 
{ 993,  689}, {1662, 4782}, { 993,  690}, { 993,  691}, {1300, 1542}, 
{ 707, 2418}, { 707,  643}, { 993,  692}, { 707,  644}, { 993,  693}, 
{ 771,  977}, { 417,  425}, { 771,  978}, { 292, 4792}, { 674,  847}, 
{ 548, 2707}, { 993,  694}, {1067, 4842}, { 707,  645}, { 238,   57}, 
{1588, 2720}, { 238, 2699}, { 238, 2697}, { 238, 2698}, { 238, 2700}, 
{ 238, 2706}, { 771,  979}, {1566, 5210}, { 413, 4840}, { 495, 2347}, 
{ 993,  695}, { 292, 4792}, { 624,  785}, { 366, 4792}, { 400, 2709}, 
{ 993,  696}, { 993,  697}, { 238,   60}, {1736, 4808}, { 195, 4792}, 
{ 195, 2665}, { 521, 2723}, { 674,  848}, {1671, 2253}, { 707,  646}, 
{1397, 1647}, { 707,  647}, { 195, 2667}, {1014, 1244}, {1813, 4603}, 
{ 707,  648}, { 299, 2284}, { 413, 2708}, { 771,  980}, { 674,  849}, 
{ 993,  698}, { 184,  191}, { 707,  649}, { 292, 2740}, {1566, 4808}, 
{ 592, 3390}, { 416, 2710}, {1337, 1355}, { 292,  387}, { 707, 2485}, 
{ 624,  786}, {  77, 2827}, {1564, 5208}, { 888, 4842}, { 993, 5114}, 
{ 993, 5115}, { 993, 5116}, { 993, 5117}, {  77, 2834}, { 292,  388}, 
{ 195, 4792}, { 707,  650}, { 548, 2711}, {1564, 5209}, { 548, 2712}, 
{1967, 2557}, { 707, 2489}, { 184,  192}, { 184,  193}, { 184,  194}, 
{ 707,  224}, {1300, 1543}, { 299,  401}, { 184, 2677}, { 184, 2681}, 
{ 184, 2679}, { 417,  426}, {   6, 2234}, { 521, 4758}, { 413, 2709}, 
{ 417,  427}, {  37, 2882}, { 797,  996}, {1034, 1267}, { 195, 2740}, 
{1034, 1268}, {1067, 2551}, { 400, 2710}, {  82, 2662}, { 707,  651}, 
{  82, 2663}, {  82, 2801}, {  82, 2802}, {  82, 2803}, {  82, 2804}, 
{ 417,  428}, { 707,  652}, { 195, 4792}, { 183, 2714}, { 707,  653}, 
{1795, 1798}, { 707,  654}, {1681, 2693}, { 857, 2557}, {1681, 2699}, 
{ 674,  850}, { 416, 4840}, {1681, 2700}, { 707,  655}, {1681, 2705}, 
{  77, 2740}, {1453, 2756}, {  52, 2740}, { 416, 2707}, { 797, 4762}, 
{ 417,  429}, { 797, 4762}, {1293, 4774}, { 495,  583}, { 417,  430}, 
{1681,  516}, {2098, 2557}, { 195, 4792}, { 623,  782}, {1556, 2774}, 
{ 299,  402}, { 195, 5057}, {1400,  284}, { 592, 4800}, { 417,  431}, 
{ 195, 4792}, {1241, 4627}, {1100, 4627}, { 417,  432}, { 707,  656}, 
{ 707,  657}, { 317,  316}, {1566, 5211}, { 887, 4259}, { 292,  389}, 
{ 183, 2715}, { 292,  390}, { 265, 2693}, { 416, 4840}, { 707,  658}, 
{ 806,   61}, { 707,  659}, { 413, 2710}, {1566, 5212}, { 265, 2705}, 
{ 707, 2530}, { 707,  660}, { 707,  661}, {  37, 2863}, { 400, 2707}, 
{ 806, 2708}, { 623,  783}, { 292,  391}, { 366, 4792}, { 292,  392}, 
{ 195, 4792}, {   6, 2237}, {1564, 4808}, {  52, 2822}, { 592,  747}, 
{  52, 2821}, { 495,  584}, { 366, 2740}, {1293, 2731}, { 416, 2764}, 
{ 707,  662}, { 264, 2693}, {1662, 2735}, { 707,  663}, { 707,  664}, 
{ 416, 2711}, {1067, 5127}, { 416, 2712}, { 264, 2705}, { 797, 2774}, 
{1014, 2701}, {1014, 2702}, {1014, 2703}, {1014, 2704}, { 707,  665}, 
{ 567,  107}, { 521, 4758}, { 416, 2701}, { 416, 2702}, { 416, 2703}, 
{ 416, 2704}, { 707,  666}, { 707,  667}, { 807, 2777}, { 238,   61}, 
{ 707,  668}, {  37,   92}, { 806, 2709}, {1119,  817}, {1071,  996}, 
{1119, 2699}, {1119, 2697}, {1119, 2698}, {1119, 2700}, {1119, 2706}, 
{1119, 2705}, { 333,  446}, { 413, 4840}, { 183, 2716}, { 624, 2701}, 
{ 624, 2702}, { 624, 2703}, { 624, 2704}, {1119, 1368}, { 413, 2707}, 
{ 507, 4792}, {1119,  183}, { 797,  997}, {1304, 4627}, { 608,  606}, 
{1858,   57}, {1992, 2261}, {1858, 2699}, {1858, 2697}, {1858, 2698}, 
{1858, 2700}, { 707,  669}, { 746, 2693}, { 466, 2693}, { 843,   57}, 
{ 707,  670}, {1071, 4762}, { 707,  671}, {1071, 4762}, { 887, 4259}, 
{ 238,  133}, { 797,  998}, {1564,  204}, { 707,  672}, { 466, 3576}, 
{ 261, 2693}, { 888, 1115}, { 333,  447}, { 888, 4842}, { 413, 4840}, 
{1564,  205}, {1400,  112}, {1400,  113}, {1400,  114}, {1400,  115}, 
{ 299,  403}, { 292,  393}, { 707,  673}, { 707,  674}, {1034, 1269}, 
{1556, 5196}, {1813, 4603}, {1967, 2058}, {1538, 1534}, {1924, 1534}, 
{ 261, 2658}, {1813, 4603}, {1752, 2557}, {1241, 2775}, { 686, 2585}, 
{ 806, 2710}, {1556, 5197}, { 707,  675}, {1640, 1843}, {1813, 4603}, 
{ 413, 2764}, { 567,  224}, {1813, 4603}, {1813, 4603}, {1813, 4603}, 
{ 183, 2713}, { 413, 2711}, { 466, 3576}, { 413, 2712}, { 333,  448}, 
{ 195, 4792}, { 857, 1077}, { 592,  748}, { 319,  256}, {1588, 1791}, 
{ 797, 4762}, {1640, 1844}, { 707,  676}, { 413, 2701}, { 413, 2702}, 
{ 413, 2703}, { 413, 2704}, { 707,  677}, { 707,  678}, { 707,  679}, 
{2098, 2608}, {1071, 2774}, { 797,  999}, { 290, 4430}, {1321, 4627}, 
{1538, 1535}, {1924, 1535}, {1681, 2708}, { 195, 4792}, { 707,  680}, 
{ 195, 4792}, { 195, 4792}, { 195, 4792}, { 195, 4792}, { 195,  259}, 
{ 797, 1000}, { 707,  284}, { 195, 4792}, { 195, 4792}, { 195, 4792}, 
{ 623, 2701}, { 623, 2702}, { 623, 2703}, { 623, 2704}, { 707,  681}, 
{ 707,  682}, { 797, 5093}, { 494, 4430}, {2098, 2607}, { 996,   57}, 
{ 707,  683}, { 996, 2699}, { 996, 2697}, { 996, 2698}, { 996, 2700}, 
{ 707, 2581}, { 516, 2693}, { 806, 2707}, {2132, 2263}, {1071,  997}, 
{2098, 2605}, { 707,  684}, { 183, 2718}, { 516, 2717}, { 183, 2719}, 
{2132, 2705}, { 299,  404}, { 265, 2708}, { 707,  685}, {1681, 2709}, 
{1967, 1926}, { 261, 2659}, {1139, 1404}, { 707,  686}, { 707,  687}, 
{1858, 1659}, { 567, 2530}, { 707,  688}, {1071, 1312}, {1119, 1369}, 
{ 700, 5066}, { 700, 5067}, { 700, 5068}, {1696, 4798}, {1304, 2784}, 
{ 700, 5069}, { 707,  689}, {2161, 2693}, { 707,  690}, { 707,  691}, 
{ 686,  889}, { 700, 2418}, { 700,  643}, { 707,  692}, { 700,  644}, 
{ 707,  693}, { 264, 2708}, {1696, 4798}, {2161, 4390}, {1119, 1370}, 
{   6, 2235}, {   6, 2236}, { 707,  694}, {1751, 1534}, { 700,  645}, 
{1702, 2701}, {1702, 2702}, {1702, 2703}, {1702, 2704}, { 265, 2709}, 
{1917, 4627}, { 495,  585}, { 308, 2355}, {1119,   61}, {1794, 2725}, 
{ 996, 4761}, { 707,  695}, { 996, 4761}, {1241, 1481}, { 609,   57}, 
{ 495,  586}, { 707,  696}, { 707,  697}, {1119, 2708}, { 507, 4792}, 
{ 261,  339}, {2098, 2606}, {1071, 4762}, { 495,  587}, {1241, 5157}, 
{ 700,  646}, {1992, 2260}, { 700,  647}, { 746, 2349}, {  32, 2877}, 
{1858,   61}, { 700,  648}, {1681, 2710}, { 264, 2709}, {1071,  999}, 
{ 319, 2665}, { 707,  698}, { 592,  749}, { 700,  649}, { 843,   61}, 
{1751, 1535}, { 308,  418}, { 319, 2667}, {1538, 1536}, {1924, 1536}, 
{ 700, 2485}, { 592, 2758}, {1071, 5129}, {  27,   83}, {1119,  818}, 
{ 707, 2701}, { 707, 2702}, { 707, 2703}, { 707, 2704}, { 592,  750}, 
{ 507, 2740}, { 261,  340}, { 700,  650}, {1071, 5130}, {  27,   84}, 
{1119, 2709}, {  32, 4808}, { 700, 2489}, { 238, 2711}, { 308,  419}, 
{ 238, 2712}, { 700, 5070}, {  27, 4808}, {1639, 4627}, { 238,  134}, 
{ 238,  309}, { 238,  135}, { 265, 2710}, { 299,  405}, {1752, 1925}, 
{ 238, 2701}, { 238, 2702}, { 238, 2703}, { 238, 2704}, { 238,  136}, 
{1701, 2602}, {1119, 2586}, {1119, 2587}, {1992, 2259}, { 851,  847}, 
{ 700,  651}, {1701, 2601}, {1538, 1537}, {1924, 1537}, {1119, 1371}, 
{ 927, 1166}, {1119, 1372}, { 700,  652}, { 261,  341}, { 319, 2740}, 
{ 700,  653}, { 714, 3142}, { 700,  654}, {1019, 2735}, {1681, 2707}, 
{1304, 1481}, { 264, 2710}, { 797, 1002}, {1681, 2240}, { 700,  655}, 
{1794, 4762}, { 807, 1018}, { 888, 2765}, { 333,  449}, { 333,  450}, 
{ 333,  451}, {1304, 5163}, { 851,  848}, { 714, 3142}, { 290, 2687}, 
{ 625, 4808}, { 308, 2361}, { 308, 2363}, { 261, 2661}, {1794, 2786}, 
{1665, 4858}, {1446, 4798}, { 261,  348}, {1241, 4808}, { 851,  849}, 
{1009, 1236}, { 261, 2657}, {1009, 1237}, {1119, 2710}, {1601, 1801}, 
{ 700,  656}, { 700,  657}, { 567,  284}, {1481, 2748}, { 996,   61}, 
{1446, 4798}, {1451, 5183}, {1451, 5184}, {1451, 5185}, { 265, 2707}, 
{ 700,  658}, {1451, 5186}, { 700,  659}, {1917, 2781}, {1778, 4842}, 
{1601, 1802}, { 700, 5071}, { 700,  660}, { 700,  661}, {1249, 1489}, 
{2145, 5293}, { 516, 2714}, {1038,   57}, {1019, 4842}, {  32,   89}, 
{ 887, 1113}, { 261, 2660}, { 887, 4259}, {  27, 4808}, { 398, 2722}, 
{1778, 4842}, {1858, 1660}, {1752, 1926}, {1751, 1536}, {1601, 2569}, 
{  32, 2748}, { 700,  662}, {1898, 4816}, { 264, 2707}, { 700,  663}, 
{ 700,  664}, {1675, 2257}, {1019, 4842}, {1681, 2701}, {1681, 2702}, 
{1681, 2703}, {1681, 2704}, { 290, 2688}, { 924, 1149}, {1778, 4842}, 
{ 700,  665}, { 996, 4761}, {1696, 2743}, {1601, 1803}, {1414, 2231}, 
{1031, 2496}, {1031, 2497}, { 700,  666}, { 700,  667}, { 924, 2233}, 
{ 851,  850}, { 700,  668}, {1139, 1405}, { 516, 2715}, { 308,  420}, 
{1119, 2707}, {1386, 4627}, { 924, 1150}, {1778, 4842}, {1675, 1876}, 
{ 875,   57}, {1038, 2726}, { 875, 2699}, { 875, 2697}, { 875, 2698}, 
{ 875, 2700}, {1139, 1406}, {1751, 1537}, {1111, 2789}, {1016,  817}, 
{1304, 4808}, {1016, 2699}, {1016, 2697}, {1016, 2698}, {1016, 2700}, 
{1016, 2706}, {1016, 2705}, { 308,  421}, {1388,   57}, {1071, 5131}, 
{  27,   76}, { 520, 4758}, { 700,  669}, { 308,  422}, {1016, 1245}, 
{1388, 2705}, { 700,  670}, {1016,  183}, { 700,  671}, { 924, 1151}, 
{1226, 2440}, {1119, 1373}, {1481, 4808}, { 308,  423}, { 700,  672}, 
{1226, 2439}, {1388, 1637}, {1794, 1798}, { 610,   57}, { 924, 1152}, 
{ 290, 4430}, {1019, 4782}, { 240,   57}, {1246, 1245}, { 240, 2699}, 
{ 240, 2697}, { 240, 2698}, { 240, 2700}, { 700,  673}, { 700,  674}, 
{ 520, 2722}, { 567,  112}, { 567,  113}, { 567,  114}, { 567,  115}, 
{ 308,  424}, {1429, 2740}, {1119, 2711}, {1016, 2726}, {1119, 2712}, 
{ 240, 2689}, { 516, 2716}, { 924, 1153}, { 700,  675}, { 494, 4430}, 
{ 507, 4792}, {1451, 3584}, {1917, 1481}, {1865, 2265}, {1119, 2701}, 
{1119, 2702}, {1119, 2703}, {1119, 2704}, { 928, 4794}, {2145, 5294}, 
{ 924, 1154}, { 261,  343}, {1246, 2726}, {1917, 5255}, {1264, 2780}, 
{1111, 1260}, {1085, 5133}, {1250, 2776}, { 700,  676}, {   4, 2234}, 
{2145, 5295}, { 924, 1155}, { 308,  425}, { 700,  677}, { 700,  678}, 
{ 700,  679}, {1858, 2701}, {1858, 2702}, {1858, 2703}, {1858, 2704}, 
{1016, 4764}, {1009, 1238}, { 625, 4808}, { 290, 2621}, { 261, 2662}, 
{ 700,  680}, { 261, 2663}, { 261,  344}, { 261,  345}, { 261,  346}, 
{ 989,   57}, { 600,  760}, { 700, 5072}, { 261, 2666}, { 261, 2668}, 
{ 261, 2669}, {1264, 4808}, { 989, 2705}, {1139, 1407}, {1246, 4764}, 
{ 700,  681}, { 700,  682}, {1865, 2264}, {1250, 4808}, {1085, 1334}, 
{ 600,  761}, { 700,  683}, { 494, 2621}, {1111, 1261}, {1678, 4792}, 
{1446, 2743}, { 700, 2581}, {1663, 2249}, { 516, 2713}, {1663, 1664}, 
{1675, 2252}, {2169, 4804}, { 700,  684}, {2169, 4804}, {2169, 4804}, 
{2169, 4804}, {2169, 4804}, { 714, 3142}, {1085, 2772}, { 700,  685}, 
{1665, 4858}, {1663, 1665}, {1038,   61}, {1665, 4858}, { 700,  686}, 
{ 700,  687}, {1019, 5120}, {1386, 2793}, { 700,  688}, {1663, 1666}, 
{ 398, 2723}, { 560, 2695}, { 560, 2696}, { 560, 2699}, {2038, 4627}, 
{1601, 2573}, { 560, 2700}, { 700,  689}, {1081, 1328}, { 700,  690}, 
{ 700,  691}, {1639, 4842}, { 560, 2418}, { 560,  643}, { 700,  692}, 
{ 560,  644}, { 700,  693}, { 308,  426}, {   4, 2237}, {1563, 5204}, 
{1778, 4842}, { 308,  427}, {1778, 5241}, { 700,  694}, {1414, 2232}, 
{ 560,  645}, {1018,  817}, {1917, 4808}, {1018, 2699}, {1018, 2697}, 
{1018, 2698}, {1018, 2700}, {1018, 2706}, {1018, 2705}, {2145, 4808}, 
{1249, 1490}, { 308,  428}, { 700,  695}, { 625, 2748}, {1786, 5244}, 
{ 875,   61}, {1018, 1245}, { 700,  696}, { 700,  697}, {1018,  183}, 
{ 996, 2701}, { 996, 2702}, { 996, 2703}, { 996, 2704}, {1016,   61}, 
{1356, 4842}, { 560,  646}, {1429, 4792}, { 560,  647}, {1264, 1458}, 
{1563, 4808}, { 308,  429}, { 560,  648}, {1388,   61}, {1016, 2708}, 
{ 308,  430}, {1250, 1458}, { 700,  698}, {1356, 1260}, { 560,  649}, 
{1264, 5161}, {1356, 4842}, {1356, 1598}, {1388, 2708}, {1939, 2725}, 
{ 308,  431}, { 560, 2485}, {1250, 5159}, { 714, 3142}, { 308,  432}, 
{1018, 2726}, { 700, 5073}, { 700, 5074}, { 700, 5075}, { 700, 5076}, 
{1250, 4808}, { 468,  224}, { 221, 2940}, { 560,  650}, { 968, 1196}, 
{1356, 4842}, {1665, 2772}, {1898, 4816}, { 560, 2489}, { 714, 3142}, 
{1016,  818}, { 520, 2723}, { 560,  224}, {1023, 2779}, { 924, 1156}, 
{1885, 5253}, {1429, 4792}, {1885, 2699}, { 968,  761}, { 468,  559}, 
{1885, 2700}, {1016, 2709}, {1885, 2705}, {1647, 2725}, {1356, 4842}, 
{1029, 1030}, {1356, 5180}, { 600, 4842}, {1386, 1481}, {1288, 4808}, 
{1388, 2709}, { 560,  651}, {1018, 4764}, {1885,  516}, { 786, 2699}, 
{ 786, 2697}, { 786, 2698}, { 786, 2700}, { 560,  652}, {1386, 5182}, 
{1898, 2611}, { 560,  653}, {  29, 2874}, { 560,  654}, {1038, 4764}, 
{1223, 2774}, { 924, 2235}, { 924, 2236}, { 272,  351}, { 127,   57}, 
{ 560,  655}, { 127, 2699}, { 127, 2697}, { 127, 2698}, { 127, 2700}, 
{ 127, 2706}, {2038, 5269}, {1029, 4762}, { 221,  224}, {1029, 4762}, 
{1898, 2752}, {1249, 1491}, {  38, 2883}, { 928, 4794}, {1663, 2733}, 
{ 989,   61}, { 928, 2741}, { 127,  169}, {1563, 5205}, { 221, 2940}, 
{ 221, 2940}, {1786, 5245}, {1639, 2765}, {1625, 1828}, { 855,  804}, 
{ 989, 2708}, { 560,  656}, { 560,  657}, {  29, 4808}, {1563, 5206}, 
{ 600,  762}, { 855, 2477}, {1786, 5246}, {1223, 4808}, {2190, 2209}, 
{1939, 4762}, { 560,  658}, {1563, 4808}, { 560,  659}, {1016, 2710}, 
{1786, 4808}, {2169, 4804}, { 560, 2530}, { 560,  660}, { 560,  661}, 
{1663, 3122}, {1023, 1260}, {1081, 1329}, {1388, 2710}, {1939, 5264}, 
{ 894, 2693}, {1016, 4764}, { 894, 2699}, { 894, 2697}, { 894, 2698}, 
{ 894, 2700}, { 894, 2706}, { 894, 2705}, {1777, 4842}, {1597, 4842}, 
{1625, 1829}, { 285, 2220}, { 560,  662}, {1758, 4627}, {1678, 2740}, 
{ 560,  663}, { 560,  664}, { 989, 2709}, { 894,  183}, { 247,  320}, 
{1246, 4764}, {2169, 4804}, { 272,  103}, {1663, 3122}, {1777, 4842}, 
{1597, 4842}, { 560,  665}, {1663, 1667}, { 869,   57}, {1414, 2230}, 
{ 719, 2990}, {1647, 4762}, {  38, 2863}, { 560,  666}, { 560,  667}, 
{ 869, 2705}, {1018,   61}, { 560,  668}, {1386, 4808}, {1023, 1261}, 
{ 924, 1157}, { 719, 2990}, {1000, 4808}, {1777, 4842}, {1597, 4842}, 
{1322, 5167}, {1018, 2708}, {1000, 4808}, {2190, 2268}, { 719, 2990}, 
{1038, 4764}, {2190, 2269}, {   4, 2235}, {   4, 2236}, {1771, 4627}, 
{ 247,  321}, {1016, 2707}, {1451, 5187}, {1451, 5188}, {1451, 5189}, 
{1451, 5190}, {1650,  108}, {1777, 4842}, {1597, 4842}, {1663, 1668}, 
{1388, 2707}, {2169, 2746}, {1020, 2484}, { 560,  669}, { 875, 4842}, 
{1663, 1669}, {  29,   89}, { 560,  670}, {1663, 1670}, { 560,  671}, 
{  38,   92}, {1223, 1458}, {1018,  818}, { 285, 2223}, { 285, 2222}, 
{ 560,  672}, { 719, 2990}, {  29,   90}, {2190, 2272}, { 221, 2940}, 
{ 989, 2710}, {2038, 5270}, {1223, 5155}, {1018, 2709}, { 875, 4190}, 
{  29, 4808}, { 719, 2990}, { 247,  322}, {2169, 4804}, { 560,  673}, 
{ 560,  674}, {1356, 4842}, {2038, 5271}, {1356, 2765}, {1650,  224}, 
{1155, 4792}, { 875, 4190}, {1663, 1671}, {1929, 4627}, { 875, 5100}, 
{1383,   57}, {2131, 4778}, {1016, 4764}, { 968, 1197}, { 560,  675}, 
{1663, 1672}, { 651, 2557}, {1383, 2705}, {2131, 4778}, { 719, 2990}, 
{1885, 2708}, {1624, 4832}, {1939, 5265}, {1016, 2711}, {1288, 4808}, 
{1016, 2712}, { 875, 2701}, { 875, 2702}, { 875, 2703}, { 875, 2704}, 
{1663, 1673}, {1246, 4764}, { 719, 2990}, {1029, 4762}, { 560,  676}, 
{1016, 2701}, {1016, 2702}, {1016, 2703}, {1016, 2704}, { 560,  677}, 
{ 560,  678}, { 560,  679}, {1039,   57}, { 719, 2990}, { 127,   61}, 
{1029,  999}, { 661, 2534}, { 468,  284}, { 661,  833}, { 546, 2362}, 
{ 546, 2364}, { 560,  680}, {  33, 2878}, {1122, 2673}, { 884, 4858}, 
{ 600,  763}, {1000, 2748}, { 989, 2707}, { 560,  284}, {1663, 3122}, 
{1122, 2674}, {1018, 2710}, {1885, 2709}, { 240,  312}, {1663, 1674}, 
{ 661,  834}, { 560,  681}, { 560,  682}, { 240, 2701}, { 240, 2702}, 
{ 240, 2703}, { 240, 2704}, { 560,  683}, {1018, 4764}, {1663, 4778}, 
{ 703, 2420}, { 703,  643}, { 560, 2581}, { 703,  644}, {2133, 4778}, 
{1678, 4792}, { 600, 2765}, {1123, 2791}, { 560,  684}, {1650, 2530}, 
{ 127,  133}, {2133, 4778}, {1707, 4808}, { 703,  645}, {2183,   57}, 
{ 560,  685}, {1039, 2726}, {1707, 4808}, {2048, 4627}, {1020, 2483}, 
{ 560,  686}, { 560,  687}, { 894, 2536}, { 285, 2221}, { 560,  688}, 
{ 221, 2940}, {2038, 4808}, {1157, 4768}, { 931, 2302}, { 221,  284}, 
{ 894, 2708}, {1981, 2725}, {1288, 2748}, { 560,  689}, { 256,  330}, 
{ 560,  690}, { 560,  691}, {1122, 2740}, {1020, 1257}, { 703,  646}, 
{ 560,  692}, { 703,  647}, { 560,  693}, { 661, 2535}, { 989, 2711}, 
{ 703,  648}, { 989, 2712}, {1153, 3009}, { 869,   61}, { 560,  694}, 
{1625, 1830}, {1625, 1831}, { 703,  649}, {1018, 2707}, {1341, 5175}, 
{1885, 2710}, {1284, 2523}, { 221, 2940}, { 869, 2708}, { 703, 2485}, 
{1069, 4842}, { 931, 2321}, { 931,  368}, { 560,  695}, {1777, 4842}, 
{1597, 4842}, {1777, 5240}, {1597, 2765}, { 560,  696}, { 560,  697}, 
{ 256,  331}, { 703,  650}, { 894, 2709}, { 389, 2730}, {1929, 2784}, 
{ 884, 4858}, { 703, 2489}, {1123, 1260}, {2169, 4804}, {1069, 4842}, 
{ 703,  224}, {1155, 2740}, {2169, 4804}, {2169, 4804}, {2169, 4804}, 
{2169, 4804}, {2169, 4804}, {1157, 2728}, { 560,  698}, { 615, 2693}, 
{ 931, 2322}, { 931, 2326}, { 931, 2327}, { 931, 2328}, { 931, 2329}, 
{ 931, 2330}, { 931, 2331}, { 931, 2332}, { 931, 2333}, { 703,  651}, 
{ 869, 2709}, {  33,   86}, { 560, 2701}, { 560, 2702}, { 560, 2703}, 
{ 560, 2704}, { 703,  652}, { 256,  332}, { 968,  763}, { 703,  653}, 
{  49, 5044}, { 703,  654}, {  33, 2748}, {1569, 4627}, {2131, 2733}, 
{1018, 2711}, {  49,  107}, {1018, 2712}, { 703,  655}, { 651,  812}, 
{1123, 1261}, {1707, 2748}, {1885, 2707}, { 898,   57}, {1341, 1260}, 
{1383,   61}, { 719, 2990}, {1018, 2701}, {1018, 2702}, {1018, 2703}, 
{1018, 2704}, { 247,  323}, { 247,  324}, { 247,  325}, { 285, 2225}, 
{1383, 2708}, { 256,  333}, { 931, 2323}, { 931, 2335}, {1981, 4762}, 
{ 894, 2710}, { 141, 2871}, {1153, 4792}, {1029, 1031}, { 703,  656}, 
{ 703,  657}, {1688, 2693}, {2216,   57}, { 599, 2693}, {2216, 2699}, 
{2216, 2697}, {2216, 2698}, {2216, 2700}, { 141, 2872}, { 703,  658}, 
{1029, 1032}, { 703,  659}, {1039,   61}, { 719, 2990}, { 719, 2990}, 
{ 703, 2530}, { 703,  660}, { 703,  661}, { 884, 4858}, {  49,  108}, 
{1596, 4842}, { 285, 2224}, {1341, 5176}, { 869, 2710}, {2048, 5280}, 
{1650,  284}, {1258, 2778}, { 894, 2537}, {  43,   57}, { 522, 4758}, 
{1688, 2722}, { 132, 2825}, {1383, 2709}, {1274, 2726}, {1087, 4858}, 
{ 703,  662}, {1596, 4842}, {2133, 2733}, { 703,  663}, { 703,  664}, 
{1685, 2699}, {1685, 2697}, {1685, 2698}, {1685, 2700}, {1685, 2706}, 
{1685, 2705}, {2029, 2781}, { 931, 4792}, {1857, 1987}, { 703,  665}, 
{1929, 1481}, {1885, 2701}, {1885, 2702}, {1885, 2703}, {1885, 2704}, 
{1596, 4842}, { 703,  666}, { 703,  667}, { 522, 2722}, {2183,   61}, 
{ 703,  668}, {1929, 5256}, { 894, 2707}, { 389, 2740}, { 762,  966}, 
{ 931, 4792}, {  33, 4808}, { 132, 2832}, { 786, 2701}, { 786, 2702}, 
{ 786, 2703}, { 786, 2704}, {1934, 4627}, { 127, 2711}, {1596, 4842}, 
{ 127, 2712}, {1284, 2526}, {1624, 4832}, { 762,  761}, { 127,  134}, 
{ 931, 2324}, { 127,  135}, {1026, 2693}, { 179, 2828}, { 383,  496}, 
{ 127, 2701}, { 127, 2702}, { 127, 2703}, { 127, 2704}, { 127,  136}, 
{ 869, 2707}, { 703,  669}, { 931, 2740}, { 931,  369}, {1982,  107}, 
{ 703,  670}, { 383,  497}, { 703,  671}, { 500, 4758}, { 931,  370}, 
{1383, 2710}, {1408, 4830}, {1586, 4627}, { 703,  672}, { 132, 2838}, 
{ 503, 4758}, {1569, 5213}, { 719, 2990}, {1069, 5128}, {1413, 2248}, 
{1974,   57}, {1413, 1664}, {1974, 2699}, {1974, 2697}, {1974, 2698}, 
{1974, 2700}, { 884, 2772}, { 703,  673}, { 703,  674}, { 179, 2840}, 
{1284, 2524}, { 613, 2694}, {1153, 5137}, {1413, 1665}, { 894, 2711}, 
{1142, 2278}, { 894, 2712}, { 500, 2722}, {1974, 2689}, {1331, 1571}, 
{1087, 4858}, {1413, 1666}, { 703,  675}, { 132,  179}, { 503, 2722}, 
{2048, 5281}, { 894, 2701}, { 894, 2702}, { 894, 2703}, { 894, 2704}, 
{2082, 2263}, { 931,  371}, {1331, 2539}, {  49,  109}, {1039, 4764}, 
{ 256,  334}, {2048, 5282}, {2082, 2705}, { 705, 3608}, { 705,  643}, 
{ 614, 2694}, { 705,  644}, { 703,  676}, {1258, 1481}, { 481,  574}, 
{ 931, 2325}, { 179, 2835}, { 703,  677}, { 703,  678}, { 703,  679}, 
{1929, 4808}, { 705,  645}, {1383, 2594}, { 931,  372}, {1258, 5160}, 
{ 931, 2334}, {1026, 2237}, {1383, 2707}, { 898,   61}, { 703,  680}, 
{2029, 1481}, { 256,  335}, { 256,  336}, { 256,  337}, { 931, 2336}, 
{ 477,  224}, { 703,  284}, { 256, 2670}, { 256, 2671}, { 256, 2672}, 
{1982,  224}, {2029, 5268}, {1142, 2279}, {1142, 2280}, { 703,  681}, 
{ 703,  682}, {1408, 4830}, { 705,  646}, { 162, 2687}, { 705,  647}, 
{ 703,  683}, {1033, 1030}, {2216,   61}, { 705,  648}, {1641,   57}, 
{ 703, 2581}, {1641, 2699}, {1641, 2697}, {1641, 2698}, {1641, 2700}, 
{ 705,  649}, { 703,  684}, {1624, 2760}, {  28, 2873}, {1683, 2693}, 
{ 838, 4627}, {1683, 2699}, { 705, 2485}, { 703,  685}, {1683, 2700}, 
{1934, 5259}, {1683, 2705}, {1641, 2689}, { 703,  686}, { 703,  687}, 
{ 762, 4842}, { 615, 2299}, { 703,  688}, {1087, 4858}, { 705,  650}, 
{2041, 4627}, {1688, 2723}, {1683,  516}, {1033, 4762}, { 705, 2489}, 
{1033, 4762}, { 703,  689}, {1977, 2794}, { 703,  690}, { 703,  691}, 
{1569, 5214}, {1938, 2725}, {1254, 1489}, { 703,  692}, { 168,   57}, 
{ 703,  693}, { 168, 2699}, { 168, 2697}, { 168, 2698}, { 168, 2700}, 
{ 168, 2706}, {1569, 5215}, { 703,  694}, { 931,  373}, {1095, 1129}, 
{2048, 4808}, {1596, 4842}, { 705,  651}, {1596, 2765}, { 522, 2723}, 
{1039, 4764}, { 162, 2688}, { 168,   60}, {1586, 5222}, { 705,  652}, 
{ 477, 2530}, { 703,  695}, { 705,  653}, { 598, 2693}, { 705,  654}, 
{1982, 2530}, { 703,  696}, { 703,  697}, {1258, 4808}, { 705, 3608}, 
{ 931,  374}, { 705,  655}, {1717,   57}, { 762,  967}, {1717, 2699}, 
{1717, 2697}, {1717, 2698}, {1717, 2700}, {1717, 2706}, { 389, 4792}, 
{ 599, 2292}, {1413, 2733}, { 705, 3608}, {  49,  110}, { 881, 5101}, 
{2029, 4808}, { 703,  698}, { 931,  375}, {1600, 2725}, {1930, 2774}, 
{1717,   60}, {1030,   57}, { 132, 2830}, {1030, 2699}, {1030, 2697}, 
{1030, 2698}, {1030, 2700}, { 705,  656}, { 705,  657}, { 481,  575}, 
{1090, 4259}, { 931,  376}, { 392, 4758}, {2013, 2557}, {2033,  437}, 
{ 931,  377}, { 522, 4758}, { 705,  658}, {1158, 1149}, { 705,  659}, 
{  69, 2823}, {1274, 4764}, {1413, 3122}, { 705, 2530}, { 705,  660}, 
{ 705,  661}, { 162,  230}, {1284, 1526}, { 132, 2826}, { 132, 2839}, 
{ 132, 2833}, {1087, 2772}, { 881, 1105}, { 500, 2723}, { 132, 2831}, 
{ 132, 2837}, { 132, 2842}, {  49,  111}, {2033,  438}, { 615, 2298}, 
{ 503, 2723}, { 392, 2722}, {1938, 4762}, { 705,  662}, { 821, 1030}, 
{ 501, 4758}, { 705,  663}, { 705,  664}, {  28,   86}, {1934, 5260}, 
{1413, 3122}, { 881, 2772}, {1977, 1833}, {1258,  204}, {1413, 1667}, 
{1569, 4808}, {1938, 5262}, { 705,  665}, {1243, 4627}, {  28, 2748}, 
{1934, 5261}, {1258,  205}, { 929, 4794}, {1977, 5267}, { 705,  666}, 
{ 705,  667}, {1095, 4363}, {1030, 4761}, { 705,  668}, {1030, 4761}, 
{ 426, 2740}, {1977, 4808}, {  56, 4794}, {2041, 5272}, { 501, 2722}, 
{2182, 4792}, { 821, 4762}, { 426, 4792}, { 821, 4762}, {1586, 5223}, 
{ 179, 2829}, { 179, 2836}, { 179, 2841}, {1074,   57}, { 178, 2845}, 
{1074, 2699}, {1074, 2697}, {1074, 2698}, {1074, 2700}, {1074, 2706}, 
{1586, 5224}, {1413, 1668}, { 503, 4758}, {1471, 2774}, {1600, 4762}, 
{1026, 2235}, {1026, 2236}, {1413, 1669}, { 599, 2291}, { 705,  669}, 
{1413, 1670}, {1074,   60}, {1158, 5138}, { 705,  670}, {  69, 2237}, 
{ 705,  671}, {1090, 4259}, {1560, 4627}, {1600, 2786}, {1033, 4762}, 
{1410,   57}, { 705,  672}, {1410, 2699}, {1410, 2697}, {1410, 2698}, 
{1410, 2700}, {1562, 4627}, {1863, 4792}, {1086, 2576}, {1683, 2708}, 
{ 178, 2856}, {1033,  999}, {2171, 2385}, {2171, 2386}, { 522, 4758}, 
{ 705,  673}, { 705,  674}, {1274, 4764}, { 131, 2843}, {1413, 1671}, 
{1930, 5257}, { 477,  284}, {  49,  112}, {  49,  113}, {  49,  114}, 
{  49,  115}, {1982,  284}, {1413, 1672}, {1700, 2609}, { 168,   61}, 
{ 705,  675}, {1930, 5258}, { 652,  814}, { 908, 2420}, { 908,  643}, 
{1631,   57}, { 908,  644}, {1631, 2699}, {1631, 2697}, {1631, 2698}, 
{1631, 2700}, {1631, 2706}, {1413, 1673}, { 762,  763}, { 652,  815}, 
{2094, 2693}, { 908,  645}, { 178, 2851}, {  28, 4808}, {1934, 4808}, 
{ 705,  676}, {1683, 2709}, { 477,  569}, {1631,   60}, { 131, 2849}, 
{ 705,  677}, { 705,  678}, { 705,  679}, {2216, 2701}, {2216, 2702}, 
{2216, 2703}, {2216, 2704}, {1717,   61}, { 583, 4792}, {1938, 5263}, 
{2182, 2740}, {1912, 4627}, { 705,  680}, {  39, 2884}, { 762, 2765}, 
{ 168,  133}, {1413, 3122}, { 908,  646}, {1254, 1490}, { 908,  647}, 
{1700, 4394}, {1413, 1674}, {2013, 2608}, { 908,  648}, {1586, 4808}, 
{1243, 2775}, {1030,   61}, { 705,  681}, { 705,  682}, { 872,   57}, 
{ 908,  649}, {1413, 4778}, {2041, 5273}, { 705,  683}, {1629, 2792}, 
{ 503, 4758}, { 131, 2854}, { 908, 2485}, { 705, 2581}, {1685, 2701}, 
{1685, 2702}, {1685, 2703}, {1685, 2704}, {2041, 5274}, { 705,  684}, 
{ 258,  339}, {1863, 2740}, {1408, 2759}, {1717,  133}, { 908,  650}, 
{2013, 2607}, { 705,  685}, {1602, 1804}, {  41, 2886}, { 908, 2489}, 
{ 652, 2493}, { 705,  686}, { 705,  687}, { 908,  224}, {1471, 1000}, 
{ 705,  688}, {1600, 1798}, {2013, 2605}, { 385, 4792}, {1683, 2710}, 
{ 131,  178}, {1086, 2577}, { 392, 2723}, {1008, 2774}, { 705,  689}, 
{1471, 5191}, { 705,  690}, { 705,  691}, {1030, 4761}, {1560, 5198}, 
{1410, 1659}, { 705,  692}, { 908,  651}, { 705,  693}, {1441, 2693}, 
{ 598, 2290}, { 258,  340}, { 821, 4762}, {1562, 5201}, { 908,  652}, 
{ 705,  694}, {1479, 2775}, { 908,  653}, { 838, 1054}, { 908,  654}, 
{1982,  112}, {1982,  113}, {1982,  114}, {1982,  115}, { 821,  999}, 
{ 426, 4792}, { 908,  655}, {1602, 2787}, {2108, 5290}, { 705,  695}, 
{ 501, 2723}, { 394, 2285}, { 583,  734}, {1782, 1804}, { 705,  696}, 
{ 705,  697}, {1974, 2686}, { 129,  171}, {1074,   61}, { 871,   57}, 
{1074, 1321}, {1974, 2701}, {1974, 2702}, {1974, 2703}, {1974, 2704}, 
{ 963, 2728}, {1700, 2610}, { 929, 4794}, { 258,  341}, { 394, 2286}, 
{ 929, 2741}, {1632, 2793}, { 908,  656}, { 908,  657}, { 705,  698}, 
{  39,   97}, {2105, 2781}, {  56, 4794}, {2013, 2606}, {1033, 1031}, 
{  56, 2741}, {1683, 2707}, { 908,  658}, {2043, 4627}, { 908,  659}, 
{1410,   61}, {  39, 2748}, {1863, 4792}, { 908, 2530}, { 908,  660}, 
{ 908,  661}, {1033, 5122}, {2041, 4808}, {1912, 2775}, {  39, 4808}, 
{1573, 4627}, {1629, 1833}, { 258,  342}, {1782, 2787}, {1074,   62}, 
{ 378, 2314}, {1158, 2235}, {1158, 2236}, {  69, 2235}, {  69, 2236}, 
{1243, 1481}, {  54, 2693}, {1629, 5228}, { 908,  662}, {2046, 5278}, 
{ 543,   57}, { 908,  663}, { 908,  664}, {  41,   97}, { 603, 2293}, 
{1629, 4808}, {1243, 5158}, { 967, 1194}, { 378, 2315}, {1213, 1453}, 
{1631,   61}, {2094, 2349}, { 908,  665}, {2023,   57}, {  41, 2748}, 
{2023, 2699}, {2023, 2697}, {2023, 2698}, {2023, 2700}, { 908,  666}, 
{ 908,  667}, { 967,  761}, {  41, 4808}, { 908,  668}, { 275, 3573}, 
{  54, 2722}, { 764,  970}, { 828, 4013}, { 598, 2289}, {  40, 2885}, 
{1641, 2684}, {2023, 2689}, { 391, 4758}, {1213, 4851}, {1863, 4792}, 
{1641, 2701}, {1641, 2702}, {1641, 2703}, {1641, 2704}, {1008, 1000}, 
{ 764,  761}, { 178, 2846}, { 178, 2852}, { 178, 2857}, {1560, 5199}, 
{1683, 2701}, {1683, 2702}, {1683, 2703}, {1683, 2704}, {2049, 5283}, 
{1008, 5118}, {1631,  133}, {1602, 1805}, {1562, 5202}, { 908,  669}, 
{1560, 5200}, {1479, 1481}, {  30, 2875}, { 908,  670}, { 131, 2847}, 
{ 908,  671}, { 391, 2722}, {2108, 5291}, { 168, 2711}, {1562, 5203}, 
{ 168, 2712}, { 908,  672}, {1479, 5192}, { 583, 2740}, { 168,  134}, 
{ 275,  353}, { 168,  135}, {1632, 1481}, {2108, 5292}, {2046, 1260}, 
{ 168, 2701}, { 168, 2702}, { 168, 2703}, { 168, 2704}, { 168,  136}, 
{ 908,  673}, { 908,  674}, {1090, 1113}, {1632, 5229}, {1090, 4259}, 
{ 131, 2844}, { 131, 2855}, { 131, 2850}, {1509, 2502}, {1509, 2503}, 
{2105, 1481}, { 131, 2848}, { 131, 2853}, { 131, 2858}, {1089,   57}, 
{ 908,  675}, {1717, 2711}, {1864, 4794}, {1717, 2712}, {2167, 2381}, 
{1441, 2349}, {2105, 5289}, {1717,  134}, {1782, 1805}, {1717,  135}, 
{1243, 4808}, {1410, 1660}, { 821, 5095}, {1717, 2701}, {1717, 2702}, 
{1717, 2703}, {1717, 2704}, {1717,  136}, {1912, 1481}, {2043, 5275}, 
{ 908,  676}, {2196,   57}, {2046, 5279}, {1787, 5247}, { 821, 5096}, 
{ 908,  677}, { 908,  678}, { 908,  679}, { 385, 2740}, {1912, 5254}, 
{1055, 1294}, {1573, 5216}, {1030, 2701}, {1030, 2702}, {1030, 2703}, 
{1030, 2704}, { 665,  181}, { 908,  680}, { 665, 2699}, { 665, 2697}, 
{ 665, 2698}, { 665, 2700}, { 665, 2706}, { 665, 2705}, { 908,  284}, 
{  40,   86}, { 258,  343}, { 702, 2418}, { 702,  643}, { 257,  338}, 
{ 702,  644}, { 665,  182}, { 908,  681}, { 908,  682}, { 665,  183}, 
{1420, 4826}, {  40, 2748}, {2177, 4792}, { 908,  683}, {1560, 4808}, 
{ 702,  645}, { 129,  172}, {1285, 2527}, { 908, 2581}, {2049, 5284}, 
{ 665,  184}, {1811, 4792}, {1811, 2673}, {1562, 4808}, { 908,  684}, 
{1074, 1322}, {1479, 4808}, { 258,  344}, { 258,  345}, { 258,  346}, 
{2049, 5285}, { 908,  685}, {2108, 4808}, { 258, 2666}, { 258, 2668}, 
{ 258, 2669}, { 908,  686}, { 908,  687}, {2049, 4808}, { 897,   57}, 
{ 908,  688}, { 702,  646}, {1632, 4808}, { 702,  647}, {  30,   86}, 
{  73, 2817}, { 897, 2705}, { 702,  648}, { 603, 2294}, { 908,  689}, 
{ 543,   61}, { 908,  690}, { 908,  691}, {1045, 2527}, { 702,  649}, 
{  30, 2748}, { 908,  692}, {1074, 2711}, { 908,  693}, {1074, 2712}, 
{2105, 4808}, { 702, 2485}, {1216, 5147}, {1216, 5148}, {1216, 5149}, 
{ 908,  694}, {1443, 4798}, {1216, 5150}, {1277, 2726}, {1074, 2701}, 
{1074, 2702}, {1074, 2703}, {1074, 2704}, { 702,  650}, { 963, 2288}, 
{1419, 4826}, {  54, 2723}, {2167, 2380}, { 702, 2489}, { 908,  695}, 
{1443, 4798}, {1811, 2740}, { 702,  224}, {1912, 4808}, { 908,  696}, 
{ 908,  697}, {1602, 1806}, {1266, 1506}, {1266, 2499}, {1787, 5248}, 
{ 764,  971}, {1266, 2498}, {1234, 2774}, {1266, 1507}, {1266, 2500}, 
{2043, 5276}, {1410, 2701}, {1410, 2702}, {1410, 2703}, {1410, 2704}, 
{1787, 5249}, { 702,  651}, { 583,  735}, {1201, 4798}, { 908,  698}, 
{ 288, 2304}, {2043, 5277}, {1573, 5217}, { 702,  652}, {1195, 4798}, 
{  40, 4808}, { 702,  653}, { 391, 2723}, { 702,  654}, {1447, 4798}, 
{ 748, 4799}, {2160, 2693}, {1201, 4798}, {1573, 5218}, {1631, 2711}, 
{ 702,  655}, {1631, 2712}, { 748,  952}, {1195, 4798}, {1761, 2774}, 
{1631,  134}, {1714, 2774}, {1631,  135}, {1447, 4798}, { 965, 4798}, 
{1318, 2774}, {1631, 2701}, {1631, 2702}, {1631, 2703}, {1631, 2704}, 
{1631,  136}, {1266, 1508}, {1782, 1806}, { 288, 2321}, { 288,  368}, 
{ 828,  262}, { 655,  814}, { 220, 2940}, { 965, 4798}, { 385, 4792}, 
{1541, 4792}, { 702,  656}, { 702,  657}, {1365, 4842}, { 220, 2940}, 
{ 897, 1125}, { 665,  185}, { 665, 2678}, { 655,  815}, {  30, 4808}, 
{2167, 2764}, { 702,  658}, {1790, 4627}, { 702,  659}, { 665, 2680}, 
{1339, 2725}, {2167, 4840}, { 702, 2530}, { 702,  660}, { 702,  661}, 
{ 612, 2728}, {2196,   61}, { 288, 2322}, { 288, 2326}, { 288, 2327}, 
{ 288, 2328}, { 288, 2329}, { 288, 2330}, { 288, 2331}, { 288, 2332}, 
{ 288, 2333}, {1197, 1445}, {1864, 4794}, { 594, 2728}, {1193, 4798}, 
{1864, 2741}, { 665,   61}, { 702,  662}, { 508, 2693}, { 765, 4798}, 
{ 702,  663}, { 702,  664}, { 665,  186}, { 969, 4798}, {  96, 2725}, 
{1197,  761}, { 665, 2708}, {1216, 3582}, {1193, 4798}, {2179, 2740}, 
{ 971, 1200}, { 702,  665}, { 569, 4856}, { 765, 4798}, {1787, 4808}, 
{ 543,  625}, {2179, 4792}, { 969, 4798}, { 702,  666}, { 702,  667}, 
{2043, 4808}, { 151,  211}, { 702,  668}, { 129,  173}, { 971,  761}, 
{1266, 1509}, { 665,  187}, { 137,  204}, {1199, 4798}, { 288, 2323}, 
{ 288, 2335}, {1340, 2725}, {1573, 4808}, {1444, 4798}, { 655, 2493}, 
{ 137,  205}, { 137,  206}, { 665,  133}, {1234, 1000}, { 665,  188}, 
{2177, 2740}, {1285, 2528}, {1199, 4798}, {1055, 1295}, { 897,   61}, 
{1420, 4826}, {1738, 4627}, {1444, 4798}, { 665, 2709}, {1234, 5156}, 
{1266, 1510}, { 373, 2740}, { 404, 2730}, { 702,  669}, { 897, 2708}, 
{  12, 4842}, { 967,  763}, { 702,  670}, { 373, 4792}, { 702,  671}, 
{1266, 2501}, {1047, 2695}, {1047, 2696}, {1047, 2699}, {1266, 2504}, 
{ 702,  672}, {1047, 2700}, { 257, 2664}, { 665,  189}, { 665,  190}, 
{1761, 5235}, {1339, 4762}, {1714, 1000}, { 268, 2691}, { 268,  204}, 
{ 764,  763}, {1318, 5165}, {1045, 2528}, { 811, 4627}, { 702,  673}, 
{ 702,  674}, {1761, 5236}, { 268,  205}, {1714, 5232}, { 288, 4792}, 
{1443, 2743}, { 204,   57}, {1318, 5166}, { 204, 2699}, { 204, 2697}, 
{ 204, 2698}, { 204, 2700}, { 204, 2706}, {1266, 2506}, { 702,  675}, 
{ 151,  103}, { 897, 2709}, { 543,  626}, {1359, 2725}, {1378, 1622}, 
{  96, 4762}, { 647,   57}, { 288, 4792}, {  73, 2815}, { 204,   60}, 
{1055, 1296}, { 787,   57}, {1266, 2505}, { 647, 2705}, { 603, 2295}, 
{1419, 4826}, {1350,  853}, { 828, 4013}, {1350,  854}, { 702,  676}, 
{ 275,  354}, { 665, 2710}, { 288, 2324}, {1811, 4792}, { 702,  677}, 
{ 702,  678}, { 702,  679}, {1201, 2743}, {1350,  645}, {1114, 2725}, 
{ 224, 2619}, {1507, 2511}, {1340, 4762}, {1195, 2743}, { 288, 2740}, 
{ 288,  369}, { 702,  680}, { 224, 2620}, {1447, 2743}, {2023, 2101}, 
{1378, 1623}, { 288,  370}, { 657,  825}, { 702,  284}, {2023, 2701}, 
{2023, 2702}, {2023, 2703}, {2023, 2704}, {1923,  437}, { 657,  826}, 
{1266, 2507}, { 702,  681}, { 702,  682}, { 965, 2743}, {1350,  855}, 
{1112, 2725}, {1350,  647}, { 702,  683}, {1365, 4842}, {1076, 4627}, 
{1350,  648}, {1277, 4764}, { 702, 2581}, { 976, 4784}, { 976, 4784}, 
{ 976, 4784}, { 976, 4784}, {1350,  649}, { 702,  684}, { 897, 2710}, 
{  73,  130}, {2092, 2693}, {1923,  438}, {2092, 2699}, {1350, 2485}, 
{ 702,  685}, {2092, 2700}, {1378, 1624}, {2092, 2705}, {1047, 1288}, 
{ 702,  686}, { 702,  687}, {1256, 2479}, { 288,  371}, { 702,  688}, 
{1541, 2740}, {1350,  856}, {2189, 2263}, { 665, 2707}, {2092,  516}, 
{ 122,   57}, {1350, 2489}, { 404, 2740}, { 702,  689}, {2189, 2705}, 
{ 702,  690}, { 702,  691}, { 288, 2325}, {2210, 2410}, {1359, 4762}, 
{ 702,  692}, {1256, 1498}, { 702,  693}, {1193, 2743}, {2179, 4792}, 
{ 288,  372}, {1378, 1625}, { 288, 2334}, { 765, 2743}, { 702,  694}, 
{ 753, 2693}, {1266, 2508}, { 969, 2743}, {1420, 2757}, {1350,  857}, 
{1367,   57}, { 288, 2336}, {1367, 2699}, {1367, 2697}, {1367, 2698}, 
{1367, 2700}, {1350,  652}, {  12,   69}, { 702,  695}, {1350,  858}, 
{1114, 4762}, {1350,  859}, { 665,  191}, { 702,  696}, { 702,  697}, 
{ 793, 2695}, { 793, 2696}, { 793, 2699}, {1350,  655}, {  12, 2765}, 
{ 793, 2700}, { 897, 2707}, {1199, 2743}, {1285, 1286}, {1442, 2693}, 
{1266, 1511}, {1180, 2693}, {1444, 2743}, { 811, 2488}, { 612, 2296}, 
{ 748,  953}, { 373, 4792}, {1350, 2560}, { 702,  698}, {1507, 2510}, 
{ 665, 2711}, {1112, 4762}, { 665, 2712}, { 665,  192}, { 665,  193}, 
{ 665,  194}, { 665,  134}, { 594, 2287}, { 665,  135}, { 665, 2677}, 
{ 665, 2681}, { 665, 2679}, { 665, 2701}, { 665, 2702}, { 665, 2703}, 
{ 665, 2704}, { 665,  136}, {1277, 4764}, { 510,  606}, {1350,  860}, 
{1790, 4842}, {1350,  659}, {1729,   57}, {2158, 2263}, {1045, 1286}, 
{1350, 2530}, {1350,  660}, {1350,  661}, {1419, 2757}, { 569,  715}, 
{2158, 2705}, { 204,   61}, {1001, 2440}, {1266, 2509}, { 181, 2687}, 
{ 288,  373}, {1642,   57}, {1001, 2439}, {1642, 2699}, {1642, 2697}, 
{1642, 2698}, {1642, 2700}, { 569, 2771}, { 787, 2377}, { 418, 2764}, 
{1350,  662}, { 647,   61}, { 368, 2764}, {1350,  663}, {1350,  664}, 
{ 418, 4840}, { 950, 2693}, { 418, 4840}, { 368, 4840}, {1642, 2689}, 
{ 368, 4840}, { 647, 2708}, { 288,  374}, {1490,  817}, {1350,  861}, 
{1490, 2699}, {1490, 2697}, {1490, 2698}, {1490, 2700}, {1490, 2706}, 
{1490, 2705}, {1350,  666}, {1350,  667}, {1798, 2725}, {1076, 2488}, 
{1350,  668}, {2212, 2410}, { 204,  133}, {1490, 1725}, { 288,  375}, 
{1439,   57}, {1490,  183}, {1439, 2699}, {1439, 2697}, {1439, 2698}, 
{1439, 2700}, { 170,  204}, { 612,  772}, {1216, 5151}, {1216, 5152}, 
{1216, 5153}, {1216, 5154}, {2211, 2410}, { 288,  376}, { 170,  205}, 
{ 170,  206}, { 789,   57}, { 288,  377}, {1439, 2689}, {1815, 1610}, 
{ 594,  754}, {1541, 4792}, { 181, 2688}, { 647, 2709}, { 976, 2736}, 
{1909,  204}, {1350,  669}, { 817, 2687}, {2210, 2411}, { 107, 2313}, 
{1350,  670}, {2137, 2693}, {1350,  862}, {1909,  205}, {1909,  206}, 
{ 793,  990}, {2210, 2412}, {2204,   57}, {1350,  672}, {2204, 2699}, 
{2204, 2697}, {2204, 2698}, {2204, 2700}, {1815, 1393}, {2088, 2687}, 
{1197,  763}, {2092, 2708}, { 962, 2693}, {1172,  204}, {2117, 2725}, 
{ 318,  204}, {1808, 2725}, {1350,  673}, {1350,  674}, {1339, 4225}, 
{1350,  262}, {1172,  205}, {1172,  206}, { 318,  205}, { 318,  206}, 
{ 315,  437}, {1378, 1626}, {1378, 1627}, {1378, 1628}, { 971,  763}, 
{2084, 2263}, { 753, 2349}, {1350,  675}, {1378, 2675}, {1378, 2676}, 
{1192, 2693}, {1351,  853}, {2084, 2705}, {1351,  854}, {2079, 2263}, 
{2080, 2263}, {1351, 1594}, {1365, 5181}, { 404, 4792}, { 492, 2313}, 
{1367,   61}, {2079, 2705}, {2080, 2705}, {1351,  645}, { 315,  438}, 
{1843, 4788}, {1790, 5250}, {1350,  676}, {2092, 2709}, {1798, 4762}, 
{ 817, 2688}, { 647, 2710}, {1350,  863}, {1350,  678}, {1350,  864}, 
{1442, 2349}, { 819,  817}, {1180, 2349}, { 819, 2699}, { 819, 2697}, 
{ 819, 2698}, { 819, 2700}, { 819, 2706}, { 819, 2705}, {1350,  680}, 
{2083, 2263}, {2188, 2263}, {2088, 2688}, { 509, 2693}, {1351,  855}, 
{1340, 4225}, {1351,  647}, {2083, 2705}, {2188, 2705}, { 819,  183}, 
{1351,  648}, { 436,  437}, {1073,   57}, {1366, 1610}, {1350,  681}, 
{1350,  865}, {1367, 1612}, {1351,  649}, { 510,  607}, {2011, 2693}, 
{1350,  866}, { 937, 2695}, { 937, 2696}, { 937, 2699}, {1351, 2485}, 
{1350, 2581}, { 937, 2700}, {1673, 2699}, {1673, 2697}, {1673, 2698}, 
{1673, 2700}, {1350,  867}, {1729,   61}, {1497,   57}, { 496, 2693}, 
{ 436,  438}, {1351,  856}, {1366, 1393}, {1350,  685}, { 628,   57}, 
{2117, 4762}, {1351, 2489}, {1808, 4762}, {1350,  686}, {1350,  687}, 
{2072, 2124}, {2212, 2411}, {1350,  688}, { 527, 2313}, {1695, 2693}, 
{1637,   57}, {2092, 2710}, { 950, 2349}, { 647, 2707}, {2212, 2412}, 
{1093,   57}, {1350,  868}, {1637, 2705}, {1350,  690}, {1350,  869}, 
{ 497, 2693}, { 395,   57}, {2211, 2411}, {1350,  870}, {1351,  857}, 
{1350,  693}, {1892, 2693}, { 396,   57}, {1490,   61}, {2019, 4808}, 
{2211, 2412}, {1351,  652}, {1350,  694}, {1999, 1166}, {1351,  858}, 
{ 590, 2693}, {1351,  859}, { 627,   57}, {1490, 2708}, { 933, 2695}, 
{ 933, 2696}, { 933, 2699}, { 790,   57}, {1351,  655}, { 933, 2700}, 
{ 593, 2693}, {1350,  695}, {1047, 2701}, {1047, 2702}, {1047, 2703}, 
{1047, 2704}, {1350,  696}, {1350,  871}, { 789, 2377}, {1993, 4792}, 
{ 811, 2487}, { 631, 2375}, {1351, 2560}, {1114, 4225}, { 113, 2313}, 
{ 204, 2711}, { 799,   57}, { 204, 2712}, { 755,   57}, { 181, 4699}, 
{ 107, 2734}, { 204,  134}, {2137, 2349}, { 204,  135}, {1490,  818}, 
{ 631, 2368}, {1350,  872}, { 204, 2701}, { 204, 2702}, { 204, 2703}, 
{ 204, 2704}, { 204,  136}, {2140, 4391}, {2092, 2707}, {1351,  860}, 
{1490, 2709}, {1351,  659}, {2204,   61}, { 962, 2349}, {1112, 4225}, 
{1351, 2530}, {1351,  660}, {1351,  661}, { 630, 2373}, {1319, 4800}, 
{ 452,   57}, {2096, 4391}, {1115,   57}, {1670, 4792}, { 822, 1035}, 
{1358,   57}, {2217,   57}, { 629,  789}, {2217, 2699}, {2217, 2697}, 
{2217, 2698}, {2217, 2700}, { 630, 2370}, {1116,   57}, { 428,   57}, 
{1351,  662}, {1192, 2349}, { 889,   57}, {1351,  663}, {1351,  664}, 
{1275,   57}, { 629,  790}, { 126,   57}, { 903, 1129}, {1687, 5230}, 
{ 492, 2734}, { 372, 4792}, {  50, 4792}, { 690, 2693}, {1351,  861}, 
{ 690, 2699}, { 690, 2697}, { 690, 2698}, { 690, 2700}, { 690, 2706}, 
{ 690, 2705}, {1351,  666}, {1351,  667}, {1976, 4788}, {1613,   57}, 
{1351,  668}, {1076, 2487}, { 817, 4699}, {2019, 2748}, {1820,   57}, 
{2168, 4804}, { 690,  183}, {2168, 4804}, {2168, 4804}, {2168, 4804}, 
{2168, 4804}, { 819,   61}, { 976, 4784}, { 976, 4784}, { 976, 4784}, 
{ 976, 4784}, { 659, 5063}, {1843, 2738}, {1490, 2710}, {2088, 4699}, 
{1609,   57}, { 819, 2708}, {2092, 2701}, {2092, 2702}, {2092, 2703}, 
{2092, 2704}, {2122, 4788}, {1817,   57}, { 964,   57}, { 425, 4792}, 
{2011, 2349}, {1351,  669}, {1073,   61}, {1025,   57}, { 668, 5064}, 
{1351,  670}, { 800,   57}, {1351,  862}, { 800, 2699}, { 800, 2697}, 
{ 800, 2698}, { 800, 2700}, { 800, 2706}, {1351,  672}, {  84, 2797}, 
{ 496, 2349}, {  84, 2699}, {  84, 2697}, {  84, 2698}, {  84, 2700}, 
{  84, 2706}, { 628, 2377}, { 819,  818}, {1497,   61}, { 800,   60}, 
{ 235, 2722}, {2072, 2734}, {1351,  673}, {1351,  674}, { 527, 2734}, 
{1695, 2349}, { 999, 4792}, {  84,   60}, { 819, 2709}, { 542,   57}, 
{  61,  123}, {1367, 2701}, {1367, 2702}, {1367, 2703}, {1367, 2704}, 
{1637,   61}, { 497, 2349}, {1351,  675}, { 395,  511}, {2181,   57}, 
{1093,   61}, {1571,  643}, {1892, 2349}, {1571,  644}, { 396,  513}, 
{1637, 2708}, { 793, 2701}, { 793, 2702}, { 793, 2703}, { 793, 2704}, 
{1490, 2707}, { 590, 2349}, { 234, 2722}, {1571,  645}, { 627, 2377}, 
{ 903, 4363}, {1703,   57}, {1351,  676}, {1749,   57}, { 790, 2377}, 
{1231, 4792}, { 593, 2349}, {1351,  863}, {1351,  678}, {1351,  864}, 
{1749, 2705}, {1080,  817}, {1529,   57}, {1080, 2699}, {1080, 2697}, 
{1080, 2698}, {1080, 2700}, {1080, 2706}, {1080, 2705}, {1351,  680}, 
{ 113, 2734}, { 953, 4792}, {  50, 2740}, {1319, 2744}, {1571,  646}, 
{1673, 2266}, {1571,  647}, { 515, 2354}, { 515, 2743}, {1080,  183}, 
{1571,  648}, { 799,   61}, {1637, 2709}, { 755,   61}, {1351,  681}, 
{1351,  865}, {1310, 2555}, {1571,  649}, {2140, 2138}, {1886, 4792}, 
{1351,  866}, { 819, 2710}, {1642, 2685}, { 822, 1036}, {1571, 2485}, 
{1351, 2581}, {1989, 4806}, {1642, 2701}, {1642, 2702}, {1642, 2703}, 
{1642, 2704}, {1351,  867}, {2096, 2138}, {1861, 4806}, {1993, 2740}, 
{1310, 1550}, {1571,  650}, {1490, 2711}, {1351,  685}, {1490, 2712}, 
{ 452,   61}, {1571, 2489}, {1115,   61}, {1351,  686}, {1351,  687}, 
{1358,   61}, {2217,   61}, {1351,  688}, {  16,   57}, {1490, 2701}, 
{1490, 2702}, {1490, 2703}, {1490, 2704}, {1116,   61}, { 428,   61}, 
{  13,   57}, {1351,  868}, { 889,   61}, {1351,  690}, {1351,  869}, 
{1275,   61}, {1439, 1694}, { 126,   61}, {1351,  870}, {1571,  651}, 
{1351,  693}, {1439, 2701}, {1439, 2702}, {1439, 2703}, {1439, 2704}, 
{  14,   57}, {1571,  652}, {1351,  694}, {1670, 2740}, {1571,  653}, 
{ 180,   57}, {1571,  654}, {1141, 2732}, { 690, 2708}, {1613,   61}, 
{1637, 2710}, {1687, 1889}, { 246,   57}, {1571,  655}, {1820,   61}, 
{2168, 4804}, {1351,  695}, { 102, 4800}, { 819, 2707}, { 372, 2740}, 
{  15,   57}, {1351,  696}, {1351,  871}, { 954, 4792}, {1679, 4792}, 
{1976, 2738}, { 659,   61}, {  84, 2866}, {1846, 2354}, {1846, 2743}, 
{1609,   61}, {1893, 4808}, { 951, 4792}, {2204, 2701}, {2204, 2702}, 
{2204, 2703}, {2204, 2704}, {1817,   61}, { 964,   61}, {1571,  656}, 
{1571,  657}, {1351,  872}, {1010, 4800}, {1025,   61}, { 668,   61}, 
{2168, 4804}, { 800,   61}, { 947, 4792}, { 800, 1012}, {1571,  658}, 
{ 690, 2709}, {1571,  659}, { 803, 2687}, {2122, 2738}, { 999, 2740}, 
{1571, 2530}, {1571,  660}, {1571,  661}, {1080, 5132}, { 660,  831}, 
{1627, 4832}, {1860, 2274}, { 425, 2740}, { 819, 1026}, {1309, 2553}, 
{2175, 4804}, { 235, 2723}, {2175, 4804}, {2175, 4804}, {2175, 4804}, 
{2175, 4804}, { 801, 2695}, { 801, 2696}, { 801, 2699}, { 542,   61}, 
{1571,  662}, { 801, 2700}, {1637, 2707}, {1571,  663}, {1571,  664}, 
{ 819, 2711}, { 409, 4806}, { 819, 2712}, {1309, 1549}, {2181,   61}, 
{1827, 4562}, {1680, 4792}, { 800,   62}, {1231, 2740}, {1571,  665}, 
{2168, 2746}, {1779, 4792}, { 819, 2701}, { 819, 2702}, { 819, 2703}, 
{ 819, 2704}, {1571,  666}, {1571,  667}, { 234, 2723}, {  36, 2881}, 
{1571,  668}, {1703,   61}, { 955, 4792}, {1749,   61}, {2185, 4792}, 
{ 761,   57}, {  91, 2861}, { 761, 2699}, { 761, 2697}, { 761, 2698}, 
{ 761, 2700}, {1080,   61}, {1529,   61}, {1749, 2708}, {1141, 1409}, 
{ 803, 2688}, {  61, 2692}, {2168, 4804}, { 690, 2710}, { 946, 4792}, 
{ 956, 4792}, {1080, 2708}, { 937, 2701}, { 937, 2702}, { 937, 2703}, 
{ 937, 2704}, {1673, 2701}, {1673, 2702}, {1673, 2703}, {1673, 2704}, 
{2017, 4806}, {1571,  669}, { 410, 4806}, {1660, 4806}, { 575,  721}, 
{1571,  670}, { 870,   57}, {1571,  671}, { 870, 2699}, { 870, 2697}, 
{ 870, 2698}, { 870, 2700}, { 870, 2706}, {1571,  672}, {1643,   57}, 
{  84, 4807}, {1643, 2699}, {1643, 2697}, {1643, 2698}, {1643, 2700}, 
{ 957, 4792}, { 953, 2740}, {1080,  818}, { 314,   57}, { 870,   60}, 
{1749, 2709}, { 784, 5092}, {1571,  673}, {1571,  674}, {1169, 4792}, 
{ 314, 2705}, { 690,  894}, {1168, 4792}, {1080, 2709}, { 958, 4792}, 
{ 959, 4792}, {1534,   57}, { 960, 4792}, {  16,   61}, {1886, 2740}, 
{ 784,  987}, { 102, 2744}, {1571,  675}, {1534, 2705}, {1598, 4792}, 
{  13,   61}, {1053,  643}, {1159, 4792}, {1053,  644}, { 945, 4792}, 
{ 933, 2701}, { 933, 2702}, { 933, 2703}, { 933, 2704}, { 961, 4792}, 
{ 690, 2707}, { 949, 4792}, { 944, 4792}, {1053,  645}, { 952, 4792}, 
{  14,   61}, { 724, 4808}, {1571,  676}, {1746,   57}, {1877, 4792}, 
{ 180,   61}, {1010, 2744}, {1571,  677}, {1571,  678}, {1571,  679}, 
{1746, 2705}, { 530, 2693}, { 246,   61}, { 530, 2699}, { 530, 2697}, 
{ 530, 2698}, { 530, 2700}, { 530, 2706}, { 530, 2705}, {1571,  680}, 
{  15,   61}, {2093, 4792}, {1783, 4227}, {1392, 2720}, {1053,  646}, 
{  36,   96}, {1053,  647}, {1826, 4555}, {  91, 2862}, { 530,  183}, 
{1053,  648}, { 121, 4792}, { 948, 4792}, {1749, 2710}, {1571,  681}, 
{1571,  682}, {1989, 2747}, {1053,  649}, {1875, 4792}, { 870, 1093}, 
{1571,  683}, {1080, 2710}, {1495, 1706}, {1861, 2747}, {1053, 2485}, 
{1571, 2581}, {  84, 4807}, {2217, 2701}, {2217, 2702}, {2217, 2703}, 
{2217, 2704}, {1571,  684}, { 734, 4792}, { 954, 2740}, {1679, 2740}, 
{ 737, 4792}, {1053,  650}, { 690, 2711}, {1571,  685}, { 690, 2712}, 
{1872, 4792}, {1053, 2489}, { 951, 2740}, {1571,  686}, {1571,  687}, 
{2175, 4804}, { 660, 2531}, {1571,  688}, { 582, 4432}, { 690, 2701}, 
{ 690, 2702}, { 690, 2703}, { 690, 2704}, { 800, 1013}, { 733, 5090}, 
{ 706, 2416}, {1571,  689}, { 947, 2740}, {1571,  690}, {1571,  691}, 
{ 738, 4792}, { 739, 4792}, {2168, 4804}, {1571,  692}, {1053,  651}, 
{1571,  693}, {2168, 4804}, {2168, 4804}, {2168, 4804}, {2168, 4804}, 
{2168, 4804}, {1053,  652}, {1571,  694}, {1897, 2687}, {1053,  653}, 
{2175, 4804}, {1053,  654}, {1404, 4808}, { 740, 4792}, {1871, 4792}, 
{1749, 2707}, { 981, 2728}, {1405, 4808}, {1053,  655}, {1164, 4808}, 
{ 800, 2711}, {1571,  695}, { 800, 2712}, {1080, 2707}, {1407, 4808}, 
{1166, 2687}, {1571,  696}, {1571,  697}, {  84, 2711}, {1893, 2748}, 
{  84, 2712}, {1680, 2740}, { 800, 2701}, { 800, 2702}, { 800, 2703}, 
{ 800, 2704}, {1779, 2740}, {1855, 4808}, {1659, 2747}, {1661, 2277}, 
{  84, 2701}, {  84, 2702}, {  84, 2703}, {  84, 2704}, {1053,  656}, 
{1053,  657}, {1571,  698}, { 955, 2740}, { 728, 5087}, {2185, 2740}, 
{1823, 1968}, { 870,   61}, { 733, 5091}, {1783, 1808}, {1053,  658}, 
{2175, 2746}, {1053,  659}, {1457, 4808}, {  25, 2687}, {1643,   61}, 
{1053, 2530}, {1053,  660}, {1053,  661}, {2209, 2270}, { 946, 2740}, 
{ 956, 2740}, {2209, 2271}, {  57, 2687}, { 314,   61}, {1495, 1728}, 
{1476,   57}, {1897, 2688}, {1476, 2699}, {1476, 2697}, {1476, 2698}, 
{1476, 2700}, { 409, 2747}, {1749, 2711}, { 314, 2708}, {1749, 2712}, 
{1053,  662}, {1534,   61}, {2175, 4804}, {1053,  663}, {1053,  664}, 
{1080, 2711}, {1827, 1971}, {1080, 2712}, {1166, 2688}, {1659, 4806}, 
{1458, 4808}, {1534, 2708}, { 870,   62}, { 575, 2751}, {1053,  665}, 
{ 957, 2740}, {  89, 4808}, {1080, 2701}, {1080, 2702}, {1080, 2703}, 
{1080, 2704}, {1053,  666}, {1053,  667}, { 174, 4627}, {1169, 2740}, 
{1053,  668}, { 727, 5085}, {1168, 2740}, {1746,   61}, { 958, 2740}, 
{ 959, 2740}, { 728, 5088}, { 960, 2740}, { 983, 4784}, { 983, 4784}, 
{ 983, 4784}, { 983, 4784}, {1638, 4362}, {1746, 2708}, {1598, 2740}, 
{ 314, 2709}, {  25, 2688}, {1159, 2740}, { 380, 4432}, { 945, 2740}, 
{2017, 2747}, { 530, 2708}, { 410, 2747}, {1660, 2747}, { 961, 2740}, 
{  57, 2688}, { 949, 2740}, { 944, 2740}, {1534, 2709}, { 952, 2740}, 
{1965, 2589}, {1053,  669}, {1965, 2588}, {1823, 1926}, {1877, 2740}, 
{1053,  670}, {1094,   57}, {1053,  671}, {1094, 2699}, {1094, 2697}, 
{1094, 2698}, {1094, 2700}, {1094, 2706}, {1053,  672}, {1713,   57}, 
{1375, 2729}, {1713, 2699}, {1713, 2697}, {1713, 2698}, {1713, 2700}, 
{ 374, 2317}, {2093, 2740}, { 530,  409}, {1745,   57}, {1094,   60}, 
{1746, 2709}, {1457, 2748}, {1053,  673}, {1053,  674}, { 882, 2576}, 
{1745, 2705}, { 121, 2740}, { 948, 2740}, { 530, 2709}, { 727, 5086}, 
{1689, 1419}, {1294,   57}, {1689, 1420}, {1875, 2740}, {  97, 2748}, 
{1638, 1841}, {2121, 2748}, {1053,  675}, {1294, 2705}, {1146, 2987}, 
{1539, 4627}, {1291,  643}, {  97, 4808}, {1291,  644}, {2121, 4808}, 
{ 530, 2339}, {1627, 2760}, { 734, 2740}, { 314, 2710}, { 724, 2748}, 
{ 737, 2740}, {1833, 2748}, { 788,   57}, {1291,  645}, {1458, 2748}, 
{1872, 2740}, { 834, 2536}, {1053,  676}, {1748,   57}, {1833, 4808}, 
{  89, 2748}, {1534, 2710}, {1053,  677}, {1053,  678}, {1053,  679}, 
{1748, 2705}, { 304, 2693}, {1147, 2729}, { 304, 2699}, { 304, 2697}, 
{ 304, 2698}, { 304, 2700}, { 304, 2706}, { 304, 2705}, {1053,  680}, 
{ 738, 2740}, { 739, 2740}, {1392, 1643}, {1826, 1970}, {1291,  646}, 
{ 386,  500}, {1291,  647}, { 574,  720}, { 386,  501}, { 304,  183}, 
{1291,  648}, {1017, 2536}, {  85, 2862}, {1746, 2710}, {1053,  681}, 
{1053,  682}, { 815, 3926}, {1291,  649}, { 740, 2740}, {1871, 2740}, 
{1053,  683}, { 530, 2710}, {2175, 4804}, {1882, 2004}, {1291, 2485}, 
{1053, 2581}, {2175, 4804}, {2175, 4804}, {2175, 4804}, {2175, 4804}, 
{2175, 4804}, {1053,  684}, { 801, 2701}, { 801, 2702}, { 801, 2703}, 
{ 801, 2704}, {1291,  650}, {  86, 2748}, {1053,  685}, { 403,  521}, 
{ 314, 2707}, {1291, 2489}, { 403,  522}, {1053,  686}, {1053,  687}, 
{1476,   61}, {1712,   57}, {1053,  688}, {1712, 2699}, {1712, 2697}, 
{1712, 2698}, {1712, 2700}, { 402,  519}, {1534, 2707}, { 902, 2598}, 
{ 402,  520}, {1053,  689}, { 902, 2599}, {1053,  690}, {1053,  691}, 
{ 981, 2297}, { 374, 2318}, { 882, 2577}, {1053,  692}, {1291,  651}, 
{1053,  693}, { 761, 2701}, { 761, 2702}, { 761, 2703}, { 761, 2704}, 
{1404, 2748}, {1291,  652}, {1053,  694}, { 664, 2536}, {1291,  653}, 
{1405, 2748}, {1291,  654}, {1164, 2748}, { 499, 2693}, {1323, 2536}, 
{1746, 2707}, {1661, 1861}, {1407, 2748}, {1291,  655}, { 983, 2736}, 
{ 870, 2711}, {1053,  695}, { 870, 2712}, { 530, 2707}, {2102, 2748}, 
{ 582, 4433}, {1053,  696}, {1053,  697}, { 530,  410}, {1753, 2693}, 
{1855, 2748}, {1889, 2693}, { 870, 2701}, { 870, 2702}, { 870, 2703}, 
{ 870, 2704}, { 574, 2751}, { 314, 2711}, {  85,  143}, { 314, 2712}, 
{1643, 2701}, {1643, 2702}, {1643, 2703}, {1643, 2704}, {1291,  656}, 
{1291,  657}, {1053,  698}, { 142, 2869}, {1878, 2693}, {1094, 1344}, 
{1534, 2711}, {1094,   61}, {1534, 2712}, {1830, 4832}, {1291,  658}, 
{ 387,  502}, {1291,  659}, { 706,  910}, { 387,  503}, {1713,   61}, 
{1291, 2530}, {1291,  660}, {1291,  661}, {1044, 1284}, { 837,  262}, 
{  72, 2813}, { 834, 2537}, {1279,  262}, {1745,   61}, {1406, 4838}, 
{1514,   57}, {  86, 4808}, {1514, 2699}, {1514, 2697}, {1514, 2698}, 
{1514, 2700}, {1276, 1520}, {1746, 2711}, {1745, 2708}, {1746, 2712}, 
{1291,  662}, {1294,   61}, { 300, 2310}, {1291,  663}, {1291,  664}, 
{ 530, 2711}, {1831, 4832}, { 530, 2712}, { 742,  945}, { 601, 2728}, 
{1686, 4844}, {1294, 2708}, {1094,   62}, { 621, 4838}, {1291,  665}, 
{2061, 4838}, {1017, 2537}, { 530, 2701}, { 530, 2702}, { 530, 2703}, 
{ 530, 2704}, {1291,  666}, {1291,  667}, { 617, 2728}, {1841,   57}, 
{1291,  668}, {1828, 4832}, {2060, 4838}, {1748,   61}, {2203,  370}, 
{1203,   57}, {1841, 2705}, {1203, 2699}, {1203, 2697}, {1203, 2698}, 
{1203, 2700}, {1829, 4832}, {1628, 4832}, {1748, 2708}, {1083,  262}, 
{1745, 2709}, {1818, 2693}, {1841, 1637}, {2102, 4808}, {1964, 2591}, 
{1044, 1285}, { 304, 2708}, {  72,  129}, {1819, 2693}, { 487, 2762}, 
{ 487,  578}, {1619, 1821}, { 464,  557}, {1294, 2709}, {1686, 2766}, 
{ 541,  422}, {1291,  669}, {1330, 4019}, { 142,  210}, { 718, 4842}, 
{1291,  670}, { 692,   57}, {1291,  671}, { 692, 2699}, { 692, 2697}, 
{ 692, 2698}, { 692, 2700}, { 692, 2706}, {1291,  672}, {1791,   57}, 
{ 489,  370}, {1791, 2699}, {1791, 2697}, {1791, 2698}, {1791, 2700}, 
{1082, 1040}, { 174, 2816}, { 304,  409}, { 664, 2537}, { 692,   60}, 
{1748, 2709}, {1785, 2582}, {1291,  673}, {1291,  674}, {1323, 2537}, 
{ 380, 4433}, { 827, 1040}, { 966,  761}, { 304, 2709}, { 760,  761}, 
{1375, 4770}, {1712,   61}, {1194,  761}, { 699, 2695}, { 699, 2696}, 
{ 699, 2699}, {  17,   75}, {1291,  675}, { 699, 2700}, { 165,  234}, 
{ 165,  235}, {1944,  853}, {1276, 1521}, {1944,  854}, {1381, 4842}, 
{ 304, 2339}, {1608, 2790}, {1196,  761}, {1745, 2710}, {1445,  761}, 
{ 837, 2761}, {1024, 4842}, {1200,  761}, {1944,  645}, {1279, 1523}, 
{1944, 1961}, { 970,  761}, {1291,  676}, {1922,   57}, {1027, 4842}, 
{1037, 4842}, {1294, 2710}, {1291,  677}, {1291,  678}, {1291,  679}, 
{1922, 2705}, { 670,  817}, {1064, 4842}, { 670, 2699}, { 670, 2697}, 
{ 670, 2698}, { 670, 2700}, { 670, 2706}, { 670, 2705}, {1291,  680}, 
{ 906, 5102}, { 906, 5103}, { 906, 5104}, {1327, 4842}, {1944,  855}, 
{ 906, 5105}, {1944,  647}, { 483, 4437}, {2055, 2118}, { 670,  183}, 
{1944,  648}, {1621, 4842}, {1147, 4770}, {1748, 2710}, {1291,  681}, 
{1291,  682}, {1960, 1622}, {1944,  649}, {1539, 1751}, { 692,  898}, 
{1291,  683}, { 304, 2710}, {1097, 1347}, { 464,  558}, {1944, 2485}, 
{1291, 2581}, {1476, 2701}, {1476, 2702}, {1476, 2703}, {1476, 2704}, 
{1083, 2761}, {1291,  684}, { 643, 5062}, {1583, 4842}, {1097, 1348}, 
{1146, 1415}, {1944,  856}, { 788,  625}, {1291,  685}, { 284, 2622}, 
{1745, 2707}, {1944, 2489}, { 876, 1099}, {1291,  686}, {1291,  687}, 
{1514,   61}, {1513,   57}, {1291,  688}, {1513, 2699}, {1513, 2697}, 
{1513, 2698}, {1513, 2700}, { 868, 2557}, {1294, 2707}, { 876, 1100}, 
{ 485, 2319}, {1291,  689}, { 485, 2320}, {1291,  690}, {1291,  691}, 
{1390, 2718}, {2012,   57}, {1390, 2719}, {1291,  692}, {1944,  857}, 
{1291,  693}, {2155, 2693}, { 983, 4784}, { 983, 4784}, { 983, 4784}, 
{ 983, 4784}, {1944,  652}, {1291,  694}, {1621, 5227}, {1944,  858}, 
{ 347,  262}, {1944,  859}, {2203,  375}, {1940,   57}, {1841,   61}, 
{1748, 2707}, { 138,  207}, { 138,  208}, {1944,  655}, { 670,  843}, 
{1094, 2711}, {1291,  695}, {1094, 2712}, { 304, 2707}, {1841, 2708}, 
{ 555,  262}, {1291,  696}, {1291,  697}, { 304,  410}, { 535, 2366}, 
{ 196,  262}, { 535, 2367}, {1094, 2701}, {1094, 2702}, {1094, 2703}, 
{1094, 2704}, {1433, 1166}, {1745, 2711}, { 541,  429}, {1745, 2712}, 
{1713, 2701}, {1713, 2702}, {1713, 2703}, {1713, 2704}, { 621, 2763}, 
{1381, 2765}, {1291,  698}, { 169, 2718}, {2065, 4627}, { 169, 2719}, 
{1294, 2711}, { 692,   61}, {1294, 2712}, { 489,  375}, {1944,  860}, 
{1077, 2557}, {1944,  659}, {  17,   76}, {1882, 2005}, {1791,   61}, 
{1944, 2530}, {1944,  660}, {1944,  661}, {  60, 2718}, {1619, 1822}, 
{  60, 2719}, {1841, 2709}, {1862, 1991}, { 815, 2491}, {1656, 2693}, 
{1475,   57}, {2150, 4627}, {1475, 2699}, {1475, 2697}, {1475, 2698}, 
{1475, 2700}, {1869, 1166}, {1748, 2711}, { 311,  433}, {1748, 2712}, 
{1944,  662}, {1964, 2590}, { 310,  204}, {1944,  663}, {1944,  664}, 
{ 304, 2711}, { 311,  434}, { 304, 2712}, { 199,  204}, {1655, 2693}, 
{ 310,  205}, {1654, 2693}, { 692,   62}, {1027, 2765}, {1944,  861}, 
{ 237,  204}, { 199,  205}, { 304, 2701}, { 304, 2702}, { 304, 2703}, 
{ 304, 2704}, {1944,  666}, {1944,  667}, { 237,  205}, {1653, 2693}, 
{1944,  668}, { 511,   57}, {1837,  204}, {1922,   61}, {1583, 5221}, 
{ 626,   57}, { 742,  946}, {1327, 5170}, {1391,  204}, { 499, 2240}, 
{1837,  205}, { 670,   61}, {1072, 2693}, {1922, 2708}, {1879, 2239}, 
{1064, 5126}, {1391,  205}, {1024, 5121}, { 266,  207}, { 266,  208}, 
{ 866,   57}, { 670, 2708}, { 267,  207}, { 267,  208}, { 865, 2557}, 
{1753, 2240}, {1037, 5124}, {1889, 2240}, {1325, 2557}, {1841, 2710}, 
{ 864, 2693}, {1944,  669}, {1068, 2557}, { 863, 2557}, {1991, 2693}, 
{1944,  670}, {1046,   57}, {1944,  862}, {1046, 2699}, {1046, 2697}, 
{1046, 2698}, {1046, 2700}, {1046, 2706}, {1944,  672}, {1878, 2240}, 
{1880, 2238}, {1330, 1280}, {1712, 2701}, {1712, 2702}, {1712, 2703}, 
{1712, 2704}, { 601,  767}, { 670,  818}, {2031, 4627}, {1046,   60}, 
{1922, 2709}, {1990, 2276}, {1944,  673}, {1944,  674}, { 284, 4439}, 
{1066, 2557}, {1785, 1607}, { 528, 2693}, { 670, 2709}, { 773, 2694}, 
{ 617,  777}, {1513,   61}, {1988, 1987}, {1065, 2557}, {1830, 2760}, 
{2000, 1166}, {  45,  102}, {1944,  675}, {1389, 4627}, { 512,   57}, 
{ 347, 2655}, {1282,  853}, {1063, 2557}, {1282,  854}, {1113, 4627}, 
{1735,   57}, { 718, 2765}, {1735, 2699}, {1735, 2697}, {1735, 2698}, 
{1735, 2700}, {1608, 1607}, {1986, 2693}, {1282,  645}, { 300,  406}, 
{ 555, 2656}, {1406, 2763}, {1944,  676}, {2059, 2557}, {1060, 5125}, 
{ 196, 2682}, {1841, 2707}, {1944,  863}, {1944,  678}, {1944,  864}, 
{1059, 4627}, {1079,  817}, {1831, 2760}, {1079, 2699}, {1079, 2697}, 
{1079, 2698}, {1079, 2700}, {1079, 2706}, {1079, 2705}, {1944,  680}, 
{2057, 2695}, {2057, 2696}, {2057, 2699}, {1289, 2693}, {1282,  855}, 
{2057, 2700}, {1282,  647}, {2061, 2763}, {1754,   57}, {1079,  183}, 
{1282,  648}, {1409, 1658}, {1828, 2760}, {1922, 2710}, {1944,  681}, 
{1944,  865}, { 856, 4627}, {1282,  649}, {1960, 1626}, {2060, 2763}, 
{1944,  866}, { 670, 2710}, {1829, 2760}, {1628, 2760}, {1282, 2485}, 
{1944, 2581}, {1514, 2701}, {1514, 2702}, {1514, 2703}, {1514, 2704}, 
{ 932,   57}, {1944,  867}, { 966,  763}, {1818, 1964}, { 760,  763}, 
{1056, 4627}, {1282,  856}, {1194,  763}, {1944,  685}, { 774, 2694}, 
{1819, 1965}, {1282, 2489}, {1515, 4627}, {1944,  686}, {1944,  687}, 
{1475,   61}, {1512,   57}, {1944,  688}, {1512, 2699}, {1512, 2697}, 
{1512, 2698}, {1512, 2700}, {1196,  763}, {1682, 2243}, {1445,  763}, 
{2138, 2693}, {1944,  868}, {1200,  763}, {1944,  690}, {1944,  869}, 
{1822, 2557}, { 970,  763}, {2055, 2119}, {1944,  870}, {1282,  857}, 
{1944,  693}, {1203, 2701}, {1203, 2702}, {1203, 2703}, {1203, 2704}, 
{1082, 1041}, {1282,  652}, {1944,  694}, { 649, 2557}, {1282,  858}, 
{1329, 5171}, {1282,  859}, { 650, 4627}, {2136, 2245}, { 406,   57}, 
{1922, 2707}, { 827, 1041}, {2135, 2693}, {1282,  655}, { 513,   57}, 
{ 692, 2711}, {1944,  695}, { 692, 2712}, { 670, 2707}, {1968, 2557}, 
{ 514,   57}, {1944,  696}, {1944,  871}, { 656,  824}, {1302, 4627}, 
{1301,   57}, { 407,   57}, { 692, 2701}, { 692, 2702}, { 692, 2703}, 
{ 692, 2704}, { 850, 2557}, {1332, 5172}, { 662,   57}, {2129, 2693}, 
{1791, 2701}, {1791, 2702}, {1791, 2703}, {1791, 2704}, {1839, 4627}, 
{2034, 2557}, {1944,  872}, { 666, 4627}, { 667, 2693}, { 849, 2557}, 
{ 669, 2557}, {1046,   61}, {1377,   57}, {2120, 2557}, {1282,  860}, 
{1117, 4627}, {1282,  659}, { 848, 2557}, {1175, 2693}, { 886,   57}, 
{1282, 2530}, {1282,  660}, {1282,  661}, { 676, 2563}, { 699, 2701}, 
{ 699, 2702}, { 699, 2703}, { 699, 2704}, { 677, 2557}, {1437, 2338}, 
{2021,   57}, { 679, 2693}, {2021, 2699}, {2021, 2697}, {2021, 2698}, 
{2021, 2700}, { 680, 5065}, {1922, 2711}, { 847, 2557}, {1922, 2712}, 
{1282,  662}, { 682, 2557}, { 691,   57}, {1282,  663}, {1282,  664}, 
{ 670, 2711}, { 683,   57}, { 670, 2712}, {1384,   57}, { 691, 2705}, 
{1036, 5123}, { 685, 2693}, {1046,   62}, {1691, 1166}, {1282,  861}, 
{1735,   61}, {1692, 1166}, { 670, 2701}, { 670, 2702}, { 670, 2703}, 
{ 670, 2704}, {1282,  666}, {1282,  667}, { 688,   57}, {1536,   57}, 
{1282,  668}, { 906, 5106}, { 906, 5107}, { 906, 5108}, { 906, 5109}, 
{ 689, 2557}, {1536, 2705}, { 483,  482}, { 798, 2693}, {2075, 2275}, 
{1520, 2693}, {1079,   61}, { 167, 2796}, { 694, 2693}, { 695, 4627}, 
{ 896,   57}, { 696, 2693}, { 697,   57}, { 640, 2695}, { 640, 2696}, 
{ 640, 2699}, {1079, 2708}, { 896, 2705}, { 640, 2700}, { 698,   57}, 
{2058, 2557}, { 883, 4627}, { 643,  796}, {1521, 2693}, {2040, 4627}, 
{2110, 4627}, {1282,  669}, { 236,  307}, {1739, 2693}, {1436, 1166}, 
{1282,  670}, {1644,   57}, {1282,  862}, {1644, 2699}, {1644, 2697}, 
{1644, 2698}, {1644, 2700}, {1644, 2706}, {1282,  672}, { 900, 2693}, 
{ 775, 2693}, {1297, 4627}, {1513, 2701}, {1513, 2702}, {1513, 2703}, 
{1513, 2704}, { 840, 4627}, {1079,  818}, { 839, 2693}, {1644,   60}, 
{1979, 4627}, {   5, 2693}, {1282,  673}, {1282,  674}, {1240,   57}, 
{1191, 2693}, { 529, 2693}, {1937, 4627}, {1079, 2709}, {1978, 4627}, 
{1617, 2693}, {1512,   61}, {1616, 2693}, {1615, 2693}, {1614, 2693}, 
{1936, 4627}, {1796,   57}, {1282,  675}, {2187, 5296}, {1756, 4627}, 
{2076, 2693}, {1814,  853}, { 463,  556}, {1814,  854}, {1190, 2693}, 
{2215,   57}, {1189, 2693}, {2215, 2699}, {2215, 2697}, {2215, 2698}, 
{2215, 2700}, {1188, 2693}, { 557, 2693}, {1814,  645}, { 759, 2693}, 
{2002, 1166}, {2099,   57}, {1282,  676}, {1342, 4627}, {2077, 1166}, 
{1836, 4627}, {1187, 2693}, {1282,  863}, {1282,  678}, {1282,  864}, 
{ 938, 2693}, {1369,  817}, {1186, 2693}, {1369, 2699}, {1369, 2697}, 
{1369, 2698}, {1369, 2700}, {1369, 2706}, {1369, 2705}, {1282,  680}, 
{1594, 4627}, {2003, 1166}, {1022, 2557}, {1933, 4627}, {1814,  855}, 
{ 758, 2693}, {1814,  647}, {1540, 2557}, {1185, 2693}, {1369,  183}, 
{1814,  648}, {1587, 4627}, { 549,   57}, {1344, 5177}, {1282,  681}, 
{1282,  865}, { 829, 4627}, {1814,  649}, {1584,   57}, { 549, 2705}, 
{1282,  866}, {1079, 2710}, { 177,   57}, { 757, 2694}, {1814, 2485}, 
{1282, 2581}, {1475, 2701}, {1475, 2702}, {1475, 2703}, {1475, 2704}, 
{1184, 2693}, {1282,  867}, {1183, 2693}, { 940, 2693}, {2008, 2244}, 
{1577, 4627}, {1814,  856}, { 941, 2693}, {1282,  685}, {1176, 2693}, 
{1348, 4627}, {1814, 2489}, { 558, 2693}, {1282,  686}, {1282,  687}, 
{2021,   61}, {2192,   57}, {1282,  688}, {2192, 2699}, {2192, 2697}, 
{2192, 2698}, {2192, 2700}, { 756, 2694}, { 942, 2693}, {1456, 4627}, 
{1916, 4627}, {1282,  868}, { 691,   61}, {1282,  690}, {1282,  869}, 
{ 108,  158}, {1182, 2693}, { 109,  159}, {1282,  870}, {1814,  857}, 
{1282,  693}, {1150, 1423}, { 691, 2708}, {1046, 1026}, { 110,  160}, 
{  44,  102}, {1814,  652}, {1282,  694}, { 231,  301}, {1814,  858}, 
{ 111,  161}, {1814,  859}, {1430, 2693}, {1544,   57}, {1536,   61}, 
{1435, 1166}, {1296, 4627}, { 230,   57}, {1814,  655}, { 112,  162}, 
{1046, 2711}, {1282,  695}, {1046, 2712}, {1079, 2707}, {1536, 2708}, 
{ 808, 2557}, {1282,  696}, {1282,  871}, { 716, 2694}, {1431, 1689}, 
{ 896,   61}, { 114,  164}, {1046, 2701}, {1046, 2702}, {1046, 2703}, 
{1046, 2704}, { 115,  165}, {1127, 5136}, {1355, 4627}, {1181, 2693}, 
{ 896, 2708}, {2091, 1166}, {1282, 1283}, {1013, 5119}, { 691, 2709}, 
{1925, 2557}, {1282,  872}, {1559,   57}, { 943, 2693}, {1012, 4627}, 
{ 823, 2693}, {1644,   61}, {1179, 2693}, {1170, 2337}, {1814,  860}, 
{ 130, 4627}, {1814,  659}, {1174, 2693}, { 896, 1124}, { 919, 2694}, 
{1814, 2530}, {1814,  660}, {1814,  661}, {1357, 4627}, { 222, 2312}, 
{ 271,  350}, {1536, 2709}, {1927,   57}, {1079, 1026}, {1755,   57}, 
{2197,   57}, {1178, 2693}, {2197, 2699}, {2197, 2697}, {2197, 2698}, 
{2197, 2700}, {1735, 2701}, {1735, 2702}, {1735, 2703}, {1735, 2704}, 
{1814,  662}, { 776, 2693}, { 896, 2709}, {1814,  663}, {1814,  664}, 
{1079, 2711}, { 816, 2693}, {1079, 2712}, {1177, 2693}, {1152, 1425}, 
{1928,   57}, {1124, 4627}, {1644,   62}, {1432, 1166}, {1814,  861}, 
{2215,   61}, { 736, 2693}, {1079, 2701}, {1079, 2702}, {1079, 2703}, 
{1079, 2704}, {1814,  666}, {1814,  667}, {1721, 4627}, { 119, 2820}, 
{1814,  668}, {2057, 2701}, {2057, 2702}, {2057, 2703}, {2057, 2704}, 
{1947, 4627}, { 812, 2557}, { 691, 2710}, { 379,  492}, { 166,  236}, 
{2071,  236}, {1369,   61}, {2200, 2395}, { 540, 2365}, { 488, 2316}, 
{ 648,   57}, { 925, 1163}, {1438, 1693}, {1137, 1402}, { 478, 2940}, 
{ 559,  640}, {1369, 2708}, { 648, 2705}, { 273, 2281}, { 279, 2227}, 
{ 534, 2307}, { 316,   57}, { 226, 2616}, {1133, 2414}, {1536, 2710}, 
{ 286, 2617}, {1814,  669}, { 549,   61}, { 316, 2705}, { 287, 2618}, 
{1814,  670}, { 713, 2309}, {1814,  862}, { 289,  379}, { 531, 2306}, 
{ 710,  914}, {1644, 2584}, { 549, 2708}, {1814,  672}, { 572,  718}, 
{ 896, 2710}, { 150, 2219}, {1512, 2701}, {1512, 2702}, {1512, 2703}, 
{1512, 2704}, {1474,   57}, {1369,  818}, {1474, 2699}, {1474, 2697}, 
{1474, 2698}, {1474, 2700}, {1814,  673}, {1814,  674}, { 296, 2352}, 
{1814,  262}, { 909, 2419}, { 907, 2416}, {1369, 2709}, { 213, 2226}, 
{ 571,  717}, {2192,   61}, { 303,  408}, {1418, 2228}, {1416, 2229}, 
{ 475, 2309}, { 479, 2940}, {1814,  675}, { 556, 2218}, { 691, 2707}, 
{ 411, 2308}, {1812,  853}, {2022,   57}, {1812,  854}, {2022, 2699}, 
{2022, 2697}, {2022, 2698}, {2022, 2700}, { 480,  573}, { 549, 2709}, 
{1984, 2309}, { 704, 2417}, {1403, 1652}, {1812,  645}, { 469,  562}, 
{1812, 1961}, { 476,  568}, {1814,  676}, { 350,  462}, {2073, 2309}, 
{ 355,  468}, {1536, 2707}, {1814,  863}, {1814,  678}, {1814,  864}, 
{ 356, 2282}, {1519,  817}, { 360, 2246}, {1519, 2699}, {1519, 2697}, 
{1519, 2698}, {1519, 2700}, {1519, 2706}, {1519, 2705}, {1814,  680}, 
{ 526, 2311}, { 361, 2247}, { 896, 2707}, { 588, 2348}, {1812,  855}, 
{ 994, 2415}, {1812,  647}, { 591, 2346}, { 570, 2309}, {1519,  183}, 
{1812,  648}, {1210, 2413}, { 439, 5061}, {1850, 2309}, {1814,  681}, 
{1814,  865}, { 915, 1136}, {1812,  649}, { 563,  709}, { 439, 2705}, 
{1814,  866}, {1369, 2710}, { 128, 2824}, {1393, 2720}, {1812, 2485}, 
{1814, 2581}, {2021, 2701}, {2021, 2702}, {2021, 2703}, {2021, 2704}, 
{2081, 2131}, {1814,  867}, {1610, 2720}, {1505, 2495}, { 633,  792}, 
{1593, 1594}, {1812,  856}, {1468, 1713}, {1814,  685}, { 435, 5060}, 
{1160, 1150}, {1812, 2489}, { 549, 2710}, {1814,  686}, {1814,  687}, 
{2197,   61}, { 435, 2705}, {1814,  688}, {1536, 2711}, {2006, 2092}, 
{1536, 2712}, {1278, 1522}, {1525, 1522}, {   7,   55}, { 197,  263}, 
{ 846, 1061}, {1814,  868}, { 349,  263}, {1814,  690}, {1814,  869}, 
{1537,   57}, { 974, 2725}, { 632,  791}, {1814,  870}, {1812,  857}, 
{1814,  693}, {1215, 1455}, {1537, 2705}, {1470, 2443}, {1225, 2444}, 
{1202,  761}, {1812,  652}, {1814,  694}, {1535, 1748}, {1812,  858}, 
{1747, 1922}, {1812,  859}, { 845, 1060}, {1532, 1744}, { 640, 2701}, 
{ 640, 2702}, { 640, 2703}, { 640, 2704}, {1812,  655}, {1120, 1374}, 
{1644, 2711}, {1814,  695}, {1644, 2712}, {1369, 2707}, { 731,  935}, 
{1118, 1367}, {1814,  696}, {1814,  871}, {1793, 1950}, {1129, 2726}, 
{ 648,   61}, {1125, 2726}, {1644, 2701}, {1644, 2702}, {1644, 2703}, 
{1644, 2704}, {1920, 2032}, {1742, 1919}, { 484,  577}, {1084, 1332}, 
{ 648, 2708}, { 316,   61}, {1669, 2727}, { 544, 2727}, { 549, 2707}, 
{1466, 2727}, {1814,  872}, { 486, 2727}, { 537, 2727}, { 975, 2728}, 
{1450, 2728}, { 316, 2708}, { 982, 2728}, {1449, 2728}, {1812,  860}, 
{1162, 1152}, {1812,  659}, { 298,  400}, {1428, 2241}, { 565,  711}, 
{1812, 2530}, {1812,  660}, {1812,  661}, { 154,  217}, {   3,   50}, 
{ 384, 2730}, {1474,   61}, { 401, 2730}, { 158,  224}, { 232,  224}, 
{ 763,   57}, { 920,  224}, { 763, 2699}, { 763, 2697}, { 763, 2698}, 
{ 763, 2700}, {2215, 2701}, {2215, 2702}, {2215, 2703}, {2215, 2704}, 
{1812,  662}, {1143,  224}, { 648, 2709}, {1812,  663}, {1812,  664}, 
{1369, 2711}, { 573,  224}, {1369, 2712}, { 568,  224}, { 223,  224}, 
{ 219,  224}, { 161,  224}, {2022,   61}, { 316, 2709}, {1812,  861}, 
{ 159,  224}, { 149, 2808}, {1369, 2701}, {1369, 2702}, {1369, 2703}, 
{1369, 2704}, {1812,  666}, {1812,  667}, { 751,  956}, {1750,   57}, 
{1812,  668}, { 549, 2711}, {1239, 2421}, { 549, 2712}, {1558, 2424}, 
{ 504,   57}, {1750, 2705}, { 504, 2699}, { 504, 2697}, { 504, 2698}, 
{ 504, 2700}, {1519,   61}, {2068, 2122}, {1887, 2009}, {1426, 1684}, 
{ 118, 2819}, {1229, 2441}, {1684, 1886}, { 833, 2533}, {2119, 2148}, 
{  76, 2807}, {1519, 2708}, {1805, 1956}, { 153,  214}, {1806, 1957}, 
{ 923, 1148}, {1214, 1454}, { 561,  559}, { 990, 1210}, { 579,  724}, 
{1698, 1893}, {1812,  669}, { 439,   61}, {1697, 1893}, { 926, 1164}, 
{1812,  670}, { 586,  739}, {1812,  862}, { 977, 1206}, { 701, 5077}, 
{ 701, 5078}, { 701, 5079}, { 439, 2708}, {1812,  672}, { 701, 5080}, 
{ 648, 2710}, { 980, 1207}, {2192, 2701}, {2192, 2702}, {2192, 2703}, 
{2192, 2704}, {2205, 2396}, {1519,  818}, { 103,  150}, { 152,  213}, 
{ 274,  352}, { 316, 2710}, {1812,  673}, {1812,  674}, { 435,   61}, 
{ 214,  273}, {2154, 2267}, {1376, 1620}, {1519, 2709}, {1548, 1757}, 
{1604, 1810}, {2147, 2164}, {1800, 1955}, {  95, 2864}, { 435, 2708}, 
{1958, 2056}, {1954, 2053}, {1812,  675}, {1363, 1606}, { 146, 2871}, 
{1762, 2427}, {1043,  853}, {1931, 2428}, {1043,  854}, {1459, 2774}, 
{1537,   61}, {1557, 2426}, {1314, 2425}, {1313, 2422}, { 439, 2709}, 
{1003, 2774}, {1906, 2774}, {1715, 2774}, {1043,  645}, {1235, 2774}, 
{1537, 2708}, {2037, 2429}, {1812,  676}, {1004, 2774}, {1224, 2774}, 
{1553, 2774}, {1759, 2423}, {1812,  863}, {1812,  678}, {1812,  864}, 
{1473, 2774}, {1727,  817}, {1764, 2474}, {1727, 2699}, {1727, 2697}, 
{1727, 2698}, {1727, 2700}, {1727, 2706}, {1727, 2705}, {1812,  680}, 
{1766, 2471}, { 435, 2709}, { 648, 2707}, {2026, 2775}, {1043,  855}, 
{1932, 2473}, {1043,  647}, {2107, 2472}, {1485, 2775}, {1727,  183}, 
{1043,  648}, {1482, 2775}, {1718, 2775}, { 316, 2707}, {1812,  681}, 
{1812,  865}, {1767, 2475}, {1043,  649}, {1768, 2476}, {1493, 2776}, 
{1812,  866}, {1519, 2710}, {1537, 2709}, {1494, 2776}, {1043, 2485}, 
{1812, 2581}, {2197, 2701}, {2197, 2702}, {2197, 2703}, {2197, 2704}, 
{1769, 2486}, {1812,  867}, {1500, 2778}, {1565, 2490}, {1262, 2779}, 
{1504, 2780}, {1043,  856}, {1770, 2494}, {1812,  685}, {2144, 2516}, 
{2143, 2781}, {1043, 2489}, { 439, 2710}, {1812,  686}, {1812,  687}, 
{2104, 2781}, {1915, 2781}, {1812,  688}, { 910, 2699}, { 910, 2697}, 
{ 910, 2698}, { 910, 2700}, {2109, 2513}, {2163, 2515}, {1773, 2512}, 
{2039, 2514}, {1812,  868}, { 910, 3593}, {1812,  690}, {1812,  869}, 
{1518, 2781}, {2030, 2781}, {1281, 2782}, {1812,  870}, {1043,  857}, 
{1812,  693}, {1570, 2517}, {1774, 2538}, {1531, 2783}, { 435, 2710}, 
{2111, 2549}, {1043,  652}, {1812,  694}, {1776, 2548}, {1043,  858}, 
{ 316, 2711}, {1043,  859}, { 316, 2712}, {2036, 2784}, {1750,   61}, 
{1881, 5252}, {1547, 2784}, {1881, 2699}, {1043,  655}, { 100, 2865}, 
{1881, 2700}, {1812,  695}, {1881, 2705}, {1519, 2707}, {1750, 2708}, 
{1537, 2710}, {1812,  696}, {1812,  871}, {1646, 1847}, { 777, 2699}, 
{ 777, 2697}, { 777, 2698}, { 777, 2700}, {1881,  516}, {2070, 2123}, 
{ 148, 2872}, {1399, 1649}, { 873, 2785}, {2112, 2567}, {1599, 2786}, 
{1780, 2568}, {2052, 2786}, {1951, 2786}, {2044, 2564}, { 439, 2707}, 
{2045, 2565}, {1812,  872}, {1474, 2701}, {1474, 2702}, {1474, 2703}, 
{1474, 2704}, {1941, 2566}, {1952, 2786}, {1799, 2786}, {1043,  860}, 
{1942, 2575}, {1043,  659}, {1807, 2787}, {1943, 2579}, {1809, 2788}, 
{1043, 2530}, {1043,  660}, {1043,  661}, {1361, 2789}, {1579, 2580}, 
{2047, 2583}, {1750, 2709}, {1962, 2790}, {1582, 2592}, {1380, 2791}, 
{1945, 2593}, { 435, 2707}, {1834, 2792}, {2022, 2701}, {2022, 2702}, 
{2022, 2703}, {2022, 2704}, {2113, 2596}, {2064, 2793}, {1946, 2597}, 
{1043,  662}, {1838, 2793}, {1789, 2595}, {1043,  663}, {1043,  664}, 
{1519, 2711}, {1635, 2793}, {1519, 2712}, {2067, 2794}, {2114, 2600}, 
{2115, 2614}, {2069, 2795}, {1537, 2707}, {1396, 2795}, {1043,  861}, 
{1648, 2795}, {1792, 2612}, {1519, 2701}, {1519, 2702}, {1519, 2703}, 
{1519, 2704}, {1043,  666}, {1043,  667}, {1589, 2615}, {1591, 2613}, 
{1043,  668}, { 439, 2711}, {1398, 2795}, { 439, 2712}, { 105,  152}, 
{ 551,  634}, {1970, 2060}, { 578, 2699}, { 578, 2697}, { 578, 2698}, 
{ 578, 2700}, {1727,   61}, { 552,  635}, {1971, 2061}, { 553,  636}, 
{ 554,  637}, { 327,  443}, { 326,  442}, {1720, 1911}, {1910, 2024}, 
{ 143, 2860}, {1727, 2708}, {2162, 2191}, { 210, 2867}, {1658, 2732}, 
{1061, 1305}, {1705, 1902}, {2100, 2141}, { 435, 2711}, {1750, 2710}, 
{ 435, 2712}, {1043,  669}, {1704, 1901}, {1460, 1706}, {  93, 2861}, 
{1043,  670}, { 145, 2868}, {1043,  862}, { 781,  985}, {1140, 1408}, 
{ 216,  276}, { 281,  361}, { 525,  619}, {1043,  672}, { 796, 2699}, 
{ 796, 2697}, { 796, 2698}, { 796, 2700}, {1286, 2529}, {1537, 2711}, 
{ 173,  243}, {1537, 2712}, {1727,  818}, {1206, 2699}, {1206, 2697}, 
{1206, 2698}, {1206, 2700}, {1043,  673}, {1043,  674}, {1526, 2525}, 
{ 766,  974}, {  74, 2818}, { 175,  244}, {1727, 2709}, { 595,  755}, 
{ 225, 2735}, { 885, 1110}, {1108, 1358}, { 911, 2695}, { 911, 2696}, 
{ 911, 2699}, {1096, 1110}, {1043,  675}, { 911, 2700}, {1088, 1110}, 
{1126, 1384}, {1101,  853}, {1128, 1387}, {1101,  854}, { 939, 2699}, 
{ 939, 2697}, { 939, 2698}, { 939, 2700}, { 101, 2814}, {1352, 1595}, 
{1343, 1584}, {1315, 1229}, { 905, 1110}, {1101,  645}, {1005, 1229}, 
{1633, 1839}, {1788, 1947}, {1043,  676}, {1516, 1738}, {  68, 2809}, 
{1270, 1515}, {1750, 2707}, {1043,  863}, {1043,  678}, {1043,  864}, 
{1775, 1937}, {1840,  817}, {1484, 1721}, {1840, 2699}, {1840, 2697}, 
{1840, 2698}, {1840, 2700}, {1840, 2706}, {1840, 2705}, {1043,  680}, 
{1881, 2708}, {1545, 1756}, {1567, 1771}, {1772, 1936}, {1101,  855}, 
{1765, 1933}, {1101,  647}, { 297, 2351}, { 424,  539}, {1840,  183}, 
{1101,  648}, {2106, 2695}, {2106, 2696}, {2106, 2699}, {1043,  681}, 
{1043,  865}, {2106, 2700}, {1101,  649}, { 362,  476}, {  47,  103}, 
{1043,  866}, {1727, 2710}, { 602, 2738}, { 618, 2738}, {1101, 2485}, 
{1043, 2581}, { 763, 2701}, { 763, 2702}, { 763, 2703}, { 763, 2704}, 
{1620, 2738}, {1043,  867}, {1440,  930}, { 420, 2737}, { 437, 2770}, 
{ 438, 2770}, {1101,  856}, { 791, 2770}, {1043,  685}, { 792, 2770}, 
{ 218,  280}, {1101, 2489}, {1881, 2709}, {1043,  686}, {1043,  687}, 
{1107, 1357}, {1575, 1355}, {1043,  688}, {1750, 2711}, {1106, 1355}, 
{1750, 2712}, {1574, 1355}, {1335, 1355}, {1336, 1577}, {1354, 1355}, 
{1345, 1587}, {1043,  868}, {1353, 1355}, {1043,  690}, {1043,  869}, 
{1130, 1389}, {1914, 2028}, {  92, 2870}, {1043,  870}, {1101,  857}, 
{1043,  693}, { 504, 2701}, { 504, 2702}, { 504, 2703}, { 504, 2704}, 
{1261, 1502}, {1101,  652}, {1043,  694}, {1427, 2740}, {1101,  858}, 
{1425, 2740}, {1101,  859}, { 619, 2740}, {1424, 1682}, {1668, 2740}, 
{2007, 2693}, { 376, 2740}, {2007, 2699}, {1101,  655}, {2180, 2740}, 
{2007, 2700}, {1043,  695}, {2007, 2705}, {1727, 2707}, {1463, 2740}, 
{1423, 2740}, {1043,  696}, {1043,  871}, {1464, 2740}, {2166, 2740}, 
{ 388, 2740}, {1465, 2740}, {1101, 2560}, {2007,  516}, {1488, 2740}, 
{ 701, 5081}, { 701, 5082}, { 701, 5083}, { 701, 5084}, {2184, 2740}, 
{1868, 2740}, { 607, 2740}, {1043, 1283}, {1467, 2740}, {2035, 2740}, 
{1881, 2710}, {1043,  872}, { 606, 2740}, { 539, 2740}, { 538, 2740}, 
{2172, 2740}, {1870, 2740}, {1508, 2740}, {2195, 2740}, {1101,  860}, 
{ 536, 2740}, {1101,  659}, {1415, 2740}, { 377, 2740}, {1290, 2740}, 
{1101, 2530}, {1101,  660}, {1101,  661}, {2201, 2740}, { 430, 2740}, 
{1510, 2740}, {2194, 2740}, { 233, 2740}, {1511, 2740}, {1506, 2740}, 
{1966, 2740}, {2198, 2740}, { 505, 2699}, { 505, 2697}, { 505, 2698}, 
{ 505, 2700}, { 824, 2695}, { 824, 2696}, { 824, 2699}, { 432, 2740}, 
{1101,  662}, { 824, 2700}, {1711, 2740}, {1101,  663}, {1101,  664}, 
{1727, 2711}, { 584,  737}, {1727, 2712}, { 585,  738}, {1502, 2741}, 
{1273, 1519}, { 741,  944}, {1263, 1503}, { 743,  947}, {1101,  861}, 
{1543, 2741}, {1260, 2741}, {1727, 2701}, {1727, 2702}, {1727, 2703}, 
{1727, 2704}, {1101,  666}, {1101,  667}, {1091, 1122}, {1542, 2741}, 
{1101,  668}, {1219, 2437}, {2028, 2741}, { 893, 1122}, { 749,  954}, 
{ 747,  951}, {1895, 2014}, {1881, 2707}, { 744,  948}, { 750,  955}, 
{ 587,  740}, {1840,   61}, {1816, 2742}, {1949, 2050}, {1845, 1980}, 
{ 471,  564}, { 358,  470}, {2018, 2695}, {2018, 2696}, {2018, 2699}, 
{1308, 2556}, {1840, 2708}, {2018, 2700}, {1803, 2571}, {1802, 2570}, 
{2134, 2159}, {1801, 2572}, {1006, 1231}, {1148, 1418}, {1554, 1231}, 
{1316, 1231}, {1101,  669}, { 390, 2745}, {1230, 1231}, {1667, 2745}, 
{1101,  670}, { 644,   57}, {1101,  862}, { 644, 2699}, { 644, 2697}, 
{ 644, 2698}, { 644, 2700}, { 644, 2706}, {1101,  672}, {1664, 2745}, 
{1222, 2436}, { 831, 2532}, { 910, 2701}, { 910, 2702}, { 910, 2703}, 
{ 910, 2704}, { 987, 1209}, {1840,  818}, {1265, 1505}, { 644,   60}, 
{  71,  128}, {2054, 2117}, {1101,  673}, {1101,  674}, {1462, 1707}, 
{2025, 2102}, {1730, 1914}, {2063, 2121}, {1840, 2709}, {1218, 1457}, 
{  26,   82}, {1848, 1981}, {1603, 1808}, {2016, 2695}, {2016, 2696}, 
{2016, 2699}, {1903, 2019}, {1101,  675}, {2016, 2700}, { 423, 2746}, 
{1907, 2746}, {1349,  853}, {1908, 2746}, {1349,  854}, {1269, 2746}, 
{1238, 2746}, {1881, 2701}, {1881, 2702}, {1881, 2703}, {1881, 2704}, 
{1237, 2746}, {1236, 2746}, {1267, 2746}, {1349,  645}, { 419, 2746}, 
{1268, 2746}, {1221, 2435}, {1101,  676}, { 777, 2701}, { 777, 2702}, 
{ 777, 2703}, { 777, 2704}, {1101,  863}, {1101,  678}, {1101,  864}, 
{ 735,  939}, {1969,  817}, { 133,  180}, {1969, 2699}, {1969, 2697}, 
{1969, 2698}, {1969, 2700}, {1969, 2706}, {1969, 2705}, {1101,  680}, 
{2007, 2708}, {1245, 1487}, {1496, 1729}, {  62,  126}, {1349,  855}, 
{1725, 1913}, {1349,  647}, { 405,  525}, { 818, 1025}, {1969,  183}, 
{1349,  648}, {1486, 2695}, {1486, 2696}, {1486, 2699}, {1101,  681}, 
{1101,  865}, {1486, 2700}, {1349,  649}, {1049, 1289}, { 644,  799}, 
{1101,  866}, {1840, 2710}, { 393,  510}, { 182,  246}, {1349, 2485}, 
{1101, 2581}, {1145, 2699}, {1145, 2697}, {1145, 2698}, {1145, 2700}, 
{1612, 1817}, {1101,  867}, {1618, 1820}, { 890, 1117}, {1368, 1613}, 
{ 338,  452}, {1349,  856}, {1421, 1419}, {1101,  685}, {1894, 2748}, 
{2159, 2190}, {1349, 2489}, {2007, 2709}, {1101,  686}, {1101,  687}, 
{1454, 2748}, {2097, 2748}, {1101,  688}, { 518, 2699}, { 518, 2697}, 
{ 518, 2698}, { 518, 2700}, {2015, 2748}, {1422, 1420}, { 715,  919}, 
{ 445,  552}, {1101,  868}, { 453,  553}, {1101,  690}, {1101,  869}, 
{1592,  262}, { 444,  551}, { 249,  327}, {1101,  870}, {1349,  857}, 
{1101,  693}, { 578, 2701}, { 578, 2702}, { 578, 2703}, { 578, 2704}, 
{ 248,  326}, {1349,  652}, {1101,  694}, { 454,  554}, {1349,  858}, 
{1741,  262}, {1349,  859}, { 328,  326}, {2146,  262}, { 329,  327}, 
{2005, 2693}, {1824, 2749}, {2005, 2699}, {1349,  655}, {1972, 2062}, 
{2005, 2700}, {1101,  695}, {2005, 2705}, {1840, 2707}, {2149, 2165}, 
{1835, 1973}, {1101,  696}, {1101,  871}, { 147, 2869}, {  98, 2862}, 
{ 785, 2360}, {1209, 2358}, {1349, 2560}, {2005,  516}, { 986, 2357}, 
{ 782, 2359}, {2153, 2185}, {1630, 1836}, { 796, 2701}, { 796, 2702}, 
{ 796, 2703}, { 796, 2704}, {1987, 2074}, { 693, 2750}, {1699, 1894}, 
{2007, 2710}, {1101,  872}, {1206, 2701}, {1206, 2702}, {1206, 2703}, 
{1206, 2704}, { 644,   61}, {1896, 2015}, {1821, 1966}, {1349,  860}, 
{2014, 2097}, {1349,  659}, {1926, 2035}, { 215,  274}, { 211,  271}, 
{1349, 2530}, {1349,  660}, {1349,  661}, { 721, 2751}, { 911, 2701}, 
{ 911, 2702}, { 911, 2703}, { 911, 2704}, { 351,  463}, { 720, 2751}, 
{ 106,  155}, {1333, 2772}, {1334, 2772}, { 939, 2701}, { 939, 2702}, 
{ 939, 2703}, { 939, 2704}, { 813, 2772}, {1105, 2772}, {1104, 2772}, 
{1349,  662}, {1078, 2772}, { 620,  222}, {1349,  663}, {1349,  664}, 
{1840, 2711}, { 163,  222}, {1840, 2712}, { 157,  222}, { 580,  222}, 
{2085, 2133}, {1859, 1989}, { 644,   62}, {   9,   56}, {1349,  861}, 
{  10,   56}, {   8,   56}, {1840, 2701}, {1840, 2702}, {1840, 2703}, 
{1840, 2704}, {1349,  666}, {1349,  667}, {1737, 1916}, {1935, 2040}, 
{1349,  668}, {1918, 2031}, {2042, 2110}, {1255, 2480}, { 301,  407}, 
{1963, 1816}, { 313, 2810}, {2007, 2707}, { 242, 2812}, {1611, 1816}, 
{ 493,  581}, {1969,   61}, {2106, 2701}, {2106, 2702}, {2106, 2703}, 
{2106, 2704}, {1743, 1920}, { 835, 2695}, { 835, 2696}, { 835, 2699}, 
{ 241, 2811}, {1969, 2708}, { 835, 2700}, { 712,  916}, {1455, 1703}, 
{1498, 2481}, {1550, 2554}, {1257, 2482}, {1549, 2552}, { 369, 2753}, 
{ 421, 2753}, {1349,  669}, {2174, 2753}, { 810, 2773}, {1477, 2773}, 
{1349,  670}, { 854,   57}, {1349,  862}, { 854, 2699}, { 854, 2697}, 
{ 854, 2698}, { 854, 2700}, { 854, 2706}, {1349,  672}, {1708, 2699}, 
{1708, 2697}, {1708, 2698}, {1708, 2700}, { 523, 2699}, { 523, 2697}, 
{ 523, 2698}, { 523, 2700}, {1969,  818}, { 917, 1138}, { 854,   60}, 
{ 711,  915}, { 564,  710}, {1349,  673}, {1349,  674}, {1138, 1403}, 
{ 470,  563}, { 916, 1137}, { 795,  910}, {1969, 2709}, { 217,  279}, 
{ 280,  360}, { 276,  356}, { 357,  469}, {1102, 2695}, {1102, 2696}, 
{1102, 2699}, { 672, 2550}, {1349,  675}, {1102, 2700}, { 422,  537}, 
{ 429,  544}, {2053,  853}, { 375,  486}, {2053,  854}, {1888, 2242}, 
{ 370,  486}, {2007, 2701}, {2007, 2702}, {2007, 2703}, {2007, 2704}, 
{1299, 1541}, {1417, 1678}, {1996, 2254}, {2053,  645}, {1876, 2255}, 
{1154, 2754}, {1156, 1429}, {1349,  676}, { 144,  210}, {2142, 2162}, 
{1825, 1969}, {1483, 1720}, {1349,  863}, {1349,  678}, {1349,  864}, 
{ 921, 2755}, {1503,  817}, {  88,  143}, {1503, 2699}, {1503, 2697}, 
{1503, 2698}, {1503, 2700}, {1503, 2706}, {1503, 2705}, {1349,  680}, 
{2005, 2708}, {1719, 1910}, {1248, 1488}, { 778, 2755}, {2053,  855}, 
{ 768, 2755}, {2053,  647}, {1051, 1290}, {1212, 2756}, {1503,  183}, 
{2053,  648}, {1015, 2695}, {1015, 2696}, {1015, 2699}, {1349,  681}, 
{1349,  865}, {1015, 2700}, {2053,  649}, {1676, 1877}, { 854, 1073}, 
{1349,  866}, {1969, 2710}, {2009, 2093}, {1674, 1875}, {2053, 2485}, 
{1349, 2581}, { 505, 2701}, { 505, 2702}, { 505, 2703}, { 505, 2704}, 
{1220, 2438}, {1349,  867}, { 824, 2701}, { 824, 2702}, { 824, 2703}, 
{ 824, 2704}, {2053,  856}, {2186, 2208}, {1349,  685}, {1161, 1157}, 
{1842, 1976}, {2053, 2489}, {2005, 2709}, {1349,  686}, {1349,  687}, 
{ 830, 1045}, { 841, 1056}, {1349,  688}, { 498, 2699}, { 498, 2697}, 
{ 498, 2698}, { 498, 2700}, {1948, 1976}, { 745,  949}, { 814, 2492}, 
{ 880, 1103}, {1349,  868}, { 337, 2760}, {1349,  690}, {1349,  869}, 
{ 345, 2760}, { 346, 2760}, { 336, 2760}, {1349,  870}, {2053,  857}, 
{1349,  693}, {1151, 2699}, {1151, 2697}, {1151, 2698}, {1151, 2700}, 
{ 332, 2760}, {2053,  652}, {1349,  694}, { 331, 2760}, {2053,  858}, 
{ 325, 2760}, {2053,  859}, { 324, 2760}, {2018, 2701}, {2018, 2702}, 
{2018, 2703}, {2018, 2704}, { 322, 2760}, {2053,  655}, { 321, 2760}, 
{ 644, 2711}, {1349,  695}, { 644, 2712}, {1969, 2707}, { 340, 2760}, 
{ 341, 2760}, {1349,  696}, {1349,  871}, { 448, 2760}, { 255, 2760}, 
{ 254, 2760}, { 194, 2760}, { 644, 2701}, { 644, 2702}, { 644, 2703}, 
{ 644, 2704}, { 252, 2760}, { 251, 2760}, { 193, 2760}, { 188, 2760}, 
{ 447, 2760}, { 450, 2760}, { 460, 2760}, { 459, 2760}, { 451, 2760}, 
{2005, 2710}, {1349,  872}, { 186, 2760}, { 457, 2760}, { 456, 2760}, 
{ 878, 1102}, { 854,   61}, { 414,  533}, {1884, 2007}, {2053,  860}, 
{2199, 2762}, {2053,  659}, { 140, 2859}, { 723, 2762}, {1173, 2762}, 
{2053, 2530}, {2053,  660}, {2053,  661}, { 877, 1101}, {2016, 2701}, 
{2016, 2702}, {2016, 2703}, {2016, 2704}, { 879, 4707}, {1098, 2558}, 
{ 455, 2763}, {1058, 1301}, {1890, 2699}, {1890, 2697}, {1890, 2698}, 
{1890, 2700}, { 645, 2695}, { 645, 2696}, { 645, 2699}, { 458, 2763}, 
{2053,  662}, { 645, 2700}, { 449, 2763}, {2053,  663}, {2053,  664}, 
{1969, 2711}, {1011, 1240}, {1969, 2712}, {1320, 1559}, { 339, 2763}, 
{ 185, 2763}, { 446, 2763}, { 854,   62}, { 320, 2763}, {2053,  861}, 
{1244, 1486}, { 344, 2763}, {1969, 2701}, {1969, 2702}, {1969, 2703}, 
{1969, 2704}, {2053,  666}, {2053,  667}, { 191, 2763}, { 192, 2763}, 
{2053,  668}, {1666, 2763}, { 802, 1014}, { 253, 2763}, { 634, 2763}, 
{ 335, 2763}, { 334, 2763}, {2005, 2707}, { 330, 2763}, { 443, 2763}, 
{ 635, 2763}, {1503,   61}, {1486, 2701}, {1486, 2702}, {1486, 2703}, 
{1486, 2704}, { 636, 2763}, {1626, 2763}, { 844, 1059}, { 343, 2763}, 
{1204, 1448}, {1503, 2708}, {1208, 1448}, { 442, 2763}, { 663, 2763}, 
{1145, 2701}, {1145, 2702}, {1145, 2703}, {1145, 2704}, { 323, 2763}, 
{ 250, 2763}, {2053,  669}, {1533, 1745}, { 637, 2763}, {1636, 1840}, 
{2053,  670}, { 899,   57}, {2053,  862}, { 899, 2699}, { 899, 2697}, 
{ 899, 2698}, { 899, 2700}, { 899, 2706}, {2053,  672}, {1622, 2763}, 
{ 681, 2578}, { 678, 2574}, { 518, 2701}, { 518, 2702}, { 518, 2703}, 
{ 518, 2704}, {2051, 2116}, {1503,  818}, {1062, 1306}, { 899,   60}, 
{ 991,  992}, { 794,  992}, {2053,  673}, {2053,  674}, { 805, 1016}, 
{ 825, 1038}, { 826, 1039}, {1298, 1540}, {1503, 2709}, {1075, 1016}, 
{ 164,  233}, { 506,  602}, { 524,  618}, {1362, 1605}, {1364, 1607}, 
{2208, 2764}, {1580, 1784}, {2053,  675}, {1042, 1280}, {1581, 1607}, 
{1555, 5195}, {1524,  853}, {1317, 5164}, {1524,  854}, {1760, 5234}, 
{2087, 2134}, {2005, 2701}, {2005, 2702}, {2005, 2703}, {2005, 2704}, 
{1469, 1233}, {1232, 1233}, {1007, 1233}, {1524,  645}, {2086, 2134}, 
{ 277,  357}, {2193, 2398}, {2053,  676}, {2090, 2135}, {2127, 2155}, 
{  19,   78}, {1694, 5231}, {2053,  863}, {2053,  678}, {2053,  864}, 
{  20,   77}, {1387,  817}, {  21,   78}, {1387, 2699}, {1387, 2697}, 
{1387, 2698}, {1387, 2700}, {1387, 2706}, {1387, 2705}, {2053,  680}, 
{  67, 5054}, { 732, 5089}, {  22,   78}, {1478, 1717}, {1524,  855}, 
{ 309, 5058}, {1524,  647}, {  23,   78}, {  18,   77}, {1387,  183}, 
{1524,  648}, { 123, 5055}, { 312, 5059}, { 904, 1131}, {2053,  681}, 
{2053,  865}, { 125,  168}, {1524,  649}, {  24,   78}, { 550,   78}, 
{2053,  866}, {1503, 2710}, {1382, 1631}, { 124, 5056}, {1524, 2485}, 
{2053, 2581}, { 772, 2699}, { 772, 2697}, { 772, 2698}, { 772, 2700}, 
{2101,   78}, {2053,  867}, { 176,  245}, {1217, 1456}, {1551, 1758}, 
{ 408,  527}, {1524,  856}, { 517, 4726}, {2053,  685}, {2089, 4699}, 
{ 415, 4726}, {1524, 2489}, { 895, 4726}, {2053,  686}, {2053,  687}, 
{ 200,  264}, {1883, 4726}, {2053,  688}, { 783, 2699}, { 783, 2697}, 
{ 783, 2698}, { 783, 2700}, {1050, 4726}, { 203, 4725}, { 202, 4724}, 
{ 201,  265}, {2053,  868}, { 139,  209}, {2053,  690}, {2053,  869}, 
{ 269,  209}, { 270,  209}, {1163, 2626}, {2053,  870}, {1524,  857}, 
{2053,  693}, {1734, 2699}, {1734, 2697}, {1734, 2698}, {1734, 2700}, 
{1171, 1438}, {1524,  652}, {2053,  694}, { 577, 4448}, {1524,  858}, 
{ 367,  482}, {1524,  859}, { 722,  925}, { 835, 2701}, { 835, 2702}, 
{ 835, 2703}, { 835, 2704}, {1693, 2625}, {1524,  655}, {   0,    0}, 
{ 854, 2711}, {2053,  695}, { 854, 2712}, {1503, 2707}, {   0,    0}, 
{   0,    0}, {2053,  696}, {2053,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 854, 2701}, { 854, 2702}, { 854, 2703}, 
{ 854, 2704}, {   0,    0}, {   0,    0}, {1708, 2701}, {1708, 2702}, 
{1708, 2703}, {1708, 2704}, { 523, 2701}, { 523, 2702}, { 523, 2703}, 
{ 523, 2704}, {2053,  872}, {   0,    0}, {   0,    0}, { 899, 1127}, 
{   0,    0}, { 899,   61}, {   0,    0}, {   0,    0}, {1524,  860}, 
{   0,    0}, {1524,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1524, 2530}, {1524,  660}, {1524,  661}, {   0,    0}, {1102, 2701}, 
{1102, 2702}, {1102, 2703}, {1102, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1311, 2699}, {1311, 2697}, {1311, 2698}, 
{1311, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1524,  662}, {   0,    0}, {   0,    0}, {1524,  663}, {1524,  664}, 
{1503, 2711}, {   0,    0}, {1503, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 899,   62}, {   0,    0}, {1524,  861}, 
{   0,    0}, {   0,    0}, {1503, 2701}, {1503, 2702}, {1503, 2703}, 
{1503, 2704}, {1524,  666}, {1524,  667}, {   0,    0}, {   0,    0}, 
{1524,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1677, 2699}, {1677, 2697}, {1677, 2698}, 
{1677, 2700}, {1387,   61}, {1015, 2701}, {1015, 2702}, {1015, 2703}, 
{1015, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1387, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1524,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1524,  670}, {1645,   57}, {1524,  862}, {1645, 2699}, {1645, 2697}, 
{1645, 2698}, {1645, 2700}, {1645, 2706}, {1524,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 498, 2701}, { 498, 2702}, { 498, 2703}, 
{ 498, 2704}, {   0,    0}, {1387,  818}, {   0,    0}, {1645,   60}, 
{   0,    0}, {   0,    0}, {1524,  673}, {1524,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1387, 2709}, {   0,    0}, 
{1151, 2701}, {1151, 2702}, {1151, 2703}, {1151, 2704}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1524,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1804,  853}, {   0,    0}, {1804,  854}, {1710, 2699}, 
{1710, 2697}, {1710, 2698}, {1710, 2700}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1804,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1524,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1524,  863}, {1524,  678}, {1524,  864}, 
{   0,    0}, { 653,  817}, {   0,    0}, { 653, 2699}, { 653, 2697}, 
{ 653, 2698}, { 653, 2700}, { 653, 2706}, { 653, 2705}, {1524,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1804,  855}, 
{   0,    0}, {1804,  647}, {   0,    0}, {   0,    0}, { 653,  183}, 
{1804,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1524,  681}, 
{1524,  865}, {   0,    0}, {1804,  649}, {   0,    0}, {   0,    0}, 
{1524,  866}, {1387, 2710}, {   0,    0}, {   0,    0}, {1804, 2485}, 
{1524, 2581}, {1890, 2701}, {1890, 2702}, {1890, 2703}, {1890, 2704}, 
{   0,    0}, {1524,  867}, { 645, 2701}, { 645, 2702}, { 645, 2703}, 
{ 645, 2704}, {1804,  856}, {   0,    0}, {1524,  685}, {   0,    0}, 
{   0,    0}, {1804, 2489}, {   0,    0}, {1524,  686}, {1524,  687}, 
{   0,    0}, {   0,    0}, {1524,  688}, { 767, 2699}, { 767, 2697}, 
{ 767, 2698}, { 767, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1524,  868}, {   0,    0}, {1524,  690}, {1524,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1524,  870}, {1804,  857}, 
{1524,  693}, { 978, 2699}, { 978, 2697}, { 978, 2698}, { 978, 2700}, 
{   0,    0}, {1804,  652}, {1524,  694}, {   0,    0}, {1804,  858}, 
{   0,    0}, {1804,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1804,  655}, {   0,    0}, 
{ 899, 2711}, {1524,  695}, { 899, 2712}, {1387, 2707}, {   0,    0}, 
{   0,    0}, {1524,  696}, {1524,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 899, 2701}, { 899, 2702}, { 899, 2703}, 
{ 899, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1524,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1645,   61}, {   0,    0}, {   0,    0}, {1804,  860}, 
{   0,    0}, {1804,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1804, 2530}, {1804,  660}, {1804,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 979, 2699}, { 979, 2697}, { 979, 2698}, 
{ 979, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1804,  662}, {   0,    0}, {   0,    0}, {1804,  663}, {1804,  664}, 
{1387, 2711}, {   0,    0}, {1387, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1645,   62}, {   0,    0}, {1804,  861}, 
{   0,    0}, {   0,    0}, {1387, 2701}, {1387, 2702}, {1387, 2703}, 
{1387, 2704}, {1804,  666}, {1804,  667}, {   0,    0}, {   0,    0}, 
{1804,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1070, 2699}, {1070, 2697}, {1070, 2698}, 
{1070, 2700}, { 653,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 653, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 772, 2701}, { 772, 2702}, { 772, 2703}, { 772, 2704}, {   0,    0}, 
{   0,    0}, {1804,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1804,  670}, {   0,    0}, {1804,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1645, 2584}, {   0,    0}, {1804,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 783, 2701}, { 783, 2702}, { 783, 2703}, 
{ 783, 2704}, {   0,    0}, { 653,  818}, {1207, 2699}, {1207, 2697}, 
{1207, 2698}, {1207, 2700}, {1804,  673}, {1804,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 653, 2709}, {   0,    0}, 
{1734, 2701}, {1734, 2702}, {1734, 2703}, {1734, 2704}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1804,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1955,  853}, {   0,    0}, {1955,  854}, {2010, 2699}, 
{2010, 2697}, {2010, 2698}, {2010, 2700}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1955,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1804,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1804,  863}, {1804,  678}, {1804,  864}, 
{   0,    0}, {2027,  817}, {   0,    0}, {2027, 2699}, {2027, 2697}, 
{2027, 2698}, {2027, 2700}, {2027, 2706}, {2027, 2705}, {1804,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1955,  855}, 
{   0,    0}, {1955,  647}, {   0,    0}, {   0,    0}, {2027,  183}, 
{1955,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1804,  681}, 
{1804,  865}, {   0,    0}, {1955,  649}, {   0,    0}, {   0,    0}, 
{1804,  866}, { 653, 2710}, {   0,    0}, {   0,    0}, {1955, 2485}, 
{1804, 2581}, {1311, 2701}, {1311, 2702}, {1311, 2703}, {1311, 2704}, 
{   0,    0}, {1804,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1955,  856}, {   0,    0}, {1804,  685}, {   0,    0}, 
{   0,    0}, {1955, 2489}, {   0,    0}, {1804,  686}, {1804,  687}, 
{   0,    0}, {   0,    0}, {1804,  688}, {1732, 2699}, {1732, 2697}, 
{1732, 2698}, {1732, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1804,  868}, {   0,    0}, {1804,  690}, {1804,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1804,  870}, {1955,  857}, 
{1804,  693}, {1677, 2701}, {1677, 2702}, {1677, 2703}, {1677, 2704}, 
{   0,    0}, {1955,  652}, {1804,  694}, {   0,    0}, {1955,  858}, 
{   0,    0}, {1955,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1955,  655}, {   0,    0}, 
{1645, 2711}, {1804,  695}, {1645, 2712}, { 653, 2707}, {   0,    0}, 
{   0,    0}, {1804,  696}, {1804,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1645, 2701}, {1645, 2702}, {1645, 2703}, 
{1645, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1804,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1955,  860}, 
{   0,    0}, {1955,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1955, 2530}, {1955,  660}, {1955,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1710, 2701}, {1710, 2702}, 
{1710, 2703}, {1710, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1955,  662}, {   0,    0}, {   0,    0}, {1955,  663}, {1955,  664}, 
{ 653, 2711}, {   0,    0}, { 653, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1955,  861}, 
{   0,    0}, {   0,    0}, { 653, 2701}, { 653, 2702}, { 653, 2703}, 
{ 653, 2704}, {1955,  666}, {1955,  667}, {   0,    0}, {   0,    0}, 
{1955,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 433, 2699}, { 433, 2697}, { 433, 2698}, 
{ 433, 2700}, {2027,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2027, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1955,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1955,  670}, {1328,   57}, {1955,  862}, {1328, 2699}, {1328, 2697}, 
{1328, 2698}, {1328, 2700}, {1328, 2706}, {1955,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 767, 2701}, { 767, 2702}, { 767, 2703}, 
{ 767, 2704}, {   0,    0}, {2027,  818}, {   0,    0}, {1328,   60}, 
{   0,    0}, {   0,    0}, {1955,  673}, {1955,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2027, 2709}, {   0,    0}, 
{ 978, 2701}, { 978, 2702}, { 978, 2703}, { 978, 2704}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1955,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1956,  853}, {   0,    0}, {1956,  854}, { 434, 2699}, 
{ 434, 2697}, { 434, 2698}, { 434, 2700}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1956,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1955,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1955,  863}, {1955,  678}, {1955,  864}, 
{   0,    0}, {1724,  817}, {   0,    0}, {1724, 2699}, {1724, 2697}, 
{1724, 2698}, {1724, 2700}, {1724, 2706}, {1724, 2705}, {1955,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1956,  855}, 
{   0,    0}, {1956,  647}, {   0,    0}, {   0,    0}, {1724,  183}, 
{1956,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1955,  681}, 
{1955,  865}, {   0,    0}, {1956,  649}, {   0,    0}, {   0,    0}, 
{1955,  866}, {2027, 2710}, {   0,    0}, {   0,    0}, {1956, 2485}, 
{1955, 2581}, { 979, 2701}, { 979, 2702}, { 979, 2703}, { 979, 2704}, 
{   0,    0}, {1955,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1956,  856}, {   0,    0}, {1955,  685}, {   0,    0}, 
{   0,    0}, {1956, 2489}, {   0,    0}, {1955,  686}, {1955,  687}, 
{   0,    0}, {   0,    0}, {1955,  688}, {2074, 2699}, {2074, 2697}, 
{2074, 2698}, {2074, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1955,  868}, {   0,    0}, {1955,  690}, {1955,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1955,  870}, {1956,  857}, 
{1955,  693}, {1070, 2701}, {1070, 2702}, {1070, 2703}, {1070, 2704}, 
{   0,    0}, {1956,  652}, {1955,  694}, {   0,    0}, {1956,  858}, 
{   0,    0}, {1956,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2004, 2693}, {   0,    0}, {2004, 2699}, {1956,  655}, {   0,    0}, 
{2004, 2700}, {1955,  695}, {2004, 2705}, {2027, 2707}, {   0,    0}, 
{   0,    0}, {1955,  696}, {1955,  871}, {   0,    0}, {2176, 2699}, 
{2176, 2697}, {2176, 2698}, {2176, 2700}, {2004,  516}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1955,  872}, {1207, 2701}, {1207, 2702}, {1207, 2703}, 
{1207, 2704}, {1328,   61}, {   0,    0}, {   0,    0}, {1956,  860}, 
{   0,    0}, {1956,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1956, 2530}, {1956,  660}, {1956,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2010, 2701}, {2010, 2702}, 
{2010, 2703}, {2010, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1956,  662}, {   0,    0}, {   0,    0}, {1956,  663}, {1956,  664}, 
{2027, 2711}, {   0,    0}, {2027, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1328,   62}, {   0,    0}, {1956,  861}, 
{   0,    0}, {   0,    0}, {2027, 2701}, {2027, 2702}, {2027, 2703}, 
{2027, 2704}, {1956,  666}, {1956,  667}, {   0,    0}, {   0,    0}, 
{1956,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 754, 2699}, { 754, 2697}, { 754, 2698}, 
{ 754, 2700}, {1724,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1724, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1956,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1956,  670}, { 673,   57}, {1956,  862}, { 673, 2699}, { 673, 2697}, 
{ 673, 2698}, { 673, 2700}, { 673, 2706}, {1956,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1732, 2701}, {1732, 2702}, {1732, 2703}, 
{1732, 2704}, {   0,    0}, {1724,  818}, {   0,    0}, { 673,   60}, 
{   0,    0}, {   0,    0}, {1956,  673}, {1956,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1724, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1956,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1757,  853}, {   0,    0}, {1757,  854}, { 995, 2699}, 
{ 995, 2697}, { 995, 2698}, { 995, 2700}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1757,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1956,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1956,  863}, {1956,  678}, {1956,  864}, 
{   0,    0}, {1726,  817}, {   0,    0}, {1726, 2699}, {1726, 2697}, 
{1726, 2698}, {1726, 2700}, {1726, 2706}, {1726, 2705}, {1956,  680}, 
{2004, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, {1757,  855}, 
{   0,    0}, {1757,  647}, {   0,    0}, {   0,    0}, {1726,  183}, 
{1757,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1956,  681}, 
{1956,  865}, {   0,    0}, {1757,  649}, {   0,    0}, {   0,    0}, 
{1956,  866}, {1724, 2710}, {   0,    0}, {   0,    0}, {1757, 2485}, 
{1956, 2581}, { 371, 2699}, { 371, 2697}, { 371, 2698}, { 371, 2700}, 
{   0,    0}, {1956,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1757,  856}, {   0,    0}, {1956,  685}, {   0,    0}, 
{   0,    0}, {1757, 2489}, {2004, 2709}, {1956,  686}, {1956,  687}, 
{   0,    0}, {   0,    0}, {1956,  688}, { 120, 2699}, { 120, 2697}, 
{ 120, 2698}, { 120, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1956,  868}, {   0,    0}, {1956,  690}, {1956,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1956,  870}, {1757,  857}, 
{1956,  693}, { 433, 2701}, { 433, 2702}, { 433, 2703}, { 433, 2704}, 
{   0,    0}, {1757,  652}, {1956,  694}, {   0,    0}, {1757,  858}, 
{   0,    0}, {1757,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1757,  655}, {   0,    0}, 
{1328, 2711}, {1956,  695}, {1328, 2712}, {1724, 2707}, {   0,    0}, 
{   0,    0}, {1956,  696}, {1956,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1328, 2701}, {1328, 2702}, {1328, 2703}, 
{1328, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2004, 2710}, {1956,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 673,   61}, {   0,    0}, {   0,    0}, {1757,  860}, 
{   0,    0}, {1757,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1757, 2530}, {1757,  660}, {1757,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 434, 2701}, { 434, 2702}, 
{ 434, 2703}, { 434, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1757,  662}, {   0,    0}, {   0,    0}, {1757,  663}, {1757,  664}, 
{1724, 2711}, {   0,    0}, {1724, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 673,   62}, {   0,    0}, {1757,  861}, 
{   0,    0}, {   0,    0}, {1724, 2701}, {1724, 2702}, {1724, 2703}, 
{1724, 2704}, {1757,  666}, {1757,  667}, {   0,    0}, {   0,    0}, 
{1757,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2004, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1726,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1726, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1757,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1757,  670}, {1306,   57}, {1757,  862}, {1306, 2699}, {1306, 2697}, 
{1306, 2698}, {1306, 2700}, {1306, 2706}, {1757,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2074, 2701}, {2074, 2702}, {2074, 2703}, 
{2074, 2704}, {   0,    0}, {1726,  818}, {   0,    0}, {1306,   60}, 
{   0,    0}, {   0,    0}, {1757,  673}, {1757,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1726, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1757,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1957,  853}, {   0,    0}, {1957,  854}, {   0,    0}, 
{   0,    0}, {2004, 2701}, {2004, 2702}, {2004, 2703}, {2004, 2704}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1957,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1757,  676}, {2176, 2701}, {2176, 2702}, 
{2176, 2703}, {2176, 2704}, {1757,  863}, {1757,  678}, {1757,  864}, 
{   0,    0}, {1247,  817}, {   0,    0}, {1247, 2699}, {1247, 2697}, 
{1247, 2698}, {1247, 2700}, {1247, 2706}, {1247, 2705}, {1757,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1957,  855}, 
{   0,    0}, {1957,  647}, {   0,    0}, {   0,    0}, {1247,  183}, 
{1957,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1757,  681}, 
{1757,  865}, {   0,    0}, {1957,  649}, {   0,    0}, {   0,    0}, 
{1757,  866}, {1726, 2710}, {   0,    0}, {   0,    0}, {1957, 2485}, 
{1757, 2581}, {1448, 2699}, {1448, 2697}, {1448, 2698}, {1448, 2700}, 
{   0,    0}, {1757,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1957,  856}, {   0,    0}, {1757,  685}, {   0,    0}, 
{   0,    0}, {1957, 2489}, {   0,    0}, {1757,  686}, {1757,  687}, 
{   0,    0}, {   0,    0}, {1757,  688}, { 399, 2699}, { 399, 2697}, 
{ 399, 2698}, { 399, 2700}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1757,  868}, {   0,    0}, {1757,  690}, {1757,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1757,  870}, {1957,  857}, 
{1757,  693}, { 754, 2701}, { 754, 2702}, { 754, 2703}, { 754, 2704}, 
{   0,    0}, {1957,  652}, {1757,  694}, {   0,    0}, {1957,  858}, 
{   0,    0}, {1957,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1957,  655}, {   0,    0}, 
{ 673, 2711}, {1757,  695}, { 673, 2712}, {1726, 2707}, {   0,    0}, 
{   0,    0}, {1757,  696}, {1757,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 673, 2701}, { 673, 2702}, { 673, 2703}, 
{ 673, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1757,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1306,   61}, {   0,    0}, {   0,    0}, {1957,  860}, 
{   0,    0}, {1957,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1957, 2530}, {1957,  660}, {1957,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 995, 2701}, { 995, 2702}, 
{ 995, 2703}, { 995, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1957,  662}, {   0,    0}, {   0,    0}, {1957,  663}, {1957,  664}, 
{1726, 2711}, {   0,    0}, {1726, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1306,   62}, {   0,    0}, {1957,  861}, 
{   0,    0}, {   0,    0}, {1726, 2701}, {1726, 2702}, {1726, 2703}, 
{1726, 2704}, {1957,  666}, {1957,  667}, {   0,    0}, {   0,    0}, 
{1957,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1205, 2699}, {1205, 2697}, {1205, 2698}, 
{1205, 2700}, {1247,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1247, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 371, 2701}, { 371, 2702}, { 371, 2703}, { 371, 2704}, {   0,    0}, 
{   0,    0}, {1957,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1957,  670}, {1242,   57}, {1957,  862}, {1242, 2699}, {1242, 2697}, 
{1242, 2698}, {1242, 2700}, {1242, 2706}, {1957,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 120, 2701}, { 120, 2702}, { 120, 2703}, 
{ 120, 2704}, {   0,    0}, {1247,  818}, {   0,    0}, {1242,   60}, 
{   0,    0}, {   0,    0}, {1957,  673}, {1957,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1247, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1957,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1911,  853}, {   0,    0}, {1911,  854}, { 622, 2699}, 
{ 622, 2697}, { 622, 2698}, { 622, 2700}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1911,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1957,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1957,  863}, {1957,  678}, {1957,  864}, 
{   0,    0}, {2173,  817}, {   0,    0}, {2173, 2699}, {2173, 2697}, 
{2173, 2698}, {2173, 2700}, {2173, 2706}, {2173, 2705}, {1957,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1911,  855}, 
{   0,    0}, {1911,  647}, {   0,    0}, {   0,    0}, {2173,  183}, 
{1911,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1957,  681}, 
{1957,  865}, {   0,    0}, {1911,  649}, {   0,    0}, {   0,    0}, 
{1957,  866}, {1247, 2710}, {   0,    0}, {   0,    0}, {1911, 2485}, 
{1957, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1957,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1911,  856}, {   0,    0}, {1957,  685}, {   0,    0}, 
{   0,    0}, {1911, 2489}, {   0,    0}, {1957,  686}, {1957,  687}, 
{   0,    0}, {   0,    0}, {1957,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1957,  868}, {   0,    0}, {1957,  690}, {1957,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1957,  870}, {1911,  857}, 
{1957,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1911,  652}, {1957,  694}, {   0,    0}, {1911,  858}, 
{   0,    0}, {1911,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1911,  655}, {   0,    0}, 
{1306, 2711}, {1957,  695}, {1306, 2712}, {1247, 2707}, {   0,    0}, 
{   0,    0}, {1957,  696}, {1957,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1306, 2701}, {1306, 2702}, {1306, 2703}, 
{1306, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1957,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1242,   61}, {   0,    0}, {   0,    0}, {1911,  860}, 
{   0,    0}, {1911,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1911, 2530}, {1911,  660}, {1911,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1911,  662}, {   0,    0}, {   0,    0}, {1911,  663}, {1911,  664}, 
{1247, 2711}, {   0,    0}, {1247, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1242,   62}, {   0,    0}, {1911,  861}, 
{   0,    0}, {   0,    0}, {1247, 2701}, {1247, 2702}, {1247, 2703}, 
{1247, 2704}, {1911,  666}, {1911,  667}, {   0,    0}, {   0,    0}, 
{1911,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2173,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2173, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1448, 2701}, {1448, 2702}, {1448, 2703}, {1448, 2704}, {   0,    0}, 
{   0,    0}, {1911,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1911,  670}, { 654,   57}, {1911,  862}, { 654, 2699}, { 654, 2697}, 
{ 654, 2698}, { 654, 2700}, { 654, 2706}, {1911,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 399, 2701}, { 399, 2702}, { 399, 2703}, 
{ 399, 2704}, {   0,    0}, {2173,  818}, {   0,    0}, { 654,   60}, 
{   0,    0}, {   0,    0}, {1911,  673}, {1911,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2173, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1911,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2103,  853}, {   0,    0}, {2103,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2103,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1911,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1911,  863}, {1911,  678}, {1911,  864}, 
{   0,    0}, {1252,  817}, {   0,    0}, {1252, 2699}, {1252, 2697}, 
{1252, 2698}, {1252, 2700}, {1252, 2706}, {1252, 2705}, {1911,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2103,  855}, 
{   0,    0}, {2103,  647}, {   0,    0}, {   0,    0}, {1252,  183}, 
{2103,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1911,  681}, 
{1911,  865}, {   0,    0}, {2103,  649}, {   0,    0}, {   0,    0}, 
{1911,  866}, {2173, 2710}, {   0,    0}, {   0,    0}, {2103, 2485}, 
{1911, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1911,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2103,  856}, {   0,    0}, {1911,  685}, {   0,    0}, 
{   0,    0}, {2103, 2489}, {   0,    0}, {1911,  686}, {1911,  687}, 
{   0,    0}, {   0,    0}, {1911,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1911,  868}, {   0,    0}, {1911,  690}, {1911,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1911,  870}, {2103,  857}, 
{1911,  693}, {1205, 2701}, {1205, 2702}, {1205, 2703}, {1205, 2704}, 
{   0,    0}, {2103,  652}, {1911,  694}, {   0,    0}, {2103,  858}, 
{   0,    0}, {2103,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2103,  655}, {   0,    0}, 
{1242, 2711}, {1911,  695}, {1242, 2712}, {2173, 2707}, {   0,    0}, 
{   0,    0}, {1911,  696}, {1911,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1242, 2701}, {1242, 2702}, {1242, 2703}, 
{1242, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1911,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 654,   61}, {   0,    0}, {   0,    0}, {2103,  860}, 
{   0,    0}, {2103,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2103, 2530}, {2103,  660}, {2103,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 622, 2701}, { 622, 2702}, 
{ 622, 2703}, { 622, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2103,  662}, {   0,    0}, {   0,    0}, {2103,  663}, {2103,  664}, 
{2173, 2711}, {   0,    0}, {2173, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 654,   62}, {   0,    0}, {2103,  861}, 
{   0,    0}, {   0,    0}, {2173, 2701}, {2173, 2702}, {2173, 2703}, 
{2173, 2704}, {2103,  666}, {2103,  667}, {   0,    0}, {   0,    0}, 
{2103,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1252,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1252, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2103,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2103,  670}, {1385,   57}, {2103,  862}, {1385, 2699}, {1385, 2697}, 
{1385, 2698}, {1385, 2700}, {1385, 2706}, {2103,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1252,  818}, {   0,    0}, {1385,   60}, 
{   0,    0}, {   0,    0}, {2103,  673}, {2103,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1252, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2103,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  853}, {   0,    0}, {1606,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1606,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2103,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2103,  863}, {2103,  678}, {2103,  864}, 
{   0,    0}, {1709,  817}, {   0,    0}, {1709, 2699}, {1709, 2697}, 
{1709, 2698}, {1709, 2700}, {1709, 2706}, {1709, 2705}, {2103,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1606,  855}, 
{   0,    0}, {1606,  647}, {   0,    0}, {   0,    0}, {1709,  183}, 
{1606,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2103,  681}, 
{2103,  865}, {   0,    0}, {1606,  649}, {   0,    0}, {   0,    0}, 
{2103,  866}, {1252, 2710}, {   0,    0}, {   0,    0}, {1606, 2485}, 
{2103, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2103,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  856}, {   0,    0}, {2103,  685}, {   0,    0}, 
{   0,    0}, {1606, 2489}, {   0,    0}, {2103,  686}, {2103,  687}, 
{   0,    0}, {   0,    0}, {2103,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2103,  868}, {   0,    0}, {2103,  690}, {2103,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2103,  870}, {1606,  857}, 
{2103,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  652}, {2103,  694}, {   0,    0}, {1606,  858}, 
{   0,    0}, {1606,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1606,  655}, {   0,    0}, 
{ 654, 2711}, {2103,  695}, { 654, 2712}, {1252, 2707}, {   0,    0}, 
{   0,    0}, {2103,  696}, {2103,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 654, 2701}, { 654, 2702}, { 654, 2703}, 
{ 654, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2103,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1385,   61}, {   0,    0}, {   0,    0}, {1606,  860}, 
{   0,    0}, {1606,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1606, 2530}, {1606,  660}, {1606,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1606,  662}, {   0,    0}, {   0,    0}, {1606,  663}, {1606,  664}, 
{1252, 2711}, {   0,    0}, {1252, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1385,   62}, {   0,    0}, {1606,  861}, 
{   0,    0}, {   0,    0}, {1252, 2701}, {1252, 2702}, {1252, 2703}, 
{1252, 2704}, {1606,  666}, {1606,  667}, {   0,    0}, {   0,    0}, 
{1606,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1709,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1709, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1606,  670}, {1950,   57}, {1606,  862}, {1950, 2699}, {1950, 2697}, 
{1950, 2698}, {1950, 2700}, {1950, 2706}, {1606,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1709,  818}, {   0,    0}, {1950,   60}, 
{   0,    0}, {   0,    0}, {1606,  673}, {1606,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1709, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1606,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  853}, {   0,    0}, {1501,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1501,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1606,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1606,  863}, {1606,  678}, {1606,  864}, 
{   0,    0}, {1744,  817}, {   0,    0}, {1744, 2699}, {1744, 2697}, 
{1744, 2698}, {1744, 2700}, {1744, 2706}, {1744, 2705}, {1606,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1501,  855}, 
{   0,    0}, {1501,  647}, {   0,    0}, {   0,    0}, {1744,  183}, 
{1501,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1606,  681}, 
{1606,  865}, {   0,    0}, {1501,  649}, {   0,    0}, {   0,    0}, 
{1606,  866}, {1709, 2710}, {   0,    0}, {   0,    0}, {1501, 2485}, 
{1606, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  856}, {   0,    0}, {1606,  685}, {   0,    0}, 
{   0,    0}, {1501, 2489}, {   0,    0}, {1606,  686}, {1606,  687}, 
{   0,    0}, {   0,    0}, {1606,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  868}, {   0,    0}, {1606,  690}, {1606,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1606,  870}, {1501,  857}, 
{1606,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  652}, {1606,  694}, {   0,    0}, {1501,  858}, 
{   0,    0}, {1501,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1501,  655}, {   0,    0}, 
{1385, 2711}, {1606,  695}, {1385, 2712}, {1709, 2707}, {   0,    0}, 
{   0,    0}, {1606,  696}, {1606,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1385, 2701}, {1385, 2702}, {1385, 2703}, 
{1385, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1606,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1950,   61}, {   0,    0}, {   0,    0}, {1501,  860}, 
{   0,    0}, {1501,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1501, 2530}, {1501,  660}, {1501,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1501,  662}, {   0,    0}, {   0,    0}, {1501,  663}, {1501,  664}, 
{1709, 2711}, {   0,    0}, {1709, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1950,   62}, {   0,    0}, {1501,  861}, 
{   0,    0}, {   0,    0}, {1709, 2701}, {1709, 2702}, {1709, 2703}, 
{1709, 2704}, {1501,  666}, {1501,  667}, {   0,    0}, {   0,    0}, 
{1501,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1744,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1744, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1501,  670}, { 862,   57}, {1501,  862}, { 862, 2699}, { 862, 2697}, 
{ 862, 2698}, { 862, 2700}, { 862, 2706}, {1501,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1744,  818}, {   0,    0}, { 862,   60}, 
{   0,    0}, {   0,    0}, {1501,  673}, {1501,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1744, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1501,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  853}, {   0,    0}, {2024,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2024,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1501,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1501,  863}, {1501,  678}, {1501,  864}, 
{   0,    0}, {2213,  817}, {   0,    0}, {2213, 2699}, {2213, 2697}, 
{2213, 2698}, {2213, 2700}, {2213, 2706}, {2213, 2705}, {1501,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2024,  855}, 
{   0,    0}, {2024,  647}, {   0,    0}, {   0,    0}, {2213,  183}, 
{2024,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1501,  681}, 
{1501,  865}, {   0,    0}, {2024,  649}, {   0,    0}, {   0,    0}, 
{1501,  866}, {1744, 2710}, {   0,    0}, {   0,    0}, {2024, 2485}, 
{1501, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  856}, {   0,    0}, {1501,  685}, {   0,    0}, 
{   0,    0}, {2024, 2489}, {   0,    0}, {1501,  686}, {1501,  687}, 
{   0,    0}, {   0,    0}, {1501,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  868}, {   0,    0}, {1501,  690}, {1501,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1501,  870}, {2024,  857}, 
{1501,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  652}, {1501,  694}, {   0,    0}, {2024,  858}, 
{   0,    0}, {2024,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2024,  655}, {   0,    0}, 
{1950, 2711}, {1501,  695}, {1950, 2712}, {1744, 2707}, {   0,    0}, 
{   0,    0}, {1501,  696}, {1501,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1950, 2701}, {1950, 2702}, {1950, 2703}, 
{1950, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1501,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 862,   61}, {   0,    0}, {   0,    0}, {2024,  860}, 
{   0,    0}, {2024,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2024, 2530}, {2024,  660}, {2024,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2024,  662}, {   0,    0}, {   0,    0}, {2024,  663}, {2024,  664}, 
{1744, 2711}, {   0,    0}, {1744, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 862,   62}, {   0,    0}, {2024,  861}, 
{   0,    0}, {   0,    0}, {1744, 2701}, {1744, 2702}, {1744, 2703}, 
{1744, 2704}, {2024,  666}, {2024,  667}, {   0,    0}, {   0,    0}, 
{2024,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2213,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2213, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2024,  670}, {1035,   57}, {2024,  862}, {1035, 2699}, {1035, 2697}, 
{1035, 2698}, {1035, 2700}, {1035, 2706}, {2024,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2213,  818}, {   0,    0}, {1035,   60}, 
{   0,    0}, {   0,    0}, {2024,  673}, {2024,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2213, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2024,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  853}, {   0,    0}, {1810,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1810,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2024,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2024,  863}, {2024,  678}, {2024,  864}, 
{   0,    0}, {1491,  817}, {   0,    0}, {1491, 2699}, {1491, 2697}, 
{1491, 2698}, {1491, 2700}, {1491, 2706}, {1491, 2705}, {2024,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1810,  855}, 
{   0,    0}, {1810,  647}, {   0,    0}, {   0,    0}, {1491,  183}, 
{1810,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2024,  681}, 
{2024,  865}, {   0,    0}, {1810,  649}, {   0,    0}, {   0,    0}, 
{2024,  866}, {2213, 2710}, {   0,    0}, {   0,    0}, {1810, 2485}, 
{2024, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  856}, {   0,    0}, {2024,  685}, {   0,    0}, 
{   0,    0}, {1810, 2489}, {   0,    0}, {2024,  686}, {2024,  687}, 
{   0,    0}, {   0,    0}, {2024,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  868}, {   0,    0}, {2024,  690}, {2024,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2024,  870}, {1810,  857}, 
{2024,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  652}, {2024,  694}, {   0,    0}, {1810,  858}, 
{   0,    0}, {1810,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1810,  655}, {   0,    0}, 
{ 862, 2711}, {2024,  695}, { 862, 2712}, {2213, 2707}, {   0,    0}, 
{   0,    0}, {2024,  696}, {2024,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 862, 2701}, { 862, 2702}, { 862, 2703}, 
{ 862, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2024,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1035,   61}, {   0,    0}, {   0,    0}, {1810,  860}, 
{   0,    0}, {1810,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1810, 2530}, {1810,  660}, {1810,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1810,  662}, {   0,    0}, {   0,    0}, {1810,  663}, {1810,  664}, 
{2213, 2711}, {   0,    0}, {2213, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1035,   62}, {   0,    0}, {1810,  861}, 
{   0,    0}, {   0,    0}, {2213, 2701}, {2213, 2702}, {2213, 2703}, 
{2213, 2704}, {1810,  666}, {1810,  667}, {   0,    0}, {   0,    0}, 
{1810,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1491,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1491, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1810,  670}, {1561,   57}, {1810,  862}, {1561, 2699}, {1561, 2697}, 
{1561, 2698}, {1561, 2700}, {1561, 2706}, {1810,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1491,  818}, {   0,    0}, {1561,   60}, 
{   0,    0}, {   0,    0}, {1810,  673}, {1810,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1491, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1810,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  853}, {   0,    0}, {2020,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2020,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1810,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1810,  863}, {1810,  678}, {1810,  864}, 
{   0,    0}, {1489,  817}, {   0,    0}, {1489, 2699}, {1489, 2697}, 
{1489, 2698}, {1489, 2700}, {1489, 2706}, {1489, 2705}, {1810,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2020,  855}, 
{   0,    0}, {2020,  647}, {   0,    0}, {   0,    0}, {1489,  183}, 
{2020,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1810,  681}, 
{1810,  865}, {   0,    0}, {2020,  649}, {   0,    0}, {   0,    0}, 
{1810,  866}, {1491, 2710}, {   0,    0}, {   0,    0}, {2020, 2485}, 
{1810, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  856}, {   0,    0}, {1810,  685}, {   0,    0}, 
{   0,    0}, {2020, 2489}, {   0,    0}, {1810,  686}, {1810,  687}, 
{   0,    0}, {   0,    0}, {1810,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  868}, {   0,    0}, {1810,  690}, {1810,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1810,  870}, {2020,  857}, 
{1810,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  652}, {1810,  694}, {   0,    0}, {2020,  858}, 
{   0,    0}, {2020,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2020,  655}, {   0,    0}, 
{1035, 2711}, {1810,  695}, {1035, 2712}, {1491, 2707}, {   0,    0}, 
{   0,    0}, {1810,  696}, {1810,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1035, 2701}, {1035, 2702}, {1035, 2703}, 
{1035, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1810,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1561,   61}, {   0,    0}, {   0,    0}, {2020,  860}, 
{   0,    0}, {2020,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2020, 2530}, {2020,  660}, {2020,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2020,  662}, {   0,    0}, {   0,    0}, {2020,  663}, {2020,  664}, 
{1491, 2711}, {   0,    0}, {1491, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1561,   62}, {   0,    0}, {2020,  861}, 
{   0,    0}, {   0,    0}, {1491, 2701}, {1491, 2702}, {1491, 2703}, 
{1491, 2704}, {2020,  666}, {2020,  667}, {   0,    0}, {   0,    0}, 
{2020,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1489,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1489, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2020,  670}, {1271,   57}, {2020,  862}, {1271, 2699}, {1271, 2697}, 
{1271, 2698}, {1271, 2700}, {1271, 2706}, {2020,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1489,  818}, {   0,    0}, {1271,   60}, 
{   0,    0}, {   0,    0}, {2020,  673}, {2020,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1489, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2020,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  853}, {   0,    0}, {1973,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1973,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2020,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2020,  863}, {2020,  678}, {2020,  864}, 
{   0,    0}, {1487,  817}, {   0,    0}, {1487, 2699}, {1487, 2697}, 
{1487, 2698}, {1487, 2700}, {1487, 2706}, {1487, 2705}, {2020,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1973,  855}, 
{   0,    0}, {1973,  647}, {   0,    0}, {   0,    0}, {1487,  183}, 
{1973,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2020,  681}, 
{2020,  865}, {   0,    0}, {1973,  649}, {   0,    0}, {   0,    0}, 
{2020,  866}, {1489, 2710}, {   0,    0}, {   0,    0}, {1973, 2485}, 
{2020, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  856}, {   0,    0}, {2020,  685}, {   0,    0}, 
{   0,    0}, {1973, 2489}, {   0,    0}, {2020,  686}, {2020,  687}, 
{   0,    0}, {   0,    0}, {2020,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  868}, {   0,    0}, {2020,  690}, {2020,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2020,  870}, {1973,  857}, 
{2020,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  652}, {2020,  694}, {   0,    0}, {1973,  858}, 
{   0,    0}, {1973,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1973,  655}, {   0,    0}, 
{1561, 2711}, {2020,  695}, {1561, 2712}, {1489, 2707}, {   0,    0}, 
{   0,    0}, {2020,  696}, {2020,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1561, 2701}, {1561, 2702}, {1561, 2703}, 
{1561, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2020,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1271,   61}, {   0,    0}, {   0,    0}, {1973,  860}, 
{   0,    0}, {1973,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1973, 2530}, {1973,  660}, {1973,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1973,  662}, {   0,    0}, {   0,    0}, {1973,  663}, {1973,  664}, 
{1489, 2711}, {   0,    0}, {1489, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1271,   62}, {   0,    0}, {1973,  861}, 
{   0,    0}, {   0,    0}, {1489, 2701}, {1489, 2702}, {1489, 2703}, 
{1489, 2704}, {1973,  666}, {1973,  667}, {   0,    0}, {   0,    0}, 
{1973,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1487,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1487, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1973,  670}, {1595,   57}, {1973,  862}, {1595, 2699}, {1595, 2697}, 
{1595, 2698}, {1595, 2700}, {1595, 2706}, {1973,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1487,  818}, {   0,    0}, {1595,   60}, 
{   0,    0}, {   0,    0}, {1973,  673}, {1973,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1487, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1973,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  853}, {   0,    0}, {2118,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2118,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1973,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1973,  863}, {1973,  678}, {1973,  864}, 
{   0,    0}, { 858,  817}, {   0,    0}, { 858, 2699}, { 858, 2697}, 
{ 858, 2698}, { 858, 2700}, { 858, 2706}, { 858, 2705}, {1973,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2118,  855}, 
{   0,    0}, {2118,  647}, {   0,    0}, {   0,    0}, { 858,  183}, 
{2118,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1973,  681}, 
{1973,  865}, {   0,    0}, {2118,  649}, {   0,    0}, {   0,    0}, 
{1973,  866}, {1487, 2710}, {   0,    0}, {   0,    0}, {2118, 2485}, 
{1973, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  856}, {   0,    0}, {1973,  685}, {   0,    0}, 
{   0,    0}, {2118, 2489}, {   0,    0}, {1973,  686}, {1973,  687}, 
{   0,    0}, {   0,    0}, {1973,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  868}, {   0,    0}, {1973,  690}, {1973,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1973,  870}, {2118,  857}, 
{1973,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  652}, {1973,  694}, {   0,    0}, {2118,  858}, 
{   0,    0}, {2118,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2118,  655}, {   0,    0}, 
{1271, 2711}, {1973,  695}, {1271, 2712}, {1487, 2707}, {   0,    0}, 
{   0,    0}, {1973,  696}, {1973,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1271, 2701}, {1271, 2702}, {1271, 2703}, 
{1271, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1973,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1595,   61}, {   0,    0}, {   0,    0}, {2118,  860}, 
{   0,    0}, {2118,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2118, 2530}, {2118,  660}, {2118,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2118,  662}, {   0,    0}, {   0,    0}, {2118,  663}, {2118,  664}, 
{1487, 2711}, {   0,    0}, {1487, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1595,   62}, {   0,    0}, {2118,  861}, 
{   0,    0}, {   0,    0}, {1487, 2701}, {1487, 2702}, {1487, 2703}, 
{1487, 2704}, {2118,  666}, {2118,  667}, {   0,    0}, {   0,    0}, 
{2118,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 858,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 858, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2118,  670}, { 671,   57}, {2118,  862}, { 671, 2699}, { 671, 2697}, 
{ 671, 2698}, { 671, 2700}, { 671, 2706}, {2118,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 858,  818}, {   0,    0}, { 671,   60}, 
{   0,    0}, {   0,    0}, {2118,  673}, {2118,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 858, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2118,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  853}, {   0,    0}, { 997,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 997,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2118,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2118,  863}, {2118,  678}, {2118,  864}, 
{   0,    0}, {1722,  817}, {   0,    0}, {1722, 2699}, {1722, 2697}, 
{1722, 2698}, {1722, 2700}, {1722, 2706}, {1722, 2705}, {2118,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 997,  855}, 
{   0,    0}, { 997,  647}, {   0,    0}, {   0,    0}, {1722,  183}, 
{ 997,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2118,  681}, 
{2118,  865}, {   0,    0}, { 997,  649}, {   0,    0}, {   0,    0}, 
{2118,  866}, { 858, 2710}, {   0,    0}, {   0,    0}, { 997, 2485}, 
{2118, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  856}, {   0,    0}, {2118,  685}, {   0,    0}, 
{   0,    0}, { 997, 2489}, {   0,    0}, {2118,  686}, {2118,  687}, 
{   0,    0}, {   0,    0}, {2118,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  868}, {   0,    0}, {2118,  690}, {2118,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2118,  870}, { 997,  857}, 
{2118,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  652}, {2118,  694}, {   0,    0}, { 997,  858}, 
{   0,    0}, { 997,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 997,  655}, {   0,    0}, 
{1595, 2711}, {2118,  695}, {1595, 2712}, { 858, 2707}, {   0,    0}, 
{   0,    0}, {2118,  696}, {2118,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1595, 2701}, {1595, 2702}, {1595, 2703}, 
{1595, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2118,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 671,   61}, {   0,    0}, {   0,    0}, { 997,  860}, 
{   0,    0}, { 997,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 997, 2530}, { 997,  660}, { 997,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 997,  662}, {   0,    0}, {   0,    0}, { 997,  663}, { 997,  664}, 
{ 858, 2711}, {   0,    0}, { 858, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 671,   62}, {   0,    0}, { 997,  861}, 
{   0,    0}, {   0,    0}, { 858, 2701}, { 858, 2702}, { 858, 2703}, 
{ 858, 2704}, { 997,  666}, { 997,  667}, {   0,    0}, {   0,    0}, 
{ 997,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1722,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1722, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 997,  670}, {  11,   57}, { 997,  862}, {  11, 2699}, {  11, 2697}, 
{  11, 2698}, {  11, 2700}, {  11, 2706}, { 997,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1722,  818}, {   0,    0}, {  11,   60}, 
{   0,    0}, {   0,    0}, { 997,  673}, { 997,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1722, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 997,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2056,  853}, {   0,    0}, {2056,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2056,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 997,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 997,  863}, { 997,  678}, { 997,  864}, 
{   0,    0}, {2202,  817}, {   0,    0}, {2202, 2699}, {2202, 2697}, 
{2202, 2698}, {2202, 2700}, {2202, 2706}, {2202, 2705}, { 997,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2056,  855}, 
{   0,    0}, {2056,  647}, {   0,    0}, {   0,    0}, {2202,  183}, 
{2056,  648}, {   0,    0}, {   0,    0}, {   0,    0}, { 997,  681}, 
{ 997,  865}, {   0,    0}, {2056,  649}, {   0,    0}, {   0,    0}, 
{ 997,  866}, {1722, 2710}, {   0,    0}, {   0,    0}, {2056, 2485}, 
{ 997, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2056,  856}, {   0,    0}, { 997,  685}, {   0,    0}, 
{   0,    0}, {2056, 2489}, {   0,    0}, { 997,  686}, { 997,  687}, 
{   0,    0}, {   0,    0}, { 997,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  868}, {   0,    0}, { 997,  690}, { 997,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 997,  870}, {2056,  857}, 
{ 997,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2056,  652}, { 997,  694}, {   0,    0}, {2056,  858}, 
{   0,    0}, {2056,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2056,  655}, {   0,    0}, 
{ 671, 2711}, { 997,  695}, { 671, 2712}, {1722, 2707}, {   0,    0}, 
{   0,    0}, { 997,  696}, { 997,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 671, 2701}, { 671, 2702}, { 671, 2703}, 
{ 671, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 997,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  11,   61}, {   0,    0}, {   0,    0}, {2056,  860}, 
{   0,    0}, {2056,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2056, 2530}, {2056,  660}, {2056,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2056,  662}, {   0,    0}, {   0,    0}, {2056,  663}, {2056,  664}, 
{1722, 2711}, {   0,    0}, {1722, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  11,   62}, {   0,    0}, {2056,  861}, 
{   0,    0}, {   0,    0}, {1722, 2701}, {1722, 2702}, {1722, 2703}, 
{1722, 2704}, {2056,  666}, {2056,  667}, {   0,    0}, {   0,    0}, 
{2056,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2202,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2202, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2056,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2056,  670}, { 832,   57}, {2056,  862}, { 832, 2699}, { 832, 2697}, 
{ 832, 2698}, { 832, 2700}, { 832, 2706}, {2056,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2202,  818}, {   0,    0}, { 832,   60}, 
{   0,    0}, {   0,    0}, {2056,  673}, {2056,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2202, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2056,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2123,  853}, {   0,    0}, {2123,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2123,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2056,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2056,  863}, {2056,  678}, {2056,  864}, 
{   0,    0}, {1733,  817}, {   0,    0}, {1733, 2699}, {1733, 2697}, 
{1733, 2698}, {1733, 2700}, {1733, 2706}, {1733, 2705}, {2056,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2123,  855}, 
{   0,    0}, {2123,  647}, {   0,    0}, {   0,    0}, {1733,  183}, 
{2123,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2056,  681}, 
{2056,  865}, {   0,    0}, {2123,  649}, {   0,    0}, {   0,    0}, 
{2056,  866}, {2202, 2710}, {   0,    0}, {   0,    0}, {2123, 2485}, 
{2056, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2056,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2123,  856}, {   0,    0}, {2056,  685}, {   0,    0}, 
{   0,    0}, {2123, 2489}, {   0,    0}, {2056,  686}, {2056,  687}, 
{   0,    0}, {   0,    0}, {2056,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2056,  868}, {   0,    0}, {2056,  690}, {2056,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2056,  870}, {2123,  857}, 
{2056,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2123,  652}, {2056,  694}, {   0,    0}, {2123,  858}, 
{   0,    0}, {2123,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2123,  655}, {   0,    0}, 
{  11, 2711}, {2056,  695}, {  11, 2712}, {2202, 2707}, {   0,    0}, 
{   0,    0}, {2056,  696}, {2056,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  11, 2701}, {  11, 2702}, {  11, 2703}, 
{  11, 2704}, {   0,    0}, {1568,   57}, {   0,    0}, {1568, 2699}, 
{1568, 2697}, {1568, 2698}, {1568, 2700}, {1568, 2706}, {   0,    0}, 
{   0,    0}, {2056,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 832,   61}, {   0,    0}, {   0,    0}, {2123,  860}, 
{1568,   60}, {2123,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2123, 2530}, {2123,  660}, {2123,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2123,  662}, {   0,    0}, {   0,    0}, {2123,  663}, {2123,  664}, 
{2202, 2711}, {   0,    0}, {2202, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 832,   62}, {   0,    0}, {2123,  861}, 
{   0,    0}, {   0,    0}, {2202, 2701}, {2202, 2702}, {2202, 2703}, 
{2202, 2704}, {2123,  666}, {2123,  667}, {   0,    0}, {   0,    0}, 
{2123,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1733,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1733, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2123,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2123,  670}, {1305,   57}, {2123,  862}, {1305, 2699}, {1305, 2697}, 
{1305, 2698}, {1305, 2700}, {1305, 2706}, {2123,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1733,  818}, {   0,    0}, {1305,   60}, 
{   0,    0}, {   0,    0}, {2123,  673}, {2123,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1733, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2123,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1461,  853}, {   0,    0}, {1461,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1461,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2123,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2123,  863}, {2123,  678}, {2123,  864}, 
{   0,    0}, {1913,  817}, {   0,    0}, {1913, 2699}, {1913, 2697}, 
{1913, 2698}, {1913, 2700}, {1913, 2706}, {1913, 2705}, {2123,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1461,  855}, 
{   0,    0}, {1461,  647}, {   0,    0}, {   0,    0}, {1913,  183}, 
{1461,  648}, {   0,    0}, {1568,   61}, {   0,    0}, {2123,  681}, 
{2123,  865}, {   0,    0}, {1461,  649}, {   0,    0}, {   0,    0}, 
{2123,  866}, {1733, 2710}, {   0,    0}, {   0,    0}, {1461, 2485}, 
{2123, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2123,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1461,  856}, {   0,    0}, {2123,  685}, {   0,    0}, 
{   0,    0}, {1461, 2489}, {   0,    0}, {2123,  686}, {2123,  687}, 
{   0,    0}, {   0,    0}, {2123,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1568,   62}, {   0,    0}, 
{   0,    0}, {2123,  868}, {   0,    0}, {2123,  690}, {2123,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2123,  870}, {1461,  857}, 
{2123,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1461,  652}, {2123,  694}, {   0,    0}, {1461,  858}, 
{   0,    0}, {1461,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1461,  655}, {   0,    0}, 
{ 832, 2711}, {2123,  695}, { 832, 2712}, {1733, 2707}, {   0,    0}, 
{   0,    0}, {2123,  696}, {2123,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 832, 2701}, { 832, 2702}, { 832, 2703}, 
{ 832, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2123,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1305,   61}, {   0,    0}, {   0,    0}, {1461,  860}, 
{   0,    0}, {1461,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1461, 2530}, {1461,  660}, {1461,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1461,  662}, {   0,    0}, {   0,    0}, {1461,  663}, {1461,  664}, 
{1733, 2711}, {   0,    0}, {1733, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1305,   62}, {   0,    0}, {1461,  861}, 
{   0,    0}, {   0,    0}, {1733, 2701}, {1733, 2702}, {1733, 2703}, 
{1733, 2704}, {1461,  666}, {1461,  667}, {   0,    0}, {   0,    0}, 
{1461,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1913,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1913, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1461,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1461,  670}, {1303,   57}, {1461,  862}, {1303, 2699}, {1303, 2697}, 
{1303, 2698}, {1303, 2700}, {1303, 2706}, {1461,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1913,  818}, {   0,    0}, {1303,   60}, 
{   0,    0}, {   0,    0}, {1461,  673}, {1461,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1913, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1461,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  853}, {   0,    0}, {1731,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1731,  645}, {   0,    0}, 
{   0,    0}, {1568, 2711}, {1461,  676}, {1568, 2712}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1461,  863}, {1461,  678}, {1461,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1568, 2701}, {1568, 2702}, 
{1568, 2703}, {1568, 2704}, {   0,    0}, {   0,    0}, {1461,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1731,  855}, 
{   0,    0}, {1731,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1731,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1461,  681}, 
{1461,  865}, {   0,    0}, {1731,  649}, {   0,    0}, {   0,    0}, 
{1461,  866}, {1913, 2710}, {   0,    0}, {   0,    0}, {1731, 2485}, 
{1461, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1461,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  856}, {   0,    0}, {1461,  685}, {   0,    0}, 
{   0,    0}, {1731, 2489}, {   0,    0}, {1461,  686}, {1461,  687}, 
{   0,    0}, {   0,    0}, {1461,  688}, {   0,    0}, { 936,   57}, 
{   0,    0}, { 936, 2699}, { 936, 2697}, { 936, 2698}, { 936, 2700}, 
{ 936, 2706}, {1461,  868}, {   0,    0}, {1461,  690}, {1461,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1461,  870}, {1731,  857}, 
{1461,  693}, {   0,    0}, { 936,   60}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  652}, {1461,  694}, {   0,    0}, {1731,  858}, 
{   0,    0}, {1731,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1731,  655}, {   0,    0}, 
{1305, 2711}, {1461,  695}, {1305, 2712}, {1913, 2707}, {   0,    0}, 
{   0,    0}, {1461,  696}, {1461,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1305, 2701}, {1305, 2702}, {1305, 2703}, 
{1305, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1461,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1303,   61}, {   0,    0}, {   0,    0}, {1731,  860}, 
{   0,    0}, {1731,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1731, 2530}, {1731,  660}, {1731,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1731,  662}, {   0,    0}, {   0,    0}, {1731,  663}, {1731,  664}, 
{1913, 2711}, {   0,    0}, {1913, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1303,   62}, {   0,    0}, {1731,  861}, 
{   0,    0}, {   0,    0}, {1913, 2701}, {1913, 2702}, {1913, 2703}, 
{1913, 2704}, {1731,  666}, {1731,  667}, {   0,    0}, {   0,    0}, 
{1731,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1998, 2089}, {   0,    0}, {1998, 2699}, {1998, 2697}, 
{1998, 2698}, {1998, 2700}, {1998, 2706}, {1998, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1998,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1731,  670}, {1572,   57}, {1731,  862}, {1572, 2699}, {1572, 2697}, 
{1572, 2698}, {1572, 2700}, {1572, 2706}, {1731,  672}, {   0,    0}, 
{ 836, 2693}, {   0,    0}, { 836, 2699}, { 836, 2697}, { 836, 2698}, 
{ 836, 2700}, { 836, 2706}, { 836, 2705}, {   0,    0}, {1572,   60}, 
{   0,    0}, {   0,    0}, {1731,  673}, {1731,  674}, {   0,    0}, 
{ 836, 1049}, {   0,    0}, {   0,    0}, { 836,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 936,   61}, 
{   0,    0}, {   0,    0}, {1731,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1728,  853}, {   0,    0}, {1728,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1728,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1731,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1731,  863}, {1731,  678}, {1731,  864}, 
{   0,    0}, {1110,  817}, {   0,    0}, {1110, 2699}, {1110, 2697}, 
{1110, 2698}, {1110, 2700}, {1110, 2706}, {1110, 2705}, {1731,  680}, 
{ 936,  133}, {   0,    0}, {   0,    0}, {   0,    0}, {1728,  855}, 
{   0,    0}, {1728,  647}, {   0,    0}, {   0,    0}, {1110,  183}, 
{1728,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1731,  681}, 
{1731,  865}, {   0,    0}, {1728,  649}, {   0,    0}, {   0,    0}, 
{1731,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {1728, 2485}, 
{1731, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1728,  856}, {   0,    0}, {1731,  685}, {   0,    0}, 
{   0,    0}, {1728, 2489}, {   0,    0}, {1731,  686}, {1731,  687}, 
{   0,    0}, {   0,    0}, {1731,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  868}, {   0,    0}, {1731,  690}, {1731,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1731,  870}, {1728,  857}, 
{1731,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1728,  652}, {1731,  694}, {   0,    0}, {1728,  858}, 
{   0,    0}, {1728,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1728,  655}, {   0,    0}, 
{1303, 2711}, {1731,  695}, {1303, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  696}, {1731,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1998, 2708}, {1303, 2701}, {1303, 2702}, {1303, 2703}, 
{1303, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1731,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1572,   61}, {   0,    0}, {   0,    0}, {1728,  860}, 
{   0,    0}, {1728,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1728, 2530}, {1728,  660}, {1728,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 836, 2708}, {   0,    0}, {   0,    0}, {1998, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1728,  662}, {   0,    0}, {   0,    0}, {1728,  663}, {1728,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1572,   62}, {2206, 2693}, {1728,  861}, 
{2206, 2699}, {2206, 2697}, {2206, 2698}, {2206, 2700}, {2206, 2706}, 
{2206, 2705}, {1728,  666}, {1728,  667}, {   0,    0}, {   0,    0}, 
{1728,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2206,  183}, { 836, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1110,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1110, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1728,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1728,  670}, {1998, 2710}, {1728,  862}, { 936, 2711}, {   0,    0}, 
{ 936, 2712}, {   0,    0}, {   0,    0}, {1728,  672}, { 936,  134}, 
{   0,    0}, { 936,  135}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 936, 2701}, { 936, 2702}, { 936, 2703}, { 936, 2704}, { 936,  136}, 
{   0,    0}, {   0,    0}, {1728,  673}, {1728,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1110, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1728,  675}, {   0,    0}, {   0,    0}, 
{ 836, 2710}, {1902,  853}, {   0,    0}, {1902,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1902,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1728,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1728,  863}, {1728,  678}, {1728,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1998, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1998, 2258}, {   0,    0}, {1728,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1902,  855}, 
{   0,    0}, {1902,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1902,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1728,  681}, 
{1728,  865}, {   0,    0}, {1902,  649}, {   0,    0}, {   0,    0}, 
{1728,  866}, {1110, 2710}, {   0,    0}, {   0,    0}, {1902, 2485}, 
{1728, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1728,  867}, { 836, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1902,  856}, {   0,    0}, {1728,  685}, {   0,    0}, 
{   0,    0}, {1902, 2489}, {   0,    0}, {1728,  686}, {1728,  687}, 
{   0,    0}, {   0,    0}, {1728,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1998, 2711}, {1728,  868}, {1998, 2712}, {1728,  690}, {1728,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1728,  870}, {1902,  857}, 
{1728,  693}, {   0,    0}, {1998, 2701}, {1998, 2702}, {1998, 2703}, 
{1998, 2704}, {1902,  652}, {1728,  694}, {   0,    0}, {1902,  858}, 
{   0,    0}, {1902,  859}, {   0,    0}, {2206, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1902,  655}, {   0,    0}, 
{1572, 2711}, {1728,  695}, {1572, 2712}, {1110, 2707}, {   0,    0}, 
{   0,    0}, {1728,  696}, {1728,  871}, {   0,    0}, { 836, 2711}, 
{   0,    0}, { 836, 2712}, {1572, 2701}, {1572, 2702}, {1572, 2703}, 
{1572, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 836, 2701}, { 836, 2702}, { 836, 2703}, { 836, 2704}, 
{   0,    0}, {1728,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1902,  860}, 
{2206, 2709}, {1902,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1902, 2530}, {1902,  660}, {1902,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1902,  662}, {   0,    0}, {   0,    0}, {1902,  663}, {1902,  664}, 
{1110, 2711}, {   0,    0}, {1110, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1902,  861}, 
{   0,    0}, {   0,    0}, {1110, 2701}, {1110, 2702}, {1110, 2703}, 
{1110, 2704}, {1902,  666}, {1902,  667}, {   0,    0}, {   0,    0}, 
{1902,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1723, 2693}, {   0,    0}, {1723, 2699}, {1723, 2697}, 
{1723, 2698}, {1723, 2700}, {1723, 2706}, {1723, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2206, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1723,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1902,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1902,  670}, {1585,   57}, {1902,  862}, {1585, 2699}, {1585, 2697}, 
{1585, 2698}, {1585, 2700}, {1585, 2706}, {1902,  672}, {   0,    0}, 
{2214, 2693}, {   0,    0}, {2214, 2699}, {2214, 2697}, {2214, 2698}, 
{2214, 2700}, {2214, 2706}, {2214, 2705}, {   0,    0}, {1585,   60}, 
{   0,    0}, {   0,    0}, {1902,  673}, {1902,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2214,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1902,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1901,  853}, {   0,    0}, {1901,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2206, 2707}, {   0,    0}, {   0,    0}, {1901,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1902,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1902,  863}, {1902,  678}, {1902,  864}, 
{   0,    0}, { 779, 2693}, {   0,    0}, { 779, 2699}, { 779, 2697}, 
{ 779, 2698}, { 779, 2700}, { 779, 2706}, { 779, 2705}, {1902,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1901,  855}, 
{   0,    0}, {1901,  647}, {   0,    0}, {   0,    0}, { 779,  183}, 
{1901,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1902,  681}, 
{1902,  865}, {   0,    0}, {1901,  649}, {   0,    0}, {   0,    0}, 
{1902,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {1901, 2485}, 
{1902, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1902,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1901,  856}, {2206, 2711}, {1902,  685}, {2206, 2712}, 
{   0,    0}, {1901, 2489}, {   0,    0}, {1902,  686}, {1902,  687}, 
{   0,    0}, {   0,    0}, {1902,  688}, {   0,    0}, {2206, 2701}, 
{2206, 2702}, {2206, 2703}, {2206, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1902,  868}, {   0,    0}, {1902,  690}, {1902,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1902,  870}, {1901,  857}, 
{1902,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1901,  652}, {1902,  694}, {   0,    0}, {1901,  858}, 
{   0,    0}, {1901,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1901,  655}, {   0,    0}, 
{   0,    0}, {1902,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1902,  696}, {1902,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1723, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1902,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1585,   61}, {   0,    0}, {   0,    0}, {1901,  860}, 
{   0,    0}, {1901,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1901, 2530}, {1901,  660}, {1901,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2214, 2708}, {   0,    0}, {   0,    0}, {1723, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1901,  662}, {   0,    0}, {   0,    0}, {1901,  663}, {1901,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1585,   62}, {1527, 2693}, {1901,  861}, 
{1527, 2699}, {1527, 2697}, {1527, 2698}, {1527, 2700}, {1527, 2706}, 
{1527, 2705}, {1901,  666}, {1901,  667}, {   0,    0}, {   0,    0}, 
{1901,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1527,  183}, {2214, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 779, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1901,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1901,  670}, {1723, 2710}, {1901,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1901,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1901,  673}, {1901,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 779, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1901,  675}, {   0,    0}, {   0,    0}, 
{2214, 2710}, {2062,  853}, {   0,    0}, {2062,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2062,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1901,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1901,  863}, {1901,  678}, {1901,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1723, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1901,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2062,  855}, 
{   0,    0}, {2062,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2062,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1901,  681}, 
{1901,  865}, {   0,    0}, {2062,  649}, {   0,    0}, {   0,    0}, 
{1901,  866}, { 779, 2710}, {   0,    0}, {   0,    0}, {2062, 2485}, 
{1901, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1901,  867}, {2214, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2062,  856}, {   0,    0}, {1901,  685}, {   0,    0}, 
{   0,    0}, {2062, 2489}, {   0,    0}, {1901,  686}, {1901,  687}, 
{   0,    0}, {   0,    0}, {1901,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1723, 2711}, {1901,  868}, {1723, 2712}, {1901,  690}, {1901,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1901,  870}, {2062,  857}, 
{1901,  693}, {   0,    0}, {1723, 2701}, {1723, 2702}, {1723, 2703}, 
{1723, 2704}, {2062,  652}, {1901,  694}, {   0,    0}, {2062,  858}, 
{   0,    0}, {2062,  859}, {   0,    0}, {1527, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2062,  655}, {   0,    0}, 
{1585, 2711}, {1901,  695}, {1585, 2712}, { 779, 2707}, {   0,    0}, 
{   0,    0}, {1901,  696}, {1901,  871}, {   0,    0}, {2214, 2711}, 
{   0,    0}, {2214, 2712}, {1585, 2701}, {1585, 2702}, {1585, 2703}, 
{1585, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2214, 2701}, {2214, 2702}, {2214, 2703}, {2214, 2704}, 
{   0,    0}, {1901,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2062,  860}, 
{1527, 2709}, {2062,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2062, 2530}, {2062,  660}, {2062,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2062,  662}, {   0,    0}, {   0,    0}, {2062,  663}, {2062,  664}, 
{ 779, 2711}, {   0,    0}, { 779, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2062,  861}, 
{   0,    0}, {   0,    0}, { 779, 2701}, { 779, 2702}, { 779, 2703}, 
{ 779, 2704}, {2062,  666}, {2062,  667}, {   0,    0}, {   0,    0}, 
{2062,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 491, 2693}, {   0,    0}, { 491, 2699}, { 491, 2697}, 
{ 491, 2698}, { 491, 2700}, { 491, 2706}, { 491, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1527, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 491,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2062,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2062,  670}, { 859,   57}, {2062,  862}, { 859, 2699}, { 859, 2697}, 
{ 859, 2698}, { 859, 2700}, { 859, 2706}, {2062,  672}, {   0,    0}, 
{ 305, 2693}, {   0,    0}, { 305, 2699}, { 305, 2697}, { 305, 2698}, 
{ 305, 2700}, { 305, 2706}, { 305, 2705}, {   0,    0}, { 859,   60}, 
{   0,    0}, {   0,    0}, {2062,  673}, {2062,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 305,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2062,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 874,  853}, {   0,    0}, { 874,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1527, 2707}, {   0,    0}, {   0,    0}, { 874,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2062,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2062,  863}, {2062,  678}, {2062,  864}, 
{   0,    0}, {1905, 2693}, {   0,    0}, {1905, 2699}, {1905, 2697}, 
{1905, 2698}, {1905, 2700}, {1905, 2706}, {1905, 2705}, {2062,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 874,  855}, 
{   0,    0}, { 874,  647}, {   0,    0}, {   0,    0}, {1905,  183}, 
{ 874,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2062,  681}, 
{2062,  865}, {   0,    0}, { 874,  649}, {   0,    0}, {   0,    0}, 
{2062,  866}, {   0,    0}, {   0,    0}, {   0,    0}, { 874, 2485}, 
{2062, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2062,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 874,  856}, {1527, 2711}, {2062,  685}, {1527, 2712}, 
{   0,    0}, { 874, 2489}, {   0,    0}, {2062,  686}, {2062,  687}, 
{   0,    0}, {   0,    0}, {2062,  688}, {   0,    0}, {1527, 2701}, 
{1527, 2702}, {1527, 2703}, {1527, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2062,  868}, {   0,    0}, {2062,  690}, {2062,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2062,  870}, { 874,  857}, 
{2062,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 874,  652}, {2062,  694}, {   0,    0}, { 874,  858}, 
{   0,    0}, { 874,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 874,  655}, {   0,    0}, 
{   0,    0}, {2062,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2062,  696}, {2062,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 491, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2062,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 859,   61}, {   0,    0}, {   0,    0}, { 874,  860}, 
{   0,    0}, { 874,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 874, 2530}, { 874,  660}, { 874,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 305, 2708}, {   0,    0}, {   0,    0}, { 491, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 874,  662}, {   0,    0}, {   0,    0}, { 874,  663}, { 874,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 859,   62}, {1528, 2693}, { 874,  861}, 
{1528, 2699}, {1528, 2697}, {1528, 2698}, {1528, 2700}, {1528, 2706}, 
{1528, 2705}, { 874,  666}, { 874,  667}, {   0,    0}, {   0,    0}, 
{ 874,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1528,  183}, { 305, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1905, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 874,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 874,  670}, { 491, 2710}, { 874,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 874,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 874,  673}, { 874,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1905, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 874,  675}, {   0,    0}, {   0,    0}, 
{ 305, 2710}, {2141,  853}, {   0,    0}, {2141,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2141,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 874,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 874,  863}, { 874,  678}, { 874,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 491, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 874,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2141,  855}, 
{   0,    0}, {2141,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2141,  648}, {   0,    0}, {   0,    0}, {   0,    0}, { 874,  681}, 
{ 874,  865}, {   0,    0}, {2141,  649}, {   0,    0}, {   0,    0}, 
{ 874,  866}, {1905, 2710}, {   0,    0}, {   0,    0}, {2141, 2485}, 
{ 874, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 874,  867}, { 305, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2141,  856}, {   0,    0}, { 874,  685}, {   0,    0}, 
{   0,    0}, {2141, 2489}, {   0,    0}, { 874,  686}, { 874,  687}, 
{   0,    0}, {   0,    0}, { 874,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 491, 2711}, { 874,  868}, { 491, 2712}, { 874,  690}, { 874,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 874,  870}, {2141,  857}, 
{ 874,  693}, {   0,    0}, { 491, 2701}, { 491, 2702}, { 491, 2703}, 
{ 491, 2704}, {2141,  652}, { 874,  694}, {   0,    0}, {2141,  858}, 
{   0,    0}, {2141,  859}, {   0,    0}, {1528, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2141,  655}, {   0,    0}, 
{ 859, 2711}, { 874,  695}, { 859, 2712}, {1905, 2707}, {   0,    0}, 
{   0,    0}, { 874,  696}, { 874,  871}, {   0,    0}, { 305, 2711}, 
{   0,    0}, { 305, 2712}, { 859, 2701}, { 859, 2702}, { 859, 2703}, 
{ 859, 2704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 305, 2701}, { 305, 2702}, { 305, 2703}, { 305, 2704}, 
{   0,    0}, { 874,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2141,  860}, 
{1528, 2709}, {2141,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2141, 2530}, {2141,  660}, {2141,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2141,  662}, {   0,    0}, {   0,    0}, {2141,  663}, {2141,  664}, 
{1905, 2711}, {   0,    0}, {1905, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2141,  861}, 
{   0,    0}, {   0,    0}, {1905, 2701}, {1905, 2702}, {1905, 2703}, 
{1905, 2704}, {2141,  666}, {2141,  667}, {   0,    0}, {   0,    0}, 
{2141,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 243, 2693}, {   0,    0}, { 243, 2699}, { 243, 2697}, 
{ 243, 2698}, { 243, 2700}, { 243, 2706}, { 243, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1528, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 243,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2141,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2141,  670}, {   0,    0}, {2141,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2141,  672}, {   0,    0}, 
{ 533, 2693}, {   0,    0}, { 533, 2699}, { 533, 2697}, { 533, 2698}, 
{ 533, 2700}, { 533, 2706}, { 533, 2705}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2141,  673}, {2141,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 533,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2141,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2148,  853}, {   0,    0}, {2148,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1528, 2707}, {   0,    0}, {   0,    0}, {2148,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2141,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2141,  863}, {2141,  678}, {2141,  864}, 
{   0,    0}, {1530, 2693}, {   0,    0}, {1530, 2699}, {1530, 2697}, 
{1530, 2698}, {1530, 2700}, {1530, 2706}, {1530, 2705}, {2141,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2148,  855}, 
{   0,    0}, {2148,  647}, {   0,    0}, {   0,    0}, {1530,  183}, 
{2148,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2141,  681}, 
{2141,  865}, {   0,    0}, {2148,  649}, {   0,    0}, {   0,    0}, 
{2141,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {2148, 2485}, 
{2141, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2141,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2148,  856}, {1528, 2711}, {2141,  685}, {1528, 2712}, 
{   0,    0}, {2148, 2489}, {   0,    0}, {2141,  686}, {2141,  687}, 
{   0,    0}, {   0,    0}, {2141,  688}, {   0,    0}, {1528, 2701}, 
{1528, 2702}, {1528, 2703}, {1528, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2141,  868}, {   0,    0}, {2141,  690}, {2141,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2141,  870}, {2148,  857}, 
{2141,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2148,  652}, {2141,  694}, {   0,    0}, {2148,  858}, 
{   0,    0}, {2148,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2148,  655}, {   0,    0}, 
{   0,    0}, {2141,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2141,  696}, {2141,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 243, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2141,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2148,  860}, 
{   0,    0}, {2148,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2148, 2530}, {2148,  660}, {2148,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 533, 2708}, {   0,    0}, {   0,    0}, { 243, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2148,  662}, {   0,    0}, {   0,    0}, {2148,  663}, {2148,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1919, 2693}, {2148,  861}, 
{1919, 2699}, {1919, 2697}, {1919, 2698}, {1919, 2700}, {1919, 2706}, 
{1919, 2705}, {2148,  666}, {2148,  667}, {   0,    0}, {   0,    0}, 
{2148,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1919,  183}, { 533, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1530, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2148,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2148,  670}, { 243, 2710}, {2148,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2148,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2148,  673}, {2148,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1530, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2148,  675}, {   0,    0}, {   0,    0}, 
{ 533, 2710}, {1307,  853}, {   0,    0}, {1307,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1307,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2148,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2148,  863}, {2148,  678}, {2148,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 243, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2148,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1307,  855}, 
{   0,    0}, {1307,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1307,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2148,  681}, 
{2148,  865}, {   0,    0}, {1307,  649}, {   0,    0}, {   0,    0}, 
{2148,  866}, {1530, 2710}, {   0,    0}, {   0,    0}, {1307, 2485}, 
{2148, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2148,  867}, { 533, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1307,  856}, {   0,    0}, {2148,  685}, {   0,    0}, 
{   0,    0}, {1307, 2489}, {   0,    0}, {2148,  686}, {2148,  687}, 
{   0,    0}, {   0,    0}, {2148,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 243, 2711}, {2148,  868}, { 243, 2712}, {2148,  690}, {2148,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2148,  870}, {1307,  857}, 
{2148,  693}, {   0,    0}, { 243, 2701}, { 243, 2702}, { 243, 2703}, 
{ 243, 2704}, {1307,  652}, {2148,  694}, {   0,    0}, {1307,  858}, 
{   0,    0}, {1307,  859}, {   0,    0}, {1919, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1307,  655}, {   0,    0}, 
{   0,    0}, {2148,  695}, {   0,    0}, {1530, 2707}, {   0,    0}, 
{   0,    0}, {2148,  696}, {2148,  871}, {   0,    0}, { 533, 2711}, 
{   0,    0}, { 533, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 533, 2701}, { 533, 2702}, { 533, 2703}, { 533, 2704}, 
{   0,    0}, {2148,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1307,  860}, 
{1919, 2709}, {1307,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1307, 2530}, {1307,  660}, {1307,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1307,  662}, {   0,    0}, {   0,    0}, {1307,  663}, {1307,  664}, 
{1530, 2711}, {   0,    0}, {1530, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1307,  861}, 
{   0,    0}, {   0,    0}, {1530, 2701}, {1530, 2702}, {1530, 2703}, 
{1530, 2704}, {1307,  666}, {1307,  667}, {   0,    0}, {   0,    0}, 
{1307,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2032, 2693}, {   0,    0}, {2032, 2699}, {2032, 2697}, 
{2032, 2698}, {2032, 2700}, {2032, 2706}, {2032, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1919, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2032,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1307,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1307,  670}, {   0,    0}, {1307,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1307,  672}, {   0,    0}, 
{ 770, 2693}, {   0,    0}, { 770, 2699}, { 770, 2697}, { 770, 2698}, 
{ 770, 2700}, { 770, 2706}, { 770, 2705}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1307,  673}, {1307,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 770,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1307,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2191,  853}, {   0,    0}, {2191,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1919, 2707}, {   0,    0}, {   0,    0}, {2191,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1307,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1307,  863}, {1307,  678}, {1307,  864}, 
{   0,    0}, {1287, 2693}, {   0,    0}, {1287, 2699}, {1287, 2697}, 
{1287, 2698}, {1287, 2700}, {1287, 2706}, {1287, 2705}, {1307,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2191,  855}, 
{   0,    0}, {2191,  647}, {   0,    0}, {   0,    0}, {1287,  183}, 
{2191,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1307,  681}, 
{1307,  865}, {   0,    0}, {2191,  649}, {   0,    0}, {   0,    0}, 
{1307,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {2191, 2485}, 
{1307, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1307,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2191,  856}, {1919, 2711}, {1307,  685}, {1919, 2712}, 
{   0,    0}, {2191, 2489}, {   0,    0}, {1307,  686}, {1307,  687}, 
{   0,    0}, {   0,    0}, {1307,  688}, {   0,    0}, {1919, 2701}, 
{1919, 2702}, {1919, 2703}, {1919, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1307,  868}, {   0,    0}, {1307,  690}, {1307,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1307,  870}, {2191,  857}, 
{1307,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2191,  652}, {1307,  694}, {   0,    0}, {2191,  858}, 
{   0,    0}, {2191,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2191,  655}, {   0,    0}, 
{   0,    0}, {1307,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1307,  696}, {1307,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2032, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1307,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2191,  860}, 
{   0,    0}, {2191,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2191, 2530}, {2191,  660}, {2191,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 770, 2708}, {   0,    0}, {   0,    0}, {2032, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2191,  662}, {   0,    0}, {   0,    0}, {2191,  663}, {2191,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 172, 2693}, {2191,  861}, 
{ 172, 2699}, { 172, 2697}, { 172, 2698}, { 172, 2700}, { 172, 2706}, 
{ 172, 2705}, {2191,  666}, {2191,  667}, {   0,    0}, {   0,    0}, 
{2191,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 172,  183}, { 770, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1287, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2191,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2191,  670}, {2032, 2710}, {2191,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2191,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2191,  673}, {2191,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1287, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2191,  675}, {   0,    0}, {   0,    0}, 
{ 770, 2710}, {1649,  853}, {   0,    0}, {1649,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1649,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2191,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2191,  863}, {2191,  678}, {2191,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2032, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2191,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1649,  855}, 
{   0,    0}, {1649,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1649,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2191,  681}, 
{2191,  865}, {   0,    0}, {1649,  649}, {   0,    0}, {   0,    0}, 
{2191,  866}, {1287, 2710}, {   0,    0}, {   0,    0}, {1649, 2485}, 
{2191, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2191,  867}, { 770, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1649,  856}, {   0,    0}, {2191,  685}, {   0,    0}, 
{   0,    0}, {1649, 2489}, {   0,    0}, {2191,  686}, {2191,  687}, 
{   0,    0}, {   0,    0}, {2191,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2032, 2711}, {2191,  868}, {2032, 2712}, {2191,  690}, {2191,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2191,  870}, {1649,  857}, 
{2191,  693}, {   0,    0}, {2032, 2701}, {2032, 2702}, {2032, 2703}, 
{2032, 2704}, {1649,  652}, {2191,  694}, {   0,    0}, {1649,  858}, 
{   0,    0}, {1649,  859}, {   0,    0}, { 172, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1649,  655}, {   0,    0}, 
{   0,    0}, {2191,  695}, {   0,    0}, {1287, 2707}, {   0,    0}, 
{   0,    0}, {2191,  696}, {2191,  871}, {   0,    0}, { 770, 2711}, 
{   0,    0}, { 770, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 770, 2701}, { 770, 2702}, { 770, 2703}, { 770, 2704}, 
{   0,    0}, {2191,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1649,  860}, 
{ 172, 2709}, {1649,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1649, 2530}, {1649,  660}, {1649,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1649,  662}, {   0,    0}, {   0,    0}, {1649,  663}, {1649,  664}, 
{1287, 2711}, {   0,    0}, {1287, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1649,  861}, 
{   0,    0}, {   0,    0}, {1287, 2701}, {1287, 2702}, {1287, 2703}, 
{1287, 2704}, {1649,  666}, {1649,  667}, {   0,    0}, {   0,    0}, 
{1649,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 306, 2693}, {   0,    0}, { 306, 2699}, { 306, 2697}, 
{ 306, 2698}, { 306, 2700}, { 306, 2706}, { 306, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 172, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 306,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1649,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1649,  670}, {   0,    0}, {1649,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1649,  672}, {   0,    0}, 
{ 769,  817}, {   0,    0}, { 769, 2699}, { 769, 2697}, { 769, 2698}, 
{ 769, 2700}, { 769, 2706}, { 769, 2705}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1649,  673}, {1649,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 769,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1649,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2164,  853}, {   0,    0}, {2164,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 172, 2707}, {   0,    0}, {   0,    0}, {2164,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1649,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1649,  863}, {1649,  678}, {1649,  864}, 
{   0,    0}, { 547, 2693}, {   0,    0}, { 547, 2699}, { 547, 2697}, 
{ 547, 2698}, { 547, 2700}, { 547, 2706}, { 547, 2705}, {1649,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2164,  855}, 
{   0,    0}, {2164,  647}, {   0,    0}, {   0,    0}, { 547,  183}, 
{2164,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1649,  681}, 
{1649,  865}, {   0,    0}, {2164,  649}, {   0,    0}, {   0,    0}, 
{1649,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {2164, 2485}, 
{1649, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1649,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2164,  856}, { 172, 2711}, {1649,  685}, { 172, 2712}, 
{   0,    0}, {2164, 2489}, {   0,    0}, {1649,  686}, {1649,  687}, 
{   0,    0}, {   0,    0}, {1649,  688}, {   0,    0}, { 172, 2701}, 
{ 172, 2702}, { 172, 2703}, { 172, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1649,  868}, {   0,    0}, {1649,  690}, {1649,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1649,  870}, {2164,  857}, 
{1649,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2164,  652}, {1649,  694}, {   0,    0}, {2164,  858}, 
{   0,    0}, {2164,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2164,  655}, {   0,    0}, 
{   0,    0}, {1649,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1649,  696}, {1649,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 306, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1649,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2164,  860}, 
{   0,    0}, {2164,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2164, 2530}, {2164,  660}, {2164,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 769, 2708}, {   0,    0}, {   0,    0}, { 306, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2164,  662}, {   0,    0}, {   0,    0}, {2164,  663}, {2164,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 985, 2693}, {2164,  861}, 
{ 985, 2699}, { 985, 2697}, { 985, 2698}, { 985, 2700}, { 985, 2706}, 
{ 985, 2705}, {2164,  666}, {2164,  667}, {   0,    0}, {   0,    0}, 
{2164,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 985,  183}, { 769, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 547, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2164,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2164,  670}, { 306, 2710}, {2164,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2164,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2164,  673}, {2164,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 547, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2164,  675}, {   0,    0}, {   0,    0}, 
{ 769, 2710}, {2165,  853}, {   0,    0}, {2165,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2165,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2164,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2164,  863}, {2164,  678}, {2164,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 306, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2164,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2165,  855}, 
{   0,    0}, {2165,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2165,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2164,  681}, 
{2164,  865}, {   0,    0}, {2165,  649}, {   0,    0}, {   0,    0}, 
{2164,  866}, { 547, 2710}, {   0,    0}, {   0,    0}, {2165, 2485}, 
{2164, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2164,  867}, { 769, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2165,  856}, {   0,    0}, {2164,  685}, {   0,    0}, 
{   0,    0}, {2165, 2489}, {   0,    0}, {2164,  686}, {2164,  687}, 
{   0,    0}, {   0,    0}, {2164,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 306, 2711}, {2164,  868}, { 306, 2712}, {2164,  690}, {2164,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2164,  870}, {2165,  857}, 
{2164,  693}, {   0,    0}, { 306, 2701}, { 306, 2702}, { 306, 2703}, 
{ 306, 2704}, {2165,  652}, {2164,  694}, {   0,    0}, {2165,  858}, 
{   0,    0}, {2165,  859}, {   0,    0}, { 985, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2165,  655}, {   0,    0}, 
{   0,    0}, {2164,  695}, {   0,    0}, { 547, 2707}, {   0,    0}, 
{   0,    0}, {2164,  696}, {2164,  871}, {   0,    0}, { 769, 2711}, 
{   0,    0}, { 769, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 769, 2701}, { 769, 2702}, { 769, 2703}, { 769, 2704}, 
{   0,    0}, {2164,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2165,  860}, 
{ 985, 2709}, {2165,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2165, 2530}, {2165,  660}, {2165,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2165,  662}, {   0,    0}, {   0,    0}, {2165,  663}, {2165,  664}, 
{ 547, 2711}, {   0,    0}, { 547, 2712}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2165,  861}, 
{   0,    0}, {   0,    0}, { 547, 2701}, { 547, 2702}, { 547, 2703}, 
{ 547, 2704}, {2165,  666}, {2165,  667}, {   0,    0}, {   0,    0}, 
{2165,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 171, 2693}, {   0,    0}, { 171, 2699}, { 171, 2697}, 
{ 171, 2698}, { 171, 2700}, { 171, 2706}, { 171, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 985, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 171,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2165,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2165,  670}, {   0,    0}, {2165,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2165,  672}, {1324,   57}, 
{   0,    0}, {1324, 2699}, {1324, 2697}, {1324, 2698}, {1324, 2700}, 
{1324, 2706}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2165,  673}, {2165,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1324,   60}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2165,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1706,  853}, {   0,    0}, {1706,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 985, 2707}, {   0,    0}, {   0,    0}, {1706,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2165,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2165,  863}, {2165,  678}, {2165,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2165,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1706,  855}, 
{   0,    0}, {1706,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1706,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {2165,  681}, 
{2165,  865}, {   0,    0}, {1706,  649}, {   0,    0}, {   0,    0}, 
{2165,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {1706, 2485}, 
{2165, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2165,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1706,  856}, { 985, 2711}, {2165,  685}, { 985, 2712}, 
{   0,    0}, {1706, 2489}, {   0,    0}, {2165,  686}, {2165,  687}, 
{   0,    0}, {   0,    0}, {2165,  688}, {   0,    0}, { 985, 2701}, 
{ 985, 2702}, { 985, 2703}, { 985, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2165,  868}, {   0,    0}, {2165,  690}, {2165,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2165,  870}, {1706,  857}, 
{2165,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1706,  652}, {2165,  694}, {   0,    0}, {1706,  858}, 
{   0,    0}, {1706,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1706,  655}, {   0,    0}, 
{   0,    0}, {2165,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2165,  696}, {2165,  871}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 171, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2165,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1706,  860}, 
{   0,    0}, {1706,  659}, {   0,    0}, {   0,    0}, {1324,   61}, 
{1706, 2530}, {1706,  660}, {1706,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 171, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1706,  662}, {   0,    0}, {   0,    0}, {1706,  663}, {1706,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1706,  861}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1324,  133}, {1706,  666}, {1706,  667}, {   0,    0}, {   0,    0}, 
{1706,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1706,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1706,  670}, { 171, 2710}, {1706,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1706,  672}, {1021,   57}, 
{   0,    0}, {1021, 2699}, {1021, 2697}, {1021, 2698}, {1021, 2700}, 
{1021, 2706}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1706,  673}, {1706,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1021,   60}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1706,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1227,  853}, {   0,    0}, {1227,  854}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1227,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1706,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1706,  863}, {1706,  678}, {1706,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 171, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1706,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1227,  855}, 
{   0,    0}, {1227,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1227,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1706,  681}, 
{1706,  865}, {   0,    0}, {1227,  649}, {   0,    0}, {   0,    0}, 
{1706,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {1227, 2485}, 
{1706, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1706,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1227,  856}, {   0,    0}, {1706,  685}, {   0,    0}, 
{   0,    0}, {1227, 2489}, {   0,    0}, {1706,  686}, {1706,  687}, 
{   0,    0}, {   0,    0}, {1706,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 171, 2711}, {1706,  868}, { 171, 2712}, {1706,  690}, {1706,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1706,  870}, {1227,  857}, 
{1706,  693}, {   0,    0}, { 171, 2701}, { 171, 2702}, { 171, 2703}, 
{ 171, 2704}, {1227,  652}, {1706,  694}, {   0,    0}, {1227,  858}, 
{   0,    0}, {1227,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1227,  655}, {   0,    0}, 
{   0,    0}, {1706,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1706,  696}, {1706,  871}, {1324, 2711}, {   0,    0}, 
{1324, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, {1324,  134}, 
{   0,    0}, {1324,  135}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1324, 2701}, {1324, 2702}, {1324, 2703}, {1324, 2704}, {1324,  136}, 
{   0,    0}, {1706,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1227,  860}, 
{   0,    0}, {1227,  659}, {   0,    0}, {   0,    0}, {1021,   61}, 
{1227, 2530}, {1227,  660}, {1227,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1227,  662}, {   0,    0}, {   0,    0}, {1227,  663}, {1227,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1227,  861}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1021,  133}, {1227,  666}, {1227,  667}, {   0,    0}, {   0,    0}, 
{1227,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 135,   57}, {   0,    0}, 
{ 135, 2699}, { 135, 2697}, { 135, 2698}, { 135, 2700}, { 135, 2706}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1227,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1227,  670}, { 135,   60}, {1227,  862}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1227,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1227,  673}, {1227,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1961,  817}, {   0,    0}, 
{1961, 2699}, {1961, 2697}, {1961, 2698}, {1961, 2700}, {1961, 2706}, 
{1961, 2705}, {   0,    0}, {1227,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1847,  853}, {   0,    0}, {1847,  854}, {   0,    0}, 
{   0,    0}, {1961,  183}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1847,  645}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1227,  676}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1227,  863}, {1227,  678}, {1227,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1227,  680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1847,  855}, 
{   0,    0}, {1847,  647}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1847,  648}, {   0,    0}, {   0,    0}, {   0,    0}, {1227,  681}, 
{1227,  865}, {   0,    0}, {1847,  649}, {   0,    0}, {   0,    0}, 
{1227,  866}, {   0,    0}, {   0,    0}, {   0,    0}, {1847, 2485}, 
{1227, 2581}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1227,  867}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1847,  856}, {   0,    0}, {1227,  685}, {   0,    0}, 
{   0,    0}, {1847, 2489}, {   0,    0}, {1227,  686}, {1227,  687}, 
{   0,    0}, {   0,    0}, {1227,  688}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1227,  868}, {   0,    0}, {1227,  690}, {1227,  869}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1227,  870}, {1847,  857}, 
{1227,  693}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1847,  652}, {1227,  694}, {   0,    0}, {1847,  858}, 
{   0,    0}, {1847,  859}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1847,  655}, {   0,    0}, 
{   0,    0}, {1227,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1227,  696}, {1227,  871}, {1021, 2711}, {   0,    0}, 
{1021, 2712}, {   0,    0}, {   0,    0}, { 135,   61}, {1021,  134}, 
{   0,    0}, {1021,  135}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1021, 2701}, {1021, 2702}, {1021, 2703}, {1021, 2704}, {1021,  136}, 
{   0,    0}, {1227,  872}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1847,  860}, 
{   0,    0}, {1847,  659}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1847, 2530}, {1847,  660}, {1847,  661}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 135,  133}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1961,   61}, {   0,    0}, 
{1847,  662}, {   0,    0}, {   0,    0}, {1847,  663}, {1847,  664}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1961, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1847,  861}, 
{2151, 2394}, {   0,    0}, {   0,    0}, {   0,    0}, {2151, 2378}, 
{   0,    0}, {1847,  666}, {1847,  667}, {   0,    0}, {   0,    0}, 
{1847,  668}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1961,  133}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2151, 2400}, 
{2151, 2401}, {   0,    0}, {2151, 2166}, {2151, 2383}, {   0,    0}, 
{1961, 2709}, {2151, 2382}, {   0,    0}, {2151, 2167}, {2151, 2384}, 
{   0,    0}, {1847,  669}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1847,  670}, { 209,   57}, {1847,  862}, { 209, 2699}, { 209, 2697}, 
{ 209, 2698}, { 209, 2700}, { 209, 2706}, {1847,  672}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2151, 2168}, 
{   0,    0}, {2151, 2169}, {   0,    0}, {   0,    0}, { 209,   60}, 
{   0,    0}, {   0,    0}, {1847,  673}, {1847,  674}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 208,   57}, 
{   0,    0}, { 208, 2699}, { 208, 2697}, { 208, 2698}, { 208, 2700}, 
{ 208, 2706}, {2151, 2170}, {1847,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 208,   60}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1847,  676}, {1961, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1847,  863}, {1847,  678}, {1847,  864}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2151, 2397}, {2152, 2394}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2152, 2379}, {1847,  680}, 
{   0,    0}, {2151, 2405}, {   0,    0}, {   0,    0}, { 245,   57}, 
{   0,    0}, { 245, 2699}, { 245, 2697}, { 245, 2698}, { 245, 2700}, 
{ 245, 2706}, {   0,    0}, {   0,    0}, {   0,    0}, {1847,  681}, 
{1847,  865}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1847,  866}, {   0,    0}, { 245,   60}, {2152, 2400}, {2152, 2401}, 
{1847, 2581}, {2152, 2166}, {2152, 2383}, {   0,    0}, {   0,    0}, 
{2152, 2382}, {1847,  867}, {2152, 2167}, {2152, 2384}, {   0,    0}, 
{2151, 2171}, {   0,    0}, {   0,    0}, {1847,  685}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1847,  686}, {1847,  687}, 
{   0,    0}, {   0,    0}, {1847,  688}, {   0,    0}, {   0,    0}, 
{1961, 2707}, {   0,    0}, {   0,    0}, {2152, 2168}, {   0,    0}, 
{2152, 2169}, {1847,  868}, {   0,    0}, {1847,  690}, {1847,  869}, 
{2151, 2172}, {2151, 2173}, {2151, 2407}, {1847,  870}, {   0,    0}, 
{1847,  693}, {   0,    0}, { 135, 2711}, {   0,    0}, { 135, 2712}, 
{2151, 2389}, {   0,    0}, {1847,  694}, { 135,  134}, {2151, 2387}, 
{2152, 2170}, {   0,    0}, {   0,    0}, {   0,    0}, { 135, 2701}, 
{ 135, 2702}, { 135, 2703}, { 135, 2704}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1847,  695}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1847,  696}, {1847,  871}, {   0,    0}, {2151, 2174}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2151,  370}, {   0,    0}, {   0,    0}, {2151, 2390}, {   0,    0}, 
{2151, 2408}, {   0,    0}, {2152, 2397}, {   0,    0}, {   0,    0}, 
{2151, 2175}, {1847,  872}, {1961, 2711}, {   0,    0}, {1961, 2712}, 
{2152, 2405}, { 209,   61}, {2151, 2388}, {1961,  134}, {   0,    0}, 
{1961,  135}, {   0,    0}, {   0,    0}, {   0,    0}, {1961, 2701}, 
{1961, 2702}, {1961, 2703}, {1961, 2704}, {1961,  136}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2151, 2403}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2151, 2176}, {   0,    0}, { 208,   61}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2152, 2171}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2151, 2391}, {   0,    0}, { 209,  133}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2151, 2177}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2152, 2172}, 
{2152, 2173}, {2152, 2407}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2151, 2178}, {   0,    0}, {   0,    0}, {   0,    0}, {2152, 2389}, 
{ 208,  133}, {   0,    0}, {   0,    0}, {2152, 2387}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 245,   61}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1280,  181}, {   0,    0}, {1280, 2699}, {1280, 2697}, {1280, 2698}, 
{1280, 2700}, {1280, 2706}, {1280, 2705}, {2152, 2174}, {   0,    0}, 
{2151, 2404}, {   0,    0}, {   0,    0}, {   0,    0}, {2152,  370}, 
{1280,  182}, {2151, 2392}, {2152, 2390}, {1280,  183}, {2152, 2408}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2152, 2175}, 
{   0,    0}, {   0,    0}, {1280, 2520}, {   0,    0}, {1280,  184}, 
{   0,    0}, {2152, 2388}, {2151, 2402}, {   0,    0}, {   0,    0}, 
{ 245,  133}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2151, 2179}, 
{2151, 2180}, {   0,    0}, {   0,    0}, {2152, 2403}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2152, 2176}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2152, 2391}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2152, 2177}, {   0,    0}, 
{2151, 2181}, {   0,    0}, {   0,    0}, {2151,  375}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2151, 2393}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {2152, 2178}, 
{   0,    0}, {   0,    0}, {2151, 2182}, {   0,    0}, {   0,    0}, 
{2151, 2183}, {2151, 2184}, {   0,    0}, {   0,    0}, {1522,  181}, 
{   0,    0}, {1522, 2699}, {1522, 2697}, {1522, 2698}, {1522, 2700}, 
{1522, 2706}, {1522, 2705}, {2151, 2399}, {2151, 2409}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1522,  182}, 
{   0,    0}, {   0,    0}, {1522,  183}, {   0,    0}, {2152, 2404}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2152, 2392}, {1522, 2520}, {   0,    0}, {1522,  184}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2152, 2402}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2152, 2179}, {2152, 2180}, 
{1280,  185}, {1280, 2678}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 209, 2711}, {   0,    0}, { 209, 2712}, {1280, 2680}, {   0,    0}, 
{   0,    0}, { 209,  134}, {   0,    0}, { 209,  135}, {   0,    0}, 
{   0,    0}, {1280, 2522}, { 209, 2701}, { 209, 2702}, { 209, 2703}, 
{ 209, 2704}, { 209,  136}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1280,   61}, {   0,    0}, {   0,    0}, { 208, 2711}, {2152, 2181}, 
{ 208, 2712}, {1280,  186}, {2152,  375}, {   0,    0}, { 208,  134}, 
{1280, 2708}, { 208,  135}, {2152, 2393}, {   0,    0}, {   0,    0}, 
{ 208, 2701}, { 208, 2702}, { 208, 2703}, { 208, 2704}, { 208,  136}, 
{   0,    0}, {2152, 2182}, {   0,    0}, {   0,    0}, {2152, 2183}, 
{2152, 2184}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1280,  187}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2152, 2399}, {2152, 2409}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1280,  133}, {   0,    0}, {1280,  188}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1280, 2709}, { 245, 2711}, {   0,    0}, 
{ 245, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, { 245,  134}, 
{   0,    0}, { 245,  135}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 245, 2701}, { 245, 2702}, { 245, 2703}, { 245, 2704}, { 245,  136}, 
{   0,    0}, {   0,    0}, {1280,  189}, {1280,  190}, {1522,  185}, 
{1522, 2678}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1522, 2680}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1280, 1524}, 
{1522, 2522}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1522,   61}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1522,  186}, {   0,    0}, {   0,    0}, { 207,   57}, {1522, 2708}, 
{ 207, 2699}, { 207, 2697}, { 207, 2698}, { 207, 2700}, { 207, 2706}, 
{1280, 2710}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 207,   60}, { 984, 2303}, {   0,    0}, {1522,  187}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1522,  133}, {   0,    0}, {1522,  188}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1522, 2709}, { 136,   57}, {   0,    0}, { 136, 2699}, 
{ 136, 2697}, { 136, 2698}, { 136, 2700}, { 136, 2706}, {   0,    0}, 
{ 984, 2321}, { 984,  368}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 136,   60}, {1522,  189}, {1522,  190}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1280, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 984, 2322}, 
{ 984, 2326}, { 984, 2327}, { 984, 2328}, { 984, 2329}, { 984, 2330}, 
{ 984, 2331}, { 984, 2332}, { 984, 2333}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1041,  181}, {   0,    0}, {1041, 2699}, {1041, 2697}, {1041, 2698}, 
{1041, 2700}, {1041, 2706}, {1041, 2705}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1280, 2521}, {1522, 2710}, 
{1041,  182}, {1280,  191}, {   0,    0}, {1041,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1041, 2520}, {   0,    0}, {1041,  184}, 
{   0,    0}, { 984, 2323}, { 984, 2335}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1280, 2711}, 
{   0,    0}, {1280, 2712}, {1280,  192}, {1280,  193}, {1280,  194}, 
{1280,  134}, {   0,    0}, {1280,  135}, {1280, 2677}, {1280, 2681}, 
{1280, 2679}, {1280, 2701}, {1280, 2702}, {1280, 2703}, {1280, 2704}, 
{1280,  136}, {1283,  181}, {   0,    0}, {1283, 2699}, {1283, 2697}, 
{1283, 2698}, {1283, 2700}, {1283, 2706}, {1283, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1283,  182}, {   0,    0}, {   0,    0}, {1283,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1522, 2707}, {   0,    0}, {1283, 2520}, {   0,    0}, 
{1283,  184}, { 984, 4792}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 207,   61}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 984, 4792}, 
{   0,    0}, {   0,    0}, { 860,  181}, {   0,    0}, { 860, 2699}, 
{ 860, 2697}, { 860, 2698}, { 860, 2700}, { 860, 2706}, { 860, 2705}, 
{   0,    0}, {   0,    0}, {1522, 2521}, {   0,    0}, { 984, 2324}, 
{1522,  191}, {   0,    0}, { 860,  182}, {   0,    0}, {   0,    0}, 
{ 860,  183}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 984, 2740}, { 984,  369}, {   0,    0}, { 207,  133}, 
{   0,    0}, { 860,  184}, { 136,   61}, { 984,  370}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1522, 2711}, {   0,    0}, 
{1522, 2712}, {1522,  192}, {1522,  193}, {1522,  194}, {1522,  134}, 
{   0,    0}, {1522,  135}, {1522, 2677}, {1522, 2681}, {1522, 2679}, 
{1522, 2701}, {1522, 2702}, {1522, 2703}, {1522, 2704}, {1522,  136}, 
{1041,  185}, {1041, 2678}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1041, 2680}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1041, 2522}, {   0,    0}, { 136,  133}, {   0,    0}, 
{ 984,  371}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1041,   61}, {   0,    0}, {   0,    0}, {   0,    0}, { 984, 2325}, 
{   0,    0}, {1041,  186}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1041, 2708}, {   0,    0}, { 984,  372}, {   0,    0}, { 984, 2334}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 984, 2336}, {   0,    0}, 
{   0,    0}, {1283,  185}, {1283, 2678}, {   0,    0}, {   0,    0}, 
{1041,  187}, {   0,    0}, {   0,    0}, {   0,    0}, {1283, 2680}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1041,  133}, {1283, 2522}, {1041,  188}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1041, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1283,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1283,  186}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1283, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1041,  189}, {1041,  190}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 860,  185}, { 860, 2678}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 860, 2680}, {1283,  187}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 984,  373}, { 860, 2519}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1283,  133}, {   0,    0}, {1283,  188}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 860,   61}, {1283, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 860,  186}, { 984,  374}, 
{   0,    0}, {   0,    0}, { 860, 2708}, {   0,    0}, {   0,    0}, 
{1041, 2710}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1283,  189}, {1283,  190}, 
{   0,    0}, { 984,  375}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 860,  187}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 984,  376}, {   0,    0}, {   0,    0}, { 860,  133}, { 984,  377}, 
{ 860,  188}, {   0,    0}, { 207, 2711}, {   0,    0}, { 207, 2712}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 207,  134}, { 860, 2709}, 
{ 207,  135}, {   0,    0}, {   0,    0}, {   0,    0}, { 207, 2701}, 
{ 207, 2702}, { 207, 2703}, { 207, 2704}, { 207,  136}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1283, 2710}, {   0,    0}, {   0,    0}, { 860,  189}, 
{ 860,  190}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1959,  181}, {1041, 2707}, {1959, 2699}, {1959, 2697}, 
{1959, 2698}, {1959, 2700}, {1959, 2706}, {1959, 2705}, {   0,    0}, 
{   0,    0}, { 136, 2711}, {   0,    0}, { 136, 2712}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 136,  134}, {   0,    0}, {1959,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 136, 2701}, { 136, 2702}, 
{ 136, 2703}, { 136, 2704}, {   0,    0}, {1040,  181}, {   0,    0}, 
{1040, 2699}, {1040, 2697}, {1040, 2698}, {1040, 2700}, {1040, 2706}, 
{1040, 2705}, {   0,    0}, {   0,    0}, {1041, 2521}, {   0,    0}, 
{   0,    0}, {1041,  191}, { 860, 2710}, {1040,  182}, {   0,    0}, 
{   0,    0}, {1040,  183}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1040,  184}, {1283, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1041, 2711}, 
{   0,    0}, {1041, 2712}, {1041,  192}, {1041,  193}, {1041,  194}, 
{1041,  134}, {   0,    0}, {1041,  135}, {1041, 2677}, {1041, 2681}, 
{1041, 2679}, {1041, 2701}, {1041, 2702}, {1041, 2703}, {1041, 2704}, 
{1041,  136}, { 658,  181}, {   0,    0}, { 658, 2699}, { 658, 2697}, 
{ 658, 2698}, { 658, 2700}, { 658, 2706}, { 658, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1283, 2521}, 
{   0,    0}, { 658,  182}, {1283,  191}, {   0,    0}, { 658,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 860, 2707}, 
{ 658,  184}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1283, 2711}, {   0,    0}, {1283, 2712}, {1283,  192}, {1283,  193}, 
{1283,  194}, {1283,  134}, {   0,    0}, {1283,  135}, {1283, 2677}, 
{1283, 2681}, {1283, 2679}, {1283, 2701}, {1283, 2702}, {1283, 2703}, 
{1283, 2704}, {1283,  136}, {1099,  181}, {   0,    0}, {1099, 2699}, 
{1099, 2697}, {1099, 2698}, {1099, 2700}, {1099, 2706}, {1099, 2705}, 
{ 860, 2518}, {   0,    0}, {   0,    0}, { 860,  191}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1099,  182}, {   0,    0}, {   0,    0}, 
{1099,  183}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1099,  184}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 860, 2711}, {   0,    0}, { 860, 2712}, { 860,  192}, 
{ 860,  193}, { 860,  194}, { 860,  134}, {   0,    0}, { 860,  135}, 
{ 860, 2677}, { 860, 2681}, { 860, 2679}, { 860, 2701}, { 860, 2702}, 
{ 860, 2703}, { 860, 2704}, { 860,  136}, {1040,  185}, {1040, 2678}, 
{   0,    0}, {1959,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1040, 2680}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1959, 2708}, {   0,    0}, {   0,    0}, {1040, 2519}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 205,   57}, {   0,    0}, 
{ 205, 2699}, { 205, 2697}, { 205, 2698}, { 205, 2700}, { 205, 2706}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1040,   61}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1040,  186}, 
{   0,    0}, { 205,   60}, {   0,    0}, {1040, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1959,  133}, {   0,    0}, {  78,   57}, 
{   0,    0}, {  78, 2699}, {  78, 2697}, {  78, 2698}, {  78, 2700}, 
{  78, 2706}, { 658,  185}, { 658, 2678}, {1959, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1040,  187}, { 658, 2680}, 
{   0,    0}, {   0,    0}, {  78,   60}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 658, 2519}, {   0,    0}, {1040,  133}, 
{   0,    0}, {1040,  188}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1040, 2709}, { 658,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 658,  186}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 658, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1040,  189}, {1040,  190}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1099,  185}, {1099, 2678}, {   0,    0}, 
{   0,    0}, { 658,  187}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1099, 2680}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1099, 1349}, {1959, 2710}, { 658,  133}, {   0,    0}, { 658,  188}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 658, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1099,   61}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1099,  186}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1099, 2708}, {1040, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 658,  189}, { 658,  190}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1099,  187}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1099,  133}, {   0,    0}, 
{1099,  188}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1959, 2707}, {1099, 2709}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 658, 2710}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 205,   61}, {1099,  189}, 
{1099,  190}, {   0,    0}, {   0,    0}, {   0,    0}, {1347,  181}, 
{1040, 2707}, {1347, 2699}, {1347, 2697}, {1347, 2698}, {1347, 2700}, 
{1347, 2706}, {1347, 2705}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1347,  182}, 
{   0,    0}, {   0,    0}, {1347,  183}, {   0,    0}, {  78,   61}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1347,  184}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 205,  133}, 
{1959, 2711}, {1040, 2518}, {1959, 2712}, {   0,    0}, {1040,  191}, 
{   0,    0}, {1959,  134}, {1099, 2710}, {1959,  135}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1959, 2701}, {1959, 2702}, {1959, 2703}, 
{1959, 2704}, {1959,  136}, {   0,    0}, { 658, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  78,  133}, {   0,    0}, {1040, 2711}, {   0,    0}, {1040, 2712}, 
{1040,  192}, {1040,  193}, {1040,  194}, {1040,  134}, {   0,    0}, 
{1040,  135}, {1040, 2677}, {1040, 2681}, {1040, 2679}, {1040, 2701}, 
{1040, 2702}, {1040, 2703}, {1040, 2704}, {1040,  136}, { 263,  181}, 
{   0,    0}, { 263, 2699}, { 263, 2697}, { 263, 2698}, { 263, 2700}, 
{ 263, 2706}, { 263, 2705}, {   0,    0}, {   0,    0}, { 658, 2518}, 
{   0,    0}, {   0,    0}, { 658,  191}, {   0,    0}, { 263,  182}, 
{   0,    0}, {   0,    0}, { 263,  183}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 263,  184}, {1099, 2707}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 658, 2711}, {   0,    0}, { 658, 2712}, { 658,  192}, { 658,  193}, 
{ 658,  194}, { 658,  134}, {   0,    0}, { 658,  135}, { 658, 2677}, 
{ 658, 2681}, { 658, 2679}, { 658, 2701}, { 658, 2702}, { 658, 2703}, 
{ 658, 2704}, { 658,  136}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1607,  181}, {   0,    0}, {1607, 2699}, {1607, 2697}, {1607, 2698}, 
{1607, 2700}, {1607, 2706}, {1607, 2705}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1099,  191}, {   0,    0}, 
{1607,  182}, {   0,    0}, {   0,    0}, {1607,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1347,  185}, 
{1347, 2678}, {   0,    0}, {   0,    0}, {   0,    0}, {1607,  184}, 
{   0,    0}, {   0,    0}, {1347, 2680}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1099, 2711}, {1347, 2559}, {1099, 2712}, {1099,  192}, 
{1099,  193}, {1099,  194}, {1099,  134}, {   0,    0}, {1099,  135}, 
{1099, 2677}, {1099, 2681}, {1099, 2679}, {1099, 2701}, {1099, 2702}, 
{1099, 2703}, {1099, 2704}, {1099,  136}, {   0,    0}, {1347,   61}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1347,  186}, {   0,    0}, {   0,    0}, {   0,    0}, {1347, 2708}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1131,   57}, {   0,    0}, {1131, 2699}, {1131, 2697}, {1131, 2698}, 
{1131, 2700}, {1131, 2706}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1347,  187}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1131, 1390}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1347,  133}, {   0,    0}, {1347,  188}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 263,  185}, 
{ 263, 2678}, {1347, 2709}, { 205, 2711}, {   0,    0}, { 205, 2712}, 
{   0,    0}, {   0,    0}, { 263, 2680}, { 205,  134}, {   0,    0}, 
{ 205,  135}, {   0,    0}, {   0,    0}, {   0,    0}, { 205, 2701}, 
{ 205, 2702}, { 205, 2703}, { 205, 2704}, { 205,  136}, {   0,    0}, 
{   0,    0}, {1347,  189}, {1347,  190}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  78, 2711}, { 263,   61}, 
{  78, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, {  78,  134}, 
{ 263,  186}, {  78,  135}, {   0,    0}, {   0,    0}, { 263, 2708}, 
{  78, 2701}, {  78, 2702}, {  78, 2703}, {  78, 2704}, {  78,  136}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1607,  185}, {1607, 2678}, {   0,    0}, {   0,    0}, { 263,  187}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1607, 2680}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1347, 2710}, 
{ 263,  133}, {   0,    0}, { 263,  188}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 263, 2709}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1607,   61}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1607,  186}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1607, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 263,  189}, { 263,  190}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1607,  187}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1607,  133}, {   0,    0}, {1607,  188}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1347, 2707}, {1607, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 263, 2710}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1131,   61}, {   0,    0}, {1607,  189}, {1607,  190}, {   0,    0}, 
{   0,    0}, {2116,  181}, {   0,    0}, {2116, 2699}, {2116, 2697}, 
{2116, 2698}, {2116, 2700}, {2116, 2706}, {2116, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1347,  191}, {2116,  182}, {   0,    0}, {   0,    0}, {2116,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2116,  184}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1131,  133}, {   0,    0}, {1347, 2711}, {   0,    0}, 
{1347, 2712}, {1347,  192}, {1347,  193}, {1347,  194}, {1347,  134}, 
{1607, 2710}, {1347,  135}, {1347, 2677}, {1347, 2681}, {1347, 2679}, 
{1347, 2701}, {1347, 2702}, {1347, 2703}, {1347, 2704}, {1347,  136}, 
{ 461,  181}, { 263, 2707}, { 461, 2699}, { 461, 2697}, { 461, 2698}, 
{ 461, 2700}, { 461, 2706}, { 461, 2705}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 461,  182}, {   0,    0}, {   0,    0}, { 461,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 461,  184}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 263,  191}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1607, 2707}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 263, 2711}, {   0,    0}, 
{ 263, 2712}, { 263,  192}, { 263,  193}, { 263,  194}, { 263,  134}, 
{   0,    0}, { 263,  135}, { 263, 2677}, { 263, 2681}, { 263, 2679}, 
{ 263, 2701}, { 263, 2702}, { 263, 2703}, { 263, 2704}, { 263,  136}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1607,  191}, {1523,  181}, {   0,    0}, {1523, 2699}, 
{1523, 2697}, {1523, 2698}, {1523, 2700}, {1523, 2706}, {1523, 2705}, 
{   0,    0}, {2116,  185}, {2116, 2678}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1523,  182}, {   0,    0}, {2116, 2680}, 
{1523,  183}, {   0,    0}, {   0,    0}, {   0,    0}, {1607, 2711}, 
{   0,    0}, {1607, 2712}, {1607,  192}, {1607,  193}, {1607,  194}, 
{1607,  134}, {1523,  184}, {1607,  135}, {1607, 2677}, {1607, 2681}, 
{1607, 2679}, {1607, 2701}, {1607, 2702}, {1607, 2703}, {1607, 2704}, 
{1607,  136}, {2116,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2116,  186}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2116, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 461,  185}, { 461, 2678}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2116,  187}, {   0,    0}, { 461, 2680}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2116,  133}, {   0,    0}, {2116,  188}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2116, 2709}, {1131, 2711}, 
{ 461,   61}, {1131, 2712}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1131,  134}, { 461,  186}, {1131,  135}, {   0,    0}, {   0,    0}, 
{ 461, 2708}, {1131, 2701}, {1131, 2702}, {1131, 2703}, {1131, 2704}, 
{1131,  136}, {   0,    0}, {   0,    0}, {2116,  189}, {2116,  190}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 461,  187}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 461,  133}, {   0,    0}, { 461,  188}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 461, 2709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {2116, 2710}, {1523,  185}, {1523, 2678}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 461,  189}, { 461,  190}, {   0,    0}, 
{1523, 2680}, { 134,  181}, {   0,    0}, { 134, 2699}, { 134, 2697}, 
{ 134, 2698}, { 134, 2700}, { 134, 2706}, { 134, 2705}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 134,  182}, {   0,    0}, {   0,    0}, { 134,  183}, 
{   0,    0}, {   0,    0}, {1523,   61}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1523,  186}, {   0,    0}, 
{ 134,  184}, {   0,    0}, {1523, 2708}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 461, 2710}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1523,  187}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2116, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1523,  133}, {   0,    0}, 
{1523,  188}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1523, 2709}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1523,  189}, 
{1523,  190}, {   0,    0}, {2116,  191}, { 189,  181}, {   0,    0}, 
{ 189, 2699}, { 189, 2697}, { 189, 2698}, { 189, 2700}, { 189, 2706}, 
{ 189, 2705}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 461, 2707}, { 189,  182}, {   0,    0}, 
{   0,    0}, { 189,  183}, {   0,    0}, {   0,    0}, {   0,    0}, 
{2116, 2711}, {   0,    0}, {2116, 2712}, {2116,  192}, {2116,  193}, 
{2116,  194}, {2116,  134}, { 189,  184}, {2116,  135}, {2116, 2677}, 
{2116, 2681}, {2116, 2679}, {2116, 2701}, {2116, 2702}, {2116, 2703}, 
{2116, 2704}, {2116,  136}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1523, 2710}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 461,  191}, { 262,  181}, {   0,    0}, { 262, 2699}, 
{ 262, 2697}, { 262, 2698}, { 262, 2700}, { 262, 2706}, { 262, 2705}, 
{   0,    0}, { 134,  185}, { 134, 2678}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 262,  182}, {   0,    0}, { 134, 2680}, 
{ 262,  183}, {   0,    0}, {   0,    0}, {   0,    0}, { 461, 2711}, 
{   0,    0}, { 461, 2712}, { 461,  192}, { 461,  193}, { 461,  194}, 
{ 461,  134}, { 262,  184}, { 461,  135}, { 461, 2677}, { 461, 2681}, 
{ 461, 2679}, { 461, 2701}, { 461, 2702}, { 461, 2703}, { 461, 2704}, 
{ 461,  136}, { 134,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 134,  186}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 134, 2708}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1523, 2707}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 134,  187}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 134,  133}, {   0,    0}, { 134,  188}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 134, 2709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1523,  191}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 189,  250}, { 189, 2678}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 134,  189}, { 134,  190}, 
{   0,    0}, { 189, 2680}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1523, 2711}, {   0,    0}, {1523, 2712}, {1523,  192}, 
{1523,  193}, {1523,  194}, {1523,  134}, {   0,    0}, {1523,  135}, 
{1523, 2677}, {1523, 2681}, {1523, 2679}, {1523, 2701}, {1523, 2702}, 
{1523, 2703}, {1523, 2704}, {1523,  136}, { 189,   61}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 189,  251}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 189, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 134, 2710}, { 262,  185}, { 262, 2678}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 189,  187}, {   0,    0}, 
{ 262, 2680}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 189,  133}, 
{   0,    0}, { 189,  252}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 189, 2709}, {   0,    0}, { 262,   61}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 262,  186}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 262, 2708}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 189,  247}, { 189,  190}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 262,  187}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 134, 2707}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 262,  133}, {   0,    0}, 
{ 262,  188}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 262, 2709}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 189, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 262,  189}, 
{ 262,  190}, {   0,    0}, { 134,  191}, { 861,  181}, {   0,    0}, 
{ 861, 2699}, { 861, 2697}, { 861, 2698}, { 861, 2700}, { 861, 2706}, 
{ 861, 2705}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 861,  182}, {   0,    0}, 
{   0,    0}, { 861,  183}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 134, 2711}, {   0,    0}, { 134, 2712}, { 134,  192}, { 134,  193}, 
{ 134,  194}, { 134,  134}, { 861,  184}, { 134,  135}, { 134, 2677}, 
{ 134, 2681}, { 134, 2679}, { 134, 2701}, { 134, 2702}, { 134, 2703}, 
{ 134, 2704}, { 134,  136}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 262, 2710}, { 259,  181}, {   0,    0}, 
{ 259, 2699}, { 259, 2697}, { 259, 2698}, { 259, 2700}, { 259, 2706}, 
{ 259, 2705}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 189, 2707}, {   0,    0}, {   0,    0}, { 259,  182}, {   0,    0}, 
{   0,    0}, { 259,  183}, {   0,    0}, {   0,    0}, { 934, 2627}, 
{ 934, 2629}, { 934, 2699}, { 934, 2697}, { 934, 2698}, { 934, 2700}, 
{ 934, 2630}, { 934, 2631}, { 259,  184}, { 934, 2632}, { 934, 2633}, 
{ 934, 2628}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 934, 2634}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 189,  191}, 
{   0,    0}, { 730,  727}, { 730,  728}, { 730, 2699}, { 730, 2697}, 
{ 730, 2698}, { 730, 2700}, { 730, 2630}, { 730, 2631}, {   0,    0}, 
{ 730, 2632}, { 730, 2633}, { 730, 4431}, {   0,    0}, { 262, 2707}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 934, 2635}, { 730, 2634}, 
{   0,    0}, {   0,    0}, { 189, 2711}, {   0,    0}, { 189, 2712}, 
{ 189,  253}, { 189,  254}, { 189,  255}, { 189,  134}, {   0,    0}, 
{ 189,  135}, { 189, 2677}, { 189, 2681}, { 189, 2679}, { 189, 2701}, 
{ 189, 2702}, { 189, 2703}, { 189, 2704}, { 189,  136}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 730, 2635}, {   0,    0}, {   0,    0}, { 262,  191}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 861,  185}, { 861, 2678}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 861, 2680}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 262, 2711}, {   0,    0}, { 262, 2712}, { 262,  192}, 
{ 262,  193}, { 262,  194}, { 262,  134}, {   0,    0}, { 262,  135}, 
{ 262, 2677}, { 262, 2681}, { 262, 2679}, { 262, 2701}, { 262, 2702}, 
{ 262, 2703}, { 262, 2704}, { 262,  136}, { 861,   61}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 861,  186}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 861, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 259,  185}, { 259, 2678}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 259, 2680}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 861,  187}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 861,  133}, 
{   0,    0}, { 861,  188}, {   0,    0}, { 259,   61}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 259,  186}, 
{ 861, 2709}, {   0,    0}, {   0,    0}, { 259, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 934, 2636}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 861,  189}, { 861,  190}, {   0,    0}, { 259,  187}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 934, 2637}, {   0,    0}, {   0,    0}, { 259,  133}, 
{   0,    0}, { 259,  188}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 259, 2709}, { 730, 2636}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 934, 2638}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 730, 2637}, {   0,    0}, 
{ 259,  189}, { 259,  190}, {   0,    0}, { 861, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 730, 2638}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 934, 2639}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 259, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 730, 2639}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 861, 2707}, { 482, 2627}, { 482, 2629}, { 482, 2699}, { 482, 2697}, 
{ 482, 2698}, { 482, 2700}, { 482, 2630}, { 482, 2631}, {   0,    0}, 
{ 482, 2632}, { 482, 2633}, { 482, 2628}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 482, 2634}, 
{ 729, 2627}, { 729, 2629}, { 729, 2699}, { 729, 2697}, { 729, 2698}, 
{ 729, 2700}, { 729, 2630}, { 729, 2631}, {   0,    0}, { 729, 2632}, 
{ 729, 2633}, { 729, 2628}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 729, 2634}, { 861,  191}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 259, 2707}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 482, 2635}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 861, 2711}, {   0,    0}, { 861, 2712}, 
{ 861,  192}, { 861,  193}, { 861,  194}, { 861,  134}, { 729, 2635}, 
{ 861,  135}, { 861, 2677}, { 861, 2681}, { 861, 2679}, { 861, 2701}, 
{ 861, 2702}, { 861, 2703}, { 861, 2704}, { 861,  136}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 259,  191}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 259, 2711}, {   0,    0}, { 259, 2712}, 
{ 259,  192}, { 259,  193}, { 259,  194}, { 259,  134}, {   0,    0}, 
{ 259,  135}, { 259, 2677}, { 259, 2681}, { 259, 2679}, { 259, 2701}, 
{ 259, 2702}, { 259, 2703}, { 259, 2704}, { 259,  136}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 934, 2652}, { 934, 2649}, { 934, 2653}, { 934, 2641}, 
{ 934, 2642}, { 934, 2645}, { 934, 2651}, { 934, 2654}, { 934, 2650}, 
{ 934, 2701}, { 934, 2702}, { 934, 2703}, { 934, 2704}, { 934, 2646}, 
{ 934, 2648}, { 934, 2640}, { 934, 2644}, { 934, 2643}, { 934, 2647}, 
{ 934, 2623}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 730, 2652}, { 730, 2649}, 
{ 730, 2653}, { 730, 2641}, { 730, 2642}, { 730, 2645}, { 730, 2651}, 
{ 730, 2654}, { 730, 2650}, { 730, 2701}, { 730, 2702}, { 730, 2703}, 
{ 730, 2704}, { 730, 2646}, { 730, 2648}, { 730, 2640}, { 730, 2644}, 
{ 730, 2643}, { 730, 2647}, { 730,  729}, {1784,  817}, {   0,    0}, 
{1784, 2699}, {1784, 2697}, {1784, 2698}, {1784, 2700}, {1784, 2706}, 
{1784, 2705}, { 482, 2636}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1784,  183}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 482, 2637}, {   0,    0}, 
{ 729, 2636}, { 935,  727}, { 935,  728}, { 935, 2699}, { 935, 2697}, 
{ 935, 2698}, { 935, 2700}, { 935, 2630}, { 935, 2631}, {   0,    0}, 
{ 935, 2632}, { 935, 2633}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 729, 2637}, {   0,    0}, { 935, 2634}, 
{   0,    0}, {   0,    0}, { 482, 2638}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 729, 2638}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 935, 2635}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 482, 2639}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 729, 2639}, {   0,    0}, { 581,  727}, { 581,  728}, { 581, 2699}, 
{ 581, 2697}, { 581, 2698}, { 581, 2700}, { 581, 2630}, { 581, 2631}, 
{   0,    0}, { 581, 2632}, { 581, 2633}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 581, 2634}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 198,  181}, {   0,    0}, { 198, 2699}, 
{ 198, 2697}, { 198, 2698}, { 198, 2700}, { 198, 2706}, { 198, 2705}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 198,  183}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 581, 2635}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1784,   61}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1784, 2708}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 935, 2636}, {1605,  817}, {   0,    0}, {1605, 2699}, 
{1605, 2697}, {1605, 2698}, {1605, 2700}, {1605, 2706}, {1605, 2705}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1784,  133}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 935, 2637}, {   0,    0}, 
{1605,  183}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1784, 2709}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 935, 2638}, { 482, 2652}, { 482, 2649}, 
{ 482, 2653}, { 482, 2641}, { 482, 2642}, { 482, 2645}, { 482, 2651}, 
{ 482, 2654}, { 482, 2650}, { 482, 2701}, { 482, 2702}, { 482, 2703}, 
{ 482, 2704}, { 482, 2646}, { 482, 2648}, { 482, 2640}, { 482, 2644}, 
{ 482, 2643}, { 482, 2647}, { 729, 2652}, { 729, 2649}, { 729, 2653}, 
{ 729, 2641}, { 729, 2642}, { 729, 2645}, { 729, 2651}, { 729, 2654}, 
{ 729, 2650}, { 729, 2701}, { 729, 2702}, { 729, 2703}, { 729, 2704}, 
{ 729, 2646}, { 729, 2648}, { 729, 2640}, { 729, 2644}, { 729, 2643}, 
{ 729, 2647}, { 935, 2639}, { 581, 2636}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1784, 2710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 581, 2637}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 198,   61}, {   0,    0}, { 302, 2305}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 198, 2708}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 581, 2638}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 302, 2321}, { 302,  368}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 198,  133}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 198, 2709}, 
{1784, 2707}, {   0,    0}, { 581, 2639}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 302, 2322}, { 302, 2326}, { 302, 2327}, { 302, 2328}, 
{ 302, 2329}, { 302, 2330}, { 302, 2331}, { 302, 2332}, { 302, 2333}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1605,   61}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1605, 2708}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 302, 2323}, { 302, 2335}, 
{   0,    0}, {   0,    0}, {1784, 2711}, {   0,    0}, {1784, 2712}, 
{   0,    0}, {   0,    0}, { 198, 2710}, {1784,  134}, {   0,    0}, 
{1784,  135}, {   0,    0}, {   0,    0}, {1605,  133}, {1784, 2701}, 
{1784, 2702}, {1784, 2703}, {1784, 2704}, {1784,  136}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1605, 2709}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 935, 2652}, { 935, 2649}, 
{ 935, 2653}, { 935, 2641}, { 935, 2642}, { 935, 2645}, { 935, 2651}, 
{ 935, 2654}, { 935, 2650}, { 935, 2701}, { 935, 2702}, { 935, 2703}, 
{ 935, 2704}, { 935, 2646}, { 935, 2648}, { 935, 2640}, { 935, 2644}, 
{ 935, 2643}, { 935, 2647}, { 935, 4448}, { 302, 4792}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 302, 4792}, {   0,    0}, {   0,    0}, { 198, 2707}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 302, 2324}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1605, 2710}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 302, 2740}, { 302,  369}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 302,  370}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 581, 2652}, 
{ 581, 2649}, { 581, 2653}, { 581, 2641}, { 581, 2642}, { 581, 2645}, 
{ 581, 2651}, { 581, 2654}, { 581, 2650}, { 581, 2701}, { 581, 2702}, 
{ 581, 2703}, { 581, 2704}, { 581, 2646}, { 581, 2648}, { 581, 2640}, 
{ 581, 2644}, { 581, 2643}, { 581, 2647}, { 581,  729}, {   0,    0}, 
{   0,    0}, { 198, 2711}, {   0,    0}, { 198, 2712}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 198,  134}, {   0,    0}, { 198,  135}, 
{   0,    0}, {   0,    0}, { 302,  371}, { 198, 2701}, { 198, 2702}, 
{ 198, 2703}, { 198, 2704}, { 198,  136}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1605, 2707}, 
{   0,    0}, { 302, 2325}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 302,  372}, 
{   0,    0}, { 302, 2334}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 302, 2336}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {1605, 2711}, {   0,    0}, {1605, 2712}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1605,  134}, {   0,    0}, {1605,  135}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1605, 2701}, {1605, 2702}, 
{1605, 2703}, {1605, 2704}, {1605,  136}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 302,  373}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 302,  374}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 302,  375}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 302,  376}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 302,  377}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yytNComb	yyNComb		[yyNTableMax - yyLastTerminal] = {
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 153,  215}, 
{ 215, 2939}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 218, 2964}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 924, 1158}, 
{ 924, 3000}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 924, 1160}, { 924, 3016}, {   0,    0}, {1681, 3018}, 
{1681, 1881}, {1681, 3022}, {   0,    0}, { 924, 1161}, { 924, 3031}, 
{   0,    0}, {   0,    0}, {1687, 3036}, {1687, 1688}, { 924, 1162}, 
{ 924, 3042}, {   0,    0}, {   0,    0}, { 924, 2995}, { 924, 2996}, 
{ 924, 2997}, { 924, 2998}, { 924, 2999}, {   0,    0}, {   0,    0}, 
{ 218,  281}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {2077, 2126}, {2077, 3123}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1413, 3070}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1413, 1675}, 
{   0,    0}, { 153, 2943}, { 153,  216}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {2126, 3124}, {2126, 2153}, {   0,    0}, 
{   0,    0}, { 359, 3173}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  49, 5046}, {  49, 5047}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 359,  473}, { 359,  474}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1203, 3296}, {   0,    0}, {  49, 5048}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 931, 3323}, 
{ 931, 3328}, { 931, 3330}, {   0,    0}, { 416, 3370}, { 416,  534}, 
{ 304,  411}, { 304,  412}, {   0,    0}, {  49, 5049}, {   0,    0}, 
{ 600,  764}, { 600,  765}, { 600, 3257}, { 477, 3064}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  49,  116}, {   0,    0}, 
{   0,    0}, { 232,  302}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 215, 2944}, { 215,  275}, { 787, 3498}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 354,  467}, 
{ 354, 3574}, { 354, 3577}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 794,  993}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 705, 3609}, { 705,  705}, { 702,  908}, { 702,  704}, { 705, 3610}, 
{ 567, 3302}, { 701, 3598}, { 701,  702}, { 221,  285}, {1351,  873}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 993, 1216}, 
{ 993, 3597}, { 993,  702}, { 993,  703}, {   0,    0}, { 705, 3611}, 
{1858, 3145}, {   0,    0}, {   0,    0}, { 705, 3612}, {   0,    0}, 
{   0,    0}, { 705, 3613}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   5,   54}, {   0,    0}, {1016, 1246}, {1016, 3865}, 
{1016, 3868}, {1016, 1247}, {1490, 1726}, {1491, 1727}, { 705, 3614}, 
{ 705, 3615}, {1323, 1563}, {   0,    0}, {   0,    0}, { 705, 3616}, 
{   0,    0}, {1016, 3869}, { 705, 3617}, { 705, 3618}, {1563, 1767}, 
{1563, 1768}, { 705, 3619}, { 705, 3620}, {   6,   52}, {   6, 3006}, 
{ 705, 3621}, {2092, 3028}, { 705, 3622}, {   0,    0}, { 808, 3898}, 
{1576, 1780}, {1356, 1599}, {   0,    0}, {   0,    0}, { 858, 1079}, 
{ 858, 1080}, { 858, 3938}, { 705, 3623}, {   0,    0}, { 705, 3624}, 
{ 705, 3625}, { 705, 3626}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 705, 3627}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {1040, 4012}, {   0,    0}, {   0,    0}, 
{ 705, 3628}, {   0,    0}, {   0,    0}, {1650, 1849}, { 705, 3629}, 
{ 705, 3630}, {   0,    0}, { 705, 3631}, {   0,    0}, { 359,  475}, 
{ 705, 3632}, { 705, 3633}, { 705, 3634}, {   0,    0}, { 705, 3635}, 
{1323, 1251}, {   0,    0}, {   0,    0}, {   0,    0}, { 705, 3636}, 
{ 260, 4492}, {1522, 4023}, { 189,  195}, {   0,    0}, { 705, 3637}, 
{ 705, 3638}, {   0,    0}, {   0,    0}, { 998, 1223}, { 998, 1224}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1386, 1635}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 705, 3639}, {   0,    0}, 
{   0,    0}, {1346, 1589}, {   0,    0}, { 705, 3640}, { 705, 3641}, 
{   0,    0}, { 705, 3642}, { 705, 3643}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 705, 3644}, {   0,    0}, {1038, 1274}, {1038, 4003}, 
{1038, 4006}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 705, 3645}, {   0,    0}, {2098, 2095}, 
{2106, 4130}, {1489, 1724}, {   0,    0}, { 705, 3646}, { 705, 3647}, 
{1351, 4183}, {1250, 1493}, {1250, 1494}, { 808,  809}, { 228, 3428}, 
{ 567,  713}, { 705, 3648}, { 705, 3649}, {1576, 1781}, {1356, 1600}, 
{   0,    0}, { 705, 3650}, { 705, 3651}, { 705, 3652}, { 761, 3298}, 
{   0,    0}, { 705, 3653}, {   0,    0}, { 477,  570}, { 705, 3654}, 
{ 705, 3655}, {   0,    0}, {   0,    0}, {   0,    0}, {1959, 4265}, 
{1929, 2036}, {   0,    0}, { 705, 3656}, {1814, 4262}, {1814,  874}, 
{   0,    0}, {1814, 3672}, { 705, 3657}, { 705, 3658}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {1560, 1764}, 
{ 705, 3659}, {   0,    0}, {   0,    0}, { 705, 3660}, {   0,    0}, 
{   0,    0}, {1814, 3673}, { 705, 3661}, { 705, 3662}, {1814, 3674}, 
{   0,    0}, {   0,    0}, {   0,    0}, {1814, 3675}, { 705, 3663}, 
{   0,    0}, { 705, 3664}, { 705, 3665}, { 705, 3666}, { 705, 3667}, 
{ 705, 3668}, {1917, 2030}, {   0,    0}, {   0,    0}, {   0,    0}, 
{1814, 3676}, {1814, 3677}, {1246, 3866}, { 794,  994}, {1650, 1850}, 
{1814, 3678}, {1389, 1639}, {1389, 4370}, {1814, 3679}, {2098, 2140}, 
{1814, 3680}, { 705, 3669}, {1814, 3681}, {1246, 3869}, {1814, 3682}, 
{   0,    0}, {1814, 3683}, { 592, 3393}, { 702, 3604}, {1814, 3684}, 
{ 359, 3171}, { 359,  286}, { 730, 4435}, { 675, 4179}, { 675,  875}, 
{ 675,  876}, {   0,    0}, { 730,  731}, { 359,  287}, {1814, 3685}, 
{1814, 3686}, {1639, 1842}, { 730, 4443}, {   0,    0}, {1132, 1396}, 
{ 730, 4453}, {1346, 1590}, {1814, 3687}, { 935, 4436}, { 935, 1171}, 
{1040,  828}, { 935, 4446}, {1985, 3501}, { 232,  303}, { 935, 4453}, 
{1639, 4371}, {1814, 3688}, { 707,  911}, { 707, 3587}, { 707,  700}, 
{1814, 3689}, {1814, 3690}, { 707,  701}, {1814, 3691}, {1057, 4123}, 
{1057, 1298}, {1814, 3692}, {1814, 3693}, {1814, 3694}, {1522, 1279}, 
{2077, 2127}, {1814, 3695}, { 189, 4491}, { 260, 4502}, {1851, 1983}, 
{1814, 3696}, {1389, 1640}, { 730, 4450}, { 730,  732}, { 787, 3494}, 
{1814, 3697}, {1814, 3698}, { 189, 4590}, { 189, 4591}, { 189,  198}, 
{ 468,  560}, { 935, 4450}, { 935,  732}, {2126, 2127}, { 135, 4600}, 
{ 135, 4603}, { 354,  466}, { 135,   67}, {1253, 3889}, {1814, 3699}, 
{1346, 1591}, { 567, 3303}, { 567,  286}, {1687, 3038}, {1814, 3700}, 
{1258, 1500}, {1814, 3701}, {1814, 3702}, {1814, 3703}, { 567,  287}, 
{ 416,  414}, { 858,  821}, {1814, 3704}, {1681, 1883}, { 477, 3065}, 
{ 477,  286}, { 730, 4460}, { 730,  733}, {1122, 1377}, {1639, 1640}, 
{1033, 3954}, { 135, 4612}, { 477,  287}, {1814, 3705}, { 701,  907}, 
{ 935, 4460}, { 935,  733}, {1071, 1313}, {1763, 1932}, {1814, 3706}, 
{1071, 1314}, {1814, 3707}, {1071, 1315}, { 993,  907}, {1071, 1316}, 
{ 993, 4705}, {1071, 1317}, {1814, 3708}, {1071, 1318}, {1814, 3709}, 
{ 989, 4692}, {1016, 1249}, {1814, 3710}, {1814, 3711}, { 797, 1003}, 
{1814, 3712}, { 135, 4604}, { 797, 1004}, {1814, 3713}, { 797, 1005}, 
{1814, 3714}, { 797, 1006}, {1814, 3715}, { 797, 1007}, {1038, 1275}, 
{ 797, 1008}, { 931,  378}, {1413, 1676}, {1814, 3716}, {  42,  100}, 
{  27,   85}, {1121, 4123}, {1959,  199}, {1814, 3717}, {1814, 3718}, 
{1650, 3189}, {1650,  286}, { 675, 4697}, {1985, 2073}, { 675,   67}, 
{1033, 3947}, {1814, 3719}, {1563, 1495}, {1650,  287}, {1386,  810}, 
{1814, 3720}, { 416,  532}, { 989, 4117}, {1814, 3721}, {1132, 1397}, 
{1814, 3722}, {1122, 4320}, {1122, 4321}, {1122, 4322}, {1122, 4323}, 
{1122, 4324}, {1814, 3723}, {1814, 3724}, {1814, 3725}, {1814, 3726}, 
{1814, 3727}, {  35,   95}, {1814, 3728}, { 675, 4612}, { 989, 4694}, 
{2126, 2154}, { 989, 4693}, { 989, 4736}, { 989, 4737}, { 703,  909}, 
{1858, 4680}, {1563, 3856}, {1386, 4669}, { 600,  766}, {1038, 1276}, 
{1851, 1984}, { 675,  877}, {1959, 4726}, {1814, 3729}, { 675,  878}, 
{ 675,  803}, { 675, 4698}, { 675,  879}, {  27, 4998}, {  29,   91}, 
{1959, 4029}, {1959,  200}, {1959,  201}, { 998, 3764}, {1959, 4720}, 
{1639, 1630}, {1959,  202}, { 221, 2963}, { 221,  286}, {1959,  203}, 
{ 228,  296}, {1959, 4730}, {  27, 4989}, {1132, 1398}, {  27, 4992}, 
{ 221,  287}, {1858, 3156}, {1121, 1375}, {  27, 4997}, {  27, 4990}, 
{1929,  810}, {  27, 4993}, {  27, 4994}, {  27, 4995}, {  27, 4996}, 
{  27, 4991}, {  27, 2914}, { 172, 4726}, {  30,   88}, {1858, 1661}, 
{1858, 4707}, { 172, 4734}, { 172, 4707}, { 172,  242}, {1560,  810}, 
{ 172, 4715}, {  33,   88}, {  42, 5041}, { 172, 4716}, {  42, 5039}, 
{ 172, 4728}, { 400,  517}, { 172, 4727}, {1386, 1483}, {  42, 5042}, 
{ 172, 4731}, {  42, 5040}, { 172, 4732}, {1929, 4669}, { 400, 3430}, 
{ 400,  200}, { 400,  201}, {  28,   88}, { 400, 4720}, {1961, 1811}, 
{ 400,  202}, {1917,  810}, { 675,  880}, { 400,  203}, {1122, 1378}, 
{ 400, 4730}, {   5, 3038}, {1560, 4669}, {  42, 2929}, {  84, 4734}, 
{1346, 4412}, {  41,   98}, {  29, 5001}, {  29, 5003}, {  35, 5020}, 
{  35, 5018}, {  84, 4907}, {  29, 5004}, {  84, 4728}, {  29, 5002}, 
{  84, 4727}, {  35, 5021}, {  35, 5019}, {  84, 4731}, {1074, 4686}, 
{  84, 4732}, {1074,   67}, {  29, 2916}, {  40,   88}, {1917, 4669}, 
{  34,   88}, {  39,   98}, {1685, 4734}, {  35, 2922}, {1386, 4341}, 
{1685, 3049}, {1080, 3939}, {  32,   93}, {1250, 3853}, {1057, 1300}, 
{  30, 4989}, { 761, 3299}, {1685, 4717}, {1685, 4718}, {1985, 3502}, 
{1985,  286}, {2092, 4726}, {  30, 4990}, {  33, 4989}, {1929, 1483}, 
{1074, 4612}, {2092, 3029}, {1985,  287}, {  30, 5006}, {2092, 3030}, 
{  33, 4990}, {1247, 3872}, {  30, 2917}, {1246, 1249}, {1074, 4665}, 
{1632, 1838}, {  33, 5014}, { 105,  153}, {1560, 1483}, {  28, 4989}, 
{ 875, 1097}, { 468,  561}, { 468,  286}, {  33, 2920}, { 105, 2942}, 
{ 105,  154}, {  28, 4990}, {1643, 4697}, { 897, 4689}, { 468,  287}, 
{1074, 4687}, { 228,  297}, {  28, 4999}, {2145, 2163}, {  28, 2915}, 
{  41, 5036}, {1851, 3199}, {1851,  286}, { 761, 3300}, {1018, 1252}, 
{1917, 1483}, {1929, 4134}, {  41, 5037}, {1110, 4250}, {1851,  287}, 
{1110,   67}, {1560, 3842}, {1018, 1253}, {1018, 3888}, {1018, 3891}, 
{  40, 4989}, {1961, 4264}, {  34, 4989}, {1997, 3091}, {1980, 2069}, 
{  39, 5031}, { 228,  298}, {  40, 4990}, {1781, 1941}, {  34, 4990}, 
{  32, 5011}, {  41, 2928}, {  39, 5032}, {  40, 5034}, { 897, 4667}, 
{  34, 5016}, {1643, 1845}, {  32, 5012}, {1899, 4396}, {1110, 4612}, 
{ 265, 4726}, {1643, 4698}, {1643, 4707}, {  34, 2921}, {1917, 3976}, 
{1566, 1770}, { 707,  706}, {  40, 2927}, {  32, 2919}, {  39, 2926}, 
{ 265, 4723}, {   5, 2892}, { 897, 4690}, {1253, 1254}, { 265, 4724}, 
{ 897, 4691}, {1121, 1300}, { 265, 4725}, {1552, 1759}, {1110, 4251}, 
{1997, 4726}, {1997, 4618}, { 272, 2938}, {1132, 4405}, { 272,  212}, 
{1786, 1945}, { 272,  105}, { 440,  311}, {1997, 3092}, {2213, 4683}, 
{ 272,  106}, { 934, 4455}, { 934, 4457}, {1121, 1376}, { 497,  591}, 
{ 497,  592}, { 762,  968}, { 762,  969}, { 762, 3243}, { 440, 4642}, 
{ 496,  588}, { 496,  589}, { 497,  593}, { 496,  590}, {  82, 4908}, 
{  82, 4909}, {1899, 4397}, {1544, 4632}, {   6, 2893}, { 703, 3607}, 
{ 703,  286}, {1977, 2067}, { 592,  751}, {1544, 4655}, {1544, 1478}, 
{ 364, 2972}, { 364, 2973}, { 703,  287}, {1158, 3001}, {1158, 1159}, 
{1158, 3006}, { 364, 2974}, {1033, 1034}, {2213, 3552}, {1258, 1483}, 
{1253, 3887}, { 592,  752}, {  37, 5025}, {1275, 4651}, { 440, 4644}, 
{1275,   67}, {1149, 3004}, {1149, 1421}, {1149, 1422}, {  37, 5026}, 
{2213, 4684}, {  31, 5008}, {1961, 1813}, {1071, 1009}, {1321,  810}, 
{ 306,  416}, { 306, 3369}, {  38, 5028}, {  31, 5009}, {1076, 1324}, 
{ 263, 4489}, { 263, 4490}, {1763, 1483}, {  37, 2924}, {  38, 5029}, 
{1899, 1900}, {  31, 2918}, { 530, 3376}, {1980, 1399}, {1275, 4612}, 
{1258, 3908}, { 769, 3286}, { 770, 3224}, { 934, 4460}, { 770,  415}, 
{1321, 1560}, {  82, 4506}, { 262,  349}, { 262, 4488}, {  38, 2925}, 
{ 770, 4696}, {1691, 4659}, {1321, 4668}, {1961, 4266}, { 894, 4326}, 
{ 894,  836}, {1605, 1812}, {1605, 4263}, { 259,  347}, { 259,  197}, 
{1763, 3840}, { 875, 1098}, { 168,  237}, { 653,  819}, { 653,  820}, 
{1487, 1722}, {1487, 3871}, { 883, 1108}, { 883,  177}, { 690,  895}, 
{  25,   81}, {  36, 5023}, { 836, 1050}, {1071, 3751}, {1079, 1327}, 
{ 690, 4329}, {1280, 1278}, {1280, 4022}, { 836, 1051}, { 860, 1082}, 
{ 860, 4011}, {1846, 4274}, {  12,   71}, {  36, 2923}, {1327, 1566}, 
{ 769, 3287}, {  61,  124}, {  61,  125}, {  17, 4919}, {1691, 4618}, 
{ 797, 3735}, {  17, 2904}, {1021, 1258}, {1021,  138}, {1583, 1786}, 
{1339, 1580}, { 168,  238}, {1581, 1785}, {1581, 4260}, { 577,  722}, 
{ 577, 4442}, { 404,  523}, { 389,  505}, {   2,   48}, {   2,   49}, 
{   1,   46}, {1330, 1570}, {1330, 1282}, {   1,   47}, {1980, 4406}, 
{1274, 4004}, { 404,  524}, { 389,  506}, { 204,  266}, { 204,  139}, 
{ 597,  757}, {1080, 1029}, {1781, 1800}, { 483, 4441}, { 870, 1094}, 
{ 483,  484}, { 208,  270}, { 208, 4598}, { 827, 1042}, {1645, 4272}, 
{ 827, 1043}, { 290,  380}, { 875,  880}, {  12, 2899}, {1683, 1885}, 
{1683, 3045}, { 870, 4664}, { 971, 1201}, { 971, 3250}, { 597, 4662}, 
{1197, 1446}, {1197, 3232}, { 967, 1195}, { 967, 3241}, {  11,   68}, 
{ 983, 1208}, { 195,  260}, { 209, 4599}, { 209, 4602}, {  43,  101}, 
{ 308, 3455}, {1885, 1884}, { 616,  776}, { 308, 3471}, {  77,  132}, 
{1019, 1255}, {1452, 1699}, {1452, 1700}, { 922, 1146}, { 319,  441}, 
{ 680, 4240}, {1885, 3046}, { 292, 3207}, {1751, 1924}, {1751, 4106}, 
{1318, 1557}, {1714, 1906}, {1761, 1931}, {1247, 3873}, {1234, 1473}, 
{ 295,  399}, { 616, 4660}, {1008, 1235}, {1471, 1715}, {1930, 2037}, 
{ 407, 3310}, {1556, 1762}, {1881, 3023}, {1111, 1361}, {1881, 1882}, 
{1247, 1248}, {1023, 1262}, { 290,  381}, {1341, 1582}, {1781, 4208}, 
{1019, 1256}, {1537, 1750}, {1537, 4112}, {1338, 1579}, {1123, 1380}, 
{ 316,  439}, { 316, 4103}, { 674,  851}, { 674, 4150}, {1326, 1565}, 
{2046, 2112}, {1953, 2052}, { 992, 1214}, {1026, 3942}, {1749, 4113}, 
{ 691,  896}, { 691, 4334}, { 972, 3248}, { 107,  157}, { 435, 4104}, 
{ 581,  730}, { 581, 4434}, { 746,  950}, {1080, 3935}, { 510,  608}, 
{ 510, 3284}, { 244,  317}, { 244, 4098}, { 365, 2948}, { 937, 4452}, 
{  83,  141}, { 937, 4705}, {  25, 2912}, {1994, 2084}, { 589, 3417}, 
{2152, 3544}, {1857, 1988}, {1857, 3160}, { 968, 1198}, { 968, 3234}, 
{ 764,  972}, { 764, 3252}, {  77, 4940}, { 352, 3568}, {1018, 1254}, 
{ 786, 3462}, { 786, 4707}, { 992, 1215}, {1388, 1638}, {1388, 4364}, 
{1602, 1807}, {1092, 4335}, { 183, 4749}, {  83,  142}, {1632, 1483}, 
{ 195,  261}, {1782, 1942}, {1065, 4161}, {1065, 1064}, {1811, 1959}, 
{1292, 4079}, {1292, 1293}, { 183, 4679}, { 647,  806}, {1597, 1795}, 
{1054, 1292}, {1054, 4078}, {  11, 2898}, { 319,  258}, {1994, 2085}, 
{1562, 1766}, {1645, 1846}, {2145, 1483}, {2152, 3507}, {1243, 1485}, 
{ 647, 4666}, {1312, 1552}, {1312, 1553}, {2038, 2107}, { 154,  218}, 
{1076,  810}, { 154,  219}, {1841, 4365}, {1383, 4336}, {1066, 4157}, 
{1241, 1482}, {1975, 2064}, {1066, 1067}, {1583, 1630}, { 649,  808}, 
{ 649, 3897}, {1569, 1773}, {1912, 2026}, {1934, 2039}, {2041, 2109}, 
{1736, 1915}, {1304, 1547}, {1566, 1460}, {1339, 1363}, {2043, 2111}, 
{1632, 4343}, {1272, 1518}, { 910, 1133}, { 793,  991}, { 894, 4327}, 
{1573, 1776}, {2145, 3988}, {  43, 2930}, {1076, 4669}, {1586, 1789}, 
{1552, 1460}, {2048, 2113}, { 850, 1068}, {1673, 1873}, {2105, 2143}, 
{ 850, 4158}, {2108, 2144}, {1479, 1718}, {1787, 1946}, {1786, 1835}, 
{2029, 2104}, {  54, 3039}, { 849, 1066}, { 871, 1095}, { 871,  904}, 
{ 849, 4156}, { 847, 1063}, {1013, 1242}, {1629, 1834}, { 847, 4160}, 
{1566, 3933}, {2049, 2114}, {1127, 1385}, {1223, 1459}, {1397, 1648}, 
{1264, 1504}, {1600, 1799}, {1552, 3746}, {1590, 1792}, {1938, 2044}, 
{2050, 2115}, {1939, 2045}, {1436, 4658}, {1079, 3937}, {1794, 1951}, 
{1977, 1835}, {1092,  897}, {1795, 1952}, {1322, 1561}, { 970, 1199}, 
{ 970, 3251}, {1615, 4298}, {1811, 1960}, {1060, 1303}, {  54,  120}, 
{1200, 1447}, {1200, 3249}, {1329, 1568}, {1332, 1572}, {1436, 1692}, 
{ 680,  883}, {1068, 4159}, {1614, 4302}, {1068, 1069}, {1786, 4332}, 
{1036, 1271}, {1890, 2010}, {1344, 1585}, {2128, 2156}, {2128, 3112}, 
{ 367,  483}, { 367, 4440}, {1296, 1539}, {1296, 4095}, { 687,  891}, 
{ 687, 4293}, {1841, 4367}, { 261, 4496}, { 261, 4499}, { 348, 4497}, 
{ 348, 4500}, {1342, 1583}, {1342, 1382}, {1564, 1769}, {1100, 1351}, 
{1100, 4191}, { 645,  801}, { 645, 3849}, {1836, 4339}, {1383, 4337}, 
{1836,  176}, {1052, 4068}, {1977, 4358}, {1702, 1899}, {1702, 4395}, 
{ 824, 3998}, { 824,  803}, {1771, 1934}, {1738, 1917}, {1274, 1276}, 
{ 380, 4425}, { 380,  493}, {1594, 4192}, {2156, 3113}, {1771, 1935}, 
{1738, 1918}, {1059, 4652}, {1749, 1747}, {1301, 4654}, { 643,  797}, 
{1430, 1687}, {1430, 3035}, { 853, 1071}, { 306,  414}, {1515, 1736}, 
{ 174, 4096}, { 435,  436}, { 891, 4294}, {1301, 1544}, {1059, 1302}, 
{  14,   73}, {1515, 1737}, { 646,  805}, {1327, 1265}, {1445, 1696}, 
{1445, 3231}, {1196, 1444}, {1196, 3233}, {1578, 1783}, { 530, 3377}, 
{1029, 1033}, {1029, 3946}, {1194, 1443}, {1194, 3240}, { 760,  965}, 
{ 760, 3256}, { 966, 1193}, { 966, 3242}, { 668,  840}, {  16,   74}, 
{  13,   72}, { 659,  829}, { 755, 4650}, {  47, 2933}, {2082, 3078}, 
{2082, 2130}, {  47,  104}, { 664, 4062}, { 664,  836}, { 482,  576}, 
{ 482, 4454}, {1083, 1331}, {1936, 2041}, { 835, 1047}, { 911, 3588}, 
{ 911,  700}, {2057, 4312}, {1615, 1819}, { 755,  964}, {1936, 2042}, 
{ 281,  362}, { 281,  363}, { 640,  793}, { 216,  277}, { 216,  278}, 
{ 640,  794}, {1614, 1818}, {1077, 4172}, {1077, 1325}, { 511, 4656}, 
{ 223,  288}, {1375, 1619}, {1815, 1963}, {1815, 4270}, { 801, 3850}, 
{ 159,  227}, {1878, 2984}, { 159,  226}, {1559, 1477}, { 820, 3932}, 
{ 198, 4493}, { 763, 3297}, {1756, 1929}, { 504,  600}, {1282, 4017}, 
{ 729,  934}, { 511,  609}, {1784, 1944}, {1937, 2043}, {1597, 1601}, 
{1602, 4229}, {1947, 2048}, {2079, 3077}, {2080, 3076}, {1846, 3445}, 
{2083, 3079}, {1782, 4231}, {1944, 2047}, {1523, 1741}, { 461,  555}, 
{1721, 1912}, { 428,  543}, {1913, 2027}, {1559, 1763}, { 819, 1027}, 
{1933, 2038}, {1119, 4289}, {2173, 3535}, { 891, 4296}, {1839, 1975}, 
{  15, 2902}, {2116, 2146}, {1733, 3972}, {1709, 3818}, {1397, 4404}, 
{1607, 1814}, {2202, 3553}, { 134,  196}, {1590, 4411}, { 670,  844}, 
{2050, 4413}, { 413,  531}, { 665,  837}, {1587, 1790}, { 838, 4075}, 
{2078, 3075}, {1240, 1479}, {1347, 1592}, { 264, 4722}, { 650,  811}, 
{1348, 1593}, {1594, 1352}, {1099, 1350}, { 407,  300}, { 861, 1083}, 
{1283, 1525}, {1012, 1241}, {1056, 1297}, { 130,  174}, {1023, 3915}, 
{1369, 4292}, { 658,  827}, {1584, 1787}, {1847, 4419}, { 543, 3478}, 
{ 666,  838}, {1111, 4246}, {1384, 1632}, {1519, 3996}, { 856, 1076}, 
{1227, 1462}, {1706, 1903}, {1326, 3918}, {1341, 4315}, {2165, 3880}, 
{2164, 4223}, {1338, 4248}, {1649, 1848}, { 295, 3443}, {1123, 4313}, 
{2046, 4210}, {1953, 4201}, {1744, 4080}, {1318, 3752}, {1714, 3739}, 
{1761, 3754}, {  14, 2901}, {1234, 3737}, {2013, 2096}, {2139, 4378}, 
{1008, 3736}, {1471, 3738}, {1930, 3755}, {1387, 1636}, {1556, 3753}, 
{2178, 3554}, { 352, 3569}, { 746, 3424}, {2191, 3846}, {1307, 1548}, 
{1840, 4355}, {1969, 4318}, {1600, 4199}, { 174,  175}, {  13, 2900}, 
{1938, 4204}, {  16, 2903}, {1939, 4206}, { 983, 1205}, { 829, 1044}, 
{1794, 4195}, {1577, 1782}, {1503, 3925}, {1795, 4197}, { 643,  798}, 
{1456, 3745}, { 853, 1072}, {1916, 2029}, {2148, 4239}, {2141, 3884}, 
{1578, 1604}, { 811, 1021}, { 874, 3671}, {2062, 3879}, { 668,  841}, 
{1901, 3763}, { 353,  465}, {1013, 1243}, {1902, 3883}, {1124, 1381}, 
{ 305,  413}, {1357, 1602}, {1127, 1386}, {1486, 3852}, { 292,  394}, 
{1728, 3881}, {1102, 4187}, { 547, 3477}, {1731, 3922}, { 533, 3372}, 
{ 560,  699}, { 700,  906}, {1211, 1451}, { 648,  807}, {1637, 4366}, 
{ 491, 3341}, {2057,  878}, {2157, 2188}, {1322, 1562}, { 910, 3592}, 
{ 793,  795}, { 107, 3321}, { 659,  830}, {1060, 1304}, {2158, 2189}, 
{2084, 2132}, {1461, 3775}, {1329, 1569}, {1332, 1573}, {1014, 3851}, 
{1790, 1948}, {1978, 4374}, {1562, 3836}, { 843, 1058}, {2183, 3537}, 
{1036, 1272}, {1243, 3828}, {1344, 1586}, { 589,  745}, { 972,  973}, 
{2038, 3838}, { 898, 1126}, {2123, 4420}, {2196, 3531}, {1729, 3876}, 
{1073, 1320}, {1497, 3875}, {1241, 3834}, {1093, 1343}, { 799, 1011}, 
{ 452, 4512}, {1979, 2068}, {1115, 1365}, { 840, 1055}, {1912, 3830}, 
{1358, 4241}, {1116, 1366}, { 889, 4279}, { 779, 3283}, {1569, 3985}, 
{2056, 4226}, {1934, 3987}, {2041, 3986}, {1736, 3977}, { 835, 1048}, 
{ 996, 3784}, {1975, 4342}, {1304, 4133}, {1030, 3951}, {1272, 3975}, 
{2043, 4138}, {2216, 3526}, { 646, 3859}, { 997, 1218}, {2204, 3522}, 
{2217, 3524}, {1573, 4137}, {1223, 3730}, {1026, 3941}, {1479, 3832}, 
{1052, 4072}, {1375, 1299}, {2105, 3978}, {2022, 3782}, {2108, 3989}, 
{1264, 3930}, {1474, 3781}, {2197, 3529}, {2029, 3979}, {1586, 4347}, 
{2118, 4238}, {2048, 4348}, {1629, 4330}, {1973, 2063}, {2020, 3776}, 
{2110, 2145}, { 922, 1147}, {2049, 4360}, {1787, 4349}, { 126, 4688}, 
{2192, 3530}, {1810, 4245}, {2215, 3528}, {2024, 3845}, {2040, 2108}, 
{ 695,  901}, {1501, 1730}, {1606, 4224}, {2103, 3923}, {2021, 3783}, 
{1911, 2025}, {1512, 3950}, {1957, 4237}, {   4, 2891}, {1613, 4288}, 
{1757, 4147}, {1820, 4281}, {1609, 1815}, {1083, 1053}, {1735, 3968}, 
{ 848, 1065}, {1117, 4295}, {1817, 4283}, {1374, 4280}, {1025, 4685}, 
{1475, 3780}, { 542, 3476}, {2181, 3536}, {1703, 4379}, {1529, 4065}, 
{1956, 2055}, {1513, 3949}, {1791, 1949}, {1955, 2054}, {1804, 4236}, 
{ 180, 4606}, { 246,  319}, {1514, 3948}, {1688, 1890}, {1367, 4282}, 
{1712, 3814}, {1713, 3819}, {1689, 3003}, {1673, 1874}, {1476, 1716}, 
{1435, 1691}, {1043, 4016}, {1758, 3761}, {1564, 3910}, {1287, 4031}, 
{1047, 4067}, {1302, 4653}, { 365,  481}, {2032, 4032}, {2065, 4368}, 
{2150, 4375}, {2031, 2105}, { 223,  289}, { 801,  802}, {1919, 4033}, 
{1530, 4057}, {1355, 4213}, {1524, 4018}, {1113, 4258}, {1410, 3144}, 
{ 243,  313}, { 985, 3373}, {1998, 3095}, {1291, 1531}, {1053, 4070}, 
{1745, 1921}, { 988, 4118}, {1596, 1794}, { 548,  632}, { 978, 3451}, 
{ 549,  633}, {1294, 1533}, {1777, 1938}, {1778, 1939}, { 979, 3448}, 
{1571, 1774}, {1070, 1311}, { 777,  982}, {1748, 1923}, {1207, 3450}, 
{2010, 3041}, { 171,  241}, { 578,  723}, { 796,  995}, {1922, 2033}, 
{1206, 3449}, {1732, 3971}, {1039, 1277}, { 433, 4646}, {1350, 4181}, 
{ 939, 1173}, {1528, 1743}, { 366, 2949}, { 505,  601}, { 933, 4451}, 
{2016, 4400}, { 434, 4647}, {1145, 1414}, { 518,  612}, {1349, 4182}, 
{1708, 3817}, {2018, 4402}, {2074, 3163}, { 869, 1092}, {1198, 3230}, 
{1696, 3227}, {1446, 3228}, {1443, 3236}, {1201, 3246}, {1195, 3237}, 
{1447, 3245}, { 965, 3254}, {1193, 3238}, { 765, 3255}, {1890, 3040}, 
{ 969, 3239}, {1199, 3247}, {1444, 3229}, { 523,  617}, {1905, 3808}, 
{2007, 3048}, {2005, 3025}, {2004, 3027}, { 314,  435}, { 767,  975}, 
{1534, 1746}, {1536, 1749}, {1527, 1742}, {1328, 1567}, { 673,  846}, 
{1306, 4145}, {1242, 1484}, {1710, 3816}, { 654,  822}, { 804, 3861}, 
{ 136, 4601}, {2214, 3131}, {1385, 1633}, {1950, 2051}, {2176, 2199}, 
{2156, 3115}, { 862, 1084}, {1035, 1270}, {1561, 1765}, {1271, 1516}, 
{1585, 1788}, {1967, 4306}, {1752, 4120}, {1295, 1538}, {2206, 3532}, 
{ 754,  963}, {1572, 1775}, {1595, 1793}, { 190,  257}, { 622, 3475}, 
{1677, 2986}, { 498,  594}, {1311, 1551}, { 671,  845}, {1734, 3970}, 
{1303, 1545}, { 615,  775}, { 599,  759}, { 598,  758}, {1305, 4143}, 
{1568, 1772}, {1205, 3288}, {1015, 4702}, { 859, 1081}, {1644, 4273}, 
{ 976, 1204}, { 399,  515}, {1448, 3290}, { 120, 4936}, {1151, 1424}, 
{ 832, 1046}, { 371,  487}, { 644,  800}, { 772,  981}, { 854, 1074}, 
{1046, 4043}, { 692,  899}, { 624, 3461}, { 623,  784}, { 783, 3459}, 
{ 995, 1217}, { 207,  269}, {1723, 3874}, {1101, 4180}, {2207, 3129}, 
{ 205,  267}, { 127,  170}, {1131, 1391}, {  78,  137}, {1717, 1909}, 
{1812, 1962}, { 206,  268}, {1340, 1581}, { 238,  310}, {1114, 1364}, 
{1112, 1362}, { 596,  756}, {1631, 1837}, { 936, 1172}, { 927, 1167}, 
{ 245,  318}, {1324, 1564}, {1027, 1264}, {2053, 4222}, {1381, 1629}, 
{1608, 4254}, {1834, 4331}, {1042, 1281}, {1380, 4314}, {1082, 1330}, 
{2064, 4345}, {1364, 1608}, {1962, 4255}, { 613,  773}, { 614,  774}, 
{1838, 4346}, { 508,  604}, {1635, 4344}, {1361, 4247}, {1809, 4243}, 
{2067, 4359}, {1807, 4230}, {2069, 4409}, {1799, 4200}, {1952, 4198}, 
{1878, 3021}, {1396, 4410}, {1648, 4407}, { 855, 1075}, {1398, 4408}, 
{1951, 4196}, { 509,  605}, { 991, 1211}, {2052, 4202}, {1658, 1858}, 
{1999, 2090}, {2155, 2187}, {1599, 4203}, { 225,  291}, { 873, 4184}, 
{ 837, 1052}, { 885, 1111}, {1433, 4619}, {1096, 1346}, {1088, 1338}, 
{ 161,  229}, { 219,  282}, {1656, 1856}, {1315, 1554}, { 905, 1132}, 
{1889, 3034}, {1547, 4135}, {1753, 4128}, { 499,  595}, {1366, 1611}, 
{1869, 3109}, {1654, 1854}, {2036, 4136}, {1653, 1853}, {1005, 1230}, 
{ 626,  788}, { 362, 2968}, { 568,  714}, {1531, 4071}, { 573,  719}, 
{1017, 1250}, {1143, 1412}, { 920, 1144}, { 490, 3327}, { 602,  768}, 
{ 834, 4049}, {1359, 1603}, { 788, 3479}, {1281, 4009}, { 582, 4426}, 
{2030, 3981}, {1518, 3980}, {1991, 2076}, { 158,  225}, { 618,  778}, 
{1620, 1824}, { 812, 1022}, {1440, 3410}, {2000, 3099}, { 420, 3480}, 
{1754, 1927}, {2138, 2161}, {1822, 1967}, {2135, 2160}, {1915, 3982}, 
{2104, 3984}, {2094, 2137}, {1441, 3415}, { 753,  962}, {1442, 1695}, 
{1180, 1441}, { 950, 1180}, {2143, 3983}, {2137, 3391}, { 962, 1192}, 
{1192, 1442}, { 492,  580}, {2011, 2094}, { 513,  610}, {1968, 2059}, 
{ 527,  620}, {1695, 1892}, { 437,  548}, {1892, 2011}, { 590,  746}, 
{ 593,  753}, { 113,  163}, { 438,  549}, { 789, 3488}, { 628, 3486}, 
{ 395,  512}, { 396,  514}, { 627, 3489}, { 790, 3485}, { 667,  839}, 
{1504, 3931}, {1262, 3916}, { 791,  988}, {1500, 3909}, { 792,  989}, 
{1432, 1690}, {1755, 1928}, {1520, 1739}, { 694,  900}, {1494, 3855}, 
{ 515,  611}, {1925, 2034}, {1493, 3854}, { 697,  903}, {2058, 2120}, 
{1575, 1778}, {1521, 1740}, { 557,  638}, {2002, 3102}, {2091, 3101}, 
{1106, 1356}, { 230, 3309}, { 558,  639}, {2099, 4398}, {2003, 2091}, 
{1540, 1752}, { 929, 1169}, { 928, 1168}, {1672, 1872}, { 431,  546}, 
{ 391,  508}, {1153, 1426}, { 427,  542}, { 501,  597}, { 392,  509}, 
{ 500,  596}, { 520,  614}, {2140, 4377}, {2096, 4376}, {  56,  121}, 
{1864, 1993}, { 317, 4099}, {1288, 1529}, {1000, 1226}, {2170, 2196}, 
{ 519,  613}, {1844, 1979}, {1843, 1978}, {  94,  146}, {1976, 2065}, 
{2122, 2150}, {1707, 1904}, { 625,  787}, {1095, 1345}, { 569,  716}, 
{ 903, 1130}, { 652,  816}, {1993, 2077}, {2177, 2200}, {1670, 1869}, 
{2182, 2205}, { 953, 1183}, {1886, 2008}, { 397, 3440}, { 954, 1184}, 
{ 398, 3441}, {1679, 1879}, { 951, 1181}, { 947, 1177}, {1680, 1880}, 
{  50,  118}, {1779, 1940}, { 372,  488}, { 955, 1185}, {2185, 2207}, 
{ 946, 1176}, { 956, 1186}, { 957, 1187}, {1169, 1436}, {1168, 1435}, 
{ 958, 1188}, { 959, 1189}, { 960, 1190}, {1598, 1796}, {1159, 1431}, 
{ 945, 1175}, { 961, 1191}, {1155, 1428}, { 235,  306}, { 949, 1179}, 
{ 944, 1174}, { 425,  540}, { 952, 1182}, {1877, 2003}, { 234,  305}, 
{2093, 2136}, { 121,  167}, { 948, 1178}, {1875, 2002}, { 999, 1225}, 
{ 734,  938}, { 737,  940}, {1872, 2000}, {1231, 1470}, { 738,  941}, 
{ 739,  942}, { 740,  943}, {1871, 1999}, { 655,  823}, {1085, 1335}, 
{ 881, 1106}, {1319, 1558}, { 102,  149}, {1010, 1239}, { 660,  832}, 
{1700, 1895}, { 162,  231}, {1142, 1411}, {1453, 1701}, {1783, 1943}, 
{ 441, 4501}, { 257, 4503}, {1900, 2018}, { 803, 1015}, {1989, 2075}, 
{1861, 1990}, { 409,  528}, { 733,  937}, {2017, 2099}, {1897, 2016}, 
{1166, 1433}, { 728,  933}, {1678, 1878}, { 410,  529}, {  57,  122}, 
{ 727,  932}, {1660, 1860}, {2019, 2100}, {1141, 1410}, {1893, 2012}, 
{ 724,  927}, {1404, 1653}, {1405, 1654}, {1164, 1432}, {1407, 1656}, 
{1457, 1704}, {1855, 1986}, { 385,  499}, {  99,  148}, {1458, 1705}, 
{ 901, 1128}, {  89,  145}, {1662, 1862}, {1541, 1753}, {2072, 2125}, 
{1823, 4305}, {1420, 1680}, {  97,  147}, {2121, 2149}, { 651,  813}, 
{1833, 1972}, {1588, 1644}, {1392, 1644}, { 574, 2951}, { 575, 2952}, 
{ 857, 1078}, { 963, 3213}, {1865, 1994}, {1001, 1227}, {1226, 1461}, 
{1701, 1896}, {1904, 2020}, { 981, 3271}, {1981, 2070}, {1659, 1859}, 
{1647, 1646}, {1331, 4069}, {1808, 1958}, {1395, 1646}, {1337, 1578}, 
{2117, 2147}, {1109, 1359}, {1798, 1954}, {  96,  146}, {1293, 1532}, 
{1657, 1857}, {1157, 1430}, { 930, 1170}, { 467, 3575}, {1419, 1679}, 
{1086, 1336}, { 882, 1107}, {1146, 1416}, { 465, 3575}, {1147, 1417}, 
{1481, 1719}, { 494,  582}, {  86,  144}, { 973, 1202}, {2102, 2142}, 
{ 436, 4105}, { 315, 4100}, {1923, 4108}, {2033, 4114}, {1921, 4093}, 
{ 684,  887}, { 867, 1090}, {1627, 4563}, {1830, 4559}, {1831, 4565}, 
{1828, 4564}, {1829, 4558}, {1628, 4556}, {2161, 4661}, {1408, 1657}, 
{1623, 1826}, {1882, 2006}, {1624, 1827}, { 224,  290}, {1406, 1655}, 
{ 621,  781}, {2061, 4566}, {2060, 4560}, {1818, 4661}, { 601, 3214}, 
{1819, 4661}, { 487,  579}, { 617, 3272}, { 220,  283}, { 612, 3269}, 
{ 594, 3211}, { 532,  621}, { 381,  494}, {2088, 1433}, { 817,  122}, 
{2203, 3543}, { 541, 3470}, {1686, 1888}, { 489, 3329}, { 181,  122}, 
{ 807, 3886}, {1619, 1823}, { 318,  440}, {1172, 1439}, {1909, 2023}, 
{ 170,  240}, {2211, 2216}, {2212, 2217}, {2210, 2215}, {1429, 1686}, 
{1785, 4261}, { 426,  541}, { 368,  485}, {1621, 1825}, { 718,  923}, 
{ 418,  535}, {1024, 1263}, {1037, 1273}, {1414, 2979}, {1064, 1308}, 
{2179, 2203}, {1365, 1609}, { 373,  489}, {1069, 1310}, {1390, 1641}, 
{ 275,  355}, { 169,  239}, {1045, 1287}, {1285, 1528}, { 725, 3332}, 
{1891, 3333}, {1370, 1614}, {1371, 1615}, {1372, 1616}, {1373, 1617}, 
{1401, 1650}, {1402, 1651}, {1412, 1663}, {1837, 1974}, {1391, 1642}, 
{1651, 1851}, {1652, 1852}, {1690, 1891}, {1849, 1982}, { 284,  367}, 
{1852, 1985}, {1983, 2071}, { 104,  151}, {2124, 2151}, {1853, 4661}, 
{2125, 2152}, {1854, 4661}, {1856, 4661}, {1740, 4661}, {1167, 1434}, 
{  81,  140}, { 604, 4661}, { 605, 4661}, {1144, 1413}, {1165, 3331}, 
{ 608, 3285}, {1434, 3334}, { 609, 4657}, {1136, 1401}, { 610, 4657}, 
{2095, 4173}, {1411, 1662}, {2133, 2158}, {1135, 1400}, {1032, 1266}, 
{1002, 1228}, {1233, 1472}, { 872, 1096}, { 914, 1135}, {1089, 1339}, 
{2160, 4661}, { 913, 1134}, {2131, 2157}, { 122, 4617}, { 892, 1121}, 
{ 842, 1057}, { 780,  984}, { 868, 1091}, {2012, 3292}, { 726,  931}, 
{1940, 2046}, { 116,  166}, { 466, 4661}, { 709,  913}, { 708,  912}, 
{ 156,  221}, { 639, 4661}, { 160,  228}, {1655, 1855}, { 611,  771}, 
{ 212,  272}, { 562,  708}, { 474,  567}, {1072, 1319}, { 227,  292}, 
{ 866, 1088}, { 865, 1087}, {1325, 4173}, { 864, 1086}, { 473,  566}, 
{ 863, 1085}, { 229,  299}, { 382,  495}, { 638, 4661}, { 363,  477}, 
{ 278,  359}, { 528, 3380}, { 773, 4663}, {1988, 3161}, { 307,  417}, 
{ 282,  364}, {  45, 2932}, { 176, 4622}, { 512, 4657}, {2101, 4631}, 
{1873, 2001}, {1986, 3151}, {2059, 4173}, { 155,  220}, { 852,  122}, 
{1289, 4055}, { 124, 4672}, { 550, 4623}, {  24, 2911}, { 932, 4638}, 
{2167, 2193}, { 774, 4663}, { 312, 4611}, {  18, 2905}, {  23, 2910}, 
{ 309, 4673}, { 888, 1116}, {1067, 1309}, {1090, 1340}, {1478, 4630}, 
{ 406,  526}, {  22, 2909}, { 732, 4635}, {  67, 4610}, { 514, 4657}, 
{  21, 2908}, { 887, 1114}, { 507,  603}, {  20, 2907}, {1284, 1527}, 
{ 583,  736}, { 662, 4060}, {2129, 3080}, {1863, 1992}, {2034, 4173}, 
{1694, 4636}, {  19, 2906}, { 277,  358}, { 669,  842}, {1377, 1621}, 
{2120, 4173}, {1898, 2017}, {1087, 1337}, {1175, 3418}, { 886, 1112}, 
{2086, 3107}, { 677,  881}, {1007, 1234}, { 679,  882}, {1232, 1471}, 
{1665, 1864}, { 682,  884}, { 683,  885}, { 884, 1109}, {  69, 4938}, 
{ 685,  888}, {1469, 1714}, {2087, 3106}, { 688,  892}, { 689,  893}, 
{ 798, 1010}, {1760, 1930}, {1317, 1556}, {1555, 1761}, {2208, 2214}, 
{ 291,  383}, { 696,  902}, {1075, 1323}, { 698,  905}, { 805, 1017}, 
{1062, 1307}, {1622, 4552}, { 851, 4151}, {  52,  119}, { 236,  308}, 
{1739, 4661}, {1866, 1995}, { 900, 4661}, { 775, 4661}, {1924, 4107}, 
{1538, 4107}, { 839, 4661}, { 771, 3447}, { 293,  395}, { 294,  396}, 
{1191, 3397}, { 529, 3379}, {1251, 1497}, { 809, 1020}, { 637, 4532}, 
{ 250, 4570}, { 503,  599}, { 522,  616}, { 521,  615}, {1796, 1953}, 
{2187, 4661}, { 502,  598}, {2076, 4661}, { 323, 4571}, {1190, 3396}, 
{1189, 3395}, {1188, 3400}, { 663,  835}, { 759, 4661}, { 442, 4586}, 
{1208, 1450}, {1671, 1871}, {1204, 1449}, { 714,  918}, {1187, 3394}, 
{ 938, 3407}, {1186, 3404}, { 918, 3143}, { 343, 4519}, {1022, 4173}, 
{1626, 4553}, { 758, 4661}, { 636, 4526}, {1185, 3401}, { 635, 4550}, 
{ 443, 4579}, { 330, 4534}, { 334, 4538}, { 177, 4626}, { 757, 4663}, 
{1184, 3403}, {1183, 3399}, { 940, 3411}, { 335, 4535}, { 299, 3267}, 
{ 941, 3409}, {1176, 3419}, { 495, 3406}, { 634, 4544}, { 756, 4663}, 
{ 942, 3413}, { 717,  922}, { 253, 4571}, {1666, 1865}, {1182, 3398}, 
{ 192, 4575}, { 191, 4572}, { 344, 4515}, {  44, 2931}, { 320, 4570}, 
{ 446, 4536}, { 412,  530}, { 185, 4574}, { 339, 4514}, { 449, 4537}, 
{ 458, 4518}, { 455, 4517}, {1173, 1440}, { 716,  920}, { 723,  926}, 
{2199, 3556}, { 456, 4530}, { 545, 3464}, { 457, 4524}, {1181, 3402}, 
{ 186,  248}, { 451, 4549}, { 459, 4525}, { 460, 4531}, { 943, 3408}, 
{ 450, 4543}, { 823, 1037}, {1179, 3420}, {1170, 1437}, { 719,  924}, 
{1174, 3422}, { 919, 1143}, { 184, 4569}, { 447, 4548}, { 188,  249}, 
{1927, 4657}, { 193, 4589}, {1178, 3421}, { 776, 4661}, { 816, 1024}, 
{1177, 3423}, { 251,  328}, {1928, 4657}, { 187, 4568}, { 252,  329}, 
{ 736, 3412}, {2175, 2198}, { 194, 4582}, {2168, 2194}, { 254, 4585}, 
{ 255, 4578}, { 166, 3202}, {2071, 3202}, { 448, 4542}, { 341,  454}, 
{ 340,  453}, { 321, 4577}, { 322, 4584}, { 324, 4585}, { 478,  571}, 
{ 325, 4578}, { 331,  444}, { 332,  445}, { 336, 4547}, { 346, 4522}, 
{ 345, 4529}, { 337, 4541}, {1948, 2049}, { 713, 3304}, {1842, 1977}, 
{1161, 3032}, {2186, 3105}, {1212, 1452}, { 768,  976}, { 778,  983}, 
{ 921, 1145}, {1154, 1427}, { 370, 3358}, { 375, 3359}, { 917, 3164}, 
{1477, 4682}, { 810, 4681}, { 475, 3172}, { 479,  572}, {2174, 3542}, 
{ 421, 3472}, { 369, 3338}, {1984, 3200}, { 712,  917}, {1611, 4268}, 
{1963, 4269}, {   8, 2895}, {  10, 2897}, {2073, 3503}, {   9, 2896}, 
{ 580,  726}, { 157,  223}, { 163,  232}, { 620,  780}, {1078, 1326}, 
{1104, 1353}, {1105, 1354}, { 570, 3066}, { 813, 1023}, {1850, 3190}, 
{1334, 1575}, {1333, 1574}, { 106,  156}, {1393, 1645}, { 720, 2953}, 
{1610, 1644}, { 721, 2954}, { 693, 4353}, {2153, 2186}, {1824, 4310}, 
{1160, 3017}, {1422, 3013}, {2015, 2098}, {2097, 2139}, {   7, 2894}, 
{1454, 1702}, { 846, 1062}, {1894, 2013}, { 974, 1203}, {1421, 3011}, 
{1268, 1513}, { 419,  536}, {1267, 1512}, {1202, 3294}, {1236, 1474}, 
{1237, 1475}, {1238, 1476}, {1269, 1514}, {1908, 2022}, {1907, 2021}, 
{ 423,  538}, {1603, 1809}, {1129, 1388}, {1125, 1383}, {  26, 2913}, 
{1664, 1863}, {1667, 1866}, {1230, 1469}, {1669, 1868}, { 544, 3497}, 
{1466, 1711}, { 486, 3360}, { 537, 3496}, { 975, 3215}, {1450, 1698}, 
{ 982, 3273}, {1449, 1697}, {1162, 3043}, { 390,  507}, {1316, 1555}, 
{ 565,  712}, {1554, 1760}, {   3, 2890}, { 384,  498}, { 401,  518}, 
{1006, 1232}, { 358,  471}, { 471,  565}, {1816, 4276}, { 893, 1123}, 
{2028, 2103}, {1542, 1754}, {1091, 1341}, {1260, 1501}, {1543, 1755}, 
{1502, 1731}, {1711, 1905}, { 432,  547}, {2198, 2212}, {1966, 2057}, 
{1887, 3053}, {1506, 1732}, {1511, 1735}, { 233,  304}, {1684, 1887}, 
{2194, 2210}, {1510, 1734}, { 430,  545}, {2201, 2213}, {2169, 2195}, 
{1290, 1530}, { 377,  491}, {1415, 1677}, { 561,  707}, { 536,  622}, 
{ 579,  725}, {1698, 3274}, {1697, 3216}, { 926, 1165}, {2195, 2211}, 
{1508, 1733}, {1870, 1998}, {2172, 2197}, { 538,  623}, { 539,  624}, 
{ 606,  769}, {2035, 2106}, {1467, 1712}, { 607,  770}, {1548, 4142}, 
{1868, 1997}, {2184, 2206}, {1488, 1723}, {1465, 1710}, { 388,  504}, 
{2166, 2192}, {1464, 1709}, {1423, 1681}, {1463, 1708}, {2180, 2204}, 
{1459, 3732}, { 376,  490}, {1668, 1867}, { 619,  779}, {1003, 3731}, 
{1906, 3744}, {1715, 3743}, {1235, 3741}, {1425, 1683}, {1004, 3740}, 
{1224, 3733}, {1553, 3749}, {1427, 1685}, {1473, 3742}, {1353, 1596}, 
{1354, 1597}, {2026, 3831}, {1335, 1576}, {1574, 1777}, {1485, 3829}, 
{1482, 3835}, {1718, 3833}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yytComb *	yyTBasePtr	[yyLastReadState + 1] = { 0,
& yyTComb [   0], & yyTComb [   0], & yyTComb [5739], & yyTComb [1813], 
& yyTComb [5360], & yyTComb [1226], & yyTComb [5722], & yyTComb [6371], 
& yyTComb [6368], & yyTComb [6370], & yyTComb [11500], & yyTComb [3104], 
& yyTComb [4069], & yyTComb [4084], & yyTComb [4104], & yyTComb [4062], 
& yyTComb [4542], & yyTComb [6657], & yyTComb [6634], & yyTComb [6639], 
& yyTComb [6641], & yyTComb [6651], & yyTComb [6656], & yyTComb [6667], 
& yyTComb [4210], & yyTComb [6227], & yyTComb [1315], & yyTComb [2445], 
& yyTComb [1868], & yyTComb [2906], & yyTComb [ 356], & yyTComb [1421], 
& yyTComb [2068], & yyTComb [ 541], & yyTComb [   1], & yyTComb [4037], 
& yyTComb [1083], & yyTComb [1872], & yyTComb [2687], & yyTComb [2862], 
& yyTComb [2725], & yyTComb [  27], & yyTComb [2402], & yyTComb [5484], 
& yyTComb [5095], & yyTComb [  22], & yyTComb [6069], & yyTComb [   0], 
& yyTComb [2340], & yyTComb [3796], & yyTComb [   0], & yyTComb [1041], 
& yyTComb [   0], & yyTComb [2980], & yyTComb [ 372], & yyTComb [2731], 
& yyTComb [4219], & yyTComb [   0], & yyTComb [   0], & yyTComb [4543], 
& yyTComb [3969], & yyTComb [6267], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [6649], & yyTComb [6046], 
& yyTComb [2679], & yyTComb [   0], & yyTComb [6219], & yyTComb [4371], 
& yyTComb [3079], & yyTComb [6019], & yyTComb [ 101], & yyTComb [5784], 
& yyTComb [1039], & yyTComb [18103], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [ 179], & yyTComb [ 798], & yyTComb [ 613], & yyTComb [3948], 
& yyTComb [4305], & yyTComb [4338], & yyTComb [   0], & yyTComb [6434], 
& yyTComb [4286], & yyTComb [ 540], & yyTComb [4021], & yyTComb [6097], 
& yyTComb [6009], & yyTComb [ 187], & yyTComb [5802], & yyTComb [3279], 
& yyTComb [4260], & yyTComb [6347], & yyTComb [ 480], & yyTComb [5916], 
& yyTComb [6039], & yyTComb [4007], & yyTComb [5794], & yyTComb [  18], 
& yyTComb [5991], & yyTComb [6363], & yyTComb [3658], & yyTComb [5474], 
& yyTComb [5476], & yyTComb [5483], & yyTComb [5489], & yyTComb [5498], 
& yyTComb [3833], & yyTComb [5510], & yyTComb [5515], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [5784], & yyTComb [5583], & yyTComb [8260], 
& yyTComb [4305], & yyTComb [3494], & yyTComb [6660], & yyTComb [6672], 
& yyTComb [6665], & yyTComb [3886], & yyTComb [2008], & yyTComb [5700], 
& yyTComb [2913], & yyTComb [5534], & yyTComb [2627], & yyTComb [2245], 
& yyTComb [6256], & yyTComb [18995], & yyTComb [16522], & yyTComb [17441], 
& yyTComb [2889], & yyTComb [4476], & yyTComb [6693], & yyTComb [6530], 
& yyTComb [2240], & yyTComb [4405], & yyTComb [6001], & yyTComb [6425], 
& yyTComb [6011], & yyTComb [5808], & yyTComb [6346], & yyTComb [5932], 
& yyTComb [5761], & yyTComb [5619], & yyTComb [3200], & yyTComb [5795], 
& yyTComb [5787], & yyTComb [5739], & yyTComb [  23], & yyTComb [   0], 
& yyTComb [6367], & yyTComb [5736], & yyTComb [5763], & yyTComb [   0], 
& yyTComb [5759], & yyTComb [2355], & yyTComb [6365], & yyTComb [6621], 
& yyTComb [4390], & yyTComb [5588], & yyTComb [5316], & yyTComb [2608], 
& yyTComb [4527], & yyTComb [3188], & yyTComb [15950], & yyTComb [15092], 
& yyTComb [6011], & yyTComb [4462], & yyTComb [6020], & yyTComb [6681], 
& yyTComb [5436], & yyTComb [2583], & yyTComb [2297], & yyTComb [4089], 
& yyTComb [3381], & yyTComb [6287], & yyTComb [1047], & yyTComb [ 774], 
& yyTComb [6562], & yyTComb [6525], & yyTComb [ 135], & yyTComb [6517], 
& yyTComb [19092], & yyTComb [ 419], & yyTComb [6575], & yyTComb [6576], 
& yyTComb [6516], & yyTComb [6509], & yyTComb [1013], & yyTComb [4673], 
& yyTComb [5722], & yyTComb [20106], & yyTComb [4560], & yyTComb [6682], 
& yyTComb [6692], & yyTComb [6691], & yyTComb [6690], & yyTComb [3395], 
& yyTComb [18077], & yyTComb [ 226], & yyTComb [17397], & yyTComb [16848], 
& yyTComb [16820], & yyTComb [6004], & yyTComb [6353], & yyTComb [   0], 
& yyTComb [5637], & yyTComb [5801], & yyTComb [6356], & yyTComb [6006], 
& yyTComb [6410], & yyTComb [6079], & yyTComb [5758], & yyTComb [3266], 
& yyTComb [1926], & yyTComb [5543], & yyTComb [5757], & yyTComb [3434], 
& yyTComb [6023], & yyTComb [5600], & yyTComb [   0], & yyTComb [  87], 
& yyTComb [   0], & yyTComb [5496], & yyTComb [5487], & yyTComb [5737], 
& yyTComb [6166], & yyTComb [3962], & yyTComb [3930], & yyTComb [5336], 
& yyTComb [4567], & yyTComb [1153], & yyTComb [ 267], & yyTComb [1771], 
& yyTComb [6391], & yyTComb [6379], & yyTComb [14270], & yyTComb [  54], 
& yyTComb [16898], & yyTComb [4096], & yyTComb [1918], & yyTComb [6328], 
& yyTComb [6320], & yyTComb [6607], & yyTComb [6515], & yyTComb [6514], 
& yyTComb [6580], & yyTComb [6508], & yyTComb [6507], & yyTComb [2103], 
& yyTComb [3113], & yyTComb [2709], & yyTComb [19457], & yyTComb [ 330], 
& yyTComb [1389], & yyTComb [19146], & yyTComb [18353], & yyTComb [1320], 
& yyTComb [1291], & yyTComb [4583], & yyTComb [4587], & yyTComb [2936], 
& yyTComb [6696], & yyTComb [6697], & yyTComb [5544], & yyTComb [1877], 
& yyTComb [5596], & yyTComb [5796], & yyTComb [2997], & yyTComb [6412], 
& yyTComb [6638], & yyTComb [   0], & yyTComb [5597], & yyTComb [6411], 
& yyTComb [6006], & yyTComb [   0], & yyTComb [ 245], & yyTComb [4622], 
& yyTComb [2035], & yyTComb [5603], & yyTComb [5607], & yyTComb [3218], 
& yyTComb [5611], & yyTComb [1436], & yyTComb [ 434], & yyTComb [ 972], 
& yyTComb [ 510], & yyTComb [ 204], & yyTComb [  22], & yyTComb [5632], 
& yyTComb [6068], & yyTComb [5752], & yyTComb [1174], & yyTComb [4710], 
& yyTComb [6378], & yyTComb [20287], & yyTComb [5640], & yyTComb [4565], 
& yyTComb [13749], & yyTComb [15390], & yyTComb [   0], & yyTComb [1525], 
& yyTComb [6654], & yyTComb [4554], & yyTComb [4550], & yyTComb [6661], 
& yyTComb [6377], & yyTComb [4237], & yyTComb [3672], & yyTComb [5610], 
& yyTComb [1058], & yyTComb [3232], & yyTComb [1403], & yyTComb [6565], 
& yyTComb [6497], & yyTComb [6495], & yyTComb [6606], & yyTComb [6490], 
& yyTComb [6488], & yyTComb [6001], & yyTComb [6000], & yyTComb [6335], 
& yyTComb [6337], & yyTComb [6585], & yyTComb [6486], & yyTComb [6483], 
& yyTComb [1195], & yyTComb [6583], & yyTComb [6582], & yyTComb [6475], 
& yyTComb [6470], & yyTComb [6299], & yyTComb [6561], & yyTComb [6502], 
& yyTComb [6503], & yyTComb [ 566], & yyTComb [6596], & yyTComb [6568], 
& yyTComb [6473], & yyTComb [6474], & yyTComb [4653], & yyTComb [  58], 
& yyTComb [5725], & yyTComb [5666], & yyTComb [6362], & yyTComb [ 685], 
& yyTComb [  59], & yyTComb [  28], & yyTComb [5668], & yyTComb [5673], 
& yyTComb [6413], & yyTComb [6200], & yyTComb [ 259], & yyTComb [5675], 
& yyTComb [5684], & yyTComb [6069], & yyTComb [   0], & yyTComb [ 356], 
& yyTComb [  38], & yyTComb [1096], & yyTComb [6708], & yyTComb [3168], 
& yyTComb [6384], & yyTComb [6418], & yyTComb [8238], & yyTComb [3883], 
& yyTComb [3140], & yyTComb [4286], & yyTComb [6415], & yyTComb [6115], 
& yyTComb [6157], & yyTComb [2799], & yyTComb [5587], & yyTComb [4471], 
& yyTComb [ 741], & yyTComb [   0], & yyTComb [2256], & yyTComb [5739], 
& yyTComb [2887], & yyTComb [4263], & yyTComb [4378], & yyTComb [6129], 
& yyTComb [2217], & yyTComb [6207], & yyTComb [3016], & yyTComb [2671], 
& yyTComb [6286], & yyTComb [2750], & yyTComb [3795], & yyTComb [3801], 
& yyTComb [ 475], & yyTComb [1659], & yyTComb [8540], & yyTComb [ 926], 
& yyTComb [5741], & yyTComb [4310], & yyTComb [4297], & yyTComb [3276], 
& yyTComb [6271], & yyTComb [5213], & yyTComb [5230], & yyTComb [6677], 
& yyTComb [4165], & yyTComb [4216], & yyTComb [5648], & yyTComb [ 197], 
& yyTComb [ 986], & yyTComb [6528], & yyTComb [6677], & yyTComb [ 885], 
& yyTComb [ 854], & yyTComb [3165], & yyTComb [6248], & yyTComb [6077], 
& yyTComb [6385], & yyTComb [6412], & yyTComb [6233], & yyTComb [6064], 
& yyTComb [3926], & yyTComb [2509], & yyTComb [ 485], & yyTComb [3878], 
& yyTComb [6413], & yyTComb [6163], & yyTComb [ 259], & yyTComb [6178], 
& yyTComb [7829], & yyTComb [7901], & yyTComb [5733], & yyTComb [3728], 
& yyTComb [6077], & yyTComb [6078], & yyTComb [5706], & yyTComb [ 405], 
& yyTComb [ 826], & yyTComb [6600], & yyTComb [6586], & yyTComb [6319], 
& yyTComb [6313], & yyTComb [6563], & yyTComb [6518], & yyTComb [6506], 
& yyTComb [6554], & yyTComb [6519], & yyTComb [6522], & yyTComb [3864], 
& yyTComb [6315], & yyTComb [6331], & yyTComb [6542], & yyTComb [6527], 
& yyTComb [6526], & yyTComb [6551], & yyTComb [6521], & yyTComb [6520], 
& yyTComb [18729], & yyTComb [   8], & yyTComb [5381], & yyTComb [4449], 
& yyTComb [ 605], & yyTComb [1377], & yyTComb [ 822], & yyTComb [1869], 
& yyTComb [5662], & yyTComb [6406], & yyTComb [6200], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [5643], & yyTComb [5664], 
& yyTComb [2458], & yyTComb [5592], & yyTComb [5644], & yyTComb [5656], 
& yyTComb [2457], & yyTComb [19790], & yyTComb [4850], & yyTComb [5748], 
& yyTComb [4470], & yyTComb [5747], & yyTComb [4362], & yyTComb [5591], 
& yyTComb [4562], & yyTComb [  16], & yyTComb [13710], & yyTComb [3708], 
& yyTComb [6381], & yyTComb [1460], & yyTComb [1152], & yyTComb [3768], 
& yyTComb [3794], & yyTComb [6860], & yyTComb [4652], & yyTComb [2472], 
& yyTComb [2704], & yyTComb [  86], & yyTComb [2479], & yyTComb [5869], 
& yyTComb [6389], & yyTComb [6619], & yyTComb [1364], & yyTComb [3307], 
& yyTComb [3737], & yyTComb [3572], & yyTComb [5025], & yyTComb [5098], 
& yyTComb [5218], & yyTComb [5224], & yyTComb [3784], & yyTComb [1480], 
& yyTComb [6674], & yyTComb [6580], & yyTComb [ 118], & yyTComb [1750], 
& yyTComb [ 955], & yyTComb [2403], & yyTComb [6750], & yyTComb [6620], 
& yyTComb [6006], & yyTComb [5683], & yyTComb [3782], & yyTComb [5086], 
& yyTComb [5365], & yyTComb [4285], & yyTComb [5612], & yyTComb [  74], 
& yyTComb [14309], & yyTComb [5598], & yyTComb [4509], & yyTComb [6154], 
& yyTComb [5748], & yyTComb [6148], & yyTComb [6147], & yyTComb [5590], 
& yyTComb [4547], & yyTComb [3968], & yyTComb [2984], & yyTComb [5743], 
& yyTComb [ 160], & yyTComb [2083], & yyTComb [15485], & yyTComb [ 772], 
& yyTComb [5426], & yyTComb [6668], & yyTComb [5989], & yyTComb [5996], 
& yyTComb [5998], & yyTComb [5999], & yyTComb [4668], & yyTComb [5646], 
& yyTComb [5391], & yyTComb [5456], & yyTComb [5593], & yyTComb [1880], 
& yyTComb [5782], & yyTComb [   0], & yyTComb [5701], & yyTComb [6402], 
& yyTComb [5746], & yyTComb [ 648], & yyTComb [1329], & yyTComb [5756], 
& yyTComb [3321], & yyTComb [5691], & yyTComb [5638], & yyTComb [5617], 
& yyTComb [5754], & yyTComb [4370], & yyTComb [4147], & yyTComb [ 152], 
& yyTComb [6706], & yyTComb [6149], & yyTComb [5780], & yyTComb [6368], 
& yyTComb [20081], & yyTComb [4331], & yyTComb [2832], & yyTComb [6182], 
& yyTComb [6184], & yyTComb [5785], & yyTComb [6203], & yyTComb [5686], 
& yyTComb [ 553], & yyTComb [3809], & yyTComb [5690], & yyTComb [1183], 
& yyTComb [3819], & yyTComb [3242], & yyTComb [6022], & yyTComb [ 940], 
& yyTComb [  38], & yyTComb [2632], & yyTComb [2377], & yyTComb [1801], 
& yyTComb [4668], & yyTComb [6069], & yyTComb [2988], & yyTComb [ 893], 
& yyTComb [ 897], & yyTComb [6146], & yyTComb [6140], & yyTComb [1368], 
& yyTComb [1543], & yyTComb [1767], & yyTComb [   0], & yyTComb [3229], 
& yyTComb [2495], & yyTComb [2524], & yyTComb [2318], & yyTComb [ 574], 
& yyTComb [4682], & yyTComb [6070], & yyTComb [6111], & yyTComb [6361], 
& yyTComb [4556], & yyTComb [8741], & yyTComb [1013], & yyTComb [ 907], 
& yyTComb [1644], & yyTComb [5029], & yyTComb [3811], & yyTComb [3773], 
& yyTComb [3686], & yyTComb [3677], & yyTComb [3645], & yyTComb [5715], 
& yyTComb [5711], & yyTComb [6581], & yyTComb [6587], & yyTComb [6593], 
& yyTComb [6610], & yyTComb [ 920], & yyTComb [1058], & yyTComb [5322], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [4886], & yyTComb [6460], 
& yyTComb [6955], & yyTComb [1048], & yyTComb [3410], & yyTComb [5599], 
& yyTComb [5207], & yyTComb [5211], & yyTComb [2170], & yyTComb [2599], 
& yyTComb [7365], & yyTComb [8980], & yyTComb [3063], & yyTComb [5227], 
& yyTComb [3446], & yyTComb [17950], & yyTComb [3920], & yyTComb [4085], 
& yyTComb [1914], & yyTComb [5237], & yyTComb [6601], & yyTComb [4455], 
& yyTComb [3115], & yyTComb [5246], & yyTComb [5247], & yyTComb [3938], 
& yyTComb [5249], & yyTComb [4845], & yyTComb [11220], & yyTComb [6410], 
& yyTComb [8140], & yyTComb [ 974], & yyTComb [   0], & yyTComb [5262], 
& yyTComb [5267], & yyTComb [6621], & yyTComb [5270], & yyTComb [5275], 
& yyTComb [6620], & yyTComb [5280], & yyTComb [5285], & yyTComb [1087], 
& yyTComb [5290], & yyTComb [1188], & yyTComb [ 427], & yyTComb [5302], 
& yyTComb [5309], & yyTComb [3892], & yyTComb [5281], & yyTComb [4780], 
& yyTComb [6342], & yyTComb [5317], & yyTComb [5318], & yyTComb [5320], 
& yyTComb [5321], & yyTComb [5328], & yyTComb [4812], & yyTComb [1504], 
& yyTComb [5903], & yyTComb [3115], & yyTComb [2218], & yyTComb [5659], 
& yyTComb [2511], & yyTComb [4338], & yyTComb [1128], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [5613], & yyTComb [6401], & yyTComb [6393], 
& yyTComb [5609], & yyTComb [1614], & yyTComb [6313], & yyTComb [5507], 
& yyTComb [ 115], & yyTComb [4671], & yyTComb [2089], & yyTComb [6363], 
& yyTComb [6357], & yyTComb [6710], & yyTComb [6531], & yyTComb [4275], 
& yyTComb [ 886], & yyTComb [   0], & yyTComb [4258], & yyTComb [4200], 
& yyTComb [19809], & yyTComb [19510], & yyTComb [5734], & yyTComb [6650], 
& yyTComb [4141], & yyTComb [4326], & yyTComb [6255], & yyTComb [5575], 
& yyTComb [4329], & yyTComb [4354], & yyTComb [4355], & yyTComb [4372], 
& yyTComb [6187], & yyTComb [4626], & yyTComb [6189], & yyTComb [6201], 
& yyTComb [6470], & yyTComb [1376], & yyTComb [6206], & yyTComb [3145], 
& yyTComb [6205], & yyTComb [6202], & yyTComb [5768], & yyTComb [ 450], 
& yyTComb [3514], & yyTComb [8109], & yyTComb [3837], & yyTComb [5466], 
& yyTComb [5437], & yyTComb [5419], & yyTComb [5393], & yyTComb [4760], 
& yyTComb [4189], & yyTComb [2404], & yyTComb [5829], & yyTComb [2976], 
& yyTComb [3274], & yyTComb [6019], & yyTComb [7420], & yyTComb [6447], 
& yyTComb [15429], & yyTComb [14869], & yyTComb [ 961], & yyTComb [7118], 
& yyTComb [5088], & yyTComb [5173], & yyTComb [5349], & yyTComb [5560], 
& yyTComb [6066], & yyTComb [6445], & yyTComb [13245], & yyTComb [   0], 
& yyTComb [6006], & yyTComb [6349], & yyTComb [7140], & yyTComb [3981], 
& yyTComb [6344], & yyTComb [1991], & yyTComb [3415], & yyTComb [4551], 
& yyTComb [3645], & yyTComb [3816], & yyTComb [6080], & yyTComb [6082], 
& yyTComb [3534], & yyTComb [6615], & yyTComb [6408], & yyTComb [6186], 
& yyTComb [1197], & yyTComb [5312], & yyTComb [3835], & yyTComb [3940], 
& yyTComb [4155], & yyTComb [6579], & yyTComb [3929], & yyTComb [ 693], 
& yyTComb [6616], & yyTComb [1104], & yyTComb [1208], & yyTComb [5504], 
& yyTComb [ 322], & yyTComb [6385], & yyTComb [3387], & yyTComb [5590], 
& yyTComb [6360], & yyTComb [6471], & yyTComb [4590], & yyTComb [5565], 
& yyTComb [3449], & yyTComb [6272], & yyTComb [3725], & yyTComb [  37], 
& yyTComb [2669], & yyTComb [3819], & yyTComb [5529], & yyTComb [6395], 
& yyTComb [6617], & yyTComb [6618], & yyTComb [4782], & yyTComb [2988], 
& yyTComb [5430], & yyTComb [6464], & yyTComb [6215], & yyTComb [11780], 
& yyTComb [5783], & yyTComb [4363], & yyTComb [6721], & yyTComb [12629], 
& yyTComb [4427], & yyTComb [2579], & yyTComb [5357], & yyTComb [5355], 
& yyTComb [6465], & yyTComb [   0], & yyTComb [1378], & yyTComb [6595], 
& yyTComb [5722], & yyTComb [5723], & yyTComb [5277], & yyTComb [5256], 
& yyTComb [5248], & yyTComb [5235], & yyTComb [1439], & yyTComb [ 561], 
& yyTComb [ 186], & yyTComb [6740], & yyTComb [2033], & yyTComb [5150], 
& yyTComb [1252], & yyTComb [11005], & yyTComb [13740], & yyTComb [17601], 
& yyTComb [19412], & yyTComb [9820], & yyTComb [5057], & yyTComb [5054], 
& yyTComb [5048], & yyTComb [5044], & yyTComb [ 128], & yyTComb [4906], 
& yyTComb [2087], & yyTComb [4220], & yyTComb [2933], & yyTComb [2853], 
& yyTComb [5933], & yyTComb [13768], & yyTComb [1729], & yyTComb [4477], 
& yyTComb [6536], & yyTComb [6526], & yyTComb [6541], & yyTComb [6470], 
& yyTComb [2394], & yyTComb [4275], & yyTComb [5330], & yyTComb [2174], 
& yyTComb [6022], & yyTComb [5258], & yyTComb [1253], & yyTComb [1202], 
& yyTComb [3881], & yyTComb [6297], & yyTComb [ 166], & yyTComb [   0], 
& yyTComb [6204], & yyTComb [2059], & yyTComb [6679], & yyTComb [5319], 
& yyTComb [3163], & yyTComb [2352], & yyTComb [7020], & yyTComb [5348], 
& yyTComb [ 205], & yyTComb [4312], & yyTComb [3776], & yyTComb [6662], 
& yyTComb [6043], & yyTComb [4854], & yyTComb [5635], & yyTComb [2791], 
& yyTComb [5634], & yyTComb [6020], & yyTComb [6212], & yyTComb [ 106], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [5699], & yyTComb [6407], 
& yyTComb [6402], & yyTComb [ 276], & yyTComb [5538], & yyTComb [5739], 
& yyTComb [6432], & yyTComb [ 565], & yyTComb [5782], & yyTComb [1707], 
& yyTComb [5592], & yyTComb [5785], & yyTComb [1619], & yyTComb [1802], 
& yyTComb [2721], & yyTComb [  67], & yyTComb [2246], & yyTComb [5164], 
& yyTComb [3813], & yyTComb [19478], & yyTComb [20000], & yyTComb [12463], 
& yyTComb [3755], & yyTComb [5404], & yyTComb [6221], & yyTComb [5447], 
& yyTComb [5451], & yyTComb [5467], & yyTComb [5527], & yyTComb [4271], 
& yyTComb [4263], & yyTComb [4203], & yyTComb [4131], & yyTComb [4306], 
& yyTComb [4270], & yyTComb [3605], & yyTComb [4116], & yyTComb [4273], 
& yyTComb [4015], & yyTComb [4107], & yyTComb [4186], & yyTComb [4204], 
& yyTComb [4234], & yyTComb [4248], & yyTComb [4249], & yyTComb [4251], 
& yyTComb [4268], & yyTComb [3676], & yyTComb [2879], & yyTComb [3932], 
& yyTComb [3219], & yyTComb [4758], & yyTComb [2957], & yyTComb [1929], 
& yyTComb [3278], & yyTComb [4787], & yyTComb [3285], & yyTComb [ 318], 
& yyTComb [ 177], & yyTComb [5721], & yyTComb [5748], & yyTComb [3465], 
& yyTComb [5786], & yyTComb [7438], & yyTComb [7509], & yyTComb [5794], 
& yyTComb [4315], & yyTComb [5751], & yyTComb [4470], & yyTComb [17405], 
& yyTComb [15652], & yyTComb [6348], & yyTComb [6216], & yyTComb [ 538], 
& yyTComb [1834], & yyTComb [5783], & yyTComb [6614], & yyTComb [  16], 
& yyTComb [ 752], & yyTComb [5688], & yyTComb [8181], & yyTComb [1473], 
& yyTComb [11248], & yyTComb [  29], & yyTComb [3918], & yyTComb [1937], 
& yyTComb [3422], & yyTComb [   0], & yyTComb [5818], & yyTComb [5826], 
& yyTComb [6045], & yyTComb [6202], & yyTComb [6637], & yyTComb [2761], 
& yyTComb [1588], & yyTComb [4032], & yyTComb [6558], & yyTComb [5528], 
& yyTComb [5522], & yyTComb [ 878], & yyTComb [6825], & yyTComb [1738], 
& yyTComb [4393], & yyTComb [1905], & yyTComb [1441], & yyTComb [2007], 
& yyTComb [16268], & yyTComb [5416], & yyTComb [1836], & yyTComb [4607], 
& yyTComb [3937], & yyTComb [2456], & yyTComb [4578], & yyTComb [   0], 
& yyTComb [1950], & yyTComb [2660], & yyTComb [1619], & yyTComb [   0], 
& yyTComb [2531], & yyTComb [1166], & yyTComb [10100], & yyTComb [5289], 
& yyTComb [4616], & yyTComb [1681], & yyTComb [2196], & yyTComb [17897], 
& yyTComb [17494], & yyTComb [6624], & yyTComb [5928], & yyTComb [4369], 
& yyTComb [3159], & yyTComb [5060], & yyTComb [3370], & yyTComb [  83], 
& yyTComb [6282], & yyTComb [6689], & yyTComb [6449], & yyTComb [ 646], 
& yyTComb [4248], & yyTComb [  26], & yyTComb [3019], & yyTComb [5169], 
& yyTComb [ 683], & yyTComb [6543], & yyTComb [5124], & yyTComb [5118], 
& yyTComb [6003], & yyTComb [6618], & yyTComb [5101], & yyTComb [4605], 
& yyTComb [5092], & yyTComb [5084], & yyTComb [ 891], & yyTComb [5056], 
& yyTComb [2048], & yyTComb [7549], & yyTComb [1314], & yyTComb [5036], 
& yyTComb [3746], & yyTComb [2742], & yyTComb [6621], & yyTComb [3463], 
& yyTComb [4974], & yyTComb [6364], & yyTComb [5125], & yyTComb [4005], 
& yyTComb [1838], & yyTComb [4771], & yyTComb [4482], & yyTComb [5749], 
& yyTComb [1551], & yyTComb [2529], & yyTComb [2374], & yyTComb [6030], 
& yyTComb [3078], & yyTComb [2635], & yyTComb [6199], & yyTComb [ 468], 
& yyTComb [3789], & yyTComb [4500], & yyTComb [2507], & yyTComb [6027], 
& yyTComb [4457], & yyTComb [6542], & yyTComb [18006], & yyTComb [1281], 
& yyTComb [6208], & yyTComb [6772], & yyTComb [   8], & yyTComb [6362], 
& yyTComb [6361], & yyTComb [6085], & yyTComb [6081], & yyTComb [6023], 
& yyTComb [ 872], & yyTComb [12685], & yyTComb [1590], & yyTComb [3425], 
& yyTComb [5103], & yyTComb [3399], & yyTComb [3866], & yyTComb [3877], 
& yyTComb [5254], & yyTComb [5735], & yyTComb [1347], & yyTComb [5729], 
& yyTComb [  13], & yyTComb [2046], & yyTComb [2087], & yyTComb [5570], 
& yyTComb [5741], & yyTComb [6031], & yyTComb [5516], & yyTComb [6033], 
& yyTComb [5739], & yyTComb [6096], & yyTComb [18479], & yyTComb [ 202], 
& yyTComb [5601], & yyTComb [ 338], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [5591], & yyTComb [6405], & yyTComb [1404], & yyTComb [6005], 
& yyTComb [3925], & yyTComb [2183], & yyTComb [5749], & yyTComb [   0], 
& yyTComb [6558], & yyTComb [4527], & yyTComb [4499], & yyTComb [6203], 
& yyTComb [ 142], & yyTComb [5480], & yyTComb [6878], & yyTComb [5568], 
& yyTComb [2276], & yyTComb [6423], & yyTComb [2090], & yyTComb [6424], 
& yyTComb [2256], & yyTComb [2677], & yyTComb [4261], & yyTComb [5715], 
& yyTComb [6461], & yyTComb [5753], & yyTComb [6695], & yyTComb [4378], 
& yyTComb [  89], & yyTComb [4177], & yyTComb [   0], & yyTComb [4246], 
& yyTComb [4243], & yyTComb [5532], & yyTComb [6703], & yyTComb [3230], 
& yyTComb [6532], & yyTComb [5536], & yyTComb [5257], & yyTComb [5453], 
& yyTComb [5567], & yyTComb [5550], & yyTComb [5531], & yyTComb [3545], 
& yyTComb [5518], & yyTComb [5475], & yyTComb [5446], & yyTComb [5444], 
& yyTComb [5422], & yyTComb [5406], & yyTComb [5400], & yyTComb [5390], 
& yyTComb [5385], & yyTComb [5383], & yyTComb [5364], & yyTComb [3699], 
& yyTComb [3269], & yyTComb [4763], & yyTComb [3199], & yyTComb [4778], 
& yyTComb [3266], & yyTComb [ 792], & yyTComb [3303], & yyTComb [4783], 
& yyTComb [3193], & yyTComb [5716], & yyTComb [4749], & yyTComb [6597], 
& yyTComb [8669], & yyTComb [6195], & yyTComb [7595], & yyTComb [6599], 
& yyTComb [6345], & yyTComb [5694], & yyTComb [ 376], & yyTComb [6442], 
& yyTComb [2976], & yyTComb [5783], & yyTComb [5719], & yyTComb [3186], 
& yyTComb [6681], & yyTComb [6226], & yyTComb [6202], & yyTComb [6461], 
& yyTComb [6250], & yyTComb [6215], & yyTComb [1873], & yyTComb [5827], 
& yyTComb [5716], & yyTComb [1595], & yyTComb [16288], & yyTComb [ 623], 
& yyTComb [5785], & yyTComb [6208], & yyTComb [3952], & yyTComb [6636], 
& yyTComb [   0], & yyTComb [3085], & yyTComb [5822], & yyTComb [6245], 
& yyTComb [6244], & yyTComb [6239], & yyTComb [5772], & yyTComb [5363], 
& yyTComb [1280], & yyTComb [8700], & yyTComb [2717], & yyTComb [6567], 
& yyTComb [6265], & yyTComb [1757], & yyTComb [8485], & yyTComb [6444], 
& yyTComb [1592], & yyTComb [1678], & yyTComb [  16], & yyTComb [9045], 
& yyTComb [  53], & yyTComb [2520], & yyTComb [6378], & yyTComb [3245], 
& yyTComb [6386], & yyTComb [2265], & yyTComb [   0], & yyTComb [6192], 
& yyTComb [6105], & yyTComb [5872], & yyTComb [6188], & yyTComb [1671], 
& yyTComb [6217], & yyTComb [3172], & yyTComb [6246], & yyTComb [6249], 
& yyTComb [6238], & yyTComb [6047], & yyTComb [10660], & yyTComb [ 725], 
& yyTComb [6186], & yyTComb [2358], & yyTComb [3884], & yyTComb [4393], 
& yyTComb [3143], & yyTComb [5722], & yyTComb [4430], & yyTComb [17094], 
& yyTComb [5897], & yyTComb [5088], & yyTComb [17550], & yyTComb [2267], 
& yyTComb [3123], & yyTComb [6009], & yyTComb [14925], & yyTComb [1988], 
& yyTComb [5137], & yyTComb [6158], & yyTComb [4528], & yyTComb [  44], 
& yyTComb [1217], & yyTComb [4530], & yyTComb [ 335], & yyTComb [5495], 
& yyTComb [5350], & yyTComb [6619], & yyTComb [6422], & yyTComb [1106], 
& yyTComb [5229], & yyTComb [5228], & yyTComb [12340], & yyTComb [1367], 
& yyTComb [12060], & yyTComb [8420], & yyTComb [14608], & yyTComb [6203], 
& yyTComb [3907], & yyTComb [3789], & yyTComb [7229], & yyTComb [ 533], 
& yyTComb [5816], & yyTComb [5815], & yyTComb [6042], & yyTComb [6205], 
& yyTComb [6627], & yyTComb [3123], & yyTComb [3769], & yyTComb [6560], 
& yyTComb [1448], & yyTComb [2104], & yyTComb [4461], & yyTComb [15988], 
& yyTComb [5052], & yyTComb [ 762], & yyTComb [4597], & yyTComb [7860], 
& yyTComb [5209], & yyTComb [4637], & yyTComb [2376], & yyTComb [5236], 
& yyTComb [6354], & yyTComb [6355], & yyTComb [6088], & yyTComb [6089], 
& yyTComb [ 978], & yyTComb [ 587], & yyTComb [3250], & yyTComb [3306], 
& yyTComb [2134], & yyTComb [5397], & yyTComb [6041], & yyTComb [5427], 
& yyTComb [6091], & yyTComb [ 221], & yyTComb [18273], & yyTComb [5454], 
& yyTComb [6488], & yyTComb [3408], & yyTComb [3688], & yyTComb [6040], 
& yyTComb [6093], & yyTComb [6090], & yyTComb [5517], & yyTComb [1723], 
& yyTComb [5542], & yyTComb [3869], & yyTComb [3373], & yyTComb [   0], 
& yyTComb [5955], & yyTComb [6619], & yyTComb [5807], & yyTComb [6620], 
& yyTComb [3272], & yyTComb [3730], & yyTComb [3519], & yyTComb [6298], 
& yyTComb [5405], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [  71], & yyTComb [4442], & yyTComb [5801], 
& yyTComb [5251], & yyTComb [3248], & yyTComb [   0], & yyTComb [5959], 
& yyTComb [4530], & yyTComb [6671], & yyTComb [2164], & yyTComb [5287], 
& yyTComb [9260], & yyTComb [1725], & yyTComb [7085], & yyTComb [1747], 
& yyTComb [5097], & yyTComb [4475], & yyTComb [4585], & yyTComb [4281], 
& yyTComb [5701], & yyTComb [   0], & yyTComb [  65], & yyTComb [5979], 
& yyTComb [ 917], & yyTComb [5993], & yyTComb [5933], & yyTComb [ 944], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [5660], & yyTComb [4371], 
& yyTComb [4376], & yyTComb [4708], & yyTComb [4383], & yyTComb [2475], 
& yyTComb [5145], & yyTComb [2769], & yyTComb [   6], & yyTComb [   0], 
& yyTComb [2472], & yyTComb [1653], & yyTComb [6156], & yyTComb [5642], 
& yyTComb [6423], & yyTComb [5641], & yyTComb [3199], & yyTComb [3134], 
& yyTComb [6300], & yyTComb [6314], & yyTComb [6124], & yyTComb [6112], 
& yyTComb [6109], & yyTComb [5783], & yyTComb [6107], & yyTComb [5748], 
& yyTComb [1565], & yyTComb [5491], & yyTComb [5508], & yyTComb [5572], 
& yyTComb [4955], & yyTComb [  30], & yyTComb [5494], & yyTComb [5338], 
& yyTComb [5268], & yyTComb [5593], & yyTComb [3629], & yyTComb [6077], 
& yyTComb [2903], & yyTComb [3543], & yyTComb [3156], & yyTComb [3308], 
& yyTComb [4780], & yyTComb [1616], & yyTComb [3204], & yyTComb [8518], 
& yyTComb [5752], & yyTComb [5749], & yyTComb [1665], & yyTComb [ 158], 
& yyTComb [ 880], & yyTComb [6306], & yyTComb [6393], & yyTComb [5468], 
& yyTComb [4247], & yyTComb [4280], & yyTComb [5812], & yyTComb [6008], 
& yyTComb [12088], & yyTComb [6221], & yyTComb [6123], & yyTComb [6127], 
& yyTComb [6130], & yyTComb [5745], & yyTComb [6142], & yyTComb [5714], 
& yyTComb [6635], & yyTComb [5715], & yyTComb [2621], & yyTComb [ 283], 
& yyTComb [5833], & yyTComb [5635], & yyTComb [4989], & yyTComb [4429], 
& yyTComb [6386], & yyTComb [6652], & yyTComb [2778], & yyTComb [   0], 
& yyTComb [1389], & yyTComb [5853], & yyTComb [6428], & yyTComb [6054], 
& yyTComb [5850], & yyTComb [6545], & yyTComb [10725], & yyTComb [6133], 
& yyTComb [10445], & yyTComb [3612], & yyTComb [10165], & yyTComb [   0], 
& yyTComb [5860], & yyTComb [5864], & yyTComb [4147], & yyTComb [6266], 
& yyTComb [3767], & yyTComb [6384], & yyTComb [   0], & yyTComb [5871], 
& yyTComb [9568], & yyTComb [6185], & yyTComb [6805], & yyTComb [5872], 
& yyTComb [5711], & yyTComb [6168], & yyTComb [3198], & yyTComb [6151], 
& yyTComb [2914], & yyTComb [6164], & yyTComb [6167], & yyTComb [5180], 
& yyTComb [4900], & yyTComb [4709], & yyTComb [5176], & yyTComb [6045], 
& yyTComb [   0], & yyTComb [5896], & yyTComb [5685], & yyTComb [5314], 
& yyTComb [5332], & yyTComb [17198], & yyTComb [18826], & yyTComb [7048], 
& yyTComb [5723], & yyTComb [6020], & yyTComb [13412], & yyTComb [13972], 
& yyTComb [4006], & yyTComb [14365], & yyTComb [5902], & yyTComb [5723], 
& yyTComb [6609], & yyTComb [4250], & yyTComb [5718], & yyTComb [5303], 
& yyTComb [5754], & yyTComb [1389], & yyTComb [4539], & yyTComb [5421], 
& yyTComb [3269], & yyTComb [6200], & yyTComb [6191], & yyTComb [5492], 
& yyTComb [6063], & yyTComb [   0], & yyTComb [5914], & yyTComb [5801], 
& yyTComb [6387], & yyTComb [6385], & yyTComb [6682], & yyTComb [ 543], 
& yyTComb [5828], & yyTComb [6204], & yyTComb [6625], & yyTComb [1142], 
& yyTComb [5814], & yyTComb [5774], & yyTComb [5526], & yyTComb [2766], 
& yyTComb [10380], & yyTComb [2775], & yyTComb [1765], & yyTComb [ 939], 
& yyTComb [5871], & yyTComb [1024], & yyTComb [6064], & yyTComb [11956], 
& yyTComb [2342], & yyTComb [5901], & yyTComb [3968], & yyTComb [12620], 
& yyTComb [2969], & yyTComb [6087], & yyTComb [6082], & yyTComb [ 654], 
& yyTComb [5449], & yyTComb [ 495], & yyTComb [5956], & yyTComb [6622], 
& yyTComb [6625], & yyTComb [5958], & yyTComb [4594], & yyTComb [5432], 
& yyTComb [13180], & yyTComb [2476], & yyTComb [5425], & yyTComb [1138], 
& yyTComb [5989], & yyTComb [ 534], & yyTComb [5990], & yyTComb [6318], 
& yyTComb [5712], & yyTComb [5414], & yyTComb [10940], & yyTComb [2188], 
& yyTComb [1862], & yyTComb [4258], & yyTComb [5934], & yyTComb [2623], 
& yyTComb [1452], & yyTComb [2776], & yyTComb [6229], & yyTComb [5799], 
& yyTComb [20191], & yyTComb [9288], & yyTComb [18409], & yyTComb [4677], 
& yyTComb [3924], & yyTComb [5710], & yyTComb [6380], & yyTComb [6294], 
& yyTComb [3903], & yyTComb [5373], & yyTComb [5372], & yyTComb [5371], 
& yyTComb [5369], & yyTComb [6296], & yyTComb [4556], & yyTComb [6077], 
& yyTComb [4493], & yyTComb [6621], & yyTComb [ 149], & yyTComb [2175], 
& yyTComb [1836], & yyTComb [6594], & yyTComb [4144], & yyTComb [4756], 
& yyTComb [2708], & yyTComb [6343], & yyTComb [2804], & yyTComb [2794], 
& yyTComb [6042], & yyTComb [   0], & yyTComb [5974], & yyTComb [6611], 
& yyTComb [3784], & yyTComb [4258], & yyTComb [1597], & yyTComb [1323], 
& yyTComb [2568], & yyTComb [3590], & yyTComb [4228], & yyTComb [5340], 
& yyTComb [7300], & yyTComb [5925], & yyTComb [1948], & yyTComb [5981], 
& yyTComb [15168], & yyTComb [2067], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [5023], & yyTComb [5010], & yyTComb [5008], & yyTComb [4988], 
& yyTComb [  15], & yyTComb [6002], & yyTComb [4127], & yyTComb [4217], 
& yyTComb [4336], & yyTComb [1135], & yyTComb [1845], & yyTComb [6219], 
& yyTComb [1649], & yyTComb [6578], & yyTComb [6209], & yyTComb [6113], 
& yyTComb [5742], & yyTComb [3867], & yyTComb [ 809], & yyTComb [ 641], 
& yyTComb [3759], & yyTComb [6454], & yyTComb [1491], & yyTComb [6449], 
& yyTComb [7269], & yyTComb [1853], & yyTComb [4108], & yyTComb [4170], 
& yyTComb [1251], & yyTComb [5187], & yyTComb [2578], & yyTComb [5783], 
& yyTComb [2412], & yyTComb [4363], & yyTComb [3888], & yyTComb [2375], 
& yyTComb [4258], & yyTComb [   0], & yyTComb [5292], & yyTComb [5295], 
& yyTComb [6715], & yyTComb [6635], & yyTComb [3783], & yyTComb [1473], 
& yyTComb [5784], & yyTComb [5781], & yyTComb [6342], & yyTComb [2619], 
& yyTComb [1446], & yyTComb [1078], & yyTComb [3995], & yyTComb [6007], 
& yyTComb [6001], & yyTComb [16008], & yyTComb [2077], & yyTComb [6746], 
& yyTComb [9325], & yyTComb [7341], & yyTComb [6181], & yyTComb [4620], 
& yyTComb [4508], & yyTComb [3119], & yyTComb [5820], & yyTComb [ 299], 
& yyTComb [2641], & yyTComb [5854], & yyTComb [6443], & yyTComb [5999], 
& yyTComb [5582], & yyTComb [11285], & yyTComb [13150], & yyTComb [7925], 
& yyTComb [6269], & yyTComb [8205], & yyTComb [5965], & yyTComb [12648], 
& yyTComb [3576], & yyTComb [6223], & yyTComb [12368], & yyTComb [7700], 
& yyTComb [11845], & yyTComb [7158], & yyTComb [5104], & yyTComb [ 800], 
& yyTComb [6375], & yyTComb [3355], & yyTComb [5337], & yyTComb [ 125], 
& yyTComb [6333], & yyTComb [5747], & yyTComb [6387], & yyTComb [9605], 
& yyTComb [4517], & yyTComb [4277], & yyTComb [5720], & yyTComb [4557], 
& yyTComb [3997], & yyTComb [5863], & yyTComb [1509], & yyTComb [1411], 
& yyTComb [4668], & yyTComb [5142], & yyTComb [5548], & yyTComb [5378], 
& yyTComb [8168], & yyTComb [2072], & yyTComb [5829], & yyTComb [6629], 
& yyTComb [3117], & yyTComb [5808], & yyTComb [ 679], & yyTComb [5834], 
& yyTComb [6067], & yyTComb [5842], & yyTComb [5857], & yyTComb [5859], 
& yyTComb [5869], & yyTComb [5874], & yyTComb [2113], & yyTComb [6065], 
& yyTComb [5890], & yyTComb [5901], & yyTComb [6052], & yyTComb [5906], 
& yyTComb [1861], & yyTComb [1467], & yyTComb [4175], & yyTComb [5935], 
& yyTComb [ 134], & yyTComb [2827], & yyTComb [4150], & yyTComb [19972], 
& yyTComb [4652], & yyTComb [1768], & yyTComb [2951], & yyTComb [6043], 
& yyTComb [5970], & yyTComb [3281], & yyTComb [4788], & yyTComb [5982], 
& yyTComb [5738], & yyTComb [1504], & yyTComb [ 987], & yyTComb [5375], 
& yyTComb [   0], & yyTComb [3588], & yyTComb [5948], & yyTComb [5801], 
& yyTComb [6209], & yyTComb [6207], & yyTComb [6206], & yyTComb [7328], 
& yyTComb [5786], & yyTComb [5788], & yyTComb [5951], & yyTComb [3646], 
& yyTComb [5952], & yyTComb [10128], & yyTComb [2985], & yyTComb [5648], 
& yyTComb [ 963], & yyTComb [5368], & yyTComb [3631], & yyTComb [6199], 
& yyTComb [3931], & yyTComb [4760], & yyTComb [4767], & yyTComb [3908], 
& yyTComb [6351], & yyTComb [5194], & yyTComb [4217], & yyTComb [6338], 
& yyTComb [6427], & yyTComb [4301], & yyTComb [4169], & yyTComb [4745], 
& yyTComb [4755], & yyTComb [4692], & yyTComb [4725], & yyTComb [   0], 
& yyTComb [4277], & yyTComb [5961], & yyTComb [6343], & yyTComb [5399], 
& yyTComb [4579], & yyTComb [5969], & yyTComb [5243], & yyTComb [6245], 
& yyTComb [4743], & yyTComb [6459], & yyTComb [3714], & yyTComb [ 850], 
& yyTComb [6200], & yyTComb [3875], & yyTComb [16568], & yyTComb [6228], 
& yyTComb [   0], & yyTComb [5696], & yyTComb [ 475], & yyTComb [   0], 
& yyTComb [  22], & yyTComb [  54], & yyTComb [4396], & yyTComb [ 112], 
& yyTComb [2422], & yyTComb [1369], & yyTComb [6369], & yyTComb [4145], 
& yyTComb [4047], & yyTComb [4986], & yyTComb [2650], & yyTComb [3081], 
& yyTComb [1671], & yyTComb [  15], & yyTComb [ 119], & yyTComb [6139], 
& yyTComb [4995], & yyTComb [6150], & yyTComb [4373], & yyTComb [4334], 
& yyTComb [ 110], & yyTComb [ 486], & yyTComb [4312], & yyTComb [6425], 
& yyTComb [4278], & yyTComb [4687], & yyTComb [5038], & yyTComb [5069], 
& yyTComb [6054], & yyTComb [4574], & yyTComb [6683], & yyTComb [6529], 
& yyTComb [1974], & yyTComb [4033], & yyTComb [5782], & yyTComb [6417], 
& yyTComb [4670], & yyTComb [6949], & yyTComb [ 360], & yyTComb [3800], 
& yyTComb [4115], & yyTComb [6300], & yyTComb [6205], & yyTComb [6350], 
& yyTComb [4160], & yyTComb [1696], & yyTComb [   8], & yyTComb [ 249], 
& yyTComb [13208], & yyTComb [12928], & yyTComb [6233], & yyTComb [  53], 
& yyTComb [13805], & yyTComb [5819], & yyTComb [6234], & yyTComb [6236], 
& yyTComb [3207], & yyTComb [6000], & yyTComb [8728], & yyTComb [2835], 
& yyTComb [12125], & yyTComb [6096], & yyTComb [5882], & yyTComb [5469], 
& yyTComb [1534], & yyTComb [6378], & yyTComb [14532], & yyTComb [5746], 
& yyTComb [ 143], & yyTComb [4837], & yyTComb [3435], & yyTComb [1390], 
& yyTComb [5524], & yyTComb [6355], & yyTComb [5546], & yyTComb [5569], 
& yyTComb [2162], & yyTComb [2527], & yyTComb [5810], & yyTComb [5847], 
& yyTComb [5417], & yyTComb [2446], & yyTComb [6376], & yyTComb [5374], 
& yyTComb [5366], & yyTComb [2571], & yyTComb [1914], & yyTComb [4932], 
& yyTComb [5946], & yyTComb [5949], & yyTComb [5951], & yyTComb [4808], 
& yyTComb [5959], & yyTComb [5967], & yyTComb [5589], & yyTComb [6471], 
& yyTComb [6199], & yyTComb [9540], & yyTComb [5937], & yyTComb [5947], 
& yyTComb [ 852], & yyTComb [5805], & yyTComb [7608], & yyTComb [7888], 
& yyTComb [8448], & yyTComb [5804], & yyTComb [17870], & yyTComb [4710], 
& yyTComb [16567], & yyTComb [5958], & yyTComb [6376], & yyTComb [4587], 
& yyTComb [4223], & yyTComb [6169], & yyTComb [1214], & yyTComb [5223], 
& yyTComb [6525], & yyTComb [5990], & yyTComb [5997], & yyTComb [6337], 
& yyTComb [10688], & yyTComb [2484], & yyTComb [  32], & yyTComb [3902], 
& yyTComb [2449], & yyTComb [5368], & yyTComb [5359], & yyTComb [  35], 
& yyTComb [2226], & yyTComb [2463], & yyTComb [   0], & yyTComb [5658], 
& yyTComb [ 646], & yyTComb [5111], & yyTComb [6347], & yyTComb [5091], 
& yyTComb [4040], & yyTComb [5080], & yyTComb [5058], & yyTComb [1244], 
& yyTComb [3828], & yyTComb [ 724], & yyTComb [ 301], & yyTComb [6423], 
& yyTComb [ 729], & yyTComb [12590], & yyTComb [3807], & yyTComb [5094], 
& yyTComb [  50], & yyTComb [5394], & yyTComb [5415], & yyTComb [8014], 
& yyTComb [6614], & yyTComb [5720], & yyTComb [6334], & yyTComb [5448], 
& yyTComb [6453], & yyTComb [7621], & yyTComb [3753], & yyTComb [4915], 
& yyTComb [2672], & yyTComb [6353], & yyTComb [6313], & yyTComb [6492], 
& yyTComb [4214], & yyTComb [6441], & yyTComb [3634], & yyTComb [10408], 
& yyTComb [5269], & yyTComb [5661], & yyTComb [2997], & yyTComb [9848], 
& yyTComb [6222], & yyTComb [5845], & yyTComb [7645], & yyTComb [6203], 
& yyTComb [2282], & yyTComb [5897], & yyTComb [5077], & yyTComb [14830], 
& yyTComb [2656], & yyTComb [5244], & yyTComb [6143], & yyTComb [5911], 
& yyTComb [5824], & yyTComb [1883], & yyTComb [5891], & yyTComb [5333], 
& yyTComb [2594], & yyTComb [6379], & yyTComb [2957], & yyTComb [5938], 
& yyTComb [5940], & yyTComb [2839], & yyTComb [5956], & yyTComb [2247], 
& yyTComb [2881], & yyTComb [ 207], & yyTComb [6616], & yyTComb [5936], 
& yyTComb [6768], & yyTComb [6218], & yyTComb [4762], & yyTComb [11528], 
& yyTComb [5134], & yyTComb [5329], & yyTComb [5117], & yyTComb [4746], 
& yyTComb [4734], & yyTComb [13488], & yyTComb [6224], & yyTComb [5966], 
& yyTComb [4967], & yyTComb [   0], & yyTComb [5975], & yyTComb [5782], 
& yyTComb [5977], & yyTComb [5931], & yyTComb [5589], & yyTComb [3779], 
& yyTComb [5667], & yyTComb [7980], & yyTComb [5313], & yyTComb [5379], 
& yyTComb [5398], & yyTComb [  11], & yyTComb [3703], & yyTComb [3704], 
& yyTComb [5708], & yyTComb [2514], & yyTComb [3734], & yyTComb [3694], 
& yyTComb [6368], & yyTComb [6639], & yyTComb [6630], & yyTComb [3466], 
& yyTComb [6676], & yyTComb [6635], & yyTComb [5520], & yyTComb [3475], 
& yyTComb [4295], & yyTComb [2814], & yyTComb [ 928], & yyTComb [3854], 
& yyTComb [6307], & yyTComb [1270], & yyTComb [5395], & yyTComb [6002], 
& yyTComb [6679], & yyTComb [4390], & yyTComb [9008], & yyTComb [5881], 
& yyTComb [2812], & yyTComb [6265], & yyTComb [5849], & yyTComb [2784], 
& yyTComb [5888], & yyTComb [5334], & yyTComb [5903], & yyTComb [5933], 
& yyTComb [5965], & yyTComb [5976], & yyTComb [5976], & yyTComb [18675], 
& yyTComb [3644], & yyTComb [10968], & yyTComb [5783], & yyTComb [5252], 
& yyTComb [4262], & yyTComb [3930], & yyTComb [11808], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [ 619], & yyTComb [6636], & yyTComb [ 847], 
& yyTComb [5238], & yyTComb [ 868], & yyTComb [2165], & yyTComb [1482], 
& yyTComb [2233], & yyTComb [6208], & yyTComb [5216], & yyTComb [5212], 
& yyTComb [3660], & yyTComb [5189], & yyTComb [ 621], & yyTComb [3840], 
& yyTComb [14048], & yyTComb [6426], & yyTComb [5876], & yyTComb [5875], 
& yyTComb [1541], & yyTComb [6336], & yyTComb [5800], & yyTComb [14328], 
& yyTComb [6342], & yyTComb [4990], & yyTComb [16767], & yyTComb [16881], 
& yyTComb [6348], & yyTComb [5800], & yyTComb [4920], & yyTComb [ 394], 
& yyTComb [1020], & yyTComb [3577], & yyTComb [6301], & yyTComb [3240], 
& yyTComb [1511], & yyTComb [6003], & yyTComb [5889], & yyTComb [15448], 
& yyTComb [15728], & yyTComb [6128], & yyTComb [2846], & yyTComb [3909], 
& yyTComb [1860], & yyTComb [ 275], & yyTComb [2623], & yyTComb [6149], 
& yyTComb [8765], & yyTComb [6387], & yyTComb [4149], & yyTComb [8026], 
& yyTComb [3129], & yyTComb [  20], & yyTComb [3098], & yyTComb [6118], 
& yyTComb [3978], & yyTComb [2614], & yyTComb [2243], & yyTComb [6138], 
& yyTComb [4188], & yyTComb [6460], & yyTComb [5377], & yyTComb [3735], 
& yyTComb [3491], & yyTComb [1791], & yyTComb [14888], & yyTComb [5460], 
& yyTComb [6636], & yyTComb [6165], & yyTComb [6152], & yyTComb [3100], 
& yyTComb [5549], & yyTComb [6170], & yyTComb [6528], & yyTComb [5589], 
& yyTComb [6162], & yyTComb [11565], & yyTComb [4521], & yyTComb [3666], 
& yyTComb [5795], & yyTComb [12852], & yyTComb [ 148], & yyTComb [6621], 
& yyTComb [4106], & yyTComb [3210], & yyTComb [3349], & yyTComb [3333], 
& yyTComb [9885], & yyTComb [13189], & yyTComb [5384], & yyTComb [2376], 
& yyTComb [3870], 
};
static	yytNComb *	yyNBasePtr	[yyLastReadState + 1] = { 0,
& yyNComb [ 476], & yyNComb [-177], & yyNComb [1434], & yyNComb [ 459], 
& yyNComb [-289], & yyNComb [-251], & yyNComb [1404], & yyNComb [1379], 
& yyNComb [1382], & yyNComb [1380], & yyNComb [   9], & yyNComb [-118], 
& yyNComb [ 325], & yyNComb [ 305], & yyNComb [ 248], & yyNComb [ 324], 
& yyNComb [-145], & yyNComb [1146], & yyNComb [1306], & yyNComb [1161], 
& yyNComb [1295], & yyNComb [1291], & yyNComb [1284], & yyNComb [1278], 
& yyNComb [ -37], & yyNComb [1425], & yyNComb [-463], & yyNComb [-356], 
& yyNComb [-409], & yyNComb [-380], & yyNComb [-232], & yyNComb [-316], 
& yyNComb [-372], & yyNComb [-323], & yyNComb [-409], & yyNComb [-190], 
& yyNComb [-241], & yyNComb [-226], & yyNComb [-322], & yyNComb [-325], 
& yyNComb [-342], & yyNComb [-441], & yyNComb [  47], & yyNComb [1286], 
& yyNComb [1139], & yyNComb [-463], & yyNComb [ 793], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 915], & yyNComb [-463], & yyNComb [1278], 
& yyNComb [-463], & yyNComb [ 176], & yyNComb [-463], & yyNComb [ 883], 
& yyNComb [1047], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ -36], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1293], & yyNComb [-463], 
& yyNComb [1774], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ -26], & yyNComb [ 798], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1218], & yyNComb [ -89], & yyNComb [  50], & yyNComb [-300], 
& yyNComb [-463], & yyNComb [1034], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 988], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 908], & yyNComb [-463], & yyNComb [1043], 
& yyNComb [ 994], & yyNComb [-463], & yyNComb [1008], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 953], & yyNComb [-463], & yyNComb [1675], 
& yyNComb [ 256], & yyNComb [1987], & yyNComb [ 442], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1255], & yyNComb [-463], & yyNComb [-463], & yyNComb [1612], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 729], 
& yyNComb [ 941], & yyNComb [1241], & yyNComb [-463], & yyNComb [1276], 
& yyNComb [-463], & yyNComb [ 594], & yyNComb [ 796], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 588], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 423], & yyNComb [-440], & yyNComb [ 736], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 614], & yyNComb [1630], & yyNComb [1708], 
& yyNComb [1861], & yyNComb [ 926], & yyNComb [ 403], & yyNComb [1570], 
& yyNComb [ 893], & yyNComb [1029], & yyNComb [1862], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1783], & yyNComb [-463], & yyNComb [ -28], 
& yyNComb [1178], & yyNComb [1157], & yyNComb [ 635], & yyNComb [-333], 
& yyNComb [-463], & yyNComb [ 464], & yyNComb [-463], & yyNComb [1267], 
& yyNComb [1406], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 630], 
& yyNComb [1152], & yyNComb [-463], & yyNComb [ 107], & yyNComb [1464], 
& yyNComb [1329], & yyNComb [1345], & yyNComb [1475], & yyNComb [1359], 
& yyNComb [-440], & yyNComb [ 767], & yyNComb [1323], & yyNComb [1322], 
& yyNComb [1361], & yyNComb [1372], & yyNComb [  60], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 558], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [  22], 
& yyNComb [ 794], & yyNComb [ 801], & yyNComb [ 789], & yyNComb [  29], 
& yyNComb [  49], & yyNComb [-463], & yyNComb [-463], & yyNComb [1723], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 727], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 894], & yyNComb [1614], 
& yyNComb [-271], & yyNComb [-463], & yyNComb [ 693], & yyNComb [1183], 
& yyNComb [ 768], & yyNComb [-463], & yyNComb [1622], & yyNComb [-269], 
& yyNComb [1627], & yyNComb [1287], & yyNComb [-463], & yyNComb [-436], 
& yyNComb [1523], & yyNComb [ 956], & yyNComb [ 950], & yyNComb [1665], 
& yyNComb [-463], & yyNComb [ 803], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 614], & yyNComb [ 274], 
& yyNComb [ 810], & yyNComb [ 631], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1272], & yyNComb [1366], & yyNComb [1369], 
& yyNComb [1319], & yyNComb [1374], & yyNComb [1375], & yyNComb [-463], 
& yyNComb [1061], & yyNComb [-463], & yyNComb [ -16], & yyNComb [-442], 
& yyNComb [ 279], & yyNComb [ -28], & yyNComb [ -46], & yyNComb [ 363], 
& yyNComb [-190], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 336], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1482], & yyNComb [-463], 
& yyNComb [1697], & yyNComb [1652], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 751], & yyNComb [1727], & yyNComb [-463], & yyNComb [1242], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [  70], & yyNComb [1239], & yyNComb [ 419], 
& yyNComb [1305], & yyNComb [1306], & yyNComb [ 381], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1769], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 825], & yyNComb [ 260], & yyNComb [1585], & yyNComb [ 355], 
& yyNComb [1285], & yyNComb [-463], & yyNComb [-463], & yyNComb [1282], 
& yyNComb [-463], & yyNComb [ 884], & yyNComb [1281], & yyNComb [ 251], 
& yyNComb [1128], & yyNComb [1154], & yyNComb [  73], & yyNComb [1326], 
& yyNComb [1381], & yyNComb [1382], & yyNComb [1280], & yyNComb [1383], 
& yyNComb [1385], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1303], & yyNComb [1386], & yyNComb [1387], 
& yyNComb [-463], & yyNComb [1304], & yyNComb [1310], & yyNComb [1388], 
& yyNComb [1391], & yyNComb [-463], & yyNComb [1330], & yyNComb [1380], 
& yyNComb [1379], & yyNComb [-463], & yyNComb [1295], & yyNComb [1324], 
& yyNComb [1390], & yyNComb [1389], & yyNComb [-463], & yyNComb [ 281], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 421], 
& yyNComb [ 782], & yyNComb [-460], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1959], & yyNComb [-458], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1254], & yyNComb [1678], & yyNComb [ 351], 
& yyNComb [ 567], & yyNComb [1146], & yyNComb [ 287], & yyNComb [1073], 
& yyNComb [1418], & yyNComb [1836], & yyNComb [ 732], & yyNComb [ 917], 
& yyNComb [1107], & yyNComb [-463], & yyNComb [1837], & yyNComb [1561], 
& yyNComb [1531], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 323], 
& yyNComb [1197], & yyNComb [1592], & yyNComb [-463], & yyNComb [1513], 
& yyNComb [ 992], & yyNComb [-463], & yyNComb [-463], & yyNComb [1554], 
& yyNComb [ -63], & yyNComb [1493], & yyNComb [ 892], & yyNComb [ 896], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 901], & yyNComb [ 902], 
& yyNComb [ 925], & yyNComb [ 927], & yyNComb [ 727], & yyNComb [-319], 
& yyNComb [1514], & yyNComb [-463], & yyNComb [-463], & yyNComb [ -64], 
& yyNComb [-463], & yyNComb [1303], & yyNComb [ 416], & yyNComb [-463], 
& yyNComb [ 964], & yyNComb [ 971], & yyNComb [-463], & yyNComb [1756], 
& yyNComb [ 734], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1076], & yyNComb [1465], & yyNComb [ 812], 
& yyNComb [1417], & yyNComb [-463], & yyNComb [1474], & yyNComb [-463], 
& yyNComb [ 936], & yyNComb [1096], & yyNComb [ 877], & yyNComb [ 370], 
& yyNComb [-463], & yyNComb [1527], & yyNComb [ 874], & yyNComb [1517], 
& yyNComb [ 644], & yyNComb [ 652], & yyNComb [ 264], & yyNComb [1280], 
& yyNComb [ 801], & yyNComb [ 806], & yyNComb [-463], & yyNComb [-125], 
& yyNComb [1060], & yyNComb [1286], & yyNComb [1302], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1327], & yyNComb [1358], & yyNComb [1378], 
& yyNComb [1331], & yyNComb [1350], & yyNComb [1346], & yyNComb [ 545], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1333], & yyNComb [1341], 
& yyNComb [1343], & yyNComb [1332], & yyNComb [1347], & yyNComb [1348], 
& yyNComb [ 405], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1407], & yyNComb [1212], & yyNComb [1402], & yyNComb [-157], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1958], & yyNComb [-463], 
& yyNComb [1613], & yyNComb [1606], & yyNComb [1849], & yyNComb [-463], 
& yyNComb [-379], & yyNComb [1945], & yyNComb [1969], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 358], & yyNComb [  54], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1504], & yyNComb [1049], & yyNComb [-463], 
& yyNComb [1498], & yyNComb [1209], & yyNComb [ 472], & yyNComb [1245], 
& yyNComb [-463], & yyNComb [1160], & yyNComb [1736], & yyNComb [ 208], 
& yyNComb [ 207], & yyNComb [ 712], & yyNComb [1287], & yyNComb [ 897], 
& yyNComb [ 895], & yyNComb [1318], & yyNComb [1313], & yyNComb [ 713], 
& yyNComb [ 649], & yyNComb [-463], & yyNComb [1232], & yyNComb [ 800], 
& yyNComb [ 815], & yyNComb [ 463], & yyNComb [ 352], & yyNComb [1281], 
& yyNComb [ 889], & yyNComb [1307], & yyNComb [1245], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 654], & yyNComb [ 907], & yyNComb [ 898], 
& yyNComb [1315], & yyNComb [1314], & yyNComb [ 674], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1249], & yyNComb [1236], 
& yyNComb [1331], & yyNComb [ 268], & yyNComb [-463], & yyNComb [1049], 
& yyNComb [ 466], & yyNComb [-463], & yyNComb [-463], & yyNComb [1534], 
& yyNComb [1505], & yyNComb [1543], & yyNComb [1544], & yyNComb [-463], 
& yyNComb [1463], & yyNComb [ 621], & yyNComb [ 730], & yyNComb [1502], 
& yyNComb [1746], & yyNComb [-463], & yyNComb [ 464], & yyNComb [ 631], 
& yyNComb [ 633], & yyNComb [1277], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 921], & yyNComb [ 926], & yyNComb [-463], & yyNComb [ 791], 
& yyNComb [1901], & yyNComb [1635], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1949], & yyNComb [-463], & yyNComb [-392], & yyNComb [ 910], 
& yyNComb [ 867], & yyNComb [1869], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 912], & yyNComb [ 997], & yyNComb [ 998], & yyNComb [-463], 
& yyNComb [  31], & yyNComb [ 638], & yyNComb [1948], & yyNComb [1860], 
& yyNComb [ 141], & yyNComb [ 917], & yyNComb [1235], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 435], & yyNComb [1229], & yyNComb [-463], & yyNComb [-182], 
& yyNComb [1230], & yyNComb [1087], & yyNComb [-463], & yyNComb [ 779], 
& yyNComb [  -2], & yyNComb [ 742], & yyNComb [ 741], & yyNComb [-437], 
& yyNComb [1081], & yyNComb [ 796], & yyNComb [-463], & yyNComb [1181], 
& yyNComb [1182], & yyNComb [1545], & yyNComb [1548], & yyNComb [1578], 
& yyNComb [1225], & yyNComb [1227], & yyNComb [1568], & yyNComb [1086], 
& yyNComb [ 796], & yyNComb [ 797], & yyNComb [ 740], & yyNComb [  31], 
& yyNComb [1084], & yyNComb [ 806], & yyNComb [1563], & yyNComb [1863], 
& yyNComb [1042], & yyNComb [ 710], & yyNComb [ 739], & yyNComb [ 738], 
& yyNComb [ 889], & yyNComb [ 846], & yyNComb [1225], & yyNComb [1222], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1315], & yyNComb [1301], & yyNComb [1299], 
& yyNComb [1271], & yyNComb [1233], & yyNComb [1216], & yyNComb [ 634], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 290], & yyNComb [ 754], 
& yyNComb [ 524], & yyNComb [ 556], & yyNComb [ 124], & yyNComb [ 472], 
& yyNComb [ 406], & yyNComb [ 389], & yyNComb [1216], & yyNComb [1202], 
& yyNComb [ 186], & yyNComb [ 698], & yyNComb [1254], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 626], & yyNComb [ 517], & yyNComb [1225], 
& yyNComb [-463], & yyNComb [1314], & yyNComb [1284], & yyNComb [ 516], 
& yyNComb [ 428], & yyNComb [ 413], & yyNComb [ 903], & yyNComb [ 489], 
& yyNComb [1455], & yyNComb [ 370], & yyNComb [ 723], & yyNComb [-463], 
& yyNComb [ 694], & yyNComb [ 232], & yyNComb [-368], & yyNComb [-463], 
& yyNComb [1463], & yyNComb [-463], & yyNComb [1293], & yyNComb [  58], 
& yyNComb [-463], & yyNComb [1468], & yyNComb [1335], & yyNComb [1128], 
& yyNComb [1300], & yyNComb [-463], & yyNComb [ 347], & yyNComb [1341], 
& yyNComb [1476], & yyNComb [ -66], & yyNComb [ 172], & yyNComb [ 758], 
& yyNComb [1447], & yyNComb [ 912], & yyNComb [ 596], & yyNComb [1311], 
& yyNComb [ 937], & yyNComb [1351], & yyNComb [-463], & yyNComb [ 787], 
& yyNComb [-454], & yyNComb [-460], & yyNComb [ -59], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-187], & yyNComb [1598], 
& yyNComb [1625], & yyNComb [-463], & yyNComb [-463], & yyNComb [1879], 
& yyNComb [1835], & yyNComb [1780], & yyNComb [-463], & yyNComb [1397], 
& yyNComb [1865], & yyNComb [1074], & yyNComb [1893], & yyNComb [1443], 
& yyNComb [1445], & yyNComb [-463], & yyNComb [1337], & yyNComb [ 977], 
& yyNComb [1526], & yyNComb [1587], & yyNComb [1048], & yyNComb [1044], 
& yyNComb [ 394], & yyNComb [-463], & yyNComb [-463], & yyNComb [1292], 
& yyNComb [1040], & yyNComb [ 945], & yyNComb [-463], & yyNComb [1430], 
& yyNComb [ 946], & yyNComb [ 949], & yyNComb [ 950], & yyNComb [ 951], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 422], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1213], & yyNComb [ 706], & yyNComb [ 327], & yyNComb [1378], 
& yyNComb [1368], & yyNComb [1361], & yyNComb [1348], & yyNComb [ 666], 
& yyNComb [-226], & yyNComb [ 234], & yyNComb [ 711], & yyNComb [ 472], 
& yyNComb [1062], & yyNComb [-463], & yyNComb [ 680], & yyNComb [1403], 
& yyNComb [ -56], & yyNComb [ -91], & yyNComb [1675], & yyNComb [ 734], 
& yyNComb [1236], & yyNComb [1255], & yyNComb [1323], & yyNComb [1423], 
& yyNComb [ 633], & yyNComb [1404], & yyNComb [ 515], & yyNComb [1584], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 740], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [  86], & yyNComb [-453], & yyNComb [1178], 
& yyNComb [1221], & yyNComb [1226], & yyNComb [ 816], & yyNComb [ 818], 
& yyNComb [ 472], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 639], 
& yyNComb [-120], & yyNComb [1305], & yyNComb [ 544], & yyNComb [-463], 
& yyNComb [ 611], & yyNComb [-463], & yyNComb [1036], & yyNComb [ 723], 
& yyNComb [1602], & yyNComb [-463], & yyNComb [1047], & yyNComb [-450], 
& yyNComb [1268], & yyNComb [1392], & yyNComb [ 713], & yyNComb [1017], 
& yyNComb [1415], & yyNComb [-463], & yyNComb [-463], & yyNComb [1424], 
& yyNComb [1147], & yyNComb [-463], & yyNComb [ 604], & yyNComb [ 243], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1411], & yyNComb [ 238], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 226], & yyNComb [-463], 
& yyNComb [ 470], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 752], 
& yyNComb [-463], & yyNComb [1059], & yyNComb [ 517], & yyNComb [ -63], 
& yyNComb [1021], & yyNComb [ 581], & yyNComb [1326], & yyNComb [ 544], 
& yyNComb [-463], & yyNComb [1398], & yyNComb [ 528], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1690], & yyNComb [ 347], & yyNComb [ 751], 
& yyNComb [ 345], & yyNComb [ 334], & yyNComb [1496], & yyNComb [1277], 
& yyNComb [ 293], & yyNComb [ 756], & yyNComb [1082], & yyNComb [ 409], 
& yyNComb [1222], & yyNComb [-458], & yyNComb [ 733], & yyNComb [ 184], 
& yyNComb [ 440], & yyNComb [ 706], & yyNComb [1402], & yyNComb [1228], 
& yyNComb [1398], & yyNComb [1263], & yyNComb [1129], & yyNComb [1379], 
& yyNComb [ 775], & yyNComb [   1], & yyNComb [ 207], & yyNComb [1235], 
& yyNComb [ 718], & yyNComb [ 762], & yyNComb [ -73], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 922], & yyNComb [1229], & yyNComb [ -34], & yyNComb [1225], 
& yyNComb [ 950], & yyNComb [1327], & yyNComb [1411], & yyNComb [1196], 
& yyNComb [ 552], & yyNComb [-463], & yyNComb [ 389], & yyNComb [1397], 
& yyNComb [1666], & yyNComb [ 143], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-192], & yyNComb [ 536], & yyNComb [-463], & yyNComb [1322], 
& yyNComb [1007], & yyNComb [-463], & yyNComb [1040], & yyNComb [-463], 
& yyNComb [ 958], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 470], & yyNComb [ 624], & yyNComb [-463], 
& yyNComb [1585], & yyNComb [1608], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1868], & yyNComb [1783], & yyNComb [1415], & yyNComb [ 915], 
& yyNComb [1405], & yyNComb [ 500], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1951], & yyNComb [ 801], & yyNComb [ 871], 
& yyNComb [ 870], & yyNComb [1046], & yyNComb [-459], & yyNComb [1292], 
& yyNComb [ 651], & yyNComb [ -96], & yyNComb [-454], & yyNComb [ 808], 
& yyNComb [  71], & yyNComb [1355], & yyNComb [ 646], & yyNComb [1372], 
& yyNComb [1375], & yyNComb [1380], & yyNComb [1409], & yyNComb [ 935], 
& yyNComb [ 930], & yyNComb [ 920], & yyNComb [ 913], & yyNComb [ 942], 
& yyNComb [ 934], & yyNComb [1216], & yyNComb [ 912], & yyNComb [ 937], 
& yyNComb [ 906], & yyNComb [ 909], & yyNComb [ 918], & yyNComb [ 921], 
& yyNComb [ 922], & yyNComb [ 925], & yyNComb [ 926], & yyNComb [ 927], 
& yyNComb [ 931], & yyNComb [1219], & yyNComb [1023], & yyNComb [-463], 
& yyNComb [1060], & yyNComb [ 668], & yyNComb [ 389], & yyNComb [ 470], 
& yyNComb [1064], & yyNComb [ 581], & yyNComb [ 384], & yyNComb [ 451], 
& yyNComb [1058], & yyNComb [1483], & yyNComb [1505], & yyNComb [1122], 
& yyNComb [-463], & yyNComb [ 625], & yyNComb [ 630], & yyNComb [-463], 
& yyNComb [1029], & yyNComb [1507], & yyNComb [ 397], & yyNComb [-463], 
& yyNComb [ 615], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 629], 
& yyNComb [-395], & yyNComb [-463], & yyNComb [1119], & yyNComb [  22], 
& yyNComb [-445], & yyNComb [-463], & yyNComb [ 741], & yyNComb [ 521], 
& yyNComb [ 843], & yyNComb [-357], & yyNComb [ 944], & yyNComb [ 880], 
& yyNComb [1360], & yyNComb [1494], & yyNComb [1529], & yyNComb [1534], 
& yyNComb [1129], & yyNComb [1848], & yyNComb [1593], & yyNComb [ 340], 
& yyNComb [-463], & yyNComb [ 954], & yyNComb [-463], & yyNComb [ 396], 
& yyNComb [ 467], & yyNComb [ 482], & yyNComb [ 724], & yyNComb [-460], 
& yyNComb [1112], & yyNComb [  63], & yyNComb [ -20], & yyNComb [-463], 
& yyNComb [   2], & yyNComb [1531], & yyNComb [ 304], & yyNComb [1076], 
& yyNComb [ 619], & yyNComb [ 533], & yyNComb [1021], & yyNComb [-463], 
& yyNComb [ 528], & yyNComb [ 524], & yyNComb [-463], & yyNComb [1445], 
& yyNComb [-153], & yyNComb [-463], & yyNComb [ 707], & yyNComb [ 505], 
& yyNComb [1077], & yyNComb [-397], & yyNComb [ 892], & yyNComb [-459], 
& yyNComb [-463], & yyNComb [1009], & yyNComb [ 916], & yyNComb [-463], 
& yyNComb [1381], & yyNComb [ 995], & yyNComb [ 603], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 451], 
& yyNComb [ 941], & yyNComb [ 321], & yyNComb [-463], & yyNComb [ 587], 
& yyNComb [-314], & yyNComb [-463], & yyNComb [ 287], & yyNComb [ 493], 
& yyNComb [-463], & yyNComb [1498], & yyNComb [-463], & yyNComb [1079], 
& yyNComb [ 275], & yyNComb [ 304], & yyNComb [1197], & yyNComb [ 380], 
& yyNComb [1083], & yyNComb [ 632], & yyNComb [-137], & yyNComb [1223], 
& yyNComb [ 540], & yyNComb [-241], & yyNComb [1600], & yyNComb [ 161], 
& yyNComb [ 484], & yyNComb [1411], & yyNComb [ 194], & yyNComb [ -13], 
& yyNComb [-463], & yyNComb [1012], & yyNComb [ 512], & yyNComb [-463], 
& yyNComb [ 921], & yyNComb [1228], & yyNComb [1214], & yyNComb [ 953], 
& yyNComb [1237], & yyNComb [1403], & yyNComb [1669], & yyNComb [ 207], 
& yyNComb [ 543], & yyNComb [-463], & yyNComb [1038], & yyNComb [ 952], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 438], & yyNComb [ 395], 
& yyNComb [ 917], & yyNComb [ 459], & yyNComb [-463], & yyNComb [1412], 
& yyNComb [1413], & yyNComb [1066], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1227], & yyNComb [-177], & yyNComb [ 301], & yyNComb [ 918], 
& yyNComb [ 654], & yyNComb [ 917], & yyNComb [ 547], & yyNComb [ 551], 
& yyNComb [ 612], & yyNComb [-463], & yyNComb [ 357], & yyNComb [-463], 
& yyNComb [-222], & yyNComb [-341], & yyNComb [ 312], & yyNComb [ 489], 
& yyNComb [1497], & yyNComb [-463], & yyNComb [ 472], & yyNComb [-463], 
& yyNComb [1496], & yyNComb [-463], & yyNComb [ 797], & yyNComb [-263], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1577], & yyNComb [1596], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 991], & yyNComb [1426], & yyNComb [ 914], & yyNComb [1625], 
& yyNComb [ 653], & yyNComb [1558], & yyNComb [1050], & yyNComb [-463], 
& yyNComb [ 355], & yyNComb [-463], & yyNComb [ 730], & yyNComb [-463], 
& yyNComb [ 876], & yyNComb [1407], & yyNComb [ 932], & yyNComb [-463], 
& yyNComb [1043], & yyNComb [ 345], & yyNComb [ 929], & yyNComb [1970], 
& yyNComb [1918], & yyNComb [1994], & yyNComb [-463], & yyNComb [ 980], 
& yyNComb [1556], & yyNComb [1043], & yyNComb [1552], & yyNComb [ 924], 
& yyNComb [ 923], & yyNComb [1784], & yyNComb [-463], & yyNComb [1155], 
& yyNComb [1335], & yyNComb [1415], & yyNComb [1288], & yyNComb [1376], 
& yyNComb [1425], & yyNComb [1422], & yyNComb [1412], & yyNComb [1215], 
& yyNComb [1404], & yyNComb [1384], & yyNComb [1371], & yyNComb [1370], 
& yyNComb [1363], & yyNComb [1356], & yyNComb [1354], & yyNComb [1346], 
& yyNComb [1345], & yyNComb [1344], & yyNComb [1330], & yyNComb [1220], 
& yyNComb [1061], & yyNComb [ 664], & yyNComb [1058], & yyNComb [ 658], 
& yyNComb [ 387], & yyNComb [1053], & yyNComb [1065], & yyNComb [ 587], 
& yyNComb [1057], & yyNComb [1899], & yyNComb [-463], & yyNComb [1743], 
& yyNComb [ 722], & yyNComb [ 641], & yyNComb [ 635], & yyNComb [1741], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 788], & yyNComb [1401], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 457], & yyNComb [1535], 
& yyNComb [-463], & yyNComb [1361], & yyNComb [ 710], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1830], & yyNComb [ 948], & yyNComb [1595], 
& yyNComb [1495], & yyNComb [ 337], & yyNComb [1532], & yyNComb [1468], 
& yyNComb [1469], & yyNComb [1470], & yyNComb [-463], & yyNComb [ 378], 
& yyNComb [ 426], & yyNComb [ 696], & yyNComb [ 415], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-273], & yyNComb [  34], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-344], & yyNComb [1293], & yyNComb [-463], 
& yyNComb [-203], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-174], & yyNComb [-463], & yyNComb [1512], 
& yyNComb [-463], & yyNComb [ 806], & yyNComb [-463], & yyNComb [ 459], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1466], & yyNComb [1464], 
& yyNComb [1471], & yyNComb [-463], & yyNComb [ 709], & yyNComb [ 442], 
& yyNComb [-463], & yyNComb [ 218], & yyNComb [ -82], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 176], 
& yyNComb [ 755], & yyNComb [ 644], & yyNComb [ 615], & yyNComb [1504], 
& yyNComb [1382], & yyNComb [-463], & yyNComb [ 598], & yyNComb [ 879], 
& yyNComb [1250], & yyNComb [1530], & yyNComb [ 940], & yyNComb [ 315], 
& yyNComb [1038], & yyNComb [ 634], & yyNComb [ 907], & yyNComb [ 422], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 285], & yyNComb [ 642], & yyNComb [ 725], & yyNComb [ 437], 
& yyNComb [ 729], & yyNComb [ 695], & yyNComb [ 739], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 713], & yyNComb [ 431], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1118], & yyNComb [1842], 
& yyNComb [1608], & yyNComb [ 333], & yyNComb [ 952], & yyNComb [-463], 
& yyNComb [ -75], & yyNComb [ 488], & yyNComb [-457], & yyNComb [ 811], 
& yyNComb [1399], & yyNComb [ 317], & yyNComb [ 203], & yyNComb [ 693], 
& yyNComb [ 497], & yyNComb [ 208], & yyNComb [1275], & yyNComb [ 498], 
& yyNComb [1418], & yyNComb [1417], & yyNComb [1778], & yyNComb [-463], 
& yyNComb [1225], & yyNComb [ 311], & yyNComb [ 118], & yyNComb [ 915], 
& yyNComb [ 306], & yyNComb [ 261], & yyNComb [-463], & yyNComb [ 507], 
& yyNComb [-463], & yyNComb [-391], & yyNComb [ 433], & yyNComb [ 541], 
& yyNComb [ 828], & yyNComb [ 818], & yyNComb [-456], & yyNComb [-463], 
& yyNComb [1775], & yyNComb [1776], & yyNComb [ 652], & yyNComb [-456], 
& yyNComb [ 492], & yyNComb [ 550], & yyNComb [ 983], & yyNComb [-463], 
& yyNComb [ 693], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 915], 
& yyNComb [1081], & yyNComb [ 953], & yyNComb [ 595], & yyNComb [-463], 
& yyNComb [ 391], & yyNComb [1280], & yyNComb [1283], & yyNComb [1284], 
& yyNComb [1283], & yyNComb [ 579], & yyNComb [ 507], & yyNComb [-463], 
& yyNComb [1322], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 681], 
& yyNComb [ 878], & yyNComb [-463], & yyNComb [ 238], & yyNComb [ 399], 
& yyNComb [ 702], & yyNComb [-365], & yyNComb [ 424], & yyNComb [ 192], 
& yyNComb [-458], & yyNComb [1176], & yyNComb [1191], & yyNComb [1026], 
& yyNComb [1472], & yyNComb [-463], & yyNComb [1038], & yyNComb [ 695], 
& yyNComb [ 263], & yyNComb [ 698], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1567], & yyNComb [1566], & yyNComb [-463], & yyNComb [ 978], 
& yyNComb [ 979], & yyNComb [1041], & yyNComb [ 981], & yyNComb [1040], 
& yyNComb [-463], & yyNComb [1044], & yyNComb [1167], & yyNComb [1604], 
& yyNComb [-451], & yyNComb [1626], & yyNComb [1532], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1017], & yyNComb [ 984], 
& yyNComb [1981], & yyNComb [1974], & yyNComb [1557], & yyNComb [-463], 
& yyNComb [1568], & yyNComb [-463], & yyNComb [1572], & yyNComb [-463], 
& yyNComb [1094], & yyNComb [ 717], & yyNComb [-463], & yyNComb [ 927], 
& yyNComb [ 844], & yyNComb [1558], & yyNComb [ 620], & yyNComb [ 224], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [1219], 
& yyNComb [1212], & yyNComb [1214], & yyNComb [1056], & yyNComb [1066], 
& yyNComb [ 656], & yyNComb [1055], & yyNComb [1059], & yyNComb [ 728], 
& yyNComb [1508], & yyNComb [1506], & yyNComb [-463], & yyNComb [ 109], 
& yyNComb [ 947], & yyNComb [1457], & yyNComb [-463], & yyNComb [ 476], 
& yyNComb [ 982], & yyNComb [ 986], & yyNComb [1525], & yyNComb [-463], 
& yyNComb [ 796], & yyNComb [-463], & yyNComb [1558], & yyNComb [1556], 
& yyNComb [1553], & yyNComb [1503], & yyNComb [1547], & yyNComb [-463], 
& yyNComb [1602], & yyNComb [-463], & yyNComb [ 341], & yyNComb [-463], 
& yyNComb [1538], & yyNComb [ 542], & yyNComb [ 581], & yyNComb [ 600], 
& yyNComb [1391], & yyNComb [1289], & yyNComb [ 458], & yyNComb [-463], 
& yyNComb [1032], & yyNComb [1544], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1543], & yyNComb [ 456], & yyNComb [ 214], & yyNComb [1552], 
& yyNComb [-345], & yyNComb [-459], & yyNComb [-458], & yyNComb [-463], 
& yyNComb [ 820], & yyNComb [ 817], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 541], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 809], 
& yyNComb [ 876], & yyNComb [1514], & yyNComb [ 443], & yyNComb [ 804], 
& yyNComb [-463], & yyNComb [1521], & yyNComb [-463], & yyNComb [1540], 
& yyNComb [-463], & yyNComb [1526], & yyNComb [1522], & yyNComb [ 567], 
& yyNComb [ 587], & yyNComb [ 593], & yyNComb [ 274], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 759], & yyNComb [ 399], & yyNComb [ 911], 
& yyNComb [ 920], & yyNComb [-440], & yyNComb [ 404], & yyNComb [ 932], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 681], & yyNComb [ 645], 
& yyNComb [ 624], & yyNComb [ 609], & yyNComb [ 744], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 882], & yyNComb [-463], & yyNComb [ 883], 
& yyNComb [ 243], & yyNComb [1523], & yyNComb [-463], & yyNComb [1081], 
& yyNComb [ 998], & yyNComb [1510], & yyNComb [1513], & yyNComb [-105], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 731], & yyNComb [1771], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 107], 
& yyNComb [1536], & yyNComb [1844], & yyNComb [1609], & yyNComb [ 344], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 355], & yyNComb [-290], 
& yyNComb [ 708], & yyNComb [ 411], & yyNComb [-451], & yyNComb [ 524], 
& yyNComb [-463], & yyNComb [  94], & yyNComb [-463], & yyNComb [ 730], 
& yyNComb [ 432], & yyNComb [-463], & yyNComb [ 952], & yyNComb [ 716], 
& yyNComb [ 446], & yyNComb [1779], & yyNComb [1061], & yyNComb [-457], 
& yyNComb [ 472], & yyNComb [ 420], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 101], & yyNComb [-463], & yyNComb [  68], & yyNComb [ 394], 
& yyNComb [ 710], & yyNComb [ 450], & yyNComb [ 464], & yyNComb [1025], 
& yyNComb [-463], & yyNComb [ 267], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 412], & yyNComb [ 717], & yyNComb [ 787], 
& yyNComb [ 269], & yyNComb [ 928], & yyNComb [ 715], & yyNComb [ 340], 
& yyNComb [-463], & yyNComb [ 247], & yyNComb [1678], & yyNComb [-463], 
& yyNComb [  58], & yyNComb [ 877], & yyNComb [ 421], & yyNComb [ 679], 
& yyNComb [ 612], & yyNComb [1474], & yyNComb [1605], & yyNComb [-463], 
& yyNComb [ 609], & yyNComb [ 330], & yyNComb [ 321], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [1246], & yyNComb [ 807], 
& yyNComb [1073], & yyNComb [1253], & yyNComb [1040], & yyNComb [1042], 
& yyNComb [-463], & yyNComb [1297], & yyNComb [1032], & yyNComb [1037], 
& yyNComb [ 454], & yyNComb [-463], & yyNComb [ 807], & yyNComb [  51], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 688], & yyNComb [-463], 
& yyNComb [ 473], & yyNComb [-463], & yyNComb [-420], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-193], & yyNComb [ 861], 
& yyNComb [ 121], & yyNComb [-463], & yyNComb [1035], & yyNComb [ 696], 
& yyNComb [ 717], & yyNComb [-328], & yyNComb [1567], & yyNComb [1569], 
& yyNComb [ 842], & yyNComb [ 840], & yyNComb [1218], & yyNComb [ 831], 
& yyNComb [1040], & yyNComb [ 767], & yyNComb [1012], & yyNComb [ 974], 
& yyNComb [-463], & yyNComb [1002], & yyNComb [-463], & yyNComb [1480], 
& yyNComb [1222], & yyNComb [1320], & yyNComb [1481], & yyNComb [1562], 
& yyNComb [1501], & yyNComb [ 904], & yyNComb [1310], & yyNComb [ 872], 
& yyNComb [ 594], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 711], & yyNComb [ 977], & yyNComb [ 911], & yyNComb [ 914], 
& yyNComb [-462], & yyNComb [-463], & yyNComb [ 450], & yyNComb [2013], 
& yyNComb [-277], & yyNComb [1056], & yyNComb [-462], & yyNComb [ 570], 
& yyNComb [1071], & yyNComb [1540], & yyNComb [ -47], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1305], & yyNComb [1226], & yyNComb [1054], 
& yyNComb [1967], & yyNComb [1966], & yyNComb [-463], & yyNComb [1089], 
& yyNComb [1148], & yyNComb [ 326], & yyNComb [ 623], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 711], & yyNComb [ 888], & yyNComb [ 656], 
& yyNComb [ 364], & yyNComb [ 688], & yyNComb [1516], & yyNComb [ 596], 
& yyNComb [ 597], & yyNComb [ 334], & yyNComb [1531], & yyNComb [-463], 
& yyNComb [ 799], & yyNComb [1545], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 360], & yyNComb [-463], & yyNComb [ 741], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 775], 
& yyNComb [ 539], & yyNComb [-463], & yyNComb [ 778], & yyNComb [ 642], 
& yyNComb [ 363], & yyNComb [ 715], & yyNComb [ 575], & yyNComb [ 436], 
& yyNComb [-463], & yyNComb [ 258], & yyNComb [1320], & yyNComb [1178], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 413], 
& yyNComb [ 628], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 641], 
& yyNComb [ 255], & yyNComb [-463], & yyNComb [ 222], & yyNComb [ 897], 
& yyNComb [1286], & yyNComb [ 871], & yyNComb [ 912], & yyNComb [ 342], 
& yyNComb [ 885], & yyNComb [ 638], & yyNComb [-463], & yyNComb [1607], 
& yyNComb [ 335], & yyNComb [-463], & yyNComb [-151], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 257], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 792], & yyNComb [ 793], & yyNComb [ 916], & yyNComb [-463], 
& yyNComb [ -43], & yyNComb [ 253], & yyNComb [1161], & yyNComb [ 469], 
& yyNComb [1273], & yyNComb [ 116], & yyNComb [ 459], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 594], & yyNComb [ 588], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 348], & yyNComb [ 351], & yyNComb [1377], 
& yyNComb [-463], & yyNComb [1042], & yyNComb [ 701], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 904], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 698], & yyNComb [1037], 
& yyNComb [ 695], & yyNComb [ 871], & yyNComb [ 157], & yyNComb [1070], 
& yyNComb [-463], & yyNComb [-282], & yyNComb [ 451], & yyNComb [1506], 
& yyNComb [ 617], & yyNComb [1108], & yyNComb [1110], & yyNComb [ 611], 
& yyNComb [-463], & yyNComb [1023], & yyNComb [1230], & yyNComb [1450], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [1035], 
& yyNComb [1036], & yyNComb [1033], & yyNComb [1034], & yyNComb [-463], 
& yyNComb [ 997], & yyNComb [ 678], & yyNComb [-463], & yyNComb [ 269], 
& yyNComb [1190], & yyNComb [ 686], & yyNComb [ 369], & yyNComb [ 431], 
& yyNComb [ 225], & yyNComb [1551], & yyNComb [ 894], & yyNComb [ 893], 
& yyNComb [-463], & yyNComb [ 296], & yyNComb [ 703], & yyNComb [-463], 
& yyNComb [1552], & yyNComb [1871], & yyNComb [-142], & yyNComb [1510], 
& yyNComb [1174], & yyNComb [1176], & yyNComb [ 983], & yyNComb [1177], 
& yyNComb [ 504], & yyNComb [-335], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 963], & yyNComb [-463], & yyNComb [1238], & yyNComb [ 884], 
& yyNComb [1489], & yyNComb [1281], & yyNComb [-463], & yyNComb [1550], 
& yyNComb [ 857], & yyNComb [1541], & yyNComb [ 952], & yyNComb [ 947], 
& yyNComb [1177], & yyNComb [-463], & yyNComb [ 943], & yyNComb [-463], 
& yyNComb [ 938], & yyNComb [ 790], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 504], & yyNComb [1567], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 476], & yyNComb [ 907], & yyNComb [2008], & yyNComb [-463], 
& yyNComb [1284], & yyNComb [ 670], & yyNComb [1527], & yyNComb [1228], 
& yyNComb [ 976], & yyNComb [1459], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [1042], & yyNComb [1234], & yyNComb [-105], & yyNComb [1035], 
& yyNComb [ 765], & yyNComb [ 768], & yyNComb [-463], & yyNComb [1363], 
& yyNComb [ 673], & yyNComb [1530], & yyNComb [1473], & yyNComb [1472], 
& yyNComb [1156], & yyNComb [-463], & yyNComb [ 880], & yyNComb [ 433], 
& yyNComb [ 631], & yyNComb [-463], & yyNComb [ 772], & yyNComb [ 478], 
& yyNComb [-268], & yyNComb [-463], & yyNComb [ 608], & yyNComb [-463], 
& yyNComb [1284], & yyNComb [ 647], & yyNComb [1282], & yyNComb [1522], 
& yyNComb [1067], & yyNComb [-463], & yyNComb [1458], & yyNComb [1465], 
& yyNComb [-304], & yyNComb [ 342], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 365], & yyNComb [ 434], & yyNComb [-463], & yyNComb [ 312], 
& yyNComb [ 348], & yyNComb [ 343], & yyNComb [ 345], & yyNComb [1248], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 657], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 351], & yyNComb [1549], 
& yyNComb [-463], & yyNComb [ 703], & yyNComb [ 708], & yyNComb [ 702], 
& yyNComb [ 319], & yyNComb [-463], & yyNComb [ 903], & yyNComb [ 900], 
& yyNComb [ 882], & yyNComb [-463], & yyNComb [-368], & yyNComb [-463], 
& yyNComb [ -73], & yyNComb [ 686], & yyNComb [1606], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [1519], & yyNComb [ 896], & yyNComb [1040], 
& yyNComb [ 432], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [ 863], & yyNComb [-463], & yyNComb [ 427], & yyNComb [ 896], 
& yyNComb [ 142], & yyNComb [ 522], & yyNComb [ 542], & yyNComb [-122], 
& yyNComb [1033], & yyNComb [-463], & yyNComb [1572], & yyNComb [1854], 
& yyNComb [-179], & yyNComb [1246], & yyNComb [-463], & yyNComb [1660], 
& yyNComb [ 962], & yyNComb [-463], & yyNComb [ 861], & yyNComb [-463], 
& yyNComb [ 902], & yyNComb [  45], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-170], & yyNComb [ 616], & yyNComb [ 837], & yyNComb [ 885], 
& yyNComb [-463], & yyNComb [ 940], & yyNComb [ 946], & yyNComb [1147], 
& yyNComb [1146], & yyNComb [-463], & yyNComb [1145], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 636], & yyNComb [1222], & yyNComb [1246], 
& yyNComb [ 504], & yyNComb [-463], & yyNComb [1454], & yyNComb [ 652], 
& yyNComb [ 966], & yyNComb [ 658], & yyNComb [ 974], & yyNComb [ 864], 
& yyNComb [ 565], & yyNComb [ 539], & yyNComb [-463], & yyNComb [ 873], 
& yyNComb [-463], & yyNComb [1540], & yyNComb [-463], & yyNComb [1509], 
& yyNComb [ 461], & yyNComb [ 758], & yyNComb [ 647], & yyNComb [ 602], 
& yyNComb [1283], & yyNComb [1451], & yyNComb [1546], & yyNComb [ 737], 
& yyNComb [-463], & yyNComb [ 419], & yyNComb [-463], & yyNComb [ 595], 
& yyNComb [ 435], & yyNComb [-463], & yyNComb [ 440], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [ 318], & yyNComb [-463], & yyNComb [ 452], 
& yyNComb [ 457], & yyNComb [ 269], & yyNComb [-463], & yyNComb [ 711], 
& yyNComb [1083], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 830], 
& yyNComb [ 469], & yyNComb [1070], & yyNComb [1419], & yyNComb [1044], 
& yyNComb [1043], & yyNComb [ 763], & yyNComb [-463], & yyNComb [ 681], 
& yyNComb [ 645], & yyNComb [-463], & yyNComb [ 690], & yyNComb [-463], 
& yyNComb [ 691], & yyNComb [-463], & yyNComb [1784], & yyNComb [1005], 
& yyNComb [1860], & yyNComb [ 658], & yyNComb [-463], & yyNComb [1342], 
& yyNComb [-463], & yyNComb [ 810], & yyNComb [ 777], & yyNComb [ 778], 
& yyNComb [-463], & yyNComb [ 729], & yyNComb [ 780], & yyNComb [ 919], 
& yyNComb [-463], & yyNComb [1719], & yyNComb [1731], & yyNComb [1146], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 941], & yyNComb [-259], 
& yyNComb [ 940], & yyNComb [1211], & yyNComb [1362], & yyNComb [1013], 
& yyNComb [1455], & yyNComb [-460], & yyNComb [ 947], & yyNComb [-463], 
& yyNComb [1269], & yyNComb [1036], & yyNComb [ 878], & yyNComb [ 773], 
& yyNComb [ 455], & yyNComb [-462], & yyNComb [-463], & yyNComb [ 457], 
& yyNComb [-463], & yyNComb [ 586], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 417], 
& yyNComb [1040], & yyNComb [ 860], & yyNComb [-463], & yyNComb [1457], 
& yyNComb [ 995], & yyNComb [ 897], & yyNComb [ 812], & yyNComb [1511], 
& yyNComb [1513], & yyNComb [-445], & yyNComb [-463], & yyNComb [ 652], 
& yyNComb [1277], & yyNComb [-463], & yyNComb [1169], & yyNComb [-463], 
& yyNComb [1159], & yyNComb [-463], & yyNComb [ 872], & yyNComb [-463], 
& yyNComb [1218], & yyNComb [ 870], & yyNComb [ 505], & yyNComb [1012], 
& yyNComb [ 759], & yyNComb [-463], & yyNComb [ 780], & yyNComb [-463], 
& yyNComb [  69], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 758], 
& yyNComb [-463], & yyNComb [ 646], & yyNComb [-463], & yyNComb [ 455], 
& yyNComb [1929], & yyNComb [-463], & yyNComb [ 820], & yyNComb [ 686], 
& yyNComb [ 911], & yyNComb [ 918], & yyNComb [-463], & yyNComb [1200], 
& yyNComb [1098], & yyNComb [-463], & yyNComb [-463], & yyNComb [ 715], 
& yyNComb [ 714], & yyNComb [1555], & yyNComb [1191], & yyNComb [1387], 
& yyNComb [1523], & yyNComb [ 889], & yyNComb [-463], & yyNComb [1542], 
& yyNComb [ 358], & yyNComb [1416], & yyNComb [1385], & yyNComb [ 695], 
& yyNComb [ 903], & yyNComb [ 460], & yyNComb [1105], & yyNComb [1559], 
& yyNComb [ 622], & yyNComb [ 905], & yyNComb [ 529], & yyNComb [1551], 
& yyNComb [ 919], & yyNComb [1886], & yyNComb [1340], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [ 738], & yyNComb [ 556], 
& yyNComb [-463], & yyNComb [1525], & yyNComb [1539], & yyNComb [ 538], 
& yyNComb [ 543], & yyNComb [1518], & yyNComb [1338], & yyNComb [-463], 
& yyNComb [1528], & yyNComb [ 367], & yyNComb [1495], & yyNComb [ 530], 
& yyNComb [-463], & yyNComb [ 703], & yyNComb [ 786], & yyNComb [1245], 
& yyNComb [-463], & yyNComb [1469], & yyNComb [1467], & yyNComb [1468], 
& yyNComb [-121], & yyNComb [ 690], & yyNComb [ 558], & yyNComb [ 527], 
& yyNComb [ 531], 
};
#ifdef YYTDefault
static	unsigned short	yyTDefault	[yyLastReadState + 1] = { 0,
 2937,  5045,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,  2947,     0,     0,     0, 
 3009,     0,  4699,  4758,     0,     0,  4616,  4709,  4710,     0, 
    0,     0,  4711,  4712,  4713,  4714,  4607,     0,     0,  4841, 
    0,     0,     0,     0,     0,     0,  4792,     0,  4639,  4640, 
    0,     0,     0,     0,     0,     0,  4807,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,  2937,  2966,  2950,  4780,     0,     0,     0, 
    0,     0,  4780,     0,     0,  3203,  4791,     0,     0,     0, 
    0,     0,  4674,  4670,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,  4592,  4595,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 2947,     0,  3166,  2970,     0,  2956,  3319,  4421,  4421,  3427, 
 4421,     0,  3319,     0,     0,  5050,     0,     0,  4641,  4641, 
    0,     0,     0,     0,     0,  4620,     0,     0,     0,     0, 
 4616,     0,     0,     0,  4838,  4832,     0,  4832,     0,  4792, 
 4838,  4838,  4832,  4832,  4498,     0,  4486,     0,  4030,  4721, 
    0,  4719,  4729,     0,     0,     0,     0,     0,     0,     0, 
    0,  2937,     0,     0,  2945,  3169,     0,  3059,  4421,     0, 
 2941,     0,  3322,     0,  4782,     0,  3206,     0,  3266,     0, 
    0,  3322,  4792,  4758,  4758,  3454,  4677,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,  4581,  4588,  4838, 
 4832,  4832,  4838,  4832,  4832,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,  4593,  4594,     0,  4596,  4597, 
    0,  2947,     0,     0,     0,     0,  3177,  3170,     0,     0, 
 3062,  2971,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,  4755,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,  3454,     0,  4671,  4678, 
 4645,  4609,     0,     0,  4119,     0,  4097,  4641,  4792,  4838, 
 4832,  4832,  4838,  4832,  4832,     0,     0,  4577,  4584,  4838, 
 4832,  4832,     0,  4838,  4838,  4832,  4832,     0,  4838,  4832, 
 4832,     0,  4838,  4838,  4832,  4832,     0,     0,  4487,     0, 
    0,     0,     0,     0,     0,     0,     0,  3180,  3167,     0, 
    0,  3141,  3063,  2967,  2955,  2955,     0,     0,  4818,  3361, 
    0,     0,     0,  3335,  3361,  4792,  4792,     0,     0,     0, 
    0,  3405,     0,  4772,     0,     0,     0,  4792,  4772,  4802, 
    0,     0,     0,     0,     0,     0,  4758,  4758,     0,     0, 
 4772,     0,     0,  4772,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,  3371,  4695,     0,     0,     0,  4804,  4786, 
 4818,     0,  4804,     0,     0,     0,     0,     0,     0,  4792, 
    0,  4792,     0,     0,  4101,  4119,  4854,  4854,  4102,     0, 
    0,  4838,  4838,  4540,  4546,  4838,  4832,  4832,  4838,  4832, 
 4832,     0,  4521,  4528,  4838,  4832,  4832,  4838,  4832,  4832, 
    0,  2936,     0,     0,     0,     0,     0,  3581,     0,     0, 
 3183,  4042,  3301,  3301,  3315,     0,  3060,     0,     0,     0, 
    0,     0,     0,     0,     0,  4766,  4836,     0,     0,     0, 
    0,  4780,     0,     0,     0,  3416,  3392,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 3434,  3432,  3435,  3433,  4798,     0,  3431,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,  4780,     0,     0,     0, 
    0,     0,     0,     0,     0,  4792,  4766,  4792,  4792,     0, 
    0,     0,  3499,  4766,     0,     0,     0,     0,     0,  4621, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,  3565, 
 3581,  3178,     0,     0,  3186,  3174,  3175,  4421,     0,  3315, 
    0,     0,  4421,  4814,  4814,     0,     0,     0,  3362,  3319, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,  4828,     0,  4768,  3259,     0,     0,     0,     0,  3258, 
 4768,  4788,     0,  3261,  3262,  4792,  4792,  3223,  3436,  3437, 
 3446,  4768,     0,     0,     0,     0,  4768,  4788,  4792,  3319, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,  4116,  4115,  4838,  4838,  4838,  4838,     0,     0,     0, 
 4703,  4704,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 4047,     0,  4838,  4061,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,  4812,     0,     0,     0,     0,     0,  3564,  3589, 
 3586,  3599,  3596,     0,     0,     0,  3567,  3301,  3181,     0, 
    0,  3193,  3315,  3139,     0,     0,  2977,  2975,  2988,  4814, 
 4814,     0,  4836,     0,  3366,  3322,  4637,  4459,     0,     0, 
    0,  4633,  4707,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,  3225,  3226,  3219,  3222,     0, 
    0,  3244,     0,  3253,  3295,     0,     0,  4822,     0,     0, 
 3438,     0,  3281,  3282,  3277,  3280,     0,  4822,     0,  3322, 
    0,     0,     0,  3456,     0,     0,     0,  3499,     0,     0, 
 4854,  4854,     0,  3595,     0,     0,  4862,     0,     0,     0, 
 3848,     0,  4701,  4637,  3864,  3885,  4868,  3896,  3899,  4860, 
    0,     0,  4858,     0,     0,     0,  4616,     0,  3943,  4874, 
 3936,     0,     0,  3997,  3999,  4001,     0,     0,     0,     0, 
 4046,     0,  4050,  4048,  4063,     0,  4834,  4077,  4089,     0, 
    0,  4122,     0,     0,     0,  4144,     0,     0,     0,     0, 
 4149,  4703,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,  4884,  3670,  4174,     0,     0,  4186,  4705,     0, 
 4858,     0,     0,     0,  4252,     0,     0,     0,     0,     0, 
    0,  4122,  4319,  4325,  4328,     0,     0,     0,     0,  4354, 
    0,     0,     0,  4613,  4252,  3590,     0,  3600,     0,     0, 
 3566,  3176,  3301,  3184,     0,     0,  3196,     0,     0,  4421, 
 4822,     0,     0,     0,     0,  3362,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,  4768,  3260,  3295,     0,     0,  3235,  3295,     0, 
    0,  3295,     0,  4762,  4768,  3289,     0,     0,     0,     0, 
 4768,  4768,  3289,     0,     0,     0,     0,     0,     0,     0, 
 3595,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,  3794,  4862,  4862,  3786,  3789,  3792,  4862,     0,     0, 
    0,     0,  3847,     0,     0,     0,  3877,     0,  3900,     0, 
    0,  3917,  4872,  3924,     0,     0,  3945,  4873,     0,     0, 
    0,  3956,  3940,     0,     0,  3847,  3995,     0,     0,     0, 
    0,  4019,     0,     0,     0,  3943,     0,  4064,     0,  4054, 
 4056,  4880,     0,     0,     0,     0,     0,     0,     0,  3847, 
    0,  4146,  4155,  4169,  4154,  4152,  4162,  4153,  4166,     0, 
 4862,     0,     0,     0,  3864,     0,     0,  4858,  3943,  4874, 
    0,     0,  4834,     0,  4858,     0,     0,  4252,     0,     0, 
 4319,     0,     0,     0,     0,  4252,     0,     0,     0,     0, 
    0,     0,     0,  4858,  4858,  4214,     0,     0,     0,     0, 
 4892,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 4311,  4792,  4896,     0,  4764,     0,  3847,     0,  4764,     0, 
    0,  4904,     0,  3179,  3301,  3187,     0,     0,     0,     0, 
 4776,     0,  4421,  3069,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,  4820,     0,     0,     0,  2991,     0,  2992, 
 2993,  2994,     0,     0,  3366,  4700,  3362,     0,     0,     0, 
    0,  4641,  4836,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,  3295,     0,  3295,     0,     0,  3295,  3295,     0, 
 3295,     0,     0,  3291,     0,     0,     0,  3291,     0,     0, 
    0,  4824,     0,     0,     0,     0,     0,  3762,     0,     0, 
    0,     0,  4862,  4862,     0,     0,     0,  3793,     0,  3789, 
    0,  3792,  3794,  4862,  4862,  4804,  4804,  4804,     0,     0, 
 4864,     0,  4864,     0,     0,  3863,  3867,  4684,     0,  4866, 
    0,  3890,  4868,     0,     0,     0,     0,  4870,  4871,  4794, 
    0,  4872,     0,  4874,     0,  3955,  4804,  4804,  4804,     0, 
    0,  4876,     0,  4000,  4005,     0,  4002,  4020,  4025,     0, 
 4878,     0,     0,     0,     0,     0,     0,     0,     0,  4792, 
    0,  4076,     0,     0,     0,     0,  4090,     0,     0,     0, 
    0,  4131,     0,  4882,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,  3786,  3789,  3792,  4862,     0,     0, 
    0,  3847,  3877,     0,  3920,  4872,  3945,     0,  3847,     0, 
    0,  3847,  4858,  4858,  4214,     0,     0,  4892,     0,     0, 
 4896,     0,     0,  3847,     0,  4904,     0,     0,     0,     0, 
    0,     0,  4214,  4214,     0,  4220,     0,     0,     0,  4891, 
 4892,     0,     0,     0,  4267,  4271,     0,     0,     0,  4301, 
 4297,  4297,  4301,     0,  4129,     0,     0,     0,  4895,  4896, 
 4340,  4624,     0,     0,     0,  4900,     0,     0,     0,  4641, 
 4641,  4753,  4753,  4903,     0,  4904,  4904,  4904,     0,  3182, 
 3191,  3194,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,  3069,     0,     0,  4792,     0,     0,     0,     0,     0, 
 3010,  3012,  4792,     0,  4792,     0,  4792,     0,     0,     0, 
    0,     0,     0,  3366,     0,     0,  3365,     0,     0,  3366, 
    0,     0,  3295,  3295,     0,  3295,  3295,     0,  4768,  4768, 
    0,     0,     0,  4808,     0,     0,     0,     0,  4862,     0, 
    0,  3774,  4792,  4792,  4792,  4766,  4792,     0,  3792,     0, 
 4862,  3791,  4862,     0,     0,     0,  4860,  4628,  4864,  4863, 
    0,  4864,     0,     0,  4864,     0,     0,  4792,     0,     0, 
    0,  4865,  4866,  4866,     0,     0,     0,     0,  4869,  4870, 
    0,  4794,     0,  4874,     0,  4792,     0,  4792,     0,  4792, 
 4792,     0,     0,     0,     0,     0,  4875,  4876,     0,     0, 
    0,     0,     0,     0,  4021,     0,     0,     0,     0,     0, 
 4880,     0,     0,     0,     0,     0,     0,  4091,     0,     0, 
    0,  4794,  4794,  4132,     0,  4881,  4882,  4148,     0,     0, 
    0,  4862,  4862,  3789,  3792,  4862,     0,     0,     0,  4864, 
    0,  4864,  4866,  4870,     0,  4874,     0,     0,  4876,     0, 
    0,     0,  4882,  4214,  4214,  4220,     0,     0,     0,     0, 
    0,     0,  4340,     0,     0,  4900,     0,  4753,     0,  4904, 
    0,  4176,  4178,     0,     0,  4220,  4220,     0,  4886,  4886, 
    0,  4888,  4227,     0,     0,     0,     0,  4894,     0,  4753, 
 4277,     0,     0,  4287,  4285,  4284,  4286,     0,  4309,  4788, 
 4317,  4838,     0,     0,     0,  4838,     0,     0,  4898,     0, 
    0,  4900,     0,  4899,  4900,     0,     0,     0,  4340,  4372, 
    0,     0,     0,     0,     0,     0,     0,  4904,     0,  3185, 
 3201,  3197,     0,     0,     0,     0,     0,  4776,     0,     0, 
    0,     0,     0,  4802,     0,  4838,  4802,  4792,  4766,     0, 
    0,  3088,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,  3055,     0,     0,  3033,  4758,  3002,  3362, 
 3363,  3364,     0,  4634,     0,  3295,  3293,  3293,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 4792,     0,     0,  4862,  4862,  3779,     0,  4864,     0,     0, 
    0,  3893,     0,  3895,     0,  3892,  3870,     0,     0,  3921, 
    0,     0,     0,     0,     0,  4876,  4668,     0,  4007,  4008, 
 4028,     0,     0,     0,     0,  4109,     0,     0,  4111,  4110, 
    0,     0,     0,  4126,  4127,     0,     0,     0,     0,  3792, 
 4862,     0,  4864,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,  4220,  4220,     0,     0, 
 4886,  4888,     0,     0,     0,  4898,  4900,     0,     0,  4340, 
    0,     0,     0,  4886,  4886,     0,  4885,     0,  4886,     0, 
    0,     0,     0,     0,     0,     0,  4888,     0,  4890,     0, 
    0,     0,  4680,     0,  4271,  4796,     0,     0,     0,     0, 
    0,     0,     0,  4810,     0,     0,     0,     0,     0,     0, 
    0,  4897,     0,  4898,     0,     0,  4641,  4900,     0,     0, 
    0,  4369,     0,     0,     0,  4798,     0,  4418,  3301,  3315, 
 3192,  3500,  3149,  3150,     0,  3148,     0,     0,     0,     0, 
    0,     0,     0,     0,  3118,     0,     0,  4792,     0,  4792, 
    0,     0,     0,  3081,     0,     0,     0,     0,     0,     0, 
 3019,  3024,  3021,  3047,  3044,     0,  3057,     0,     0,     0, 
 3366,     0,     0,  4808,     0,     0,  4399,     0,     0,  4401, 
    0,     0,  3882,     0,  3807,  4862,  4804,  4804,  4641,     0, 
    0,  4864,     0,     0,  4876,     0,  4876,  4668,     0,     0, 
 4119,     0,  4119,  4092,     0,     0,  4124,  4125,  4882,  4862, 
    0,     0,     0,  4876,  4668,     0,     0,  4886,  4886,     0, 
    0,     0,     0,     0,     0,     0,     0,  4369,     0,     0, 
 4886,  4886,  4886,     0,     0,     0,     0,     0,     0,     0, 
    0,  4894,  4277,     0,     0,  4792,     0,     0,     0,     0, 
    0,     0,     0,     0,  4900,     0,  4902,     0,     0,  4904, 
    0,  3188,  3203,  3315,  3195,     0,  3162,  3147,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,  3026,     0,     0,     0,  3037, 
    0,     0,     0,     0,  4808,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,  3844,  4864,  3894,  4794,  4876,  4876, 
    0,     0,  4119,  4121,  4792,  4882,     0,  4864,     0,     0, 
 4876,  4668,  4882,     0,     0,  4886,     0,  4900,  4902,  4904, 
    0,  4886,     0,  4221,  4235,     0,     0,     0,  4307,     0, 
    0,     0,  3878,  4900,     0,  4901,  4902,  4373,  4904,     0, 
 3198,  4780,  3315,     0,     0,  3146,     0,  3111,  3111,  3111, 
    0,  3111,  3111,     0,     0,  3138,  3138,  4700,  3094,  3103, 
 3100,     0,     0,     0,     0,     0,  4808,     0,     0,     0, 
 4629,     0,     0,  4876,  4876,     0,     0,  4876,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,  4308, 
    0,     0,     0,  3506,  3506,  3128,  3125,     0,     0,  3110, 
    0,  3073,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,  4876,     0,  4876,  4193,     0,     0,     0,     0, 
    0,     0,  3130,     0,     0,  3074,     0,     0,     0,  3104, 
    0,     0,     0,     0,     0,  4792,     0,  3527,  3525,     0, 
    0,  4792,     0,  4818,  3523,     0,     0,  3551,     0,  4792, 
    0,     0,     0,  4792,     0,  3132,  3126,  3071,  3072,     0, 
    0,     0,     0,  4792,  4792,     0,     0,  4792,  4836,     0, 
 4792,     0,     0,     0,     0,     0,     0,  4840,     0,  3563, 
 3563,  3563,     0,     0,     0,     0,     0, 
};
#endif
#ifdef YYNDefault
static	unsigned short	yyNDefault	[yyLastReadState + 1] = { 0,
    0,     0,     0,     6,   183,     0,     0,     0,     0,     0, 
  859,     0,  2178,  2178,    16,  2178,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,    17,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,   272,     0,     0,     0, 
    0,     0,     0,   183,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,    84,     0,     0,  1021,     0,     0, 
    0,     0,     0,   786,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   107,     0,     0,     0,     0,     0,     0,   786, 
    0,     0,     0,     0,     0,  2178,  1021,     0,     0,  1296, 
    0,     0,     0,   259,    84,   135,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
  272,     0,     0,     0,     0,     0,     0,   159,     0,     0, 
  159,     0,     0,     0,     0,     0,     0,  1021,     0,     0, 
  172,   400,     0,  1836,     0,     0,     0,     0,     0,  2178, 
    0,     0,     0,     0,     0,     0,     0,     0,  1959,   319, 
    0,     0,     0,     0,     0,     0,     0,  1959,     0,     0, 
    0,     0,     0,   208,   204,  1021,   208,   209,   135,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,   159,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,   407, 
    0,     0,     0,     0,     0,     0,     0,  1021,   440,   440, 
    0,     0,   172,     0,  1021,  2178,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,   262,  1959, 
   82,   263,   189,   265,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,   931,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,   931,     0,   530,   306,   770,     0,     0,     0,     0, 
    0,     0,     0,   316,     0,   435,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,    82,     0,     0, 
    0,     0,   354,   616,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,   365,   483,     0,     0,     0, 
  786,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,   511,   511,     0,     0,   786,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   416,     0,     0,   770,   308,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,   755,     0,     0, 
    0,     0,   786,   786,   989,     0,     0,     0,   435,   786, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,  2178,     0,     0,     0,     0,     0,     0,     0,     0, 
  259,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,   934,     0,     0,     0,     0,     0,     0,     0,     0, 
  770,   107,     0,     0,     0,   746,   746,   786,  1878,     0, 
    0,     0,     0,   761,   786,     0,     0,   616,   616,     0, 
    0,     0,   511,     0,  1846,   183,     0,   786,     0,     0, 
    0,     0,   786,     0,     0,     0,   107,     0,     0,   172, 
    0,     0,   770,     0,     0,     0,     0,     0,     0,     0, 
    0,  2178,  1275,     0,     0,     0,   770,   989,   989,     0, 
    0,     0,     0,     0,     0,     0,   616,   616,     0,   707, 
    0,     0,     0,     0,     0,   567,     0,   159,     0,     0, 
    0,     0,   159,     0,     0,   934,     0,   786,     0,     0, 
  730,   380,     0,     0,     0,     0,     0,     0,     0,   746, 
    0,     0,   746,     0,     0,   597,     0,   616,   616,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   597,   597,   616,     0,     0,     0,     0,     0, 
    0,   786,   786,   786,     0,   511,   787,   787,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,   793, 
    0,     0,  1836,   870,   801,     0,   897,   897,   808,  1321, 
    0,     0,   858,   859,     0,     0,     0,   860,  2178,     0, 
    0,     0,     0,     0,   259,  1059,   616,  2178,     0,  2213, 
  859,     0,   859,     0,  1351,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,   891,     0,     0,   172, 
 1092,   870,     0,   616,  1836,     0,   871,     0,   911,   993, 
  937,   703,   705,     0,     0,     0,   993,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,   482,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   746,   786,  2178,     0,     0,     0,     0,     0, 
  786,   600,   761,     0,   972,     0,   786,     0,   172,   172, 
    0,   786,     0,     0,     0,     0,   786,     0,   770,     0, 
    0,     0,   786,     0,     0,     0,     0,   609,   787,   787, 
    0,     0,   937,     0,     0,   786,  1071,     0,  2178,  1074, 
  824,     0,     0,   727,     0,   897,     0,     0,     0,     0, 
 1076,  1077,     0,     0,     0,     0,     0,     0,  1079,  1080, 
 1029,     0,     0,   937,     0,     0,     0,     0,  1836,     0, 
    0,   870,     0,   664,   824,   172,  1083,  1302,     0,  1836, 
    0,     0,  2178,     0,     0,     0,  1065,   847,  1066,  1068, 
    0,     0,  1836,   870,   646,  1321,     0,  2213,  1074,  1040, 
  259,   859,     0,     0,     0,     0,     0,     0,   691,   859, 
    0,     0,     0,  1814,  1643,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,  2178,     0, 
 1836,     0,     0,   172,     0,  1092,  2178,  2178,  1074,     0, 
    0,     0,     0,     0,     0,   701,     0,   703,     0,   786, 
  793,   567,     0,     0,     0,     0,     0,     0,     0,   159, 
    0,     0,     0,  1158,     0,     0,  2207,     0,     0,     0, 
    0,     0,   937,   786,     0,  1021,     0,     0,   786,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,   746, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,   746,     0,  1275,   972,     0,     0,     0,   972,     0, 
    0,     0,     0,     0,     0,   983,     0,   786,   786,     0, 
    0,     0,     0,   931,   172,     0,     0,   989,  2178,     0, 
  794,     0,   702,     0,   786,  1643,  1814,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,  1321,  1321,   824,   937,  1487,  1323,  1487,     0,     0, 
  204,     0,     0,     0,  2178,     0,  1327,     0,  1033,  1643, 
    0,     0,     0,     0,   859,  1013,     0,   755,  1038,   259, 
 1280,  1330,  1814,     0,     0,  1074,   824,     0,     0,     0, 
    0,     0,   705,  1292,     0,  1296,  1375,     0,  1836,  1013, 
    0,     0,  1065,     0,     0,     0,     0,     0,     0,   786, 
    0,     0,  2178,    84,     0,  1836,     0,     0,  2213,  2213, 
    0,   827,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,   647,  2178,  1074,     0,     0,     0,     0,   259,  1594, 
 1351,   824,     0,     0,     0,     0,     0,     0,     0,   769, 
    0,  1339,  1836,  1339,  2178,  2178,  1836,     0,  2213,     0, 
    0,     0,     0,  1342,     0,     0,  1013,     0,     0,     0, 
 1021,  1980,     0,   567,     0,     0,     0,     0,     0,     0, 
    0,     0,   159,     0,   786,     0,     0,     0,     0,     0, 
  786,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,   746, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,   746,   972,     0,   972,     0,     0,   972,   972,     0, 
  972,     0,   761,     0,   786,   786,   786,     0,     0,     0, 
  993,     0,     0,     0,     0,   701,     0,     0,     0,     0, 
    0,     0,  1552,     0,     0,     0,  1814,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,  1559, 
 1560,   859,  1560,     0,     0,     0,  2213,     0,     0,  1563, 
    0,  1247,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,  1566,     0,     0,     0,     0,     0,     0, 
  859,  1917,     0,     0,     0,     0,  1274,     0,     0,  1522, 
    0,  1814,  1280,     0,     0,     0,   172,     0,     0,     0, 
  705,     0,     0,   989,  1751,   174,   174,     0,     0,     0, 
 1544,  1836,   859,  1929,   859,   859,  1814,     0,     0,     0, 
  786,   998,     0,     0,     0,     0,     0,     0,     0,     0, 
 1836,  1013,     0,  1021,     0,     0,     0,   859,  1013,     0, 
    0,  1013,     0,     0,     0,     0,     0,     0,     0,  1339, 
    0,   883,     0,  1013,     0,  1132,   259,  1100,  1351,  1351, 
 1814,     0,     0,     0,  1836,  1597,  1836,  2178,  1578,     0, 
    0,     0,     0,  1581,     0,  1815,  1643,     0,  2213,     0, 
    0,     0,     0,  1643,     0,     0,     0,     0,     0,     0, 
 1583,     0,   897,  1559,   859,  1836,  2213,  1841,  1836,     0, 
    0,     0,     0,     0,     0,     0,  1980,     0,     0,   567, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,  1858, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,  1687, 
    0,  2207,  2207,     0,  1436,  2207,     0,     0,   440,     0, 
  746,   746,   972,   972,     0,   972,   972,   786,     0,     0, 
  701,     0,     0,     0,     0,  1836,     0,     0,     0,     0, 
 1814,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,  1643,  1643,  1643,     0,     0,  1763,     0, 
    0,     0,     0,     0,     0,   824,  1247,     0,  1487,  1487, 
 1487,     0,     0,     0,     0,     0,  2178,     0,     0,     0, 
 1814,     0,  2213,     0,     0,     0,     0,     0,     0,     0, 
    0,  1643,  1643,  1643,  1321,     0,     0,     0,  2213,   616, 
  616,   259,   259,  1814,     0,     0,   172,   172,  2178,   172, 
    0,     0,     0,  1537,     0,  1537,  1749,     0,   174,  1077, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,  1544,  1836, 
  859,  1560,     0,  1258,     0,     0,     0,   859,  1917,     0, 
  705,   859,  1929,     0,     0,  1356,  1836,     0,     0,     0, 
    0,     0,     0,  1559,   859,  1386,  1389,     0,     0,  1980, 
    0,     0,     0,  1836,   859,  1597,     0,     0,     0,  1781, 
    0,     0,     0,     0,  1961,  1814,   259,  1785,  2178,     0, 
    0,     0,  2178,   616,   616,  1615,  1614,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,  1786,     0, 
 1021,     0,     0,     0,     0,     0,   897,     0,  1836,     0, 
  440,   440,  2178,  1645,   859,     0,     0,     0,  1814,     0, 
    0,     0,   616,   616,     0,   616,     0,     0,     0,     0, 
    0,     0,  1413,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   786,     0,     0,     0,   786,     0,     0,     0, 
 1881,     0,  1885,     0,   786,     0,   183,    54,  1149,     0, 
    0,  1691,     0,     0,   746,   972,     0,     0,     0,     0, 
    0,  1899,  2178,     0,     0,  1814,     0,   786,  2213,   786, 
    0,  1643,  1643,     0,     0,     0,  1021,     0,     0,     0, 
 1321,  1247,   172,  1247,     0,  1247,  1247,  1814,  2178,     0, 
 1814,   786,  2213,   786,  1643,  1917,     0,  1321,     0,     0, 
    0,     0,     0,  2213,   989,  1749,     0,   989,   989,  1749, 
    0,  1967,  1878,   511,   511,  1321,  1814,  1836,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 1321,     0,     0,     0,     0,     0,  1597,  1597,     0,     0, 
    0,     0,     0,  1605,     0,     0,  1632,     0,     0,  1639, 
 1643,     0,     0,  1781,  1781,     0,     0,     0,     0,     0, 
    0,     0,     0,  1814,     0,     0,     0,     0,     0,  1814, 
    0,  1814,     0,     0,     0,     0,  2178,     0,     0,  2178, 
    0,  1077,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,   883,     0,     0,  1321,  2213, 
  897,     0,     0,     0,     0,     0,  1814,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,  2178,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,  2207,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
 2092,     0,     0,     0,  2092,     0,     0,     0,  1878,   786, 
    0,   746,     0,     0,     0,     0,     0,     0,   871,     0, 
 1814,  1814,     0,     0,   172,     0,     0,     0,     0,     0, 
 1814,  1560,  1487,     0,     0,  1836,  1836,     0,   172,     0, 
    0,   989,     0,     0,  1077,     0,     0,     0,  1836,     0, 
    0,     0,  1321,  1917,     0,  1321,  1321,  1781,  1781,     0, 
    0,     0,     0,  1814,     0,     0,  1321,     0,     0,   859, 
    0,     0,     0,     0,  1814,  1814,  1814,     0,  1021,     0, 
 1959,     0,     0,     0,     0,     0,  2095,  1077,  2213,     0, 
    0,     0,  1814,   440,  1386,     0,     0,  1836,  1836,     0, 
    0,   567,     0,     0,     0,     0,     0,     0,     0,     0, 
  616,     0,     0,     0,     0,     0,   400,   172,  2207,  2207, 
    0,  2207,  2207,  2092,  2092,     0,  2092,     0,     0,   786, 
  746,     0,  2098,     0,     0,   937,     0,   937,     0,  1814, 
 1643,  1643,   440,  1814,     0,     0,  1247,     0,  2145,     0, 
 1836,   172,     0,     0,     0,     0,     0,  1560,     0,  1836, 
 1917,     0,  1929,     0,     0,     0,     0,  1386,  1977,  1980, 
    0,     0,  1814,     0,     0,  1814,   824,  1077,     0,     0, 
    0,  1814,     0,     0,  1836,     0,     0,     0,     0,     0, 
    0,     0,     0,   786,     0,     0,  2207,  2082,  2082,  2082, 
    0,  2128,  2082,  2128,     0,     0,     0,     0,     0,     0, 
 2207,   400,     0,   746,     0,     0,     0,  1077,   871,     0, 
    0,     0,  1814,     0,  2145,  2057,     0,  2145,     0,  1836, 
    0,     0,     0,     0,     0,   259,     0,  1814,     0,     0, 
    0,     0,  1814,     0,     0,  2207,     0,  2156,     0,  2156, 
    0,  2156,     0,     0,   616,     0,   746,   616,  2098,     0, 
 1814,     0,     0,     0,     0,     0,     0,  1814,     0,  1836, 
 2152,     0,     0,     0,   616,     0,  2128,  2128,     0,     0, 
    0,     0,     0,  1814,  1814,     0,     0,     0,     0,     0, 
    0,     0,  2213,     0,     0,   786,     0,  1275,     0,     0, 
 2178,     0,  2178,     0,     0,     0,     0,  2156,  2156,     0, 
 1814,  1643,     0,     0,     0,  2178,  1643,     0,     0,     0, 
    0,  2213,     0,  1643,     0,   172,  1691,     0,     0,     0, 
    0,     0,  1110,   172,  1643,  1643,  1643, 
};
#endif
#if ! defined NO_RECOVER | defined YYDEC_TABLE
static	unsigned char	yyLength	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
    2,     2,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     2,     1,     6,     6,     0, 
    2,     4,     0,     3,     1,     1,     1,     0,     3,     0, 
    5,     5,     0,     3,     3,     4,     4,     0,     0,     3, 
    3,     3,     3,     3,     3,     2,     3,     3,     0,     3, 
    3,     3,     0,     0,     2,     2,     2,     3,     7,     3, 
    7,     4,     0,     1,     1,     1,     5,     0,     3,     0, 
    3,     5,     0,     2,     2,     2,     2,     2,     2,     2, 
    2,     2,     1,     2,     3,     4,     2,     1,     1,     1, 
    1,     1,     1,     2,     1,     2,     4,     4,     1,     2, 
    4,     4,     1,     1,     1,     2,     1,     3,     2,     2, 
    3,     1,     1,     1,     2,     3,     5,     1,     2,     3, 
    1,     2,     1,     2,     1,     2,     4,     1,     2,     1, 
    3,     4,     3,     5,     4,     5,     3,     0,     4,     0, 
    3,     0,     2,     2,     0,     0,     2,     2,     3,     6, 
    7,     0,     2,     7,     7,     5,     6,     5,     5,     5, 
    5,     5,     6,     2,     4,     4,     2,     2,     4,     3, 
    1,     3,     2,     4,     4,     4,     4,     4,     4,     4, 
    4,     4,     4,     5,     4,     4,     6,     8,     5,     5, 
    4,     3,     1,     0,     1,     2,     1,     1,     1,     1, 
    0,     1,     1,     2,     0,     1,     2,     1,     3,     2, 
    0,     3,     0,     3,     0,     4,     4,     5,     5,     4, 
    0,     3,     5,     0,     0,     2,     3,     5,     6,     5, 
    4,     4,     4,     6,     3,     5,     4,     2,     1,     1, 
    1,     1,     1,     2,     1,     3,     8,     3,     0,     2, 
    3,     0,     0,     2,     3,     2,     2,     2,     5,     0, 
    0,     5,     0,     0,     5,     0,     0,     5,     0,     0, 
    3,     2,     3,     0,     5,     0,     0,     5,     0,     0, 
    3,     2,     3,     0,     2,     0,     5,     5,     0,     2, 
    2,     2,     4,     4,     6,     6,     4,     6,     9,     4, 
    4,     4,     4,     4,     4,     3,     5,     4,     4,     9, 
    8,     8,     7,     8,     7,     7,     6,     5,     8,     7, 
    7,     6,     7,     6,     6,     5,     4,     8,     7,     7, 
    6,     7,     6,     6,     5,     4,     6,     5,     5,     4, 
    3,     3,     5,     3,     3,     4,     4,     4,     0,     2, 
    4,     4,     6,     6,     4,     6,     9,     4,     4,     4, 
    4,     4,     4,     4,     4,     5,     1,     2,     3,     3, 
    2,     0,     2,     0,     3,     0,     3,     0,     4,     2, 
    2,     1,     1,     0,     2,     2,     3,    10,    10,     5, 
    5,     4,     6,     7,     7,     6,     1,     0,     2,     4, 
    2,     0,     1,     1,     0,     2,     2,     2,     3,     3, 
    1,     3,     1,     7,     5,    10,     8,     1,     2,     2, 
    2,     3,     3,     3,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    2,     2,     2,     0,     0,     5,     5,     3,     0,     1, 
    2,     1,     2,     1,     3,     5,     0,     1,     3,     1, 
    1,     3,     3,     1,     1,     1,     1,     1,     1,     8, 
    7,     8,     1,    11,     0,     2,     4,     4,     4,     4, 
    4,     4,     4,     4,     4,     4,     4,     0,     2,     4, 
    4,     4,     6,     4,     3,     4,     1,     6,     0,     2, 
    4,     4,     4,     4,     4,     4,     1,     1,     4,     0, 
    2,     2,     3,     3,     3,     3,     3,     3,     4,     4, 
    5,     2,     2,     2,     1,     1,     1,     1,     0,     2, 
    2,     3,     3,     2,     3,     4,     0,     2,     4,     6, 
    7,     5,     5,     4,     5,     5,     3,     1,     3,     1, 
    3,     3,     3,     1,     2,     3,     3,     4,     3,     3, 
    3,     5,     2,     2,     1,     2,     1,     3,     2,     1, 
    3,     2,     2,     1,     2,     1,     1,     1,     3,     3, 
    3,     0,     0,     2,     2,     3,     4,     4,     0,     2, 
    2,     2,     1,     1,     1,     2,     2,     1,     1,     1, 
    1,     1,     1,     1,     3,     1,     5,     1,     5,     1, 
    5,     3,     3,     3,     3,     1,     3,     2,     2,     2, 
    3,     1,     3,     1,     2,     3,     1,     1,     1,     1, 
    1,     1,     1,     1,     4,     3,     2,     2,     3,     1, 
    1,     1,     1,     1,     1,     0,     5,     4,     6,     5, 
    3,     1,     0,     2,     2,     0,     1,     2,     1,     1, 
    3,     3,     6,     0,     3,     2,     4,     3,     1,     1, 
    2,     1,     2,     4,     1,     0,     2,     0,     1,     1, 
    2,     1,     2,     2,     2,     1,     1,     3,     2,     2, 
    1,     2,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     2,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     5,     4,     6,     5,     5,     3,     4,     5, 
    6,     7,     4,     5,     6,     7,     8,     6,     5,     4, 
    6,     5,     5,     3,     4,     5,     6,     7,     4,     5, 
    6,     7,     8,     6,     2,     6,     1,     1,     1,     1, 
    1,     1,     2,     2,     2,     2,     3,     4,     7,     1, 
    1,     4,     4,     4,     7,     7,     2,     2,     0,     2, 
    3,     0,     3,     2,     0,     2,     0,     2,     2,     2, 
    2,     2,     2,     2,     2,     2,     2,     2,     2,     4, 
    5,     2,     2,     2,     2,     2,     4,     2,     4,     4, 
    4,     4,     2,     2,     2,     2,     2,     2,     2,     2, 
    5,     6,     8,     9,     6,     7,     5,     6,     5,     6, 
    8,     9,     6,     7,     5,     6,     4,     5,     9,     0, 
    2,     1,     2,     3,     5,     5,     6,     6,     5,     6, 
    6,     1,     1,     2,     2,     2,     0,     1,     2,     1, 
    1,     1,     3,     1,     2,     1,     4,     3,     4,     0, 
    3,     4,     7,     3,     3,     4,     7,     2,     3,     5, 
    1,     2,     1,     1,     3,     3,     5,     3,     2,     1, 
    2,     1,     2,     4,     4,     5,     4,     3,     3,     1, 
    5,     6,     5,     6,     1,     1,     1,     4,     5,     3, 
    4,     5,     3,     3,     4,     7,     3,     6,     1,     2, 
    2,     1,     5,     6,     3,     5,     6,     3,     1,     2, 
    1,     2,     2,     2,     2,     0,     3,     0,     1,     2, 
    4,     4,     4,     2,     2,     2,     1,     2,     0,     2, 
    2,     2,     2,     3,     3,     2,     2,     2,     2,     2, 
    4,     2,     4,     4,     4,     3,     3,     5,     8,     7, 
   10,     9,     6,     9,     8,    11,    10,     5,     8,     7, 
   10,     9,     6,     9,     8,    11,    10,     3,     6,     2, 
    3,     2,     4,     2,     4,     1,     2,     1,     1,     3, 
    3,     5,     5,     1,     3,     1,     1,     1,     2,     3, 
    3,     0,     2,     3,     1,     3,     1,     1,     1,     1, 
    3,     1,     1,     5,     9,     8,     0,     1,     1,     2, 
    1,     1,     1,     2,     1,     4,     1,     2,     1,     1, 
    2,     3,     2,     3,     2,     2,     2,     4,     2,     5, 
    1,     1,     2,     1,     2,     2,     3,     6,     1,     2, 
    4,     4,     2,     3,     1,     3,     1,     3,     2,     0, 
    1,     2,     4,     1,     1,     1,     1,     1,     1,     1, 
    1,     2,     4,     5,     7,     8,     0,     1,     2,     3, 
    1,     2,     2,     2,     2,     1,     2,     2,     1,     2, 
    4,     2,     2,     2,     1,     2,     4,     3,     3,     6, 
    6,     0,     7,     8,     0,     2,     4,     4,     3,     3, 
    4,     0,     4,     4,     5,     5,     8,     6,     9,     5, 
    8,     6,     9,     2,     6,     3,     0,     2,     0,     2, 
    0,     2,     1,     2,     2,     2,     2,     2,     1,     2, 
    1,     2,     1,     2,     1,     2,     4,     3,     1,     4, 
    3,     1,     3,     1,     1,     2,     2,     4,     5,     5, 
    5,     2,     4,     5,     5,     5,     2,     1,     1,     3, 
    3,     3,     0,     1,     3,     7,     2,     7,     8,     7, 
    8,     6,     7,     8,     9,     6,     7,     8,     7,     8, 
    6,     7,     8,     9,     6,     2,     0,     2,     3,     3, 
    3,     2,     0,     3,     4,     7,     3,     0,     4,     0, 
    2,     6,     7,     6,     7,     1,     1,     3,     2,     3, 
    5,     6,     2,     5,     2,     7,     7,     3,     4,     5, 
    4,     5,     2,     2,     0,     1,     6,     8,     6,     8, 
    2,     0,     1,     2,     3,     1,     3,     3,     1,     4, 
    6,     8,     1,     0,     3,     3,     2,     1,     2,     0, 
    2,     3,     5,     7,     5,     7,     5,     5,     5,     5, 
    6,     4,     4,     4,     5,     1,     2,     3,     1,     0, 
    2,     3,     3,     0,     2,     3,     3,     7,     8,     8, 
    9,     5,     4,     0,     4,     4,     5,     4,     5,     3, 
    6,     0,     1,     1,     1,     1,     1,     2,     3,     3, 
    2,     2,     6,     7,     6,     7,     1,     2,     4,     1, 
    1,     3,     0,     5,     8,     6,     6,     9,     7,     5, 
    8,     6,     6,     9,     7,     2,     2,     7,     3,     3, 
    8,     9,     8,     9,     3,     0,     1,     3,     2,     1, 
    3,     0,     1,     2,     1,     4,     4,     7,     8,     9, 
    9,     5,     5,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     2,     0,     1,     1,     0,     1,     2,     1, 
    4,     1,     3,     1,     3,     2,     5,     4,     8,     6, 
    5,     9,     5,     5,     4,     8,     6,     5,     9,     5, 
    3,     4,     7,     0,     2,     2,     2,     4,     6,     1, 
    1,     1,     0,     3,     0,     0,     1,     2,     3,     3, 
    2,     0,     1,     2,     3,     1,     3,     3,     1,     4, 
    0,     4,     1,     3,     3,     1,     1,     2,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     3, 
    1,     3,     1,     2,     3,     2,     4,     6,     3,     4, 
    1,     3,     4,     5,     3,     3,     1,     1,     1,     1, 
    1,     1,     1,     1,     3,     1,     3,     3,     1,     4, 
    4,     3,     2,     3,     3,     1,     4,     4,     6,     2, 
    3,     3,     4,     4,     6,     2,     3,     3,     4,     4, 
    3,     2,     3,     3,     4,     4,     6,     2,     3,     3, 
    4,     4,     6,     2,     3,     3,     1,     3,     3,     1, 
    4,     4,     6,     2,     3,     3,     4,     4,     6,     2, 
    2,     2,     3,     3,     2,     1,     2,     2,     1,     3, 
    3,     5,     1,     2,     2,     1,     3,     3,     5,     1, 
    2,     2,     1,     1,     1,     3,     3,     1,     3,     3, 
    1,     3,     2,     2,     1,     1,     1,     3,     3,     1, 
    5,     5,     2,     6,     1,     1,     5,     5,     1,     3, 
    1,     3,     1,     5,     2,     6,     1,     5,     2,     0, 
    1,     5,     2,     6,     1,     1,     5,     2,     6,     1, 
    3,     1,     1,     0,     2,     2,     1,     1,     3,     3, 
    4,     5,     1,     2,     1,     2,     1,     2,     1,     2, 
    1,     2,     1,     2,     1,     2,     1,     2,     1,     2, 
    1,     2,     2,     5,     3,     6,     1,     1,     1,     1, 
    2,     1,     1,     2,     2,     1,     1,     3,     1,     1, 
    3,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     3,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     3,     3,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     2,     2,     2,     2, 
    2,     2,     2,     2,     1,     0,     1,     0,     1,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     1,     0,     1, 
    0,     1,     0,     1,     0,     1,     0,     4,     2,     2, 
    2,     2,     2,     2,     2,     2,     2,     2,     2,     2, 
    2,     1,     2,     3,     2,     6,     5,     5,     2,     2, 
    2,     4,     2,     2,     3,     3,     2,     2,     3,     2, 
    2,     3,     2,     2,     2,     1,     3,     3,     2,     2, 
    2,     2,     1,     3,     3,     2,     2,     2,     3,     3, 
    2,     2,     2,     3,     3,     2,     2,     2,     2,     3, 
    3,     2,     2,     2,     3,     3,     2,     3,     3,     2, 
    2,     1,     2,     2,     2,     4,     3,     3,     2,     3, 
    3,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     0,     2,     2, 
    2,     2,     3,     0,     0,     0, 
};
static	yytNonterminal	yyLeftHandSide	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNTprograms,
yyNTprogram_end_o,
yyNTprogram_end_o,
yyNTprogram_end,
yyNTprogram_l,
yyNTprogram_l,
yyNTprogram,
yyNTsce,
yyNTidentification_division,
yyNTenvironment_division_o,
yyNTdata_division_o,
yyNTprocedure_division_o,
yyNTprocedure_division_o,
yyNTidentification_o,
yyNTidentification_o,
yyNTprogram_id_o,
yyNTprogram_id_o,
yyNTprogram_id_o,
yyNTprogram_o,
yyNTprogram_o,
yyNTprogram_o,
yyNTprogram_o,
yyNTprogram_o,
yyNTidentification_l,
yyNTidentification_l,
yyNTidentification_l,
yyNTidentification_l,
yyNTidentification_l,
yyNTidentification_l,
yyNTidentification_l,
yyNTidentification_l,
yyNTenvironment_division,
yyNTenvironment_o,
yyNTenvironment_o,
yyNTconfiguration_section_o,
yyNTinput_output_section_o,
yyNTconfiguration_o,
yyNTconfiguration_o,
yyNTconfiguration_l,
yyNTconfiguration_l,
yyNTconfiguration_l,
yyNTconfiguration_l,
yyNTsource_computer_o,
yyNTsource_computer_o,
yyNTobject_computer_o,
yyNTobject_computer_o,
yyNTmemory_o,
yyNTmemory_o,
yyNTmemory_unit,
yyNTmemory_unit,
yyNTmemory_unit,
yyNTprogram_sequence_o,
yyNTprogram_sequence_o,
yyNTsegment_limit_o,
yyNTsegment_limit_o,
yyNTspecial_names_o,
yyNTspecial_names_o,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTspecial_names_l,
yyNTimplementor_name_l,
yyNTimplementor_name_l,
yyNTimplementor_name_e,
yyNTimplementor_name_e,
yyNTimplementor_name_e,
yyNTimplementor_name,
yyNTimplementor_name,
yyNTbuiltin_name,
yyNTbuiltin_name,
yyNTbuiltin_name,
yyNTon_off,
yyNTon_off,
yyNTon_off,
yyNTon_off,
yyNTon_status,
yyNToff_status,
yyNTalphabet_names,
yyNTalphabet_names,
yyNTalphabet_entry,
yyNTalphabet_entry,
yyNTalphabet_name,
yyNTalphabet_name,
yyNTalphabet_l,
yyNTalphabet_l,
yyNTalphabet_e,
yyNTalphabet_e,
yyNTalphabet_e,
yyNTalso_l,
yyNTalso_l,
yyNTalphabet_or_class_literal,
yyNTalphabet_or_class_literal,
yyNTsymbolic_characters,
yyNTsymbolic_characters,
yyNTsymbolic_entry,
yyNTsymbolic_entry,
yyNTsymbolic_l,
yyNTsymbolic_l,
yyNTsymbolic_e,
yyNTsymbolic_character_l,
yyNTsymbolic_character_l,
yyNTinteger_l,
yyNTinteger_l,
yyNTclass_names,
yyNTclass_names,
yyNTclass,
yyNTclass_l,
yyNTclass_l,
yyNTclass_e,
yyNTclass_e,
yyNTcurrency_o,
yyNTdecimal_point_o,
yyNTnumeric_sign_o,
yyNTcall_convention_o,
yyNTconsole_o,
yyNTcursor_o,
yyNTcursor_o,
yyNTcrt_status_o,
yyNTcrt_status_o,
yyNTinput_output_o,
yyNTinput_output_o,
yyNTfile_control,
yyNTfile_control_o,
yyNTfile_control_o,
yyNTfile_control_entry_l,
yyNTfile_control_entry_l,
yyNTfile_control_entry_l,
yyNTfile_control_entry_l,
yyNTfile_control_entry,
yyNTfile_control_entry,
yyNTselect_clause_l,
yyNTselect_clause_l,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTselect_clause,
yyNTassign_o,
yyNTassign_o,
yyNTassign_l,
yyNTassign_l,
yyNTassign_e,
yyNTassign_e,
yyNTexternal_o,
yyNTexternal_o,
yyNTexternal_o,
yyNTinteger_or_no,
yyNTinteger_or_no,
yyNTorganization_is_o,
yyNTorganization_is_o,
yyNTalternate_l,
yyNTalternate_l,
yyNTalternate_e,
yyNTalternate_e,
yyNTduplicates_o_2,
yyNTduplicates_o_2,
yyNTpassword_o,
yyNTpassword_o,
yyNTsuppress_o,
yyNTsuppress_o,
yyNTlock_o,
yyNTlock_o,
yyNTlock_o,
yyNTlock_o,
yyNTlock_o,
yyNTlock_o,
yyNTi_o_control_o,
yyNTi_o_control_o,
yyNTi_o_control_o,
yyNTi_o_control_l,
yyNTi_o_control_l,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTi_o_control_e,
yyNTrerun_e,
yyNTrerun_e,
yyNTrerun_e,
yyNTrerun_e,
yyNTrerun_e,
yyNTsame,
yyNTsame,
yyNTsame,
yyNTmultiple_l,
yyNTmultiple_l,
yyNTmultiple_e,
yyNTmultiple_e,
yyNTdata_division,
yyNTdata_o,
yyNTdata_o,
yyNTfile_section_o,
yyNTfile_o,
yyNTfile_o,
yyNTfile_description_l,
yyNTfile_description_l,
yyNTfile_description_l,
yyNTfile_description_l,
yyNTfile_description_e,
yyNTfile_description_e,
yyNTworking_storage_section_o,
yyNTworking_storage_section_o,
yyNTxx_working_storage_section_o_1_4,
yyNTlocal_storage_section_o,
yyNTlocal_storage_section_o,
yyNTxx_local_storage_section_o_1_4,
yyNTlinkage_section_o,
yyNTlinkage_section_o,
yyNTxx_linkage_section_o_1_4,
yyNTcommunication_section_o,
yyNTcommunication_section_o,
yyNTxx_communication_section_o_1_4,
yyNTcommunication_description_l,
yyNTcommunication_description_l,
yyNTcommunication_description_l,
yyNTcommunication_description_l,
yyNTreport_section_o,
yyNTreport_section_o,
yyNTxx_report_section_o_1_4,
yyNTscreen_section_o,
yyNTscreen_section_o,
yyNTxx_screen_section_o_1_4,
yyNTreport_description_entry_l,
yyNTreport_description_entry_l,
yyNTreport_description_entry_l,
yyNTreport_description_entry_l,
yyNTreport_group_description_entry_l,
yyNTreport_group_description_entry_l,
yyNTfile_description_entry,
yyNTsort_merge_file_description_entry,
yyNTfile_clause_l,
yyNTfile_clause_l,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTfile_clause,
yyNTsort_merge_clause_l,
yyNTsort_merge_clause_l,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTsort_merge_clause,
yyNTvalue_l,
yyNTvalue_l,
yyNTvalue_e,
yyNTvalue_e,
yyNTfrom_o,
yyNTfrom_o,
yyNTto_o,
yyNTto_o,
yyNTdepending_o,
yyNTdepending_o,
yyNTbottom_o,
yyNTbottom_o,
yyNTfooting,
yyNTtop,
yyNTbottom,
yyNTlinage,
yyNTlinage,
yyNTdata_description_entry_l,
yyNTdata_description_entry_l,
yyNTdata_description_entry_l,
yyNTdata_description_entry_l,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTdata_description_entry,
yyNTperiod_o,
yyNTperiod_o,
yyNTrenames_tail,
yyNTrenames_tail,
yyNTredefines_o,
yyNTredefines_o,
yyNTname_or_Filler,
yyNTname_or_Filler,
yyNTdata_clause_l,
yyNTdata_clause_l,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTdata_clause,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTusage,
yyNTsign_1,
yyNTsign_1,
yyNTseparate_o,
yyNTseparate_o,
yyNTa_de_scending_l,
yyNTa_de_scending_l,
yyNTa_de_scending_l,
yyNTindexed_o,
yyNTindexed_o,
yyNTindexed_l,
yyNTindexed_l,
yyNTcondition_l,
yyNTcondition_l,
yyNTdd_condition_e,
yyNTdd_condition_e,
yyNTfalse_o,
yyNTfalse_o,
yyNTconst_expression,
yyNTconst_expression,
yyNTconst_primary,
yyNTconst_primary,
yyNTconst_primary,
yyNTconst_primary,
yyNToperator,
yyNToperator,
yyNToperator,
yyNToperator,
yyNToperator,
yyNToperator,
yyNTcommunication_description_entry,
yyNTcommunication_description_entry,
yyNTcommunication_description_entry,
yyNTcd_input_o,
yyNTcd_input_o,
yyNTcd_input_l,
yyNTcd_input_l,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_input_e,
yyNTcd_output_l,
yyNTcd_output_l,
yyNTcd_output_e,
yyNTcd_output_e,
yyNTcd_output_e,
yyNTcd_output_e,
yyNTcd_output_e,
yyNTcd_output_e,
yyNTcd_output_e,
yyNTcd_i_o_o,
yyNTcd_i_o_o,
yyNTcd_i_o_l,
yyNTcd_i_o_l,
yyNTcd_i_o_e,
yyNTcd_i_o_e,
yyNTcd_i_o_e,
yyNTcd_i_o_e,
yyNTcd_i_o_e,
yyNTcd_i_o_e,
yyNTname_or_filler,
yyNTname_or_filler,
yyNTreport_description_entry,
yyNTreport_clause_l,
yyNTreport_clause_l,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTreport_clause,
yyNTlimit_o,
yyNTlimit_o,
yyNTlimit_o,
yyNTlimit_o,
yyNTline_o,
yyNTline_o,
yyNTpage_l,
yyNTpage_l,
yyNTpage_e,
yyNTpage_e,
yyNTpage_e,
yyNTpage_e,
yyNTreport_group_description_entry,
yyNTreport_group_description_entry,
yyNTreport_group_clause_l,
yyNTreport_group_clause_l,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTreport_group_clause,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTtype,
yyNTname_or_final,
yyNTname_or_final,
yyNTsign_2,
yyNTsign_2,
yyNTreset_o,
yyNTreset_o,
yyNTscreen_description_entry_l,
yyNTscreen_description_entry_l,
yyNTscreen_description_entry_l,
yyNTscreen_description_entry_l,
yyNTscreen_description_entry,
yyNTscreen_description_entry,
yyNTscreen_clause_l,
yyNTscreen_clause_l,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTscreen_clause,
yyNTsign_line_o,
yyNTsign_line_o,
yyNTsign_line_o,
yyNTsign_line_o,
yyNTprocedure_division,
yyNTprocedure_division,
yyNTprocedure_division,
yyNTprocedure_division,
yyNTprocedure_head,
yyNTmnemonic_name_o,
yyNTmnemonic_name_o,
yyNTusing_o,
yyNTusing_o,
yyNTusing_o,
yyNTusing_l,
yyNTusing_l,
yyNTusing_1_e,
yyNTusing_1_e,
yyNTusing_2_e,
yyNTusing_2_e,
yyNTdeclaratives_o,
yyNTdeclaratives_o,
yyNTd_section_l,
yyNTd_section_l,
yyNTd_section_l,
yyNTd_section_l,
yyNTsection_l,
yyNTsection_l,
yyNTsection_l,
yyNTsection_e,
yyNTsection_e,
yyNTsection_head,
yyNTsegment_number_o,
yyNTsegment_number_o,
yyNTuse_o,
yyNTuse_o,
yyNTparagraph_l,
yyNTparagraph_l,
yyNTparagraph_l,
yyNTparagraph_e,
yyNTparagraph_e,
yyNTparagraph_head,
yyNTsentence_l,
yyNTsentence_l,
yyNTsentence_l,
yyNTsentence_l,
yyNTsentence_l,
yyNTsentence_l,
yyNTstatement_l,
yyNTstatement_l,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTstatement,
yyNTimperative_statement,
yyNTimperative_statement,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTstatement_i,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTaccept_i,
yyNTexception_2,
yyNTexception_2,
yyNTaccept_name,
yyNTaccept_name,
yyNTaccept_from,
yyNTaccept_from,
yyNTaccept_from,
yyNTaccept_from,
yyNTaccept_from,
yyNTaccept_from,
yyNTaccept_from,
yyNTaccept_from,
yyNTescape,
yyNTescape,
yyNTescape,
yyNTexception_or_escape,
yyNTexception_or_escape,
yyNTline_column,
yyNTline_column,
yyNTline_column,
yyNTline_column,
yyNTline_column,
yyNTline_column,
yyNTfrom_crt_o,
yyNTfrom_crt_o,
yyNTfrom_crt,
yyNTmode_block_o,
yyNTmode_block_o,
yyNTmode_block,
yyNTwith_o,
yyNTwith_o,
yyNTwith,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTwith_l,
yyNTadd,
yyNTadd,
yyNTadd,
yyNTadd,
yyNTadd,
yyNTadd,
yyNTadd,
yyNTadd,
yyNTadd_i,
yyNTadd_i,
yyNTadd_i,
yyNTadd_i,
yyNTadd_i,
yyNTadd_i,
yyNTadd_i,
yyNTadd_i,
yyNTsize_error,
yyNTsize_error,
yyNTsize_error,
yyNTgiving,
yyNTalter,
yyNTalter_l,
yyNTalter_l,
yyNTalter_e,
yyNTalter_e,
yyNTcall,
yyNTcall,
yyNTcall,
yyNTcall_i,
yyNTcall_i,
yyNTcall_i,
yyNTcall_name,
yyNTcall_name,
yyNTcall_name,
yyNTcall_name,
yyNTcall_using_o,
yyNTcall_using_o,
yyNTcall_using_l,
yyNTcall_using_l,
yyNTcall_using_1_e,
yyNTcall_using_1_e,
yyNTcall_using_2_e,
yyNTcall_using_2_e,
yyNTcall_chain_l,
yyNTcall_chain_l,
yyNTcall_chain_e,
yyNTcall_chain_e,
yyNTgiving_o,
yyNTgiving_o,
yyNTgiving_o,
yyNToverflow,
yyNToverflow,
yyNToverflow,
yyNTon_overflow,
yyNTexception,
yyNTexception,
yyNTexception,
yyNTcancel,
yyNTchain,
yyNTchain,
yyNTchain_l,
yyNTchain_l,
yyNTchain_1_e,
yyNTchain_1_e,
yyNTchain_2_e,
yyNTchain_2_e,
yyNTchain_2_e,
yyNTchain_2_e,
yyNTclose,
yyNTclose_l,
yyNTclose_l,
yyNTclose_e,
yyNTclose_e,
yyNTclose_e,
yyNTclose_e,
yyNTclose_e,
yyNTclose_e,
yyNTclose_e,
yyNTclose_e,
yyNTcommit,
yyNTcompute,
yyNTcompute,
yyNTcompute_i,
yyNTcompute_i,
yyNTequal_2,
yyNTequal_2,
yyNTcontinue,
yyNTdelete,
yyNTdelete,
yyNTdelete,
yyNTdelete_i,
yyNTdelete_i,
yyNTdelete_i,
yyNTinvalid,
yyNTinvalid,
yyNTinvalid,
yyNTdisable,
yyNTdisable,
yyNTdevice,
yyNTdevice,
yyNTdevice,
yyNTdevice,
yyNTdisplay,
yyNTdisplay,
yyNTdisplay,
yyNTdisplay_i,
yyNTdisplay_i,
yyNTdisplay_i,
yyNTdisplay_l,
yyNTdisplay_l,
yyNTdisplay_2_l,
yyNTdisplay_2_l,
yyNTdisplay_2_e,
yyNTupon_o,
yyNTupon_o,
yyNTupon_o,
yyNTdisplay_advancing_o,
yyNTdisplay_advancing_o,
yyNTdisplay_3_l,
yyNTdisplay_3_l,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTdisplay_3_e,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTwith_display_l,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTdivide_i,
yyNTenable,
yyNTenable,
yyNTenter,
yyNTenter,
yyNTentry,
yyNTentry,
yyNTentry,
yyNTentry,
yyNTentry_l,
yyNTentry_l,
yyNTentry_1_e,
yyNTentry_1_e,
yyNTentry_2_e,
yyNTentry_2_e,
yyNTevaluate,
yyNTevaluate_i,
yyNTevaluate_expression_l,
yyNTevaluate_expression_l,
yyNTevaluate_expression,
yyNTevaluate_expression,
yyNTevaluate_expression,
yyNTcase_l,
yyNTcase_l,
yyNTwhen_other_o,
yyNTwhen_other_o,
yyNTwhen_l,
yyNTwhen_l,
yyNTwhen_label_l,
yyNTwhen_label_l,
yyNTwhen_label,
yyNTwhen_label,
yyNTwhen_label,
yyNTwhen_label,
yyNTwhen_label,
yyNTexpression_or_literal,
yyNTexpression_or_literal,
yyNTexamine,
yyNTexamine,
yyNTexamine,
yyNTexamine_repl,
yyNTexamine_repl_o,
yyNTexamine_repl_o,
yyNTexamine_repl_o,
yyNTexamine_repl_o,
yyNTexamine_tally_o,
yyNTexamine_tally_o,
yyNTexamine_tally_o,
yyNTexecute,
yyNTexhibit,
yyNTexhibit_o,
yyNTexhibit_o,
yyNTexhibit_o,
yyNTexit,
yyNTexit,
yyNTexit,
yyNTexit,
yyNTexit,
yyNTexit,
yyNTexit,
yyNTgiving_2_o,
yyNTgiving_2_o,
yyNTgiving_2_o,
yyNTgiving_2_o,
yyNTgiving_or_returning,
yyNTgiving_or_returning,
yyNTgenerate,
yyNTgoback,
yyNTgoback,
yyNTgoto,
yyNTgoto,
yyNTgoto,
yyNTprocedure_name_l,
yyNTprocedure_name_l,
yyNTif,
yyNTif_i,
yyNTthen,
yyNTelse,
yyNTelse,
yyNTelse_i,
yyNTelse_i,
yyNTinitialize,
yyNTreplacing_o,
yyNTreplacing_o,
yyNTinitialize_replacing_l,
yyNTinitialize_replacing_l,
yyNTinitialize_replacing_e,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTreplacing_mode,
yyNTinitiate,
yyNTinspect,
yyNTinspect,
yyNTinspect,
yyNTinspect,
yyNTinspect_repl,
yyNTtallying_l,
yyNTtallying_l,
yyNTtallying_e,
yyNTfor_l,
yyNTfor_l,
yyNTfor_e,
yyNTfor_e,
yyNTfor_e,
yyNTall_leading_l,
yyNTall_leading_l,
yyNTall_leading_e,
yyNTreplacing_l,
yyNTreplacing_l,
yyNTreplacing_e,
yyNTreplacing_e,
yyNTreplacing_e,
yyNTreplacing_e,
yyNTall_leading_first_l,
yyNTall_leading_first_l,
yyNTall_leading_first_e,
yyNTbefore_after_o,
yyNTbefore_after_o,
yyNTbefore_after_o,
yyNTbefore_after_o,
yyNTbefore_after_o,
yyNTmerge,
yyNTmerge,
yyNTsort_merge_l,
yyNTsort_merge_l,
yyNTsort_merge_e,
yyNTsort_merge_e,
yyNTsort_merge_e,
yyNTsort_merge_e,
yyNTsequence_o,
yyNTsequence_o,
yyNToutput,
yyNTmove,
yyNTmove,
yyNTmultiply,
yyNTmultiply,
yyNTmultiply,
yyNTmultiply,
yyNTmultiply_i,
yyNTmultiply_i,
yyNTmultiply_i,
yyNTmultiply_i,
yyNTnext_sentence,
yyNTon,
yyNTon_every_o,
yyNTon_every_o,
yyNTon_until_o,
yyNTon_until_o,
yyNTon_else_o,
yyNTon_else_o,
yyNTopen,
yyNTopen_l,
yyNTopen_l,
yyNTopen_e,
yyNTopen_e,
yyNTopen_e,
yyNTopen_e,
yyNTfile_name_1_l,
yyNTfile_name_1_l,
yyNTfile_name_2_l,
yyNTfile_name_2_l,
yyNTfile_name_3_l,
yyNTfile_name_3_l,
yyNTfile_name_1,
yyNTfile_name_1,
yyNTfile_name_1,
yyNTfile_name_1,
yyNTfile_name_2,
yyNTfile_name_2,
yyNTfile_name_2,
yyNTfile_name_3,
yyNTfile_name_3,
yyNTfile_name,
yyNTfile_name_l,
yyNTfile_name_l,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform,
yyNTperform_body,
yyNTperform_body,
yyNTprocedure,
yyNTprocedure,
yyNTtest_o,
yyNTtest_o,
yyNTtest_o,
yyNTvarying_l,
yyNTvarying_l,
yyNTvarying_e,
yyNTpurge,
yyNTread,
yyNTread,
yyNTread,
yyNTread,
yyNTread,
yyNTread,
yyNTread,
yyNTread,
yyNTread,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTread_i,
yyNTinto_o,
yyNTinto_o,
yyNTwith_read_o,
yyNTwith_read_o,
yyNTwith_read_o,
yyNTwith_read_o,
yyNTwith_read_o,
yyNTwith_read_o,
yyNTend,
yyNTend,
yyNTend,
yyNTend_o,
yyNTend_o,
yyNTnot_end_o,
yyNTnot_end_o,
yyNTready_trace,
yyNTreceive,
yyNTreceive,
yyNTreceive_i,
yyNTreceive_i,
yyNTmessage_segment,
yyNTmessage_segment,
yyNTdata,
yyNTdata,
yyNTdata,
yyNTdata,
yyNTdata,
yyNTrelease,
yyNTrelease,
yyNTreset_trace,
yyNTreturn,
yyNTreturn_i,
yyNTreturn_end,
yyNTrewrite,
yyNTrewrite,
yyNTrewrite_i,
yyNTrewrite_i,
yyNTfrom_2_o,
yyNTfrom_2_o,
yyNTfrom_2_o,
yyNTrollback,
yyNTsearch,
yyNTsearch,
yyNTsearch_i,
yyNTsearch_i,
yyNTvarying_o,
yyNTvarying_o,
yyNTsearch_when_l,
yyNTsearch_when_l,
yyNTwhen_e,
yyNTsearch_l,
yyNTsearch_l,
yyNTsearch_e,
yyNTsearch_e,
yyNTsend,
yyNTsend,
yyNTsend,
yyNTadvancing_o,
yyNTadvancing_o,
yyNTadvancing,
yyNTadvancing,
yyNTadvance,
yyNTadvance,
yyNTsend_replacing_o,
yyNTsend_replacing_o,
yyNTservice,
yyNTservice,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset,
yyNTset_l,
yyNTset_l,
yyNTset_e,
yyNTset_e,
yyNTon_off_l,
yyNTon_off_l,
yyNTon_off_e,
yyNTon_off_e,
yyNTtrue_false_l,
yyNTtrue_false_l,
yyNTtrue_false_e,
yyNTtrue_false_e,
yyNTsort,
yyNTsort,
yyNTsort,
yyNTsort,
yyNTsort,
yyNTduplicates_o,
yyNTduplicates_o,
yyNTinput,
yyNTstart,
yyNTstart,
yyNTstart_i,
yyNTstart_i,
yyNTkey_o,
yyNTkey_o,
yyNTkey_o,
yyNTstart_operator,
yyNTstart_operator,
yyNTstart_operator,
yyNTstart_operator,
yyNTstart_operator,
yyNTstop,
yyNTstop,
yyNTstop,
yyNTstop,
yyNTstop,
yyNTstring_v,
yyNTstring_v,
yyNTstring_i,
yyNTstring_i,
yyNTstring_l,
yyNTstring_l,
yyNTstring_e,
yyNTdelimiter,
yyNTdelimiter,
yyNTpointer_o,
yyNTpointer_o,
yyNTsubtract,
yyNTsubtract,
yyNTsubtract,
yyNTsubtract,
yyNTsubtract,
yyNTsubtract,
yyNTsubtract_i,
yyNTsubtract_i,
yyNTsubtract_i,
yyNTsubtract_i,
yyNTsubtract_i,
yyNTsubtract_i,
yyNTsuppress,
yyNTterminate,
yyNTtransform,
yyNTunlock,
yyNTunlock,
yyNTunstring,
yyNTunstring,
yyNTunstring_i,
yyNTunstring_i,
yyNTdelimited_o,
yyNTdelimited_o,
yyNTdelimited_l,
yyNTdelimited_l,
yyNTdelimited_e,
yyNTdelimited_e,
yyNTtallying_o,
yyNTtallying_o,
yyNTunstring_l,
yyNTunstring_l,
yyNTunstring_e,
yyNTunstring_e,
yyNTunstring_e,
yyNTunstring_e,
yyNTuse,
yyNTuse,
yyNTuse,
yyNTuse,
yyNTuse,
yyNTexception_or_error,
yyNTexception_or_error,
yyNTbegin_or_end,
yyNTbegin_or_end,
yyNTuse_files,
yyNTuse_files,
yyNTuse_files,
yyNTuse_files,
yyNTuse_files,
yyNTgiving_use_o,
yyNTgiving_use_o,
yyNTfile_or_reel,
yyNTfile_or_reel,
yyNTfile_or_reel,
yyNTuse_l,
yyNTuse_l,
yyNTuse_e,
yyNTuse_e,
yyNTuse_e,
yyNTuse_e,
yyNTuse_e,
yyNTuse_e,
yyNTuse_e,
yyNTwrite,
yyNTwrite,
yyNTwrite,
yyNTwrite,
yyNTwrite,
yyNTwrite,
yyNTwrite,
yyNTwrite_i,
yyNTwrite_i,
yyNTwrite_i,
yyNTwrite_i,
yyNTwrite_i,
yyNTwrite_i,
yyNTwrite_i,
yyNTend_of_page,
yyNTend_of_page,
yyNTend_of_page,
yyNTcopy_o,
yyNTcopy_o,
yyNTcopy_or_replace,
yyNTcopy_or_replace,
yyNTcopy,
yyNTcopy,
yyNTcopy_name,
yyNTcopy_name,
yyNTcopy_suppress_o,
yyNTcopy_suppress_o,
yyNTcopy_replacing_o,
yyNTcopy_replacing_o,
yyNTxx_copy_replacing_o_1_1,
yyNTcopy_replacing_l,
yyNTcopy_replacing_l,
yyNTcopy_replacing_e,
yyNTreplace,
yyNTreplace,
yyNTxx_replace_1_2,
yyNTreplace_l,
yyNTreplace_l,
yyNTreplace_e,
yyNTreplacing_item_1,
yyNTreplacing_item_1,
yyNTpseudo_text_1,
yyNTreplacing_item_2,
yyNTreplacing_item_2,
yyNTxx_replacing_item_2_2_1,
yyNTpseudo_text_2,
yyNTreplacing_item,
yyNTreplacing_item,
yyNTreplacing_item,
yyNTreplacing_item,
yyNTtoken_l,
yyNTtoken_l,
yyNTtoken_e,
yyNTtoken_e,
yyNTtoken_e,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTcondition,
yyNTcondition,
yyNTand_condition,
yyNTand_condition,
yyNTnot_condition,
yyNTnot_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTprimary_condition,
yyNTclassification,
yyNTclassification,
yyNTclassification,
yyNTclassification,
yyNTsign_3,
yyNTsign_3,
yyNTsign_3,
yyNTsign_3,
yyNTpointer_operand,
yyNTpointer_operand,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTrelational_operator,
yyNTequal,
yyNTequal,
yyNTequal,
yyNTgreater,
yyNTgreater,
yyNTgreater,
yyNTgreater_equal,
yyNTgreater_equal,
yyNTgreater_equal,
yyNTgreater_equal,
yyNTless,
yyNTless,
yyNTless_equal,
yyNTless_equal,
yyNTless_equal,
yyNTless_equal,
yyNTis_relational_operator,
yyNTis_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTno_is_relational_operator,
yyNTrelational_operator_2,
yyNTrelational_operator_2,
yyNTexpression,
yyNTexpression,
yyNTexpression,
yyNTmultiplicative_expression,
yyNTmultiplicative_expression,
yyNTmultiplicative_expression,
yyNTpower_expression,
yyNTpower_expression,
yyNTunary_expression,
yyNTunary_expression,
yyNTunary_expression,
yyNTprimary_expression,
yyNTprimary_expression,
yyNTprimary_expression,
yyNTprimary_expression,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTsubscription,
yyNTsubscription,
yyNTsubscription,
yyNTqualification,
yyNTqualification,
yyNTqualification_f,
yyNTqualification_f,
yyNTidentifier_w,
yyNTidentifier_w,
yyNTidentifier_w,
yyNTidentifier_w,
yyNTsubscription_w,
yyNTsubscription_w,
yyNTqualification_w,
yyNTxx_qualification_w_1_1,
yyNTidentifier_c,
yyNTidentifier_c,
yyNTidentifier_c,
yyNTidentifier_c,
yyNTqualification_c,
yyNTidentifier_n,
yyNTidentifier_n,
yyNTidentifier_n,
yyNTidentifier_n,
yyNTqualification_n,
yyNTqualification_n,
yyNTin_of,
yyNTin_of,
yyNTindex_l,
yyNTindex_l,
yyNTindex_l,
yyNTindex,
yyNTindex,
yyNTindex,
yyNTindex,
yyNTreference_modifier,
yyNTreference_modifier,
yyNTidentifier_l,
yyNTidentifier_l,
yyNTidentifier_l_w,
yyNTidentifier_l_w,
yyNTidentifier_l_c,
yyNTidentifier_l_c,
yyNTqualification_l,
yyNTqualification_l,
yyNTqualification_l_f,
yyNTqualification_l_f,
yyNTname_l,
yyNTname_l,
yyNTname_l_f,
yyNTname_l_f,
yyNTidentifier_or_numeric_literal_l,
yyNTidentifier_or_numeric_literal_l,
yyNTidentifier_or_non_numeric_literal_l,
yyNTidentifier_or_non_numeric_literal_l,
yyNTidentifier_rounded_l_w,
yyNTidentifier_rounded_l_w,
yyNTfunction_call,
yyNTfunction_call,
yyNTfunction_call,
yyNTfunction_call,
yyNTfunction_name_1,
yyNTfunction_name_2,
yyNTfunction_name_2,
yyNTargument_l,
yyNTargument_l,
yyNTsymbolic_character,
yyNTcondition_name,
yyNTidentifier_rounded_w,
yyNTidentifier_rounded_c,
yyNTidentifier_or_literal,
yyNTidentifier_or_literal,
yyNTidentifier_or_literal,
yyNTidentifier_or_numeric_literal,
yyNTidentifier_or_numeric_literal,
yyNTidentifier_or_numeric_literal,
yyNTidentifier_or_non_numeric_literal,
yyNTidentifier_or_non_numeric_literal,
yyNTidentifier_or_non_numeric_literal,
yyNTidentifier_or_non_all_literal,
yyNTidentifier_or_non_all_literal,
yyNTidentifier_or_non_all_literal,
yyNTname_or_literal,
yyNTname_or_literal,
yyNTidentifier_or_integer,
yyNTidentifier_or_integer,
yyNTname_,
yyNTname_f,
yyNTprocedure_name,
yyNTprocedure_name,
yyNTchapter_name,
yyNTchapter_name,
yyNTchapter_name,
yyNTinteger,
yyNTinteger,
yyNTinteger,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTliteral,
yyNTliteral,
yyNTnon_figurative_literal,
yyNTnon_figurative_literal,
yyNTnon_numeric_literal,
yyNTnon_numeric_literal,
yyNTnon_numeric_literal,
yyNTconcat_expression,
yyNTconcat_expression,
yyNTconcat_operand,
yyNTconcat_operand,
yyNTconcat_operand,
yyNTnumeric_literal,
yyNTnumeric_literal,
yyNTfigurative_non_numeric_literal,
yyNTfigurative_non_numeric_literal,
yyNTfigurative_numeric_literal,
yyNTfigurative_numeric_literal,
yyNTnon_figurative_non_numeric_literal,
yyNTnon_figurative_numeric_literal,
yyNTnon_figurative_numeric_literal,
yyNTnon_all_figurative_literal,
yyNTnon_all_figurative_literal,
yyNTnon_all_figurative_non_numeric_literal,
yyNTnon_all_figurative_non_numeric_literal,
yyNTnon_all_figurative_non_numeric_literal,
yyNTnon_all_figurative_non_numeric_literal,
yyNTnon_all_figurative_numeric_literal,
yyNTnon_all_figurative_numeric_literal,
yyNTall_figurative_non_numeric_literal,
yyNTall_figurative_non_numeric_literal,
yyNTall_figurative_non_numeric_literal,
yyNTall_figurative_non_numeric_literal,
yyNTall_figurative_non_numeric_literal,
yyNTall_figurative_non_numeric_literal,
yyNTall_figurative_numeric_literal,
yyNTall_figurative_numeric_literal,
yyNTAdvancing,
yyNTAdvancing,
yyNTAre,
yyNTAre,
yyNTAreIs,
yyNTAreIs,
yyNTAreIs,
yyNTArea,
yyNTArea,
yyNTAt,
yyNTAt,
yyNTBy,
yyNTBy,
yyNTCharacter,
yyNTCharacter,
yyNTCharacters,
yyNTCharacters,
yyNTCollating,
yyNTCollating,
yyNTContains,
yyNTContains,
yyNTData,
yyNTData,
yyNTEvery,
yyNTEvery,
yyNTFile,
yyNTFile,
yyNTFiller,
yyNTFiller,
yyNTFor,
yyNTFor,
yyNTFrom,
yyNTFrom,
yyNTIndicate,
yyNTIndicate,
yyNTIn,
yyNTIn,
yyNTInto,
yyNTInto,
yyNTIs,
yyNTIs,
yyNTKey,
yyNTKey,
yyNTLine,
yyNTLine,
yyNTLines,
yyNTLines,
yyNTMessage,
yyNTMessage,
yyNTMode,
yyNTMode,
yyNTNumber,
yyNTNumber,
yyNTOf,
yyNTOf,
yyNTOn,
yyNTOn,
yyNTOrder,
yyNTOrder,
yyNTPrinting,
yyNTPrinting,
yyNTProgram,
yyNTProgram,
yyNTReferences,
yyNTReferences,
yyNTRight,
yyNTRight,
yyNTSign,
yyNTSign,
yyNTSize,
yyNTSize,
yyNTStandard,
yyNTStandard,
yyNTStatus,
yyNTStatus,
yyNTSymbolic,
yyNTSymbolic,
yyNTTape,
yyNTTape,
yyNTThan,
yyNTThan,
yyNTThen,
yyNTThen,
yyNTTimes,
yyNTTimes,
yyNTTo,
yyNTTo,
yyNTWhen,
yyNTWhen,
yyNTWith,
yyNTWith,
yyNTTrailing,
yyNTTrailing,
yyNTSet,
yyNTSet,
yyNTAlternate,
yyNTAlternate,
yyNTMultiple,
yyNTMultiple,
yyNTGlobal,
yyNTGlobal,
yyNTInitial,
yyNTInitial,
yyNTOptional,
yyNTOptional,
yyNTRecord,
yyNTRecord,
yyNTRounded,
yyNTRounded,
yyNTEnd_accept,
yyNTEnd_accept,
yyNTEnd_add,
yyNTEnd_add,
yyNTEnd_call,
yyNTEnd_call,
yyNTEnd_chain,
yyNTEnd_chain,
yyNTEnd_compute,
yyNTEnd_compute,
yyNTEnd_delete,
yyNTEnd_delete,
yyNTEnd_display,
yyNTEnd_display,
yyNTEnd_divide,
yyNTEnd_divide,
yyNTEnd_evaluate,
yyNTEnd_evaluate,
yyNTEnd_if,
yyNTEnd_if,
yyNTEnd_multiply,
yyNTEnd_multiply,
yyNTEnd_perform,
yyNTEnd_perform,
yyNTEnd_read,
yyNTEnd_read,
yyNTEnd_receive,
yyNTEnd_receive,
yyNTEnd_return,
yyNTEnd_return,
yyNTEnd_rewrite,
yyNTEnd_rewrite,
yyNTEnd_search,
yyNTEnd_search,
yyNTEnd_start,
yyNTEnd_start,
yyNTEnd_string,
yyNTEnd_string,
yyNTEnd_subtract,
yyNTEnd_subtract,
yyNTEnd_unstring,
yyNTEnd_unstring,
yyNTEnd_write,
yyNTEnd_write,
yyNTRELATIVE_Key_Is_name,
yyNTON_head,
yyNTON_head,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTnot,
yyNTALTERNATE_AREA,
yyNTALTERNATE_AREA,
yyNTON_head_or_WITH_DATA,
yyNTON_head_or_WITH_DATA,
yyNTname_Message_COUNT,
yyNTidentifier_or_numeric_literal_GIVING,
yyNTexamine_replacing,
yyNTexamine_replacing,
yyNTexamine_replacing,
yyNTexamine_replacing,
yyNTqualification_FROM,
yyNTidentifier_replacing,
yyNTidentifier_replacing,
yyNTidentifier_replacing,
yyNTidentifier_FOR,
yyNTCONSOLE_Is_CRT,
yyNTimplementor_name_e_head,
yyNTimplementor_name_e_head,
yyNTimplementor_name_e_head,
yyNTsymbolic_e_head,
yyNTnon_screen_display,
yyNTnon_screen_display,
yyNTnon_screen_display,
yyNTparentheses_relational_operator,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTrelational_operator_3,
yyNTin_of_qualification_TIMES,
yyNTon_size_error_head,
yyNTon_exception_head,
yyNTon_overflow_head,
yyNTinvalid_head,
yyNTat_end_head,
yyNTat_end_of_page_head,
yyNTon_error_head,
yyNTnot_size_error_head,
yyNTnot_exception_head,
yyNTnot_overflow_head,
yyNTnot_invalid_head,
yyNTnot_end_head,
yyNTnot_end_of_page_head,
yyNTsize_error_head,
yyNTsize_error_head,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTaccept_tail,
yyNTadd_tail,
yyNTadd_tail,
yyNTcall_tail,
yyNTcall_tail,
yyNTcall_tail,
yyNTcall_tail,
yyNTcall_tail,
yyNTcompute_tail,
yyNTcompute_tail,
yyNTdelete_tail,
yyNTdelete_tail,
yyNTdelete_tail,
yyNTdisplay_tail,
yyNTdisplay_tail,
yyNTdisplay_tail,
yyNTdivide_tail,
yyNTdivide_tail,
yyNTmultiply_tail,
yyNTmultiply_tail,
yyNTread_tail,
yyNTread_tail,
yyNTread_tail,
yyNTread_tail,
yyNTread_tail,
yyNTreturn_tail,
yyNTreturn_tail,
yyNTrewrite_tail,
yyNTrewrite_tail,
yyNTrewrite_tail,
yyNTstart_tail,
yyNTstart_tail,
yyNTstart_tail,
yyNTstring_tail,
yyNTstring_tail,
yyNTstring_tail,
yyNTsubtract_tail,
yyNTsubtract_tail,
yyNTunstring_tail,
yyNTunstring_tail,
yyNTunstring_tail,
yyNTwrite_tail,
yyNTwrite_tail,
yyNTwrite_tail,
yyNTwrite_tail,
yyNTwrite_tail,
yyNTdescriptions,
yyNTdescription_l,
yyNTdescription_l,
yyNTdescription_l,
yyNTdescription_l,
yyNTdescription_l,
yyNTdescription_l,
yyNT2_release_Trial_2,
yyNT5_accept_i_Trial_2,
yyNT5_accept_Trial_2,
};
#endif
#ifndef NO_RECOVER
static	yySymbolRange	yyContinuation	[yyLastReadState + 1] = { 0,
    0,     0,    85,     1,     1,     1,    26,   325,   325,   325, 
    1,   423,     1,     1,     1,     1,   274,   446,   446,   446, 
  446,   446,   446,   446,   208,   263,   282,   133,   170,   136, 
  220,   170,   139,   142,   131,   147,   220,   220,   282,   152, 
  282,   143,     1,     1,     1,     0,     0,     0,     0,    96, 
    0,     1,     0,     3,    31,     1,     1,     0,     0,   440, 
    1,   271,     0,     0,     0,     0,     1,   193,     1,     1, 
  261,   334,   334,   187,     1,   101,   161,     1,     1,     1, 
    1,   267,   282,   282,   282,   373,     1,   373,   170,   170, 
  170,   220,   170,   131,   131,   131,   282,   282,   143,   143, 
  189,    95,   124,     0,     0,     0,    12,     1,     1,     1, 
    1,     1,    12,     1,     1,     0,     1,    96,     1,     4, 
    1,     1,     1,     1,   446,     1,    19,    17,    19,     1, 
  161,   161,   271,     8,     1,     1,   457,     1,     1,   407, 
  131,   373,   164,   373,   170,   131,   282,   143,    95,    12, 
  131,   124,     0,     0,     1,     0,    12,   187,    12,    12, 
   12,   331,    12,   429,   429,     0,     1,     1,     1,     1, 
    8,     8,   184,   334,   187,     1,     1,   161,   161,     1, 
  458,   271,   378,   417,     1,     1,   417,     1,   161,   161, 
    1,     1,     1,     1,    12,   447,    12,     8,     1,     1, 
  458,     1,     1,     1,     1,   447,     1,     1,     1,   164, 
  306,    36,    12,   124,     0,     0,   359,     0,     0,     1, 
    0,     1,    12,     1,   203,    12,    12,    12,    12,     1, 
    1,    12,     1,     1,     1,    12,     1,   447,   447,   447, 
  334,   334,     8,    61,     1,     1,   161,     1,     1,     1, 
    1,     1,     1,     1,     1,   161,   264,   161,     8,     8, 
  267,     8,     8,     8,     8,     1,     1,   447,     1,     1, 
    1,   131,    12,   124,    12,   359,     0,     0,    12,   359, 
    0,     0,     1,   272,    36,    12,    12,    12,    12,    12, 
  203,    12,     1,     1,     3,    12,   194,    65,    12,    12, 
  331,    12,    12,     8,     1,     1,    12,    12,     1,     1, 
    1,     1,   334,     1,     1,     1,     1,     1,   161,     1, 
    1,     1,     1,     1,     1,   161,   161,     1,     1,     1, 
    1,     1,   161,     1,     1,     1,     1,   271,     1,     1, 
    1,   161,     1,     1,     1,     1,   447,   267,    12,    12, 
  306,    12,     1,     1,    12,    12,   359,     0,     0,    12, 
   12,     0,     0,     0,     0,     0,   462,   440,    12,     8, 
    4,     8,   228,    12,     8,    45,     1,   176,     6,    12, 
    1,    12,   215,     3,     1,   317,   317,     1,     3,     1, 
    1,     1,   271,   176,     1,     1,     3,     3,     4,     8, 
    3,   317,   317,     3,   271,     1,     1,   453,     1,     1, 
   12,    12,    12,     1,     1,    12,    12,   440,     3,    12, 
   12,   367,     3,   199,     8,   228,     1,     1,   367,    55, 
  121,     1,     4,     4,     1,     1,     1,     1,     1,   447, 
  264,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    8,     0,     1,   323,    12,    12,    12,     0,    12,   359, 
    0,     0,     0,     0,     0,    12,     0,    12,    12,    12, 
   72,     1,    12,    50,   440,     8,    12,     8,   228,    45, 
    1,    12,   334,    12,    12,    12,    12,     4,   382,     1, 
    1,     1,     1,     1,     4,   432,     1,     1,     1,     1, 
   12,    12,    12,    12,    12,   378,    12,     4,     1,     1, 
    1,     1,     4,   432,   181,    12,    12,     1,     1,     8, 
   12,   177,     1,    12,   440,     3,    12,     3,     3,     8, 
  228,     1,    12,    12,   349,   121,     1,     1,     1,     1, 
  161,   161,   161,   161,   447,    12,     1,     1,    12,     0, 
    0,     0,    12,   359,     0,     0,     0,     0,     1,     0, 
   12,    12,     0,     0,     0,   462,   462,     4,    12,    12, 
    1,    12,     1,   224,   224,   116,   232,    12,    12,     1, 
   12,    12,     1,    12,    12,     1,     1,   381,   381,    12, 
   12,     3,     1,    12,    12,     1,     1,    12,    12,    12, 
   12,    12,     1,     1,   381,   381,    12,     3,     1,    12, 
  177,     4,     4,     4,     1,     1,     1,     1,   200,   200, 
  200,     1,     1,     1,     1,     1,     1,    12,    12,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,   215,     1,     1,   215,     1,     1,     8,     1,   256, 
   12,     1,     1,    12,     8,     1,     1,     1,     1,     1, 
    1,   366,     1,   215,     1,     1,     1,   410,     1,     1, 
  410,     1,     1,     1,     1,   226,     1,     1,     1,   354, 
    1,     1,    12,     1,     1,     1,     1,     1,     0,     0, 
    0,     0,     0,    12,    12,    12,     0,     0,     0,    12, 
  359,     0,     0,     0,   276,     1,     0,     0,     0,     0, 
    0,   462,    12,     1,    12,    12,     1,     1,     1,    12, 
   50,     1,     1,     1,   270,     1,     1,     1,     1,     1, 
  224,   102,   224,   232,   398,     1,   224,    95,   224,   232, 
   95,   310,     1,     4,     1,    12,    12,    12,    12,   409, 
    1,    12,     1,    12,    12,   186,     4,     3,     1,     1, 
   12,     4,    12,    12,    12,    12,     4,     3,     1,    12, 
  177,   286,     4,    12,   286,     4,     1,    12,     1,     1, 
    1,     1,   131,     1,   359,     4,    12,     1,     1,   408, 
   12,   408,     1,    12,    12,    12,    12,    12,     1,     1, 
  443,     1,    12,   398,     1,     1,     1,   271,    12,    12, 
    1,   219,     1,    12,    12,    12,   434,    24,     1,   396, 
    1,     1,    12,    12,    12,     1,    13,    12,    12,     1, 
  396,    33,     1,   408,    50,    13,     1,     1,     1,     1, 
   12,     1,     1,     1,     1,     1,     1,     1,     1,     8, 
    8,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,    12,    12,    12,   420,   407,     1,     1,   400, 
   12,   249,     1,    35,    12,     1,    35,     1,     1,   271, 
  408,    12,    12,    12,    12,   219,   112,     1,   189,    12, 
  189,   317,   219,     1,    12,     0,    12,     0,    12,    12, 
    0,     0,     0,     0,    12,   359,     0,    12,     1,    12, 
    3,    12,   108,    12,     9,    12,     1,     1,     1,     1, 
   12,     1,     1,   462,     1,     1,     1,     1,     4,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,    12,    12,    12,   409,   409,    12,    12,   409, 
  409,    12,    49,     1,    12,    12,   117,     4,     4,   117, 
   12,    12,    12,    12,     8,   286,   260,     1,     1,   110, 
    1,   108,     1,    12,     4,    67,    71,     1,    48,   165, 
  170,    12,    12,    12,    12,    12,    12,    12,   238,    95, 
  408,     1,     1,     1,     1,     1,    12,     1,     1,   242, 
    1,    12,    12,    12,     1,     1,    12,    12,   435,    67, 
   96,     1,     1,   238,     1,     1,    12,     1,     1,     8, 
   28,    12,    71,   334,    19,    12,   114,    12,   271,    12, 
   12,    12,    71,    21,   334,     1,   428,   408,     1,     1, 
  167,    13,    12,     1,    12,    12,     1,    12,     1,     4, 
   12,     1,     1,   408,    12,   443,     1,    12,    12,    12, 
  219,   434,    13,    50,    12,   249,    35,    12,     1,    35, 
   12,   219,     1,   189,   219,    12,   420,   407,     8,     1, 
   71,     1,    42,    12,    12,    12,   219,   189,    35,     1, 
   12,   434,     1,   434,     1,     1,     1,    50,   274,    50, 
   12,   161,    12,     1,     1,   189,     1,   189,     1,   219, 
   19,    12,    12,     0,     0,     0,    12,   359,   439,   179, 
    1,   317,    12,    12,     4,    12,   368,   250,     1,     1, 
    4,     1,     1,     3,    70,   372,     1,    12,     1,     1, 
    1,     1,   462,     1,    12,     1,    12,     1,     1,     1, 
  462,     1,    12,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,    12,   409,    12,   409,   409,    12,    12,   409, 
   12,    49,     1,    12,     4,     4,     4,    12,   286,    12, 
    1,    43,    18,   108,    42,     1,   447,    12,   224,   384, 
  266,   255,    12,    12,    48,   170,    71,    12,    96,    12, 
   48,    12,    12,    12,    12,     1,     1,     1,    95,     1, 
   12,     1,    12,   408,   271,    12,    12,     1,   323,    12, 
    1,    12,    12,   323,   330,   242,   346,    12,    12,    13, 
  220,    12,   224,    12,   261,     1,     1,     1,     1,   193, 
    1,    12,   224,    12,    12,   323,    12,    13,    13,   280, 
   12,    71,    28,    19,    19,   184,     8,     1,     1,     1, 
   71,    12,    50,     1,    61,     1,    12,   428,   368,    33, 
    1,    12,     1,    12,     1,     1,    71,   242,   242,   242, 
    4,     1,   132,   132,    12,    12,    12,    12,    95,   408, 
    1,     1,    12,     1,    12,    12,    12,     1,     1,   140, 
  141,     1,    12,    12,    12,   219,    35,    12,   434,   434, 
   12,     1,   189,     1,   219,    12,     8,     1,    71,    71, 
   71,   189,    12,    12,     1,    12,     1,     1,   131,    12, 
   12,   434,   131,   434,    12,    12,     1,   271,     1,     1, 
    1,     1,     1,     1,    12,   126,     1,   161,    12,    12, 
   12,    12,     1,     1,     1,    12,     1,     1,     1,     1, 
    1,   296,     1,    12,   143,    12,    12,    12,   143,     0, 
    0,     0,    12,     1,     1,     1,     1,     1,     1,     1, 
    1,    12,    12,   436,     3,    12,   368,    12,     1,     1, 
    1,     1,     1,   221,     1,    96,     3,    70,   367,     1, 
    1,     1,     1,    12,     1,     1,    12,     9,   447,    12, 
    1,     1,    12,    12,   409,    12,    12,     4,    12,    12, 
    1,   170,   164,     1,   336,     1,   170,   170,    12,   170, 
   71,    12,     3,     1,     3,     1,     1,    18,    12,    48, 
   12,    12,    12,     1,     1,     1,    12,     1,    12,    12, 
  373,    12,   373,   193,    12,     1,     1,     1,     1,     1, 
    1,    12,    12,    12,   282,   271,     1,   346,    12,    12, 
   71,    13,     1,    12,    17,     3,   356,     1,   159,     3, 
    1,     1,     1,     1,     1,   193,    12,    12,     1,     1, 
    1,    28,     8,    71,    13,   184,     8,     8,     1,     8, 
   12,    50,   408,     1,    50,     1,     1,    12,   334,     1, 
    1,     1,     1,    12,   193,    12,    12,    12,   346,   346, 
  447,    12,    12,    12,    12,    12,   132,    95,     1,    12, 
    1,    12,    12,    12,   137,    12,   193,     1,    12,   140, 
   71,     1,    12,    12,    12,    12,     1,   131,   148,   434, 
  434,   150,    12,     1,     1,    12,     1,   296,   154,    12, 
  154,    12,    12,     1,     1,    12,    12,     1,    12,    12, 
  242,    12,    12,   131,     1,    71,     8,    12,     1,     1, 
   12,   271,     1,    12,    12,    12,    12,   271,    12,    12, 
   12,     1,     1,     1,   234,     1,     1,     1,    12,   294, 
    1,    12,   193,    12,    12,   408,     1,   219,    12,     1, 
  447,   447,     1,     1,     1,   143,   143,    12,    71,     0, 
    0,     0,     1,     1,     1,     1,     1,     1,   322,     1, 
   63,     1,    12,   127,     1,     1,    39,   210,     1,     1, 
  369,    12,     4,   384,   369,   384,     4,     1,     1,     1, 
  382,     1,     3,     1,     8,   367,     1,     3,     1,    12, 
   12,    12,   462,     1,     1,    12,    12,    12,   302,   226, 
  170,     1,     1,   170,   170,    71,   165,     4,     1,     4, 
    1,     1,     1,    12,    12,    12,     1,    12,   373,   164, 
    1,    12,     8,    12,   271,    12,    12,    71,     1,    12, 
   71,     4,     1,     4,     1,    12,   328,     1,    12,    12, 
   13,    50,   334,     1,     1,    12,    50,     1,    12,    12, 
   61,   193,   382,    12,    12,     1,    71,     1,   132,    12, 
   12,   132,    12,   133,   193,   133,   134,   134,   136,   138, 
    1,   193,   139,   141,   193,   142,    12,    12,     1,   145, 
   12,    12,   147,     1,   149,    12,    12,   193,   152,    12, 
    1,   154,    50,    12,    12,     1,    12,   131,    12,   131, 
  242,   242,   242,    71,   101,   101,    12,   131,    12,    71, 
  161,    71,    13,    71,    12,    12,     1,   408,   408,     1, 
  302,     1,   193,    12,   373,     1,     1,     1,     1,     1, 
    1,    12,   282,    12,   282,     1,     1,    12,     1,     1, 
    1,    12,     1,     1,   239,    12,    71,    12,     0,     0, 
    0,     0,    12,    12,     1,    12,     1,     1,   322,     1, 
    1,     1,   127,     1,     1,    39,   210,     1,     1,     1, 
    1,     1,    31,    12,     1,   369,     1,   382,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,   367,   382,     4, 
   12,     1,     1,     1,   226,   302,     1,     1,    12,     1, 
   71,    71,    12,   170,    12,    12,     1,     1,     1,   164, 
   71,    12,     1,   220,    12,     1,    12,   328,     8,    50, 
   12,     1,    12,    12,     1,   302,    12,    12,    12,    12, 
  132,   133,     1,    12,   328,     1,     1,    12,    12,     1, 
  145,   146,   147,    71,   151,   152,     1,    12,   239,     1, 
   12,    12,    12,   131,    71,    71,    71,   131,     8,   161, 
    1,    12,    12,   414,   274,     1,   193,     1,     1,   161, 
  161,   282,    71,   447,    12,     1,    12,     1,     1,    12, 
  143,     0,     0,     0,     0,     1,     1,    12,     1,     1, 
    1,   369,     1,     1,   246,   369,     1,   382,     1,     1, 
   31,     1,     1,     3,     3,     1,     3,     1,   384,     1, 
    1,     1,     1,   302,     1,     1,     1,     1,   170,    71, 
    1,     1,   447,    71,    12,    12,    12,    13,    12,    12, 
    1,     8,     1,    12,     1,    12,   132,    12,   139,     1, 
   12,   328,    12,   145,   145,    12,   149,    12,    12,    12, 
  420,    12,    71,    12,    12,    71,     1,     1,    12,     1, 
    1,    71,    12,    12,     1,    12,    12,     1,    12,   143, 
    0,     8,     0,     4,     1,    12,     1,    12,    12,    12, 
   17,    12,    12,     1,   322,    12,    12,    12,    12,    12, 
   12,     3,     1,     1,    12,    12,     1,     1,     1,   170, 
    1,   373,    71,    12,    12,     1,   133,    12,   139,     1, 
  142,   145,   152,   153,   154,     8,   131,    71,   101,    12, 
  282,     1,    71,     8,     8,    12,     1,     1,     1,    12, 
    1,    12,     1,   242,     1,     1,     1,     1,     1,    12, 
   71,   373,    12,   139,    12,    12,   131,    71,   282,     1, 
   12,    12,    12,   126,     1,    12,     1,     1,   274,    12, 
   12,   164,   139,    71,    71,     1,   356,     8,     8,     1, 
  159,     1,     1,     8,     8,     4,     8,    12,   228,     1, 
    1,   121,     1,     1,     1,    12,     1,    12,    12,   317, 
   71,     1,   440,     1,     1,     1,     1,     1,     8,     8, 
    1,     1,   228,     1,   121,     8,     1,     1,   317,     1, 
    1,     1,     1,     8,     1,     1,     1, 
};
static	unsigned short	yyCondition	[yyLastState - yyLastReduceState + 1] =
{ 0,
  127,  4675,    78,  4498,    78,    78,  4101,  4102,  4627,    57, 
   57,    57,   641,   642,    58,    59,   224,   472,   284,    63, 
   64,    65,    66,   641,   642,    58,    59,    63,    64,    65, 
   66,    79,    80,    79,    80,   936,    79,    80,   986,  1001, 
   70,  1031,  1032,    79,    80,  4627,    70,  1104,   641,   642, 
   58,    59,    63,    64,    65,    66,  3583,   642,    58,    59, 
   63,    64,    65,    66,  1001,  4627,    70,    70,  1032,  4627, 
   70,  4627,    70,    70,    70,  1000,  1001,  1002,  1028,  1333, 
   87,    70,  4627,   117,    51,  3585,   642,    58,    59,    63, 
   64,    65,    66,  3582,   642,    58,    59,    63,    64,    65, 
   66,    87,  1001,    87,    87,    87,    87,    87,    87,    87, 
 1233,  1000,  1001,  4627,  1259,  1261,    70,  4627,  4627,  1360, 
 1261,  1379,  1261,  4627,  1394,  1395,  1261,    70,    87,  3584, 
  642,    58,    59,    63,    64,    65,    66,  1001,    87,  1458, 
   87,  1233,  1000,  1001,  1480,  1481,    87,  1480,  1481,    87, 
 1492,  1458,    87,  1499,  1481,    87,  1028,  1458,    87,  1517, 
 1481,    87,  1546,  1481,    87,  1261,    70,    70,  1634,  1481, 
   87,  1394,  1647,    70,    87,    87,  3033,    78,  1001,    87, 
 1233,  1000,  1001,  1480,  1481,    87,    70,    70,  1797,  1798, 
 1832,  1833,    87,  1634,  1481,    87,    70,  3119,  3019,  3044, 
   87,    87,    87,  1000,  1001,  1517,  1481,    87,  1797,  1798, 
 1797,  1798,    87,    87,    87,  1480,  1481,    87,  1517,  1481, 
   87,  1546,  1481,    87,  1797,  1261,  1634,  1481,    87,  2066, 
 1833,    87,  1394,  1647,  2128,    87,  1517,  1481,    87,  1517, 
 1481,    87,  3126, 
};
#endif
static	unsigned short	yyFinalToProd	[yyLastReadReduceState -
						yyFirstReadReduceState + 2] = {
 2936,  2946,  2957,  2958,  2959,  2960,  2961,  2962,  2965,  2969, 
 2976,  2978,  2981,  2982,  2983,  2989,  3005,  3007,  3008,  3009, 
 3014,  3015,  3020,  3050,  3051,  3052,  3054,  3056,  3058,  3061, 
 3067,  3068,  3082,  3083,  3084,  3085,  3086,  3087,  3089,  3090, 
 3093,  3096,  3097,  3098,  3108,  3114,  3116,  3117,  3120,  3127, 
 3133,  3134,  3135,  3136,  3137,  3140,  3152,  3153,  3154,  3155, 
 3157,  3158,  3159,  3165,  3168,  3204,  3205,  3208,  3209,  3210, 
 3212,  3217,  3218,  3220,  3221,  3263,  3264,  3265,  3268,  3270, 
 3275,  3276,  3278,  3279,  3305,  3306,  3307,  3308,  3311,  3312, 
 3313,  3314,  3316,  3317,  3318,  3320,  3324,  3325,  3326,  3336, 
 3337,  3339,  3340,  3342,  3343,  3344,  3345,  3346,  3347,  3348, 
 3349,  3350,  3351,  3352,  3353,  3354,  3355,  3356,  3357,  3367, 
 3368,  3378,  3381,  3382,  3383,  3384,  3385,  3386,  3387,  3388, 
 3389,  3425,  3426,  3429,  3439,  3442,  3444,  3452,  3453,  3457, 
 3458,  3460,  3463,  3465,  3466,  3467,  3468,  3469,  3473,  3474, 
 3481,  3482,  3483,  3484,  3487,  3490,  3491,  3492,  3493,  3495, 
 3504,  3505,  3508,  3509,  3510,  3511,  3512,  3513,  3514,  3515, 
 3516,  3517,  3518,  3519,  3520,  3521,  3533,  3534,  3538,  3539, 
 3540,  3541,  3545,  3546,  3547,  3548,  3549,  3550,  3555,  3557, 
 3558,  3559,  3560,  3561,  3562,  3580,  3591,  3594,  3601,  3602, 
 3603,  3605,  3606,  3734,  3747,  3748,  3750,  3756,  3757,  3758, 
 3759,  3760,  3765,  3766,  3767,  3768,  3769,  3770,  3771,  3772, 
 3773,  3777,  3778,  3785,  3787,  3788,  3790,  3795,  3796,  3797, 
 3798,  3799,  3800,  3801,  3802,  3803,  3804,  3805,  3806,  3809, 
 3810,  3811,  3812,  3813,  3815,  3820,  3821,  3822,  3823,  3824, 
 3825,  3826,  3827,  3837,  3839,  3841,  3843,  3857,  3858,  3860, 
 3862,  3901,  3902,  3903,  3904,  3905,  3906,  3907,  3911,  3912, 
 3913,  3914,  3919,  3927,  3928,  3929,  3934,  3944,  3952,  3953, 
 3957,  3958,  3959,  3960,  3961,  3962,  3963,  3964,  3965,  3966, 
 3967,  3969,  3973,  3974,  3990,  3991,  3992,  3993,  3994,  4010, 
 4014,  4015,  4024,  4026,  4027,  4035,  4036,  4037,  4038,  4039, 
 4040,  4041,  4042,  4044,  4045,  4051,  4052,  4053,  4058,  4059, 
 4073,  4074,  4081,  4082,  4083,  4084,  4085,  4086,  4087,  4088, 
 4139,  4140,  4141,  4163,  4164,  4165,  4167,  4168,  4170,  4171, 
 4175,  4177,  4185,  4188,  4189,  4194,  4205,  4207,  4209,  4211, 
 4212,  4215,  4216,  4217,  4218,  4219,  4228,  4232,  4233,  4234, 
 4242,  4244,  4249,  4253,  4256,  4257,  4275,  4278,  4290,  4291, 
 4299,  4300,  4303,  4304,  4316,  4333,  4338,  4350,  4351,  4352, 
 4356,  4357,  4361,  4381,  4382,  4383,  4384,  4386,  4387,  4388, 
 4389,  4392,  4393,  4403,  4414,  4415,  4416,  4417,  4422,  4423, 
 4424,  4427,  4428,  4429,  4438,  4444,  4445,  4447,  4449,  4456, 
 4458,  4459,  4461,  4462,  4463,  4464,  4465,  4466,  4467,  4468, 
 4469,  4470,  4471,  4472,  4473,  4474,  4475,  4476,  4477,  4478, 
 4479,  4480,  4481,  4482,  4483,  4484,  4485,  4494,  4495,  4504, 
 4505,  4507,  4508,  4509,  4510,  4511,  4513,  4516,  4520,  4523, 
 4527,  4533,  4539,  4545,  4551,  4554,  4557,  4561,  4567,  4573, 
 4576,  4580,  4583,  4587,  4605,  4608,  4614,  4615,  4625,  4639, 
 4640,  4643,  4648,  4649,  4676,  4699,  4700,  4703,  4704,  4706, 
 4708,  4709,  4710,  4711,  4712,  4713,  4714,  4733,  4735,  4738, 
 4739,  4740,  4741,  4742,  4743,  4744,  4745,  4746,  4747,  4748, 
 4750,  4751,  4752,  4754,  4756,  4757,  4759,  4761,  4763,  4765, 
 4767,  4769,  4771,  4773,  4775,  4777,  4779,  4781,  4783,  4785, 
 4787,  4789,  4791,  4793,  4795,  4797,  4799,  4801,  4803,  4805, 
 4807,  4809,  4811,  4813,  4815,  4817,  4819,  4821,  4823,  4825, 
 4827,  4829,  4831,  4833,  4835,  4837,  4839,  4841,  4843,  4845, 
 4847,  4849,  4853,  4855,  4857,  4859,  4861,  4863,  4865,  4867, 
 4869,  4871,  4873,  4875,  4877,  4879,  4881,  4883,  4885,  4887, 
 4889,  4891,  4893,  4895,  4897,  4899,  4901,  4903,  4905,  4906, 
 4910,  4911,  4912,  4913,  4914,  4915,  4916,  4917,  4918,  4920, 
 4921,  4922,  4923,  4924,  4925,  4926,  4927,  4928,  4929,  4930, 
 4931,  4932,  4933,  4934,  4935,  4937,  4939,  4941,  4942,  4943, 
 4944,  4945,  4946,  4947,  4948,  4949,  4950,  4951,  4952,  4953, 
 4954,  4955,  4956,  4957,  4958,  4959,  4960,  4961,  4962,  4963, 
 4964,  4965,  4966,  4967,  4968,  4969,  4970,  4971,  4972,  4973, 
 4974,  4975,  4976,  4977,  4978,  4979,  4980,  4981,  4982,  4983, 
 4984,  4985,  4986,  4987,  4988,  5000,  5005,  5007,  5010,  5013, 
 5015,  5017,  5022,  5024,  5027,  5030,  5033,  5035,  5038,  5043, 
0
};
static	unsigned short	yyStartLine	[yyLastStopState - yyFirstReduceState
							+ 2] = { 0,
536,
2900,
};
#ifdef YYaccDefault
static unsigned long yyl1 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl2 [] = {
0x00000041, 0x00400000, 0x00000000, 0x00000000, 0x00000000, 0x00040000, 
0x00000000, 0x00000000, 0x00000000, 0x02000000, 0x00000000, 0x00000020, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl3 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05001000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl4 [] = {
0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 
0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 
0xFFFFFFFF, 0xFFFFFFFF, 0x00007FFF, };
static unsigned long yyl5 [] = {
0x00000078, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl6 [] = {
0x6BEDBDFE, 0x222FD76A, 0x9F8800BF, 0x6E4B8820, 0x37FFFFFF, 0xFD01774E, 
0xFAE0C6FF, 0x753CE7D5, 0x4C2C58FE, 0xBCB00252, 0x89EEC148, 0x8D7D214B, 
0xBBD891C6, 0xFF4F33FF, 0x00007FFF, };
static unsigned long yyl7 [] = {
0x6BEDB1FA, 0x222DD768, 0x0B8800AD, 0x0E418820, 0x37FFFFF7, 0xF901774E, 
0x7A6046FF, 0x7538E7D4, 0x0C2C58FE, 0xBCB00212, 0x89EEC048, 0x8D7C2149, 
0xB9D891C6, 0xBF4C33DF, 0x00003BFF, };
static unsigned long yyl8 [] = {
0x00000002, 0x00000000, 0x00000002, 0x41001000, 0x00000000, 0x84000000, 
0x80008000, 0x00040001, 0x00000020, 0x00000040, 0x00000000, 0x00200000, 
0x00010000, 0x00020000, 0x00000000, };
static unsigned long yyl9 [] = {
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000002, 
0x00000020, 0x00000400, 0x00000080, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x38000002, 0x0000000E, };
static unsigned long yyl10 [] = {
0x0000014E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl11 [] = {
0x0008004E, 0x00000002, 0x00000000, 0x00080000, 0x00000000, 0x80808420, 
0x00800800, 0x00000000, 0x06000000, 0x00000000, 0x00000000, 0x00200000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl12 [] = {
0x00000000, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl13 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x00000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl14 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00004000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl15 [] = {
0x00001000, 0x00006000, 0x1007FC00, 0x06000000, 0x00000000, 0x00010000, 
0x60020004, 0x00000010, 0x10004000, 0x00008050, 0x00000002, 0x00100000, 
0x08000400, 0x00002200, 0x00000000, };
static unsigned long yyl16 [] = {
0x0228B1FE, 0x03A9B3E0, 0x030FFDC1, 0x8E608821, 0x37FF7F77, 0xD2017942, 
0x0276467D, 0x517847D0, 0x580C08D8, 0x3DB0826E, 0xB9648060, 0xCC6EA141, 
0x38408146, 0x3F4D814F, 0x000001FF, };
static unsigned long yyl17 [] = {
0x4B2CB0FA, 0x02280000, 0x01080081, 0x0A408820, 0x37FF7F75, 0x80007346, 
0x22604039, 0x51000500, 0x002C0898, 0x1C200202, 0x08600040, 0x806C2041, 
0x20188146, 0xFD4C000E, 0x000003FF, };
static unsigned long yyl18 [] = {
0x4B2CB0FA, 0x02280000, 0x01080081, 0x0A408820, 0x37FF7F75, 0x80007346, 
0x22604039, 0x51000500, 0x002C0898, 0x1C200202, 0x08600040, 0x806C2041, 
0x20188146, 0xFD4C000E, 0x000033FF, };
static unsigned long yyl19 [] = {
0x00000000, 0x00000010, 0x10100200, 0x000001A0, 0x40000000, 0x001C1000, 
0x05001000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl20 [] = {
0x00000001, 0x00000010, 0x10100200, 0x00000180, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl21 [] = {
0x00000001, 0x00000010, 0x10000200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl22 [] = {
0x00001000, 0x00006000, 0x1007FC00, 0x06000000, 0x00000000, 0x00010000, 
0x60020004, 0x00000010, 0x10004000, 0x00008050, 0x00000000, 0x00100000, 
0x08000400, 0x00002200, 0x00000000, };
static unsigned long yyl23 [] = {
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x08000000, 
0x00900800, 0x00000000, 0x02000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl24 [] = {
0x00001000, 0x00010000, 0x00000004, 0x00000020, 0x00000000, 0x00010000, 
0x20000004, 0x00002004, 0x00000000, 0xA0000000, 0x00028000, 0x00000000, 
0x00000000, 0x00002000, 0x00000000, };
static unsigned long yyl25 [] = {
0x00001000, 0x00000000, 0x16000002, 0x00000000, 0x00000000, 0x00000000, 
0x20000004, 0x00000000, 0x40000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00080000, 0x00000000, };
static unsigned long yyl26 [] = {
0x00001000, 0x00010000, 0x00000000, 0x00000020, 0x00000000, 0x00000000, 
0x00000000, 0x00000004, 0x00000000, 0x20000000, 0x00000000, 0x00000000, 
0x00000000, 0x00002000, 0x00000000, };
static unsigned long yyl27 [] = {
0x00000001, 0x00400000, 0x00000000, 0x00000000, 0x00000000, 0x00040000, 
0x00000000, 0x00000000, 0x00000000, 0x02000000, 0x00000000, 0x00000020, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl28 [] = {
0x0008007A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x80000000, 0x000000F0, };
static unsigned long yyl29 [] = {
0x0201B002, 0x22280008, 0x890800A9, 0x2A428020, 0x37FF7F7D, 0x28007542, 
0x9260C019, 0x55044001, 0x040C00B0, 0x1C200042, 0x08604140, 0x806C2043, 
0x21009146, 0x084E002C, 0x00000000, };
static unsigned long yyl30 [] = {
0x4B24B002, 0x02280000, 0x01080081, 0x0A408820, 0x37FF7F75, 0x00007346, 
0x22604039, 0x51000400, 0x002C0898, 0x1C200202, 0x08600040, 0x804C2041, 
0x20188146, 0xFD4C000E, 0x0000390F, };
static unsigned long yyl31 [] = {
0x000801FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080100, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x45000000, 0x000001F1, };
static unsigned long yyl32 [] = {
0x000801FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080100, 0x00200000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x45000000, 0x000001F1, };
static unsigned long yyl33 [] = {
0x0B04B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x002C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20188146, 0x804C000C, 0x00000000, };
static unsigned long yyl34 [] = {
0x0304B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x002C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20188146, 0x804C000C, 0x00000000, };
static unsigned long yyl35 [] = {
0x4B24B002, 0x02280000, 0x01080081, 0x0A408820, 0x37FF7F75, 0x00007346, 
0x22604039, 0x51000400, 0x002C0898, 0x1C200202, 0x08600040, 0x804C2041, 
0x20188146, 0xFD4C000E, 0x0000000E, };
static unsigned long yyl36 [] = {
0x4FFDF1FA, 0x423DF76C, 0x17AFFCAF, 0x0E40A824, 0x37FFFFF7, 0xF00B774E, 
0x726646FF, 0x553C67D4, 0x7CAC58FE, 0xBCB09A53, 0x89F6C068, 0x8D7E2141, 
0xB95887C7, 0xFF4C33CF, 0x000031FF, };
static unsigned long yyl37 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00080000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl38 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x00141000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl39 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x04003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl40 [] = {
0x00000001, 0x00000010, 0x00100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl41 [] = {
0x00000001, 0x00000000, 0x00100200, 0x00000020, 0x40000008, 0x001C1000, 
0x01003000, 0x00030000, 0x00002000, 0x00084000, 0x00008000, 0x12001030, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl42 [] = {
0x00001000, 0x00006000, 0x0007FC00, 0x06000000, 0x00000000, 0x00010000, 
0x60020004, 0x00000010, 0x10004000, 0x00008050, 0x00000000, 0x00100000, 
0x08000400, 0x00002200, 0x00000000, };
static unsigned long yyl43 [] = {
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00900800, 0x00000000, 0x02000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl44 [] = {
0x000801FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00080100, 0x00000010, 0x00800000, 0x00000000, 0x84000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl45 [] = {
0x000801FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl46 [] = {
0x00001000, 0x00004000, 0x00000020, 0x06000000, 0x00000000, 0x00000000, 
0x40000080, 0x00004010, 0x00000010, 0x00000010, 0x00000000, 0x01100000, 
0x88000080, 0x00002200, 0x00000000, };
static unsigned long yyl47 [] = {
0x000800FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000100, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0xC5000000, 0x000000F0, };
static unsigned long yyl48 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl49 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x000C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl50 [] = {
0x0208B002, 0x22280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000010, 0x000C0090, 0x1C200002, 0x08604040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl51 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08604040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl52 [] = {
0x000901FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080100, 0x00000100, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x45000000, 0x000001F1, };
static unsigned long yyl53 [] = {
0x000901FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080100, 0x00200100, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x45000000, 0x000001F1, };
static unsigned long yyl54 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00010000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl55 [] = {
0x00000001, 0x00000010, 0x00100200, 0x000001A0, 0x40000008, 0x00180000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x00008200, 0x12001210, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl56 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05002000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl57 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00000000, 0x00084000, 0x0000A200, 0x00001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl58 [] = {
0x00001100, 0x0000F360, 0x0207FC28, 0x06000000, 0x00000002, 0x70010008, 
0x60020444, 0x00204290, 0x18004040, 0x00108050, 0x01040000, 0x00300100, 
0x09000400, 0x03003201, 0x00000000, };
static unsigned long yyl59 [] = {
0x00000000, 0x00002000, 0x0007FC00, 0x06000000, 0x00000000, 0x00000000, 
0x00020000, 0x00000000, 0x10000000, 0x00008040, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl60 [] = {
0x00001000, 0x00000000, 0x00000000, 0x00100000, 0x00000000, 0x00000010, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00020201, 0x00000000, 0x00000000, };
static unsigned long yyl61 [] = {
0x0000007A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl62 [] = {
0x00000002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x02000000, 
0x20000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00008000, 0x00000000, };
static unsigned long yyl63 [] = {
0x000811FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00020000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04080000, 
0x01000000, 0x05040000, 0x000000F0, };
static unsigned long yyl64 [] = {
0x000811FA, 0x00016000, 0x0007FC24, 0x06000020, 0x00000000, 0x00030000, 
0x60020284, 0x00086014, 0x10004010, 0xA0808050, 0x00028000, 0x05180000, 
0x89100480, 0x05042200, 0x000000F0, };
static unsigned long yyl65 [] = {
0x00000078, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x20000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl66 [] = {
0x00000078, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x20000000, 0x00000000, 0x00000010, 0x00000020, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl67 [] = {
0x00000000, 0x01800000, 0x02000000, 0x00200000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x40000000, 0x0000000C, 0x30008000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl68 [] = {
0x0208B000, 0x22280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000010, 0x000C0090, 0x1C200002, 0x08604040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl69 [] = {
0x0208B102, 0x22280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007140, 
0x02604219, 0x51080010, 0x000C0090, 0x1CA00002, 0x08604040, 0x844C2041, 
0x20008146, 0x054C000C, 0x00000000, };
static unsigned long yyl70 [] = {
0x00000102, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x05000000, 0x00000000, };
static unsigned long yyl71 [] = {
0x0200B04F, 0x02280010, 0x01180281, 0x0A4081A0, 0x7000000C, 0x001C7140, 
0x07607019, 0x51030000, 0x000C2010, 0x1C284002, 0x08608240, 0x924C3271, 
0x20008146, 0x0060000C, 0x000000F0, };
static unsigned long yyl72 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00020000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl73 [] = {
0x0200B041, 0x02680010, 0x11180281, 0x0A4081A0, 0x77FF7F7D, 0x001C7140, 
0x07607019, 0x51030000, 0x000C2090, 0x1E284002, 0x0860A240, 0x924C3271, 
0x20008146, 0x006C000C, 0x000000F0, };
static unsigned long yyl74 [] = {
0x00000041, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x000000F0, };
static unsigned long yyl75 [] = {
0x00000001, 0x00000010, 0x00100200, 0x000001A0, 0x40000008, 0x001C0000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x00008200, 0x12000230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl76 [] = {
0x00001000, 0x00006002, 0x0007FC00, 0x060C0000, 0x00000000, 0x00010000, 
0x60060004, 0x00000010, 0x10004000, 0x00008050, 0x00000000, 0x00100000, 
0x08000400, 0x00002200, 0x00000000, };
static unsigned long yyl77 [] = {
0x00001000, 0x00006000, 0x0007FC00, 0x06000000, 0x00000000, 0x00010000, 
0x60020004, 0x00000010, 0x10004000, 0x00008050, 0x00000002, 0x00100000, 
0x08000400, 0x00002200, 0x00000000, };
static unsigned long yyl78 [] = {
0x00001000, 0x00000000, 0x00000000, 0x00000000, 0x00000008, 0x00000000, 
0x00000000, 0x02000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00024201, 0x00000000, 0x00000000, };
static unsigned long yyl79 [] = {
0x00001000, 0x00000000, 0x80000000, 0x00000000, 0x00000008, 0x00000000, 
0x00000000, 0x02000000, 0x00000000, 0x00400000, 0x00000000, 0x01000000, 
0x00020239, 0x00000000, 0x00000000, };
static unsigned long yyl80 [] = {
0x00001000, 0x00000000, 0x16000002, 0x00000000, 0x00000000, 0x05000000, 
0x20000104, 0x00000008, 0x40000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00080000, 0x00000000, };
static unsigned long yyl81 [] = {
0x00000078, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000010, 0x00000020, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl82 [] = {
0x00000001, 0x00000010, 0x00100200, 0x000001A0, 0x40000008, 0x001C0000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x00008200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl83 [] = {
0x0200B04F, 0x02280010, 0x11180281, 0x0A4081A0, 0x7000000C, 0x001C7140, 
0x07607019, 0x51030000, 0x000C2010, 0x1C284002, 0x0860A240, 0x924C3271, 
0x20008146, 0x0060000C, 0x000000F0, };
static unsigned long yyl84 [] = {
0x00000001, 0x00000010, 0x10100000, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl85 [] = {
0x20001001, 0x00000010, 0x00100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x20030000, 0x00002000, 0x00084000, 0x0008A200, 0x12001238, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl86 [] = {
0x00101003, 0x40100010, 0x00300200, 0x000021A4, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002800, 0x00085800, 0x0000A200, 0x12001230, 
0x00000200, 0x00200000, 0x00000000, };
static unsigned long yyl87 [] = {
0x00001000, 0x00006002, 0x0007FC00, 0x06080000, 0x00000000, 0x00010000, 
0x60060004, 0x00000010, 0x10004000, 0x00008050, 0x00000000, 0x00100000, 
0x08000400, 0x00002200, 0x00000000, };
static unsigned long yyl88 [] = {
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00400000, 0x00000000, 0x01000000, 
0x00000038, 0x00000000, 0x00000000, };
static unsigned long yyl89 [] = {
0x00001078, 0x20010000, 0x00000004, 0x00040020, 0x00000000, 0x20010000, 
0x20000004, 0x00002004, 0x00000000, 0xA0000000, 0x00028000, 0x00200000, 
0x01000000, 0x00002000, 0x000000F0, };
static unsigned long yyl90 [] = {
0x00001078, 0x20010000, 0x00000000, 0x00040020, 0x00000000, 0x20000000, 
0x00000000, 0x00000004, 0x00000000, 0x20000000, 0x00000000, 0x00200000, 
0x01000000, 0x00002000, 0x000000F0, };
static unsigned long yyl91 [] = {
0x0208B102, 0x22280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x81007140, 
0x02604219, 0x51080010, 0x000C0090, 0x1CA00002, 0x08604040, 0x844C2041, 
0x20008146, 0x054C000C, 0x00000000, };
static unsigned long yyl92 [] = {
0x0208BDFE, 0x022C0000, 0x01080081, 0x0A448020, 0x37FF7F75, 0x80007140, 
0x0261401B, 0x51000100, 0x020C8090, 0x1C200002, 0x08600040, 0x804C20C1, 
0x21118146, 0xF84D001C, 0x00007FFF, };
static unsigned long yyl93 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200000, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl94 [] = {
0x0200B04E, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x000000F0, };
static unsigned long yyl95 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl96 [] = {
0x0000004F, 0x00000010, 0x00100200, 0x000001A0, 0x40000008, 0x001C0000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x00008200, 0x12001230, 
0x00000000, 0x00200000, 0x000000F0, };
static unsigned long yyl97 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x00002200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl98 [] = {
0x00000041, 0x00400010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x000000F0, };
static unsigned long yyl99 [] = {
0x0208BDFE, 0x022C0000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007540, 
0x0260401B, 0x51000100, 0x040C0090, 0x1C200002, 0x08E00040, 0x804C2041, 
0x20008146, 0xF84C100C, 0x00007FFF, };
static unsigned long yyl100 [] = {
0x00081DFE, 0x00040000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000100, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0xF8000000, 0x00007FFF, };
static unsigned long yyl101 [] = {
0x00081DFE, 0x00040000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00010000, 0x00000100, 0x00008000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0xB8000000, 0x00007FFF, };
static unsigned long yyl102 [] = {
0x00001078, 0x20010000, 0x00000004, 0x00040020, 0x00000000, 0x20010000, 
0x20000004, 0x00002004, 0x00000000, 0xA0000000, 0x00028000, 0x00000000, 
0x01000000, 0x00002000, 0x000000F0, };
static unsigned long yyl103 [] = {
0x00001078, 0x20010000, 0x00000000, 0x00040020, 0x00000000, 0x20000000, 
0x00000000, 0x00000004, 0x00000000, 0x20000000, 0x00000000, 0x00000000, 
0x01000000, 0x00002000, 0x000000F0, };
static unsigned long yyl104 [] = {
0x00001000, 0x00004000, 0x00000020, 0x06000000, 0x00000000, 0x00000000, 
0x40000080, 0x00004010, 0x00000000, 0x00000010, 0x00000000, 0x01100000, 
0x88000080, 0x00002200, 0x00000000, };
static unsigned long yyl105 [] = {
0x0200B04E, 0x02280000, 0x11080081, 0x0A408000, 0x3000000C, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08602040, 0x804C2041, 
0x20008146, 0x0040000C, 0x000000F0, };
static unsigned long yyl106 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002005, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl107 [] = {
0x0200B07E, 0x02280000, 0x01080081, 0x0A448020, 0x37FF7F75, 0x80007140, 
0x0260401B, 0x51000000, 0x020C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x21118146, 0x004D001C, 0x000000F0, };
static unsigned long yyl108 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007540, 
0x0260401B, 0x51000000, 0x040C0090, 0x1C200002, 0x08E00040, 0x804C2041, 
0x20008146, 0x004C100C, 0x00000000, };
static unsigned long yyl109 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002045, 0x00007540, 
0x0260401B, 0x51000000, 0x040C0090, 0x1C200002, 0x08E00040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl110 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl111 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007142, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600140, 0x806C2041, 
0x20008146, 0x084C000C, 0x00000000, };
static unsigned long yyl112 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002205, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl113 [] = {
0x0AEDB1FA, 0x022DD768, 0x038800AD, 0x0A408820, 0x37FFFFF7, 0xF001754E, 
0x7260465F, 0x553863D4, 0x0C0C58F6, 0xBCB00012, 0x89E68048, 0x8C7C2141, 
0x39408146, 0x4F4C33CD, 0x000039F1, };
static unsigned long yyl114 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002405, 0x00007540, 
0x02604019, 0x51000000, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0048000C, 0x00000000, };
static unsigned long yyl115 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl116 [] = {
0x0208B1FA, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007540, 
0x02604219, 0x51080100, 0x000C00B0, 0x1CA00002, 0x08600040, 0x844C2041, 
0x20008146, 0x0544000C, 0x000000F0, };
static unsigned long yyl117 [] = {
0x000800FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000100, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl118 [] = {
0x0200A000, 0x02280000, 0x01080081, 0x0A408000, 0x30000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl119 [] = {
0x00000000, 0x00000002, 0x00000010, 0x00080000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00040000, 0x00000000, 0x00000000, 0x00010000, 
0x00000000, 0x00001000, 0x00000000, };
static unsigned long yyl120 [] = {
0x0200A000, 0x02280000, 0x01080081, 0x0A408000, 0x00000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040001C, 0x00000000, };
static unsigned long yyl121 [] = {
0x0200B07A, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20118146, 0x004D001C, 0x000000F0, };
static unsigned long yyl122 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FE7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl123 [] = {
0x00001000, 0x00000000, 0x00000000, 0x00000020, 0x37FF7F71, 0x00000000, 
0x00000000, 0x00000000, 0x00000080, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x000C0000, 0x00000000, };
static unsigned long yyl124 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl125 [] = {
0x0200B07A, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007140, 
0x0260401B, 0x51000000, 0x020C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20018146, 0x004D001C, 0x000000F0, };
static unsigned long yyl126 [] = {
0x0200B07A, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007140, 
0x02614019, 0x51000000, 0x000C8090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20118146, 0x004D001C, 0x000000F0, };
static unsigned long yyl127 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408000, 0x3002200D, 0x00007140, 
0x9A60C019, 0x51040001, 0x000C00A0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004A000C, 0x00000000, };
static unsigned long yyl128 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30102005, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl129 [] = {
0x0200B000, 0x02280002, 0x01080091, 0x4A488020, 0x37FF7F75, 0x00007140, 
0x02E04019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804D2041, 
0x20008146, 0x004C100C, 0x00000000, };
static unsigned long yyl130 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30402005, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl131 [] = {
0x0008104E, 0x00000000, 0x00000000, 0x00010000, 0x00000000, 0x00000000, 
0x08000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl132 [] = {
0x0204B000, 0x02280408, 0x01080081, 0x0A408000, 0x3400A005, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl133 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001220, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl134 [] = {
0x04005000, 0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00080000, 
0x00040000, 0x00044000, 0x20800000, 0x20000001, 0x00100020, 0x00420000, 
0x00000001, 0x00000000, 0x00000000, };
static unsigned long yyl135 [] = {
0x00001000, 0x20010000, 0x00000004, 0x00040020, 0x00000000, 0x00010000, 
0x20000004, 0x00002004, 0x00000000, 0xA0000000, 0x00028000, 0x00000000, 
0x01000000, 0x00002000, 0x00000000, };
static unsigned long yyl136 [] = {
0x00001000, 0x20010000, 0x00000000, 0x00040020, 0x00000000, 0x00000000, 
0x00000000, 0x00000004, 0x00000000, 0x20000000, 0x00000000, 0x00000000, 
0x01000000, 0x00002000, 0x00000000, };
static unsigned long yyl137 [] = {
0x0200B000, 0x02289360, 0x03080081, 0x0A408020, 0x37FF7F77, 0x50007140, 
0x02604459, 0x513002C0, 0x080C00D0, 0x1C300002, 0x89640040, 0x886C2141, 
0x30408146, 0x024C014D, 0x00000000, };
static unsigned long yyl138 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002015, 0x00007140, 
0x02604019, 0x55000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0048000C, 0x00000000, };
static unsigned long yyl139 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002015, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0048000C, 0x00000000, };
static unsigned long yyl140 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002015, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl141 [] = {
0x000800F8, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000100, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl142 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002045, 0x00007540, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl143 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002405, 0x00007540, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl144 [] = {
0x0208B1FA, 0x0228D308, 0x030800A9, 0x0A408020, 0x37FF7F75, 0x90007148, 
0x02604659, 0x55284180, 0x080C0090, 0x1CA00002, 0x09600040, 0x846C2041, 
0x20008146, 0x054C008D, 0x000000F0, };
static unsigned long yyl145 [] = {
0x0208B1FA, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x80007140, 
0x02604219, 0x51080100, 0x000C0090, 0x1CA00002, 0x08600040, 0x844C2041, 
0x20008146, 0x0544000C, 0x000000F0, };
static unsigned long yyl146 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30003005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl147 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl148 [] = {
0x0200A000, 0x02280000, 0x01080081, 0x0A408000, 0x00000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl149 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x0000F140, 
0x02E04819, 0x51000000, 0x020C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl150 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x0000F140, 
0x02E04819, 0x51000000, 0x020C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl151 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F65, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl152 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007540, 
0x0260401B, 0x51000000, 0x040C0090, 0x1C200002, 0x08E00040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl153 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl154 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007540, 
0x02604019, 0x51000000, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl155 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7B75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl156 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FF7F7D, 0x00007140, 
0x9A60C019, 0x51040001, 0x000C00A0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004E000C, 0x00000000, };
static unsigned long yyl157 [] = {
0x0204B000, 0x02280408, 0x01080081, 0x0A408020, 0x37FFFF75, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl158 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408000, 0x3002200D, 0x00007140, 
0x8A60C019, 0x51040000, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004A000C, 0x00000000, };
static unsigned long yyl159 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408000, 0x3002200D, 0x00007140, 
0x9260C019, 0x51040001, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004A000C, 0x00000000, };
static unsigned long yyl160 [] = {
0x0200B000, 0x02280000, 0x01080091, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02E04019, 0x51000000, 0x00080090, 0x1C200002, 0x08600040, 0x804D2041, 
0x20008146, 0x0044100C, 0x00000000, };
static unsigned long yyl161 [] = {
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000002, 
0x00000020, 0x00000400, 0x00000080, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x38000000, 0x0000000C, };
static unsigned long yyl162 [] = {
0x00000102, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04200000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl163 [] = {
0x00080102, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl164 [] = {
0x00000001, 0x00400010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl165 [] = {
0x0000007A, 0x00000000, 0x00000000, 0x00000000, 0x00000008, 0x80000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000004, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl166 [] = {
0x000001F8, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x20000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl167 [] = {
0x00101000, 0x40100000, 0x00000000, 0x00002004, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl168 [] = {
0x00001002, 0x40100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl169 [] = {
0x00101002, 0x40100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl170 [] = {
0x00101002, 0x00100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl171 [] = {
0x04005002, 0x00006006, 0x0007FC00, 0x46080000, 0x00000000, 0x00090000, 
0x60060004, 0x00044010, 0x30804000, 0x20008051, 0x00100020, 0x00520000, 
0x08000501, 0x08082200, 0x00000000, };
static unsigned long yyl172 [] = {
0x00001000, 0x00000000, 0x00000000, 0x00100000, 0x00000000, 0x00000010, 
0x00040000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00020201, 0x00000000, 0x00000000, };
static unsigned long yyl173 [] = {
0x00001000, 0x20010000, 0x00000004, 0x00040020, 0x00000000, 0x00010000, 
0x20000004, 0x00002004, 0x00000000, 0xA0000000, 0x00028000, 0x00000000, 
0x00000000, 0x00002000, 0x00000000, };
static unsigned long yyl174 [] = {
0x00001000, 0x20010000, 0x00000000, 0x00040020, 0x00000000, 0x00000000, 
0x00000000, 0x00000004, 0x00000000, 0x20000000, 0x00000000, 0x00000000, 
0x00000000, 0x00002000, 0x00000000, };
static unsigned long yyl175 [] = {
0x00000000, 0x00000800, 0x00000000, 0x00000000, 0x08000000, 0x00000410, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl176 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002015, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl177 [] = {
0x0000007A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl178 [] = {
0x0201B000, 0x022C0000, 0x01880081, 0x0A408020, 0x37FF7F75, 0x00007540, 
0x0260401B, 0x51000000, 0x040C0090, 0x1C200002, 0x08E00048, 0x804C2041, 
0x20008146, 0x004C200C, 0x00000000, };
static unsigned long yyl179 [] = {
0x0209B1FA, 0x022C0000, 0x01880081, 0x0A408020, 0x37FF7FF5, 0x80007540, 
0x0260421B, 0x51080100, 0x040C0090, 0x1CA00002, 0x08E00048, 0x844C2041, 
0x20008146, 0x054C200C, 0x000000F0, };
static unsigned long yyl180 [] = {
0x0201B000, 0x022C0000, 0x01880081, 0x0A408020, 0x37FF7FF5, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600048, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl181 [] = {
0x0208B1FA, 0x02280008, 0x010800A9, 0x0A408020, 0x37FF7F75, 0x80007140, 
0x02604219, 0x55084100, 0x000C0090, 0x1CA00002, 0x08600040, 0x844C2041, 
0x20008146, 0x054C008C, 0x000000F0, };
static unsigned long yyl182 [] = {
0x0200B000, 0x022C0000, 0x01080081, 0x0A408000, 0x30002005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600048, 0x804C2041, 
0x20008146, 0x0040200C, 0x00000000, };
static unsigned long yyl183 [] = {
0x0200A000, 0x02280000, 0x01080081, 0x0A408000, 0x00000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl184 [] = {
0x0300A000, 0x02280000, 0x01080081, 0x0A408000, 0x00000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl185 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x55000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl186 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F65, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl187 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007540, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl188 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7D75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl189 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007540, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl190 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FF7F7D, 0x00007140, 
0x8A60C019, 0x51040000, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004E000C, 0x00000000, };
static unsigned long yyl191 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FF7F7D, 0x00007140, 
0x9260C019, 0x51040001, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004E000C, 0x00000000, };
static unsigned long yyl192 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37EF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl193 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37BF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl194 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x33FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl195 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408000, 0x3002200D, 0x00007140, 
0x8260C019, 0x51040000, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004A000C, 0x00000000, };
static unsigned long yyl196 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408000, 0x3002200D, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl197 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08604040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl198 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl199 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02E04019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C100C, 0x00000000, };
static unsigned long yyl200 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30802005, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl201 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200042, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl202 [] = {
0x000800FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000100, 0x40000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl203 [] = {
0x00000001, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x02084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl204 [] = {
0x00101002, 0x40100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl205 [] = {
0x0008014A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x44000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl206 [] = {
0x0008014A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl207 [] = {
0x000001F8, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl208 [] = {
0x00001000, 0x00006000, 0x0007FC00, 0x06100000, 0x00000000, 0x00010010, 
0x60020004, 0x00000010, 0x10004000, 0x00008050, 0x00000000, 0x00100000, 
0x08020601, 0x00002200, 0x00000000, };
static unsigned long yyl209 [] = {
0x00001000, 0x00010000, 0x00000004, 0x00040020, 0x00000000, 0x00010000, 
0x20000004, 0x00002004, 0x00000000, 0xA0000000, 0x00028000, 0x00000000, 
0x00000000, 0x00002000, 0x00000000, };
static unsigned long yyl210 [] = {
0x00001000, 0x00010000, 0x00000000, 0x00040020, 0x00000000, 0x00000000, 
0x00000000, 0x00000004, 0x00000000, 0x20000000, 0x00000000, 0x00000000, 
0x00000000, 0x00002000, 0x00000000, };
static unsigned long yyl211 [] = {
0x0008004E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl212 [] = {
0x000801FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000200, 0x00080100, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl213 [] = {
0x0208B1FA, 0x02289360, 0x03080081, 0x0A408020, 0x37FF7F77, 0x50007140, 
0x22604659, 0x513802C0, 0x080C00D0, 0x1CB00002, 0x89640040, 0x8C6C2141, 
0x30408146, 0x074C014D, 0x000000F0, };
static unsigned long yyl214 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x806C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl215 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x806C2043, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl216 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30000005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl217 [] = {
0x0200B002, 0x02280002, 0x01080091, 0x4A488020, 0x37FF7F75, 0x00007140, 
0x02E04019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804D2041, 
0x20008146, 0x004C100C, 0x00000000, };
static unsigned long yyl218 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F74, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl219 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F65, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl220 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F55, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl221 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F35, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl222 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7E75, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl223 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7B75, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl224 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7775, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl225 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF3F75, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl226 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FF7F7D, 0x00007140, 
0x8260C019, 0x51040000, 0x000C00B0, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004E000C, 0x00000000, };
static unsigned long yyl227 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FF7F7D, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl228 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl229 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x36FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl230 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408000, 0x3002200D, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl231 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30082005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl232 [] = {
0x0200B000, 0x02280000, 0x01080091, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02E04019, 0x51000000, 0x004C0090, 0x1C200002, 0x08600040, 0x804D2041, 
0x20008146, 0x004C100C, 0x00000000, };
static unsigned long yyl233 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x12604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl234 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x32002005, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20009146, 0x0040000C, 0x00000000, };
static unsigned long yyl235 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200042, 0x08600040, 0x804C2041, 
0x20009146, 0x004C000C, 0x00000000, };
static unsigned long yyl236 [] = {
0x00000041, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl237 [] = {
0x00000000, 0x00000000, 0x00000000, 0x80000000, 0x00000000, 0x00000000, 
0x20000000, 0x00000000, 0x00000000, 0x01000000, 0x00000000, 0x00020000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl238 [] = {
0x00000102, 0x00000000, 0x00000000, 0x82800000, 0x00000000, 0x00090000, 
0x00000000, 0x20004002, 0x00000000, 0x00001800, 0x00000004, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl239 [] = {
0x00000000, 0x00000080, 0x00000000, 0x00000000, 0x00000000, 0x00000800, 
0x20000000, 0x00400000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl240 [] = {
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00040000, 0x00004000, 0x00000000, 0x20000000, 0x00000020, 0x00020000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl241 [] = {
0x00080102, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x20000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl242 [] = {
0x00101002, 0x40100000, 0x00200000, 0x00002005, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl243 [] = {
0x00101000, 0x40100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl244 [] = {
0x0208B1FA, 0x02289360, 0x03080081, 0x0A408020, 0x37FF7F77, 0x50007140, 
0x02604659, 0x513802C0, 0x080C00D0, 0x1CB00002, 0x89640040, 0x8C6C2141, 
0x30408146, 0x074C014D, 0x000000F0, };
static unsigned long yyl245 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x20007140, 
0x02604019, 0x55000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl246 [] = {
0x0201B000, 0x022C0000, 0x01880081, 0x0A408020, 0x37FF7FF5, 0x00007540, 
0x0260401B, 0x51000000, 0x040C0090, 0x1C200002, 0x08E00048, 0x804C2041, 
0x20008146, 0x004C200C, 0x00000000, };
static unsigned long yyl247 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x34522205, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl248 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408000, 0x30002805, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x806C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl249 [] = {
0x0208B000, 0x22280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x01007140, 
0x02604019, 0x51000010, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl250 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FF7F7D, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl251 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FD7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl252 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x37FB7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0044000C, 0x00000000, };
static unsigned long yyl253 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x377F7F75, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl254 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20009146, 0x0044000C, 0x00000000, };
static unsigned long yyl255 [] = {
0x0A00A000, 0x02280000, 0x01080081, 0x0A408000, 0x00000004, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl256 [] = {
0x0200B000, 0x02280000, 0x01080091, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02E04019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804D2041, 
0x20008146, 0x004C100C, 0x00000000, };
static unsigned long yyl257 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x32002005, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl258 [] = {
0x0200B000, 0x02280008, 0x01080081, 0x0A408020, 0x37FFFF75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08604040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl259 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x34002005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl260 [] = {
0x20001000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x20000000, 0x00000000, 0x00000000, 0x00080000, 0x00000008, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl261 [] = {
0x00000102, 0x00000000, 0x00000000, 0x02800000, 0x00000000, 0x00080000, 
0x00000000, 0x20004002, 0x00000000, 0x00001800, 0x00000004, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl262 [] = {
0x00080102, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x04000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl263 [] = {
0x000801FA, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000200, 0x00080000, 0x00000000, 0x00800000, 0x00000000, 0x44000000, 
0x00000000, 0x05000000, 0x000000F0, };
static unsigned long yyl264 [] = {
0x00005000, 0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00080000, 
0x00040000, 0x00044000, 0x20800000, 0x20000001, 0x00100020, 0x00420000, 
0x00000001, 0x00000000, 0x00000000, };
static unsigned long yyl265 [] = {
0x0018114A, 0x40100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00000200, 0x00080000, 0x00000800, 0x00801800, 0x00000000, 0x04000000, 
0x00000200, 0x00000000, 0x000000F0, };
static unsigned long yyl266 [] = {
0x00000002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00008000, 
0x00800800, 0x00000000, 0x02000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl267 [] = {
0x0008104E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl268 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30002455, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl269 [] = {
0x0200B002, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x806C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl270 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x040C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl271 [] = {
0x0000004E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000000F0, };
static unsigned long yyl272 [] = {
0x00000001, 0x00400010, 0x00100200, 0x000001A0, 0x40000008, 0x001C0000, 
0x05003000, 0x00030000, 0x00002000, 0x00084000, 0x00008200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl273 [] = {
0x00000041, 0x00000010, 0x10100200, 0x000001A0, 0x40000008, 0x001C1000, 
0x05003000, 0x00030000, 0x00002000, 0x02084000, 0x0000A200, 0x12001230, 
0x00000000, 0x00200000, 0x00000000, };
static unsigned long yyl274 [] = {
0x20001002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00000000, 0x20000000, 0x00000000, 0x00000000, 0x00080000, 0x00000008, 
0x00000000, 0x00000000, 0x00000000, };
static unsigned long yyl275 [] = {
0x00101002, 0x40100000, 0x00200000, 0x00002004, 0x00000000, 0x00000000, 
0x00010000, 0x00000000, 0x00000800, 0x00001800, 0x00000000, 0x00000000, 
0x00000200, 0x00000000, 0x00000000, };
static unsigned long yyl276 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x31006925, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl277 [] = {
0x0208B102, 0x22280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x81007140, 
0x02604219, 0x51080010, 0x000C0090, 0x1CA00002, 0x08600040, 0x844C2041, 
0x20008146, 0x054C000C, 0x00000000, };
static unsigned long yyl278 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408020, 0x35FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x00080010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl279 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30022005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl280 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x30042005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl281 [] = {
0x0200B000, 0x02280000, 0x01080081, 0x0A408000, 0x32802005, 0x00007140, 
0x02604019, 0x51000000, 0x000C0010, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x0040000C, 0x00000000, };
static unsigned long yyl282 [] = {
0x00001100, 0x0000D360, 0x02000028, 0x02000000, 0x00000002, 0x70000008, 
0x40000440, 0x00204290, 0x08004040, 0x00100010, 0x01040000, 0x00300100, 
0x09000000, 0x03003201, 0x00000000, };
static unsigned long yyl283 [] = {
0x04005000, 0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
0x00040000, 0x00044000, 0x20800000, 0x20000001, 0x00100020, 0x00420000, 
0x00000001, 0x00000000, 0x00000000, };
static unsigned long yyl284 [] = {
0x04005000, 0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00080000, 
0x00040000, 0x00044000, 0x20800000, 0x20000001, 0x00100020, 0x00420000, 
0x00000101, 0x00000000, 0x00000000, };
static unsigned long yyl285 [] = {
0x04005002, 0x00000004, 0x00000000, 0x40000000, 0x00000000, 0x00080000, 
0x00040000, 0x00044000, 0x20800000, 0x20000001, 0x00100020, 0x00420000, 
0x00000101, 0x00080000, 0x00000000, };
static unsigned long yyl286 [] = {
0x0204B000, 0x02280000, 0x01080081, 0x0A408020, 0x37FF7F75, 0x00007140, 
0x02604019, 0x51000000, 0x000C0090, 0x1C200002, 0x08600040, 0x804C2041, 
0x20008146, 0x004C000C, 0x00000000, };
static unsigned long yyl287 [] = {
0x04005000, 0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00080000, 
0x00040000, 0x00044000, 0x20800000, 0x20000000, 0x00100020, 0x00420000, 
0x00000101, 0x00000000, 0x00000000, };
static unsigned long yyl288 [] = {
0x00001000, 0x0000D360, 0x02000028, 0x02000000, 0x00000002, 0x70000008, 
0x40000440, 0x00204290, 0x08004040, 0x00100010, 0x01040000, 0x00300100, 
0x09000000, 0x03003201, 0x00000000, };
static unsigned long yyl289 [] = {
0x04005000, 0x00000004, 0x00000000, 0x40000000, 0x00000000, 0x00080000, 
0x00040000, 0x00044000, 0x20800000, 0x20000001, 0x00100020, 0x00420000, 
0x00000101, 0x00080000, 0x00000000, };
static unsigned long yyl290 [] = {
0x0000007A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x80000000, 
0x00000000, 0x00000000, 0x00000000, 0x00000020, 0x00000000, 0x00000000, 
0x00000000, 0x00000000, 0x000001F1, };

static	unsigned long *	yyDefaultLook	[yyLastReadState + 1] = { 0,
   yyl1,   yyl2,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,   yyl3,      0,
      0,      0,   yyl4,      0,   yyl4,   yyl5,      0,      0,
   yyl6,   yyl4,   yyl4,      0,      0,      0,   yyl4,   yyl4,
   yyl4,   yyl4,   yyl7,      0,      0,   yyl8,      0,      0,
      0,      0,      0,      0,   yyl9,      0,  yyl10,  yyl10,
      0,      0,      0,      0,      0,      0,  yyl11,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,  yyl12,
  yyl13,  yyl14,  yyl15,      0,      0,      0,      0,      0,
  yyl15,      0,      0,   yyl2,  yyl16,      0,      0,      0,
      0,      0,   yyl7,   yyl7,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,  yyl17,  yyl18,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,  yyl19,      0,
  yyl20,  yyl21,      0,   yyl1,  yyl22,  yyl23,  yyl24,  yyl25,
  yyl26,      0,  yyl22,      0,      0,  yyl27,      0,      0,
  yyl28,  yyl28,      0,      0,      0,      0,      0,  yyl29,
      0,      0,      0,      0,  yyl30,      0,      0,      0,
  yyl31,  yyl32,      0,  yyl32,      0,   yyl9,  yyl31,  yyl31,
  yyl31,  yyl31,  yyl33,      0,  yyl34,      0,  yyl35,  yyl36,
      0,  yyl36,  yyl36,      0,      0,      0,      0,      0,
      0,      0,      0,  yyl12,      0,      0,  yyl37,  yyl38,
      0,  yyl39,  yyl40,      0,  yyl41,      0,  yyl42,      0,
  yyl43,      0,  yyl24,      0,  yyl26,      0,      0,  yyl42,
  yyl44,  yyl45,  yyl45,  yyl46,  yyl47,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,  yyl31,
  yyl31,  yyl31,  yyl32,  yyl32,  yyl31,  yyl31,  yyl31,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,  yyl17,  yyl17,      0,  yyl18,  yyl18,      0,  yyl19,
      0,      0,      0,      0,  yyl48,   yyl1,      0,      0,
  yyl49,   yyl1,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,   yyl5,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,  yyl46,      0,   yyl7,  yyl47,  yyl28,   yyl7,
      0,      0,  yyl50,      0,  yyl51,  yyl28,   yyl9,  yyl31,
  yyl31,  yyl31,  yyl31,  yyl31,  yyl31,      0,      0,  yyl31,
  yyl31,  yyl52,  yyl53,  yyl53,      0,  yyl52,  yyl52,  yyl52,
  yyl52,      0,  yyl52,  yyl53,  yyl53,      0,  yyl52,  yyl52,
  yyl52,  yyl52,      0,      0,  yyl34,      0,      0,      0,
      0,      0,      0,      0,      0,  yyl54,  yyl55,      0,
      0,  yyl56,   yyl1,  yyl57,   yyl1,   yyl1,      0,      0,
  yyl42,  yyl58,      0,      0,      0,  yyl42,  yyl58,  yyl59,
  yyl45,      0,      0,      0,      0,  yyl60,      0,   yyl5,
      0,      0,      0,  yyl61,   yyl5,  yyl62,      0,      0,
      0,      0,      0,      0,   yyl5,   yyl5,      0,      0,
   yyl5,      0,      0,   yyl5,      0,      0,      0,      0,
      0,      0,      0,      0,      0,  yyl63,  yyl64,      0,
      0,      0,  yyl65,  yyl46,  yyl46,      0,  yyl66,      0,
      0,      0,      0,      0,      0,  yyl67,      0,  yyl45,
      0,      0,  yyl68,  yyl69,  yyl70,  yyl70,  yyl68,      0,
      0,  yyl31,  yyl31,  yyl52,  yyl52,  yyl52,  yyl52,  yyl52,
  yyl52,  yyl52,  yyl52,      0,  yyl52,  yyl52,  yyl52,  yyl52,
  yyl52,  yyl52,  yyl52,  yyl52,      0,  yyl12,      0,      0,
      0,      0,      0,  yyl71,      0,      0,  yyl72,  yyl73,
  yyl74,  yyl74,   yyl1,      0,  yyl75,      0,      0,      0,
      0,      0,      0,      0,      0,  yyl58,  yyl76,      0,
      0,      0,      0,  yyl77,      0,      0,      0,  yyl78,
  yyl79,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,  yyl25,  yyl25,
  yyl25,  yyl25,  yyl80,      0,  yyl25,      0,      0,      0,
      0,      0,      0,      0,      0,      0,  yyl77,      0,
      0,      0,      0,      0,      0,      0,      0,   yyl5,
  yyl46,  yyl81,  yyl81,      0,      0,      0,  yyl46,  yyl46,
      0,      0,      0,      0,      0,  yyl29,      0,      0,
      0,      0,      0,      0,      0,      0,      0,  yyl82,
  yyl83,  yyl74,      0,      0,  yyl84,  yyl82,  yyl82,  yyl85,
      0,   yyl1,      0,      0,  yyl86,   yyl1,   yyl1,      0,
      0,      0,  yyl87,  yyl42,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,  yyl88,
      0,  yyl24,  yyl24,      0,      0,      0,      0,  yyl24,
  yyl24,  yyl89,      0,  yyl24,  yyl24,  yyl45,  yyl45,  yyl24,
  yyl25,  yyl25,  yyl80,  yyl26,      0,      0,      0,      0,
  yyl26,  yyl90,  yyl45,  yyl42,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,  yyl91,
  yyl91,  yyl52,  yyl52,  yyl52,  yyl52,      0,      0,      0,
  yyl92,  yyl92,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,  yyl93,      0,  yyl94,  yyl95,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,  yyl95,      0,      0,      0,
      0,      0,   yyl1,  yyl82,   yyl1,  yyl96,  yyl96,      0,
      0,      0,  yyl82,  yyl74,  yyl74,      0,      0,  yyl97,
  yyl98,   yyl1,      0,      0,   yyl1,   yyl1,   yyl1,   yyl1,
   yyl1,      0,  yyl76,      0,  yyl42,  yyl42,  yyl99, yyl100,
      0,      0,      0, yyl101, yyl100,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,  yyl24,  yyl24,  yyl24,  yyl24,      0,
      0,  yyl24,      0,  yyl24,  yyl24,      0,      0, yyl102,
      0,      0,  yyl25,      0,  yyl26,  yyl26,  yyl26,  yyl26,
      0, yyl103,      0,  yyl42,      0,      0,      0, yyl104,
      0,      0,      0,  yyl46,      0,      0,  yyl70,  yyl70,
      0, yyl105,      0,      0, yyl106,      0,      0,      0,
  yyl95,      0, yyl107, yyl108, yyl109,  yyl95,  yyl95,  yyl95,
 yyl110, yyl111,      0,      0, yyl112,      0,      0,      0,
 yyl113,      0, yyl114, yyl115, yyl116,      0,      0, yyl115,
 yyl115, yyl115,      0,      0,      0,      0, yyl117,      0,
  yyl95,  yyl95,  yyl95,      0, yyl118,  yyl95,  yyl95,      0,
      0, yyl119,      0,      0,      0, yyl120,      0,      0,
      0,      0,  yyl95, yyl121,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
 yyl122, yyl123, yyl124,      0,      0, yyl125, yyl126,      0,
 yyl127,      0,      0,      0, yyl128,      0,      0,      0,
      0,      0,      0, yyl129, yyl130,  yyl95,  yyl95,      0,
      0,      0,      0,  yyl95,      0,      0,      0, yyl131,
 yyl132,   yyl1,      0,  yyl96,      0,      0,   yyl1,  yyl82,
  yyl74,  yyl74,      0,      0, yyl133,      0,      0, yyl134,
   yyl5,      0,      0,      0,      0,  yyl87,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,  yyl24,  yyl24,  yyl24,      0,      0,  yyl24,
  yyl24,      0,      0,  yyl24,      0,  yyl61,  yyl24, yyl135,
      0,      0,      0,      0,  yyl26,  yyl26, yyl136,      0,
      0,      0,      0,      0,      0,      0, yyl105,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0, yyl137, yyl115, yyl115, yyl138, yyl139, yyl140, yyl106,
      0,      0,      0,      0, yyl141,      0,      0,      0,
 yyl142,      0, yyl110,      0,      0, yyl115, yyl115, yyl124,
      0,      0, yyl143,  yyl95,      0,      0,      0, yyl144,
 yyl145,      0,      0, yyl141, yyl124,      0,      0,      0,
      0, yyl146,      0,      0,      0,  yyl95,      0,  yyl95,
      0,  yyl95,  yyl95, yyl147,      0,      0,      0,      0,
      0,      0,      0, yyl141,      0, yyl148, yyl149, yyl150,
 yyl149, yyl149, yyl150, yyl149, yyl150,      0, yyl151,      0,
      0,      0, yyl152,      0,      0, yyl153, yyl154, yyl155,
      0,      0, yyl118,      0, yyl156,      0,      0, yyl153,
      0,      0, yyl153,      0,      0,      0,      0, yyl157,
      0,      0,      0,      0,      0,      0,      0, yyl158,
 yyl158, yyl159,      0,      0,      0,      0, yyl115,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
 yyl160, yyl161, yyl115,      0, yyl162,      0, yyl141,      0,
 yyl163,      0,      0, yyl115,      0,  yyl82,  yyl74, yyl164,
      0,      0,      0,      0, yyl165,      0, yyl134, yyl134,
      0,      0,      0,      0,      0,      0,      0,      0,
      0, yyl166,      0,      0,      0, yyl167,      0, yyl168,
 yyl169, yyl170,      0,      0,  yyl42, yyl171,  yyl87,      0,
      0,      0,      0,  yyl28, yyl172,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
  yyl24,      0,  yyl24,      0,      0,  yyl24,  yyl24,      0,
  yyl24,      0,      0, yyl173,      0,      0,      0, yyl174,
      0,      0,      0, yyl175,      0,      0,      0,      0,
      0, yyl176,      0,      0,      0,      0, yyl106, yyl115,
      0,      0,      0,  yyl95,      0, yyl139,      0, yyl140,
 yyl137, yyl106, yyl115, yyl177, yyl177, yyl177,      0,      0,
 yyl106,      0, yyl106,      0,      0, yyl152, yyl178, yyl179,
      0, yyl106,      0, yyl180,  yyl95,      0,      0,      0,
      0, yyl106,  yyl95, yyl148,      0, yyl115,      0, yyl106,
      0, yyl181, yyl177, yyl177, yyl177,      0,      0, yyl106,
      0, yyl115, yyl182,      0, yyl115, yyl183, yyl184,      0,
 yyl115,      0,      0,      0,      0,      0,      0,      0,
      0,  yyl45,      0,  yyl95,      0,      0,      0,      0,
  yyl95,      0,      0,      0,      0,  yyl95,      0, yyl106,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0, yyl185,  yyl95, yyl124, yyl186,      0,      0,
      0, yyl141, yyl187,      0,  yyl95, yyl188, yyl189,      0,
 yyl141,      0,      0, yyl141, yyl190, yyl190, yyl191,      0,
      0, yyl192,      0,      0, yyl193,      0,      0, yyl141,
      0, yyl194,      0,      0,      0,      0,      0,      0,
 yyl195, yyl195,      0, yyl196,      0,      0,      0,  yyl95,
 yyl115,      0,      0,      0, yyl124, yyl197,      0,      0,
      0, yyl198, yyl198, yyl198, yyl198,      0, yyl199,      0,
      0,      0,  yyl95, yyl115, yyl200, yyl201,      0,      0,
      0, yyl106,      0,      0,      0,  yyl28,  yyl28, yyl202,
 yyl202,  yyl95,      0, yyl115, yyl115, yyl115,      0,  yyl82,
 yyl164, yyl203,      0,      0,      0,      0,      0,      0,
      0,      0,      0, yyl134,      0,      0,   yyl5,      0,
      0,      0,      0,      0, yyl204, yyl204, yyl205,      0,
 yyl206,      0, yyl207,      0,      0,      0,      0,      0,
      0,  yyl42,      0,      0, yyl208,      0,      0,  yyl60,
      0,      0,  yyl24,  yyl24,      0,  yyl24,  yyl24,      0,
 yyl209, yyl210,      0,      0,      0, yyl211,      0,      0,
      0,      0, yyl115,      0,      0, yyl176,   yyl5, yyl212,
   yyl5, yyl213, yyl177,      0, yyl140,      0, yyl106,  yyl95,
 yyl115,      0,      0,      0, yyl214, yyl215, yyl106,  yyl95,
      0, yyl115,      0,      0, yyl115,      0,      0,  yyl45,
      0,      0,      0,  yyl95, yyl115, yyl115,      0,      0,
      0,      0,  yyl95, yyl115,      0, yyl148,      0, yyl115,
      0,   yyl5,      0, yyl212,      0,   yyl5, yyl177,      0,
      0,      0,      0,      0,  yyl95, yyl115,      0,      0,
      0,      0,      0,      0, yyl183,      0,      0,      0,
      0,      0, yyl216,      0,      0,      0,      0,      0,
      0,  yyl95,      0,      0,      0, yyl217, yyl217,  yyl95,
      0,  yyl95, yyl115, yyl218,      0,      0,      0, yyl186,
 yyl219,  yyl95, yyl124, yyl186,      0,      0,      0, yyl220,
      0, yyl220, yyl221, yyl222,      0, yyl223,      0,      0,
 yyl224,      0,      0,      0, yyl225, yyl226, yyl226, yyl227,
      0,      0,      0,      0,      0,      0, yyl228,      0,
      0, yyl229,      0, yyl202,      0, yyl194,      0,  yyl95,
  yyl95,      0,      0, yyl230, yyl230,      0, yyl115, yyl115,
      0, yyl115, yyl231,      0,      0,      0,      0, yyl115,
      0, yyl202,  yyl95,      0,      0,  yyl95,  yyl95,  yyl95,
  yyl95,      0,  yyl95, yyl232, yyl233,  yyl31,      0,      0,
      0,  yyl31,      0,      0, yyl106,      0,      0, yyl106,
      0,  yyl95, yyl115,      0,      0,      0, yyl234, yyl235,
      0,      0,      0,      0,      0,      0,      0, yyl115,
      0,  yyl82, yyl203, yyl236,      0,      0,      0,      0,
      0, yyl165,      0,      0,      0,      0,      0, yyl237,
      0, yyl238, yyl239, yyl240, yyl241,      0,      0, yyl134,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0, yyl242,      0,      0, yyl243,   yyl5,
 yyl204,  yyl87,  yyl87,  yyl87,      0, yyl101,      0,  yyl24,
  yyl24,  yyl26,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0, yyl244,      0,
      0, yyl106, yyl115, yyl245,      0, yyl115,      0,      0,
      0, yyl246,      0, yyl246,      0, yyl246, yyl178,      0,
      0, yyl247,      0,      0,      0,      0,      0, yyl106,
 yyl248,      0, yyl182, yyl182, yyl184,      0,      0,      0,
      0, yyl249,      0,      0, yyl249, yyl249,      0,      0,
      0, yyl129, yyl129,      0,      0,      0,      0, yyl124,
 yyl186,      0, yyl220,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
 yyl250, yyl250,      0,      0, yyl251, yyl252,      0,      0,
      0, yyl253, yyl229,      0,      0, yyl254,      0,      0,
      0, yyl115, yyl115,      0,  yyl95,      0, yyl115,      0,
      0,      0,      0,      0,      0,      0, yyl115,      0,
 yyl115,      0,      0,      0, yyl255,      0, yyl197,  yyl95,
      0,      0,      0,      0,      0,      0,      0, yyl256,
      0,      0,      0,      0,      0,      0,      0,  yyl95,
      0, yyl115,      0,      0,  yyl28, yyl115,      0,      0,
      0, yyl257,      0,      0,      0, yyl258,      0, yyl259,
  yyl98, yyl164,  yyl82, yyl236, yyl260, yyl260,      0, yyl260,
      0,      0,      0,      0,      0,      0,      0,      0,
 yyl261,      0,      0, yyl262,      0, yyl263,      0,      0,
      0, yyl264,      0,      0,      0,      0,      0,      0,
 yyl243, yyl265, yyl204, yyl265, yyl243,      0, yyl204,      0,
      0,      0,  yyl42,      0,      0, yyl266,      0,      0,
 yyl267,      0,      0, yyl267,      0,      0, yyl268,      0,
 yyl137, yyl115, yyl177, yyl177,  yyl28,      0,      0, yyl106,
      0,      0, yyl115,      0, yyl106, yyl248,      0,      0,
  yyl95,      0, yyl249,  yyl95,      0,      0, yyl129, yyl129,
 yyl106, yyl186,      0,      0,      0, yyl224, yyl269,      0,
      0, yyl251, yyl251,      0,      0,      0,      0,      0,
      0,      0,      0, yyl270,      0,      0, yyl115, yyl115,
 yyl115,      0,      0,      0,      0,      0,      0,      0,
      0, yyl115,  yyl95,      0,      0, yyl271,      0,      0,
      0,      0,      0,      0,      0,      0, yyl106,      0,
 yyl106,      0,      0, yyl115,      0, yyl272, yyl273, yyl203,
  yyl82,      0, yyl274, yyl260,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
      0,      0,      0,      0,      0, yyl265,      0,      0,
      0, yyl275,      0,      0,      0,      0, yyl266,      0,
      0,      0,      0,      0,      0,      0,      0,      0,
 yyl276, yyl115, yyl246, yyl148, yyl106, yyl115,      0,      0,
 yyl277,  yyl95, yyl271, yyl115,      0, yyl220,      0,      0,
 yyl224, yyl269, yyl225,      0,      0, yyl251,      0, yyl229,
 yyl278, yyl194,      0, yyl115,      0, yyl279, yyl280,      0,
      0,      0,  yyl95,      0,      0,      0, yyl281, yyl115,
      0,  yyl95, yyl115, yyl235, yyl115,      0, yyl203, yyl282,
 yyl236,      0,      0, yyl260,      0, yyl283, yyl134, yyl134,
      0, yyl134, yyl134,      0,      0, yyl134, yyl134, yyl134,
 yyl134, yyl134, yyl134,      0,      0,      0,      0,      0,
 yyl266,      0,      0,      0, yyl215,      0,      0, yyl115,
 yyl106,      0,      0, yyl224,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0,      0,  yyl95,
      0,      0,      0, yyl282, yyl282, yyl284, yyl285,      0,
      0, yyl134,      0, yyl134,      0,      0,      0,      0,
      0,      0,      0,      0,      0,      0, yyl115,      0,
 yyl224, yyl286,      0,      0,      0,      0,      0,      0,
 yyl287,      0,      0, yyl134,      0,      0,      0, yyl134,
      0,      0,      0,      0,      0, yyl177,      0, yyl282,
 yyl282,      0,      0, yyl177,      0, yyl282, yyl282,      0,
      0, yyl288,      0, yyl177,      0,      0,      0,  yyl45,
      0, yyl134, yyl289, yyl134, yyl134,      0,      0,      0,
      0, yyl290, yyl290,      0,      0, yyl290, yyl282,      0,
 yyl212,      0,      0,      0,      0,      0,      0,  yyl45,
      0, yyl177, yyl177, yyl177,      0,      0,      0,      0,
      0,
};
#endif

static	int	yyParse			ARGS ((yyStateRange yyStartSymbol,
				yySymbolRange yyToken, int yyLine));

#ifndef NO_RECOVER
static	yyStateRange yyNext		ARGS ((yyStateRange yyState,
				yySymbolRange yySymbol));
static	void	yyErrorRecovery		ARGS ((yySymbolRange * yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr));
static	void	yyComputeContinuation	ARGS ((yyStateRange * yyStack,
				short yyStackPtr, tSet * yyContinueSet));
static	rbool	yyIsContinuation	ARGS ((yySymbolRange yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr));
static	void	yyComputeRestartPoints	ARGS ((yyStateRange * yyStateStack,
				short yyStackPtr, tSet * yyRestartSet));
#endif

#if defined YYTrialParse | defined YYReParse | defined YYGetLook

#ifndef yyInitBufferSize
#define yyInitBufferSize 100
#endif
#ifndef TOKENOP
#define TOKENOP
#endif
#ifndef BEFORE_TRIAL
#define BEFORE_TRIAL
#endif
#ifndef AFTER_TRIAL
#define AFTER_TRIAL
#endif

typedef struct { yySymbolRange	yyToken;
		 tScanAttribute	yyAttribute;
#ifdef YYMemoParse
		 short		yyStart;
#endif
	       } yytBuffer;

static yytBuffer *	yyBuffer	;
static unsigned long	yyBufferSize	= yyInitBufferSize;
static long		yyBufferNext	= 1;
static long		yyBufferLast	= 1;
static rbool		yyBufferClear	= rtrue;
static unsigned short	yyParseLevel	= 0;

static void yyBufferSet
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyToken)
#else
   (yyToken) yySymbolRange yyToken;
#endif
{
   if (yyBufferNext == yyBufferLast) {
      if (yyBufferClear) yyBufferLast = 0;
      if (++ yyBufferLast >= (long) yyBufferSize) {
	 ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			  (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (0);
	    (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
		yyBufferSize / 2, yyBufferSize); yyNl ();
	 }
#endif
      }
      yyBuffer [yyBufferLast].yyToken	= yyToken;
      yyBuffer [yyBufferLast].yyAttribute= Attribute;
#ifdef YYMemoParse
      yyBuffer [yyBufferLast].yyStart	= 0;
#endif
      yyBufferNext = yyBufferLast;
   }
}

static int yyGetToken ARGS ((void))
{
   register yySymbolRange yyToken;

   if (yyBufferNext < yyBufferLast) {
      yyToken = yyBuffer [++ yyBufferNext].yyToken;
      Attribute = yyBuffer [yyBufferNext].yyAttribute;
   } else {
      yyToken = GetToken ();
      if ((yytrial | yybuffer) & yyControl.yyMode) {
	 if (++ yyBufferLast >= (long) yyBufferSize) {
	    ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			     (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (0);
	       (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
		  yyBufferSize / 2, yyBufferSize); yyNl ();
	    }
#endif
	 }
	 yyBuffer [yyBufferLast].yyToken = yyToken;
	 yyBuffer [yyBufferLast].yyAttribute = Attribute;
#ifdef YYMemoParse
	 yyBuffer [yyBufferLast].yyStart = 0;
#endif
	 yyBufferNext = yyBufferLast;
      }
   }
   TOKENOP
   return yyToken;
}

#else
#define yyGetToken GetToken
#endif

#ifdef YYGetLook

static int yyGetLookahead
#if defined __STDC__ | defined __cplusplus
   (int yyk, yySymbolRange yyToken)
#else
   (yyk, yyToken) int yyk; yySymbolRange yyToken;
#endif
{
   if (yyk == 0) return yyToken;
   if (yyControl.yyMode == yystandard) yyBufferSet (yyToken);
   while (yyBufferNext + yyk > yyBufferLast) {
      if (yyBuffer [yyBufferLast].yyToken == EofToken) return EofToken;
      if (++ yyBufferLast >= (long) yyBufferSize) {
	 ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			  (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (0);
	    (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
	       yyBufferSize / 2, yyBufferSize); yyNl ();
	 }
#endif
      }
      yyBuffer [yyBufferLast].yyToken = GetToken ();
      yyBuffer [yyBufferLast].yyAttribute = Attribute;
#ifdef YYMemoParse
      yyBuffer [yyBufferLast].yyStart = 0;
#endif
   }
   Attribute = yyBuffer [yyBufferNext].yyAttribute;
   return yyBuffer [yyBufferNext + yyk].yyToken;
}

static void xxGetAttribute
#if defined __STDC__ | defined __cplusplus
   (int yyk, yySymbolRange yyToken, tScanAttribute * yyAttribute)
#else
   (yyk, yyToken, yyAttribute)
   int yyk; yySymbolRange yyToken; tScanAttribute * yyAttribute;
#endif
{
   if (yyk == 0) * yyAttribute = Attribute;
   else {
      (void) yyGetLookahead (yyk, yyToken);
      * yyAttribute =
	 yyBuffer [Min (yyBufferNext + yyk, yyBufferLast)].yyAttribute;
   }
}

#endif

#ifdef YYReParse

#define BufferOn(Actions, Messages) yyBufferOn (Actions, Messages, yyTerminal)
#define BufferPosition	yyBufferNext

static yytControl yyPrevControl;

static long yyBufferOn
#if defined __STDC__ | defined __cplusplus
   (rbool yyActions, rbool yyMessages, yySymbolRange yyToken)
#else
   (yyActions, yyMessages, yyToken)
   rbool yyActions, yyMessages; yySymbolRange yyToken;
#endif
{
   if (yyControl.yyMode == yystandard) {
      yyPrevControl		= yyControl;
      yyControl.yyMode		= yybuffer;
      yyControl.yyActions	= yyActions;
      yyControl.yyMessages	= yyMessages;
      yyBufferSet (yyToken);
      yyBufferClear		= rfalse;
   }
   return yyBufferNext;
}

static long BufferOff ARGS ((void))
{
   if (yyControl.yyMode == yybuffer) yyControl = yyPrevControl;
   return yyBufferNext;
}

static void BufferClear ARGS ((void))
{
   yyBufferClear = rtrue;
}

#endif

#ifdef YYDEBUG

static void yyNl ARGS ((void))
{ (void) putc ('\n', yyTrace); (void) fflush (yyTrace); }

static void yyPrintState
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState)
#else
   (yyState) yyStateRange yyState;
#endif
{
   (void) fprintf (yyTrace, "%4ld:", ++ yyCount);
   WritePosition  (yyTrace, Attribute.Position);
   (void) fprintf (yyTrace, ":%5d  %c  ", yyState,
      " ST-B---R" [yyControl.yyMode]);
#if defined YYTrialParse | defined YYReParse
   if (yyParseLevel > 0) {
      register int yyi = yyParseLevel;
      (void) fprintf (yyTrace, "%2d  ", yyi);
      do (void) fputs ("  ", yyTrace); while (-- yyi);
   } else
#endif
   (void) fputs ("    ", yyTrace);
}

static rbool yyPrintResult
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState, int yyLine, rbool yyCondition)
#else
   (yyState, yyLine, yyCondition)
   yyStateRange	yyState;
   int		yyLine;
   rbool	yyCondition;
#endif
{
   if (Parser_Debug) {
      yyPrintState (yyState);
      (void) fprintf (yyTrace, "check   predicate in line %d, result = %d",
	 yyLine, yyCondition); yyNl ();
   }
   return yyCondition;
}

#else
#define yyPrintResult(State, Line, Condition) Condition
#endif

#if defined YYDEBUG | defined YYDEC_TABLE
#define yyGotoReduce(State, Rule)	{ yyState = State; goto yyReduce; }
#define yyGotoRead(State)		{ yyState = State; goto yyRead; }
#else
#define yyGotoReduce(State, Rule)	goto Rule;
#define yyGotoRead(State)		{ yyState = State; goto yyRead; }
#endif

static unsigned long	yyStateStackSize	= yyInitStackSize;
static yyStateRange *	yyStateStack		;
static yyStateRange *	yyEndOfStack		;
static unsigned long	yyAttrStackSize 	= yyInitStackSize;
static tParsAttribute * yyAttributeStack	;
#if defined YYTrialParse | defined YYReParse
static yyStateRange *	yyStateStackPtr 	;
static tParsAttribute * yyAttrStackPtr		;
#endif
static yyStateRange *	yyIsContStackPtr	;
static unsigned long	yyIsContStackSize	= yyInitStackSize;
static yyStateRange *	yyCompResStackPtr	;
static unsigned long	yyCompResStackSize	= yyInitStackSize;

int Parser ARGS ((void))
   {
      return Parser2 (yyStartState);
   }

int Parser2
#if defined __STDC__ | defined __cplusplus
   (int yyStartSymbol)
#else
   (yyStartSymbol) int yyStartSymbol;
#endif
   {
      int		yyErrorCount;
#if defined YYDEBUG | defined YYDCRP
      yyTrace		= stdout;
#endif
      BeginParser ();
      MakeArray ((char * *) & yyStateStack, & yyStateStackSize,
		     (unsigned long) sizeof (yyStateRange));
      MakeArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
		     (unsigned long) sizeof (tParsAttribute));
      MakeArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
		     (unsigned long) sizeof (yyStateRange));
      MakeArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
		     (unsigned long) sizeof (yyStateRange));
#if defined YYTrialParse | defined YYReParse | defined YYGetLook
      MakeArray ((char * *) & yyBuffer, & yyBufferSize,
		     (unsigned long) sizeof (yytBuffer));
#endif
      yyEndOfStack	= & yyStateStack [yyStateStackSize - 1];
#if defined YYTrialParse | defined YYReParse
      yyStateStackPtr	= yyStateStack;
      yyAttrStackPtr	= yyAttributeStack;
      yyBufferNext	= 1;
      yyBufferLast	= 1;
      yyParseLevel	= 0;
#endif
#ifdef YYDEBUG
      if (Parser_Debug) {
	 (void) fprintf (yyTrace,
      "  #|Position|State|Mod|Lev|Action |Terminal and Lookahead or Rule\n");
	 yyNl ();
      }
#endif
      yyControl.yyMode		= yystandard;
      yyControl.yyActions	= rtrue;
      yyControl.yyMessages	= rtrue;
      yyErrorCount = yyParse ((yyStateRange) yyStartSymbol,
	 (yySymbolRange) yyGetToken (), (int) yyStartLine [yyStartSymbol]);
      ReleaseArray ((char * *) & yyStateStack, & yyStateStackSize,
			(unsigned long) sizeof (yyStateRange));
      ReleaseArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
			(unsigned long) sizeof (tParsAttribute));
      ReleaseArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
			(unsigned long) sizeof (yyStateRange));
      ReleaseArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			(unsigned long) sizeof (yyStateRange));
#if defined YYTrialParse | defined YYReParse | defined YYGetLook
      ReleaseArray ((char * *) & yyBuffer, & yyBufferSize,
			(unsigned long) sizeof (yytBuffer));
#endif
      return yyErrorCount;
   }

#ifdef YYTrialParse

#ifdef YYMemoParse
#define MemoryClear(Position) yyBuffer [Position].yyStart = 0
#endif

static int yyTrialParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, yySymbolRange yyToken, int yyLine)
#else
   (yyStartSymbol, yyToken, yyLine)
   yyStateRange		yyStartSymbol	;
   yySymbolRange	yyToken		;
   int			yyLine		;
#endif
   {
      int	yyErrorCount		;
      unsigned long yyPrevStateStackPtr	= yyStateStackPtr - yyStateStack;
      unsigned long yyPrevAttrStackPtr	= yyAttrStackPtr - yyAttributeStack;
      long	yyPrevBufferNext	;
      yytControl yyPrevControl		;

      BEFORE_TRIAL
#ifdef YYMemoParse
      if (yyBuffer [yyBufferNext].yyStart ==   yyStartSymbol) return 0;
      if (yyBuffer [yyBufferNext].yyStart == - yyStartSymbol) return 1;
#endif
      yyPrevControl		= yyControl;
      yyStateStackPtr		++;
      yyAttrStackPtr		++;
      yyParseLevel		++;
      if (yyControl.yyMode == yystandard) yyBufferSet (yyToken);
      yyPrevBufferNext		= yyBufferNext;
      yyControl.yyMode		= yytrial;
      yyControl.yyActions	= rfalse;
      yyControl.yyMessages	= rfalse;
      yyErrorCount		= yyParse (yyStartSymbol, yyToken, yyLine);
#ifdef YYMemoParse
      yyBuffer [yyPrevBufferNext].yyStart = yyErrorCount ?
					- yyStartSymbol : yyStartSymbol;
#endif
      yyStateStackPtr		= yyStateStack + yyPrevStateStackPtr;
      yyAttrStackPtr		= yyAttributeStack + yyPrevAttrStackPtr;
      yyBufferNext		= yyPrevBufferNext;
      yyControl			= yyPrevControl;
      yyParseLevel		--;
      Attribute		= yyBuffer [yyBufferNext].yyAttribute;
      AFTER_TRIAL
      return yyErrorCount;
   }

#endif

#ifdef YYReParse

static int ReParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, int yyFrom, int yyTo, rbool yyActions,
      rbool yyMessages)
#else
   (yyStartSymbol, yyFrom, yyTo, yyActions, yyMessages)
   yyStateRange	yyStartSymbol		;
   int		yyFrom, yyTo		;
   rbool	yyActions, yyMessages	;
#endif
   {
      int yyErrorCount = 1;

      if (1 <= yyFrom && yyFrom <= yyTo && yyTo <= yyBufferLast) {
	 unsigned long yyPrevStateStackPtr = yyStateStackPtr - yyStateStack;
	 unsigned long yyPrevAttrStackPtr = yyAttrStackPtr - yyAttributeStack;
	 long	yyPrevBufferNext	= yyBufferNext;
	 int	yyToToken		= yyBuffer [yyTo].yyToken;
	 yytControl yyPrevControl	;

	 yyPrevControl		= yyControl;
	 yyStateStackPtr	++;
	 yyAttrStackPtr		++;
	 yyParseLevel		++;
	 yyBufferNext		= yyFrom - 1;
	 yyBuffer [yyTo].yyToken= EofToken;
	 yyControl.yyMode	= yyreparse;
	 yyControl.yyActions	= yyActions;
	 yyControl.yyMessages	= yyMessages;
	 yyErrorCount		= yyParse (yyStartSymbol,
	    (yySymbolRange) yyGetToken (), (int) yyStartLine [yyStartSymbol]);
	 yyStateStackPtr	= yyStateStack + yyPrevStateStackPtr;
	 yyAttrStackPtr		= yyAttributeStack + yyPrevAttrStackPtr;
	 yyBufferNext		= yyPrevBufferNext;
	 yyControl		= yyPrevControl;
	 yyParseLevel		--;
	 yyBuffer [yyTo].yyToken= yyToToken;
	 Attribute		= yyBuffer [yyBufferNext].yyAttribute;
      } else {
      Message ("invalid call of ReParse", xxError, Attribute.Position);
      }
      return yyErrorCount;
   }

#endif

static int yyParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, yySymbolRange yyToken, int yyLine)
#else
   (yyStartSymbol, yyToken, yyLine)
   yyStateRange		yyStartSymbol	;
   yySymbolRange	yyToken		;
   int			yyLine		;
#endif
   {
      register	yyStateRange	yyState		= yyStartSymbol;
      register	yySymbolRange	yyTerminal	= yyToken;
      register	rbool		yyIsRepairing	= rfalse;
		tParsAttribute	yySynAttribute	;   /* synthesized attribute */
		int		yyErrorCount	= 0;
#if ! (defined YYTrialParse | defined YYReParse)
      register	yyStateRange *	yyStateStackPtr	= yyStateStack;
      register	tParsAttribute *yyAttrStackPtr	= yyAttributeStack;
#endif
#ifdef YYDEBUG
		long		yyStartCount	= yyCount + 1;
		yySymbolRange	yyPrevTerminal	= yyToken;
#endif
#ifdef YYGetLook
		yySymbolRange	yy2;
#endif

/* line 50 "Parser.lrk" */



#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (yyStartSymbol);
	 (void) fprintf (yyTrace,
	    "parse   for predicate in line %d, lookahead: %s", yyLine,
	    Parser_TokenName [yyTerminal]); yyNl ();
      }
#endif

   yyParseLoop:
      for (;;) {
	 if (yyStateStackPtr >= yyEndOfStack) {
	    unsigned long yyyStateStackPtr = yyStateStackPtr - yyStateStack;
	    unsigned long yyyAttrStackPtr = yyAttrStackPtr - yyAttributeStack;
	    ExtendArray ((char * *) & yyStateStack, & yyStateStackSize,
			     (unsigned long) sizeof (yyStateRange));
	    ExtendArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
			     (unsigned long) sizeof (tParsAttribute));
	    yyStateStackPtr	= yyStateStack + yyyStateStackPtr;
	    yyAttrStackPtr	= yyAttributeStack + yyyAttrStackPtr;
	    yyEndOfStack	= & yyStateStack [yyStateStackSize - 1];
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (yyState);
	       (void) fprintf (yyTrace, "extend  stack from %ld to %ld",
		  yyStateStackSize / 2, yyStateStackSize); yyNl ();
	    }
#endif
	 }
	 * yyStateStackPtr = yyState;

   yyTermTrans:
	 for (;;) { /* SPEC State = Next (State, Terminal); terminal transit */
	    register yytComb * yyTCombPtr = yyTBasePtr [yyState] + yyTerminal;
#if defined YYTDefault & defined YYaccDefault
	    register unsigned long * yylp;
#endif
	    if (yyTCombPtr->Check == yyState) {
	       yyState = yyTCombPtr->Next; break;
	    }
#ifdef YYTDefault
#ifdef YYaccDefault
	    if ((yylp = yyDefaultLook [yyState]) &&
	       (yylp [yyTerminal >> 5] >> (yyTerminal & 0x1f)) & 1) {
	       yyState = yyTDefault [yyState]; break;
	    }
#else
	    if ((yyState = yyTDefault [yyState]) != yyNoState) goto yyTermTrans;
#endif
#endif

							/* syntax error */
	    if (! yyIsRepairing) {			/* report and recover */
	       yySymbolRange yyyTerminal = (yySymbolRange) yyTerminal;

#ifdef YYTrialParse
	       if (yyControl.yyMode == yytrial) YYABORT;
#endif
	       ERROR;
#ifndef NO_RECOVER
	       yyErrorCount ++;
	       yyErrorRecovery (& yyyTerminal, yyStateStack,
				yyStateStackPtr - yyStateStack);
	       yyTerminal = yyyTerminal;
	       yyIsRepairing = rtrue;
#else
	       YYABORT;
#endif
	    }
#ifndef NO_RECOVER
	    yyState = * yyStateStackPtr;
	    for (;;) {
	       if (yyNext (yyState, (yySymbolRange) yyTerminal) == yyNoState) {
		  yySymbolRange		yyRepairToken;		/* repair */
		  tScanAttribute	yyRepairAttribute;

		  yyRepairToken = yyContinuation [yyState];
		  yyState = yyNext (yyState, yyRepairToken);
		  if (yyState > yyLastReduceState) {		/* dynamic ? */
		     yyState = yyCondition [yyState - yyLastReduceState];
		  }
		  if (yyState <= yyLastReadReduceState) {
						/* read or read reduce ? */
		     ErrorAttribute ((int) yyRepairToken,
					& yyRepairAttribute);
		     if (yyControl.yyMessages)
		        ErrorMessageI (xxTokenInserted, xxRepair,
				Attribute.Position, xxString,
				Parser_TokenName [yyRepairToken]);
#ifdef YYDEBUG
		     if (Parser_Debug) {
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "insert  %s",
				Parser_TokenName [yyRepairToken]); yyNl ();
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "shift   %s, lookahead: %s",
			   Parser_TokenName [yyRepairToken],
			   Parser_TokenName [yyTerminal]); yyNl ();
		     }
#endif
		     if (yyState >= yyFirstFinalState) { /* avoid second push */
			yyState =
			   yyFinalToProd [yyState - yyFirstReadReduceState];
		     }
		     yyGetAttribute (yyAttrStackPtr ++, yyRepairAttribute);
		     * ++ yyStateStackPtr = yyState;
		  }
		  if (yyState >= yyFirstFinalState) goto yyFinal;
							/* final state ? */
	       } else {
		  yyState = yyNext (yyState, (yySymbolRange) yyTerminal);
		  goto yyFinal;
	       }
	    }
#endif
	 }

   yyFinal:
	 if (yyState >= yyFirstFinalState) {		/* final state ? */
	    if (yyState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStateStackPtr ++;
	       yyGetAttribute (yyAttrStackPtr ++, Attribute);
	       yyTerminal = yyGetToken ();
#ifdef YYDEBUG
	       if (Parser_Debug) {
		  yyStateStackPtr [0] = yyStateStackPtr [-1];
		  yyPrintState (* yyStateStackPtr);
		  (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		     Parser_TokenName [yyPrevTerminal],
		     Parser_TokenName [yyTerminal]); yyNl ();
		  yyPrevTerminal = yyTerminal;
	       }
#endif
	       yyIsRepairing = rfalse;
	       yyState = yyFinalToProd [yyState - yyFirstReadReduceState];
	    }

	    for (;;) {
	       register yytNonterminal yyNonterminal;	/* left-hand side */

   yyReduce:
#ifdef YYDEBUG
	       if (Parser_Debug) {
		  if (yyState <= yyLastReadReduceState)	/* read reduce ? */
		     yyState = yyFinalToProd [yyState - yyFirstReadReduceState];
		  yyPrintState (* yyStateStackPtr);
		  if (yyState <= yyLastReduceState) {
		     (void) fprintf (yyTrace, "reduce  %s",
			yyRule [yyState - yyLastReadReduceState]); yyNl ();
		  } else {
		     (void) fprintf (yyTrace, "dynamic decision %d",
			yyState - yyLastReduceState); yyNl ();
		  }
	       }
#endif
#ifdef YYDEC_TABLE
	       if (yyLastStopState < yyState && yyState <= yyLastReduceState) {
		  register int yyd = yyLength [yyState - yyFirstReduceState];
		  yyStateStackPtr -= yyd;
		  yyAttrStackPtr  -= yyd;
		  yyNonterminal = yyLeftHandSide [yyState - yyFirstReduceState];
	       }
#endif
switch (yyState) {
case 2888:
YYACCEPT;
case 2889:
YYACCEPT;
case 2890:
YYACCEPT;
case 2891:
YYACCEPT;
case 2892:
YYACCEPT;
case 2893:
YYACCEPT;
case 2894:
YYACCEPT;
case 2895:
YYACCEPT;
case 2896:
YYACCEPT;
case 2897:
YYACCEPT;
case 2898:
YYACCEPT;
case 2899:
YYACCEPT;
case 2900:
YYACCEPT;
case 2901:
YYACCEPT;
case 2902:
YYACCEPT;
case 2903:
YYACCEPT;
case 2904:
YYACCEPT;
case 2905:
YYACCEPT;
case 2906:
YYACCEPT;
case 2907:
YYACCEPT;
case 2908:
YYACCEPT;
case 2909:
YYACCEPT;
case 2910:
YYACCEPT;
case 2911:
YYACCEPT;
case 2912:
YYACCEPT;
case 2913:
YYACCEPT;
case 2914:
YYACCEPT;
case 2915:
YYACCEPT;
case 2916:
YYACCEPT;
case 2917:
YYACCEPT;
case 2918:
YYACCEPT;
case 2919:
YYACCEPT;
case 2920:
YYACCEPT;
case 2921:
YYACCEPT;
case 2922:
YYACCEPT;
case 2923:
YYACCEPT;
case 2924:
YYACCEPT;
case 2925:
YYACCEPT;
case 2926:
YYACCEPT;
case 2927:
YYACCEPT;
case 2928:
YYACCEPT;
case 2929:
YYACCEPT;
case 2930:
YYACCEPT;
case 2931:
YYACCEPT;
case 2932:
YYACCEPT;
case 2933: yyDecrement (2) yySetNT (yyNTprograms) {
} break;
case 2934: yyDecrement (1) yySetNT (yyNTprogram_end_o) {
} break;
case 2935: yyDecrement (6) yySetNT (yyNTprogram_end_o) {
} break;
case 2936: yyDecrement (6) yySetNT (yyNTprogram_end) {
} break;
case 2937: yySetNT (yyNTprogram_l) {
} break;
case 2938: yyDecrement (2) yySetNT (yyNTprogram_l) {
} break;
case 2939: yyDecrement (4) yySetNT (yyNTprogram) {
} break;
case 2940: yySetNT (yyNTsce) {
/* line 543 "Parser.lrk" */
 ;
{  Start_Comment_Entry (); ; } ;

} break;
case 2941: yyDecrement (3) yySetNT (yyNTidentification_division) {
} break;
case 2942: yyDecrement (1) yySetNT (yyNTenvironment_division_o) {
} break;
case 2943: yyDecrement (1) yySetNT (yyNTdata_division_o) {
} break;
case 2944: yyDecrement (1) yySetNT (yyNTprocedure_division_o) {
} break;
case 2945: yySetNT (yyNTprocedure_division_o) {
} break;
case 2946: yyDecrement (3) yySetNT (yyNTidentification_o) {
/* line 551 "Parser.lrk" */
 ;
{  Section = cID_DIV; ; } ;

} break;
case 2947: yySetNT (yyNTidentification_o) {
/* line 554 "Parser.lrk" */
 ;
{  Section = cID_DIV; ; } ;

} break;
case 2948: yyDecrement (5) yySetNT (yyNTprogram_id_o) {
/* line 557 "Parser.lrk" */
 ;
{  (void) DeclareLabel (yyA [3].Scan, lPROGRAM, PrevEPos); ; } ;

} break;
case 2949: yyDecrement (5) yySetNT (yyNTprogram_id_o) {
/* line 560 "Parser.lrk" */
 ;
{ 	char word [128];
			StGetString (yyA [3].Scan.string.Value, word);
			yyA [3].Scan.name.Ident = MakeIdent (word, strlen (word));
			(void) DeclareLabel (yyA [3].Scan, lPROGRAM, PrevEPos); ; } ;

} break;
case 2950: yySetNT (yyNTprogram_id_o) {
} break;
case 2951: yyDecrement (3) yySetNT (yyNTprogram_o) {
} break;
case 2952: yyDecrement (3) yySetNT (yyNTprogram_o) {
} break;
case 2953: yyDecrement (4) yySetNT (yyNTprogram_o) {
} break;
case 2954: yyDecrement (4) yySetNT (yyNTprogram_o) {
} break;
case 2955: yySetNT (yyNTprogram_o) {
} break;
case 2956: yySetNT (yyNTidentification_l) {
} break;
case 2957: yyDecrement (3) yySetNT (yyNTidentification_l) {
} break;
case 2958: yyDecrement (3) yySetNT (yyNTidentification_l) {
} break;
case 2959: yyDecrement (3) yySetNT (yyNTidentification_l) {
} break;
case 2960: yyDecrement (3) yySetNT (yyNTidentification_l) {
} break;
case 2961: yyDecrement (3) yySetNT (yyNTidentification_l) {
} break;
case 2962: yyDecrement (3) yySetNT (yyNTidentification_l) {
} break;
case 2963: yyDecrement (2) yySetNT (yyNTidentification_l) {
} break;
case 2964: yyDecrement (3) yySetNT (yyNTenvironment_division) {
} break;
case 2965: yyDecrement (3) yySetNT (yyNTenvironment_o) {
/* line 581 "Parser.lrk" */
 ;
{  Section = cENV_DIV; ; } ;

} break;
case 2966: yySetNT (yyNTenvironment_o) {
/* line 584 "Parser.lrk" */
 ;
{  Section = cENV_DIV; ; } ;

} break;
case 2967: yyDecrement (3) yySetNT (yyNTconfiguration_section_o) {
} break;
case 2968: yyDecrement (3) yySetNT (yyNTinput_output_section_o) {
} break;
case 2969: yyDecrement (3) yySetNT (yyNTconfiguration_o) {
/* line 589 "Parser.lrk" */
 ;
{  decimal_point_is_comma = rfalse; Section = cCONF_SCT; ; } ;

} break;
case 2970: yySetNT (yyNTconfiguration_o) {
/* line 592 "Parser.lrk" */
 ;
{  decimal_point_is_comma = rfalse; Section = cCONF_SCT; ; } ;

} break;
case 2971: yySetNT (yyNTconfiguration_l) {
} break;
case 2972: yyDecrement (2) yySetNT (yyNTconfiguration_l) {
} break;
case 2973: yyDecrement (2) yySetNT (yyNTconfiguration_l) {
} break;
case 2974: yyDecrement (2) yySetNT (yyNTconfiguration_l) {
} break;
case 2975: yyDecrement (3) yySetNT (yyNTsource_computer_o) {
} break;
case 2976: yyDecrement (7) yySetNT (yyNTsource_computer_o) {
/* line 600 "Parser.lrk" */
 ;
{  IsDebugging = rtrue; ; } ;

} break;
case 2977: yyDecrement (3) yySetNT (yyNTobject_computer_o) {
} break;
case 2978: yyDecrement (7) yySetNT (yyNTobject_computer_o) {
} break;
case 2979: yyDecrement (4) yySetNT (yyNTmemory_o) {
} break;
case 2980: yySetNT (yyNTmemory_o) {
} break;
case 2981: yyDecrement (1) yySetNT (yyNTmemory_unit) {
} break;
case 2982: yyDecrement (1) yySetNT (yyNTmemory_unit) {
} break;
case 2983: yyDecrement (1) yySetNT (yyNTmemory_unit) {
} break;
case 2984: yyDecrement (5) yySetNT (yyNTprogram_sequence_o) {
} break;
case 2985: yySetNT (yyNTprogram_sequence_o) {
} break;
case 2986: yyDecrement (3) yySetNT (yyNTsegment_limit_o) {
} break;
case 2987: yySetNT (yyNTsegment_limit_o) {
} break;
case 2988: yyDecrement (3) yySetNT (yyNTspecial_names_o) {
} break;
case 2989: yyDecrement (5) yySetNT (yyNTspecial_names_o) {
} break;
case 2990: yySetNT (yyNTspecial_names_l) {
} break;
case 2991:
yy104: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2992: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2993: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2994: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2995: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2996: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2997: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2998: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 2999: yyDecrement (2) yySetNT (yyNTspecial_names_l) {
} break;
case 3000: yyDecrement (1) yySetNT (yyNTimplementor_name_l) {
} break;
case 3001: yyDecrement (2) yySetNT (yyNTimplementor_name_l) {
} break;
case 3002: yyDecrement (3) yySetNT (yyNTimplementor_name_e) {
/* line 628 "Parser.lrk" */
 ;
{  (void) Declare (MN, yyA [2].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3003: yyDecrement (4) yySetNT (yyNTimplementor_name_e) {
/* line 631 "Parser.lrk" */
 ;
{  (void) Declare (MN, yyA [2].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3004: yyDecrement (2) yySetNT (yyNTimplementor_name_e) {
/* line 634 "Parser.lrk" */
 ;
{  (void) Declare (MN, yyA [0].Scan, oBTIN, PrevEPos); ; } ;

} break;
case 3005: yyDecrement (1) yySetNT (yyNTimplementor_name) {
} break;
case 3006: yyDecrement (1) yySetNT (yyNTimplementor_name) {
} break;
case 3007: yyDecrement (1) yySetNT (yyNTbuiltin_name) {
} break;
case 3008: yyDecrement (1) yySetNT (yyNTbuiltin_name) {
} break;
case 3009:
yy122: yyDecrement (1) yySetNT (yyNTbuiltin_name) {
} break;
case 3010: yyDecrement (1) yySetNT (yyNTon_off) {
} break;
case 3011: yyDecrement (2) yySetNT (yyNTon_off) {
} break;
case 3012: yyDecrement (1) yySetNT (yyNTon_off) {
} break;
case 3013: yyDecrement (2) yySetNT (yyNTon_off) {
} break;
case 3014: yyDecrement (4) yySetNT (yyNTon_status) {
/* line 646 "Parser.lrk" */
 ;
{  (void) Declare (CN, yyA [3].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3015: yyDecrement (4) yySetNT (yyNToff_status) {
/* line 649 "Parser.lrk" */
 ;
{  (void) Declare (CN, yyA [3].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3016: yyDecrement (1) yySetNT (yyNTalphabet_names) {
} break;
case 3017: yyDecrement (2) yySetNT (yyNTalphabet_names) {
} break;
case 3018: yyDecrement (4) yySetNT (yyNTalphabet_entry) {
} break;
case 3019:
yy132: yyDecrement (4) yySetNT (yyNTalphabet_entry) {
} break;
case 3020: yyDecrement (1) yySetNT (yyNTalphabet_name) {
} break;
case 3021: yyDecrement (1) yySetNT (yyNTalphabet_name) {
} break;
case 3022: yyDecrement (1) yySetNT (yyNTalphabet_l) {
} break;
case 3023: yyDecrement (2) yySetNT (yyNTalphabet_l) {
} break;
case 3024: yyDecrement (1) yySetNT (yyNTalphabet_e) {
} break;
case 3025: yyDecrement (3) yySetNT (yyNTalphabet_e) {
} break;
case 3026: yyDecrement (2) yySetNT (yyNTalphabet_e) {
} break;
case 3027: yyDecrement (2) yySetNT (yyNTalso_l) {
} break;
case 3028: yyDecrement (3) yySetNT (yyNTalso_l) {
} break;
case 3029: yyDecrement (1) yySetNT (yyNTalphabet_or_class_literal) {
} break;
case 3030: yyDecrement (1) yySetNT (yyNTalphabet_or_class_literal) {
} break;
case 3031: yyDecrement (1) yySetNT (yyNTsymbolic_characters) {
} break;
case 3032: yyDecrement (2) yySetNT (yyNTsymbolic_characters) {
} break;
case 3033:
yy146: yyDecrement (3) yySetNT (yyNTsymbolic_entry) {
} break;
case 3034: yyDecrement (5) yySetNT (yyNTsymbolic_entry) {
} break;
case 3035: yyDecrement (1) yySetNT (yyNTsymbolic_l) {
} break;
case 3036: yyDecrement (2) yySetNT (yyNTsymbolic_l) {
} break;
case 3037: yyDecrement (3) yySetNT (yyNTsymbolic_e) {
} break;
case 3038: yyDecrement (1) yySetNT (yyNTsymbolic_character_l) {
} break;
case 3039: yyDecrement (2) yySetNT (yyNTsymbolic_character_l) {
} break;
case 3040: yyDecrement (1) yySetNT (yyNTinteger_l) {
} break;
case 3041: yyDecrement (2) yySetNT (yyNTinteger_l) {
} break;
case 3042: yyDecrement (1) yySetNT (yyNTclass_names) {
} break;
case 3043: yyDecrement (2) yySetNT (yyNTclass_names) {
} break;
case 3044:
yy157: yyDecrement (4) yySetNT (yyNTclass) {
} break;
case 3045: yyDecrement (1) yySetNT (yyNTclass_l) {
} break;
case 3046: yyDecrement (2) yySetNT (yyNTclass_l) {
} break;
case 3047: yyDecrement (1) yySetNT (yyNTclass_e) {
} break;
case 3048: yyDecrement (3) yySetNT (yyNTclass_e) {
} break;
case 3049: yyDecrement (4) yySetNT (yyNTcurrency_o) {
} break;
case 3050: yyDecrement (3) yySetNT (yyNTdecimal_point_o) {
/* line 686 "Parser.lrk" */
 ;
{  decimal_point_is_comma = rtrue; ; } ;

} break;
case 3051: yyDecrement (5) yySetNT (yyNTnumeric_sign_o) {
} break;
case 3052: yyDecrement (4) yySetNT (yyNTcall_convention_o) {
} break;
case 3053: yyDecrement (5) yySetNT (yyNTconsole_o) {
} break;
case 3054: yyDecrement (3) yySetNT (yyNTcursor_o) {
} break;
case 3055: yySetNT (yyNTcursor_o) {
} break;
case 3056: yyDecrement (4) yySetNT (yyNTcrt_status_o) {
} break;
case 3057: yySetNT (yyNTcrt_status_o) {
} break;
case 3058: yyDecrement (3) yySetNT (yyNTinput_output_o) {
/* line 696 "Parser.lrk" */
 ;
{  Section = cIO_SCT; ; } ;

} break;
case 3059: yySetNT (yyNTinput_output_o) {
/* line 699 "Parser.lrk" */
 ;
{  Section = cIO_SCT; ; } ;

} break;
case 3060: yyDecrement (2) yySetNT (yyNTfile_control) {
} break;
case 3061: yyDecrement (2) yySetNT (yyNTfile_control_o) {
} break;
case 3062: yySetNT (yyNTfile_control_o) {
} break;
case 3063: yySetNT (yyNTfile_control_entry_l) {
} break;
case 3064: yyDecrement (2) yySetNT (yyNTfile_control_entry_l) {
} break;
case 3065: yyDecrement (2) yySetNT (yyNTfile_control_entry_l) {
} break;
case 3066: yyDecrement (3) yySetNT (yyNTfile_control_entry_l) {
} break;
case 3067: yyDecrement (6) yySetNT (yyNTfile_control_entry) {
} break;
case 3068: yyDecrement (7) yySetNT (yyNTfile_control_entry) {
} break;
case 3069: yySetNT (yyNTselect_clause_l) {
} break;
case 3070: yyDecrement (2) yySetNT (yyNTselect_clause_l) {
} break;
case 3071: yyDecrement (7) yySetNT (yyNTselect_clause) {
} break;
case 3072: yyDecrement (7) yySetNT (yyNTselect_clause) {
} break;
case 3073: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3074: yyDecrement (6) yySetNT (yyNTselect_clause) {
} break;
case 3075: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3076: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3077: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3078: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3079: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3080: yyDecrement (6) yySetNT (yyNTselect_clause) {
} break;
case 3081:
yy194: yyDecrement (2) yySetNT (yyNTselect_clause) {
} break;
case 3082: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3083: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3084: yyDecrement (2) yySetNT (yyNTselect_clause) {
} break;
case 3085: yyDecrement (2) yySetNT (yyNTselect_clause) {
} break;
case 3086: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3087: yyDecrement (3) yySetNT (yyNTselect_clause) {
} break;
case 3088: yyDecrement (1) yySetNT (yyNTselect_clause) {
} break;
case 3089: yyDecrement (3) yySetNT (yyNTselect_clause) {
} break;
case 3090: yyDecrement (2) yySetNT (yyNTselect_clause) {
} break;
case 3091: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3092: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3093: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3094: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3095: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3096: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3097: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3098: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3099: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3100: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3101: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3102: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3103: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3104: yyDecrement (6) yySetNT (yyNTselect_clause) {
} break;
case 3105: yyDecrement (8) yySetNT (yyNTselect_clause) {
} break;
case 3106: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3107: yyDecrement (5) yySetNT (yyNTselect_clause) {
} break;
case 3108: yyDecrement (4) yySetNT (yyNTselect_clause) {
} break;
case 3109: yyDecrement (3) yySetNT (yyNTselect_clause) {
} break;
case 3110: yyDecrement (1) yySetNT (yyNTassign_o) {
} break;
case 3111:
yy224: yySetNT (yyNTassign_o) {
} break;
case 3112: yyDecrement (1) yySetNT (yyNTassign_l) {
} break;
case 3113: yyDecrement (2) yySetNT (yyNTassign_l) {
} break;
case 3114: yyDecrement (1) yySetNT (yyNTassign_e) {
} break;
case 3115: yyDecrement (1) yySetNT (yyNTassign_e) {
} break;
case 3116: yyDecrement (1) yySetNT (yyNTexternal_o) {
} break;
case 3117: yyDecrement (1) yySetNT (yyNTexternal_o) {
} break;
case 3118: yySetNT (yyNTexternal_o) {
} break;
case 3119:
yy232: yyDecrement (1) yySetNT (yyNTinteger_or_no) {
} break;
case 3120: yyDecrement (1) yySetNT (yyNTinteger_or_no) {
} break;
case 3121: yyDecrement (2) yySetNT (yyNTorganization_is_o) {
} break;
case 3122: yySetNT (yyNTorganization_is_o) {
} break;
case 3123: yyDecrement (1) yySetNT (yyNTalternate_l) {
} break;
case 3124: yyDecrement (2) yySetNT (yyNTalternate_l) {
} break;
case 3125: yyDecrement (1) yySetNT (yyNTalternate_e) {
} break;
case 3126:
yy239: yyDecrement (3) yySetNT (yyNTalternate_e) {
} break;
case 3127: yyDecrement (2) yySetNT (yyNTduplicates_o_2) {
} break;
case 3128: yySetNT (yyNTduplicates_o_2) {
} break;
case 3129: yyDecrement (3) yySetNT (yyNTpassword_o) {
} break;
case 3130: yySetNT (yyNTpassword_o) {
} break;
case 3131: yyDecrement (3) yySetNT (yyNTsuppress_o) {
} break;
case 3132: yySetNT (yyNTsuppress_o) {
} break;
case 3133: yyDecrement (4) yySetNT (yyNTlock_o) {
} break;
case 3134: yyDecrement (4) yySetNT (yyNTlock_o) {
} break;
case 3135: yyDecrement (5) yySetNT (yyNTlock_o) {
} break;
case 3136: yyDecrement (5) yySetNT (yyNTlock_o) {
} break;
case 3137: yyDecrement (4) yySetNT (yyNTlock_o) {
} break;
case 3138: yySetNT (yyNTlock_o) {
} break;
case 3139: yyDecrement (3) yySetNT (yyNTi_o_control_o) {
} break;
case 3140: yyDecrement (5) yySetNT (yyNTi_o_control_o) {
} break;
case 3141: yySetNT (yyNTi_o_control_o) {
} break;
case 3142: yySetNT (yyNTi_o_control_l) {
} break;
case 3143: yyDecrement (2) yySetNT (yyNTi_o_control_l) {
} break;
case 3144: yyDecrement (3) yySetNT (yyNTi_o_control_e) {
} break;
case 3145: yyDecrement (5) yySetNT (yyNTi_o_control_e) {
} break;
case 3146: yyDecrement (6) yySetNT (yyNTi_o_control_e) {
} break;
case 3147: yyDecrement (5) yySetNT (yyNTi_o_control_e) {
} break;
case 3148: yyDecrement (4) yySetNT (yyNTi_o_control_e) {
} break;
case 3149: yyDecrement (4) yySetNT (yyNTi_o_control_e) {
} break;
case 3150: yyDecrement (4) yySetNT (yyNTi_o_control_e) {
} break;
case 3151: yyDecrement (6) yySetNT (yyNTi_o_control_e) {
} break;
case 3152: yyDecrement (3) yySetNT (yyNTrerun_e) {
} break;
case 3153: yyDecrement (5) yySetNT (yyNTrerun_e) {
} break;
case 3154: yyDecrement (4) yySetNT (yyNTrerun_e) {
} break;
case 3155: yyDecrement (2) yySetNT (yyNTrerun_e) {
} break;
case 3156: yyDecrement (1) yySetNT (yyNTrerun_e) {
} break;
case 3157: yyDecrement (1) yySetNT (yyNTsame) {
} break;
case 3158: yyDecrement (1) yySetNT (yyNTsame) {
} break;
case 3159: yyDecrement (1) yySetNT (yyNTsame) {
} break;
case 3160: yyDecrement (1) yySetNT (yyNTmultiple_l) {
} break;
case 3161: yyDecrement (2) yySetNT (yyNTmultiple_l) {
} break;
case 3162: yyDecrement (1) yySetNT (yyNTmultiple_e) {
} break;
case 3163: yyDecrement (3) yySetNT (yyNTmultiple_e) {
} break;
case 3164: yyDecrement (8) yySetNT (yyNTdata_division) {
} break;
case 3165: yyDecrement (3) yySetNT (yyNTdata_o) {
/* line 807 "Parser.lrk" */
 ;
{  Section = cDATA_DIV; ; } ;

} break;
case 3166: yySetNT (yyNTdata_o) {
/* line 810 "Parser.lrk" */
 ;
{  Section = cDATA_DIV; ; } ;

} break;
case 3167: yyDecrement (2) yySetNT (yyNTfile_section_o) {
} break;
case 3168: yyDecrement (3) yySetNT (yyNTfile_o) {
/* line 814 "Parser.lrk" */
 ;
{  Section = cFILE_SCT; ; } ;

} break;
case 3169: yySetNT (yyNTfile_o) {
/* line 817 "Parser.lrk" */
 ;
{  Section = cFILE_SCT; ; } ;

} break;
case 3170: yySetNT (yyNTfile_description_l) {
} break;
case 3171: yyDecrement (2) yySetNT (yyNTfile_description_l) {
} break;
case 3172: yyDecrement (3) yySetNT (yyNTfile_description_l) {
} break;
case 3173: yyDecrement (2) yySetNT (yyNTfile_description_l) {
} break;
case 3174: yyDecrement (2) yySetNT (yyNTfile_description_e) {
} break;
case 3175: yyDecrement (2) yySetNT (yyNTfile_description_e) {
} break;
case 3176: yyDecrement (5) yySetNT (yyNTworking_storage_section_o) {
} break;
case 3177: yySetNT (yyNTworking_storage_section_o) {
} break;
case 3178: yySetNT (yyNTxx_working_storage_section_o_1_4) {
/* line 827 "Parser.lrk" */
 ;
{  Section = cWS_SCT; ; } ;

} break;
case 3179: yyDecrement (5) yySetNT (yyNTlocal_storage_section_o) {
} break;
case 3180: yySetNT (yyNTlocal_storage_section_o) {
} break;
case 3181: yySetNT (yyNTxx_local_storage_section_o_1_4) {
/* line 832 "Parser.lrk" */
 ;
{  Section = cLS_SCT; ; } ;

} break;
case 3182: yyDecrement (5) yySetNT (yyNTlinkage_section_o) {
} break;
case 3183: yySetNT (yyNTlinkage_section_o) {
} break;
case 3184: yySetNT (yyNTxx_linkage_section_o_1_4) {
/* line 837 "Parser.lrk" */
 ;
{  Section = cLINK_SCT; ; } ;

} break;
case 3185: yyDecrement (5) yySetNT (yyNTcommunication_section_o) {
} break;
case 3186: yySetNT (yyNTcommunication_section_o) {
} break;
case 3187: yySetNT (yyNTxx_communication_section_o_1_4) {
/* line 842 "Parser.lrk" */
 ;
{  Section = cCOMM_SCT; ; } ;

} break;
case 3188: yyDecrement (3) yySetNT (yyNTcommunication_description_l) {
} break;
case 3189: yyDecrement (2) yySetNT (yyNTcommunication_description_l) {
} break;
case 3190: yyDecrement (3) yySetNT (yyNTcommunication_description_l) {
} break;
case 3191: yySetNT (yyNTcommunication_description_l) {
} break;
case 3192: yyDecrement (5) yySetNT (yyNTreport_section_o) {
} break;
case 3193: yySetNT (yyNTreport_section_o) {
} break;
case 3194: yySetNT (yyNTxx_report_section_o_1_4) {
/* line 851 "Parser.lrk" */
 ;
{  Section = cREP_SCT; ; } ;

} break;
case 3195: yyDecrement (5) yySetNT (yyNTscreen_section_o) {
} break;
case 3196: yySetNT (yyNTscreen_section_o) {
} break;
case 3197: yySetNT (yyNTxx_screen_section_o_1_4) {
/* line 856 "Parser.lrk" */
 ;
{  Section = cSCRN_SCT ; } ;

} break;
case 3198: yyDecrement (3) yySetNT (yyNTreport_description_entry_l) {
} break;
case 3199: yyDecrement (2) yySetNT (yyNTreport_description_entry_l) {
} break;
case 3200: yyDecrement (3) yySetNT (yyNTreport_description_entry_l) {
} break;
case 3201: yySetNT (yyNTreport_description_entry_l) {
} break;
case 3202: yyDecrement (2) yySetNT (yyNTreport_group_description_entry_l) {
} break;
case 3203: yySetNT (yyNTreport_group_description_entry_l) {
} break;
case 3204: yyDecrement (5) yySetNT (yyNTfile_description_entry) {
/* line 866 "Parser.lrk" */
 ;
{  (void) Declare (FD, yyA [1].Scan, oFILE, PrevEPos); ; } ;

} break;
case 3205: yyDecrement (5) yySetNT (yyNTsort_merge_file_description_entry) {
/* line 869 "Parser.lrk" */
 ;
{  (void) Declare (SD, yyA [1].Scan, oFILE, PrevEPos); ; } ;

} break;
case 3206: yySetNT (yyNTfile_clause_l) {
} break;
case 3207: yyDecrement (2) yySetNT (yyNTfile_clause_l) {
} break;
case 3208: yyDecrement (2) yySetNT (yyNTfile_clause) {
} break;
case 3209: yyDecrement (2) yySetNT (yyNTfile_clause) {
} break;
case 3210: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3211: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3212: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3213: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3214: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3215: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3216: yyDecrement (9) yySetNT (yyNTfile_clause) {
} break;
case 3217: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3218: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3219: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3220: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3221: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3222: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3223: yyDecrement (3) yySetNT (yyNTfile_clause) {
} break;
case 3224: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3225: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3226: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3227: yyDecrement (9) yySetNT (yyNTfile_clause) {
} break;
case 3228: yyDecrement (8) yySetNT (yyNTfile_clause) {
} break;
case 3229: yyDecrement (8) yySetNT (yyNTfile_clause) {
} break;
case 3230: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3231: yyDecrement (8) yySetNT (yyNTfile_clause) {
} break;
case 3232: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3233: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3234: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3235: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3236: yyDecrement (8) yySetNT (yyNTfile_clause) {
} break;
case 3237: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3238: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3239: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3240: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3241: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3242: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3243: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3244: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3245: yyDecrement (8) yySetNT (yyNTfile_clause) {
} break;
case 3246: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3247: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3248: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3249: yyDecrement (7) yySetNT (yyNTfile_clause) {
} break;
case 3250: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3251: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3252: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3253: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3254: yyDecrement (6) yySetNT (yyNTfile_clause) {
} break;
case 3255: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3256: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3257: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3258: yyDecrement (3) yySetNT (yyNTfile_clause) {
} break;
case 3259: yyDecrement (3) yySetNT (yyNTfile_clause) {
} break;
case 3260: yyDecrement (5) yySetNT (yyNTfile_clause) {
} break;
case 3261: yyDecrement (3) yySetNT (yyNTfile_clause) {
} break;
case 3262: yyDecrement (3) yySetNT (yyNTfile_clause) {
} break;
case 3263: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3264: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3265: yyDecrement (4) yySetNT (yyNTfile_clause) {
} break;
case 3266: yySetNT (yyNTsort_merge_clause_l) {
} break;
case 3267: yyDecrement (2) yySetNT (yyNTsort_merge_clause_l) {
} break;
case 3268: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3269: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3270: yyDecrement (6) yySetNT (yyNTsort_merge_clause) {
} break;
case 3271: yyDecrement (6) yySetNT (yyNTsort_merge_clause) {
} break;
case 3272: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3273: yyDecrement (6) yySetNT (yyNTsort_merge_clause) {
} break;
case 3274: yyDecrement (9) yySetNT (yyNTsort_merge_clause) {
} break;
case 3275: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3276: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3277: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3278: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3279: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3280: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3281: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3282: yyDecrement (4) yySetNT (yyNTsort_merge_clause) {
} break;
case 3283: yyDecrement (5) yySetNT (yyNTsort_merge_clause) {
} break;
case 3284: yyDecrement (1) yySetNT (yyNTvalue_l) {
} break;
case 3285: yyDecrement (2) yySetNT (yyNTvalue_l) {
} break;
case 3286: yyDecrement (3) yySetNT (yyNTvalue_e) {
/* line 952 "Parser.lrk" */
 ;
{  (void) UseName (yyA [0].Scan); ; } ;

} break;
case 3287: yyDecrement (3) yySetNT (yyNTvalue_e) {
/* line 955 "Parser.lrk" */
 ;
{  (void) UseName (yyA [0].Scan); ; } ;

} break;
case 3288: yyDecrement (2) yySetNT (yyNTfrom_o) {
} break;
case 3289: yySetNT (yyNTfrom_o) {
} break;
case 3290: yyDecrement (2) yySetNT (yyNTto_o) {
} break;
case 3291: yySetNT (yyNTto_o) {
} break;
case 3292: yyDecrement (3) yySetNT (yyNTdepending_o) {
} break;
case 3293: yySetNT (yyNTdepending_o) {
} break;
case 3294: yyDecrement (3) yySetNT (yyNTbottom_o) {
} break;
case 3295: yySetNT (yyNTbottom_o) {
} break;
case 3296: yyDecrement (4) yySetNT (yyNTfooting) {
} break;
case 3297: yyDecrement (2) yySetNT (yyNTtop) {
} break;
case 3298: yyDecrement (2) yySetNT (yyNTbottom) {
} break;
case 3299: yyDecrement (1) yySetNT (yyNTlinage) {
} break;
case 3300: yyDecrement (1) yySetNT (yyNTlinage) {
} break;
case 3301: yySetNT (yyNTdata_description_entry_l) {
} break;
case 3302: yyDecrement (2) yySetNT (yyNTdata_description_entry_l) {
} break;
case 3303: yyDecrement (2) yySetNT (yyNTdata_description_entry_l) {
} break;
case 3304: yyDecrement (3) yySetNT (yyNTdata_description_entry_l) {
} break;
case 3305: yyDecrement (10) yySetNT (yyNTdata_description_entry) {
/* line 975 "Parser.lrk" */
 ;
{  (void) Declare (yyA [0].Scan.level_number.Value, yyA [1].name_or_Filler.Scan, oDATA, PrevEPos); ; } ;

} break;
case 3306: yyDecrement (10) yySetNT (yyNTdata_description_entry) {
/* line 978 "Parser.lrk" */
 ;
{  (void) Declare (77, yyA [1].name_or_Filler.Scan, oDATA, PrevEPos); ; } ;

} break;
case 3307: yyDecrement (5) yySetNT (yyNTdata_description_entry) {
/* line 981 "Parser.lrk" */
 ;
{  (void) Declare (yyA [0].Scan.level_number.Value, yyA [1].name_or_Filler.Scan, oDATA, PrevEPos); ; } ;

} break;
case 3308: yyDecrement (5) yySetNT (yyNTdata_description_entry) {
/* line 984 "Parser.lrk" */
 ;
{  (void) Declare (77, yyA [1].name_or_Filler.Scan, oDATA, PrevEPos); ; } ;

} break;
case 3309: yyDecrement (4) yySetNT (yyNTdata_description_entry) {
/* line 987 "Parser.lrk" */
 ;
{  (void) Declare (66, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3310: yyDecrement (6) yySetNT (yyNTdata_description_entry) {
/* line 990 "Parser.lrk" */
 ;
{  (void) Declare (66, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3311: yyDecrement (7) yySetNT (yyNTdata_description_entry) {
/* line 993 "Parser.lrk" */
 ;
{  (void) Declare (88, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3312: yyDecrement (7) yySetNT (yyNTdata_description_entry) {
/* line 996 "Parser.lrk" */
 ;
{  (void) Declare (88, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3313: yyDecrement (6) yySetNT (yyNTdata_description_entry) {
/* line 999 "Parser.lrk" */
 ;
{  (void) Declare (78, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3314: yyDecrement (1) yySetNT (yyNTperiod_o) {
} break;
case 3315: yySetNT (yyNTperiod_o) {
} break;
case 3316: yyDecrement (2) yySetNT (yyNTrenames_tail) {
} break;
case 3317: yyDecrement (4) yySetNT (yyNTrenames_tail) {
} break;
case 3318: yyDecrement (2) yySetNT (yyNTredefines_o) {
/* line 1006 "Parser.lrk" */
 ;
{  (void) UseName (yyA [1].Scan); ; } ;

} break;
case 3319: yySetNT (yyNTredefines_o) {
} break;
case 3320: yyDecrement (1) yySetNT (yyNTname_or_Filler) {
/* line 1010 "Parser.lrk" */
 yyS.name_or_Filler.Scan = yyA [0].Scan;
 ;

} break;
case 3321: yyDecrement (1) yySetNT (yyNTname_or_Filler) {
/* line 1013 "Parser.lrk" */
 yyS.name_or_Filler.Scan.name.Ident = NoIdent;
			    yyS.name_or_Filler.Scan.Position   = NoPosition; ;
 ;

} break;
case 3322: yySetNT (yyNTdata_clause_l) {
} break;
case 3323: yyDecrement (2) yySetNT (yyNTdata_clause_l) {
} break;
case 3324: yyDecrement (2) yySetNT (yyNTdata_clause) {
} break;
case 3325: yyDecrement (2) yySetNT (yyNTdata_clause) {
} break;
case 3326: yyDecrement (3) yySetNT (yyNTdata_clause) {
} break;
case 3327: yyDecrement (3) yySetNT (yyNTdata_clause) {
} break;
case 3328: yyDecrement (1) yySetNT (yyNTdata_clause) {
} break;
case 3329: yyDecrement (3) yySetNT (yyNTdata_clause) {
} break;
case 3330: yyDecrement (1) yySetNT (yyNTdata_clause) {
} break;
case 3331: yyDecrement (7) yySetNT (yyNTdata_clause) {
} break;
case 3332: yyDecrement (5) yySetNT (yyNTdata_clause) {
} break;
case 3333: yyDecrement (10) yySetNT (yyNTdata_clause) {
} break;
case 3334: yyDecrement (8) yySetNT (yyNTdata_clause) {
} break;
case 3335: yyDecrement (1) yySetNT (yyNTdata_clause) {
} break;
case 3336: yyDecrement (2) yySetNT (yyNTdata_clause) {
} break;
case 3337: yyDecrement (2) yySetNT (yyNTdata_clause) {
} break;
case 3338: yyDecrement (2) yySetNT (yyNTdata_clause) {
} break;
case 3339: yyDecrement (3) yySetNT (yyNTdata_clause) {
} break;
case 3340: yyDecrement (3) yySetNT (yyNTdata_clause) {
} break;
case 3341: yyDecrement (3) yySetNT (yyNTdata_clause) {
} break;
case 3342: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3343: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3344: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3345: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3346: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3347: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3348: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3349: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3350: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3351: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3352: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3353: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3354: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3355: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3356: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3357: yyDecrement (1) yySetNT (yyNTusage) {
} break;
case 3358: yyDecrement (2) yySetNT (yyNTsign_1) {
} break;
case 3359: yyDecrement (2) yySetNT (yyNTsign_1) {
} break;
case 3360: yyDecrement (2) yySetNT (yyNTseparate_o) {
} break;
case 3361: yySetNT (yyNTseparate_o) {
} break;
case 3362: yySetNT (yyNTa_de_scending_l) {
} break;
case 3363: yyDecrement (5) yySetNT (yyNTa_de_scending_l) {
} break;
case 3364: yyDecrement (5) yySetNT (yyNTa_de_scending_l) {
} break;
case 3365: yyDecrement (3) yySetNT (yyNTindexed_o) {
} break;
case 3366: yySetNT (yyNTindexed_o) {
} break;
case 3367: yyDecrement (1) yySetNT (yyNTindexed_l) {
/* line 1062 "Parser.lrk" */
 ;
{  (void) Declare (IB, yyA [0].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3368: yyDecrement (2) yySetNT (yyNTindexed_l) {
/* line 1065 "Parser.lrk" */
 ;
{  (void) Declare (IB, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3369: yyDecrement (1) yySetNT (yyNTcondition_l) {
} break;
case 3370: yyDecrement (2) yySetNT (yyNTcondition_l) {
} break;
case 3371: yyDecrement (1) yySetNT (yyNTdd_condition_e) {
} break;
case 3372: yyDecrement (3) yySetNT (yyNTdd_condition_e) {
} break;
case 3373: yyDecrement (5) yySetNT (yyNTfalse_o) {
} break;
case 3374: yySetNT (yyNTfalse_o) {
} break;
case 3375: yyDecrement (1) yySetNT (yyNTconst_expression) {
} break;
case 3376: yyDecrement (3) yySetNT (yyNTconst_expression) {
} break;
case 3377: yyDecrement (1) yySetNT (yyNTconst_primary) {
} break;
case 3378: yyDecrement (1) yySetNT (yyNTconst_primary) {
} break;
case 3379: yyDecrement (3) yySetNT (yyNTconst_primary) {
} break;
case 3380: yyDecrement (3) yySetNT (yyNTconst_primary) {
} break;
case 3381: yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 3382: yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 3383: yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 3384: yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 3385: yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 3386: yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 3387: yyDecrement (8) yySetNT (yyNTcommunication_description_entry) {
/* line 1086 "Parser.lrk" */
 ;
{  (void) Declare (CD, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3388: yyDecrement (7) yySetNT (yyNTcommunication_description_entry) {
/* line 1089 "Parser.lrk" */
 ;
{  (void) Declare (CD, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3389: yyDecrement (8) yySetNT (yyNTcommunication_description_entry) {
/* line 1092 "Parser.lrk" */
 ;
{  (void) Declare (CD, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3390: yyDecrement (1) yySetNT (yyNTcd_input_o) {
} break;
case 3391: yyDecrement (11) yySetNT (yyNTcd_input_o) {
} break;
case 3392: yySetNT (yyNTcd_input_l) {
} break;
case 3393: yyDecrement (2) yySetNT (yyNTcd_input_l) {
} break;
case 3394: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3395: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3396: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3397: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3398: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3399: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3400: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3401: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3402: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3403: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3404: yyDecrement (4) yySetNT (yyNTcd_input_e) {
} break;
case 3405: yySetNT (yyNTcd_output_l) {
} break;
case 3406: yyDecrement (2) yySetNT (yyNTcd_output_l) {
} break;
case 3407: yyDecrement (4) yySetNT (yyNTcd_output_e) {
} break;
case 3408: yyDecrement (4) yySetNT (yyNTcd_output_e) {
} break;
case 3409: yyDecrement (4) yySetNT (yyNTcd_output_e) {
} break;
case 3410: yyDecrement (6) yySetNT (yyNTcd_output_e) {
} break;
case 3411: yyDecrement (4) yySetNT (yyNTcd_output_e) {
} break;
case 3412: yyDecrement (3) yySetNT (yyNTcd_output_e) {
} break;
case 3413: yyDecrement (4) yySetNT (yyNTcd_output_e) {
} break;
case 3414: yyDecrement (1) yySetNT (yyNTcd_i_o_o) {
} break;
case 3415: yyDecrement (6) yySetNT (yyNTcd_i_o_o) {
} break;
case 3416: yySetNT (yyNTcd_i_o_l) {
} break;
case 3417: yyDecrement (2) yySetNT (yyNTcd_i_o_l) {
} break;
case 3418: yyDecrement (4) yySetNT (yyNTcd_i_o_e) {
} break;
case 3419: yyDecrement (4) yySetNT (yyNTcd_i_o_e) {
} break;
case 3420: yyDecrement (4) yySetNT (yyNTcd_i_o_e) {
} break;
case 3421: yyDecrement (4) yySetNT (yyNTcd_i_o_e) {
} break;
case 3422: yyDecrement (4) yySetNT (yyNTcd_i_o_e) {
} break;
case 3423: yyDecrement (4) yySetNT (yyNTcd_i_o_e) {
} break;
case 3424: yyDecrement (1) yySetNT (yyNTname_or_filler) {
} break;
case 3425: yyDecrement (1) yySetNT (yyNTname_or_filler) {
} break;
case 3426: yyDecrement (4) yySetNT (yyNTreport_description_entry) {
/* line 1131 "Parser.lrk" */
 ;
{  (void) Declare (RD, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3427: yySetNT (yyNTreport_clause_l) {
} break;
case 3428: yyDecrement (2) yySetNT (yyNTreport_clause_l) {
} break;
case 3429: yyDecrement (2) yySetNT (yyNTreport_clause) {
} break;
case 3430: yyDecrement (3) yySetNT (yyNTreport_clause) {
} break;
case 3431: yyDecrement (3) yySetNT (yyNTreport_clause) {
} break;
case 3432: yyDecrement (3) yySetNT (yyNTreport_clause) {
} break;
case 3433: yyDecrement (3) yySetNT (yyNTreport_clause) {
} break;
case 3434: yyDecrement (3) yySetNT (yyNTreport_clause) {
} break;
case 3435: yyDecrement (3) yySetNT (yyNTreport_clause) {
} break;
case 3436: yyDecrement (4) yySetNT (yyNTreport_clause) {
} break;
case 3437: yyDecrement (4) yySetNT (yyNTreport_clause) {
} break;
case 3438: yyDecrement (5) yySetNT (yyNTreport_clause) {
} break;
case 3439: yyDecrement (2) yySetNT (yyNTreport_clause) {
} break;
case 3440: yyDecrement (2) yySetNT (yyNTlimit_o) {
} break;
case 3441: yyDecrement (2) yySetNT (yyNTlimit_o) {
} break;
case 3442: yyDecrement (1) yySetNT (yyNTlimit_o) {
} break;
case 3443: yyDecrement (1) yySetNT (yyNTlimit_o) {
} break;
case 3444: yyDecrement (1) yySetNT (yyNTline_o) {
} break;
case 3445: yyDecrement (1) yySetNT (yyNTline_o) {
} break;
case 3446: yySetNT (yyNTpage_l) {
} break;
case 3447: yyDecrement (2) yySetNT (yyNTpage_l) {
} break;
case 3448: yyDecrement (2) yySetNT (yyNTpage_e) {
} break;
case 3449: yyDecrement (3) yySetNT (yyNTpage_e) {
} break;
case 3450: yyDecrement (3) yySetNT (yyNTpage_e) {
} break;
case 3451: yyDecrement (2) yySetNT (yyNTpage_e) {
} break;
case 3452: yyDecrement (3) yySetNT (yyNTreport_group_description_entry) {
} break;
case 3453: yyDecrement (4) yySetNT (yyNTreport_group_description_entry) {
/* line 1160 "Parser.lrk" */
 ;
{  (void) Declare (yyA [0].Scan.level_number.Value, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3454: yySetNT (yyNTreport_group_clause_l) {
} break;
case 3455: yyDecrement (2) yySetNT (yyNTreport_group_clause_l) {
} break;
case 3456:
yy569: yyDecrement (4) yySetNT (yyNTreport_group_clause) {
} break;
case 3457: yyDecrement (6) yySetNT (yyNTreport_group_clause) {
} break;
case 3458: yyDecrement (7) yySetNT (yyNTreport_group_clause) {
} break;
case 3459: yyDecrement (5) yySetNT (yyNTreport_group_clause) {
} break;
case 3460: yyDecrement (5) yySetNT (yyNTreport_group_clause) {
} break;
case 3461: yyDecrement (4) yySetNT (yyNTreport_group_clause) {
} break;
case 3462: yyDecrement (5) yySetNT (yyNTreport_group_clause) {
} break;
case 3463: yyDecrement (5) yySetNT (yyNTreport_group_clause) {
} break;
case 3464: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3465: yyDecrement (1) yySetNT (yyNTreport_group_clause) {
} break;
case 3466: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3467: yyDecrement (1) yySetNT (yyNTreport_group_clause) {
} break;
case 3468: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3469: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3470: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3471: yyDecrement (1) yySetNT (yyNTreport_group_clause) {
} break;
case 3472: yyDecrement (2) yySetNT (yyNTreport_group_clause) {
} break;
case 3473: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3474: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3475: yyDecrement (4) yySetNT (yyNTreport_group_clause) {
} break;
case 3476: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3477: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3478: yyDecrement (3) yySetNT (yyNTreport_group_clause) {
} break;
case 3479: yyDecrement (5) yySetNT (yyNTreport_group_clause) {
} break;
case 3480: yyDecrement (2) yySetNT (yyNTreport_group_clause) {
} break;
case 3481: yyDecrement (2) yySetNT (yyNTtype) {
} break;
case 3482: yyDecrement (1) yySetNT (yyNTtype) {
} break;
case 3483: yyDecrement (2) yySetNT (yyNTtype) {
} break;
case 3484: yyDecrement (1) yySetNT (yyNTtype) {
} break;
case 3485: yyDecrement (3) yySetNT (yyNTtype) {
} break;
case 3486: yyDecrement (2) yySetNT (yyNTtype) {
} break;
case 3487: yyDecrement (1) yySetNT (yyNTtype) {
} break;
case 3488: yyDecrement (3) yySetNT (yyNTtype) {
} break;
case 3489: yyDecrement (2) yySetNT (yyNTtype) {
} break;
case 3490: yyDecrement (2) yySetNT (yyNTtype) {
} break;
case 3491: yyDecrement (1) yySetNT (yyNTtype) {
} break;
case 3492: yyDecrement (2) yySetNT (yyNTtype) {
} break;
case 3493: yyDecrement (1) yySetNT (yyNTtype) {
} break;
case 3494: yyDecrement (1) yySetNT (yyNTname_or_final) {
} break;
case 3495: yyDecrement (1) yySetNT (yyNTname_or_final) {
} break;
case 3496: yyDecrement (3) yySetNT (yyNTsign_2) {
} break;
case 3497: yyDecrement (3) yySetNT (yyNTsign_2) {
} break;
case 3498: yyDecrement (3) yySetNT (yyNTreset_o) {
} break;
case 3499: yySetNT (yyNTreset_o) {
} break;
case 3500: yySetNT (yyNTscreen_description_entry_l) {
} break;
case 3501: yyDecrement (2) yySetNT (yyNTscreen_description_entry_l) {
} break;
case 3502: yyDecrement (2) yySetNT (yyNTscreen_description_entry_l) {
} break;
case 3503: yyDecrement (3) yySetNT (yyNTscreen_description_entry_l) {
} break;
case 3504: yyDecrement (4) yySetNT (yyNTscreen_description_entry) {
/* line 1213 "Parser.lrk" */
 ;
{  (void) Declare (yyA [0].Scan.level_number.Value, yyA [1].Scan, oDATA, PrevEPos); ; } ;

} break;
case 3505: yyDecrement (4) yySetNT (yyNTscreen_description_entry) {
/* line 1216 "Parser.lrk" */
 ;
{ 	tScanAttribute a;
			a.name.Ident = NoIdent;
			a.Position   = yyA [0].Scan.Position;
			(void) Declare (yyA [0].Scan.level_number.Value, a, oDATA, PrevEPos); ; } ;

} break;
case 3506: yySetNT (yyNTscreen_clause_l) {
} break;
case 3507: yyDecrement (2) yySetNT (yyNTscreen_clause_l) {
} break;
case 3508: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3509: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3510: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3511: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3512: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3513: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3514: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3515: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3516: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3517: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3518: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3519: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3520: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3521: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3522: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3523: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3524: yyDecrement (5) yySetNT (yyNTscreen_clause) {
} break;
case 3525: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3526: yyDecrement (5) yySetNT (yyNTscreen_clause) {
} break;
case 3527: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3528: yyDecrement (5) yySetNT (yyNTscreen_clause) {
} break;
case 3529: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3530: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3531: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3532: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3533: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3534: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3535: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3536: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3537: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3538: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3539: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3540: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3541: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3542: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3543: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3544: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3545: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3546: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3547: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3548: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3549: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3550: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3551: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3552: yyDecrement (4) yySetNT (yyNTscreen_clause) {
} break;
case 3553: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3554: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3555: yyDecrement (2) yySetNT (yyNTscreen_clause) {
} break;
case 3556: yyDecrement (3) yySetNT (yyNTscreen_clause) {
} break;
case 3557: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3558: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3559: yyDecrement (1) yySetNT (yyNTscreen_clause) {
} break;
case 3560: yyDecrement (1) yySetNT (yyNTsign_line_o) {
} break;
case 3561: yyDecrement (1) yySetNT (yyNTsign_line_o) {
} break;
case 3562: yyDecrement (1) yySetNT (yyNTsign_line_o) {
} break;
case 3563: yySetNT (yyNTsign_line_o) {
} break;
case 3564: yyDecrement (5) yySetNT (yyNTprocedure_division) {
} break;
case 3565: yyDecrement (4) yySetNT (yyNTprocedure_division) {
} break;
case 3566: yyDecrement (6) yySetNT (yyNTprocedure_division) {
} break;
case 3567: yyDecrement (5) yySetNT (yyNTprocedure_division) {
} break;
case 3568: yyDecrement (3) yySetNT (yyNTprocedure_head) {
/* line 1284 "Parser.lrk" */
 ;
{  Section = cPROC_DIV; ; } ;

} break;
case 3569: yyDecrement (1) yySetNT (yyNTmnemonic_name_o) {
} break;
case 3570: yySetNT (yyNTmnemonic_name_o) {
} break;
case 3571: yyDecrement (2) yySetNT (yyNTusing_o) {
} break;
case 3572: yyDecrement (2) yySetNT (yyNTusing_o) {
} break;
case 3573: yySetNT (yyNTusing_o) {
} break;
case 3574: yyDecrement (1) yySetNT (yyNTusing_l) {
} break;
case 3575: yyDecrement (2) yySetNT (yyNTusing_l) {
} break;
case 3576: yyDecrement (1) yySetNT (yyNTusing_1_e) {
} break;
case 3577: yyDecrement (1) yySetNT (yyNTusing_1_e) {
} break;
case 3578: yyDecrement (3) yySetNT (yyNTusing_2_e) {
} break;
case 3579: yyDecrement (3) yySetNT (yyNTusing_2_e) {
} break;
case 3580: yyDecrement (6) yySetNT (yyNTdeclaratives_o) {
} break;
case 3581: yySetNT (yyNTdeclaratives_o) {
} break;
case 3582:
yy695: yyDecrement (3) yySetNT (yyNTd_section_l) {
} break;
case 3583:
yy696: yyDecrement (2) yySetNT (yyNTd_section_l) {
} break;
case 3584:
yy697: yyDecrement (4) yySetNT (yyNTd_section_l) {
} break;
case 3585:
yy698: yyDecrement (3) yySetNT (yyNTd_section_l) {
} break;
case 3586:
yy699: yyDecrement (1) yySetNT (yyNTsection_l) {
} break;
case 3587: yyDecrement (1) yySetNT (yyNTsection_l) {
} break;
case 3588: yyDecrement (2) yySetNT (yyNTsection_l) {
} break;
case 3589:
yy702: yyDecrement (1) yySetNT (yyNTsection_e) {
/* line 1307 "Parser.lrk" */
 ;
{  DeclareEnd (lSECTION, PrevEPos); ; } ;

} break;
case 3590:
yy703: yyDecrement (2) yySetNT (yyNTsection_e) {
/* line 1310 "Parser.lrk" */
 ;
{  DeclareEnd (lSECTION, PrevEPos); ; } ;

} break;
case 3591: yyDecrement (4) yySetNT (yyNTsection_head) {
/* line 1313 "Parser.lrk" */
 ;
{  (void) DeclareLabel (yyA [0].chapter_name.Scan, lSECTION, PrevEPos); ; } ;

} break;
case 3592: yyDecrement (1) yySetNT (yyNTsegment_number_o) {
} break;
case 3593: yySetNT (yyNTsegment_number_o) {
} break;
case 3594: yyDecrement (2) yySetNT (yyNTuse_o) {
} break;
case 3595: yySetNT (yyNTuse_o) {
} break;
case 3596: yyDecrement (1) yySetNT (yyNTparagraph_l) {
} break;
case 3597: yyDecrement (1) yySetNT (yyNTparagraph_l) {
} break;
case 3598: yyDecrement (2) yySetNT (yyNTparagraph_l) {
} break;
case 3599: yyDecrement (1) yySetNT (yyNTparagraph_e) {
/* line 1323 "Parser.lrk" */
 ;
{  DeclareEnd (lPARAGRPH, PrevEPos); ; } ;

} break;
case 3600: yyDecrement (2) yySetNT (yyNTparagraph_e) {
/* line 1326 "Parser.lrk" */
 ;
{  DeclareEnd (lPARAGRPH, PrevEPos); ; } ;

} break;
case 3601: yyDecrement (2) yySetNT (yyNTparagraph_head) {
/* line 1329 "Parser.lrk" */
 ;
{  (void) DeclareLabel (yyA [0].chapter_name.Scan, lPARAGRPH, PrevEPos); ; } ;

} break;
case 3602: yyDecrement (2) yySetNT (yyNTsentence_l) {
} break;
case 3603: yyDecrement (1) yySetNT (yyNTsentence_l) {
} break;
case 3604: yyDecrement (1) yySetNT (yyNTsentence_l) {
} break;
case 3605: yyDecrement (3) yySetNT (yyNTsentence_l) {
} break;
case 3606: yyDecrement (2) yySetNT (yyNTsentence_l) {
} break;
case 3607: yyDecrement (2) yySetNT (yyNTsentence_l) {
} break;
case 3608: yyDecrement (1) yySetNT (yyNTstatement_l) {
} break;
case 3609: yyDecrement (2) yySetNT (yyNTstatement_l) {
} break;
case 3610: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3611: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3612: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3613: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3614: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3615: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3616: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3617: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3618: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3619: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3620: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3621: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3622: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3623: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3624: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3625: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3626: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3627: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3628: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3629: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3630: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3631: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3632: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3633: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3634: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3635: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3636: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3637: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3638: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3639: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3640: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3641: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3642: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3643: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3644: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3645: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3646: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3647: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3648: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3649: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3650: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3651: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3652: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3653: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3654: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3655: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3656: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3657: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3658: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3659: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3660: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3661: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3662: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3663: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3664: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3665: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3666: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3667: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3668: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3669: yyDecrement (1) yySetNT (yyNTstatement) {
} break;
case 3670: yyDecrement (1) yySetNT (yyNTimperative_statement) {
} break;
case 3671: yyDecrement (2) yySetNT (yyNTimperative_statement) {
} break;
case 3672: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3673: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3674: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3675: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3676: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3677: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3678: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3679: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3680: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3681: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3682: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3683: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3684: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3685: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3686: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3687: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3688: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3689: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3690: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3691: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3692: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3693: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3694: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3695: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3696: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3697: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3698: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3699: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3700: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3701: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3702: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3703: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3704: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3705: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3706: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3707: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3708: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3709: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3710: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3711: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3712: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3713: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3714: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3715: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3716: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3717: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3718: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3719: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3720: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3721: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3722: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3723: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3724: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3725: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3726: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3727: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3728: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3729: yyDecrement (1) yySetNT (yyNTstatement_i) {
} break;
case 3730: yyDecrement (5) yySetNT (yyNTaccept) {
} break;
case 3731: yyDecrement (4) yySetNT (yyNTaccept) {
} break;
case 3732: yyDecrement (6) yySetNT (yyNTaccept) {
} break;
case 3733: yyDecrement (5) yySetNT (yyNTaccept) {
} break;
case 3734: yyDecrement (5) yySetNT (yyNTaccept) {
} break;
case 3735: yyDecrement (3) yySetNT (yyNTaccept) {
} break;
case 3736: yyDecrement (4) yySetNT (yyNTaccept) {
} break;
case 3737: yyDecrement (5) yySetNT (yyNTaccept) {
} break;
case 3738: yyDecrement (6) yySetNT (yyNTaccept) {
} break;
case 3739: yyDecrement (7) yySetNT (yyNTaccept) {
} break;
case 3740: yyDecrement (4) yySetNT (yyNTaccept) {
} break;
case 3741: yyDecrement (5) yySetNT (yyNTaccept) {
} break;
case 3742: yyDecrement (6) yySetNT (yyNTaccept) {
} break;
case 3743: yyDecrement (7) yySetNT (yyNTaccept) {
} break;
case 3744: yyDecrement (8) yySetNT (yyNTaccept) {
} break;
case 3745: yyDecrement (6) yySetNT (yyNTaccept) {
} break;
case 3746: yyDecrement (5) yySetNT (yyNTaccept_i) {
} break;
case 3747: yyDecrement (4) yySetNT (yyNTaccept_i) {
} break;
case 3748: yyDecrement (6) yySetNT (yyNTaccept_i) {
} break;
case 3749: yyDecrement (5) yySetNT (yyNTaccept_i) {
} break;
case 3750: yyDecrement (5) yySetNT (yyNTaccept_i) {
} break;
case 3751: yyDecrement (3) yySetNT (yyNTaccept_i) {
} break;
case 3752: yyDecrement (4) yySetNT (yyNTaccept_i) {
} break;
case 3753: yyDecrement (5) yySetNT (yyNTaccept_i) {
} break;
case 3754: yyDecrement (6) yySetNT (yyNTaccept_i) {
} break;
case 3755: yyDecrement (7) yySetNT (yyNTaccept_i) {
} break;
case 3756: yyDecrement (4) yySetNT (yyNTaccept_i) {
} break;
case 3757: yyDecrement (5) yySetNT (yyNTaccept_i) {
} break;
case 3758: yyDecrement (6) yySetNT (yyNTaccept_i) {
} break;
case 3759: yyDecrement (7) yySetNT (yyNTaccept_i) {
} break;
case 3760: yyDecrement (8) yySetNT (yyNTaccept_i) {
} break;
case 3761: yyDecrement (6) yySetNT (yyNTaccept_i) {
} break;
case 3762: yyDecrement (2) yySetNT (yyNTexception_2) {
} break;
case 3763: yyDecrement (6) yySetNT (yyNTexception_2) {
} break;
case 3764: yyDecrement (1) yySetNT (yyNTaccept_name) {
} break;
case 3765: yyDecrement (1) yySetNT (yyNTaccept_name) {
} break;
case 3766: yyDecrement (1) yySetNT (yyNTaccept_from) {
} break;
case 3767: yyDecrement (1) yySetNT (yyNTaccept_from) {
} break;
case 3768: yyDecrement (1) yySetNT (yyNTaccept_from) {
} break;
case 3769: yyDecrement (1) yySetNT (yyNTaccept_from) {
} break;
case 3770: yyDecrement (2) yySetNT (yyNTaccept_from) {
} break;
case 3771: yyDecrement (2) yySetNT (yyNTaccept_from) {
} break;
case 3772: yyDecrement (2) yySetNT (yyNTaccept_from) {
} break;
case 3773: yyDecrement (2) yySetNT (yyNTaccept_from) {
} break;
case 3774: yyDecrement (3) yySetNT (yyNTescape) {
} break;
case 3775: yyDecrement (4) yySetNT (yyNTescape) {
} break;
case 3776: yyDecrement (7) yySetNT (yyNTescape) {
} break;
case 3777: yyDecrement (1) yySetNT (yyNTexception_or_escape) {
} break;
case 3778: yyDecrement (1) yySetNT (yyNTexception_or_escape) {
} break;
case 3779: yyDecrement (4) yySetNT (yyNTline_column) {
} break;
case 3780: yyDecrement (4) yySetNT (yyNTline_column) {
} break;
case 3781: yyDecrement (4) yySetNT (yyNTline_column) {
} break;
case 3782: yyDecrement (7) yySetNT (yyNTline_column) {
} break;
case 3783: yyDecrement (7) yySetNT (yyNTline_column) {
} break;
case 3784: yyDecrement (2) yySetNT (yyNTline_column) {
} break;
case 3785: yyDecrement (2) yySetNT (yyNTfrom_crt_o) {
} break;
case 3786: yySetNT (yyNTfrom_crt_o) {
} break;
case 3787: yyDecrement (2) yySetNT (yyNTfrom_crt) {
} break;
case 3788: yyDecrement (3) yySetNT (yyNTmode_block_o) {
} break;
case 3789: yySetNT (yyNTmode_block_o) {
} break;
case 3790: yyDecrement (3) yySetNT (yyNTmode_block) {
} break;
case 3791: yyDecrement (2) yySetNT (yyNTwith_o) {
} break;
case 3792:
yy905: yySetNT (yyNTwith_o) {
} break;
case 3793: yyDecrement (2) yySetNT (yyNTwith) {
} break;
case 3794: yySetNT (yyNTwith_l) {
} break;
case 3795: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3796: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3797: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3798: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3799: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3800: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3801: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3802: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3803: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3804: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3805: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3806: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3807: yyDecrement (4) yySetNT (yyNTwith_l) {
} break;
case 3808: yyDecrement (5) yySetNT (yyNTwith_l) {
} break;
case 3809: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3810: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3811: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3812: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3813: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3814: yyDecrement (4) yySetNT (yyNTwith_l) {
} break;
case 3815: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3816: yyDecrement (4) yySetNT (yyNTwith_l) {
} break;
case 3817: yyDecrement (4) yySetNT (yyNTwith_l) {
} break;
case 3818: yyDecrement (4) yySetNT (yyNTwith_l) {
} break;
case 3819: yyDecrement (4) yySetNT (yyNTwith_l) {
} break;
case 3820: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3821: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3822: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3823: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3824: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3825: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3826: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3827: yyDecrement (2) yySetNT (yyNTwith_l) {
} break;
case 3828: yyDecrement (5) yySetNT (yyNTadd) {
} break;
case 3829: yyDecrement (6) yySetNT (yyNTadd) {
} break;
case 3830: yyDecrement (8) yySetNT (yyNTadd) {
} break;
case 3831: yyDecrement (9) yySetNT (yyNTadd) {
} break;
case 3832: yyDecrement (6) yySetNT (yyNTadd) {
} break;
case 3833: yyDecrement (7) yySetNT (yyNTadd) {
} break;
case 3834: yyDecrement (5) yySetNT (yyNTadd) {
} break;
case 3835: yyDecrement (6) yySetNT (yyNTadd) {
} break;
case 3836: yyDecrement (5) yySetNT (yyNTadd_i) {
} break;
case 3837: yyDecrement (6) yySetNT (yyNTadd_i) {
} break;
case 3838: yyDecrement (8) yySetNT (yyNTadd_i) {
} break;
case 3839: yyDecrement (9) yySetNT (yyNTadd_i) {
} break;
case 3840: yyDecrement (6) yySetNT (yyNTadd_i) {
} break;
case 3841: yyDecrement (7) yySetNT (yyNTadd_i) {
} break;
case 3842: yyDecrement (5) yySetNT (yyNTadd_i) {
} break;
case 3843: yyDecrement (6) yySetNT (yyNTadd_i) {
} break;
case 3844: yyDecrement (4) yySetNT (yyNTsize_error) {
} break;
case 3845: yyDecrement (5) yySetNT (yyNTsize_error) {
} break;
case 3846: yyDecrement (9) yySetNT (yyNTsize_error) {
} break;
case 3847:
yy960: yySetNT (yyNTgiving) {
} break;
case 3848: yyDecrement (2) yySetNT (yyNTalter) {
} break;
case 3849: yyDecrement (1) yySetNT (yyNTalter_l) {
} break;
case 3850: yyDecrement (2) yySetNT (yyNTalter_l) {
} break;
case 3851: yyDecrement (3) yySetNT (yyNTalter_e) {
} break;
case 3852: yyDecrement (5) yySetNT (yyNTalter_e) {
} break;
case 3853: yyDecrement (5) yySetNT (yyNTcall) {
} break;
case 3854: yyDecrement (6) yySetNT (yyNTcall) {
} break;
case 3855: yyDecrement (6) yySetNT (yyNTcall) {
} break;
case 3856: yyDecrement (5) yySetNT (yyNTcall_i) {
} break;
case 3857: yyDecrement (6) yySetNT (yyNTcall_i) {
} break;
case 3858: yyDecrement (6) yySetNT (yyNTcall_i) {
} break;
case 3859: yyDecrement (1) yySetNT (yyNTcall_name) {
/* line 1589 "Parser.lrk" */
 ;
{   UseLabelExtern (yyA [0].qualification_n.Scan); ; } ;

} break;
case 3860: yyDecrement (1) yySetNT (yyNTcall_name) {
/* line 1592 "Parser.lrk" */
 ;
{ 	char word [128];
			StGetString (yyA [0].Scan.string.Value, word);
			yyA [0].Scan.name.Ident = MakeIdent (word, strlen (word));
			UseLabelExtern (yyA [0].Scan); ; } ;

} break;
case 3861: yyDecrement (2) yySetNT (yyNTcall_name) {
/* line 1598 "Parser.lrk" */
 ;
{   UseLabelExtern (yyA [1].qualification_n.Scan);
			UseLabelExtern (yyA [0].Scan); ; } ;

} break;
case 3862: yyDecrement (2) yySetNT (yyNTcall_name) {
/* line 1602 "Parser.lrk" */
 ;
{ 	char word [128];
			StGetString (yyA [1].Scan.string.Value, word);
			yyA [1].Scan.name.Ident = MakeIdent (word, strlen (word));
			UseLabelExtern (yyA [1].Scan);
			UseLabelExtern (yyA [0].Scan); ; } ;

} break;
case 3863: yyDecrement (2) yySetNT (yyNTcall_using_o) {
} break;
case 3864: yySetNT (yyNTcall_using_o) {
} break;
case 3865: yyDecrement (1) yySetNT (yyNTcall_using_l) {
} break;
case 3866: yyDecrement (2) yySetNT (yyNTcall_using_l) {
} break;
case 3867: yyDecrement (1) yySetNT (yyNTcall_using_1_e) {
} break;
case 3868: yyDecrement (1) yySetNT (yyNTcall_using_1_e) {
} break;
case 3869: yyDecrement (1) yySetNT (yyNTcall_using_2_e) {
} break;
case 3870: yyDecrement (3) yySetNT (yyNTcall_using_2_e) {
} break;
case 3871: yyDecrement (1) yySetNT (yyNTcall_chain_l) {
} break;
case 3872: yyDecrement (2) yySetNT (yyNTcall_chain_l) {
} break;
case 3873: yyDecrement (1) yySetNT (yyNTcall_chain_e) {
} break;
case 3874: yyDecrement (4) yySetNT (yyNTcall_chain_e) {
} break;
case 3875: yyDecrement (3) yySetNT (yyNTgiving_o) {
} break;
case 3876: yyDecrement (4) yySetNT (yyNTgiving_o) {
} break;
case 3877: yySetNT (yyNTgiving_o) {
} break;
case 3878: yyDecrement (3) yySetNT (yyNToverflow) {
} break;
case 3879: yyDecrement (4) yySetNT (yyNToverflow) {
} break;
case 3880: yyDecrement (7) yySetNT (yyNToverflow) {
} break;
case 3881: yyDecrement (3) yySetNT (yyNTon_overflow) {
} break;
case 3882: yyDecrement (3) yySetNT (yyNTexception) {
} break;
case 3883: yyDecrement (4) yySetNT (yyNTexception) {
} break;
case 3884: yyDecrement (7) yySetNT (yyNTexception) {
} break;
case 3885: yyDecrement (2) yySetNT (yyNTcancel) {
} break;
case 3886: yyDecrement (3) yySetNT (yyNTchain) {
} break;
case 3887: yyDecrement (5) yySetNT (yyNTchain) {
} break;
case 3888: yyDecrement (1) yySetNT (yyNTchain_l) {
} break;
case 3889: yyDecrement (2) yySetNT (yyNTchain_l) {
} break;
case 3890: yyDecrement (1) yySetNT (yyNTchain_1_e) {
} break;
case 3891: yyDecrement (1) yySetNT (yyNTchain_1_e) {
} break;
case 3892: yyDecrement (3) yySetNT (yyNTchain_2_e) {
} break;
case 3893: yyDecrement (3) yySetNT (yyNTchain_2_e) {
} break;
case 3894: yyDecrement (5) yySetNT (yyNTchain_2_e) {
} break;
case 3895: yyDecrement (3) yySetNT (yyNTchain_2_e) {
} break;
case 3896: yyDecrement (2) yySetNT (yyNTclose) {
} break;
case 3897: yyDecrement (1) yySetNT (yyNTclose_l) {
} break;
case 3898: yyDecrement (2) yySetNT (yyNTclose_l) {
} break;
case 3899:
yy1012: yyDecrement (1) yySetNT (yyNTclose_e) {
} break;
case 3900:
yy1013: yyDecrement (2) yySetNT (yyNTclose_e) {
} break;
case 3901: yyDecrement (4) yySetNT (yyNTclose_e) {
} break;
case 3902: yyDecrement (4) yySetNT (yyNTclose_e) {
} break;
case 3903: yyDecrement (5) yySetNT (yyNTclose_e) {
} break;
case 3904: yyDecrement (4) yySetNT (yyNTclose_e) {
} break;
case 3905: yyDecrement (3) yySetNT (yyNTclose_e) {
} break;
case 3906: yyDecrement (3) yySetNT (yyNTclose_e) {
} break;
case 3907: yyDecrement (1) yySetNT (yyNTcommit) {
} break;
case 3908: yyDecrement (5) yySetNT (yyNTcompute) {
} break;
case 3909: yyDecrement (6) yySetNT (yyNTcompute) {
} break;
case 3910: yyDecrement (5) yySetNT (yyNTcompute_i) {
} break;
case 3911: yyDecrement (6) yySetNT (yyNTcompute_i) {
} break;
case 3912: yyDecrement (1) yySetNT (yyNTequal_2) {
} break;
case 3913: yyDecrement (1) yySetNT (yyNTequal_2) {
} break;
case 3914: yyDecrement (1) yySetNT (yyNTcontinue) {
} break;
case 3915: yyDecrement (4) yySetNT (yyNTdelete) {
} break;
case 3916: yyDecrement (5) yySetNT (yyNTdelete) {
} break;
case 3917: yyDecrement (3) yySetNT (yyNTdelete) {
} break;
case 3918: yyDecrement (4) yySetNT (yyNTdelete_i) {
} break;
case 3919: yyDecrement (5) yySetNT (yyNTdelete_i) {
} break;
case 3920: yyDecrement (3) yySetNT (yyNTdelete_i) {
} break;
case 3921: yyDecrement (3) yySetNT (yyNTinvalid) {
} break;
case 3922: yyDecrement (4) yySetNT (yyNTinvalid) {
} break;
case 3923: yyDecrement (7) yySetNT (yyNTinvalid) {
} break;
case 3924:
yy1037: yyDecrement (3) yySetNT (yyNTdisable) {
} break;
case 3925: yyDecrement (6) yySetNT (yyNTdisable) {
} break;
case 3926: yyDecrement (1) yySetNT (yyNTdevice) {
} break;
case 3927: yyDecrement (2) yySetNT (yyNTdevice) {
} break;
case 3928: yyDecrement (2) yySetNT (yyNTdevice) {
} break;
case 3929: yyDecrement (1) yySetNT (yyNTdevice) {
} break;
case 3930: yyDecrement (5) yySetNT (yyNTdisplay) {
} break;
case 3931: yyDecrement (6) yySetNT (yyNTdisplay) {
} break;
case 3932: yyDecrement (3) yySetNT (yyNTdisplay) {
} break;
case 3933: yyDecrement (5) yySetNT (yyNTdisplay_i) {
} break;
case 3934: yyDecrement (6) yySetNT (yyNTdisplay_i) {
} break;
case 3935: yyDecrement (3) yySetNT (yyNTdisplay_i) {
} break;
case 3936:
yy1049: yyDecrement (1) yySetNT (yyNTdisplay_l) {
} break;
case 3937: yyDecrement (2) yySetNT (yyNTdisplay_l) {
} break;
case 3938: yyDecrement (1) yySetNT (yyNTdisplay_2_l) {
} break;
case 3939: yyDecrement (2) yySetNT (yyNTdisplay_2_l) {
} break;
case 3940:
yy1053: yyDecrement (2) yySetNT (yyNTdisplay_2_e) {
} break;
case 3941: yyDecrement (2) yySetNT (yyNTupon_o) {
} break;
case 3942: yyDecrement (2) yySetNT (yyNTupon_o) {
} break;
case 3943: yySetNT (yyNTupon_o) {
} break;
case 3944: yyDecrement (3) yySetNT (yyNTdisplay_advancing_o) {
} break;
case 3945:
yy1058: yySetNT (yyNTdisplay_advancing_o) {
} break;
case 3946: yyDecrement (1) yySetNT (yyNTdisplay_3_l) {
} break;
case 3947: yyDecrement (2) yySetNT (yyNTdisplay_3_l) {
} break;
case 3948: yyDecrement (4) yySetNT (yyNTdisplay_3_e) {
} break;
case 3949: yyDecrement (4) yySetNT (yyNTdisplay_3_e) {
} break;
case 3950: yyDecrement (4) yySetNT (yyNTdisplay_3_e) {
} break;
case 3951: yyDecrement (2) yySetNT (yyNTdisplay_3_e) {
} break;
case 3952: yyDecrement (2) yySetNT (yyNTdisplay_3_e) {
} break;
case 3953: yyDecrement (2) yySetNT (yyNTdisplay_3_e) {
} break;
case 3954: yyDecrement (1) yySetNT (yyNTdisplay_3_e) {
} break;
case 3955: yyDecrement (2) yySetNT (yyNTdisplay_3_e) {
} break;
case 3956: yySetNT (yyNTwith_display_l) {
} break;
case 3957: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3958: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3959: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3960: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3961: yyDecrement (3) yySetNT (yyNTwith_display_l) {
} break;
case 3962: yyDecrement (3) yySetNT (yyNTwith_display_l) {
} break;
case 3963: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3964: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3965: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3966: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3967: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3968: yyDecrement (4) yySetNT (yyNTwith_display_l) {
} break;
case 3969: yyDecrement (2) yySetNT (yyNTwith_display_l) {
} break;
case 3970: yyDecrement (4) yySetNT (yyNTwith_display_l) {
} break;
case 3971: yyDecrement (4) yySetNT (yyNTwith_display_l) {
} break;
case 3972: yyDecrement (4) yySetNT (yyNTwith_display_l) {
} break;
case 3973: yyDecrement (3) yySetNT (yyNTwith_display_l) {
} break;
case 3974: yyDecrement (3) yySetNT (yyNTwith_display_l) {
} break;
case 3975: yyDecrement (5) yySetNT (yyNTdivide) {
} break;
case 3976: yyDecrement (8) yySetNT (yyNTdivide) {
} break;
case 3977: yyDecrement (7) yySetNT (yyNTdivide) {
} break;
case 3978: yyDecrement (10) yySetNT (yyNTdivide) {
} break;
case 3979: yyDecrement (9) yySetNT (yyNTdivide) {
} break;
case 3980: yyDecrement (6) yySetNT (yyNTdivide) {
} break;
case 3981: yyDecrement (9) yySetNT (yyNTdivide) {
} break;
case 3982: yyDecrement (8) yySetNT (yyNTdivide) {
} break;
case 3983: yyDecrement (11) yySetNT (yyNTdivide) {
} break;
case 3984: yyDecrement (10) yySetNT (yyNTdivide) {
} break;
case 3985: yyDecrement (5) yySetNT (yyNTdivide_i) {
} break;
case 3986: yyDecrement (8) yySetNT (yyNTdivide_i) {
} break;
case 3987: yyDecrement (7) yySetNT (yyNTdivide_i) {
} break;
case 3988: yyDecrement (10) yySetNT (yyNTdivide_i) {
} break;
case 3989: yyDecrement (9) yySetNT (yyNTdivide_i) {
} break;
case 3990: yyDecrement (6) yySetNT (yyNTdivide_i) {
} break;
case 3991: yyDecrement (9) yySetNT (yyNTdivide_i) {
} break;
case 3992: yyDecrement (8) yySetNT (yyNTdivide_i) {
} break;
case 3993: yyDecrement (11) yySetNT (yyNTdivide_i) {
} break;
case 3994: yyDecrement (10) yySetNT (yyNTdivide_i) {
} break;
case 3995:
yy1108: yyDecrement (3) yySetNT (yyNTenable) {
} break;
case 3996: yyDecrement (6) yySetNT (yyNTenable) {
} break;
case 3997: yyDecrement (2) yySetNT (yyNTenter) {
/* line 1743 "Parser.lrk" */
 ;
{  UseLabelExtern (yyA [1].Scan); ; } ;

} break;
case 3998: yyDecrement (3) yySetNT (yyNTenter) {
/* line 1746 "Parser.lrk" */
 ;
{  UseLabelExtern (yyA [1].Scan); ; } ;

} break;
case 3999: yyDecrement (2) yySetNT (yyNTentry) {
/* line 1749 "Parser.lrk" */
 ;
{  (void) DeclareLabel (yyA [1].Scan, lENTRY, PrevEPos); ; } ;

} break;
case 4000: yyDecrement (4) yySetNT (yyNTentry) {
/* line 1752 "Parser.lrk" */
 ;
{  (void) DeclareLabel (yyA [1].Scan, lENTRY, PrevEPos); ; } ;

} break;
case 4001: yyDecrement (2) yySetNT (yyNTentry) {
/* line 1755 "Parser.lrk" */
 ;
{ 	char word [128];
			StGetString (yyA [1].Scan.string.Value, word);
			yyA [1].Scan.name.Ident = MakeIdent (word, strlen (word));
			(void) DeclareLabel (yyA [1].Scan, lENTRY, PrevEPos); ; } ;

} break;
case 4002: yyDecrement (4) yySetNT (yyNTentry) {
/* line 1761 "Parser.lrk" */
 ;
{ 	char word [128];
			StGetString (yyA [1].Scan.string.Value, word);
			yyA [1].Scan.name.Ident = MakeIdent (word, strlen (word));
			(void) DeclareLabel (yyA [1].Scan, lENTRY, PrevEPos); ; } ;

} break;
case 4003: yyDecrement (1) yySetNT (yyNTentry_l) {
} break;
case 4004: yyDecrement (2) yySetNT (yyNTentry_l) {
} break;
case 4005: yyDecrement (1) yySetNT (yyNTentry_1_e) {
} break;
case 4006: yyDecrement (1) yySetNT (yyNTentry_1_e) {
} break;
case 4007: yyDecrement (3) yySetNT (yyNTentry_2_e) {
} break;
case 4008: yyDecrement (3) yySetNT (yyNTentry_2_e) {
} break;
case 4009: yyDecrement (5) yySetNT (yyNTevaluate) {
} break;
case 4010: yyDecrement (5) yySetNT (yyNTevaluate_i) {
} break;
case 4011: yyDecrement (1) yySetNT (yyNTevaluate_expression_l) {
} break;
case 4012: yyDecrement (3) yySetNT (yyNTevaluate_expression_l) {
} break;
case 4013: yyDecrement (1) yySetNT (yyNTevaluate_expression) {
} break;
case 4014: yyDecrement (1) yySetNT (yyNTevaluate_expression) {
} break;
case 4015: yyDecrement (1) yySetNT (yyNTevaluate_expression) {
} break;
case 4016: yyDecrement (2) yySetNT (yyNTcase_l) {
} break;
case 4017: yyDecrement (3) yySetNT (yyNTcase_l) {
} break;
case 4018: yyDecrement (3) yySetNT (yyNTwhen_other_o) {
} break;
case 4019: yySetNT (yyNTwhen_other_o) {
} break;
case 4020: yyDecrement (2) yySetNT (yyNTwhen_l) {
} break;
case 4021: yyDecrement (3) yySetNT (yyNTwhen_l) {
} break;
case 4022: yyDecrement (1) yySetNT (yyNTwhen_label_l) {
} break;
case 4023: yyDecrement (3) yySetNT (yyNTwhen_label_l) {
} break;
case 4024: yyDecrement (1) yySetNT (yyNTwhen_label) {
} break;
case 4025: yyDecrement (1) yySetNT (yyNTwhen_label) {
} break;
case 4026: yyDecrement (1) yySetNT (yyNTwhen_label) {
} break;
case 4027: yyDecrement (1) yySetNT (yyNTwhen_label) {
} break;
case 4028: yyDecrement (3) yySetNT (yyNTwhen_label) {
} break;
case 4029: yyDecrement (1) yySetNT (yyNTexpression_or_literal) {
} break;
case 4030: yyDecrement (1) yySetNT (yyNTexpression_or_literal) {
} break;
case 4031: yyDecrement (5) yySetNT (yyNTexamine) {
} break;
case 4032: yyDecrement (9) yySetNT (yyNTexamine) {
} break;
case 4033: yyDecrement (8) yySetNT (yyNTexamine) {
} break;
case 4034:
yy1147: yySetNT (yyNTexamine_repl) {
} break;
case 4035: yyDecrement (1) yySetNT (yyNTexamine_repl_o) {
} break;
case 4036: yyDecrement (1) yySetNT (yyNTexamine_repl_o) {
} break;
case 4037: yyDecrement (2) yySetNT (yyNTexamine_repl_o) {
} break;
case 4038: yyDecrement (1) yySetNT (yyNTexamine_repl_o) {
} break;
case 4039: yyDecrement (1) yySetNT (yyNTexamine_tally_o) {
} break;
case 4040: yyDecrement (1) yySetNT (yyNTexamine_tally_o) {
} break;
case 4041: yyDecrement (2) yySetNT (yyNTexamine_tally_o) {
} break;
case 4042: yyDecrement (1) yySetNT (yyNTexecute) {
} break;
case 4043: yyDecrement (4) yySetNT (yyNTexhibit) {
} break;
case 4044: yyDecrement (1) yySetNT (yyNTexhibit_o) {
} break;
case 4045: yyDecrement (2) yySetNT (yyNTexhibit_o) {
} break;
case 4046: yyDecrement (1) yySetNT (yyNTexhibit_o) {
} break;
case 4047: yyDecrement (1) yySetNT (yyNTexit) {
} break;
case 4048: yyDecrement (2) yySetNT (yyNTexit) {
} break;
case 4049: yyDecrement (3) yySetNT (yyNTexit) {
} break;
case 4050: yyDecrement (2) yySetNT (yyNTexit) {
} break;
case 4051: yyDecrement (3) yySetNT (yyNTexit) {
} break;
case 4052: yyDecrement (2) yySetNT (yyNTexit) {
} break;
case 4053: yyDecrement (2) yySetNT (yyNTexit) {
} break;
case 4054: yyDecrement (2) yySetNT (yyNTgiving_2_o) {
} break;
case 4055: yyDecrement (4) yySetNT (yyNTgiving_2_o) {
} break;
case 4056: yyDecrement (2) yySetNT (yyNTgiving_2_o) {
} break;
case 4057: yyDecrement (5) yySetNT (yyNTgiving_2_o) {
} break;
case 4058: yyDecrement (1) yySetNT (yyNTgiving_or_returning) {
} break;
case 4059: yyDecrement (1) yySetNT (yyNTgiving_or_returning) {
} break;
case 4060: yyDecrement (2) yySetNT (yyNTgenerate) {
} break;
case 4061: yyDecrement (1) yySetNT (yyNTgoback) {
} break;
case 4062: yyDecrement (2) yySetNT (yyNTgoback) {
} break;
case 4063: yyDecrement (2) yySetNT (yyNTgoto) {
} break;
case 4064: yyDecrement (3) yySetNT (yyNTgoto) {
} break;
case 4065: yyDecrement (6) yySetNT (yyNTgoto) {
} break;
case 4066: yyDecrement (1) yySetNT (yyNTprocedure_name_l) {
} break;
case 4067: yyDecrement (2) yySetNT (yyNTprocedure_name_l) {
} break;
case 4068: yyDecrement (4) yySetNT (yyNTif) {
} break;
case 4069: yyDecrement (4) yySetNT (yyNTif_i) {
} break;
case 4070: yyDecrement (2) yySetNT (yyNTthen) {
} break;
case 4071: yyDecrement (3) yySetNT (yyNTelse) {
} break;
case 4072: yyDecrement (1) yySetNT (yyNTelse) {
} break;
case 4073: yyDecrement (3) yySetNT (yyNTelse_i) {
} break;
case 4074: yyDecrement (1) yySetNT (yyNTelse_i) {
} break;
case 4075: yyDecrement (3) yySetNT (yyNTinitialize) {
} break;
case 4076: yyDecrement (2) yySetNT (yyNTreplacing_o) {
} break;
case 4077: yySetNT (yyNTreplacing_o) {
} break;
case 4078: yyDecrement (1) yySetNT (yyNTinitialize_replacing_l) {
} break;
case 4079: yyDecrement (2) yySetNT (yyNTinitialize_replacing_l) {
} break;
case 4080: yyDecrement (4) yySetNT (yyNTinitialize_replacing_e) {
} break;
case 4081: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4082: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4083: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4084: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4085: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4086: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4087: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4088: yyDecrement (1) yySetNT (yyNTreplacing_mode) {
} break;
case 4089: yyDecrement (2) yySetNT (yyNTinitiate) {
} break;
case 4090: yyDecrement (4) yySetNT (yyNTinspect) {
} break;
case 4091: yyDecrement (5) yySetNT (yyNTinspect) {
} break;
case 4092: yyDecrement (7) yySetNT (yyNTinspect) {
} break;
case 4093: yyDecrement (8) yySetNT (yyNTinspect) {
} break;
case 4094:
yy1207: yySetNT (yyNTinspect_repl) {
} break;
case 4095: yyDecrement (1) yySetNT (yyNTtallying_l) {
} break;
case 4096: yyDecrement (2) yySetNT (yyNTtallying_l) {
} break;
case 4097: yyDecrement (3) yySetNT (yyNTtallying_e) {
} break;
case 4098: yyDecrement (1) yySetNT (yyNTfor_l) {
} break;
case 4099: yyDecrement (2) yySetNT (yyNTfor_l) {
} break;
case 4100: yyDecrement (2) yySetNT (yyNTfor_e) {
} break;
case 4101:
yy1214: yyDecrement (2) yySetNT (yyNTfor_e) {
} break;
case 4102:
yy1215: yyDecrement (2) yySetNT (yyNTfor_e) {
} break;
case 4103: yyDecrement (1) yySetNT (yyNTall_leading_l) {
} break;
case 4104: yyDecrement (2) yySetNT (yyNTall_leading_l) {
} break;
case 4105: yyDecrement (2) yySetNT (yyNTall_leading_e) {
} break;
case 4106: yyDecrement (1) yySetNT (yyNTreplacing_l) {
} break;
case 4107: yyDecrement (2) yySetNT (yyNTreplacing_l) {
} break;
case 4108: yyDecrement (4) yySetNT (yyNTreplacing_e) {
} break;
case 4109: yyDecrement (2) yySetNT (yyNTreplacing_e) {
} break;
case 4110: yyDecrement (2) yySetNT (yyNTreplacing_e) {
} break;
case 4111: yyDecrement (2) yySetNT (yyNTreplacing_e) {
} break;
case 4112: yyDecrement (1) yySetNT (yyNTall_leading_first_l) {
} break;
case 4113: yyDecrement (2) yySetNT (yyNTall_leading_first_l) {
} break;
case 4114: yyDecrement (4) yySetNT (yyNTall_leading_first_e) {
} break;
case 4115: yyDecrement (3) yySetNT (yyNTbefore_after_o) {
} break;
case 4116: yyDecrement (3) yySetNT (yyNTbefore_after_o) {
} break;
case 4117: yyDecrement (6) yySetNT (yyNTbefore_after_o) {
} break;
case 4118: yyDecrement (6) yySetNT (yyNTbefore_after_o) {
} break;
case 4119: yySetNT (yyNTbefore_after_o) {
} break;
case 4120: yyDecrement (7) yySetNT (yyNTmerge) {
} break;
case 4121: yyDecrement (8) yySetNT (yyNTmerge) {
} break;
case 4122: yySetNT (yyNTsort_merge_l) {
} break;
case 4123: yyDecrement (2) yySetNT (yyNTsort_merge_l) {
} break;
case 4124: yyDecrement (4) yySetNT (yyNTsort_merge_e) {
} break;
case 4125: yyDecrement (4) yySetNT (yyNTsort_merge_e) {
} break;
case 4126: yyDecrement (3) yySetNT (yyNTsort_merge_e) {
} break;
case 4127: yyDecrement (3) yySetNT (yyNTsort_merge_e) {
} break;
case 4128: yyDecrement (4) yySetNT (yyNTsequence_o) {
} break;
case 4129: yySetNT (yyNTsequence_o) {
} break;
case 4130: yyDecrement (4) yySetNT (yyNToutput) {
} break;
case 4131: yyDecrement (4) yySetNT (yyNTmove) {
} break;
case 4132: yyDecrement (5) yySetNT (yyNTmove) {
} break;
case 4133: yyDecrement (5) yySetNT (yyNTmultiply) {
} break;
case 4134: yyDecrement (8) yySetNT (yyNTmultiply) {
} break;
case 4135: yyDecrement (6) yySetNT (yyNTmultiply) {
} break;
case 4136: yyDecrement (9) yySetNT (yyNTmultiply) {
} break;
case 4137: yyDecrement (5) yySetNT (yyNTmultiply_i) {
} break;
case 4138: yyDecrement (8) yySetNT (yyNTmultiply_i) {
} break;
case 4139: yyDecrement (6) yySetNT (yyNTmultiply_i) {
} break;
case 4140: yyDecrement (9) yySetNT (yyNTmultiply_i) {
} break;
case 4141: yyDecrement (2) yySetNT (yyNTnext_sentence) {
} break;
case 4142: yyDecrement (6) yySetNT (yyNTon) {
} break;
case 4143: yyDecrement (3) yySetNT (yyNTon_every_o) {
} break;
case 4144: yySetNT (yyNTon_every_o) {
} break;
case 4145: yyDecrement (2) yySetNT (yyNTon_until_o) {
} break;
case 4146: yySetNT (yyNTon_until_o) {
} break;
case 4147: yyDecrement (2) yySetNT (yyNTon_else_o) {
} break;
case 4148: yySetNT (yyNTon_else_o) {
} break;
case 4149: yyDecrement (2) yySetNT (yyNTopen) {
} break;
case 4150: yyDecrement (1) yySetNT (yyNTopen_l) {
} break;
case 4151: yyDecrement (2) yySetNT (yyNTopen_l) {
} break;
case 4152: yyDecrement (2) yySetNT (yyNTopen_e) {
} break;
case 4153: yyDecrement (2) yySetNT (yyNTopen_e) {
} break;
case 4154: yyDecrement (2) yySetNT (yyNTopen_e) {
} break;
case 4155: yyDecrement (2) yySetNT (yyNTopen_e) {
} break;
case 4156: yyDecrement (1) yySetNT (yyNTfile_name_1_l) {
} break;
case 4157: yyDecrement (2) yySetNT (yyNTfile_name_1_l) {
} break;
case 4158: yyDecrement (1) yySetNT (yyNTfile_name_2_l) {
} break;
case 4159: yyDecrement (2) yySetNT (yyNTfile_name_2_l) {
} break;
case 4160: yyDecrement (1) yySetNT (yyNTfile_name_3_l) {
} break;
case 4161: yyDecrement (2) yySetNT (yyNTfile_name_3_l) {
} break;
case 4162:
yy1275: yyDecrement (1) yySetNT (yyNTfile_name_1) {
} break;
case 4163: yyDecrement (2) yySetNT (yyNTfile_name_1) {
} break;
case 4164: yyDecrement (4) yySetNT (yyNTfile_name_1) {
} break;
case 4165: yyDecrement (3) yySetNT (yyNTfile_name_1) {
} break;
case 4166:
yy1279: yyDecrement (1) yySetNT (yyNTfile_name_2) {
} break;
case 4167: yyDecrement (4) yySetNT (yyNTfile_name_2) {
} break;
case 4168: yyDecrement (3) yySetNT (yyNTfile_name_2) {
} break;
case 4169:
yy1282: yyDecrement (1) yySetNT (yyNTfile_name_3) {
} break;
case 4170: yyDecrement (3) yySetNT (yyNTfile_name_3) {
} break;
case 4171: yyDecrement (1) yySetNT (yyNTfile_name) {
/* line 1935 "Parser.lrk" */
 ;
{  (void) UseName (yyA [0].Scan); ; } ;

} break;
case 4172: yyDecrement (1) yySetNT (yyNTfile_name_l) {
} break;
case 4173: yyDecrement (2) yySetNT (yyNTfile_name_l) {
} break;
case 4174:
yy1287: yyDecrement (2) yySetNT (yyNTperform) {
} break;
case 4175: yyDecrement (4) yySetNT (yyNTperform) {
} break;
case 4176: yyDecrement (5) yySetNT (yyNTperform) {
} break;
case 4177: yyDecrement (5) yySetNT (yyNTperform) {
} break;
case 4178: yyDecrement (5) yySetNT (yyNTperform) {
} break;
case 4179: yyDecrement (2) yySetNT (yyNTperform) {
} break;
case 4180: yyDecrement (4) yySetNT (yyNTperform) {
} break;
case 4181: yyDecrement (5) yySetNT (yyNTperform) {
} break;
case 4182: yyDecrement (5) yySetNT (yyNTperform) {
} break;
case 4183: yyDecrement (5) yySetNT (yyNTperform) {
} break;
case 4184: yyDecrement (2) yySetNT (yyNTperform_body) {
} break;
case 4185: yyDecrement (1) yySetNT (yyNTperform_body) {
} break;
case 4186: yyDecrement (1) yySetNT (yyNTprocedure) {
} break;
case 4187: yyDecrement (3) yySetNT (yyNTprocedure) {
} break;
case 4188: yyDecrement (3) yySetNT (yyNTtest_o) {
} break;
case 4189: yyDecrement (3) yySetNT (yyNTtest_o) {
} break;
case 4190: yySetNT (yyNTtest_o) {
} break;
case 4191: yyDecrement (1) yySetNT (yyNTvarying_l) {
} break;
case 4192: yyDecrement (3) yySetNT (yyNTvarying_l) {
} break;
case 4193: yyDecrement (7) yySetNT (yyNTvarying_e) {
} break;
case 4194: yyDecrement (2) yySetNT (yyNTpurge) {
/* line 1960 "Parser.lrk" */
 ;
{  (void) UseName (yyA [1].Scan); ; } ;

} break;
case 4195: yyDecrement (7) yySetNT (yyNTread) {
} break;
case 4196: yyDecrement (8) yySetNT (yyNTread) {
} break;
case 4197: yyDecrement (7) yySetNT (yyNTread) {
} break;
case 4198: yyDecrement (8) yySetNT (yyNTread) {
} break;
case 4199: yyDecrement (6) yySetNT (yyNTread) {
} break;
case 4200: yyDecrement (7) yySetNT (yyNTread) {
} break;
case 4201: yyDecrement (8) yySetNT (yyNTread) {
} break;
case 4202: yyDecrement (9) yySetNT (yyNTread) {
} break;
case 4203: yyDecrement (6) yySetNT (yyNTread) {
} break;
case 4204: yyDecrement (7) yySetNT (yyNTread_i) {
} break;
case 4205: yyDecrement (8) yySetNT (yyNTread_i) {
} break;
case 4206: yyDecrement (7) yySetNT (yyNTread_i) {
} break;
case 4207: yyDecrement (8) yySetNT (yyNTread_i) {
} break;
case 4208: yyDecrement (6) yySetNT (yyNTread_i) {
} break;
case 4209: yyDecrement (7) yySetNT (yyNTread_i) {
} break;
case 4210: yyDecrement (8) yySetNT (yyNTread_i) {
} break;
case 4211: yyDecrement (9) yySetNT (yyNTread_i) {
} break;
case 4212: yyDecrement (6) yySetNT (yyNTread_i) {
} break;
case 4213: yyDecrement (2) yySetNT (yyNTinto_o) {
} break;
case 4214: yySetNT (yyNTinto_o) {
} break;
case 4215: yyDecrement (2) yySetNT (yyNTwith_read_o) {
} break;
case 4216: yyDecrement (3) yySetNT (yyNTwith_read_o) {
} break;
case 4217: yyDecrement (3) yySetNT (yyNTwith_read_o) {
} break;
case 4218: yyDecrement (3) yySetNT (yyNTwith_read_o) {
} break;
case 4219: yyDecrement (2) yySetNT (yyNTwith_read_o) {
} break;
case 4220:
yy1333: yySetNT (yyNTwith_read_o) {
} break;
case 4221: yyDecrement (3) yySetNT (yyNTend) {
} break;
case 4222: yyDecrement (4) yySetNT (yyNTend) {
} break;
case 4223: yyDecrement (7) yySetNT (yyNTend) {
} break;
case 4224: yyDecrement (3) yySetNT (yyNTend_o) {
} break;
case 4225: yySetNT (yyNTend_o) {
} break;
case 4226: yyDecrement (4) yySetNT (yyNTnot_end_o) {
} break;
case 4227: yySetNT (yyNTnot_end_o) {
} break;
case 4228: yyDecrement (2) yySetNT (yyNTready_trace) {
} break;
case 4229: yyDecrement (6) yySetNT (yyNTreceive) {
} break;
case 4230: yyDecrement (7) yySetNT (yyNTreceive) {
} break;
case 4231: yyDecrement (6) yySetNT (yyNTreceive_i) {
} break;
case 4232: yyDecrement (7) yySetNT (yyNTreceive_i) {
} break;
case 4233: yyDecrement (1) yySetNT (yyNTmessage_segment) {
} break;
case 4234: yyDecrement (1) yySetNT (yyNTmessage_segment) {
} break;
case 4235: yyDecrement (3) yySetNT (yyNTdata) {
} break;
case 4236: yyDecrement (2) yySetNT (yyNTdata) {
} break;
case 4237: yyDecrement (3) yySetNT (yyNTdata) {
} break;
case 4238: yyDecrement (5) yySetNT (yyNTdata) {
} break;
case 4239: yyDecrement (6) yySetNT (yyNTdata) {
} break;
case 4240: yyDecrement (2) yySetNT (yyNTrelease) {
} break;
case 4241: yyDecrement (5) yySetNT (yyNTrelease) {
} break;
case 4242: yyDecrement (2) yySetNT (yyNTreset_trace) {
} break;
case 4243: yyDecrement (7) yySetNT (yyNTreturn) {
} break;
case 4244: yyDecrement (7) yySetNT (yyNTreturn_i) {
} break;
case 4245: yyDecrement (3) yySetNT (yyNTreturn_end) {
} break;
case 4246: yyDecrement (4) yySetNT (yyNTrewrite) {
} break;
case 4247: yyDecrement (5) yySetNT (yyNTrewrite) {
} break;
case 4248: yyDecrement (4) yySetNT (yyNTrewrite_i) {
} break;
case 4249: yyDecrement (5) yySetNT (yyNTrewrite_i) {
} break;
case 4250: yyDecrement (2) yySetNT (yyNTfrom_2_o) {
} break;
case 4251: yyDecrement (2) yySetNT (yyNTfrom_2_o) {
} break;
case 4252: yySetNT (yyNTfrom_2_o) {
} break;
case 4253: yyDecrement (1) yySetNT (yyNTrollback) {
} break;
case 4254: yyDecrement (6) yySetNT (yyNTsearch) {
} break;
case 4255: yyDecrement (8) yySetNT (yyNTsearch) {
} break;
case 4256: yyDecrement (6) yySetNT (yyNTsearch_i) {
} break;
case 4257: yyDecrement (8) yySetNT (yyNTsearch_i) {
} break;
case 4258: yyDecrement (2) yySetNT (yyNTvarying_o) {
} break;
case 4259: yySetNT (yyNTvarying_o) {
} break;
case 4260: yyDecrement (1) yySetNT (yyNTsearch_when_l) {
} break;
case 4261: yyDecrement (2) yySetNT (yyNTsearch_when_l) {
} break;
case 4262: yyDecrement (3) yySetNT (yyNTwhen_e) {
} break;
case 4263: yyDecrement (1) yySetNT (yyNTsearch_l) {
} break;
case 4264: yyDecrement (3) yySetNT (yyNTsearch_l) {
} break;
case 4265: yyDecrement (3) yySetNT (yyNTsearch_e) {
} break;
case 4266: yyDecrement (1) yySetNT (yyNTsearch_e) {
} break;
case 4267:
yy1380: yyDecrement (4) yySetNT (yyNTsend) {
} break;
case 4268: yyDecrement (6) yySetNT (yyNTsend) {
} break;
case 4269: yyDecrement (8) yySetNT (yyNTsend) {
} break;
case 4270: yyDecrement (1) yySetNT (yyNTadvancing_o) {
} break;
case 4271: yySetNT (yyNTadvancing_o) {
} break;
case 4272: yyDecrement (3) yySetNT (yyNTadvancing) {
} break;
case 4273: yyDecrement (3) yySetNT (yyNTadvancing) {
} break;
case 4274: yyDecrement (2) yySetNT (yyNTadvance) {
} break;
case 4275: yyDecrement (1) yySetNT (yyNTadvance) {
} break;
case 4276: yyDecrement (2) yySetNT (yyNTsend_replacing_o) {
} break;
case 4277: yySetNT (yyNTsend_replacing_o) {
} break;
case 4278: yyDecrement (2) yySetNT (yyNTservice) {
} break;
case 4279: yyDecrement (3) yySetNT (yyNTservice) {
} break;
case 4280: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4281: yyDecrement (7) yySetNT (yyNTset) {
} break;
case 4282: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4283: yyDecrement (7) yySetNT (yyNTset) {
} break;
case 4284: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4285: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4286: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4287: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4288: yyDecrement (6) yySetNT (yyNTset) {
} break;
case 4289: yyDecrement (4) yySetNT (yyNTset) {
} break;
case 4290: yyDecrement (4) yySetNT (yyNTset) {
} break;
case 4291: yyDecrement (4) yySetNT (yyNTset) {
} break;
case 4292: yyDecrement (5) yySetNT (yyNTset) {
} break;
case 4293: yyDecrement (1) yySetNT (yyNTset_l) {
} break;
case 4294: yyDecrement (2) yySetNT (yyNTset_l) {
} break;
case 4295: yyDecrement (3) yySetNT (yyNTset_e) {
} break;
case 4296: yyDecrement (1) yySetNT (yyNTset_e) {
} break;
case 4297: yySetNT (yyNTon_off_l) {
} break;
case 4298: yyDecrement (2) yySetNT (yyNTon_off_l) {
} break;
case 4299: yyDecrement (3) yySetNT (yyNTon_off_e) {
} break;
case 4300: yyDecrement (3) yySetNT (yyNTon_off_e) {
} break;
case 4301: yySetNT (yyNTtrue_false_l) {
} break;
case 4302: yyDecrement (2) yySetNT (yyNTtrue_false_l) {
} break;
case 4303: yyDecrement (3) yySetNT (yyNTtrue_false_e) {
} break;
case 4304: yyDecrement (3) yySetNT (yyNTtrue_false_e) {
} break;
case 4305: yyDecrement (7) yySetNT (yyNTsort) {
} break;
case 4306: yyDecrement (8) yySetNT (yyNTsort) {
} break;
case 4307: yyDecrement (8) yySetNT (yyNTsort) {
} break;
case 4308: yyDecrement (9) yySetNT (yyNTsort) {
} break;
case 4309: yyDecrement (5) yySetNT (yyNTsort) {
} break;
case 4310: yyDecrement (4) yySetNT (yyNTduplicates_o) {
} break;
case 4311:
yy1424: yySetNT (yyNTduplicates_o) {
} break;
case 4312: yyDecrement (4) yySetNT (yyNTinput) {
} break;
case 4313: yyDecrement (4) yySetNT (yyNTstart) {
} break;
case 4314: yyDecrement (5) yySetNT (yyNTstart) {
} break;
case 4315: yyDecrement (4) yySetNT (yyNTstart_i) {
} break;
case 4316: yyDecrement (5) yySetNT (yyNTstart_i) {
} break;
case 4317:
yy1430: yyDecrement (3) yySetNT (yyNTkey_o) {
} break;
case 4318: yyDecrement (6) yySetNT (yyNTkey_o) {
} break;
case 4319: yySetNT (yyNTkey_o) {
} break;
case 4320: yyDecrement (1) yySetNT (yyNTstart_operator) {
} break;
case 4321: yyDecrement (1) yySetNT (yyNTstart_operator) {
} break;
case 4322: yyDecrement (1) yySetNT (yyNTstart_operator) {
} break;
case 4323: yyDecrement (1) yySetNT (yyNTstart_operator) {
} break;
case 4324: yyDecrement (1) yySetNT (yyNTstart_operator) {
} break;
case 4325: yyDecrement (2) yySetNT (yyNTstop) {
} break;
case 4326: yyDecrement (3) yySetNT (yyNTstop) {
} break;
case 4327: yyDecrement (3) yySetNT (yyNTstop) {
} break;
case 4328: yyDecrement (2) yySetNT (yyNTstop) {
} break;
case 4329: yyDecrement (2) yySetNT (yyNTstop) {
} break;
case 4330: yyDecrement (6) yySetNT (yyNTstring_v) {
} break;
case 4331: yyDecrement (7) yySetNT (yyNTstring_v) {
} break;
case 4332: yyDecrement (6) yySetNT (yyNTstring_i) {
} break;
case 4333: yyDecrement (7) yySetNT (yyNTstring_i) {
} break;
case 4334: yyDecrement (1) yySetNT (yyNTstring_l) {
} break;
case 4335: yyDecrement (2) yySetNT (yyNTstring_l) {
} break;
case 4336: yyDecrement (4) yySetNT (yyNTstring_e) {
} break;
case 4337: yyDecrement (1) yySetNT (yyNTdelimiter) {
} break;
case 4338: yyDecrement (1) yySetNT (yyNTdelimiter) {
} break;
case 4339: yyDecrement (3) yySetNT (yyNTpointer_o) {
} break;
case 4340:
yy1453: yySetNT (yyNTpointer_o) {
} break;
case 4341: yyDecrement (5) yySetNT (yyNTsubtract) {
} break;
case 4342: yyDecrement (8) yySetNT (yyNTsubtract) {
} break;
case 4343: yyDecrement (6) yySetNT (yyNTsubtract) {
} break;
case 4344: yyDecrement (6) yySetNT (yyNTsubtract) {
} break;
case 4345: yyDecrement (9) yySetNT (yyNTsubtract) {
} break;
case 4346: yyDecrement (7) yySetNT (yyNTsubtract) {
} break;
case 4347: yyDecrement (5) yySetNT (yyNTsubtract_i) {
} break;
case 4348: yyDecrement (8) yySetNT (yyNTsubtract_i) {
} break;
case 4349: yyDecrement (6) yySetNT (yyNTsubtract_i) {
} break;
case 4350: yyDecrement (6) yySetNT (yyNTsubtract_i) {
} break;
case 4351: yyDecrement (9) yySetNT (yyNTsubtract_i) {
} break;
case 4352: yyDecrement (7) yySetNT (yyNTsubtract_i) {
} break;
case 4353: yyDecrement (2) yySetNT (yyNTsuppress) {
} break;
case 4354: yyDecrement (2) yySetNT (yyNTterminate) {
} break;
case 4355: yyDecrement (7) yySetNT (yyNTtransform) {
} break;
case 4356: yyDecrement (3) yySetNT (yyNTunlock) {
} break;
case 4357: yyDecrement (3) yySetNT (yyNTunlock) {
} break;
case 4358: yyDecrement (8) yySetNT (yyNTunstring) {
} break;
case 4359: yyDecrement (9) yySetNT (yyNTunstring) {
} break;
case 4360: yyDecrement (8) yySetNT (yyNTunstring_i) {
} break;
case 4361: yyDecrement (9) yySetNT (yyNTunstring_i) {
} break;
case 4362: yyDecrement (3) yySetNT (yyNTdelimited_o) {
} break;
case 4363: yySetNT (yyNTdelimited_o) {
} break;
case 4364: yyDecrement (1) yySetNT (yyNTdelimited_l) {
} break;
case 4365: yyDecrement (3) yySetNT (yyNTdelimited_l) {
} break;
case 4366: yyDecrement (2) yySetNT (yyNTdelimited_e) {
} break;
case 4367: yyDecrement (1) yySetNT (yyNTdelimited_e) {
} break;
case 4368: yyDecrement (3) yySetNT (yyNTtallying_o) {
} break;
case 4369: yySetNT (yyNTtallying_o) {
} break;
case 4370: yyDecrement (1) yySetNT (yyNTunstring_l) {
} break;
case 4371: yyDecrement (2) yySetNT (yyNTunstring_l) {
} break;
case 4372: yyDecrement (1) yySetNT (yyNTunstring_e) {
} break;
case 4373: yyDecrement (4) yySetNT (yyNTunstring_e) {
} break;
case 4374: yyDecrement (4) yySetNT (yyNTunstring_e) {
} break;
case 4375: yyDecrement (7) yySetNT (yyNTunstring_e) {
} break;
case 4376: yyDecrement (8) yySetNT (yyNTuse) {
} break;
case 4377: yyDecrement (9) yySetNT (yyNTuse) {
} break;
case 4378: yyDecrement (9) yySetNT (yyNTuse) {
} break;
case 4379: yyDecrement (5) yySetNT (yyNTuse) {
} break;
case 4380: yyDecrement (5) yySetNT (yyNTuse) {
} break;
case 4381: yyDecrement (1) yySetNT (yyNTexception_or_error) {
} break;
case 4382: yyDecrement (1) yySetNT (yyNTexception_or_error) {
} break;
case 4383: yyDecrement (1) yySetNT (yyNTbegin_or_end) {
} break;
case 4384: yyDecrement (1) yySetNT (yyNTbegin_or_end) {
} break;
case 4385: yyDecrement (1) yySetNT (yyNTuse_files) {
} break;
case 4386: yyDecrement (1) yySetNT (yyNTuse_files) {
} break;
case 4387: yyDecrement (1) yySetNT (yyNTuse_files) {
} break;
case 4388: yyDecrement (1) yySetNT (yyNTuse_files) {
} break;
case 4389: yyDecrement (1) yySetNT (yyNTuse_files) {
} break;
case 4390: yyDecrement (2) yySetNT (yyNTgiving_use_o) {
} break;
case 4391: yySetNT (yyNTgiving_use_o) {
} break;
case 4392: yyDecrement (1) yySetNT (yyNTfile_or_reel) {
} break;
case 4393: yyDecrement (1) yySetNT (yyNTfile_or_reel) {
} break;
case 4394: yySetNT (yyNTfile_or_reel) {
} break;
case 4395: yyDecrement (1) yySetNT (yyNTuse_l) {
} break;
case 4396: yyDecrement (2) yySetNT (yyNTuse_l) {
} break;
case 4397: yyDecrement (1) yySetNT (yyNTuse_e) {
} break;
case 4398: yyDecrement (4) yySetNT (yyNTuse_e) {
} break;
case 4399: yyDecrement (1) yySetNT (yyNTuse_e) {
} break;
case 4400: yyDecrement (3) yySetNT (yyNTuse_e) {
} break;
case 4401: yyDecrement (1) yySetNT (yyNTuse_e) {
} break;
case 4402: yyDecrement (3) yySetNT (yyNTuse_e) {
} break;
case 4403: yyDecrement (2) yySetNT (yyNTuse_e) {
} break;
case 4404: yyDecrement (5) yySetNT (yyNTwrite) {
} break;
case 4405: yyDecrement (4) yySetNT (yyNTwrite) {
} break;
case 4406: yyDecrement (8) yySetNT (yyNTwrite) {
} break;
case 4407: yyDecrement (6) yySetNT (yyNTwrite) {
} break;
case 4408: yyDecrement (5) yySetNT (yyNTwrite) {
} break;
case 4409: yyDecrement (9) yySetNT (yyNTwrite) {
} break;
case 4410: yyDecrement (5) yySetNT (yyNTwrite) {
} break;
case 4411: yyDecrement (5) yySetNT (yyNTwrite_i) {
} break;
case 4412: yyDecrement (4) yySetNT (yyNTwrite_i) {
} break;
case 4413: yyDecrement (8) yySetNT (yyNTwrite_i) {
} break;
case 4414: yyDecrement (6) yySetNT (yyNTwrite_i) {
} break;
case 4415: yyDecrement (5) yySetNT (yyNTwrite_i) {
} break;
case 4416: yyDecrement (9) yySetNT (yyNTwrite_i) {
} break;
case 4417: yyDecrement (5) yySetNT (yyNTwrite_i) {
} break;
case 4418: yyDecrement (3) yySetNT (yyNTend_of_page) {
} break;
case 4419: yyDecrement (4) yySetNT (yyNTend_of_page) {
} break;
case 4420: yyDecrement (7) yySetNT (yyNTend_of_page) {
} break;
case 4421: yySetNT (yyNTcopy_o) {
} break;
case 4422: yyDecrement (2) yySetNT (yyNTcopy_o) {
} break;
case 4423: yyDecrement (2) yySetNT (yyNTcopy_or_replace) {
} break;
case 4424: yyDecrement (2) yySetNT (yyNTcopy_or_replace) {
} break;
case 4425: yyDecrement (4) yySetNT (yyNTcopy) {
/* line 2202 "Parser.lrk" */
 ;
{  (void) Copy (yyA [1].copy_name.Ident, yyA [1].copy_name.Pos); ; } ;

} break;
case 4426: yyDecrement (6) yySetNT (yyNTcopy) {
/* line 2205 "Parser.lrk" */
 ;
{  (void) Copy (yyA [1].copy_name.Ident, yyA [1].copy_name.Pos); ; } ;

} break;
case 4427: yyDecrement (1) yySetNT (yyNTcopy_name) {
/* line 2208 "Parser.lrk" */
yyS.copy_name.Ident = yyA [0].Scan.name.Ident;
 yyS.copy_name.Pos  =  yyA [0].Scan.Position;
 ;

} break;
case 4428: yyDecrement (1) yySetNT (yyNTcopy_name) {
/* line 2212 "Parser.lrk" */
 yyS.copy_name.Pos  =  yyA [0].Scan.Position;
 {
			      char word [128];
			      StGetString (yyA [0].Scan.string.Value, word);
			      yyS.copy_name.Ident = MakeIdent (& word [1], strlen (word) - 2);
			   } ;
 ;

} break;
case 4429: yyDecrement (1) yySetNT (yyNTcopy_suppress_o) {
} break;
case 4430: yySetNT (yyNTcopy_suppress_o) {
} break;
case 4431: yyDecrement (3) yySetNT (yyNTcopy_replacing_o) {
/* line 2222 "Parser.lrk" */
 ;
{  end_replacing (); ; } ;

} break;
case 4432: yySetNT (yyNTcopy_replacing_o) {
} break;
case 4433: yySetNT (yyNTxx_copy_replacing_o_1_1) {
/* line 2225 "Parser.lrk" */
 ;
{  begin_replacing (); ; } ;

} break;
case 4434: yyDecrement (1) yySetNT (yyNTcopy_replacing_l) {
} break;
case 4435: yyDecrement (2) yySetNT (yyNTcopy_replacing_l) {
} break;
case 4436: yyDecrement (3) yySetNT (yyNTcopy_replacing_e) {
} break;
case 4437: yyDecrement (3) yySetNT (yyNTreplace) {
/* line 2232 "Parser.lrk" */
 ;
{  end_replacing (); ; } ;

} break;
case 4438: yyDecrement (2) yySetNT (yyNTreplace) {
} break;
case 4439: yySetNT (yyNTxx_replace_1_2) {
/* line 2235 "Parser.lrk" */
 ;
{  begin_replacing (); ; } ;

} break;
case 4440: yyDecrement (1) yySetNT (yyNTreplace_l) {
} break;
case 4441: yyDecrement (2) yySetNT (yyNTreplace_l) {
} break;
case 4442: yyDecrement (3) yySetNT (yyNTreplace_e) {
} break;
case 4443: yyDecrement (1) yySetNT (yyNTreplacing_item_1) {
} break;
case 4444: yyDecrement (3) yySetNT (yyNTreplacing_item_1) {
} break;
case 4445: yyDecrement (3) yySetNT (yyNTpseudo_text_1) {
} break;
case 4446: yyDecrement (1) yySetNT (yyNTreplacing_item_2) {
} break;
case 4447: yyDecrement (4) yySetNT (yyNTreplacing_item_2) {
} break;
case 4448: yySetNT (yyNTxx_replacing_item_2_2_1) {
/* line 2247 "Parser.lrk" */
 ;
{  start_pseudo_text (); ; } ;

} break;
case 4449: yyDecrement (4) yySetNT (yyNTpseudo_text_2) {
} break;
case 4450: yyDecrement (1) yySetNT (yyNTreplacing_item) {
} break;
case 4451: yyDecrement (3) yySetNT (yyNTreplacing_item) {
} break;
case 4452: yyDecrement (3) yySetNT (yyNTreplacing_item) {
} break;
case 4453: yyDecrement (1) yySetNT (yyNTreplacing_item) {
} break;
case 4454: yyDecrement (1) yySetNT (yyNTtoken_l) {
} break;
case 4455: yyDecrement (2) yySetNT (yyNTtoken_l) {
} break;
case 4456: yyDecrement (1) yySetNT (yyNTtoken_e) {
} break;
case 4457: yyDecrement (1) yySetNT (yyNTtoken_e) {
} break;
case 4458: yyDecrement (1) yySetNT (yyNTtoken_e) {
} break;
case 4459:
yy1572: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4460: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4461: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4462: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4463: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4464: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4465: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4466: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4467: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4468: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4469: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4470: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4471: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4472: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4473: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4474: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4475: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4476: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4477: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4478: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4479: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4480: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4481: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4482: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4483: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4484: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4485: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 4486: yyDecrement (1) yySetNT (yyNTcondition) {
} break;
case 4487: yyDecrement (3) yySetNT (yyNTcondition) {
} break;
case 4488: yyDecrement (1) yySetNT (yyNTand_condition) {
} break;
case 4489: yyDecrement (3) yySetNT (yyNTand_condition) {
} break;
case 4490: yyDecrement (1) yySetNT (yyNTnot_condition) {
} break;
case 4491: yyDecrement (2) yySetNT (yyNTnot_condition) {
} break;
case 4492: yyDecrement (3) yySetNT (yyNTprimary_condition) {
} break;
case 4493: yyDecrement (2) yySetNT (yyNTprimary_condition) {
} break;
case 4494: yyDecrement (4) yySetNT (yyNTprimary_condition) {
} break;
case 4495: yyDecrement (6) yySetNT (yyNTprimary_condition) {
} break;
case 4496: yyDecrement (3) yySetNT (yyNTprimary_condition) {
} break;
case 4497: yyDecrement (4) yySetNT (yyNTprimary_condition) {
} break;
case 4498:
yy1611: yyDecrement (1) yySetNT (yyNTprimary_condition) {
} break;
case 4499: yyDecrement (3) yySetNT (yyNTprimary_condition) {
} break;
case 4500: yyDecrement (4) yySetNT (yyNTprimary_condition) {
} break;
case 4501: yyDecrement (5) yySetNT (yyNTprimary_condition) {
} break;
case 4502: yyDecrement (3) yySetNT (yyNTprimary_condition) {
} break;
case 4503: yyDecrement (3) yySetNT (yyNTprimary_condition) {
} break;
case 4504: yyDecrement (1) yySetNT (yyNTclassification) {
} break;
case 4505: yyDecrement (1) yySetNT (yyNTclassification) {
} break;
case 4506: yyDecrement (1) yySetNT (yyNTclassification) {
} break;
case 4507: yyDecrement (1) yySetNT (yyNTclassification) {
} break;
case 4508: yyDecrement (1) yySetNT (yyNTsign_3) {
} break;
case 4509: yyDecrement (1) yySetNT (yyNTsign_3) {
} break;
case 4510: yyDecrement (1) yySetNT (yyNTsign_3) {
} break;
case 4511: yyDecrement (1) yySetNT (yyNTsign_3) {
} break;
case 4512: yyDecrement (3) yySetNT (yyNTpointer_operand) {
} break;
case 4513: yyDecrement (1) yySetNT (yyNTpointer_operand) {
} break;
case 4514: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4515: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4516: yyDecrement (1) yySetNT (yyNTrelational_operator) {
} break;
case 4517: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4518: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4519: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4520: yyDecrement (2) yySetNT (yyNTrelational_operator) {
} break;
case 4521: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4522: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4523: yyDecrement (1) yySetNT (yyNTrelational_operator) {
} break;
case 4524: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4525: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4526: yyDecrement (6) yySetNT (yyNTrelational_operator) {
} break;
case 4527: yyDecrement (2) yySetNT (yyNTrelational_operator) {
} break;
case 4528: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4529: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4530: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4531: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4532: yyDecrement (6) yySetNT (yyNTrelational_operator) {
} break;
case 4533: yyDecrement (2) yySetNT (yyNTrelational_operator) {
} break;
case 4534: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4535: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4536: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4537: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4538: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4539: yyDecrement (2) yySetNT (yyNTrelational_operator) {
} break;
case 4540: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4541: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4542: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4543: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4544: yyDecrement (6) yySetNT (yyNTrelational_operator) {
} break;
case 4545: yyDecrement (2) yySetNT (yyNTrelational_operator) {
} break;
case 4546: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4547: yyDecrement (3) yySetNT (yyNTrelational_operator) {
} break;
case 4548: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4549: yyDecrement (4) yySetNT (yyNTrelational_operator) {
} break;
case 4550: yyDecrement (6) yySetNT (yyNTrelational_operator) {
} break;
case 4551: yyDecrement (2) yySetNT (yyNTrelational_operator) {
} break;
case 4552: yyDecrement (3) yySetNT (yyNTequal) {
} break;
case 4553: yyDecrement (3) yySetNT (yyNTequal) {
} break;
case 4554: yyDecrement (1) yySetNT (yyNTequal) {
} break;
case 4555: yyDecrement (3) yySetNT (yyNTgreater) {
} break;
case 4556: yyDecrement (3) yySetNT (yyNTgreater) {
} break;
case 4557: yyDecrement (1) yySetNT (yyNTgreater) {
} break;
case 4558: yyDecrement (4) yySetNT (yyNTgreater_equal) {
} break;
case 4559: yyDecrement (4) yySetNT (yyNTgreater_equal) {
} break;
case 4560: yyDecrement (6) yySetNT (yyNTgreater_equal) {
} break;
case 4561: yyDecrement (2) yySetNT (yyNTgreater_equal) {
} break;
case 4562: yyDecrement (3) yySetNT (yyNTless) {
} break;
case 4563: yyDecrement (3) yySetNT (yyNTless) {
} break;
case 4564: yyDecrement (4) yySetNT (yyNTless_equal) {
} break;
case 4565: yyDecrement (4) yySetNT (yyNTless_equal) {
} break;
case 4566: yyDecrement (6) yySetNT (yyNTless_equal) {
} break;
case 4567: yyDecrement (2) yySetNT (yyNTless_equal) {
} break;
case 4568: yyDecrement (2) yySetNT (yyNTis_relational_operator) {
} break;
case 4569: yyDecrement (2) yySetNT (yyNTis_relational_operator) {
} break;
case 4570: yyDecrement (3) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4571: yyDecrement (3) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4572: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4573: yyDecrement (1) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4574: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4575: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4576: yyDecrement (1) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4577: yyDecrement (3) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4578: yyDecrement (3) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4579: yyDecrement (5) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4580: yyDecrement (1) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4581: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4582: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4583: yyDecrement (1) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4584: yyDecrement (3) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4585: yyDecrement (3) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4586: yyDecrement (5) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4587: yyDecrement (1) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4588: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4589: yyDecrement (2) yySetNT (yyNTno_is_relational_operator) {
} break;
case 4590: yyDecrement (1) yySetNT (yyNTrelational_operator_2) {
} break;
case 4591: yyDecrement (1) yySetNT (yyNTrelational_operator_2) {
} break;
case 4592: yyDecrement (1) yySetNT (yyNTexpression) {
} break;
case 4593: yyDecrement (3) yySetNT (yyNTexpression) {
} break;
case 4594: yyDecrement (3) yySetNT (yyNTexpression) {
} break;
case 4595: yyDecrement (1) yySetNT (yyNTmultiplicative_expression) {
} break;
case 4596: yyDecrement (3) yySetNT (yyNTmultiplicative_expression) {
} break;
case 4597: yyDecrement (3) yySetNT (yyNTmultiplicative_expression) {
} break;
case 4598: yyDecrement (1) yySetNT (yyNTpower_expression) {
} break;
case 4599: yyDecrement (3) yySetNT (yyNTpower_expression) {
} break;
case 4600: yyDecrement (2) yySetNT (yyNTunary_expression) {
} break;
case 4601: yyDecrement (2) yySetNT (yyNTunary_expression) {
} break;
case 4602: yyDecrement (1) yySetNT (yyNTunary_expression) {
} break;
case 4603: yyDecrement (1) yySetNT (yyNTprimary_expression) {
} break;
case 4604: yyDecrement (1) yySetNT (yyNTprimary_expression) {
} break;
case 4605: yyDecrement (3) yySetNT (yyNTprimary_expression) {
} break;
case 4606: yyDecrement (3) yySetNT (yyNTprimary_expression) {
} break;
case 4607:
yy1720: yyDecrement (1) yySetNT (yyNTidentifier) {
} break;
case 4608: yyDecrement (5) yySetNT (yyNTidentifier) {
} break;
case 4609:
yy1722: yyDecrement (5) yySetNT (yyNTidentifier) {
} break;
case 4610: yyDecrement (2) yySetNT (yyNTidentifier) {
} break;
case 4611: yyDecrement (6) yySetNT (yyNTidentifier) {
} break;
case 4612: yyDecrement (1) yySetNT (yyNTidentifier) {
} break;
case 4613: yyDecrement (1) yySetNT (yyNTsubscription) {
} break;
case 4614: yyDecrement (5) yySetNT (yyNTsubscription) {
} break;
case 4615: yyDecrement (5) yySetNT (yyNTsubscription) {
} break;
case 4616: yyDecrement (1) yySetNT (yyNTqualification) {
} if (yyControl.yyActions) {
/* line 2417 "Parser.lrk" */
 yyS.qualification.Scan = yyA [0].Scan;
 yyS.qualification.decl = UseName (yyA [0].Scan);
 ;

} break;
case 4617: yyDecrement (3) yySetNT (yyNTqualification) {
} if (yyControl.yyActions) {
/* line 2421 "Parser.lrk" */
 yyS.qualification.Scan = yyA [0].Scan;

		  yyS.qualification.decl = yyA [2].qualification.decl ? UseField (yyA [0].Scan, yyA [2].qualification.decl->fields) : NULL;
 ;

} break;
case 4618: yyDecrement (1) yySetNT (yyNTqualification_f) {
} break;
case 4619: yyDecrement (3) yySetNT (yyNTqualification_f) {
/* line 2427 "Parser.lrk" */
 ;
{  UseFieldForward (yyA [0].Scan); ; } ;

} break;
case 4620: yyDecrement (1) yySetNT (yyNTidentifier_w) {
} break;
case 4621: yyDecrement (5) yySetNT (yyNTidentifier_w) {
} break;
case 4622: yyDecrement (2) yySetNT (yyNTidentifier_w) {
} break;
case 4623: yyDecrement (6) yySetNT (yyNTidentifier_w) {
} break;
case 4624: yyDecrement (1) yySetNT (yyNTsubscription_w) {
} break;
case 4625: yyDecrement (5) yySetNT (yyNTsubscription_w) {
} break;
case 4626: yyDecrement (2) yySetNT (yyNTqualification_w) {
} if (yyControl.yyActions) {
/* line 2436 "Parser.lrk" */
 ;
{  acc = PAF_REF_READ; ; } ;

} break;
case 4627:
yy1740: yySetNT (yyNTxx_qualification_w_1_1) {
} if (yyControl.yyActions) {
/* line 2439 "Parser.lrk" */
 ;
{  acc = PAF_REF_WRITE; ; } ;

} break;
case 4628: yyDecrement (1) yySetNT (yyNTidentifier_c) {
} break;
case 4629: yyDecrement (5) yySetNT (yyNTidentifier_c) {
} break;
case 4630: yyDecrement (2) yySetNT (yyNTidentifier_c) {
} break;
case 4631: yyDecrement (6) yySetNT (yyNTidentifier_c) {
} break;
case 4632: yyDecrement (1) yySetNT (yyNTqualification_c) {
} break;
case 4633:
yy1746: yyDecrement (1) yySetNT (yyNTidentifier_n) {
} break;
case 4634:
yy1747: yyDecrement (5) yySetNT (yyNTidentifier_n) {
} break;
case 4635: yyDecrement (2) yySetNT (yyNTidentifier_n) {
} break;
case 4636: yyDecrement (6) yySetNT (yyNTidentifier_n) {
} break;
case 4637:
yy1750: yyDecrement (1) yySetNT (yyNTqualification_n) {
/* line 2451 "Parser.lrk" */
 yyS.qualification_n.Scan = yyA [0].Scan;
 ;

} break;
case 4638: yyDecrement (3) yySetNT (yyNTqualification_n) {
/* line 2454 "Parser.lrk" */
 yyS.qualification_n.Scan = yyA [0].Scan;
 ;

} break;
case 4639: yyDecrement (1) yySetNT (yyNTin_of) {
} break;
case 4640: yyDecrement (1) yySetNT (yyNTin_of) {
} break;
case 4641: yySetNT (yyNTindex_l) {
} break;
case 4642: yyDecrement (2) yySetNT (yyNTindex_l) {
} break;
case 4643: yyDecrement (2) yySetNT (yyNTindex_l) {
} break;
case 4644: yyDecrement (1) yySetNT (yyNTindex) {
} break;
case 4645: yyDecrement (1) yySetNT (yyNTindex) {
} break;
case 4646: yyDecrement (3) yySetNT (yyNTindex) {
} break;
case 4647: yyDecrement (3) yySetNT (yyNTindex) {
} break;
case 4648: yyDecrement (4) yySetNT (yyNTreference_modifier) {
} break;
case 4649: yyDecrement (5) yySetNT (yyNTreference_modifier) {
} break;
case 4650: yyDecrement (1) yySetNT (yyNTidentifier_l) {
} break;
case 4651: yyDecrement (2) yySetNT (yyNTidentifier_l) {
} break;
case 4652: yyDecrement (1) yySetNT (yyNTidentifier_l_w) {
} break;
case 4653: yyDecrement (2) yySetNT (yyNTidentifier_l_w) {
} break;
case 4654: yyDecrement (1) yySetNT (yyNTidentifier_l_c) {
} break;
case 4655: yyDecrement (2) yySetNT (yyNTidentifier_l_c) {
} break;
case 4656: yyDecrement (1) yySetNT (yyNTqualification_l) {
} break;
case 4657: yyDecrement (2) yySetNT (yyNTqualification_l) {
} break;
case 4658: yyDecrement (1) yySetNT (yyNTqualification_l_f) {
} break;
case 4659: yyDecrement (2) yySetNT (yyNTqualification_l_f) {
} break;
case 4660: yyDecrement (1) yySetNT (yyNTname_l) {
} break;
case 4661: yyDecrement (2) yySetNT (yyNTname_l) {
} break;
case 4662: yyDecrement (1) yySetNT (yyNTname_l_f) {
} break;
case 4663: yyDecrement (2) yySetNT (yyNTname_l_f) {
} break;
case 4664: yyDecrement (1) yySetNT (yyNTidentifier_or_numeric_literal_l) {
} break;
case 4665: yyDecrement (2) yySetNT (yyNTidentifier_or_numeric_literal_l) {
} break;
case 4666: yyDecrement (1) yySetNT (yyNTidentifier_or_non_numeric_literal_l) {
} break;
case 4667: yyDecrement (2) yySetNT (yyNTidentifier_or_non_numeric_literal_l) {
} break;
case 4668: yyDecrement (1) yySetNT (yyNTidentifier_rounded_l_w) {
} break;
case 4669: yyDecrement (2) yySetNT (yyNTidentifier_rounded_l_w) {
} break;
case 4670:
yy1783: yyDecrement (2) yySetNT (yyNTfunction_call) {
} break;
case 4671:
yy1784: yyDecrement (5) yySetNT (yyNTfunction_call) {
} break;
case 4672: yyDecrement (3) yySetNT (yyNTfunction_call) {
} break;
case 4673: yyDecrement (6) yySetNT (yyNTfunction_call) {
} break;
case 4674:
yy1787: yyDecrement (1) yySetNT (yyNTfunction_name_1) {
} break;
case 4675:
yy1788: yyDecrement (1) yySetNT (yyNTfunction_name_2) {
} break;
case 4676: yyDecrement (1) yySetNT (yyNTfunction_name_2) {
} break;
case 4677: yyDecrement (1) yySetNT (yyNTargument_l) {
} break;
case 4678: yyDecrement (2) yySetNT (yyNTargument_l) {
} break;
case 4679: yyDecrement (1) yySetNT (yyNTsymbolic_character) {
} break;
case 4680: yyDecrement (1) yySetNT (yyNTcondition_name) {
} break;
case 4681: yyDecrement (2) yySetNT (yyNTidentifier_rounded_w) {
} break;
case 4682: yyDecrement (2) yySetNT (yyNTidentifier_rounded_c) {
} break;
case 4683: yyDecrement (1) yySetNT (yyNTidentifier_or_literal) {
} break;
case 4684: yyDecrement (1) yySetNT (yyNTidentifier_or_literal) {
} break;
case 4685: yyDecrement (3) yySetNT (yyNTidentifier_or_literal) {
} break;
case 4686: yyDecrement (1) yySetNT (yyNTidentifier_or_numeric_literal) {
} break;
case 4687: yyDecrement (1) yySetNT (yyNTidentifier_or_numeric_literal) {
} break;
case 4688: yyDecrement (3) yySetNT (yyNTidentifier_or_numeric_literal) {
} break;
case 4689: yyDecrement (1) yySetNT (yyNTidentifier_or_non_numeric_literal) {
} break;
case 4690: yyDecrement (1) yySetNT (yyNTidentifier_or_non_numeric_literal) {
} break;
case 4691: yyDecrement (1) yySetNT (yyNTidentifier_or_non_numeric_literal) {
} break;
case 4692: yyDecrement (1) yySetNT (yyNTidentifier_or_non_all_literal) {
} break;
case 4693: yyDecrement (1) yySetNT (yyNTidentifier_or_non_all_literal) {
} break;
case 4694: yyDecrement (1) yySetNT (yyNTidentifier_or_non_all_literal) {
} break;
case 4695: yyDecrement (1) yySetNT (yyNTname_or_literal) {
} break;
case 4696: yyDecrement (1) yySetNT (yyNTname_or_literal) {
} break;
case 4697: yyDecrement (1) yySetNT (yyNTidentifier_or_integer) {
} break;
case 4698: yyDecrement (1) yySetNT (yyNTidentifier_or_integer) {
} break;
case 4699: yyDecrement (1) yySetNT (yyNTname_) {
} if (yyControl.yyActions) {
/* line 2517 "Parser.lrk" */
 ;
{  (void) UseName (yyA [0].Scan); ; } ;

} break;
case 4700: yyDecrement (1) yySetNT (yyNTname_f) {
/* line 2520 "Parser.lrk" */
 ;
{  UseForward (yyA [0].Scan); ; } ;

} break;
case 4701: yyDecrement (1) yySetNT (yyNTprocedure_name) {
/* line 2523 "Parser.lrk" */
 ;
{  UseLabel (yyA [0].chapter_name.Scan); ; } ;

} break;
case 4702: yyDecrement (3) yySetNT (yyNTprocedure_name) {
/* line 2526 "Parser.lrk" */
 ;
{  UseLabel2 (yyA [0].chapter_name.Scan, yyA [2].chapter_name.Scan); ; } ;

} break;
case 4703:
yy1816: yyDecrement (1) yySetNT (yyNTchapter_name) {
/* line 2529 "Parser.lrk" */
 yyS.chapter_name.Scan = yyA [0].Scan;
 ;

} break;
case 4704: yyDecrement (1) yySetNT (yyNTchapter_name) {
/* line 2532 "Parser.lrk" */
 yyS.chapter_name.Scan = yyA [0].Scan;
 ;

} break;
case 4705: yyDecrement (1) yySetNT (yyNTchapter_name) {
/* line 2535 "Parser.lrk" */
 { char word [128];
			yyS.chapter_name.Scan = yyA [0].u_integer.Scan;
			(void) sprintf (word, "%ld", yyA [0].u_integer.Value);
			yyS.chapter_name.Scan.name.Ident = MakeIdent (word, strlen (word)); } ;
 ;

} break;
case 4706: yyDecrement (1) yySetNT (yyNTinteger) {
} if (yyControl.yyActions) {
/* line 2541 "Parser.lrk" */
yyS.integer.Value = yyA [0].Scan.plus_integer.Value;

} break;
case 4707:
yy1820: yyDecrement (1) yySetNT (yyNTinteger) {
} if (yyControl.yyActions) {
/* line 2543 "Parser.lrk" */
yyS.integer.Value = yyA [0].u_integer.Value;

} break;
case 4708: yyDecrement (1) yySetNT (yyNTinteger) {
} if (yyControl.yyActions) {
/* line 2545 "Parser.lrk" */
yyS.integer.Value = yyA [0].Scan.minus_integer.Value;

} break;
case 4709: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 2547 "Parser.lrk" */
yyS.u_integer.Value = yyA [0].Scan.unsigned_integer.Value;
 yyS.u_integer.Scan = yyA [0].Scan	;
 ;

} break;
case 4710: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 2551 "Parser.lrk" */
yyS.u_integer.Value = yyA [0].Scan.level_number.Value;
 yyS.u_integer.Scan = yyA [0].Scan	;
 ;

} break;
case 4711: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 2555 "Parser.lrk" */
 yyS.u_integer.Value = 66;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 4712: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 2559 "Parser.lrk" */
 yyS.u_integer.Value = 77;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 4713: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 2563 "Parser.lrk" */
 yyS.u_integer.Value = 78;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 4714: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 2567 "Parser.lrk" */
 yyS.u_integer.Value = 88;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 4715: yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 4716: yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 4717: yyDecrement (1) yySetNT (yyNTnon_figurative_literal) {
} break;
case 4718: yyDecrement (1) yySetNT (yyNTnon_figurative_literal) {
} break;
case 4719: yyDecrement (1) yySetNT (yyNTnon_numeric_literal) {
} break;
case 4720: yyDecrement (1) yySetNT (yyNTnon_numeric_literal) {
} break;
case 4721: yyDecrement (1) yySetNT (yyNTnon_numeric_literal) {
} break;
case 4722: yyDecrement (3) yySetNT (yyNTconcat_expression) {
} break;
case 4723: yyDecrement (3) yySetNT (yyNTconcat_expression) {
} break;
case 4724: yyDecrement (1) yySetNT (yyNTconcat_operand) {
} break;
case 4725: yyDecrement (1) yySetNT (yyNTconcat_operand) {
} break;
case 4726: yyDecrement (1) yySetNT (yyNTconcat_operand) {
} break;
case 4727: yyDecrement (1) yySetNT (yyNTnumeric_literal) {
} break;
case 4728: yyDecrement (1) yySetNT (yyNTnumeric_literal) {
} break;
case 4729: yyDecrement (1) yySetNT (yyNTfigurative_non_numeric_literal) {
} break;
case 4730: yyDecrement (1) yySetNT (yyNTfigurative_non_numeric_literal) {
} break;
case 4731: yyDecrement (1) yySetNT (yyNTfigurative_numeric_literal) {
} break;
case 4732: yyDecrement (1) yySetNT (yyNTfigurative_numeric_literal) {
} break;
case 4733: yyDecrement (1) yySetNT (yyNTnon_figurative_non_numeric_literal) {
} break;
case 4734: yyDecrement (1) yySetNT (yyNTnon_figurative_numeric_literal) {
} break;
case 4735: yyDecrement (1) yySetNT (yyNTnon_figurative_numeric_literal) {
} break;
case 4736: yyDecrement (1) yySetNT (yyNTnon_all_figurative_literal) {
} break;
case 4737: yyDecrement (1) yySetNT (yyNTnon_all_figurative_literal) {
} break;
case 4738: yyDecrement (1) yySetNT (yyNTnon_all_figurative_non_numeric_literal) {
} break;
case 4739: yyDecrement (1) yySetNT (yyNTnon_all_figurative_non_numeric_literal) {
} break;
case 4740: yyDecrement (1) yySetNT (yyNTnon_all_figurative_non_numeric_literal) {
} break;
case 4741: yyDecrement (1) yySetNT (yyNTnon_all_figurative_non_numeric_literal) {
} break;
case 4742: yyDecrement (1) yySetNT (yyNTnon_all_figurative_numeric_literal) {
} break;
case 4743: yyDecrement (1) yySetNT (yyNTnon_all_figurative_numeric_literal) {
} break;
case 4744: yyDecrement (2) yySetNT (yyNTall_figurative_non_numeric_literal) {
} break;
case 4745: yyDecrement (2) yySetNT (yyNTall_figurative_non_numeric_literal) {
} break;
case 4746: yyDecrement (2) yySetNT (yyNTall_figurative_non_numeric_literal) {
} break;
case 4747: yyDecrement (2) yySetNT (yyNTall_figurative_non_numeric_literal) {
} break;
case 4748: yyDecrement (2) yySetNT (yyNTall_figurative_non_numeric_literal) {
} break;
case 4749: yyDecrement (2) yySetNT (yyNTall_figurative_non_numeric_literal) {
} break;
case 4750: yyDecrement (2) yySetNT (yyNTall_figurative_numeric_literal) {
} break;
case 4751: yyDecrement (2) yySetNT (yyNTall_figurative_numeric_literal) {
} break;
case 4752: yyDecrement (1) yySetNT (yyNTAdvancing) {
} break;
case 4753: yySetNT (yyNTAdvancing) {
} break;
case 4754: yyDecrement (1) yySetNT (yyNTAre) {
} break;
case 4755: yySetNT (yyNTAre) {
} break;
case 4756: yyDecrement (1) yySetNT (yyNTAreIs) {
} break;
case 4757: yyDecrement (1) yySetNT (yyNTAreIs) {
} break;
case 4758: yySetNT (yyNTAreIs) {
} break;
case 4759: yyDecrement (1) yySetNT (yyNTArea) {
} break;
case 4760: yySetNT (yyNTArea) {
} break;
case 4761: yyDecrement (1) yySetNT (yyNTAt) {
} break;
case 4762: yySetNT (yyNTAt) {
} break;
case 4763: yyDecrement (1) yySetNT (yyNTBy) {
} break;
case 4764: yySetNT (yyNTBy) {
} break;
case 4765: yyDecrement (1) yySetNT (yyNTCharacter) {
} break;
case 4766: yySetNT (yyNTCharacter) {
} break;
case 4767: yyDecrement (1) yySetNT (yyNTCharacters) {
} break;
case 4768: yySetNT (yyNTCharacters) {
} break;
case 4769: yyDecrement (1) yySetNT (yyNTCollating) {
} break;
case 4770: yySetNT (yyNTCollating) {
} break;
case 4771: yyDecrement (1) yySetNT (yyNTContains) {
} break;
case 4772: yySetNT (yyNTContains) {
} break;
case 4773: yyDecrement (1) yySetNT (yyNTData) {
} break;
case 4774: yySetNT (yyNTData) {
} break;
case 4775: yyDecrement (1) yySetNT (yyNTEvery) {
} break;
case 4776: yySetNT (yyNTEvery) {
} break;
case 4777: yyDecrement (1) yySetNT (yyNTFile) {
} break;
case 4778: yySetNT (yyNTFile) {
} break;
case 4779: yyDecrement (1) yySetNT (yyNTFiller) {
} break;
case 4780: yySetNT (yyNTFiller) {
} break;
case 4781: yyDecrement (1) yySetNT (yyNTFor) {
} break;
case 4782: yySetNT (yyNTFor) {
} break;
case 4783: yyDecrement (1) yySetNT (yyNTFrom) {
} break;
case 4784: yySetNT (yyNTFrom) {
} break;
case 4785: yyDecrement (1) yySetNT (yyNTIndicate) {
} break;
case 4786: yySetNT (yyNTIndicate) {
} break;
case 4787: yyDecrement (1) yySetNT (yyNTIn) {
} break;
case 4788: yySetNT (yyNTIn) {
} break;
case 4789: yyDecrement (1) yySetNT (yyNTInto) {
} break;
case 4790: yySetNT (yyNTInto) {
} break;
case 4791: yyDecrement (1) yySetNT (yyNTIs) {
} break;
case 4792:
yy1905: yySetNT (yyNTIs) {
} break;
case 4793: yyDecrement (1) yySetNT (yyNTKey) {
} break;
case 4794: yySetNT (yyNTKey) {
} break;
case 4795: yyDecrement (1) yySetNT (yyNTLine) {
} break;
case 4796: yySetNT (yyNTLine) {
} break;
case 4797: yyDecrement (1) yySetNT (yyNTLines) {
} break;
case 4798: yySetNT (yyNTLines) {
} break;
case 4799: yyDecrement (1) yySetNT (yyNTMessage) {
} break;
case 4800: yySetNT (yyNTMessage) {
} break;
case 4801: yyDecrement (1) yySetNT (yyNTMode) {
} break;
case 4802: yySetNT (yyNTMode) {
} break;
case 4803: yyDecrement (1) yySetNT (yyNTNumber) {
} break;
case 4804: yySetNT (yyNTNumber) {
} break;
case 4805: yyDecrement (1) yySetNT (yyNTOf) {
} break;
case 4806: yySetNT (yyNTOf) {
} break;
case 4807: yyDecrement (1) yySetNT (yyNTOn) {
} break;
case 4808: yySetNT (yyNTOn) {
} break;
case 4809: yyDecrement (1) yySetNT (yyNTOrder) {
} break;
case 4810: yySetNT (yyNTOrder) {
} break;
case 4811: yyDecrement (1) yySetNT (yyNTPrinting) {
} break;
case 4812: yySetNT (yyNTPrinting) {
} break;
case 4813: yyDecrement (1) yySetNT (yyNTProgram) {
} break;
case 4814: yySetNT (yyNTProgram) {
} break;
case 4815: yyDecrement (1) yySetNT (yyNTReferences) {
} break;
case 4816: yySetNT (yyNTReferences) {
} break;
case 4817: yyDecrement (1) yySetNT (yyNTRight) {
} break;
case 4818: yySetNT (yyNTRight) {
} break;
case 4819: yyDecrement (1) yySetNT (yyNTSign) {
} break;
case 4820: yySetNT (yyNTSign) {
} break;
case 4821: yyDecrement (1) yySetNT (yyNTSize) {
} break;
case 4822: yySetNT (yyNTSize) {
} break;
case 4823: yyDecrement (1) yySetNT (yyNTStandard) {
} break;
case 4824: yySetNT (yyNTStandard) {
} break;
case 4825: yyDecrement (1) yySetNT (yyNTStatus) {
} break;
case 4826: yySetNT (yyNTStatus) {
} break;
case 4827: yyDecrement (1) yySetNT (yyNTSymbolic) {
} break;
case 4828: yySetNT (yyNTSymbolic) {
} break;
case 4829: yyDecrement (1) yySetNT (yyNTTape) {
} break;
case 4830: yySetNT (yyNTTape) {
} break;
case 4831: yyDecrement (1) yySetNT (yyNTThan) {
} break;
case 4832: yySetNT (yyNTThan) {
} break;
case 4833: yyDecrement (1) yySetNT (yyNTThen) {
} break;
case 4834: yySetNT (yyNTThen) {
} break;
case 4835: yyDecrement (1) yySetNT (yyNTTimes) {
} break;
case 4836: yySetNT (yyNTTimes) {
} break;
case 4837: yyDecrement (1) yySetNT (yyNTTo) {
} break;
case 4838: yySetNT (yyNTTo) {
} break;
case 4839: yyDecrement (1) yySetNT (yyNTWhen) {
} break;
case 4840: yySetNT (yyNTWhen) {
} break;
case 4841: yyDecrement (1) yySetNT (yyNTWith) {
} break;
case 4842: yySetNT (yyNTWith) {
} break;
case 4843: yyDecrement (1) yySetNT (yyNTTrailing) {
} break;
case 4844: yySetNT (yyNTTrailing) {
} break;
case 4845: yyDecrement (1) yySetNT (yyNTSet) {
} break;
case 4846: yySetNT (yyNTSet) {
} break;
case 4847: yyDecrement (1) yySetNT (yyNTAlternate) {
} break;
case 4848: yySetNT (yyNTAlternate) {
} break;
case 4849: yyDecrement (1) yySetNT (yyNTMultiple) {
} break;
case 4850: yySetNT (yyNTMultiple) {
} break;
case 4851: yyDecrement (1) yySetNT (yyNTGlobal) {
} break;
case 4852: yySetNT (yyNTGlobal) {
} break;
case 4853: yyDecrement (1) yySetNT (yyNTInitial) {
} break;
case 4854: yySetNT (yyNTInitial) {
} break;
case 4855: yyDecrement (1) yySetNT (yyNTOptional) {
} break;
case 4856: yySetNT (yyNTOptional) {
} break;
case 4857: yyDecrement (1) yySetNT (yyNTRecord) {
} break;
case 4858:
yy1971: yySetNT (yyNTRecord) {
} break;
case 4859: yyDecrement (1) yySetNT (yyNTRounded) {
} break;
case 4860: yySetNT (yyNTRounded) {
} break;
case 4861: yyDecrement (1) yySetNT (yyNTEnd_accept) {
} break;
case 4862:
yy1975: yySetNT (yyNTEnd_accept) {
} break;
case 4863: yyDecrement (1) yySetNT (yyNTEnd_add) {
} break;
case 4864:
yy1977: yySetNT (yyNTEnd_add) {
} break;
case 4865: yyDecrement (1) yySetNT (yyNTEnd_call) {
} break;
case 4866:
yy1979: yySetNT (yyNTEnd_call) {
} break;
case 4867: yyDecrement (1) yySetNT (yyNTEnd_chain) {
} break;
case 4868: yySetNT (yyNTEnd_chain) {
} break;
case 4869: yyDecrement (1) yySetNT (yyNTEnd_compute) {
} break;
case 4870:
yy1983: yySetNT (yyNTEnd_compute) {
} break;
case 4871: yyDecrement (1) yySetNT (yyNTEnd_delete) {
} break;
case 4872:
yy1985: yySetNT (yyNTEnd_delete) {
} break;
case 4873: yyDecrement (1) yySetNT (yyNTEnd_display) {
} break;
case 4874:
yy1987: yySetNT (yyNTEnd_display) {
} break;
case 4875: yyDecrement (1) yySetNT (yyNTEnd_divide) {
} break;
case 4876:
yy1989: yySetNT (yyNTEnd_divide) {
} break;
case 4877: yyDecrement (1) yySetNT (yyNTEnd_evaluate) {
} break;
case 4878: yySetNT (yyNTEnd_evaluate) {
} break;
case 4879: yyDecrement (1) yySetNT (yyNTEnd_if) {
} break;
case 4880: yySetNT (yyNTEnd_if) {
} break;
case 4881: yyDecrement (1) yySetNT (yyNTEnd_multiply) {
} break;
case 4882:
yy1995: yySetNT (yyNTEnd_multiply) {
} break;
case 4883: yyDecrement (1) yySetNT (yyNTEnd_perform) {
} break;
case 4884: yySetNT (yyNTEnd_perform) {
} break;
case 4885: yyDecrement (1) yySetNT (yyNTEnd_read) {
} break;
case 4886:
yy1999: yySetNT (yyNTEnd_read) {
} break;
case 4887: yyDecrement (1) yySetNT (yyNTEnd_receive) {
} break;
case 4888: yySetNT (yyNTEnd_receive) {
} break;
case 4889: yyDecrement (1) yySetNT (yyNTEnd_return) {
} break;
case 4890: yySetNT (yyNTEnd_return) {
} break;
case 4891: yyDecrement (1) yySetNT (yyNTEnd_rewrite) {
} break;
case 4892:
yy2005: yySetNT (yyNTEnd_rewrite) {
} break;
case 4893: yyDecrement (1) yySetNT (yyNTEnd_search) {
} break;
case 4894: yySetNT (yyNTEnd_search) {
} break;
case 4895: yyDecrement (1) yySetNT (yyNTEnd_start) {
} break;
case 4896:
yy2009: yySetNT (yyNTEnd_start) {
} break;
case 4897: yyDecrement (1) yySetNT (yyNTEnd_string) {
} break;
case 4898:
yy2011: yySetNT (yyNTEnd_string) {
} break;
case 4899: yyDecrement (1) yySetNT (yyNTEnd_subtract) {
} break;
case 4900:
yy2013: yySetNT (yyNTEnd_subtract) {
} break;
case 4901: yyDecrement (1) yySetNT (yyNTEnd_unstring) {
} break;
case 4902:
yy2015: yySetNT (yyNTEnd_unstring) {
} break;
case 4903: yyDecrement (1) yySetNT (yyNTEnd_write) {
} break;
case 4904:
yy2017: yySetNT (yyNTEnd_write) {
} break;
case 4905: yyDecrement (4) yySetNT (yyNTRELATIVE_Key_Is_name) {
} break;
case 4906: yyDecrement (2) yySetNT (yyNTON_head) {
} break;
case 4907: yyDecrement (2) yySetNT (yyNTON_head) {
} break;
case 4908: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4909: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4910: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4911: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4912: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4913: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4914: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4915: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4916: yyDecrement (2) yySetNT (yyNTnot) {
} break;
case 4917: yyDecrement (2) yySetNT (yyNTALTERNATE_AREA) {
} break;
case 4918: yyDecrement (2) yySetNT (yyNTALTERNATE_AREA) {
} break;
case 4919: yyDecrement (1) yySetNT (yyNTON_head_or_WITH_DATA) {
} break;
case 4920: yyDecrement (2) yySetNT (yyNTON_head_or_WITH_DATA) {
} break;
case 4921: yyDecrement (3) yySetNT (yyNTname_Message_COUNT) {
} break;
case 4922: yyDecrement (2) yySetNT (yyNTidentifier_or_numeric_literal_GIVING) {
} break;
case 4923: yyDecrement (6) yySetNT (yyNTexamine_replacing) {
} break;
case 4924: yyDecrement (5) yySetNT (yyNTexamine_replacing) {
} break;
case 4925: yyDecrement (5) yySetNT (yyNTexamine_replacing) {
} break;
case 4926: yyDecrement (2) yySetNT (yyNTexamine_replacing) {
} break;
case 4927: yyDecrement (2) yySetNT (yyNTqualification_FROM) {
} break;
case 4928: yyDecrement (2) yySetNT (yyNTidentifier_replacing) {
} break;
case 4929: yyDecrement (4) yySetNT (yyNTidentifier_replacing) {
} break;
case 4930: yyDecrement (2) yySetNT (yyNTidentifier_replacing) {
} break;
case 4931: yyDecrement (2) yySetNT (yyNTidentifier_FOR) {
} break;
case 4932: yyDecrement (3) yySetNT (yyNTCONSOLE_Is_CRT) {
} break;
case 4933: yyDecrement (3) yySetNT (yyNTimplementor_name_e_head) {
} break;
case 4934: yyDecrement (2) yySetNT (yyNTimplementor_name_e_head) {
} break;
case 4935: yyDecrement (2) yySetNT (yyNTimplementor_name_e_head) {
} break;
case 4936: yyDecrement (3) yySetNT (yyNTsymbolic_e_head) {
} break;
case 4937: yyDecrement (2) yySetNT (yyNTnon_screen_display) {
} break;
case 4938: yyDecrement (2) yySetNT (yyNTnon_screen_display) {
} break;
case 4939: yyDecrement (3) yySetNT (yyNTnon_screen_display) {
} break;
case 4940: yyDecrement (2) yySetNT (yyNTparentheses_relational_operator) {
} break;
case 4941: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4942: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4943: yyDecrement (1) yySetNT (yyNTrelational_operator_3) {
} break;
case 4944: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4945: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4946: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4947: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4948: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4949: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4950: yyDecrement (1) yySetNT (yyNTrelational_operator_3) {
} break;
case 4951: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4952: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4953: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4954: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4955: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4956: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4957: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4958: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4959: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4960: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4961: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4962: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4963: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4964: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4965: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4966: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4967: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4968: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4969: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4970: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4971: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4972: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4973: yyDecrement (3) yySetNT (yyNTrelational_operator_3) {
} break;
case 4974: yyDecrement (2) yySetNT (yyNTrelational_operator_3) {
} break;
case 4975: yyDecrement (3) yySetNT (yyNTin_of_qualification_TIMES) {
} break;
case 4976: yyDecrement (3) yySetNT (yyNTon_size_error_head) {
} break;
case 4977: yyDecrement (2) yySetNT (yyNTon_exception_head) {
} break;
case 4978: yyDecrement (2) yySetNT (yyNTon_overflow_head) {
} break;
case 4979: yyDecrement (1) yySetNT (yyNTinvalid_head) {
} break;
case 4980: yyDecrement (2) yySetNT (yyNTat_end_head) {
} break;
case 4981: yyDecrement (2) yySetNT (yyNTat_end_of_page_head) {
} break;
case 4982: yyDecrement (2) yySetNT (yyNTon_error_head) {
} break;
case 4983: yyDecrement (4) yySetNT (yyNTnot_size_error_head) {
} break;
case 4984: yyDecrement (3) yySetNT (yyNTnot_exception_head) {
} break;
case 4985: yyDecrement (3) yySetNT (yyNTnot_overflow_head) {
} break;
case 4986: yyDecrement (2) yySetNT (yyNTnot_invalid_head) {
} break;
case 4987: yyDecrement (3) yySetNT (yyNTnot_end_head) {
} break;
case 4988: yyDecrement (3) yySetNT (yyNTnot_end_of_page_head) {
} break;
case 4989: yyDecrement (1) yySetNT (yyNTsize_error_head) {
} break;
case 4990: yyDecrement (1) yySetNT (yyNTsize_error_head) {
} break;
case 4991: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4992: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4993: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4994: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4995: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4996: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4997: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4998: yyDecrement (1) yySetNT (yyNTaccept_tail) {
} break;
case 4999: yyDecrement (1) yySetNT (yyNTadd_tail) {
} break;
case 5000: yyDecrement (1) yySetNT (yyNTadd_tail) {
} break;
case 5001: yyDecrement (1) yySetNT (yyNTcall_tail) {
} break;
case 5002: yyDecrement (1) yySetNT (yyNTcall_tail) {
} break;
case 5003: yyDecrement (1) yySetNT (yyNTcall_tail) {
} break;
case 5004: yyDecrement (1) yySetNT (yyNTcall_tail) {
} break;
case 5005: yyDecrement (1) yySetNT (yyNTcall_tail) {
} break;
case 5006: yyDecrement (1) yySetNT (yyNTcompute_tail) {
} break;
case 5007: yyDecrement (1) yySetNT (yyNTcompute_tail) {
} break;
case 5008: yyDecrement (1) yySetNT (yyNTdelete_tail) {
} break;
case 5009: yyDecrement (1) yySetNT (yyNTdelete_tail) {
} break;
case 5010: yyDecrement (1) yySetNT (yyNTdelete_tail) {
} break;
case 5011: yyDecrement (1) yySetNT (yyNTdisplay_tail) {
} break;
case 5012: yyDecrement (1) yySetNT (yyNTdisplay_tail) {
} break;
case 5013: yyDecrement (1) yySetNT (yyNTdisplay_tail) {
} break;
case 5014: yyDecrement (1) yySetNT (yyNTdivide_tail) {
} break;
case 5015: yyDecrement (1) yySetNT (yyNTdivide_tail) {
} break;
case 5016: yyDecrement (1) yySetNT (yyNTmultiply_tail) {
} break;
case 5017: yyDecrement (1) yySetNT (yyNTmultiply_tail) {
} break;
case 5018: yyDecrement (1) yySetNT (yyNTread_tail) {
} break;
case 5019: yyDecrement (1) yySetNT (yyNTread_tail) {
} break;
case 5020: yyDecrement (1) yySetNT (yyNTread_tail) {
} break;
case 5021: yyDecrement (1) yySetNT (yyNTread_tail) {
} break;
case 5022: yyDecrement (1) yySetNT (yyNTread_tail) {
} break;
case 5023: yyDecrement (1) yySetNT (yyNTreturn_tail) {
} break;
case 5024: yyDecrement (1) yySetNT (yyNTreturn_tail) {
} break;
case 5025: yyDecrement (1) yySetNT (yyNTrewrite_tail) {
} break;
case 5026: yyDecrement (1) yySetNT (yyNTrewrite_tail) {
} break;
case 5027: yyDecrement (1) yySetNT (yyNTrewrite_tail) {
} break;
case 5028: yyDecrement (1) yySetNT (yyNTstart_tail) {
} break;
case 5029: yyDecrement (1) yySetNT (yyNTstart_tail) {
} break;
case 5030: yyDecrement (1) yySetNT (yyNTstart_tail) {
} break;
case 5031: yyDecrement (1) yySetNT (yyNTstring_tail) {
} break;
case 5032: yyDecrement (1) yySetNT (yyNTstring_tail) {
} break;
case 5033: yyDecrement (1) yySetNT (yyNTstring_tail) {
} break;
case 5034: yyDecrement (1) yySetNT (yyNTsubtract_tail) {
} break;
case 5035: yyDecrement (1) yySetNT (yyNTsubtract_tail) {
} break;
case 5036: yyDecrement (1) yySetNT (yyNTunstring_tail) {
} break;
case 5037: yyDecrement (1) yySetNT (yyNTunstring_tail) {
} break;
case 5038: yyDecrement (1) yySetNT (yyNTunstring_tail) {
} break;
case 5039: yyDecrement (1) yySetNT (yyNTwrite_tail) {
} break;
case 5040: yyDecrement (1) yySetNT (yyNTwrite_tail) {
} break;
case 5041: yyDecrement (1) yySetNT (yyNTwrite_tail) {
} break;
case 5042: yyDecrement (1) yySetNT (yyNTwrite_tail) {
} break;
case 5043: yyDecrement (1) yySetNT (yyNTwrite_tail) {
} break;
case 5044: yyDecrement (1) yySetNT (yyNTdescriptions) {
} break;
case 5045: yySetNT (yyNTdescription_l) {
} break;
case 5046: yyDecrement (2) yySetNT (yyNTdescription_l) {
} break;
case 5047: yyDecrement (2) yySetNT (yyNTdescription_l) {
} break;
case 5048: yyDecrement (2) yySetNT (yyNTdescription_l) {
} break;
case 5049: yyDecrement (2) yySetNT (yyNTdescription_l) {
} break;
case 5050: yyDecrement (3) yySetNT (yyNTdescription_l) {
} break;
case 5051:
yy2164: yySetNT (yyNT2_release_Trial_2) {
} break;
case 5052:
yy2165: yySetNT (yyNT5_accept_i_Trial_2) {
} break;
case 5053:
yy2166: yySetNT (yyNT5_accept_Trial_2) {
} break;
case 5054: /* STATE 87 */
if (yyPrintResult (* yyStateStackPtr, 2408, yyTrialParse (
18, yyTerminal, 2408) == 0)) yyGotoReduce (4607, yy1720)
else { yyGotoRead (127)
}
case 5055: /* STATE 260 */
/* line 2492 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2492,
( yyA [-1].Scan.name.Ident == iCURRENT_DATE || yyA [-1].Scan.name.Ident == iWHEN_COMPILED 
))) yyGotoReduce (4674, yy1787)
else { yyGotoReduce (4675, yy1788)
}
case 5056: /* STATE 262 */
if (yyPrintResult (* yyStateStackPtr, 2488, yyTrialParse (
23, yyTerminal, 2488) != 0)) yyGotoReduce (4670, yy1783)
else { yyGotoRead (78)
}
case 5057: /* STATE 418 */
if (yyPrintResult (* yyStateStackPtr, 2648, yyTrialParse (
26, yyTerminal, 2648) == 0)) yyGotoReduce (4792, yy1905)
else { yyGotoReduce (4498, yy1611)
}
case 5058: /* STATE 609 */
if (yyPrintResult (* yyStateStackPtr, 2489, yyTrialParse (
24, yyTerminal, 2489) != 0)) yyGotoReduce (4671, yy1784)
else { yyGotoRead (78)
}
case 5059: /* STATE 616 */
if (yyPrintResult (* yyStateStackPtr, 2410, yyTrialParse (
19, yyTerminal, 2410) != 0)) yyGotoReduce (4609, yy1722)
else { yyGotoRead (78)
}
case 5060: /* STATE 820 */
if (yyPrintResult (* yyStateStackPtr, 1865, yyTrialParse (
15, yyTerminal, 1865) == 0)) yyGotoReduce (4101, yy1214)
else { yyGotoRead (57)
}
case 5061: /* STATE 831 */
if (yyPrintResult (* yyStateStackPtr, 1866, yyTrialParse (
16, yyTerminal, 1866) == 0)) yyGotoReduce (4102, yy1215)
else { yyGotoRead (57)
}
case 5062: /* STATE 1215 */
if (yyPrintResult (* yyStateStackPtr, 1464, yyTrialParse (
45, yyTerminal, 1464) == 0)) yyGotoReduce (5053, yy2166)
else { yyGotoReduce (4627, yy1740)
}
case 5063: /* STATE 1233 */
if (yyPrintResult (* yyStateStackPtr, 1798, yyTrialParse (
13, yyTerminal, 1798) == 0)) yyGotoReduce (4034, yy1147)
else { yyGotoRead (57)
}
case 5064: /* STATE 1242 */
if (yyPrintResult (* yyStateStackPtr, 1858, yyTrialParse (
14, yyTerminal, 1858) == 0)) yyGotoReduce (4094, yy1207)
else { yyGotoRead (57)
}
case 5065: /* STATE 1254 */
if (yyPrintResult (* yyStateStackPtr, 2018, yyTrialParse (
43, yyTerminal, 2018) == 0)) yyGotoReduce (5051, yy2164)
else { yyGotoRead (57)
}
case 5066: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (641)
}
case 5067: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (642)
}
case 5068: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (58)
}
case 5069: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (59)
}
case 5070: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (224)
}
case 5071: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (472)
}
case 5072: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (284)
}
case 5073: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (63)
}
case 5074: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (64)
}
case 5075: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (65)
}
case 5076: /* STATE 1276 */
/* line 1307 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1307,
( GetLookahead (2) == 359              
))) yyGotoReduce (3589, yy702)
else { yyGotoRead (66)
}
case 5077: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (641)
}
case 5078: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (642)
}
case 5079: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (58)
}
case 5080: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (59)
}
case 5081: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (63)
}
case 5082: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (64)
}
case 5083: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (65)
}
case 5084: /* STATE 1277 */
/* line 1304 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1304,
( GetLookahead (2) == 359              
))) yyGotoReduce (3586, yy699)
else { yyGotoRead (66)
}
case 5085: /* STATE 1376 */
/* line 2451 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2451,
( GetLookahead (2) == 50         
))) yyGotoReduce (4637, yy1750)
else { yyGotoRead (79)
}
case 5086: /* STATE 1376 */
/* line 2451 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2451,
( GetLookahead (2) == 50         
))) yyGotoReduce (4637, yy1750)
else { yyGotoRead (80)
}
case 5087: /* STATE 1377 */
/* line 2260 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2260,
( GetLookahead (2) == 50         
))) yyGotoReduce (4459, yy1572)
else { yyGotoRead (79)
}
case 5088: /* STATE 1377 */
/* line 2260 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2260,
( GetLookahead (2) == 50         
))) yyGotoReduce (4459, yy1572)
else { yyGotoRead (80)
}
case 5089: /* STATE 1385 */
/* line 2447 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2447,
( GetLookahead (2) == 50         
))) yyGotoReduce (4633, yy1746)
else { yyGotoRead (936)
}
case 5090: /* STATE 1386 */
/* line 2543 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2543,
( GetLookahead (2) == 50         
))) yyGotoReduce (4707, yy1820)
else { yyGotoRead (79)
}
case 5091: /* STATE 1386 */
/* line 2543 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2543,
( GetLookahead (2) == 50         
))) yyGotoReduce (4707, yy1820)
else { yyGotoRead (80)
}
case 5092: /* STATE 1467 */
/* line 1165 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1165,
( GetLookahead (2) == 199            
))) yyGotoReduce (3456, yy569)
else { yyGotoRead (986)
}
case 5093: /* STATE 1493 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5094: /* STATE 1515 */
/* line 1645 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1645,
( GetLookahead (2) == 101           
))) yyGotoReduce (3899, yy1012)
else { yyGotoRead (70)
}
case 5095: /* STATE 1531 */
if (yyPrintResult (* yyStateStackPtr, 1682, yyTrialParse (
12, yyTerminal, 1682) == 0)) yyGotoReduce (3936, yy1049)
else { yyGotoRead (1031)
}
case 5096: /* STATE 1531 */
if (yyPrintResult (* yyStateStackPtr, 1682, yyTrialParse (
12, yyTerminal, 1682) == 0)) yyGotoReduce (3936, yy1049)
else { yyGotoRead (1032)
}
case 5097: /* STATE 1576 */
if (yyPrintResult (* yyStateStackPtr, 2529, yyTrialParse (
25, yyTerminal, 2529) != 0)) yyGotoReduce (4703, yy1816)
else { yyGotoRead (79)
}
case 5098: /* STATE 1576 */
if (yyPrintResult (* yyStateStackPtr, 2529, yyTrialParse (
25, yyTerminal, 2529) != 0)) yyGotoReduce (4703, yy1816)
else { yyGotoRead (80)
}
case 5099: /* STATE 1577 */
if (yyPrintResult (* yyStateStackPtr, 1480, yyTrialParse (
44, yyTerminal, 1480) == 0)) yyGotoReduce (5052, yy2165)
else { yyGotoReduce (4627, yy1740)
}
case 5100: /* STATE 1635 */
/* line 1940 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1940,
( GetLookahead (2) == 101           
))) yyGotoReduce (4174, yy1287)
else { yyGotoRead (70)
}
case 5101: /* STATE 1668 */
/* line 2714 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2714,
( GetLookahead (2) == 366               
))) yyGotoReduce (4858, yy1971)
else { yyGotoRead (1104)
}
case 5102: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (641)
}
case 5103: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (642)
}
case 5104: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (58)
}
case 5105: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (59)
}
case 5106: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (63)
}
case 5107: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (64)
}
case 5108: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (65)
}
case 5109: /* STATE 1704 */
/* line 1310 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1310,
( GetLookahead (2) == 359              
))) yyGotoReduce (3590, yy703)
else { yyGotoRead (66)
}
case 5110: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (641)
}
case 5111: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (642)
}
case 5112: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (58)
}
case 5113: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (59)
}
case 5114: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (63)
}
case 5115: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (64)
}
case 5116: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (65)
}
case 5117: /* STATE 1825 */
/* line 1301 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1301,
( GetLookahead (2) == 359              
))) yyGotoReduce (3583, yy696)
else { yyGotoRead (66)
}
case 5118: /* STATE 1841 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5119: /* STATE 1847 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5120: /* STATE 1861 */
/* line 1646 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1646,
( GetLookahead (2) == 101           
))) yyGotoReduce (3900, yy1013)
else { yyGotoRead (70)
}
case 5121: /* STATE 1875 */
/* line 1670 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1670,
( GetLookahead (2) == 101           
))) yyGotoReduce (3924, yy1037)
else { yyGotoRead (70)
}
case 5122: /* STATE 1888 */
/* line 1686 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1686,
( GetLookahead (2) == 101           
))) yyGotoReduce (3940, yy1053)
else { yyGotoRead (1032)
}
case 5123: /* STATE 1892 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5124: /* STATE 1893 */
/* line 1741 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1741,
( GetLookahead (2) == 101           
))) yyGotoReduce (3995, yy1108)
else { yyGotoRead (70)
}
case 5125: /* STATE 1923 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5126: /* STATE 1928 */
/* line 1933 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1933,
( GetLookahead (2) == 101           
))) yyGotoReduce (4169, yy1282)
else { yyGotoRead (70)
}
case 5127: /* STATE 1932 */
/* line 1926 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1926,
( GetLookahead (2) == 101           
))) yyGotoReduce (4162, yy1275)
else { yyGotoRead (70)
}
case 5128: /* STATE 1935 */
/* line 1930 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1930,
( GetLookahead (2) == 101           
))) yyGotoReduce (4166, yy1279)
else { yyGotoRead (70)
}
case 5129: /* STATE 1938 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1000)
}
case 5130: /* STATE 1938 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5131: /* STATE 1938 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1002)
}
case 5132: /* STATE 1947 */
if (yyPrintResult (* yyStateStackPtr, 2730, yyTrialParse (
32, yyTerminal, 2730) != 0)) yyGotoReduce (4874, yy1987)
else { yyGotoRead (1028)
}
case 5133: /* STATE 1952 */
/* line 2714 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2714,
( GetLookahead (2) == 366               
))) yyGotoReduce (4858, yy1971)
else { yyGotoRead (1333)
}
case 5134: /* STATE 1995 */
if (yyPrintResult (* yyStateStackPtr, 2088, yyTrialParse (
17, yyTerminal, 2088) == 0)) yyGotoReduce (4311, yy1424)
else { yyGotoRead (87)
}
case 5135: /* STATE 1995 */
if (yyPrintResult (* yyStateStackPtr, 2088, yyTrialParse (
17, yyTerminal, 2088) == 0)) yyGotoReduce (4311, yy1424)
else { yyGotoRead (70)
}
case 5136: /* STATE 2004 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5137: /* STATE 2038 */
/* line 641 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 641,
( GetLookahead (2) == 1           
))) yyGotoReduce (3009, yy122)
else { yyGotoRead (117)
}
case 5138: /* STATE 2043 */
if (yyPrintResult (* yyStateStackPtr, 617, yyTrialParse (
3, yyTerminal, 617) == 0)) yyGotoReduce (2991, yy104)
else { yyGotoRead (51)
}
case 5139: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (641)
}
case 5140: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (642)
}
case 5141: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (58)
}
case 5142: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (59)
}
case 5143: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (63)
}
case 5144: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (64)
}
case 5145: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (65)
}
case 5146: /* STATE 2141 */
/* line 1303 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1303,
( GetLookahead (2) == 359              
))) yyGotoReduce (3585, yy698)
else { yyGotoRead (66)
}
case 5147: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (641)
}
case 5148: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (642)
}
case 5149: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (58)
}
case 5150: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (59)
}
case 5151: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (63)
}
case 5152: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (64)
}
case 5153: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (65)
}
case 5154: /* STATE 2146 */
/* line 1300 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1300,
( GetLookahead (2) == 359              
))) yyGotoReduce (3582, yy695)
else { yyGotoRead (66)
}
case 5155: /* STATE 2161 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (87)
}
case 5156: /* STATE 2177 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5157: /* STATE 2185 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5158: /* STATE 2187 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5159: /* STATE 2201 */
if (yyPrintResult (* yyStateStackPtr, 2722, yyTrialParse (
29, yyTerminal, 2722) != 0)) yyGotoReduce (4866, yy1979)
else { yyGotoRead (87)
}
case 5160: /* STATE 2213 */
if (yyPrintResult (* yyStateStackPtr, 2726, yyTrialParse (
30, yyTerminal, 2726) != 0)) yyGotoReduce (4870, yy1983)
else { yyGotoRead (87)
}
case 5161: /* STATE 2224 */
if (yyPrintResult (* yyStateStackPtr, 2730, yyTrialParse (
32, yyTerminal, 2730) != 0)) yyGotoReduce (4874, yy1987)
else { yyGotoRead (87)
}
case 5162: /* STATE 2236 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5163: /* STATE 2295 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (87)
}
case 5164: /* STATE 2312 */
/* line 1522 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1522,
( GetLookahead (2) == 101           
))) yyGotoReduce (3792, yy905)
else { yyGotoRead (1233)
}
case 5165: /* STATE 2313 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1000)
}
case 5166: /* STATE 2313 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5167: /* STATE 2318 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5168: /* STATE 2322 */
if (yyPrintResult (* yyStateStackPtr, 2728, yyTrialParse (
31, yyTerminal, 2728) != 0)) yyGotoReduce (4872, yy1985)
else { yyGotoRead (1259)
}
case 5169: /* STATE 2322 */
if (yyPrintResult (* yyStateStackPtr, 2728, yyTrialParse (
31, yyTerminal, 2728) != 0)) yyGotoReduce (4872, yy1985)
else { yyGotoRead (1261)
}
case 5170: /* STATE 2323 */
/* line 1691 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1691,
( GetLookahead (2) == 101           
))) yyGotoReduce (3945, yy1058)
else { yyGotoRead (70)
}
case 5171: /* STATE 2326 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5172: /* STATE 2329 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5173: /* STATE 2335 */
if (yyPrintResult (* yyStateStackPtr, 2748, yyTrialParse (
37, yyTerminal, 2748) != 0)) yyGotoReduce (4892, yy2005)
else { yyGotoRead (1360)
}
case 5174: /* STATE 2335 */
if (yyPrintResult (* yyStateStackPtr, 2748, yyTrialParse (
37, yyTerminal, 2748) != 0)) yyGotoReduce (4892, yy2005)
else { yyGotoRead (1261)
}
case 5175: /* STATE 2338 */
if (yyPrintResult (* yyStateStackPtr, 2752, yyTrialParse (
38, yyTerminal, 2752) != 0)) yyGotoReduce (4896, yy2009)
else { yyGotoRead (1379)
}
case 5176: /* STATE 2338 */
if (yyPrintResult (* yyStateStackPtr, 2752, yyTrialParse (
38, yyTerminal, 2752) != 0)) yyGotoReduce (4896, yy2009)
else { yyGotoRead (1261)
}
case 5177: /* STATE 2341 */
if (yyPrintResult (* yyStateStackPtr, 1577, yyTrialParse (
11, yyTerminal, 1577) == 0)) yyGotoReduce (3847, yy960)
else { yyGotoReduce (4627, yy1740)
}
case 5178: /* STATE 2343 */
if (yyPrintResult (* yyStateStackPtr, 2760, yyTrialParse (
42, yyTerminal, 2760) != 0)) yyGotoReduce (4904, yy2017)
else { yyGotoRead (1394)
}
case 5179: /* STATE 2343 */
if (yyPrintResult (* yyStateStackPtr, 2760, yyTrialParse (
42, yyTerminal, 2760) != 0)) yyGotoReduce (4904, yy2017)
else { yyGotoRead (1395)
}
case 5180: /* STATE 2359 */
/* line 1988 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1988,
( yy2 = GetLookahead (2),
		      yy2 == 101          		||
		      yy2 == 35        		||
		      yy2 == 131       		||
		      yy2 == 143       	||
		      yy2 == 274        		||
		      yy2 == 373          		||
		      yy2 == 282              		||
		      yy2 == 170               		||
		      yy2 == 165             
))) yyGotoReduce (4220, yy1333)
else { yyGotoRead (1261)
}
case 5181: /* STATE 2372 */
/* line 2044 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2044,
( GetLookahead (2) == 101           
))) yyGotoReduce (4267, yy1380)
else { yyGotoRead (70)
}
case 5182: /* STATE 2405 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (87)
}
case 5183: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (641)
}
case 5184: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (642)
}
case 5185: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (58)
}
case 5186: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (59)
}
case 5187: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (63)
}
case 5188: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (64)
}
case 5189: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (65)
}
case 5190: /* STATE 2518 */
/* line 1302 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1302,
( GetLookahead (2) == 359              
))) yyGotoReduce (3584, yy697)
else { yyGotoRead (66)
}
case 5191: /* STATE 2573 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5192: /* STATE 2585 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5193: /* STATE 2703 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1458)
}
case 5194: /* STATE 2703 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (87)
}
case 5195: /* STATE 2708 */
/* line 1522 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1522,
( GetLookahead (2) == 101           
))) yyGotoReduce (3792, yy905)
else { yyGotoRead (1233)
}
case 5196: /* STATE 2709 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1000)
}
case 5197: /* STATE 2709 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5198: /* STATE 2714 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1480)
}
case 5199: /* STATE 2714 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1481)
}
case 5200: /* STATE 2714 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5201: /* STATE 2716 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1480)
}
case 5202: /* STATE 2716 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1481)
}
case 5203: /* STATE 2716 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5204: /* STATE 2717 */
if (yyPrintResult (* yyStateStackPtr, 2722, yyTrialParse (
29, yyTerminal, 2722) != 0)) yyGotoReduce (4866, yy1979)
else { yyGotoRead (1492)
}
case 5205: /* STATE 2717 */
if (yyPrintResult (* yyStateStackPtr, 2722, yyTrialParse (
29, yyTerminal, 2722) != 0)) yyGotoReduce (4866, yy1979)
else { yyGotoRead (1458)
}
case 5206: /* STATE 2717 */
if (yyPrintResult (* yyStateStackPtr, 2722, yyTrialParse (
29, yyTerminal, 2722) != 0)) yyGotoReduce (4866, yy1979)
else { yyGotoRead (87)
}
case 5207: /* STATE 2718 */
if (yyPrintResult (* yyStateStackPtr, 2726, yyTrialParse (
30, yyTerminal, 2726) != 0)) yyGotoReduce (4870, yy1983)
else { yyGotoRead (1499)
}
case 5208: /* STATE 2718 */
if (yyPrintResult (* yyStateStackPtr, 2726, yyTrialParse (
30, yyTerminal, 2726) != 0)) yyGotoReduce (4870, yy1983)
else { yyGotoRead (1481)
}
case 5209: /* STATE 2718 */
if (yyPrintResult (* yyStateStackPtr, 2726, yyTrialParse (
30, yyTerminal, 2726) != 0)) yyGotoReduce (4870, yy1983)
else { yyGotoRead (87)
}
case 5210: /* STATE 2721 */
if (yyPrintResult (* yyStateStackPtr, 2730, yyTrialParse (
32, yyTerminal, 2730) != 0)) yyGotoReduce (4874, yy1987)
else { yyGotoRead (1028)
}
case 5211: /* STATE 2721 */
if (yyPrintResult (* yyStateStackPtr, 2730, yyTrialParse (
32, yyTerminal, 2730) != 0)) yyGotoReduce (4874, yy1987)
else { yyGotoRead (1458)
}
case 5212: /* STATE 2721 */
if (yyPrintResult (* yyStateStackPtr, 2730, yyTrialParse (
32, yyTerminal, 2730) != 0)) yyGotoReduce (4874, yy1987)
else { yyGotoRead (87)
}
case 5213: /* STATE 2724 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1517)
}
case 5214: /* STATE 2724 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1481)
}
case 5215: /* STATE 2724 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5216: /* STATE 2730 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (1546)
}
case 5217: /* STATE 2730 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (1481)
}
case 5218: /* STATE 2730 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (87)
}
case 5219: /* STATE 2733 */
/* line 1988 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1988,
( yy2 = GetLookahead (2),
		      yy2 == 101          		||
		      yy2 == 35        		||
		      yy2 == 131       		||
		      yy2 == 143       	||
		      yy2 == 274        		||
		      yy2 == 373          		||
		      yy2 == 282              		||
		      yy2 == 170               		||
		      yy2 == 165             
))) yyGotoReduce (4220, yy1333)
else { yyGotoRead (1261)
}
case 5220: /* STATE 2733 */
/* line 1988 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1988,
( yy2 = GetLookahead (2),
		      yy2 == 101          		||
		      yy2 == 35        		||
		      yy2 == 131       		||
		      yy2 == 143       	||
		      yy2 == 274        		||
		      yy2 == 373          		||
		      yy2 == 282              		||
		      yy2 == 170               		||
		      yy2 == 165             
))) yyGotoReduce (4220, yy1333)
else { yyGotoRead (70)
}
case 5221: /* STATE 2742 */
/* line 2117 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2117,
( GetLookahead (2) == 101           
))) yyGotoReduce (4340, yy1453)
else { yyGotoRead (70)
}
case 5222: /* STATE 2745 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (1634)
}
case 5223: /* STATE 2745 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (1481)
}
case 5224: /* STATE 2745 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (87)
}
case 5225: /* STATE 2749 */
if (yyPrintResult (* yyStateStackPtr, 2760, yyTrialParse (
42, yyTerminal, 2760) != 0)) yyGotoReduce (4904, yy2017)
else { yyGotoRead (1394)
}
case 5226: /* STATE 2749 */
if (yyPrintResult (* yyStateStackPtr, 2760, yyTrialParse (
42, yyTerminal, 2760) != 0)) yyGotoReduce (4904, yy2017)
else { yyGotoRead (1647)
}
case 5227: /* STATE 2793 */
/* line 2094 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2094,
( GetLookahead (2) == 101           
))) yyGotoReduce (4317, yy1430)
else { yyGotoRead (70)
}
case 5228: /* STATE 2804 */
if (yyPrintResult (* yyStateStackPtr, 2754, yyTrialParse (
39, yyTerminal, 2754) != 0)) yyGotoReduce (4898, yy2011)
else { yyGotoRead (87)
}
case 5229: /* STATE 2810 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (87)
}
case 5230: /* STATE 2891 */
if (yyPrintResult (* yyStateStackPtr, 669, yyTrialParse (
5, yyTerminal, 669) != 0)) yyGotoReduce (3033, yy146)
else { yyGotoRead (53)
}
case 5231: /* STATE 2903 */
if (yyPrintResult (* yyStateStackPtr, 2448, yyTrialParse (
22, yyTerminal, 2448) != 0)) yyGotoReduce (4634, yy1747)
else { yyGotoRead (78)
}
case 5232: /* STATE 2938 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5233: /* STATE 2986 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5234: /* STATE 3027 */
/* line 1522 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1522,
( GetLookahead (2) == 101           
))) yyGotoReduce (3792, yy905)
else { yyGotoRead (1233)
}
case 5235: /* STATE 3028 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1000)
}
case 5236: /* STATE 3028 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5237: /* STATE 3033 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1480)
}
case 5238: /* STATE 3033 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1481)
}
case 5239: /* STATE 3033 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5240: /* STATE 3056 */
/* line 1988 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1988,
( yy2 = GetLookahead (2),
		      yy2 == 101          		||
		      yy2 == 35        		||
		      yy2 == 131       		||
		      yy2 == 143       	||
		      yy2 == 274        		||
		      yy2 == 373          		||
		      yy2 == 282              		||
		      yy2 == 170               		||
		      yy2 == 165             
))) yyGotoReduce (4220, yy1333)
else { yyGotoRead (70)
}
case 5241: /* STATE 3057 */
/* line 1988 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 1988,
( yy2 = GetLookahead (2),
		      yy2 == 101          		||
		      yy2 == 35        		||
		      yy2 == 131       		||
		      yy2 == 143       	||
		      yy2 == 274        		||
		      yy2 == 373          		||
		      yy2 == 282              		||
		      yy2 == 170               		||
		      yy2 == 165             
))) yyGotoReduce (4220, yy1333)
else { yyGotoRead (70)
}
case 5242: /* STATE 3060 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1797)
}
case 5243: /* STATE 3060 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1798)
}
case 5244: /* STATE 3067 */
if (yyPrintResult (* yyStateStackPtr, 2754, yyTrialParse (
39, yyTerminal, 2754) != 0)) yyGotoReduce (4898, yy2011)
else { yyGotoRead (1832)
}
case 5245: /* STATE 3067 */
if (yyPrintResult (* yyStateStackPtr, 2754, yyTrialParse (
39, yyTerminal, 2754) != 0)) yyGotoReduce (4898, yy2011)
else { yyGotoRead (1833)
}
case 5246: /* STATE 3067 */
if (yyPrintResult (* yyStateStackPtr, 2754, yyTrialParse (
39, yyTerminal, 2754) != 0)) yyGotoReduce (4898, yy2011)
else { yyGotoRead (87)
}
case 5247: /* STATE 3068 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (1634)
}
case 5248: /* STATE 3068 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (1481)
}
case 5249: /* STATE 3068 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (87)
}
case 5250: /* STATE 3072 */
/* line 2117 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 2117,
( GetLookahead (2) == 101           
))) yyGotoReduce (4340, yy1453)
else { yyGotoRead (70)
}
case 5251: /* STATE 3196 */
if (yyPrintResult (* yyStateStackPtr, 723, yyTrialParse (
7, yyTerminal, 723) != 0)) yyGotoReduce (3081, yy194)
else { yyGotoReduce (3119, yy232)
}
case 5252: /* STATE 3207 */
if (yyPrintResult (* yyStateStackPtr, 655, yyTrialParse (
4, yyTerminal, 655) == 0)) yyGotoReduce (3019, yy132)
else { yyGotoRead (53)
}
case 5253: /* STATE 3215 */
if (yyPrintResult (* yyStateStackPtr, 680, yyTrialParse (
6, yyTerminal, 680) == 0)) yyGotoReduce (3044, yy157)
else { yyGotoRead (53)
}
case 5254: /* STATE 3268 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5255: /* STATE 3282 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5256: /* STATE 3298 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (87)
}
case 5257: /* STATE 3302 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1000)
}
case 5258: /* STATE 3302 */
if (yyPrintResult (* yyStateStackPtr, 2718, yyTrialParse (
27, yyTerminal, 2718) == 0)) yyGotoReduce (4862, yy1975)
else { yyGotoRead (1001)
}
case 5259: /* STATE 3315 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1517)
}
case 5260: /* STATE 3315 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1481)
}
case 5261: /* STATE 3315 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5262: /* STATE 3322 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1797)
}
case 5263: /* STATE 3322 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1798)
}
case 5264: /* STATE 3323 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1797)
}
case 5265: /* STATE 3323 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1798)
}
case 5266: /* STATE 3393 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (87)
}
case 5267: /* STATE 3397 */
if (yyPrintResult (* yyStateStackPtr, 2758, yyTrialParse (
41, yyTerminal, 2758) != 0)) yyGotoReduce (4902, yy2015)
else { yyGotoRead (87)
}
case 5268: /* STATE 3481 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5269: /* STATE 3498 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1480)
}
case 5270: /* STATE 3498 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (1481)
}
case 5271: /* STATE 3498 */
if (yyPrintResult (* yyStateStackPtr, 2720, yyTrialParse (
28, yyTerminal, 2720) != 0)) yyGotoReduce (4864, yy1977)
else { yyGotoRead (87)
}
case 5272: /* STATE 3502 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1517)
}
case 5273: /* STATE 3502 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1481)
}
case 5274: /* STATE 3502 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5275: /* STATE 3504 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (1546)
}
case 5276: /* STATE 3504 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (1481)
}
case 5277: /* STATE 3504 */
if (yyPrintResult (* yyStateStackPtr, 2738, yyTrialParse (
34, yyTerminal, 2738) != 0)) yyGotoReduce (4882, yy1995)
else { yyGotoRead (87)
}
case 5278: /* STATE 3509 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1797)
}
case 5279: /* STATE 3509 */
if (yyPrintResult (* yyStateStackPtr, 2742, yyTrialParse (
35, yyTerminal, 2742) != 0)) yyGotoReduce (4886, yy1999)
else { yyGotoRead (1261)
}
case 5280: /* STATE 3516 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (1634)
}
case 5281: /* STATE 3516 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (1481)
}
case 5282: /* STATE 3516 */
if (yyPrintResult (* yyStateStackPtr, 2756, yyTrialParse (
40, yyTerminal, 2756) != 0)) yyGotoReduce (4900, yy2013)
else { yyGotoRead (87)
}
case 5283: /* STATE 3517 */
if (yyPrintResult (* yyStateStackPtr, 2758, yyTrialParse (
41, yyTerminal, 2758) != 0)) yyGotoReduce (4902, yy2015)
else { yyGotoRead (2066)
}
case 5284: /* STATE 3517 */
if (yyPrintResult (* yyStateStackPtr, 2758, yyTrialParse (
41, yyTerminal, 2758) != 0)) yyGotoReduce (4902, yy2015)
else { yyGotoRead (1833)
}
case 5285: /* STATE 3517 */
if (yyPrintResult (* yyStateStackPtr, 2758, yyTrialParse (
41, yyTerminal, 2758) != 0)) yyGotoReduce (4902, yy2015)
else { yyGotoRead (87)
}
case 5286: /* STATE 3518 */
if (yyPrintResult (* yyStateStackPtr, 2760, yyTrialParse (
42, yyTerminal, 2760) != 0)) yyGotoReduce (4904, yy2017)
else { yyGotoRead (1394)
}
case 5287: /* STATE 3518 */
if (yyPrintResult (* yyStateStackPtr, 2760, yyTrialParse (
42, yyTerminal, 2760) != 0)) yyGotoReduce (4904, yy2017)
else { yyGotoRead (1647)
}
case 5288: /* STATE 3574 */
/* line 753 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 753,
( GetLookahead (2) == 384             
))) yyGotoReduce (3111, yy224)
else { yyGotoRead (2128)
}
case 5289: /* STATE 3631 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5290: /* STATE 3640 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1517)
}
case 5291: /* STATE 3640 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1481)
}
case 5292: /* STATE 3640 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5293: /* STATE 3723 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1517)
}
case 5294: /* STATE 3723 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (1481)
}
case 5295: /* STATE 3723 */
if (yyPrintResult (* yyStateStackPtr, 2732, yyTrialParse (
33, yyTerminal, 2732) != 0)) yyGotoReduce (4876, yy1989)
else { yyGotoRead (87)
}
case 5296: /* STATE 3812 */
/* line 768 "Parser.lrk" */
if (yyPrintResult (* yyStateStackPtr, 768,
( GetLookahead (2) == 443        
))) yyGotoReduce (3126, yy239)
else { yyGotoRead (53)
}
default: switch (yyState) {
case 1: goto yyAbort;
case 2: goto yyRead;
case 3: goto yyReduce;
}
}

       /* SPEC State = Next (Top (), Nonterminal); nonterminal transition */
#ifdef YYNDefault
	       yyState = * yyStateStackPtr ++;
	       for (;;) {
		  register yytComb * yyNCombPtr =
				yyNBasePtr [yyState] + (int) yyNonterminal;
		  if (yyNCombPtr->Check == yyState) {
		     yyState = yyNCombPtr->Next; break;
		  }
		  yyState = yyNDefault [yyState];
	       }
#else
	       yyState = yyNBasePtr [* yyStateStackPtr ++] [yyNonterminal];
#endif
	       * yyAttrStackPtr ++ = yySynAttribute;
	       if (yyState < yyFirstFinalState) goto yyParseLoop;
							/* read reduce ? */
#ifdef YYDEBUG
	       yyStateStackPtr [0] = yyStateStackPtr [-1];
#endif
	    }

	 } else {					/* read */
   yyRead:  yyStateStackPtr ++;
	    yyGetAttribute (yyAttrStackPtr ++, Attribute);
	    yyTerminal = yyGetToken ();
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (yyStateStackPtr [-1]);
	       (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		  Parser_TokenName [yyPrevTerminal],
		  Parser_TokenName [yyTerminal]); yyNl ();
	       yyPrevTerminal = yyTerminal;
	    }
#endif
	    yyIsRepairing = rfalse;
	 }
      }

   yyAbort:
#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "fail    parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return ++ yyErrorCount;

   yyAccept:
#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "accept  parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return yyErrorCount;
   }

#ifndef NO_RECOVER
static void yyErrorRecovery
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange * yyTerminal, yyStateRange * yyStateStack, short yyStackPtr)
#else
   (yyTerminal, yyStateStack, yyStackPtr)
   yySymbolRange *	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
#endif
   {
#define	yyContinueSize	5000
      rbool	yyTokensSkipped	;
      tSet	yyContinueSet	;
      tSet	yyRestartSet	;
      int	yyLength	;
      char	yyContinueString [yyContinueSize + 2];

      if (yyControl.yyMessages) {
   /* 1. report an error */
	 ErrorMessage (xxSyntaxError, xxError, Attribute.Position);

   /* 2. report the offending token */
	 (void) strcpy (yyContinueString, Parser_TokenName [* yyTerminal]);
#ifdef SPELLING
	 if (strncmp (yyContinueString, TokenPtr, TokenLength)) {
	    yyContinueString [yyLength = strlen (yyContinueString)] = ' ';
	    (void) GetWord (& yyContinueString [++ yyLength]);
	 }
#endif
	 ErrorMessageI (xxTokenFound, xxInformation, Attribute.Position,
	    xxString, yyContinueString);

   /* 3. report the set of expected terminal symbols */
	 MakeSet (& yyContinueSet, (short) yyLastTerminal);
	 yyComputeContinuation (yyStateStack, yyStackPtr, & yyContinueSet);
	 yyLength = 0;
	 yyContinueString [0] = '\0';
	 while (! IsEmpty (& yyContinueSet)) {
	    char * yyTokenString = Parser_TokenName [Extract (& yyContinueSet)];
	    int yyl = strlen (yyTokenString);
	    if (yyLength + yyl >= yyContinueSize) break;
	    (void) strcpy (& yyContinueString [yyLength], yyTokenString);
	    yyLength += yyl;
	    yyContinueString [yyLength ++] = ' ';
	 }
	 yyContinueString [-- yyLength] = '\0';
	 ErrorMessageI (xxExpectedTokens, xxInformation, Attribute.Position,
	    xxString, yyContinueString);
	 ReleaseSet (& yyContinueSet);
      }

   /* 4. compute the set of terminal symbols for restart of the parse */
      MakeSet (& yyRestartSet, (short) yyLastTerminal);
      yyComputeRestartPoints (yyStateStack, yyStackPtr, & yyRestartSet);

   /* 5. skip terminal symbols until a restart point is reached */
      yyTokensSkipped = rfalse;
      while (! IsElement (* yyTerminal, & yyRestartSet)) {
#ifdef YYDEBUG
	 yySymbolRange yyPrevTerminal = * yyTerminal;
#endif
	 * yyTerminal = yyGetToken ();
	 yyTokensSkipped = rtrue;
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (yyStateStack [yyStackPtr]);
	    (void) fprintf (yyTrace, "skip    %s, lookahead: %s",
	       Parser_TokenName [yyPrevTerminal],
	       Parser_TokenName [* yyTerminal]); yyNl ();
	 }
#endif
      }
      ReleaseSet (& yyRestartSet);

   /* 6. report the restart point */
      if (yyTokensSkipped & yyControl.yyMessages)
	 ErrorMessage (xxRestartPoint, xxInformation, Attribute.Position);
   }

/*
   compute the set of terminal symbols that can be accepted (read)
   in a given stack configuration (eventually after reduce actions)
*/

static void yyComputeContinuation
#if defined __STDC__ | defined __cplusplus
   (yyStateRange * yyStack, short yyStackPtr, tSet * yyContinueSet)
#else
   (yyStack, yyStackPtr, yyContinueSet)
   yyStateRange *	yyStack		;
   short		yyStackPtr	;
   tSet *		yyContinueSet	;
#endif
   {
      register yySymbolRange	yyTerminal;
      register yyStateRange	yyState = yyStack [yyStackPtr];

      AssignEmpty (yyContinueSet);
      for (yyTerminal = yyFirstTerminal; yyTerminal <= yyLastTerminal;
							yyTerminal ++) {
	 if (yyNext (yyState, yyTerminal) != yyNoState &&
	    yyIsContinuation (yyTerminal, yyStack, yyStackPtr)) {
	    Include (yyContinueSet, (short) yyTerminal);
	 }
      }
   }

/*
   check whether a given terminal symbol can be accepted (read)
   in a certain stack configuration (eventually after reduce actions)
*/

static rbool yyIsContinuation
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyTerminal, yyStateRange * yyStateStack, short yyStackPtr)
#else
   (yyTerminal, yyStateStack, yyStackPtr)
   yySymbolRange	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
#endif
   {
      register yyStateRange	yState		;
      register yytNonterminal	yyNonterminal	;

      while (yyStackPtr >= (short) yyIsContStackSize)  /* pass Stack by value */
	 ExtendArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
			  (unsigned long) sizeof (yyStateRange));
#ifdef BCOPY
      bcopy ((char *) yyStateStack, (char *) yyIsContStackPtr,
		 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#else
      (void) memcpy ((char *) yyIsContStackPtr, (char *) yyStateStack,
			 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#endif

      yState = yyIsContStackPtr [yyStackPtr];
      for (;;) {
	 yyIsContStackPtr [yyStackPtr] = yState;
	 yState = yyNext (yState, yyTerminal);
	 if (yState == yyNoState) return rfalse;

	 do {						/* reduce */
	    if (yState > yyLastReduceState) {		/* dynamic ? */
	       yState = yyCondition [yState - yyLastReduceState];
	    }
	    if (yState <= yyLastStopState) { /* read, read reduce, or accept? */
	       return rtrue;
	    } else {					/* reduce */
	       yyStackPtr -= yyLength [yState - yyFirstReduceState];
	       yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
	    }

	    yState = yyNext (yyIsContStackPtr [yyStackPtr],
				(yySymbolRange) yyNonterminal);
	    if (yyStackPtr >= (short) yyIsContStackSize)
	       ExtendArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
				(unsigned long) sizeof (yyStateRange));
	    yyStackPtr ++;
	 } while (yState >= yyFirstFinalState);
      }
   }

/*
   compute a set of terminal symbols that can be used to restart
   parsing in a given stack configuration. we simulate parsing until
   end of file using a suffix program synthesized by the function (array)
   yyContinuation. All symbols acceptable in the states reached during
   the simulation can be used to restart parsing.
*/

static void yyComputeRestartPoints
#if defined __STDC__ | defined __cplusplus
   (yyStateRange * yyStateStack, short yyStackPtr, tSet * yyRestartSet)
#else
   (yyStateStack, yyStackPtr, yyRestartSet)
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
   tSet *		yyRestartSet	;
#endif
   {
      register yyStateRange	yState		;
      register yytNonterminal	yyNonterminal	;
	       tSet		yyContinueSet	;

      while (yyStackPtr >= (short) yyCompResStackSize) /* pass Stack by value */
	 ExtendArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			  (unsigned long) sizeof (yyStateRange));
#ifdef BCOPY
      bcopy ((char *) yyStateStack, (char *) yyCompResStackPtr,
		 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#else
      (void) memcpy ((char *) yyCompResStackPtr, (char *) yyStateStack,
			 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#endif

      MakeSet (& yyContinueSet, (short) yyLastTerminal);
      AssignEmpty (yyRestartSet);
      yState = yyCompResStackPtr [yyStackPtr];

      for (;;) {
	 if (yyStackPtr >= (short) yyCompResStackSize)
	    ExtendArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			     (unsigned long) sizeof (yyStateRange));
	 yyCompResStackPtr [yyStackPtr] = yState;
	 yyComputeContinuation (yyCompResStackPtr, yyStackPtr, & yyContinueSet);
	 Union (yyRestartSet, & yyContinueSet);
	 yState = yyNext (yState, yyContinuation [yState]);

	 if (yState >= yyFirstFinalState) {		/* final state ? */
	    if (yState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStackPtr ++;
	       yState = yyFinalToProd [yState - yyFirstReadReduceState];
#ifdef YYDCRP
	       yyCompResStackPtr [yyStackPtr] =
					yyCompResStackPtr [yyStackPtr - 1];
	       (void) fprintf (yyTrace, "%5d shift   %s\n",
		  yyCompResStackPtr [yyStackPtr],
		  Parser_TokenName [yyContinuation [yyCompResStackPtr
		  [yyStackPtr]]]);
#endif
	    }

	    do {					/* reduce */
	       if (yState > yyLastReduceState) {	/* dynamic ? */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d dynamic decision %d\n",
		  yyCompResStackPtr [yyStackPtr], yState - yyLastReduceState);
#endif
		  yState = yyCondition [yState - yyLastReduceState];
	       }
	       if (yyFirstReduceState <= yState &&
		   yState <= yyLastStopState) {		/* accept */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d accept\n",
		     yyCompResStackPtr [yyStackPtr]);
#endif
		  ReleaseSet (& yyContinueSet);
		  return;
	       } else if (yState < yyFirstFinalState) {	/* read */
		  goto yyRead;
	       } else {					/* reduce */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d reduce  %s\n",
		     yyCompResStackPtr [yyStackPtr],
		     yyRule [yState - yyLastReadReduceState]);
#endif
		  yyStackPtr -= yyLength [yState - yyFirstReduceState];
		  yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
	       }

	       yState = yyNext (yyCompResStackPtr [yyStackPtr],
				(yySymbolRange) yyNonterminal);
	       yyStackPtr ++;
	    } while (yState >= yyFirstFinalState);
	 } else {					/* read */
yyRead:
#ifdef YYDCRP
	    (void) fprintf (yyTrace, "%5d shift   %s\n",
	       yyCompResStackPtr [yyStackPtr],
	       Parser_TokenName [yyContinuation [yyCompResStackPtr [yyStackPtr]]]);
#endif
	    yyStackPtr ++;
	 }
      }
   }

/* access the parse table:   Next : State x Symbol -> Action */

static yyStateRange yyNext
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState, yySymbolRange yySymbol)
#else
   (yyState, yySymbol) yyStateRange yyState; yySymbolRange yySymbol;
#endif
   {
      if (yySymbol <= yyLastTerminal) {
	 for (;;) {
	    register yytComb * yyTCombPtr = yyTBasePtr [yyState] + yySymbol;
#if defined YYTDefault & defined YYaccDefault
	    register unsigned long * yylp;
#endif
	    if (yyTCombPtr->Check == yyState) return yyTCombPtr->Next;
#ifdef YYTDefault
#ifdef YYaccDefault
	    return (yylp = yyDefaultLook [yyState]) &&
	       (yylp [yySymbol >> 5] >> (yySymbol & 0x1f)) & 1 ?
		  yyTDefault [yyState] : yyNoState;
#else
	    if ((yyState = yyTDefault [yyState]) == yyNoState) return yyNoState;
#endif
#else
	    return yyNoState;
#endif
	 }
      }
#ifdef YYNDefault
      for (;;) {
	 register yytComb * yyNCombPtr = yyNBasePtr [yyState] + yySymbol;
	 if (yyNCombPtr->Check == yyState) return yyNCombPtr->Next;
	 yyState = yyNDefault [yyState];
      }
#else
      return yyNBasePtr [yyState] [yySymbol];
#endif
   }
#endif

void BeginParser ARGS ((void))
   {
/* line 53 "Parser.lrk" */


   iCURRENT_DATE	= MakeIdent ("CURRENT-DATE"	, 12);
   iWHEN_COMPILED	= MakeIdent ("WHEN-COMPILED"	, 13);


   }

void CloseParser ARGS ((void))
   {
/* line 60 "Parser.lrk" */


   }


