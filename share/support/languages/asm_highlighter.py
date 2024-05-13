"""
Provides a basic highlighter for all assembly languages.
"""
from highlighter.common import tag_keyword, region, tag_comment, \
        hl_comment_notes, hl_inside_strings, simple, tag_number, \
        register_highlighter, tag_string, tag_block, tag_type
from gs_utils import hook


@hook('gps_started')
def on_gps_started():
    spec = (
            # Match comments lines
            region(r"#", "\n", tag=tag_comment,
                   highlighter=(hl_comment_notes,)),

            # Match function names
            simple(r"^[-\w\d+_:]+\:$", tag=tag_block),

            # Match instructions
            simple(r"^\s*\w+", tag=tag_keyword),

            # Match section names
            simple(r"[^\w]\.\w+", tag=tag_type),

            # Match number literals
            simple(r"\b[0-9]*\.?[0-9]+\b", tag=tag_number),

            # Match strings
            region(
                r'"', r'"|[^\\]$',  tag=tag_string,
                highlighter=(hl_inside_strings,)
                )
            )
    register_highlighter(
        language="asm,asm2,asm_cpp",
        spec=spec
    )
