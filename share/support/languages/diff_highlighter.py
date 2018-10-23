from highlighter.common import register_highlighter, region, existing_style, \
    tag_keyword, simple


file_tag = existing_style("Diff-Patch-File-Header-Variant", "file-diff")
code_tag = existing_style("Diff-Patch-Code-Header-Variant", "code-diff")
added_tag = existing_style("Diff-Patch-Append-Variant", "added-diff")
removed_tag = existing_style("Diff-Patch-Remove-Variant", "removed-diff")


register_highlighter(
    language="diff",
    spec=(
        # Match file section
        region(r"^diff", "\n", tag=file_tag),
        # Match code section
        region(r"^\@\@", "\n", tag=code_tag),
        # Match added line
        region(r"^\+", "\n", tag=added_tag),
        # Match removed line
        region(r"^\-", "\n", tag=removed_tag),
        # Match header keyword
        simple(r"^[\w|-]+:", tag=tag_keyword)
    )
)
