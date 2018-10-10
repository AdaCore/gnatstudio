from highlighter.common import register_highlighter, region, existing_style


file_tag = existing_style("Diff-File-Header-Color", "file-diff")
code_tag = existing_style("Diff-Code-Header-Color", "code-diff")
added_tag = existing_style("Diff-Append-Color", "added-diff")
removed_tag = existing_style("Diff-Remove-Color", "removed-diff")


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
        region(r"^\-", "\n", tag=removed_tag)
    )
)
