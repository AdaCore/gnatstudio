"""
Verify that TextMate themes can control semantic highlighting:

1. Override a default semantic style (variable.globalvariable).
2. Add a new semantic style absent from the base theme (namespace).
3. Add style_overrides/ custom semantic theme on top of a
built-in theme (Darkside).

test.cmd installs test_semantic.tmTheme in ~/.gnatstudio/themes/
and namespace_override.tmTheme in ~/.gnatstudio/style_overrides/.
"""

import GPS
from gs_utils.internal.utils import *
import colorschemes

# Timeout for semantic highlighting to be
# fully applied  after opening the file
SEMANTIC_HIGHLIGHT_TIMEOUT = 1000


@run_test_driver
def test_driver():
    GPS.Preference("LSP-Semantic-Highlighting").set(True)
    yield wait_tasks()

    # Reload themes so the freshly installed .tmTheme is picked up
    colorschemes.themes = []

    theme = None
    for t in colorschemes.get_themes():
        if t.name == "Test Semantic Theme":
            theme = t
            break
    gps_assert(theme is not None, True, "Test Semantic Theme not found")

    # The .tmTheme must produce entries for these semantic styles;
    # 'namespace' is particularly important: it has no base-theme default.
    for key in [
        "function",
        "variable",
        "variable.readonly",
        "variable.globalvariable",
        "namespace",
    ]:
        gps_assert(
            key in theme.d, True, "Theme missing expected semantic style '%s'" % key
        )

    # Apply the custom theme and check editor overlays
    colorschemes.the_theme_switcher.apply_theme(theme)
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("p.adb"))
    yield timeout(SEMANTIC_HIGHLIGHT_TIMEOUT)

    tags = get_all_tags(buf)

    # Overridden default: 'H' is a global constant
    gps_assert(
        "variable-globalvariable" in tags,
        True,
        "variable-globalvariable tag not found; tags:\n" + tags,
    )

    # New style with no base-theme default: 'Ada.Text_IO'
    gps_assert("namespace" in tags, True, "namespace tag not found; tags:\n" + tags)

    # --- style_overrides/ on top of a built-in theme ---

    darkside = None
    for t in colorschemes.get_themes():
        if t.name == "Darkside":
            darkside = t
            break
    gps_assert(darkside is not None, True, "Darkside theme not found")

    colorschemes.the_theme_switcher.apply_theme(darkside)
    yield wait_tasks()

    # Overrides must not permanently modify the theme dict
    gps_assert(
        "namespace" not in darkside.d,
        True,
        "Darkside.d must stay pristine after apply_theme",
    )

    # The override file should still produce the namespace overlay
    buf = GPS.EditorBuffer.get(GPS.File("p.adb"))
    yield timeout(SEMANTIC_HIGHLIGHT_TIMEOUT)

    tags = get_all_tags(buf)
    gps_assert(
        "namespace" in tags,
        True,
        "namespace tag not found after Darkside + override; " "tags:\n" + tags,
    )
