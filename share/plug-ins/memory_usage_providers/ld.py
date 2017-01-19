import GPS
from . import core


xml = """
<filter name="ld_supports_map_file" shell_lang="python"
        shell_cmd="memory_usage_providers.ld.LD.map_file_is_supported(
GPS.current_context())" />
"""


@core.register_memory_usage_provider("LD")
class LD(core.MemoryUsageProvider):

    _cache = {}

    def async_fetch_memory_regions(self, visitor):
        # TODO: parse the generated map file to fetch the memory regions
        regions = []
        visitor.on_memory_regions_fetched(regions)

    @staticmethod
    def map_file_is_supported(context):
        """
        The filter used to know if the ld linker supports the '-map' switch.
        """

        target = GPS.get_target()
        build_mode = GPS.get_build_mode()

        v = LD._cache.get((target, build_mode), None)
        if v is not None:
            return v

        if not target or target == 'native' or build_mode != 'default':
            return False

        ld_exe = target + '-ld'

        try:
            process = GPS.Process([ld_exe, '--help'])
            output = process.get_result()
            v = '-map' in output
        except:
            v = False

        LD._cache[(target, build_mode)] = v

        return v


GPS.parse_xml(xml)
