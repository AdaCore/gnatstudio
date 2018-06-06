"""
Support file for libclang integration. In order to analyze files correctly with
libclang, we need to get the compiler's search paths.

This file implements a generic function that will spawn the compiler and get
back the search paths
"""
import GPS
import os.path
import subprocess
import re

paths_cache = {}


def get_compiler_search_paths(project_name, language,
                              logger=None, use_cache=True):

    def is_known_compiler(compiler):
        """Return True is we can assume that this is a compiler of the gcc/g++
           family, from which we can extract predefined paths"""
        return any(name in compiler
                   for name in ['gcc', 'clang', 'clang++', 'c++', 'g++'])

    logger = logger or GPS.Logger("LIBCLANG.SEARCH_PATHS")

    # First find the driver.
    # Heuristic for finding the driver:
    #    A) we look for an explicitly set Compiler'Driver setting
    #    B) if this is not explicitely set, we use
    #         <target>gcc for C
    #         <target>c++ for C++

    ccs = {'c': 'gcc', 'c++': 'c++'}
    "map of languages -> compilers"

    # Put the language in lowercase
    language = language.lower()

    compiler = ''
    try:
        logger.log('Trying to get the Compiler.Driver attribute for the '
                   'project')
        compiler = GPS.Project(project_name).get_attribute_as_string(
            'driver', package='compiler', index=language
        )
    except Exception:
        logger.log(
            'No project {}, trying to determine the compiler in a project'
            'agnostic way'.format(project_name)
        )

    if not compiler:
        if language in ccs:
            compiler = "-".join(filter(bool,
                                       [GPS.get_target(), ccs[language]]))
        else:
            return ''

    # Normalize the compiler path, needed for instance on Windows to transform
    # forward slashes coming from the project. Do this if the compiler is not
    # a basename already.
    if os.path.basename(compiler) != compiler:
        compiler = os.path.abspath(compiler)

    logger.log('Compiler: {}'.format(compiler))

    # We use a tuple (compiler, language) for the cache, because it is possible
    # that the user defined the same driver for both C and C++. The cache needs
    # to be able to distinguish between the two
    if use_cache:
        ret = paths_cache.get((compiler, language), None)
        if ret:
            logger.log('Returning cached search paths: {}'.format(ret))
            return ret

    # Spawn the compiler, get the include paths
    if is_known_compiler(compiler):
        try:
            logger.log('Spawning {} to find the search paths'.format(compiler))
            cmd = "echo | {} -x {} -E -v -".format(compiler, language)
            logger.log("Compiler command : {}".format(cmd))

            out = subprocess.check_output(
                cmd, shell=True, stderr=subprocess.STDOUT,
                # This is needed for consoleless processes under windows
                # - OB11-026
                stdin=subprocess.PIPE
            )

            logger.log("Compiler's output: {}".format(out))

            m = re.findall(r'\> search starts here:(.*) ?End',
                           out, re.DOTALL)[0]
            ret = map(str.strip, m.strip().splitlines())

        except Exception as e:
            import traceback
            logger.log('Spawning failed !')
            logger.log(traceback.format_exc(e))
            ret = []
    else:
        logger.log('Compiler {} not known, not spawning it'.format(compiler))
        ret = []

    logger.log('Returning {}'.format(ret))

    # NOTE: Since the spawning logic is *exactly* the same each time, we'll
    # cache the results *even when spawning failed*, so that we don't try to
    # spawn executables repeatedly
    paths_cache[(compiler, language)] = ret

    return ret


GPS.__get_compiler_search_paths = get_compiler_search_paths
