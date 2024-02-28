*******************************
Building Python for GNAT Studio
*******************************

Dependencies
============

GNAT Studio and its default plugins requires the following python packages:

* :file:`cmsis_pack_manager` (0.2.10)

  cmsis-pack-manager is a python module, Rust crate and command line utility
  for managing current device information that is stored in many CMSIS PACKs.

* :file:`coverage` (5.3)

  Coverage.py measures code coverage, typically during test execution.
  It uses the code analysis tools and tracing hooks provided in the Python
  standard library to determine which lines are executable, and which have
  been executed.

* :file:`jedi` (0.11.1)

  Jedi is a static analysis tool for Python that is typically used in
  IDEs/editors plugins. Jedi has a focus on autocompletion and goto
  functionality. Other features include refactoring, code search and
  finding references.

* :file:`pycairo` (1.25.0)

  Pycairo is a Python module providing bindings for the cairo graphics
  library.

* :file:`pycodestyle` (2.6.0)

  pycodestyle is a tool to check your Python code against some of the style
  conventions in PEP 8.

* :file:`pynput` (1.7.3)

  This library allows you to control and monitor input devices.

* :file:`pyocd` (0.25.0)

  pyOCD is an open source Python package for programming and debugging
  Arm Cortex-M microcontrollers using multiple supported types of USB debug
  probes. It is fully cross-platform, with support for Linux, macOS
  and Windows.

* :file:`PyGObject` (3.42.2)

  PyGObject is a Python package which provides bindings for GObject based
  libraries such as GTK, GStreamer, WebKitGTK, GLib, GIO and many more.

* :file:`pyusb` (1.2.1)

  PyUSB offers easy USB devices communication in Python. It should work
  without additional code in any environment with Python >= 3.6, ctypes
  and a pre-built USB backend library (currently: libusb 1.x, libusb 
  0.1.x or OpenUSB).

.. note::
   This package will only be found on Linux. For Windows use :file:`pywinusb` (0.4.0)

* :file:`PyYAML` (5.3.1)

  YAML is a data serialization format designed for human readability and
  interaction with scripting languages. PyYAML is a YAML parser and emitter
  for Python.


* :file:`zmq` (0.0.0)

  PyZMQ provides Python bindings for libzmq.


Building wheels
===============

On Linux, all the wheels are prebuilt and can be downloaded by executing
``pip wheel ${package}=={version}``.

On Windows, PyGObject is not prebuilt. Thus trying to download the wheel
will trigger a local build. The recommended way to build it is using ``msys2``.
For ``cygwin`` users, it's still possible to create the wheel with the
following steps:

#. Install the wanted python3.X thirdparty
#. In :file:`distutils.cfg`, modify the ``[build]`` section ``compiler`` field to ``mingw32``
#. In :file:`ccompiler.py`, modify the ``default_compilers`` map to have ``'nt'`` returning ``'mingw32'``
#. In :file:`cygwincompiler.py`, modify ``get_msvcr`` to return ``['mscvr100']`` or ``['']``.
   The first case will require to install Microsoft Visual C++ 2014.
#. (Optional) Define _MSC_VER macro if it's missing
#. Download `PyGObject archive <https://pypi.org/project/PyGObject/#files>`_.
#. Build PyGObject by running ``python setup.py build``
#. Create the wheel by running ``python setup.py bdist_wheel``


Python transition
=================

When changing the python version, the steps below are recommended.
They are assuming the user have access to e3, gnat and the targeted python
in PYTHON_DIST environment variable.

*  Build GNATCOLL.Bindings with the new python version and run its internal
   testsuite.

.. code-block:: bash

   git clone git@ssh.gitlab.adacore-it.com:eng/toolchain/gnatcoll-bindings.git
   cd gnatcoll-bindings/python3
   ./setup.py build
   ./setup.py install
   cd tests
   ADA_PYTHON_HOME=${PYTHON_DIST} ./testsuite.py

*  Have a look at `Python Whats New <https://docs.python.org/3/whatsnew/index.html>`_. Especially for the change in the
   standard API and CPython. Update GNAT Studio python API and plug-ins accordingly.

*  Build the wheels for new python version. It must be done on Linux and
   Windows 64 bits. See :title:`Dependencies` section, the versions should be
   enforced to avoid incompatibility with the packaged libraries, especially
   GTK.

.. code-block:: bash

   pip wheel cmsis_pack_manager==0.2.10
   pip wheel coverage==5.3
   pip wheel jedi==0.11.1
   pip wheel pycairo==1.25.0
   pip wheel pycodestyle==2.6.0
   pip wheel pynput==1.7.3
   pip wheel pyocd==0.25.0
   pip wheel PyYAML==5.3.1
   pip wheel zmq==0.0.0

Extra commands for Linux:

.. code-block:: bash

   pip wheel pyusb==1.2.1
   pip wheel PyGObject==3.42.2

Extra commands for Windows:

.. code-block:: bash

   pip wheel pywinusb==0.4.0
   # Download PyGobject archive and assuming PYTHON_DIST has been modified
   # to use mingw32
   cd PyGObject-3.42.2
   python setup.py build
   python setup.py bdist_wheel
   # add dist/PyGObject-3.42.2-cp311-cp311-win_amd64.whl to the wheel archive

.. warning::
   Downloading a wheel will also download wheels for its dependencies which will
   results into multiple wheels for the same package with different versions.
   They must be manually purged and only the versions defined in :title:`Dependencies`
   should be kept.

*  Update the anod specs by changing the python version and upload the new
   wheel archive.

*  Modify GNAT Studio configure script and add the new python version the
   existing list in ``######### checking for python`` section.

*  Build and Run GNAT Studio testsuite on ``edge`` for both platforms.

.. code-block:: bash

   anod vcs --add-repo gnatcoll-bindings ${path_to_gnatcoll_bindings}
   anod vcs --add-repo gnatstudio-edge ${path_to_gnatstudio_edge}
   anod build gnatcoll-bindings
   anod build python4gnatstudio
   anod build gps --qualifier=edge
   anod test gps --qualifer=edge
