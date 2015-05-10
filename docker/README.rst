Building the UCD-API Docker container
=====================================

There are three Docker containers defined: run.docker defines the actual
container used to run UCD, build.docker is used to build the wheels required
for the "run" container, and base.docker is used to define a base container
which is shared between the "build" and "run" containers as an optimization.

Build process
-------------

1. Build the base container.

   .. code-block:: ShellSession

      $ docker build -t mithrandi/ucd-base -f docker/base.docker .

2. Build the build container.

   .. code-block:: ShellSession

      $ docker build -t mithrandi/ucd-build -f docker/build.docker .

3. Run the build container to populate the Halcyon cache.

   .. code-block:: ShellSession

      $ docker run --rm -ti -v "${PWD}:/build" -v "${PWD}/halcyon-cache:/var/tmp/halcyon-cache" mithrandi/ucd-build

   Halcyon will cache a number of things, but the important part is the
   "install" files that get cached; this allows for building the "run"
   container without any of the build tools being included in the final
   container image.

4. Build the run container.

   .. code-block:: ShellSession

      $ docker build -t mithrandi/ucd -f docker/run.docker .
