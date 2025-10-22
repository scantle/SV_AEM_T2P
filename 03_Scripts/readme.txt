Required python packages can be installed by creating a conda environment using anaconda:
https://www.anaconda.com/docs/getting-started/miniconda/main

And creating a python package running:
conda env create -f conda_env.yml

from the main project folder.

The scripts generally assume you are running them from the main project folder.
It is suggested you run them in order (01, 02, 03...) as they create files that subsequent scripts
rely on. You may additionally need to run models to generate necessary files.

Most of the "magic" happens in the first script, "01_AEM_Categorize_Cluster.py"
Many of the other scripts (particularly those that do not start with a number) should be considered
as utilities - they can be repurposed for a given project, but may not immediately work without
some customization. Happy coding!