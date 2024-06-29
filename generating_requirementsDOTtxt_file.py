# -*- coding: utf-8 -*-
"""generating_requirementsDOTtxt_file.ipynb

Automatically generated from Colab.
"""

'''Because i have my environment used directly from the google colab server,
I found no need to create a virtual enviroment for my local system

This script helps generate the requirements.txt file for your notebook (ipynb) by using pipreqsnb even when i loaded my libraries from the seerver.
'''
#!pip install pipreqsnb   # Uncommment if from server

from google.colab import files
uploaded = files.upload() #This creates an upload button, choose the notebook file and upload.


notebook_name = 'malaria_detector_AI_model.ipynb' # Rename this value of the notebook_name variable to the name of the notebook file.
#!pipreqsnb $notebook_name # Uncomment if from server

files.download('/content/requirements.txt') # This automatically downloads the requrements.txt file

