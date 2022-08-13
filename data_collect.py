import os
from pprint import pprint
from feat import Detector
from matplotlib import pyplot as plt
import pandas as pd

"""
This module runs the Library Py-Feat to detect FEX in a video (or a list of videos) present in
the directory and the result dataframe is saved as a .csv file in the destination directory

The class FACS contains the following attribute:
root : root directory which contains all the videos
dest : destination directory which will contain all the csv files but in the same order as root directory
frames (default = 30) : defines the number of frames to skip in the function detect_video()
au_model (default = "rf") : defines the type of model to run for action units
emotion_model (default = "rf") : defines the type of model to run to detect emotions


The class FACS contains the following methods :
create_directory(dir) : checks if the directory to store csv files exists or not, if not then is created
get_all_videos(dir)   : generates a dictionary self.videos to store all the video location as the
                        key and csv location as the value
generate_features()   : this function is used to call the detector function of the py-feat library
                        to detect all the FEX from the video
"""
class FACS:
    def __init__(self, root, dest, frames =30, au_model = "rf", emotion_model = "rf") -> None:
        self.detector = Detector(au_model=au_model, emotion_model=emotion_model)
        self.videos = {}
        self.root = root
        self.dest = dest
        self.frames = frames
    
    # main function to call other functions. only called once in the entire run to generate features
    # in general functions are called from other child classes 
    def start_collection(self):
        # call all functions
        self.create_directory(self.dest)
        self.get_all_videos(self.root)
        self.generate_features()
        # pprint(self.videos)

    def get_all_videos(self, loc):
        # creates a dictionary with key as all the videos that need to be processed
        # value is the destination location of the .csv file

        for file in os.listdir(loc):
            subdir = os.path.join(loc, file)

            if os.path.isdir(subdir):
                self.get_all_videos(subdir)

            elif os.path.isfile(subdir) and subdir.endswith('front-video_Z_S_L.mov'):
                output = file.replace('.mov', '.csv')
                # add desired destination to output file name
                dir = os.path.join(self.dest, (os.path.dirname(subdir) + "_"+str(self.frames)+ "_frames"))

                # check if the current directory exists or not
                self.create_directory(dir)
                output = os.path.join(dir, output) 

                # add filename to the videos dictionary
                self.videos[subdir] = output

    def create_directory(self, loc):
        # check if the destination directory exists,
        # if not then create one
        if not os.path.exists(loc):
            os.makedirs(loc)
            print("created directory ", loc)

    def generate_features(self):
        # call the detector function of the py-feat library to detect all the FEX from the video

        for video, output in self.videos.items():
            # check if the output file already exists
            if os.path.exists(output):
                print("Already processed ", video)
            else:
                # assert os.path.isfile(output)

                with open(output, "w") as file:
                    print("Processing ", video)
                    self.detector.detect_video(video, outputFname= output, skip_frames=self.frames)


    # def plot_graph(self):
        
    #     for video, filename in self.videos.items():
    #         print(filename)
    #         # self.read_processed_csv_files(filename=filename)
    #         # image = filename.replace('csv','png')
    #         # figure = self.output.plot_detections()
    #         # figure.savefig(image)
        