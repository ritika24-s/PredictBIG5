import os
from pprint import pprint
from feat import Detector


"""
This module runs the Library Py-Feat to detect FEX in a video (or a list of videos) present in
the directory and the result dataframe is saved as a .csv file in the destination directory

The class FACS contains the following attribute:
root : root directory which contains all the videos
dest : destination directory which will contain all the csv files but in the same order as root directory
frames (default = 30) : defines the number of frames to skip in the function detect_video()
au_model (default = "rf") : defines the type of model to run for action units
emotion_model (default = "resmasknet") : defines the type of model to run to detect emotions


The class FACS contains the following methods :
create_directory(dir) : checks if the directory to store csv files exists or not, if not then is created
get_all_videos(dir)   : generates a dictionary self.videos to store all the video location as the
                        key and csv location as the value
generate_features()   : this function is used to call the detector function of the py-feat library
                        to detect all the FEX from the video
"""
class FACS:
    def __init__(self, root, dest, frames = 1, au_model="rf", emotion_model = "resmasknet") -> None:
        self.detector = Detector(au_model = au_model, emotion_model = emotion_model)
        self.videos = {}
        self.root = root
        self.dest = dest
        self.frames = frames

        # call all functions
        self.create_directory(self.dest)
        self.get_all_videos(self.root)
        self.generate_features()
        pprint(self.videos)


    def get_all_videos(self, loc):
        # creates a dictionary with key as all the videos that need to be processed
        # value is the destination location of the .csv file
        for file in os.listdir(loc):
            subdir = os.path.join(loc, file)
            if os.path.isdir(subdir):
                self.get_all_videos(subdir)
            elif os.path.isfile(subdir) and subdir.endswith('.mov'):
                self.videos[subdir] = file.replace('.mov', '.csv')


    def create_directory(self, loc):
        # check if the destination directory exists,
        # if not then create one
        if not os.path.exists(loc):
            os.makedirs(loc)
            print("created directory ", loc)


    def generate_features(self):
        # call the detector function of the py-feat library to detect all the FEX from the video

        for video, output in self.videos.items():
            # add desired destination to output file name
            dir = os.path.join(self.dest, (os.path.dirname(video) + "_"+str(self.frames)+ "_frames"))
            output = os.path.join(dir, output) 

            # check if the output file already exists
            if os.path.exists(output):
                print("Already processed ", video)
            else:
                # if the output file doesn't exist, first check for the current directory
                self.create_directory(dir)
                # assert os.path.isfile(output)

                with open(output, "w") as file:
                    print("Processing ", video)
                    self.detector.detect_video(video, output, skip_frames=self.frames)


ROOT = "VIDEO_LOW_QUALITY"
DEST = "Processed"
facs = FACS(ROOT, DEST)
