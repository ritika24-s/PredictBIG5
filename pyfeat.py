from feat import Detector
import os
import re

class FACS:
    def __init__(self, root, au_model="rf", emotion_model = "resmasknet") -> None:
        self.detector = Detector(au_model = au_model, emotion_model = emotion_model)
        self.videos = {}
        self.root = root

        # call all functions
        self.get_all_videos(self.root)
        self.generate_features()
        # print(self.videos)


    def get_all_videos(self, dir):
        for file in os.listdir(dir):
            subdir = os.path.join(dir, file)
            if os.path.isdir(subdir):
                self.get_all_videos(subdir)
            elif subdir.endswith('all_video_Z_S_L.mov'):   
                self.videos[subdir] = subdir.replace('.mov','.csv')
        

    def generate_features(self):
        for video, output in self.videos.items():
            if os.path.exists(output):
                print("Already processed ", video)
            else:
                print("Processing ", video)
                self.detector.detect_video(video, output, skip_frames=30)

root = "VIDEO_LOW_QUALITY"
facs = FACS(root)