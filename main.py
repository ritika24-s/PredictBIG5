from data_preprocess import Preprocess
import sys

if __name__ == "__main__":
    ROOT = "VIDEO_LOW_QUALITY"
    
    # to generate data using RF model
    def rf():
        DEST = "Processed_RF"
        au_model = "rf"
        emotion_model = "rf"
        preprocess = Preprocess(root=ROOT, dest=DEST, 
                            au_model=au_model,
                            emotion_model=emotion_model)
    
    def svm():
        DEST = "Processed_svm"
        au_model = "svm"
        emotion_model = "svm"
        preprocess = Preprocess(root=ROOT, dest=DEST, 
                            au_model=au_model,
                            emotion_model=emotion_model)
    
    if len(sys.argv) == 1 or sys.argv[1] == 'rf':
        rf()
    if len(sys.argv) == 1 or sys.argv[1] == 'svm':
        svm()
    else:
        print('Argument must be either - rf/svm')
        exit()