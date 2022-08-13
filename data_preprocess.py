from feat.utils import read_feat, read_openface
from feat.data import Fex
import pandas as pd
import re
from sklearn import preprocessing
from data_collect import FACS


class Preprocess(FACS):
    def __init__(self, root, dest, frames=30, au_model="rf", emotion_model="rf") -> None:
        super().__init__(root, dest, frames, au_model, emotion_model)
        self.fex = pd.DataFrame()
        self.concatenate_files()

    # remove baseline median face value from the facial expressions
    def baseline(self, data):
        # remove baseline using Fex Function
        normalised_data  = data.aus().baseline(baseline = "median")

        # normalize the data using MinMax
        scaler = preprocessing.MinMaxScaler()
        names = normalised_data.columns
        d = scaler.fit_transform(normalised_data)
        scaled_df = pd.DataFrame(d, columns=names)
        # copy scaled au columns to original dataset
        data[names] = scaled_df[names]
        return data

    # convert percentile scores to binary variable
    def binary(self, row):
        if isinstance(row,float):
            return None
        if int(row)>=50:
            return 'High'
        else:
            return 'Low'

    # collect preprocessed data for BFI-44 self assessment
    def process_bfi44(self):
        bfi_44 =  pd.read_excel("Personality test\BFI-44-assessment.xlsx",
                            sheet_name="Raw and Percentile scores-ALL")
        bfi_44 = bfi_44.drop([1,2,3,4])

        # get binary values for each trait
        bfi_44['extraversion_score'] = bfi_44['Unnamed: 4'].apply(self.binary)
        bfi_44['agreeableness_score'] = bfi_44['Unnamed: 6'].apply(self.binary)
        bfi_44['conscientious_score'] = bfi_44['Unnamed: 8'].apply(self.binary)
        bfi_44['neurotcism_score'] = bfi_44['Unnamed: 10'].apply(self.binary)
        bfi_44['openness_score'] = bfi_44['Unnamed: 12'].apply(self.binary)
        bfi_44 = bfi_44.drop(['Unnamed: 0','Unnamed: 2', 'EXTRAVERSION', 'Unnamed: 4',
            'AGREEABLENESS', 'Unnamed: 6', 'CONSCIENTIOUSNESS', 'Unnamed: 8',
            'NEUROTICISM', 'Unnamed: 10', 'OPENNESS', 'Unnamed: 12'], axis=1)
        bfi_44= bfi_44.iloc[1:41,:]

        bfi_44 = bfi_44.rename(columns={'Unnamed: 1':'person'})
        bfi_44.dropna(inplace=True)

        return bfi_44
        
    # collect data for BFI-10 other-personality assessment
    def collect_bfi_10(self):
        bfi10 =  pd.read_excel("Personality test\BFI-10-assessment.xlsx", 
                            sheet_name=1)
        gender  = bfi10.iloc[:2,:]
        gender.drop(list(gender.filter(regex = 'Unnamed')), axis = 1, inplace = True)
        gender = gender.iloc[:,5:]

        df = gender.T
        df.rename(columns={0:'id', 1:'gender'}, inplace=True)
        df['person'] = df['id'].apply(lambda x: re.findall(r'P\d{3}', x)[0])
        df.drop('id', axis=1, inplace=True)

        return df

    # read csv files of pyfeat detector
    def read_pyfeat_csv(self, filename):
        return read_feat(filename)   

    # read csv files of openface detector
    def read_openface_csv(self, filename):
        return read_openface(filename)

    # remove all the extra columns
    # columns required for this project -
    # person name, aus, emotions, opposite person
    def modify_columns(self, data):
        # select only the required columns
        self.data = self.data[['frame',	'AU01', 'AU02',	'AU04',	'AU05',	'AU06',	'AU07', 'AU09', 'AU10',
                 'AU11','AU12', 'AU14', 'AU15', 'AU17', 'AU20', 'AU23', 'AU24', 'AU25', 
                 'AU26', 'AU28', 'AU43', 'anger','disgust', 'fear', 'happiness', 'sadness',
                 'surprise', 'neutral', 'input']]
        
        # fetch person and opposite person name string from the input column
        self.data['person'] = self.data['input'].apply(lambda x : re.findall(r'P\d+', x)[0])
        self.data['opposite_person'] = self.data['person'].apply(lambda x:  x[0]+str(int(x[2:])+1).zfill(3))
        self.data.drop(['input'], axis=1, inplace=True)
        self.data.insert(0, 'person', self.data.pop('person'))

    # concatenate all the session files
    def concatenate_files(self, type="pyfeat", baseline=False):
        first = 0
        self.get_all_videos(self.dest)
        if type == "pyfeat":
            for video, filename in self.videos.items():  
                print(filename)
                data = self.modify_columns(self.read_pyfeat_csv(filename=filename)) 
                if baseline:
                    data = self.baseline(data)
                if first == 0: 
                    self.fex = data
                    first = 1
                else:
                    self.fex = pd.concat([self.fex, data])

        elif type == "openface":
            for video, filename in self.videos.items(): 
                self.data = self.read_openface_csv(filename=filename)  
                self.modify_columns(self.data)             
                if first == 0: 
                    self.fex = self.data
                    first = 1
                else:
                    self.fex = pd.concat([self.fex, self.data])

        else:
            print("Invalid values. Give [\"openface\", \"pyfeat\"]")
        self.fex.dropna(inplace=True)
        # collect personality scores preprocessed data
        bfi_44 = self.process_bfi44()
        bfi10 = self.collect_bfi_10()
        bfi = pd.merge(bfi10, bfi_44, how="inner", on="person")

        # merge bfi personality scores with the whole dataset person wise
        final_dataset = pd.merge(self.fex, bfi, how="inner", on='person')
        self.save_csv(final_dataset, baseline=baseline)
    
    # save data as csv
    def save_csv(self, data, baseline=False):
        if baseline:
            data.to_csv('Processed_RF\VIDEO_LOW_QUALITY\VIDEO_LOW_QUALITY\normalized_dataset_rf')
        else:
            data.to_csv('Processed_RF\VIDEO_LOW_QUALITY\VIDEO_LOW_QUALITY\dataset_rf')
