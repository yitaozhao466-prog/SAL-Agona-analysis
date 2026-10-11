import pandas as pd
import argparse
import time


parser = argparse.ArgumentParser(description="Generate virulence and resistance gene pivot tables separately,'pivot_VF.txt' and 'pivot_AMR.txt' will be generated ")
parser.add_argument('-i','--input',nargs='+',type=str,help='Input effectors txt, support one or more files')
args = parser.parse_args()

print(time.asctime(time.localtime(time.time())))

df_all =pd.DataFrame()
df_name = pd.DataFrame()
for filename in list(args.input):
    df = pd.read_csv(filename, sep = "\t")
    row,col = df.shape
    df2 = pd.DataFrame([filename] * row)
    df_name = pd.concat([df_name,df2],axis=0,ignore_index=True)
    df_all = pd.concat([df_all,df],axis=0,ignore_index=True)

df_name.columns = ['Name']
df_compose = pd.concat([df_all,df_name],axis=1)
print(df_compose)
vf_df = df_compose.loc[df_compose['category'] == "VIRULENCE"]
amr_df = df_compose.loc[df_compose['category'] == "AMR"]


print("Generating virulence gene pivot table")
vf_pivot = vf_df.pivot_table(values='category',index='Name',columns='gene',aggfunc=len, fill_value=0)
vf_pivot.to_csv('pivot_VF.txt',sep='\t')
print("Generating drug resistance gene pivot table")
amr_pivot = amr_df.pivot_table(values='category',index='Name',columns='gene',aggfunc=len, fill_value=0)
amr_pivot.to_csv('pivot_AMR.txt',sep='\t')
print("Finished!")
print(time.asctime(time.localtime(time.time())))

