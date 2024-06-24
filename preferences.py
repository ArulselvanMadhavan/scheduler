from utils import GUESTS
from utils import TOTAL_CHORES_NAMES, TOTAL_CHORES_SET
import os
import csv
import pandas as pd

HEADER=["task", "pref(0,1,2)"]
PREF_DIR = "preferences"

def create_prefs(file_name):
    rows = []
    for c in TOTAL_CHORES_NAMES:
        rows.append([c, 1])
    with open(file_name, "w+") as f:
        w = csv.writer(f)
        w.writerow(HEADER)
        w.writerows(rows)

def merge_prefs(file_name):
    df = pd.read_csv(file_name)
    df = df[HEADER]             # select only the columns of interest
    tasks_saved = set(df["task"].to_list())
    new_prefs = []
    for c in TOTAL_CHORES_SET:
        if c in tasks_saved:
            continue
        else:
            new_prefs.append([c, 1])
    new_df = pd.DataFrame(data=new_prefs, columns=HEADER)
    updated_df = pd.concat([df, new_df])
    updated_df.to_csv(file_name, index=False)

def make_tasks():
    for g in GUESTS:
        file_name = f"{PREF_DIR}/{g}.csv"
        if os.path.isfile(file_name):
            merge_prefs(file_name)
        else:
            create_prefs(file_name)

if __name__ == "__main__":
    try:
        os.makedirs(PREF_DIR, exist_ok=True)
    except OSError:
        None
    make_tasks()
