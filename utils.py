import sys
import pandas as pd

DAYS_ABBR = [
    "sun",
    "mon",
    "tue",
    "wed",
    "thu",
    "fri",
    "sat"
]
DAYS_DICT = {DAYS_ABBR[d]:d for d in range(len(DAYS_ABBR))}

EVERYDAY_CHORES_NAMES = [
    "AM_clean",
    "cook_1",
#    "cook_2",
    "PM_clean_1",
    "PM_clean_2"
]

EVERYDAY_CHORES_HOURS = [
    0.5,
    2,
#    2,
    1,
    1
]

PRIORITY_MIN = 1
PRIORITY_MAX = 5
EVERYDAY_CHORES_PRIORITY = [
    3,
    3,
#    3,
    3,
    3
]

LAST_TWO_DAYS = DAYS_ABBR[-2:]

def is_eligible_chore(wd, c):
    if wd in LAST_TWO_DAYS and c != EVERYDAY_CHORES_NAMES[0]:
        return False
    return True

WEEKLY_CHORES_NAMES_RAW = [ wd + "_" + c for wd in DAYS_ABBR
                        for c in EVERYDAY_CHORES_NAMES]

WEEKLY_CHORES_HOURS_RAW = [c for _ in DAYS_ABBR
                       for c in EVERYDAY_CHORES_HOURS]

ELIGIBLE_WEEKLY_CHORES = [is_eligible_chore(wd, c) for wd in DAYS_ABBR
                          for c in EVERYDAY_CHORES_NAMES]

WEEKLY_CHORES_NAMES = [WEEKLY_CHORES_NAMES_RAW[i]
                       for i in range(len(WEEKLY_CHORES_NAMES_RAW))
                       if ELIGIBLE_WEEKLY_CHORES[i]]

WEEKLY_CHORES_HOURS = [WEEKLY_CHORES_HOURS_RAW[i]
                       for i in range(len(WEEKLY_CHORES_HOURS_RAW))
                       if ELIGIBLE_WEEKLY_CHORES[i]]

WEEKLY_CHORES_PRIORITY = [c for _ in DAYS_ABBR
                          for c in EVERYDAY_CHORES_PRIORITY]

WEEKLY_CHORES_NAMES_SET = set(WEEKLY_CHORES_NAMES)

MISC_CHORES_NAMES = [
    "rag_clean",
    "floor1_bathroom_clean",
    "floor1_clean"
]

MISC_CHORES_SET = set(MISC_CHORES_NAMES)

MISC_CHORES_HOURS = [
    0.5,
    1,
    1
]

MISC_CHORES_PRIORITY = [
    1,
    1,
    1
]

TOTAL_CHORES_NAMES = WEEKLY_CHORES_NAMES + MISC_CHORES_NAMES
TOTAL_CHORES_HOURS = WEEKLY_CHORES_HOURS + MISC_CHORES_HOURS
TOTAL_CHORES_PRIORITY = WEEKLY_CHORES_PRIORITY + MISC_CHORES_PRIORITY
TOTAL_CHORES_NAMES_DICT = {TOTAL_CHORES_NAMES[ch] : ch
                           for ch in range(0, len(TOTAL_CHORES_NAMES))}

def get_day(ch):
    return ch.split("_")[0]

def export_schedule(x, possible_combos, chore_assignment):
    # Solution found
    if chore_assignment.status == 1:
        data = {DAYS_ABBR[d]:None for d in range(len(DAYS_ABBR))}
        max_col_len = 0
        misc_data = []
        for c in possible_combos:
            if x[c].value() == 1.0:
                g, ch = c
                is_weekly = ch in WEEKLY_CHORES_NAMES_SET
                if is_weekly:
                    day = get_day(ch)
                    entry = (ch, g)
                    if data.get(day, None) is None:
                        data[day] = [entry]
                    else:
                        data[day].append(entry)
                        if max_col_len < len(data[day]):
                            max_col_len = len(data[day])
                else:
                    misc_data.append((ch, g))

        for k, v in data.items():
            data[k] = v + ([None] * (max_col_len - len(v)))
            data[k] = sorted(data[k], key=lambda entry: TOTAL_CHORES_NAMES_DICT[entry[0]]
                   if entry is not None else sys.maxsize)

        df = pd.DataFrame(data=data)
        df.to_excel("weekly_chores.xlsx", index=False)
        df = pd.DataFrame(data=misc_data, columns=["tasks", "owner"])
        df.to_excel("misc_chores.xlsx", index=False)
    else:
         print("No optimal solution found!")
