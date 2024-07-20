import pulp
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import sys
from utils import export_schedule, get_hours
import csv

HEADER=["task", "pref"]
def guests_df():
    guests_df = pd.read_csv(f"guests/guests.csv", dtype={"guest": str, "hours": float})
    guests_df = guests_df[guests_df['hours'] > 0]
    GUESTS = guests_df['guest'].to_list()
    HOURS = guests_df['hours'].to_list()
    GUESTS_AND_HOURS = zip(GUESTS, HOURS)
    return GUESTS, GUESTS_AND_HOURS

def chores_df():
    chores_df = pd.read_csv("chores/misc_chores.csv")
    chores_df["priority"] = chores_df["priority"].astype(int)
    chores_df = chores_df[chores_df["priority"] > 0]

    CHORES_NAMES = chores_df["chore_name"].to_list()

    CHORES_SET = set(CHORES_NAMES)

    CHORES_HOURS = chores_df["hours"].to_list()
    CHORES_PRIORITY = chores_df["priority"].to_list()
    CHORES_NAMES_DICT = {CHORES_NAMES[ch] : ch
                               for ch in range(0, len(CHORES_NAMES))}
    return CHORES_NAMES_DICT, CHORES_HOURS, CHORES_PRIORITY, CHORES_NAMES

def solve ():
    PREFERENCES_DICT = {}
    GUESTS, GUESTS_AND_HOURS = guests_df ()
    CHORES_NAMES_DICT, CHORES_HOURS, CHORES_PRIORITY, CHORES_NAMES = chores_df ()

    for g in GUESTS:
        PREFERENCES_DICT[g] = {}
        with open(f"preferences/{g}.csv", "r+") as f:
            r = csv.reader(f)
            next(r)                 # Skip header
            for row in r:
                PREFERENCES_DICT[g][row[0]] = int(row[1])

    def happiness(CHORES_NAMES_DICT, CHORES_HOURS, CHORES_PRIORITY, PREFERENCES_DICT, c):
        g, ch = c
        factor = 1.0
        ch_idx, hours = get_hours(CHORES_NAMES_DICT, CHORES_HOURS, ch)
        chore_priority = CHORES_PRIORITY[ch_idx]
        factor += (hours + chore_priority)
        # Preference multipler
        try:
            pref = int(PREFERENCES_DICT[g][ch])
        except Exception as e:
            print(f"{g} has non-numeric preference for task {ch}. Default to 1.")
            pref = 1
        return factor * pref * chore_priority

    ### LP setup ###
    possible_combos = [(g, tc) for g in GUESTS for tc in CHORES_NAMES
                       if PREFERENCES_DICT[g][tc] != 0]

    x = pulp.LpVariable.dicts(
        "chore", possible_combos, lowBound=0, upBound=1, cat=pulp.LpInteger
    )

    chore_assignment = pulp.LpProblem("Chore assignment combo", pulp.LpMaximize)

    chore_assignment += pulp.lpSum(happiness(CHORES_NAMES_DICT, CHORES_HOURS, CHORES_PRIORITY, PREFERENCES_DICT, c) * x[c]
                                   for c in possible_combos)

    # Every guest gets a per week max hours limit
    for guest, max_hours in GUESTS_AND_HOURS:
        print(guest, max_hours)
        chore_assignment += (
            pulp.lpSum([get_hours(CHORES_NAMES_DICT, CHORES_HOURS, ch)[1] * x[(g, ch)]
                        for (g, ch) in possible_combos
                        if g == guest]) <= max_hours
        )

    # every chore with the highest priority must have a guest assigned to it
    for chore in CHORES_NAMES:
        ch_idx = CHORES_NAMES_DICT[chore]
        ch_prio = CHORES_PRIORITY[ch_idx]
        # if ch_prio > 1:
        #     # High priority chores should have a guest assigned to their name
        #     chore_assignment += (
        #         pulp.lpSum([x[(g, ch)]
        #                     for (g, ch) in possible_combos if ch == chore]) == 1.0
        #     )
        # else:
        chore_assignment += (
            pulp.lpSum([x[(g, ch)]
                        for (g, ch) in possible_combos if ch == chore]) <= 1.0
        )

    # per day - max hours for guests should be less than 2
    chore_assignment.solve()

    print(f"The chosen tables are out of a total of {len(possible_combos)}:")
    # for c in possible_combos:
    #     if x[c].value() == 1.0:
    #         print(c, x[c].value())

    # print(chore_assignment)

    is_found = export_schedule(CHORES_NAMES_DICT, CHORES_HOURS, x, possible_combos, chore_assignment)
    return is_found

# solve ()
