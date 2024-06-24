import pulp
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import sys
from utils import TOTAL_CHORES_NAMES_DICT
from utils import TOTAL_CHORES_NAMES
from utils import TOTAL_CHORES_HOURS
from utils import TOTAL_CHORES_PRIORITY
from utils import export_schedule, get_hours
from utils import GUESTS
import csv
from preferences import PREF_DIR

MAX_HOURS_PER_GUEST = 4

# Happiness is a function
# maximize chore hours
# prefer weekly chore assignment first
# every guest should have a maximum of 4 hours
# every guest should be assigned to a maximum of 1 chore per day
PREFERENCES_DICT = {}

for g in GUESTS:
    PREFERENCES_DICT[g] = {}
    with open(f"{PREF_DIR}/{g}.csv", "r+") as f:
        r = csv.reader(f)
        next(r)                 # Skip header
        for row in r:
            PREFERENCES_DICT[g][row[0]] = int(row[1])

def happiness(c):
    g, ch = c
    factor = 1.0
    ch_idx, hours = get_hours(ch)
    chore_priority = TOTAL_CHORES_PRIORITY[ch_idx]
    factor += (hours + chore_priority)
    # Preference multipler
    try:
        pref = int(PREFERENCES_DICT[g][ch])
    except Exception as e:
        print(f"{g} has non-numeric preference for task {ch}. Default to 1.")
        pref = 1
    return factor * pref * chore_priority

### LP setup ###
possible_combos = [(g, tc) for g in GUESTS for tc in TOTAL_CHORES_NAMES
                   if PREFERENCES_DICT[g][tc] != 0]

x = pulp.LpVariable.dicts(
    "chore", possible_combos, lowBound=0, upBound=1, cat=pulp.LpInteger
)

chore_assignment = pulp.LpProblem("Chore assignment combo", pulp.LpMaximize)

chore_assignment += pulp.lpSum(happiness(c) * x[c] for c in possible_combos)

# Every guest gets a per week max hours limit
for guest in GUESTS:
    chore_assignment += (
        pulp.lpSum([get_hours(ch)[1] * x[(g, ch)]
                    for (g, ch) in possible_combos
                    if g == guest]) <= MAX_HOURS_PER_GUEST
    )

# every chore must have one guest
for chore in TOTAL_CHORES_NAMES:
    chore_assignment += (
        pulp.lpSum([x[(g, ch)]
                    for (g, ch) in possible_combos if ch == chore]) == 1.0
    )

chore_assignment.solve()

print(f"The chosen tables are out of a total of {len(possible_combos)}:")
# for c in possible_combos:
#     if x[c].value() == 1.0:
#         print(c, x[c].value())

# print(chore_assignment)

export_schedule(x, possible_combos, chore_assignment)
