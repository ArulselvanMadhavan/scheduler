import pulp
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import sys
from utils import TOTAL_CHORES_NAMES_DICT
from utils import TOTAL_CHORES_NAMES
from utils import TOTAL_CHORES_HOURS
from utils import TOTAL_CHORES_PRIORITY
from utils import export_schedule

GUESTS = [
    "Arul",
    "Asa",
    "Rachel",
    "Triana",
    "Oliver",
    "Catherine",
    "Shahanae"
]

MAX_HOURS_PER_GUEST = 4

# Happiness is a function
# maximize chore hours
# prefer weekly chore assignment first
# every guest should have a maximum of 4 hours
# every guest should be assigned to a maximum of 1 chore per day
def get_hours(ch):
    ch_idx = TOTAL_CHORES_NAMES_DICT[ch]
    hours = TOTAL_CHORES_HOURS[ch_idx]
    return ch_idx, hours

def happiness(c):
    g, ch = c
    factor = 1.0
    ch_idx, hours = get_hours(ch)
    priority = TOTAL_CHORES_PRIORITY[ch_idx]
    factor += (hours + priority)
    return factor

def get_chore(ch):
    xs = ch.split("_")[1:]
    return "_".join(xs)

# def table2pdf(df):
#     #https://stackoverflow.com/questions/32137396/how-do-i-plot-only-a-table-in-matplotlib
#     fig, ax =plt.subplots(figsize=(4, 10))
#     ax.axis('tight')
#     ax.axis('off')
#     the_table = ax.table(cellText=df.values,colLabels=df.columns,loc='center')

#     #https://stackoverflow.com/questions/4042192/reduce-left-and-right-margins-in-matplotlib-plot
#     pp = PdfPages("chores.pdf")
#     pp.savefig(fig, bbox_inches='tight')
#     pp.close()

### LP setup ###
possible_combos = [(g, tc) for g in GUESTS for tc in TOTAL_CHORES_NAMES]

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
        pulp.lpSum([x[(g, ch)] for (g, ch) in possible_combos if ch == chore]) == 1.0
    )

chore_assignment.solve()

print(f"The chosen tables are out of a total of {len(possible_combos)}:")
for c in possible_combos:
    if x[c].value() == 1.0:
        print(c, x[c].value())

export_schedule(x, possible_combos, chore_assignment)
