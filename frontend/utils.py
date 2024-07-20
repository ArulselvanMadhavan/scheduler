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

def get_day(ch):
    return ch.split("_")[0]

def get_chore(ch):
    xs = ch.split("_")[1:]
    return "_".join(xs)

def get_hours(CHORES_NAMES_DICT, CHORES_HOURS, ch):
    ch_idx = CHORES_NAMES_DICT[ch]
    hours = CHORES_HOURS[ch_idx]
    return ch_idx, hours

def multiple_dfs(df_list, offsets, sheets, file_name, spaces):
    with pd.ExcelWriter(file_name,engine='xlsxwriter') as writer:
        row = 0
        col = 0
        workbook = writer.book
        for df_idx in range(len(df_list)):
            df = df_list[df_idx]
            offset = offsets[df_idx]
            if offset == 'row':
                df.to_excel(writer, sheet_name=sheets,
                            startrow=row, startcol=0, index=False)
            elif offset == 'col':
                df.to_excel(writer, sheet_name=sheets,
                            startrow=0, startcol=col, index=False)

            worksheet = writer.sheets[sheets]
            for idx, df_col in enumerate(df):  # loop through all columns
                series = df[df_col]
                max_len = max((
                    series.astype(str).map(len).max(),  # len of largest item
                    len(str(series.name))  # len of column name/header
                    )) + 1  # adding a little extra space
                # set column width
                worksheet.set_column(col + idx, col + idx, max_len)

            row = row + len(df.index) + spaces + 1
            col = col + len(df.columns) + spaces + 1

        df = df_list[0]
        # Merge cells of day column
        merge_format = workbook.add_format({'align': 'center', 'valign': 'vcenter', 'border': 2})
        for day in df['day'].unique():
            # find indices and add one to account for header
            u=df.loc[df['day']==day].index.values + 1

            if len(u) <2:
                pass # do not merge cells if there is only one car name
            else:
                # merge cells using the first and last indices
                worksheet.merge_range(u[0], 0, u[-1], 0, df.loc[u[0],'day'], merge_format)

def export_schedule(CHORES_NAMES_DICT, CHORES_HOURS,
                    x, possible_combos, chore_assignment):
    # Solution found
    if chore_assignment.status == 1:
        # Keep Sunday last
        data = {DAYS_ABBR[d]:None for d in range(1, len(DAYS_ABBR))}
        data[DAYS_ABBR[0]] = None
        # data = {DAYS_ABBR[d]:None for d in range(len(DAYS_ABBR))}
        max_col_len = 0
        misc_data = []
        for c in possible_combos:
            if x[c].value() == 1.0:
                g, ch = c
                _, hours = get_hours(CHORES_NAMES_DICT, CHORES_HOURS, ch)
                is_weekly = get_day(ch) in DAYS_ABBR
                entry = (ch, g, hours)
                if is_weekly:
                    day = get_day(ch)
                    if data.get(day, None) is None:
                        data[day] = [entry]
                    else:
                        data[day].append(entry)
                        if max_col_len < len(data[day]):
                            max_col_len = len(data[day])
                else:
                    misc_data.append(entry)

        for k, v in data.items():
            if v is not None:
                data[k] = v + ([None] * (max_col_len - len(v)))
                data[k] = sorted(data[k], key=lambda entry: CHORES_NAMES_DICT[entry[0]]
                       if entry is not None else sys.maxsize)
                # cut day from chore name
                data[k] = [None if cg is None else (get_chore(cg[0]), cg[1], cg[2]) for cg in data[k]]
        columns = ["day", "tasks", "owner", "hours"]
        timely_tasks = []
        for day, tasks in data.items():
            if tasks is not None:
                for t in tasks:
                    if t is not None:
                        ch, g, hours = t
                        timely_tasks.append([day, ch, g, hours])
                timely_tasks.append([day, None, None, None])

        guest_hours_dict = {}
        for r in timely_tasks:
            g = r[2]
            if g is None:
                continue
            else:
                guest_hours_dict[g] = guest_hours_dict.get(g, 0) + r[3]
        for m in misc_data:
            g = m[1]
            h = m[2]
            guest_hours_dict[g] = guest_hours_dict.get(g, 0) + h
        guest_hours_list = [[k, v] for k, v in guest_hours_dict.items()]
        # Rotate tasks by 1 - so Sunday comes last
        df0 = pd.DataFrame(data=timely_tasks, columns=columns)
        df1 = pd.DataFrame(data=misc_data, columns=["tasks", "owner", "hours"])
        df2 = pd.DataFrame(data=guest_hours_list, columns=["guests", "hours"])
        multiple_dfs([df0, df1, df2], ["row", "col", "col"], "chores", "chores.xlsx", 1)
        return True
    else:
        return False
