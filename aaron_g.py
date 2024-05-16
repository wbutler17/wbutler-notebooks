# -*- coding: utf-8 -*-
"""
Created on Wed May 15 11:00:42 2024

@author: wbutl
"""
import pandas as pd
import numpy as np
import seaborn as sns
from functools import partial
from nba_api.stats.endpoints import shotchartdetail

threes = pd.concat([shotchartdetail.ShotChartDetail(team_id = 0, player_id = 0, 
context_measure_simple = 'FG3A', 
season_type_all_star = ['Regular Season', 'Playoffs'], season_nullable = '2019-20'
).get_data_frames()[0],
shotchartdetail.ShotChartDetail(team_id = 0, player_id = 0, 
context_measure_simple = 'FG3A', 
season_type_all_star = ['Regular Season', 'Playoffs'], season_nullable = '2020-21'
).get_data_frames()[0],
shotchartdetail.ShotChartDetail(team_id = 0, player_id = 0, 
context_measure_simple = 'FG3A', 
season_type_all_star = ['Regular Season', 'Playoffs'], season_nullable = '2021-22'
).get_data_frames()[0],
shotchartdetail.ShotChartDetail(team_id = 0, player_id = 0, 
context_measure_simple = 'FG3A', 
season_type_all_star = ['Regular Season', 'Playoffs'], season_nullable = '2022-23'
).get_data_frames()[0],
shotchartdetail.ShotChartDetail(team_id = 0, player_id = 0, 
context_measure_simple = 'FG3A', 
season_type_all_star = ['Regular Season', 'Playoffs'], season_nullable = '2023-24'
).get_data_frames()[0]])


# Group by PLAYER_NAME and calculate summarised values
grouped_threes = threes.groupby('PLAYER_NAME').agg(
    total_threes_attempted=('EVENT_TYPE', 'size'),
    total_threes_made=('EVENT_TYPE', lambda x: (x == 'Made Shot').sum())
)

# Filter based on conditions
grouped_threes = grouped_threes[(grouped_threes['total_threes_attempted'] >= 11) & (grouped_threes['total_threes_made'] >= 8)]

# Filter the DataFrame based on grouped_threes
ag_like_threes = threes[threes['PLAYER_NAME'].isin(grouped_threes.index)]

# Create a binary outcome column
ag_like_threes['binary_outcome'] = np.where(ag_like_threes['EVENT_TYPE'] == 'Made Shot', 1, 0)

# Define rolling sum function
roll_sum = partial(np.convolve, v=np.ones(11), mode='valid')

# Group by PLAYER_NAME and calculate rolling sum
ag_like_threes['rolled'] = ag_like_threes.groupby('PLAYER_NAME')['binary_outcome'].rolling(11, min_periods=1).sum().reset_index(drop=True)

# Filter based on condition
ag_like_threes = ag_like_threes[ag_like_threes['rolled'] >= 8]

# Only count each game once where a player achieved 8+ out of 11
ag_like_threes = ag_like_threes[~ag_like_threes.duplicated(['GAME_ID', 'PLAYER_ID'])]

streak_counts_by_player = grouped_threes.join(pd.DataFrame(ag_like_threes.groupby('PLAYER_NAME')['rolled'].count()))

streak_counts_by_player = streak_counts_by_player[streak_counts_by_player['total_threes_attempted'] >= 500]

streak_counts_by_player['streak_perc'] = streak_counts_by_player['rolled'] / streak_counts_by_player['total_threes_attempted']
streak_counts_by_player['threept_perc'] = streak_counts_by_player['total_threes_made'] / streak_counts_by_player['total_threes_attempted']

sns.scatterplot(data = streak_counts_by_player, x = 'threept_perc', y = 'streak_perc')