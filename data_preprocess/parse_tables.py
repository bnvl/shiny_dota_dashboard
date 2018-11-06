import bs4 as bs
import os
import pandas as dp
import numpy as np
import re

path = 'D:/Projects/test_crawler/games/'
htmls = [f for f in os.listdir(path) if f.endswith('html')]

for fname in htmls:
    with open(path+fname, encoding='utf-8') as f:
        soup = bs.BeautifulSoup(f, 'lxml')
        char_row = []

        for char in soup.find_all("img", class_ = "image-hero image-medicon"):
            
            game = re.sub('[^0-9]', "", str(char.find_parent('tr').select_one('td > div.match-link > a').get('href')))
            char_name = char.get("title")
            side = char.find_parent('td', class_ = "r-none-tablet cell-xxlarge").find('span', class_ = ['the-dire', 'the-radiant']).get_text()
            first_status = "First" if char.find_parent('td', class_ = "r-none-tablet cell-xxlarge").find('acronym', rel = 'tooltip') is not None else "Second"
            pick_type = char.find_parent('div', class_ = ['ban', 'pick']).attrs['class'][0]
            pick_num = int(char.find_parent('div', class_ = ['ban', 'pick']).find('div', class_ = 'seq').get_text())
            td_index = char.find_parent('tr').find_all('td').index(char.find_parent('td'))
            winner = char.find_parent('tr').find('td', class_ = 'winner').find('img', class_ = 'img-team').get('alt')
            drafter = char.find_parent('table', class_ = "table").select("thead > tr > th")[td_index].find_next('img', class_ = 'img-team').get('alt')
            win = "Win" if winner == drafter else "Loss"
            
            with open(path+'parse_table.csv', 'a') as f:
                f.write(f"{game},{char_name}, {win}, {side}, {first_status}, {pick_type}, {pick_num}\n")
    print(f"Parsed {fname}")
