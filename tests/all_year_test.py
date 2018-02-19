from os import sys, path
sys.path.append(path.dirname(path.dirname(path.abspath(__file__))))
from calculations.all_year import all_year
from utils.helpers import create_folders

create_folders()

start_date = '10/1'
directory_name = 'tests/testFiles'
end_with = 'Case_1.csv'
class_number = 3
gauge_numbers = [11475560]

print('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
print('All Year calculation')
all_year(start_date, directory_name, end_with, class_number, None, False)
all_year(start_date, directory_name, end_with, None, gauge_numbers, False)
