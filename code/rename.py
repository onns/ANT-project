#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys

data_dir = 'data/'

if __name__ == "__main__":
    os.chdir(os.path.dirname(sys.argv[0]))
    file_path = os.path.join(os.path.abspath(os.getcwd()), data_dir)
    # print(file_path)
    file_list = os.listdir(file_path)
    file_list.sort()

    temp = ''
    for filename in file_list:
        # print(filename)
        if(filename.endswith('.json')):
            sitename = filename[:-19]
            if(temp == sitename):
                n += 1
            else:
                n = 0
                temp = sitename
            src = os.path.join(file_path, filename)
            dst = os.path.join(file_path, sitename + '_' + str(n) + '.json')
            # print(dst)
            os.rename(src, dst)
